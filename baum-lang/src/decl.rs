use crate::expr::ExprParser;
use crate::types::mixfix::{Precedence, Regex};
use crate::types::parse::*;
use baum_core::types as core;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

macro_rules! log {
  ($($arg:tt)*) => {
    // eprintln!($($arg)*);
  };
}

type Error = ();
type Result<T> = std::result::Result<T, Error>;

pub struct DeclParser<'a> {
  tracker: Tracker<'a>,
  scope: Env<'a>,
  known_ops: HashSet<String>,
  errors: Vec<String>,
}

impl<'a> DeclParser<'a> {
  pub fn new(
    tracker: Tracker<'a>,
    scope: Env<'a>,
    known_ops: HashSet<String>,
    errors: Vec<String>,
  ) -> Self {
    DeclParser {
      tracker,
      scope,
      known_ops,
      errors,
    }
  }

  pub fn into_inner(self) -> (Tracker<'a>, HashSet<String>, Vec<String>) {
    (self.tracker, self.known_ops, self.errors)
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, decl: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn expect_reserved(&mut self, str: &str) -> Result<()> {
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == str => {
        self.tracker.next();
        return Ok(());
      }
      _ => {
        self.add_error(self.tracker.pos(), &format!("expected '{}'", str));
        return Err(());
      }
    }
  }

  fn peek_is(&mut self, ty: TokenType) -> Result<()> {
    match self.tracker.peek() {
      Some(t) if t.ty == ty => {
        return Ok(());
      }
      _ => {
        self.add_error(
          self.tracker.pos(),
          match ty {
            TokenType::Ident => "expected identifier",
            TokenType::String => "expected string",
            _ => panic!(),
          },
        );
        return Err(());
      }
    }
  }

  fn expect_id(&mut self) -> Result<Id<'a>> {
    self.peek_is(TokenType::Ident)?;
    let s = self.tracker.peek().unwrap().str;
    self.tracker.next();
    Ok(Id::new(s))
  }

  fn allow_semicolon(&mut self) {
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ";" => {
        self.tracker.next();
      }
      _ => {}
    }
  }

  fn ids(&mut self) -> Vec<Id<'a>> {
    // space-separated identifiers
    let mut ids = Vec::new();
    loop {
      match self.tracker.peek() {
        Some(t) if t.ty == TokenType::Ident => {
          ids.push(Id::new(t.str));
          self.tracker.next();
        }
        _ => break,
      }
    }
    if ids.is_empty() {
      self.add_error(self.tracker.pos(), "expected identifier list");
    }
    ids
  }

  pub fn def(&mut self) -> Option<Def<'a>> {
    let outer_indent = self.tracker.save_indent();
    let indent = match self.tracker.peek() {
      Some(t) => t.indent,
      None => return None,
    };
    let id = self.expect_id().ok()?;
    self.tracker.set_indent(indent);
    let def = self.def_rest(id);
    self.tracker.restore_indent(outer_indent);
    def.ok()
  }

  fn def_rest(&mut self, name: Id<'a>) -> Result<Def<'a>> {
    let mut args = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          self.add_error(self.tracker.end_of_line(), "expected definition");
          return Err(());
        }
      };
      if t.ty == TokenType::Reserved {
        if t.str == ":" || t.str == "=" {
          break;
        }
        if t.str == "(" || t.str == "{" {
          let vis = if t.str == "(" {
            Vis::Explicit
          } else {
            Vis::Implicit
          };
          self.tracker.next();
          let ids = self.ids();
          let match_cl = match vis {
            Vis::Explicit => ")",
            Vis::Implicit => "}",
          };
          let arg = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
              self.tracker.next();
              let e = self.expr_or_hole(Some(match_cl));
              (vis, ids, Some(Box::new(e)))
            }
            _ => (vis, ids, None),
          };
          self.expect_reserved(match_cl)?;
          args.push(arg);
        } else {
          self.add_error(self.tracker.pos(), "expected argument list or assignment");
          return Err(());
        }
      } else {
        let ids = self.ids();
        args.push((Vis::Explicit, ids, None));
      }
    }
    let ty = match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
        self.tracker.next();
        let e = self.expr_or_hole(Some("="));
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_reserved("=")?;
    let body = self.expr_or_hole(None);
    Ok(Def {
      name,
      args,
      ty,
      body: Box::new(body),
    })
  }

  fn expr(&mut self) -> Option<Expr<'a>> {
    let tracker = std::mem::take(&mut self.tracker);
    let known_ops = std::mem::take(&mut self.known_ops);
    let errors = std::mem::take(&mut self.errors);
    log!("EXPR INTO: {:?}", tracker.peek());
    log!("SCOPE: {:?}", self.scope.modules);
    let mut e = ExprParser::new(tracker, &self.scope, known_ops, errors);
    let res = e.expr();
    log!("EXPR RES: {:?}", res);
    let (tracker, known_ops, errors) = e.into_inner();
    self.tracker = tracker;
    self.known_ops = known_ops;
    self.errors = errors;
    res
  }

  fn end_of_expr(&mut self) -> bool {
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Ident && self.known_ops.contains(t.str) => true,
      Some(t)
        if t.ty == TokenType::Reserved
          && (self.known_ops.contains(t.str) || t.str == ";" || t.str == "}") =>
      {
        true
      }
      None => true,
      _ => false,
    }
  }

  fn expr_or_hole(&mut self, wait_next: Option<&str>) -> Expr<'a> {
    let pos = self.tracker.pos();
    let e = self.expr().unwrap_or(Expr(ExprF::Hole, pos));

    if let Some(wait_next) = wait_next {
      // next must be wait_next
      match self.tracker.peek() {
        Some(t) if t.ty == TokenType::Reserved && t.str == wait_next => {}
        _ => {
          self.add_error(self.tracker.pos(), "unexpected token?");
        }
      }
    } else {
      if !self.end_of_expr() {
        let t = self.tracker.peek().unwrap();
        self.add_error(
          self.tracker.pos(),
          &format!("unexpected end of expr: {:?}", t.str),
        );
        while !self.end_of_expr() {
          self.tracker.next();
        }
      }
    }

    e
  }

  fn syntax(&mut self, pos: TokenPos) -> Result<(Syntax<'a>, Decl<'a>)> {
    // syntax (w/o "syntax")
    let (precs, prec_str) = match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Precedence => match Precedence::parse(t.str) {
        Some(precs) => {
          let s = t.str;
          self.tracker.next();
          (precs, Some(s))
        }
        None => {
          self.add_error(self.tracker.pos(), "failed to parse precedence");
          return Err(());
        }
      },
      _ => ((Precedence::Terminal, Precedence::Terminal), None),
    };
    let mut tokens = Vec::new();
    loop {
      match self.tracker.peek() {
        Some(t) => {
          if t.ty == TokenType::Reserved && t.str == "=" {
            self.tracker.next();
            break;
          } else {
            // TODO: restrict some symbols
            tokens.push(t.clone());
            self.tracker.next();
          }
        }
        _ => {
          self.add_error(
            self.tracker.end_of_line(),
            "expected body of syntax definition",
          );
          return Err(());
        }
      }
    }
    let e_pos = self.tracker.pos();
    let e = self.expr_or_hole(None); // TODO: nat/rat/chr/str placeholder
    let fvs = match fv(&e) {
      Some(fvs) => fvs,
      None => {
        self.add_error(e_pos, "declarations cannot be found in syntax declaration");
        return Err(());
      }
    };
    // TODO: dup check?
    let fvs: HashMap<Id, Role> = fvs.into_iter().collect();
    let mut rs = Vec::new();
    let mut defs = Vec::new();
    for t in tokens {
      match t.ty {
        TokenType::Reserved => {
          rs.push(Regex::token(t.str));
          defs.push(SynDefF::Token(t.str));
        }
        TokenType::Ident => {
          let id = Id::new(t.str);
          if let Some(role) = fvs.get(&id) {
            match role {
              Role::Expr => {
                rs.push(Regex::e());
                defs.push(SynDefF::Expr(id));
              }
              Role::Ident => {
                rs.push(Regex::id());
                defs.push(SynDefF::Ident(id));
              }
            }
          } else {
            rs.push(Regex::token(t.str));
            defs.push(SynDefF::Token(t.str));
          }
        }
        _ => {
          self.add_error(t.pos, "expected identifier or symbols");
          return Err(());
        }
      }
    }
    let regex = Regex::seqs(rs.iter().collect());
    let captured_defs = defs.clone();
    let captured_e = e.clone();
    let interpreter: SyntaxInterpreter<'a> = Rc::new(move |elems, mod_name, c| {
      fn replace<'a>(
        e: &Expr<'a>,
        e_map: &HashMap<Id<'a>, core::Expr>,
        i_map: &HashMap<Id<'a>, core::Id>,
        mod_name: &Vec<Id<'a>>,
        c: &mut dyn SyntaxHandler<'a>,
      ) -> core::Expr {
        match &e.0 {
          ExprF::Var(id) => match e_map.get(id) {
            Some(e) => e.clone(),
            None => c.convert_e(&e),
          },
          ExprF::Syntax(x, _, ref elems) => {
            let es = elems
              .iter()
              .map(|elem| match elem {
                SyntaxElem::Ident(id) => match i_map.get(id) {
                  Some(i) => CoreElem::Ident(i.clone()),
                  None => c.convert_se(elem),
                },
                SyntaxElem::Expr(e) => {
                  let e = replace(e, e_map, i_map, &mod_name, c);
                  CoreElem::Expr(e)
                }
                _ => c.convert_se(elem),
              })
              .collect();
            (x.t)(es, mod_name, c)
          }
          _ => c.convert_e(&e),
        }
      }
      let defs = &captured_defs;
      assert!(elems.len() == defs.len());
      let mut e_map = HashMap::new();
      let mut i_map = HashMap::new();
      for (d, e) in defs.iter().zip(elems.iter()) {
        match d {
          SynDefF::Token(s1) => match e {
            CoreElem::Token(s2) => {
              assert_eq!(s1, s2);
            }
            _ => panic!(),
          },
          SynDefF::Expr(id) => match e {
            CoreElem::Expr(e) => {
              e_map.insert(id.clone(), e.clone());
            }
            _ => panic!(),
          },
          SynDefF::Ident(id) => match e {
            CoreElem::Ident(i) => {
              i_map.insert(id.clone(), i.clone());
            }
            CoreElem::Nat(n) => {
              todo!();
            }
            CoreElem::Rat(r) => {
              todo!();
            }
            CoreElem::Chr(c) => {
              todo!();
            }
            CoreElem::Str(s) => {
              todo!();
            }
            _ => panic!(),
          },
        }
      }
      replace(&captured_e, &e_map, &i_map, mod_name, c)
    });
    let syntax = Syntax::new(precs.0, precs.1, regex, interpreter);
    log!("SYNTAX: {:?}", syntax);
    return Ok((
      syntax,
      Decl(DeclF::Syntax(prec_str, defs, Box::new(e)), pos),
    ));
  }

  fn extract_modref(&mut self, e: Expr<'a>) -> Result<(Vec<Id<'a>>, Vec<(Vis, Box<Expr<'a>>)>)> {
    let mut rev_params = Vec::new();
    let mut e = e;
    loop {
      match e.0 {
        ExprF::Mod(name) => {
          let params = rev_params.into_iter().rev().collect();
          return Ok((name, params));
        }
        ExprF::Syntax(_, _, args) => {
          if let [SyntaxElem::Expr(e0), SyntaxElem::Expr(e1)] = args.as_slice() {
            rev_params.push((Vis::Explicit, e1.clone()));
            e = *e0.clone();
            continue;
          } else if let [SyntaxElem::Expr(e0), SyntaxElem::Token("{"), SyntaxElem::Expr(e1), SyntaxElem::Token("}")] =
            args.as_slice()
          {
            rev_params.push((Vis::Implicit, e1.clone()));
            e = *e0.clone();
            continue;
          }
        }
        _ => {}
      }
      self.add_error(e.1, "expected module application");
      return Err(());
    }
  }

  fn modref_internal(&mut self) -> Result<ModRef<'a>> {
    let t = match self.tracker.peek() {
      Some(t) => t.clone(),
      None => {
        self.add_error(self.tracker.end_of_line(), "expected module reference");
        return Err(());
      }
    };
    if t.ty != TokenType::Ident {
      self.add_error(t.pos, "expected module reference");
      return Err(());
    }
    if t.str == "import" {
      // import
      self.tracker.next();
      self.peek_is(TokenType::String)?;
      let s = self.tracker.peek().unwrap().str;
      self.tracker.next();
      return Ok(ModRefF::Import(s));
    }

    // reference with parameters

    let e = self.expr_or_hole(None);
    let (name, params) = self.extract_modref(e)?;
    Ok(ModRefF::App(name, params))
  }

  fn modref(&mut self, indent: Indent) -> Result<ModRef<'a>> {
    let outer_indent = self.tracker.save_indent();
    self.tracker.set_indent(indent);
    let module = self.modref_internal();
    self.tracker.restore_indent(outer_indent);
    module
  }

  pub fn resolve_modref(&self, mr: &ModRef<'a>) -> Option<&Rc<Env<'a>>> {
    match mr {
      ModRefF::Import(_) => {
        unimplemented!()
      }
      ModRefF::App(is, _) => self.scope.lookup(&is),
    }
  }

  fn decl_internal(&mut self, cur_mod: &mut Env<'a>) -> Result<Decl<'a>> {
    let t = match self.tracker.peek() {
      Some(t) => t.clone(),
      None => {
        self.add_error(self.tracker.end_of_line(), "expected declaration");
        return Err(());
      }
    };
    if t.ty != TokenType::Ident {
      self.add_error(self.tracker.pos(), "expected declaration");
      return Err(());
    }
    if t.str == "local" {
      // local
      self.tracker.next();
      let t2 = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          self.add_error(t.pos, "expected declaration or block");
          return Err(());
        }
      };
      let mut empty_env = Env::new();
      let d = if t2.ty == TokenType::Reserved && t2.str == "{" {
        // local block
        self.tracker.next();
        let ds = self.decls(&mut empty_env);
        self.expect_reserved("}")?;
        DeclF::Local(ds)
      } else {
        // local single declaration
        let d = self.decl(&mut empty_env)?;
        DeclF::Local(vec![d])
      };
      return Ok(Decl(d, t.pos));
    };
    if t.str == "module" {
      // module declaration
      self.tracker.next();
      let mod_pos = self.tracker.pos();
      let mod_indent = t.indent;
      let name = self.expect_id()?;
      let mut params = Vec::new();
      loop {
        let t = match self.tracker.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(
              self.tracker.end_of_line(),
              "expected module parameters or body",
            );
            return Err(());
          }
        };
        if t.ty == TokenType::Reserved {
          if t.str == "=" {
            self.tracker.next();
            break;
          }
          if t.str == "(" || t.str == "{" {
            let vis = if t.str == "(" {
              Vis::Explicit
            } else {
              Vis::Implicit
            };
            self.tracker.next();
            let ids = self.ids();
            self.expect_reserved(":")?;
            let match_cl = match vis {
              Vis::Explicit => ")",
              Vis::Implicit => "}",
            };
            let e = self.expr_or_hole(Some(match_cl));
            self.expect_reserved(match_cl)?;
            params.push((vis, ids, Some(Box::new(e))));
          } else {
            self.add_error(self.tracker.pos(), "expected module parameters or body");
            return Err(());
          }
        } else {
          self.add_error(self.tracker.pos(), "expected module parameters or body");
          return Err(());
        }
      }
      let mod_decl = ModDeclF { name, params };
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          self.add_error(self.tracker.pos(), "expected module body");
          return Err(());
        }
      };
      let (md, env) = if t.ty == TokenType::Reserved && t.str == "{" {
        // declarations
        let pos = self.tracker.pos();
        self.tracker.next();
        let scope = self.scope.clone();
        let mut mod_env = Env::new();
        let ds = self.decls(&mut mod_env);
        self.scope = scope;
        self.expect_reserved("}")?;
        (ModDef(ModDefF::Decls(ds), pos), Some(Rc::new(mod_env)))
      } else {
        let mr = self.modref(mod_indent)?;
        let e = self.resolve_modref(&mr).cloned();
        (ModDef(ModDefF::Ref(mr), t.pos), e)
      };
      match env {
        Some(env) => {
          let name = mod_decl.name.clone();
          cur_mod.add_module(name.clone(), env.clone());
          self.scope.add_module(name, env);
        }
        None => {
          self.add_error(md.1, "module not found...");
          return Err(());
        }
      }
      return Ok(Decl(DeclF::ModDef(mod_decl, Box::new(md)), mod_pos));
    }
    if t.str == "use" {
      // use (= local open)
      self.tracker.next();
      let mr = self.modref(t.indent)?;
      if let Some(e) = self.resolve_modref(&mr).cloned() {
        if let Err(_) = self.scope.merge(e) {
          self.add_error(t.pos, "module name conflict");
        }
      }
      let open = Decl(DeclF::Open(mr), t.pos);
      return Ok(Decl(DeclF::Local(vec![open]), t.pos));
    }
    if t.str == "open" {
      // open
      self.tracker.next();
      let mr = self.modref(t.indent)?;
      if let Some(e) = self.resolve_modref(&mr).cloned() {
        if let Err(_) = cur_mod.merge(e.clone()) {
          self.add_error(t.pos, "module name conflict");
        }
        let _ = self.scope.merge(e);
      }
      return Ok(Decl(DeclF::Open(mr), t.pos));
    }
    if t.str == "syntax" {
      // syntax
      let outer_indent = self.tracker.save_indent();
      self.tracker.next();
      self.tracker.set_indent(t.indent);
      let res = self.syntax(t.pos);
      self.tracker.restore_indent(outer_indent);
      let (syntax, d) = res?;
      cur_mod.add_syntax(syntax.clone());
      self.scope.add_syntax(syntax.clone());
      return Ok(d);
    }

    // general identifier: definition
    let def = self.def().ok_or(())?;
    return Ok(Decl(DeclF::Def(def), t.pos));
  }

  fn decl(&mut self, cur_mod: &mut Env<'a>) -> Result<Decl<'a>> {
    let res = self.decl_internal(cur_mod)?;
    self.allow_semicolon();
    Ok(res)
  }

  pub fn decls(&mut self, cur_mod: &mut Env<'a>) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    loop {
      match self.tracker.peek() {
        Some(t) => {
          if self.known_ops.contains(t.str) {
            break;
          }
          if t.ty != TokenType::Ident {
            break;
          }
        }
        None => break,
      }
      let orig_pos = self.tracker.pos();
      match self.decl(cur_mod) {
        Ok(d) => ds.push(d),
        Err(_) => self.tracker.skip_to_next_head(),
      }
      let cur_pos = self.tracker.pos();
      if orig_pos == cur_pos {
        break;
      }
    }
    return ds;
  }

  pub fn program(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    let mut cur_mod = Env::new();
    loop {
      let orig_pos = self.tracker.pos();
      match self.tracker.peek() {
        Some(_) => ds.extend(self.decls(&mut cur_mod)),
        None => break,
      }
      let cur_pos = self.tracker.pos();
      if orig_pos == cur_pos {
        self.add_error(cur_pos, "expected declaration");
        self.tracker.next();
        self.tracker.skip_to_next_head();
      }
    }
    if let Some(t) = self.tracker.peek_raw() {
      let t = t.clone();
      self.add_error(t.pos, "not all tokens consumed");
    }
    ds
  }
}
