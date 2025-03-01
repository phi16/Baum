use crate::expr::ExprParser;
use crate::types::env::{Env, Syntax};
use crate::types::precedence::Precedence;
use crate::types::regex::Regex;
use crate::types::token::{ErrorPos, Indent, TokenPos, TokenType};
use crate::types::tracker::Tracker;
use crate::types::tree::*;
use crate::types::tree_base::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type Result<T> = std::result::Result<T, (ErrorPos, String)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Role {
  Expr,
  Ident,
}

fn fv<'a>(e: &Expr<'a>) -> Option<Vec<(Id<'a>, Role)>> {
  fn fv_internal<'a>(e: &Expr<'a>, v: &mut Vec<(Id<'a>, Role)>, invalid: &mut bool) {
    match e.0 {
      ExprF::Var(ref id) => {
        v.push((id.clone(), Role::Expr));
      }
      ExprF::Syntax(_, _, ref elems) => {
        for elem in elems {
          match elem {
            SyntaxElem::Ident(ref id) => {
              v.push((id.clone(), Role::Ident));
            }
            SyntaxElem::Expr(ref e) => {
              fv_internal(e, v, invalid);
            }
            _ => {}
          }
        }
      }
      ExprF::Let(_, _) => {
        *invalid = true;
      }
      _ => {}
    }
  }
  let mut v = Vec::new();
  let mut invalid = false;
  fv_internal(e, &mut v, &mut invalid);
  if invalid {
    None
  } else {
    Some(v)
  }
}

pub struct DeclParser<'a> {
  tracker: Tracker<'a>,
  env: Env<'a>,
  next_syntax_id: u16,
  known_ops: HashSet<String>,
  errors: Vec<String>,
}

impl<'a> DeclParser<'a> {
  pub fn new(
    tracker: Tracker<'a>,
    env: Env<'a>,
    next_syntax_id: u16,
    known_ops: HashSet<String>,
    errors: Vec<String>,
  ) -> Self {
    DeclParser {
      tracker,
      env,
      next_syntax_id,
      known_ops,
      errors,
    }
  }

  pub fn into_inner(self) -> (Tracker<'a>, Env<'a>, u16, HashSet<String>, Vec<String>) {
    (
      self.tracker,
      self.env,
      self.next_syntax_id,
      self.known_ops,
      self.errors,
    )
  }

  fn add_error(&mut self, pos: ErrorPos, msg: &str) {
    let s = format!("{}, decl: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn err_here<T>(&mut self, msg: &str) -> Result<T> {
    Err((self.tracker.epos(), msg.to_string()))
  }

  fn expect_str(&mut self, s: &str) -> Result<()> {
    match self.tracker.peek_str() {
      Some(t) if t == s => {
        self.tracker.next();
        Ok(())
      }
      _ => self.err_here(&format!("expected '{}'", s)),
    }
  }

  fn expect_id(&mut self) -> Result<Id<'a>> {
    match self.tracker.peek_ty_str() {
      Some((TokenType::Ident, s)) => {
        self.tracker.next();
        Ok(Id::new(s))
      }
      _ => self.err_here("expected identifier"),
    }
  }

  fn allow_semicolon(&mut self) {
    match self.tracker.peek_str() {
      Some(";") => {
        self.tracker.next();
      }
      _ => {}
    }
  }

  fn ids(&mut self) -> Option<Vec<Id<'a>>> {
    let mut ids = Vec::new();
    loop {
      match self.tracker.peek_ty_str() {
        Some((TokenType::Ident, s)) => {
          ids.push(Id::new(s));
          self.tracker.next();
        }
        _ => break,
      }
    }
    if ids.is_empty() {
      self.add_error(self.tracker.epos(), "expected identifier list");
      None
    } else {
      Some(ids)
    }
  }

  fn def_rest(&mut self, name: Id<'a>) -> Result<Def<'a>> {
    let mut args = Vec::new();
    loop {
      match self.tracker.peek_str() {
        None => return self.err_here("expected definition"),
        Some(":") | Some("=") => break,
        Some("(") | Some("{") => {
          let vis = if self.tracker.peek().unwrap().str == "(" {
            Vis::Explicit
          } else {
            Vis::Implicit
          };
          self.tracker.next();
          let ids = self
            .ids()
            .ok_or((self.tracker.epos(), "expected identifier list".to_string()))?;
          let match_cl = match vis {
            Vis::Explicit => ")",
            Vis::Implicit => "}",
          };
          let arg = match self.tracker.peek_str() {
            Some(":") => {
              self.tracker.next();
              let e = self.expr_or_hole(Some(match_cl));
              (vis, ids, Some(Box::new(e)))
            }
            _ => (vis, ids, None),
          };
          self.expect_str(match_cl)?;
          args.push(arg);
        }
        Some(_) => {
          let ids = self
            .ids()
            .ok_or((self.tracker.epos(), "expected identifier list".to_string()))?;
          args.push((Vis::Explicit, ids, None));
        }
      }
    }
    let ty = match self.tracker.peek_str() {
      Some(":") => {
        self.tracker.next();
        let e = self.expr_or_hole(Some("="));
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_str("=")?;
    let body = self.expr_or_hole(None);
    Ok(Def {
      name,
      args,
      ty,
      body: Box::new(body),
    })
  }

  fn expr(&mut self, in_syntax: bool) -> Result<Expr<'a>> {
    let pos = self.tracker.epos();

    let mut tracker = Tracker::new(Vec::new());
    std::mem::swap(&mut self.tracker, &mut tracker);
    let known_ops = std::mem::take(&mut self.known_ops);
    let errors = std::mem::take(&mut self.errors);

    let mut e = ExprParser::new(
      tracker,
      &self.env,
      self.next_syntax_id,
      in_syntax,
      known_ops,
      errors,
    );
    let res = e.expr();

    let (tracker, next_syntax_id, known_ops, errors) = e.into_inner();
    self.tracker = tracker;
    self.next_syntax_id = next_syntax_id;
    self.known_ops = known_ops;
    self.errors = errors;
    res.ok_or((pos, "expected expression".to_string()))
  }

  fn end_of_expr(&mut self) -> bool {
    match self.tracker.peek_str() {
      Some(";") | Some("}") => true,
      Some(s) if self.known_ops.contains(s) => true,
      None => true,
      _ => false,
    }
  }

  fn expr_or_hole(&mut self, wait_next: Option<&str>) -> Expr<'a> {
    let e = match self.expr(false) {
      Ok(e) => e,
      Err((epos, msg)) => {
        self.add_error(epos, &msg);
        Expr(ExprF::Hole, self.tracker.range_here())
      }
    };

    if let Some(wait_next) = wait_next {
      match self.tracker.peek_str() {
        Some(s) if s == wait_next => {}
        _ => {
          self.add_error(self.tracker.epos(), &format!("expected '{}'", wait_next));
        }
      }
    } else {
      if !self.end_of_expr() {
        self.add_error(self.tracker.epos(), "expected end of expression");
      }
      while !self.end_of_expr() {
        self.tracker.next();
      }
    }

    e
  }

  fn expr_in_syntax(&mut self) -> Result<Expr<'a>> {
    self.expr(true)
  }

  fn syntax(&mut self) -> Result<(Syntax, DeclInternal<'a>)> {
    // syntax (w/o "syntax")
    let (precs, prec_str) = match self.tracker.peek_ty_str() {
      Some((TokenType::Precedence, prec_str)) => match Precedence::parse(prec_str) {
        None => return self.err_here("failed to parse precedence"),
        Some(precs) => {
          self.tracker.next();
          (precs, Some(prec_str))
        }
      },
      _ => ((Precedence::Terminal, Precedence::Terminal), None),
    };

    let mut tokens = Vec::new();
    loop {
      match self.tracker.peek_str() {
        None => return self.err_here("expected body of syntax definition"),
        Some("=") => {
          self.tracker.next();
          break;
        }
        Some(_) => {
          // TODO: restrict some symbols...
          tokens.push(self.tracker.peek().unwrap().clone());
          self.tracker.next();
        }
      }
    }
    let pos = self.tracker.epos();
    let e = self.expr_in_syntax()?;
    let fvs = match fv(&e) {
      None => {
        return Err((
          pos,
          "let expression cannot be used in syntax declaration".to_string(),
        ))
      }
      Some(fvs_v) => {
        let mut fvs = HashMap::new();
        for (id, role) in fvs_v {
          if fvs.contains_key(&id) {
            if fvs.get(&id) == Some(&Role::Ident) || role == Role::Ident {
              return Err((
                pos,
                "duplicated identifier in syntax declaration".to_string(),
              ));
            }
          }
          fvs.insert(id, role);
        }
        fvs
      }
    };
    let mut rs = Vec::new();
    let mut defs = Vec::new();
    let mut last_is_expr = false;
    for t in tokens {
      let mut is_expr = false;
      match t.ty {
        TokenType::Reserved => {
          rs.push(Regex::token(t.str));
          defs.push(SynDefF::Token(t.str));
        }
        TokenType::Ident => {
          let id = Id::new(t.str);
          if let Some(role) = fvs.get(&id) {
            if last_is_expr {
              return Err((
                t.pos.into(),
                "placeholders must not be consecutive".to_string(),
              ));
            }
            match role {
              Role::Expr => {
                rs.push(Regex::e());
                defs.push(SynDefF::Expr(id));
                is_expr = true;
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
        _ => return Err((t.pos.into(), "expected identifier or symbols".to_string())),
      }
      last_is_expr = is_expr;
    }
    let regex = Regex::seqs(rs.iter().collect());
    let sid = SyntaxId::User(self.next_syntax_id);
    self.next_syntax_id += 1;
    let syntax = Syntax::new(precs.0, precs.1, regex, sid.clone());
    Ok((syntax, DeclF::Syntax(sid, prec_str, defs, Box::new(e))))
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
        ExprF::Syntax(_, SyntaxId::AppE, args) => {
          use SyntaxElemF::*;
          // e0 e1
          if let [Expr(e0), Expr(e1)] = args.as_slice() {
            rev_params.push((Vis::Explicit, e1.clone()));
            e = *e0.clone();
            continue;
          } else {
            panic!();
          }
        }
        ExprF::Syntax(_, SyntaxId::AppI, args) => {
          use SyntaxElemF::*;
          // e0 { e1 }
          if let [Expr(e0), Token("{"), Expr(e1), Token("}")] = args.as_slice() {
            rev_params.push((Vis::Implicit, e1.clone()));
            e = *e0.clone();
            continue;
          } else {
            panic!();
          }
        }
        _ => {}
      }
      return Err((e.1.into(), "expected module application".to_string()));
    }
  }

  fn modref_internal(&mut self) -> Result<ModRef<'a>> {
    match self.tracker.peek_str() {
      None => return Err((self.tracker.epos(), "expected module reference".to_string())),
      Some("import") => {
        self.tracker.next();
        let s = match self.tracker.peek_ty_str() {
          Some((TokenType::String, s)) => {
            self.tracker.next();
            s
          }
          _ => return Err((self.tracker.epos(), "expected string literal".to_string())),
        };
        Ok(ModRefF::Import(s))
      }
      Some(_) => {
        let e = self.expr_or_hole(None);
        let (name, params) = self.extract_modref(e)?;
        Ok(ModRefF::App(name, params))
      }
    }
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
      ModRefF::App(is, _) => self.env.lookup(&is),
    }
  }

  fn decl_internal(&mut self, cur_mod: &mut Env<'a>) -> Result<DeclInternal<'a>> {
    match self.tracker.peek_str() {
      None => return Err((self.tracker.epos(), "expected declaration".to_string())),
      Some("local") => {
        let mut empty_env = Env::new();
        self.tracker.next();
        match self.tracker.peek_str() {
          None => {
            return Err((
              self.tracker.epos(),
              "expected declaration or block".to_string(),
            ))
          }
          Some("{") => {
            self.tracker.next();
            let ds = self.decls(&mut empty_env);
            self.expect_str("}")?;
            Ok(DeclF::Local(ds))
          }
          Some(_) => {
            let d = self.decl(&mut empty_env)?;
            Ok(DeclF::Local(vec![d]))
          }
        }
      }
      Some("module") => {
        let mod_indent = self.tracker.peek().unwrap().indent;
        self.tracker.next();
        let name = self.expect_id()?;
        let mut params: Vec<Arg<'a>> = Vec::new();
        loop {
          match self.tracker.peek_ty_str() {
            Some((_, "=")) => {
              self.tracker.next();
              break;
            }
            Some((_, "(")) | Some((_, "{")) => {
              let vis = if self.tracker.peek().unwrap().str == "(" {
                Vis::Explicit
              } else {
                Vis::Implicit
              };
              self.tracker.next();
              let ids = self
                .ids()
                .ok_or((self.tracker.epos(), "expected identifier list".to_string()))?;
              let match_cl = match vis {
                Vis::Explicit => ")",
                Vis::Implicit => "}",
              };
              let ty = match self.tracker.peek_str() {
                Some(":") => {
                  self.tracker.next();
                  let e = self.expr_or_hole(Some(match_cl));
                  Some(Box::new(e))
                }
                _ => None,
              };
              self.expect_str(match_cl)?;
              params.push((vis, ids, ty));
            }
            Some((TokenType::Ident, _)) => {
              // explicit argument with no type: rejected in the later pass.
              params.push((Vis::Explicit, vec![self.expect_id().unwrap()], None));
            }
            _ => return self.err_here("expected module parameters or body"),
          }
        }
        let md = ModDefF { name, params };
        let (mb, env) = match self.tracker.peek_str() {
          None => return self.err_here("expected module body"),
          Some("{") => {
            // declarations
            self.tracker.next();
            let env = self.env.clone();
            let mut mod_env = Env::new();
            let ds = self.decls(&mut mod_env);
            self.env = env;
            self.expect_str("}")?;
            (ModBody::Decls(ds), Rc::new(mod_env))
          }
          Some(_) => {
            // module reference
            let pos = self.tracker.epos();
            let mr = self.modref(mod_indent)?;
            let e = match self.resolve_modref(&mr) {
              Some(e) => e.clone(),
              None => return Err((pos, "module not found".to_string())),
            };
            (ModBody::Ref(mr), e)
          }
        };
        let name = md.name.clone();
        cur_mod.add_module(name.clone(), env.clone());
        self.env.add_module(name, env);
        Ok(DeclF::Mod(md, mb))
      }
      Some("open") | Some("use") => {
        // use == local open
        let is_open = self.tracker.peek().unwrap().str == "open";
        let mod_indent = self.tracker.peek().unwrap().indent;
        self.tracker.next();
        let pos = self.tracker.epos();
        let mr = self.modref(mod_indent)?;
        let e = match self.resolve_modref(&mr) {
          Some(e) => e.clone(),
          None => return Err((pos, "module not found".to_string())),
        };
        if is_open {
          self.env.merge(e.clone());
        }
        cur_mod.merge(e);
        Ok(if is_open {
          DeclF::Open(mr)
        } else {
          DeclF::Use(mr)
        })
      }
      Some("syntax") => {
        let syn_indent = self.tracker.peek().unwrap().indent;
        let outer_indent = self.tracker.save_indent();
        self.tracker.next();
        self.tracker.set_indent(syn_indent);
        let res = self.syntax();
        self.tracker.restore_indent(outer_indent);
        let (syntax, d) = res?;
        cur_mod.add_syntax(syntax.clone());
        self.env.add_syntax(syntax);
        Ok(d)
      }
      Some(_) => {
        // definition
        let def_indent = self.tracker.peek().unwrap().indent;
        let outer_indent = self.tracker.save_indent();
        let id = self.expect_id()?;
        self.tracker.set_indent(def_indent);
        let def = self.def_rest(id);
        self.tracker.restore_indent(outer_indent);
        Ok(DeclF::Def(def?))
      }
    }
  }

  fn decl(&mut self, cur_mod: &mut Env<'a>) -> Result<Decl<'a>> {
    let begin = self.tracker.pos();
    let res = self.decl_internal(cur_mod)?;
    let range = self.tracker.range_from(begin);
    self.allow_semicolon();
    Ok(Decl(res, range))
  }

  pub fn decls(&mut self, cur_mod: &mut Env<'a>) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    loop {
      match self.tracker.peek() {
        None => break,
        Some(t) => {
          if self.known_ops.contains(t.str) || t.ty != TokenType::Ident {
            break;
          }
        }
      }
      let orig_loc = self.tracker.get_location();
      match self.decl(cur_mod) {
        Ok(d) => ds.push(d),
        Err((pos, msg)) => {
          self.add_error(pos, &msg);
          self.tracker.skip_to_next_head();
        }
      }
      let cur_loc = self.tracker.get_location();
      if orig_loc == cur_loc {
        break;
      }
    }
    return ds;
  }

  pub fn program(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    let mut cur_mod = Env::new();
    loop {
      let orig_loc = self.tracker.get_location();
      match self.tracker.peek() {
        Some(_) => ds.extend(self.decls(&mut cur_mod)),
        None => break,
      }
      let cur_loc = self.tracker.get_location();
      if orig_loc == cur_loc {
        self.add_error(self.tracker.epos(), "expected declaration (skipped)");
        self.tracker.next();
        self.tracker.skip_to_next_head();
      }
    }
    assert!(self.tracker.is_done());
    ds
  }
}
