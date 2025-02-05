use crate::expr::ExprParser;
use crate::types::parse::*;
use std::collections::HashSet;

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

  pub fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, decl: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn expect_reserved(&mut self, str: &str) -> Result<(), ()> {
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == str => {
        self.tracker.next();
        return Ok(());
      }
      _ => {
        let pos = self.tracker.pos();
        self.add_error(pos, &format!("expected '{}'", str));
        self.tracker.skip_to_next_head();
        return Err(());
      }
    }
  }

  fn peek_is(&mut self, ty: TokenType) -> Result<(), ()> {
    match self.tracker.peek() {
      Some(t) if t.ty == ty => {
        return Ok(());
      }
      _ => {
        let pos = self.tracker.pos();
        self.add_error(
          pos,
          match ty {
            TokenType::Ident => "expected identifier",
            TokenType::String => "expected string",
            _ => unimplemented!(),
          },
        );
        self.tracker.skip_to_next_head();
        return Err(());
      }
    }
  }

  fn expect_id(&mut self) -> Result<Id<'a>, ()> {
    self.peek_is(TokenType::Ident)?;
    let s = self.tracker.peek().unwrap().str;
    self.tracker.next();
    Ok(Id::new(s))
  }

  fn ids(&mut self) -> Vec<Id<'a>> {
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
      let last_pos = self.tracker.pos();
      self.add_error(last_pos, "expected identifier list");
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

  fn def_rest(&mut self, name: Id<'a>) -> Result<Def<'a>, ()> {
    let mut args = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          let pos = self.tracker.end_of_line();
          self.add_error(pos, "expected definition");
          self.tracker.skip_to_next_head();
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
          let mut hasType = false;
          self.tracker.next();
          let ids = self.ids();
          let arg = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
              hasType = true;
              self.tracker.next();
              let e = self.expr_or_hole();
              (vis, ids, Some(Box::new(e)))
            }
            _ => (vis, ids, None),
          };
          let (pos, cl) = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && (t.str == ")" || t.str == "}") => {
              let pos = t.pos;
              let str = t.str;
              self.tracker.next();
              (pos, str)
            }
            _ => {
              let msg = if hasType {
                "expected ')' or '}'"
              } else {
                "expected ')', '}' or ':'"
              };
              let pos = self.tracker.pos();
              self.add_error(pos, msg);
              self.tracker.skip_to_next_head();
              return Err(());
            }
          };
          let match_cl = match vis {
            Vis::Explicit => ")",
            Vis::Implicit => "}",
          };
          if cl != match_cl {
            self.add_error(pos, &format!("expected '{}', not '{}'", match_cl, cl));
          }
          args.push(arg);
        } else {
          let pos = self.tracker.pos();
          self.add_error(pos, "expected argument list or assignment");
          self.tracker.skip_to_next_head();
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
        let e = self.expr_or_hole();
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_reserved("=")?;
    let body = self.expr_or_hole();
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ";" => {
        self.tracker.next();
      }
      _ => {}
    }
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
    eprintln!("EXPR INTO: {:?}", tracker.peek());
    eprintln!("SCOPE: {:?}", self.scope.modules);
    let mut e = ExprParser::new(tracker, &self.scope, known_ops, errors);
    let res = e.expr();
    eprintln!("EXPR RES: {:?}", res);
    let (tracker, known_ops, errors) = e.into_inner();
    self.tracker = tracker;
    self.known_ops = known_ops;
    self.errors = errors;
    res
  }

  fn expr_or_hole(&mut self) -> Expr<'a> {
    let pos = self.tracker.pos();
    match self.expr() {
      Some(e) => e,
      None => {
        // TODO: skip to...?
        Expr(ExprF::Hole, pos)
      }
    }
  }

  fn syntax(&mut self) -> Result<Syntax, ()> {
    // syntax (w/o "syntax")
    let precs = match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Precedence => match Precedence::parse(t.str) {
        Some(precs) => {
          self.tracker.next();
          precs
        }
        None => {
          let pos = self.tracker.pos();
          self.add_error(pos, "failed to parse precedence");
          self.tracker.skip_to_next_head();
          return Err(());
        }
      },
      _ => (Precedence::Terminal, Precedence::Terminal),
    };
    let regex = unimplemented!();
    let syntax = Syntax::new(precs.0, precs.1, regex);
    return Ok(syntax);
  }

  fn modref_internal(&mut self) -> Result<ModRef<'a>, ()> {
    let t = match self.tracker.peek() {
      Some(t) => t.clone(),
      None => {
        let pos = self.tracker.end_of_line();
        self.add_error(pos, "expected module reference");
        self.tracker.skip_to_next_head();
        return Err(());
      }
    };
    if t.ty != TokenType::Ident {
      self.add_error(t.pos, "expected module reference");
      self.tracker.skip_to_next_head();
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

    // let e = self.expr_or_hole();
    // println!("{:?}", e);
    // unimplemented!()

    let mut name = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          let pos = self.tracker.end_of_line();
          self.add_error(pos, "expected module name");
          self.tracker.skip_to_next_head();
          return Err(());
        }
      };
      if t.ty == TokenType::Reserved {
        break;
      }
      if t.ty != TokenType::Ident {
        let pos = self.tracker.pos();
        self.add_error(pos, "expected module name");
        self.tracker.skip_to_next_head();
        return Err(());
      }
      name.push(Id::new(t.str));
      self.tracker.next();
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          break;
        }
      };
      if t.ty == TokenType::Reserved && t.str == "." {
        self.tracker.next();
      } else {
        break;
      }
    }
    let mut params = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          break;
        }
      };
      if t.ty == TokenType::Reserved && t.str == "{" {
        self.tracker.next();
        let e = self.expr_or_hole();
        self.expect_reserved("}")?;
        params.push((Vis::Implicit, Box::new(e)));
      } else if t.ty == TokenType::Reserved && t.str == "(" {
        self.tracker.next();
        let e = self.expr_or_hole();
        self.expect_reserved(")")?;
        params.push((Vis::Explicit, Box::new(e)));
      } else if t.ty == TokenType::Ident {
        let mut ids = Vec::new();
        loop {
          match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Ident => {
              ids.push(Id::new(t.str));
              self.tracker.next();
            }
            _ => break,
          }
          match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && t.str == "." => {
              self.tracker.next();
            }
            _ => {
              break;
            }
          };
        }
        let name = ids.pop().unwrap();
        let e = if ids.is_empty() {
          ExprF::Var(name)
        } else {
          ExprF::Ext(ids, name)
        };
        params.push((Vis::Explicit, Box::new(Expr(e, t.pos))));
      } else {
        let pos = self.tracker.pos();
        self.add_error(pos, "expected module parameters");
        self.tracker.skip_to_next_head();
        return Err(());
      }
    }
    return Ok(ModRefF::App(name, params));
  }

  fn modref(&mut self, indent: Indent) -> Result<ModRef<'a>, ()> {
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

  fn decl(&mut self, cur_mod: &mut Env<'a>) -> Result<Decl<'a>, ()> {
    let t = match self.tracker.peek() {
      Some(t) => t.clone(),
      None => {
        let pos = self.tracker.end_of_line();
        self.add_error(pos, "expected declaration");
        self.tracker.skip_to_next_head();
        return Err(());
      }
    };
    if t.ty != TokenType::Ident {
      let pos = self.tracker.pos();
      self.add_error(pos, "expected declaration");
      self.tracker.skip_to_next_head();
      return Err(());
    }
    if t.str == "local" {
      // local
      self.tracker.next();
      let t2 = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          self.add_error(t.pos, "expected declaration or block");
          self.tracker.skip_to_next_head();
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
            let pos = self.tracker.end_of_line();
            self.add_error(pos, "expected module parameters or body");
            self.tracker.skip_to_next_head();
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
            let e = self.expr_or_hole();
            let arg = (vis, ids, Some(Box::new(e)));
            let (pos, cl) = match self.tracker.peek() {
              Some(t) if t.ty == TokenType::Reserved && (t.str == ")" || t.str == "}") => {
                let pos = t.pos;
                let str = t.str;
                self.tracker.next();
                (pos, str)
              }
              _ => {
                let pos = self.tracker.pos();
                self.add_error(pos, "expected ')' or '}'");
                self.tracker.skip_to_next_head();
                return Err(());
              }
            };
            let match_cl = match vis {
              Vis::Explicit => ")",
              Vis::Implicit => "}",
            };
            if cl != match_cl {
              self.add_error(pos, &format!("expected '{}', not '{}'", match_cl, cl));
            }
            params.push(arg);
          } else {
            let pos = self.tracker.pos();
            self.add_error(pos, "expected module parameters or body");
            self.tracker.skip_to_next_head();
            return Err(());
          }
        } else {
          let pos = self.tracker.pos();
          self.add_error(pos, "expected module parameters or body");
          self.tracker.skip_to_next_head();
          return Err(());
        }
      }
      let mod_decl = ModDeclF { name, params };
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          let pos = self.tracker.pos();
          self.add_error(pos, "expected module body");
          self.tracker.skip_to_next_head();
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
          self.tracker.skip_to_next_head();
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
      let syntax = self.syntax();
      self.tracker.restore_indent(outer_indent);
      let syntax = syntax?;
      cur_mod.add_syntax(syntax.clone());
      self.scope.add_syntax(syntax.clone());
      return Ok(Decl(DeclF::Syntax(syntax), t.pos));
    }

    // general identifier: definition
    let def = self.def().ok_or(())?;
    return Ok(Decl(DeclF::Def(def), t.pos));
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
      if let Ok(d) = self.decl(cur_mod) {
        ds.push(d);
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
