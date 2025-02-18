use baum_front::types::tree::ModDef;

use crate::types::env::{Env, Syntax};
use crate::types::token::{Indent, TokenPos, TokenType};
use crate::types::tracker::Tracker;
use crate::types::tree::*;
use crate::types::tree_base::*;
use std::collections::HashSet;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

pub struct DeclParser<'a> {
  tracker: Tracker<'a>,
  env: Env<'a>,
  known_ops: HashSet<String>,
  errors: Vec<String>,
}

impl<'a> DeclParser<'a> {
  pub fn new(
    tracker: Tracker<'a>,
    env: Env<'a>,
    known_ops: HashSet<String>,
    errors: Vec<String>,
  ) -> Self {
    DeclParser {
      tracker,
      env,
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

  fn expect_str(&mut self, s: &str) -> Result<()> {
    match self.tracker.peek_str() {
      Some(t) if t == s => {
        self.tracker.next();
        Ok(())
      }
      _ => Err(format!("expected '{}'", s)),
    }
  }

  fn expect_id(&mut self) -> Result<Id<'a>> {
    match self.tracker.peek_ty() {
      Some(TokenType::Ident) => {
        let s = self.tracker.peek().unwrap().str;
        Ok(Id::new(s))
      }
      _ => Err("expected identifier".to_string()),
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

  fn ids(&mut self) -> Vec<Id<'a>> {
    let mut ids = Vec::new();
    loop {
      match self.tracker.peek_ty() {
        Some(TokenType::Ident) => {
          let s = self.tracker.peek().unwrap().str;
          ids.push(Id::new(s));
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

  fn def(&mut self) -> Result<Def<'a>> {
    unimplemented!()
  }

  fn expr_or_hole(&mut self, wait_next: Option<&str>) -> Expr<'a> {
    unimplemented!()
  }

  fn syntax(&mut self) -> Result<(Syntax, DeclInternal<'a>)> {
    unimplemented!()
  }

  fn modref_internal(&mut self) -> Result<ModRef<'a>> {
    unimplemented!()
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
      None => return Err("expected declaration".to_string()),
      Some("local") => {
        let mut empty_env = Env::new();
        self.tracker.next();
        match self.tracker.peek_str() {
          None => return Err("expected declaration or block".to_string()),
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
          match self.tracker.peek_str() {
            Some("=") => {
              self.tracker.next();
              break;
            }
            Some("(") | Some("{") => {
              let vis = if self.tracker.peek().unwrap().str == "(" {
                Vis::Explicit
              } else {
                Vis::Implicit
              };
              self.tracker.next();
              let ids = self.ids();
              self.expect_str(":")?;
              let match_cl = match vis {
                Vis::Explicit => ")",
                Vis::Implicit => "}",
              };
              let e = self.expr_or_hole(Some(match_cl));
              self.expect_str(match_cl)?;
              params.push((vis, ids, Some(Box::new(e))));
            }
            _ => return Err("expected module parameters or body".to_string()),
          }
        }
        let md = ModDefF { name, params };
        let (mb, env) = match self.tracker.peek_str() {
          None => return Err("expected module body".to_string()),
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
            let mr = self.modref(mod_indent)?;
            let e = match self.resolve_modref(&mr) {
              Some(e) => e.clone(),
              None => return Err("module not found".to_string()),
            };
            (ModBody::Ref(mr), e)
          }
        };
        let name = md.name.clone();
        cur_mod.add_module(name.clone(), env.clone());
        self.env.add_module(name, env);
        return Ok(DeclF::Mod(md, mb));
      }
      Some("open") | Some("use") => {
        // use == local open
        let is_open = self.tracker.peek().unwrap().str == "open";
        let mod_indent = self.tracker.peek().unwrap().indent;
        self.tracker.next();
        let mr = self.modref(mod_indent)?;
        let e = match self.resolve_modref(&mr) {
          Some(e) => e.clone(),
          None => return Err("module not found".to_string()),
        };
        if is_open {
          let _ = self.env.merge(e.clone());
        }
        if cur_mod.merge(e).is_err() {
          self.add_error(self.tracker.pos(), "module name conflict");
        }
        return Ok(if is_open {
          DeclF::Open(mr)
        } else {
          DeclF::Use(mr)
        });
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
        return Ok(d);
      }
      Some(_) => {
        let def = self.def()?;
        return Ok(DeclF::Def(def));
      }
    }
  }

  fn decl(&mut self, cur_mod: &mut Env<'a>) -> Result<Decl<'a>> {
    let begin = self.tracker.pos();
    let res = self.decl_internal(cur_mod)?;
    let end = self.tracker.pos();
    self.allow_semicolon();
    Ok(Decl(res, TokenRange { begin, end }))
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
      let orig_pos = self.tracker.pos();
      match self.decl(cur_mod) {
        Ok(d) => ds.push(d),
        Err(e) => {
          self.add_error(self.tracker.pos(), &e);
          self.tracker.skip_to_next_head();
        }
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
    assert!(self.tracker.is_done());
    ds
  }
}
