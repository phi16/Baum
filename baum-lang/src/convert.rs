use crate::types::syntax::{FrontElem, SyntaxHandler};
use crate::types::tree::*;
use crate::types::tree_base::*;
use baum_front::types::tree as front;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
enum Entity<'a> {
  Def(front::Id),
  Mod(front::Id, Env<'a>),
}

#[derive(Debug, Clone)]
struct Env<'a> {
  lookup: HashMap<Id<'a>, Entity<'a>>,
}

impl<'a> Env<'a> {
  fn new() -> Self {
    Self {
      lookup: HashMap::new(),
    }
  }
}

struct Builder<'a> {
  mod_name: Vec<Id<'a>>,
  envs: Vec<Env<'a>>,
  next_id: u32,
  syntax: HashMap<SyntaxId, SyntaxHandler<'a>>,
  symbols: HashMap<front::Id, String>,
}

impl<'a> Builder<'a> {
  fn new(syntax: HashMap<SyntaxId, SyntaxHandler<'a>>) -> Self {
    Self {
      mod_name: Vec::new(),
      envs: vec![Env::new()],
      next_id: 0,
      syntax,
      symbols: HashMap::new(),
    }
  }

  fn here(&mut self) -> &mut HashMap<Id<'a>, Entity<'a>> {
    &mut self.envs.last_mut().unwrap().lookup
  }

  fn new_id(&mut self, id: &Id<'a>) -> front::Id {
    let i = front::Id(self.next_id);
    self.next_id += 1;
    self.symbols.insert(i, id.as_str().to_string());
    i
  }

  fn fresh_id(&mut self) -> front::Id {
    let i = front::Id(self.next_id);
    self.next_id += 1;
    i
  }

  fn lookup_mod(&self, n: &Vec<Id<'a>>) -> Option<(Vec<front::Id>, Env<'a>)> {
    'k: for env in self.envs.iter().rev() {
      let mut cur_env = env;
      let mut mod_name = Vec::new();
      for name in n {
        match cur_env.lookup.get(name) {
          Some(Entity::Mod(i, env)) => {
            mod_name.push(*i);
            cur_env = env;
          }
          _ => continue 'k,
        }
      }
      return Some((mod_name, cur_env.clone()));
    }
    None
  }

  fn lookup_id(&self, i: &Id<'a>, env: &Env<'a>) -> Option<front::Id> {
    match env.lookup.get(i) {
      Some(Entity::Def(i)) => Some(*i),
      _ => None,
    }
  }

  fn syntax_elem(&mut self, se: &SyntaxElem<'a>) -> FrontElem<'a> {
    match se {
      SyntaxElem::Token(s) => FrontElem::Token(s),
      SyntaxElem::Ident(i) => {
        let i = self.new_id(i);
        FrontElem::Ident(i)
      }
      SyntaxElem::Nat(s) => FrontElem::Nat(s),
      SyntaxElem::Rat(s) => FrontElem::Rat(s),
      SyntaxElem::Chr(s) => FrontElem::Chr(s),
      SyntaxElem::Str(s) => FrontElem::Str(s),
      SyntaxElem::Expr(e) => FrontElem::Expr(self.e(e)), // TODO: Fix
    }
  }

  fn e_internal(&mut self, e: &Expr<'a>) -> Result<front::Expr> {
    match &e.0 {
      ExprF::Hole => Ok(front::Expr(front::ExprF::Hole)),
      ExprF::Var(i) => {
        for env in self.envs.iter().rev() {
          if let Some(Entity::Def(i)) = env.lookup.get(i) {
            return Ok(front::Expr(front::ExprF::Var(*i)));
          }
        }
        Err(format!("symbol not found: {:?}", i))
      }
      ExprF::Mod(_) => Err("module reference is not allowed in expression".to_string()),
      ExprF::Ext(mod_name, i) => {
        let (m, env) = self
          .lookup_mod(mod_name)
          .ok_or(format!("module not found: {:?}.{:?}", mod_name, i))?;
        let i = self
          .lookup_id(i, &env)
          .ok_or(format!("symbol not found: {:?}.{:?}", mod_name, i))?;
        Ok(front::Expr(front::ExprF::Ext(m, i.clone())))
      }
      ExprF::Let(ds, e) => {
        self.envs.push(Env::new());
        let mut decls = Vec::new();
        self.ds(ds, &mut Env::new(), &mut decls);
        let e = Rc::new(self.e(e));
        self.envs.pop();
        Ok(front::Expr(front::ExprF::Let(decls, e)))
      }
      ExprF::Syntax(mod_name, sid, elems) => {
        let es = elems.iter().map(|e| self.syntax_elem(e)).collect();
        let handler = self
          .syntax
          .get(sid)
          .ok_or(format!("syntax handler not found: {:?}", sid))?;
        handler(es)
      }
    }
  }

  fn e(&mut self, e: &Expr<'a>) -> front::Expr {
    match self.e_internal(e) {
      Ok(e) => e,
      Err(msg) => {
        eprintln!("{}", msg);
        front::Expr(front::ExprF::Hole)
      }
    }
  }

  fn define_mod(
    &mut self,
    m_params: &Vec<(Vis, Vec<Id<'a>>, Option<Box<Expr<'a>>>)>,
    m_body: &ModBody<'a>,
  ) -> (
    Vec<(Vis, front::Id, Rc<front::Expr>)>,
    front::ModBody,
    Env<'a>,
  ) {
    self.envs.push(Env::new());
    let mut param_names = Vec::new();
    let mut params = Vec::new();
    for (vis, names, ty) in m_params {
      let ty = if let Some(ty) = ty {
        self.e(&ty)
      } else {
        front::Expr(front::ExprF::Hole)
      };
      let ty = Rc::new(ty);
      for name in names {
        let i = self.new_id(name);
        self.here().insert(name.clone(), Entity::Def(i));
        param_names.push(name);
        params.push((vis.clone(), i, ty.clone()));
      }
    }
    let mut mod_env = Env::new();
    let mut mod_decls = Vec::new();
    let mod_body = match m_body {
      ModBodyF::Decls(ds) => {
        // overwrite the module parameter name references to actual definitions
        for ((_, i, _), name) in params.iter().zip(param_names.into_iter()) {
          let j = self.new_id(name);
          let def_i = front::Expr(front::ExprF::Var(i.clone()));
          mod_decls.push(front::Decl::Def(j, def_i));
          self.here().insert(name.clone(), Entity::Def(j));
        }
        self.ds(ds, &mut mod_env, &mut mod_decls);
        front::ModBody::Decls(mod_decls)
      }
      ModBodyF::Ref(ModRefF::App(m, m_args)) => {
        if let Some((m, env)) = self.lookup_mod(&m) {
          mod_env = env.clone();
          let mut args = Vec::new();
          for (vis, e) in m_args {
            args.push((vis.clone(), self.e(e)));
          }
          front::ModBody::App(m, args)
        } else {
          eprintln!("module not found: {:?}", m);
          front::ModBody::Decls(Vec::new())
        }
      }
      ModBodyF::Ref(ModRefF::Import(_)) => unimplemented!(),
    };
    self.envs.pop();
    (params, mod_body, mod_env)
  }

  fn d(&mut self, d: &Decl<'a>, cur_mod: &mut Env<'a>, decls: &mut Vec<front::Decl>) {
    match &d.0 {
      DeclF::Local(ds) => {
        let mut empty_env = Env::new();
        self.ds(ds, &mut empty_env, decls);
      }
      DeclF::Mod(md, mb) => {
        self.mod_name.push(md.name.clone());
        let (params, body, env) = self.define_mod(&md.params, mb);
        self.mod_name.pop();
        let mod_id = self.new_id(&md.name);
        decls.push(front::Decl::Mod(mod_id, params, body));
        self
          .here()
          .insert(md.name.clone(), Entity::Mod(mod_id, env.clone())); // uh
        cur_mod
          .lookup
          .insert(md.name.clone(), Entity::Mod(mod_id, env));
      }
      DeclF::Open(mr) => {
        // define a dummy module to consume m_args
        let mb = ModBody::Ref(mr.clone());
        let (params, body, env) = self.define_mod(&Vec::new(), &mb);
        let mod_id = self.fresh_id();
        decls.push(front::Decl::Mod(mod_id, params, body));

        // inserts all entities from the dummy module to current module
        env.lookup.into_iter().for_each(|(name, entity)| {
          decls.push(match entity {
            Entity::Def(i) => front::Decl::Def(i, front::Expr(front::ExprF::Ext(vec![mod_id], i))),
            Entity::Mod(i, _) => {
              let mn = vec![mod_id, i];
              front::Decl::Mod(i, Vec::new(), front::ModBody::App(mn, Vec::new()))
            }
          });
          self.here().insert(name, entity);
        });
      }
      DeclF::Use(mr) => {
        let d = Decl(
          DeclF::Local(vec![Decl(DeclF::Open(mr.clone()), d.1.clone())]),
          d.1.clone(),
        );
        self.d(&d, cur_mod, decls);
      }
      DeclF::Def(def) => {
        self.envs.push(Env::new());
        let mut conts = Vec::new();
        for (vis, names, ty) in &def.args {
          let ty = if let Some(ty) = ty {
            self.e(&ty)
          } else {
            front::Expr(front::ExprF::Hole)
          };
          let ty = Rc::new(ty);
          for name in names {
            let i = self.new_id(name);
            self.here().insert(name.clone(), Entity::Def(i));
            conts.push((vis.clone(), i, ty.clone()));
          }
        }
        let e = self.e(&def.body);
        let e = if let Some(ty) = &def.ty {
          let ty = self.e(&ty);
          front::Expr(front::ExprF::Ann(Rc::new(e), Rc::new(ty)))
        } else {
          e
        };
        let mut e = e;
        for (vis, i, ty) in conts.into_iter().rev() {
          match vis {
            Vis::Explicit => e = front::Expr(front::ExprF::LamE(i, ty, Rc::new(e))),
            Vis::Implicit => e = front::Expr(front::ExprF::LamI(i, ty, Rc::new(e))),
          }
        }
        self.envs.pop();
        let i = self.new_id(&def.name);
        decls.push(front::Decl::Def(i, e));
        self.here().insert(def.name.clone(), Entity::Def(i));
        cur_mod.lookup.insert(def.name.clone(), Entity::Def(i));
      }
      DeclF::Syntax(sid, _, syndefs, e) => {
        eprintln!("{:?} {:?} {:?}", sid, syndefs, e);
        let handler = |elems| {
          eprintln!("{:?}", elems);
          unimplemented!();
        };
        self.syntax.insert(sid.clone(), Box::new(handler));
      }
    }
  }

  fn ds(&mut self, ds: &Vec<Decl<'a>>, cur_mod: &mut Env<'a>, decls: &mut Vec<front::Decl>) {
    for d in ds {
      self.d(d, cur_mod, decls);
    }
  }
}

pub fn convert<'a>(
  ds: &Vec<Decl<'a>>,
  syntax_handlers: HashMap<SyntaxId, SyntaxHandler<'a>>,
) -> front::Program {
  let mut b = Builder::new(syntax_handlers);
  let mut cur_mod = Env::new();
  let mut decls = Vec::new();
  b.ds(ds, &mut cur_mod, &mut decls);
  let symbols = b.symbols;
  front::Program { decls, symbols }
}
