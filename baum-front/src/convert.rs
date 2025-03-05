use crate::types::tree::*;
use baum_core::types::tree as core;
use std::collections::HashMap;
use std::rc::Rc;

struct Env {
  defs: HashMap<Id, core::DefId>,
  binds: HashMap<Id, core::BindId>,
}

impl Env {
  fn new() -> Self {
    Self {
      defs: HashMap::new(),
      binds: HashMap::new(),
    }
  }
}

struct Builder {
  symbols: HashMap<Id, String>,
  envs: Vec<Env>,
  names_id: HashMap<Id, core::NameId>,
  names_ix: HashMap<u8, core::NameId>,
  def_symbols: HashMap<core::DefId, String>,
  bind_symbols: HashMap<core::BindId, String>,
  name_symbols: HashMap<core::NameId, String>,
  next_def_id: u32,
  next_bind_id: u32,
  next_name_id: u32,
  errors: Vec<String>,
}

impl Builder {
  fn new(symbols: HashMap<Id, String>) -> Self {
    Self {
      symbols,
      envs: vec![Env::new()],
      names_id: HashMap::new(),
      names_ix: HashMap::new(),
      def_symbols: HashMap::new(),
      bind_symbols: HashMap::new(),
      name_symbols: HashMap::new(),
      next_def_id: 0,
      next_bind_id: 0,
      next_name_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_def(&mut self, i: &Id) -> core::DefId {
    let id = core::DefId(self.next_def_id);
    self.next_def_id += 1;
    self.envs.last_mut().unwrap().defs.insert(i.clone(), id);
    if let Some(s) = self.symbols.get(i) {
      self.def_symbols.insert(id, s.clone());
    }
    id
  }

  fn add_bind(&mut self, i: &Id) -> core::BindId {
    let id = core::BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self.envs.last_mut().unwrap().binds.insert(i.clone(), id);
    if let Some(s) = self.symbols.get(i) {
      self.bind_symbols.insert(id, s.clone());
    }
    id
  }

  fn lookup_bind(&mut self, i: &Id) -> Option<core::BindId> {
    unimplemented!()
  }

  fn name_from_id(&mut self, i: &Id) -> core::NameId {
    if let Some(i) = self.names_id.get(i) {
      return *i;
    } else {
      let id = core::NameId(self.next_name_id);
      self.next_name_id += 1;
      self.names_id.insert(i.clone(), id);
      if let Some(s) = self.symbols.get(i) {
        self.name_symbols.insert(id, s.clone());
      }
      id
    }
  }

  fn name_from_index(&mut self, i: &u8) -> core::NameId {
    if let Some(i) = self.names_ix.get(i) {
      return *i;
    } else {
      let id = core::NameId(self.next_name_id);
      self.next_name_id += 1;
      self.names_ix.insert(*i, id);
      self.name_symbols.insert(id, i.to_string());
      id
    }
  }

  fn e(&mut self, e: &Expr) -> core::Expr {
    use ExprF::*;
    core::Expr(match &e.0 {
      Hole => core::ExprF::Hole,
      Bind(i) => match self.lookup_bind(i) {
        Some(i) => core::ExprF::Bind(i),
        None => core::ExprF::Hole,
      },
      Ann(e, ty) => core::ExprF::Ann(Rc::new(self.e(&e)), Rc::new(self.e(&ty))),
      Uni => core::ExprF::Uni,

      Ext(l, mod_name, i) => unimplemented!(),
      Let(ds, e) => {
        self.envs.push(Env::new());
        let defs = self.ds(ds);
        let e = Rc::new(self.e(e));
        self.envs.pop();
        core::ExprF::Let(defs, e)
      }
      Lit(l) => unimplemented!(),

      PiE(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = i.map(|i| self.add_bind(&i));
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, i, ty, e)
      }
      LamE(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = self.add_bind(i);
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, i, ty, e)
      }
      AppE(e1, e2) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
        };
        let e1 = Rc::new(self.e(&e1));
        let e2 = Rc::new(self.e(&e2));
        core::ExprF::App(tag, e1, e2)
      }

      PiI(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = i.map(|i| self.add_bind(&i));
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, i, ty, e)
      }
      LamI(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = self.add_bind(i);
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, i, ty, e)
      }
      AppI(e1, e2) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
        };
        let e1 = Rc::new(self.e(&e1));
        let e2 = Rc::new(self.e(&e2));
        core::ExprF::App(tag, e1, e2)
      }

      TupleTy(elems) => {
        let tag = core::STag { is_tuple: true };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (ix, (bind, e)) in elems.iter().enumerate() {
          let ix = ix as u8;
          let name = self.name_from_index(&ix);
          let bind = bind.map(|i| self.add_bind(&i));
          let e = Rc::new(self.e(&e));
          es.push((name, bind, e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      TupleCon(elems) => {
        let tag = core::STag { is_tuple: true };
        let mut es = Vec::new();
        for (ix, e) in elems.iter().enumerate() {
          let ix = ix as u8;
          let name = self.name_from_index(&ix);
          let e = Rc::new(self.e(&e));
          es.push((name, e));
        }
        core::ExprF::Obj(tag, es)
      }
      Proj(i, e) => {
        let tag = core::STag { is_tuple: true };
        let i = self.name_from_index(i);
        let e = Rc::new(self.e(&e));
        core::ExprF::Prop(tag, i, e)
      }

      ObjTy(elems) => {
        let tag = core::STag { is_tuple: false };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let bind = self.add_bind(i);
          let e = Rc::new(self.e(&e));
          es.push((name, Some(bind), e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      ObjCon(elems) => {
        let tag = core::STag { is_tuple: false };
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let e = Rc::new(self.e(&e));
          es.push((name, e));
        }
        core::ExprF::Obj(tag, es)
      }
      Prop(i, e) => {
        let tag = core::STag { is_tuple: false };
        let name = self.name_from_id(i);
        let e = Rc::new(self.e(&e));
        core::ExprF::Prop(tag, name, e)
      }
    })
  }

  fn d(&mut self, d: &Decl, defs: &mut Vec<(core::DefId, Rc<core::Expr>)>) {
    match d {
      Decl::Mod(name, params, body) => {
        unimplemented!()
      }
      Decl::Def(i, e) => {
        let i = self.add_def(&i);
        let e = self.e(&e);
        defs.push((i, Rc::new(e)));
      }
    }
  }

  fn ds(&mut self, ds: &Vec<Decl>) -> Vec<(core::DefId, Rc<core::Expr>)> {
    let mut defs = Vec::new();
    for d in ds {
      self.d(d, &mut defs);
    }
    defs
  }
}

pub fn convert(p: Program) -> (core::Program, Vec<String>) {
  let mut b = Builder::new(p.symbols);
  let defs = b.ds(&p.decls);
  (
    core::Program {
      defs,
      def_symbols: b.def_symbols,
      bind_symbols: b.bind_symbols,
      name_symbols: b.name_symbols,
    },
    b.errors,
  )
}
