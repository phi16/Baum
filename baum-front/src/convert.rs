use crate::types::tree::*;
use baum_core::types::tree as core;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PTag {
  pub is_mod_param: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct STag {
  pub is_tuple: bool,
  pub is_mod: bool,
}

impl baum_core::types::common::Tag for PTag {}
impl baum_core::types::common::Tag for STag {}

impl From<Vis> for core::Vis {
  fn from(v: Vis) -> Self {
    match v {
      Vis::Explicit => core::Vis::Explicit,
      Vis::Implicit => core::Vis::Implicit,
    }
  }
}

type CoreExpr<T> = core::Expr<T, PTag, STag>;

fn wrap<T>(e: core::ExprF<T, PTag, STag, Box<CoreExpr<T>>>) -> Box<CoreExpr<T>> {
  Box::new(core::Expr(e))
}

#[derive(Debug, Clone)]
enum Entity {
  Bind(core::BindId),
  Def(core::DefId),
  Mod(core::DefId),
}

#[derive(Debug, Clone)]
struct Env {
  lookup: HashMap<Id, Entity>,
}

impl Env {
  fn new() -> Self {
    Self {
      lookup: HashMap::new(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DefType {
  Def,
  Mod,
}

struct Def {
  name: Option<String>,
  ty: DefType,
}

struct Bind {
  name: Option<String>,
}

enum Name {
  Str(String),
  Ix(u8),
  Def(u32),
}

struct Decls<T> {
  defs: Vec<(core::DefId, T, Box<CoreExpr<T>>)>,
}

struct Builder<T: Clone> {
  symbols: HashMap<Id, String>,
  envs: Vec<Env>,
  names_id: HashMap<Id, core::NameId>,
  names_id_str: HashMap<String, core::NameId>,
  names_ix: HashMap<u8, core::NameId>,
  names_def: HashMap<u32, core::NameId>,
  def_symbols: HashMap<core::DefId, Def>,
  bind_symbols: HashMap<core::BindId, Bind>,
  name_symbols: HashMap<core::NameId, Name>,
  next_def_id: u32,
  next_bind_id: u32,
  next_name_id: u32,
  errors: Vec<(T, String)>,
}

impl<T: Clone> Builder<T> {
  fn new(symbols: HashMap<Id, String>) -> Self {
    Self {
      symbols,
      envs: vec![Env::new()],
      names_id: HashMap::new(),
      names_id_str: HashMap::new(),
      names_ix: HashMap::new(),
      names_def: HashMap::new(),
      def_symbols: HashMap::new(),
      bind_symbols: HashMap::new(),
      name_symbols: HashMap::new(),
      next_def_id: 0,
      next_bind_id: 0,
      next_name_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, t: &T, s: String) {
    self.errors.push((t.clone(), s));
  }

  fn add_bind(&mut self, i: &Id) -> core::BindId {
    let id = core::BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self
      .envs
      .last_mut()
      .unwrap()
      .lookup
      .insert(i.clone(), Entity::Bind(id));
    let b = Bind {
      name: self.symbols.get(i).cloned(),
    };
    self.bind_symbols.insert(id, b);
    id
  }

  fn fresh_bind(&mut self) -> core::BindId {
    let id = core::BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self.bind_symbols.insert(id, Bind { name: None });
    id
  }

  fn add_bind_maybe(&mut self, i: &Option<Id>) -> core::BindId {
    if let Some(i) = i {
      self.add_bind(i)
    } else {
      self.fresh_bind()
    }
  }

  fn add_def(&mut self, i: &Id, ty: DefType) -> core::DefId {
    let id = core::DefId(self.next_def_id);
    self.next_def_id += 1;
    self
      .envs
      .last_mut()
      .unwrap()
      .lookup
      .insert(i.clone(), Entity::Def(id));
    let d = Def {
      name: self.symbols.get(i).cloned(),
      ty,
    };
    self.def_symbols.insert(id, d);
    id
  }

  fn lookup(&mut self, i: &Id) -> Option<&Entity> {
    for env in self.envs.iter().rev() {
      if let Some(e) = env.lookup.get(i) {
        return Some(e);
      }
    }
    None
  }

  fn name_from_id(&mut self, i: &Id) -> core::NameId {
    if let Some(id) = self.names_id.get(i) {
      return *id;
    } else {
      let s = self.symbols.get(i).unwrap();
      if let Some(id) = self.names_id_str.get(s) {
        self.names_id.insert(i.clone(), *id);
        return *id;
      } else {
        let id = core::NameId(self.next_name_id);
        self.next_name_id += 1;
        self.names_id.insert(i.clone(), id);
        self.names_id_str.insert(s.clone(), id);
        self.name_symbols.insert(id, Name::Str(s.clone()));
        id
      }
    }
  }

  fn name_from_index(&mut self, i: &u8) -> core::NameId {
    if let Some(i) = self.names_ix.get(i) {
      return *i;
    } else {
      let id = core::NameId(self.next_name_id);
      self.next_name_id += 1;
      self.names_ix.insert(*i, id);
      self.name_symbols.insert(id, Name::Ix(*i));
      id
    }
  }

  fn name_from_def(&mut self, i: &Id) -> core::NameId {
    if let Some(i) = self.names_def.get(&i.0) {
      return *i;
    } else {
      let id = core::NameId(self.next_name_id);
      self.next_name_id += 1;
      self.names_def.insert(i.0, id);
      self.name_symbols.insert(id, Name::Def(i.0));
      id
    }
  }

  fn e(&mut self, e: &Expr<T>) -> CoreExpr<T> {
    use ExprF::*;
    core::Expr(match &e.0 {
      Hole => core::ExprF::Hole,
      Bind(i) => match self.lookup(i) {
        Some(Entity::Bind(i)) => core::ExprF::Bind(*i),
        Some(Entity::Def(i)) => core::ExprF::Def(*i),
        Some(Entity::Mod(_)) => {
          self.add_error(&e.1, format!("expected bind/def, found module"));
          core::ExprF::Hole
        }
        None => {
          // TODO: definition?
          self.add_error(&e.1, format!("unbound identifier: {:?}", i));
          core::ExprF::Hole
        }
      },
      Ann(e, ty) => core::ExprF::Ann(Box::new(self.e(&e)), Box::new(self.e(&ty))),
      Uni => core::ExprF::Uni,
      Wrap(e) => return self.e(&e),

      Def(_, mod_name, i) => {
        if mod_name.is_empty() {
          match self.lookup(i) {
            Some(Entity::Bind(i)) => core::ExprF::Bind(*i),
            Some(Entity::Def(i)) => core::ExprF::Def(*i),
            Some(Entity::Mod(_)) => {
              self.add_error(&e.1, format!("expected bind/def, found module"));
              core::ExprF::Hole
            }
            None => {
              self.add_error(&e.1, format!("definition not found: {:?}", i));
              core::ExprF::Hole
            }
          }
        } else {
          match self.lookup(&mod_name[0]) {
            Some(Entity::Mod(def)) => {
              let mut e = core::ExprF::Def(*def);
              for m in mod_name.iter().skip(1) {
                e = core::ExprF::Prop(
                  STag {
                    is_tuple: false,
                    is_mod: true,
                  },
                  wrap(e),
                  self.name_from_def(m),
                );
              }
              core::ExprF::Prop(
                STag {
                  is_tuple: false,
                  is_mod: true,
                },
                wrap(e),
                self.name_from_def(i),
              )
            }
            _ => {
              self.add_error(&e.1, format!("module not found: {:?}", mod_name[0]));
              core::ExprF::Hole
            }
          }
        }
      }
      Let(ds, e) => {
        self.envs.push(Env::new());
        let decls = self.ds(ds);
        let e = Box::new(self.e(e));
        self.envs.pop();
        core::ExprF::Let(decls.defs, e)
      }
      Lit(l) => {
        self.add_error(&e.1, format!("literal not yet supported: {:?}", l));
        core::ExprF::Hole
      }

      PiE(i, ty, e) => {
        let tag = PTag {
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Box::new(self.e(&ty));
        let i = self.add_bind_maybe(i);
        let e = Box::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, core::Vis::Explicit, i, ty, e)
      }
      LamE(i, ty, e) => {
        let tag = PTag {
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Box::new(self.e(&ty));
        let i = self.add_bind(i);
        let e = Box::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, core::Vis::Explicit, i, ty, e)
      }
      AppE(e1, e2) => {
        let tag = PTag {
          is_mod_param: false,
        };
        let e1 = Box::new(self.e(&e1));
        let e2 = Box::new(self.e(&e2));
        core::ExprF::App(tag, core::Vis::Explicit, e1, e2)
      }

      PiI(i, ty, e) => {
        let tag = PTag {
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Box::new(self.e(&ty));
        let i = self.add_bind_maybe(i);
        let e = Box::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, core::Vis::Implicit, i, ty, e)
      }
      LamI(i, ty, e) => {
        let tag = PTag {
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Box::new(self.e(&ty));
        let i = self.add_bind(i);
        let e = Box::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, core::Vis::Implicit, i, ty, e)
      }
      AppI(e1, e2) => {
        let tag = PTag {
          is_mod_param: false,
        };
        let e1 = Box::new(self.e(&e1));
        let e2 = Box::new(self.e(&e2));
        core::ExprF::App(tag, core::Vis::Implicit, e1, e2)
      }

      TupleTy(elems) => {
        let tag = STag {
          is_tuple: true,
          is_mod: false,
        };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (ix, (bind, e)) in elems.iter().enumerate() {
          let ix = ix as u8;
          let name = self.name_from_index(&ix);
          let bind = self.add_bind_maybe(bind);
          let e = Box::new(self.e(&e));
          es.push((name, bind, e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      TupleCon(elems) => {
        let tag = STag {
          is_tuple: true,
          is_mod: false,
        };
        let mut es = Vec::new();
        for (ix, e) in elems.iter().enumerate() {
          let ix = ix as u8;
          let name = self.name_from_index(&ix);
          let e = Box::new(self.e(&e));
          es.push((name, e));
        }
        core::ExprF::Obj(tag, es)
      }
      Proj(e, i) => {
        let tag = STag {
          is_tuple: true,
          is_mod: false,
        };
        let i = self.name_from_index(i);
        let e = Box::new(self.e(&e));
        core::ExprF::Prop(tag, e, i)
      }

      ObjTy(elems) => {
        let tag = STag {
          is_tuple: false,
          is_mod: false,
        };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let bind = self.add_bind(i);
          let e = Box::new(self.e(&e));
          es.push((name, bind, e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      ObjCon(elems) => {
        let tag = STag {
          is_tuple: false,
          is_mod: false,
        };
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let e = Box::new(self.e(&e));
          es.push((name, e));
        }
        core::ExprF::Obj(tag, es)
      }
      Prop(e, i) => {
        let tag = STag {
          is_tuple: false,
          is_mod: false,
        };
        let name = self.name_from_id(i);
        let e = Box::new(self.e(&e));
        core::ExprF::Prop(tag, e, name)
      }
    })
  }

  fn d(&mut self, d: &Decl<T>, decls: &mut Decls<T>) {
    match &d.0 {
      DeclF::Mod(name, params, body) => {
        self.envs.push(Env::new());
        let mut ps = Vec::new();
        for (vis, i, ty) in params {
          let ty = self.e(ty);
          let i = self.add_bind(i);
          ps.push((vis.clone(), i, Box::new(ty)));
        }
        let e = match &body.0 {
          ModBodyF::Decls(ds) => {
            self.envs.push(Env::new());
            let decls = self.ds(ds);
            let env = self.envs.pop().unwrap();
            let mut defs = Vec::new();
            for (i, e) in env.lookup {
              let name = self.name_from_def(&i);
              let e = match e {
                Entity::Bind(i) => core::ExprF::Bind(i),
                Entity::Def(i) => core::ExprF::Def(i),
                Entity::Mod(i) => core::ExprF::Def(i),
              };
              defs.push((name, wrap(e)));
            }
            let obj = core::ExprF::Obj(
              STag {
                is_tuple: false,
                is_mod: true,
              },
              defs,
            );
            core::ExprF::Let(decls.defs, wrap(obj))
          }
          ModBodyF::Import(_) => {
            self.add_error(&d.2, "`import` not yet supported".to_string());
            core::ExprF::Hole
          }
          ModBodyF::App(_, mod_name, args) => match self.lookup(&mod_name[0]) {
            Some(Entity::Mod(def)) => {
              let mut e = core::ExprF::Def(*def);
              for m in mod_name.iter().skip(1) {
                e = core::ExprF::Prop(
                  STag {
                    is_tuple: false,
                    is_mod: true,
                  },
                  wrap(e),
                  self.name_from_def(m),
                );
              }
              for (vis, arg) in args {
                let arg = self.e(arg);
                e = core::ExprF::App(
                  PTag { is_mod_param: true },
                  core::Vis::from(vis.clone()),
                  wrap(e),
                  Box::new(arg),
                );
              }
              e
            }
            _ => {
              self.add_error(&d.2, format!("module not found: {:?}", mod_name[0]));
              core::ExprF::Hole
            }
          },
        };
        self.envs.pop();
        let mut e = e;
        for p in ps.into_iter().rev() {
          let (vis, i, ty) = p;
          let tag = PTag { is_mod_param: true };
          e = core::ExprF::Lam(tag, core::Vis::from(vis.clone()), i, ty, wrap(e));
        }
        let def = self.add_def(name, DefType::Mod);
        decls.defs.push((def, d.1.clone(), wrap(e)));
        self
          .envs
          .last_mut()
          .unwrap()
          .lookup
          .insert(name.clone(), Entity::Mod(def));
      }
      DeclF::Def(i, e) => {
        let e = self.e(&e);
        let def = self.add_def(&i, DefType::Def);
        decls.defs.push((def, d.1.clone(), Box::new(e)));
        self
          .envs
          .last_mut()
          .unwrap()
          .lookup
          .insert(i.clone(), Entity::Def(def));
      }
    };
  }

  fn ds(&mut self, ds: &Vec<Decl<T>>) -> Decls<T> {
    let mut decls = Decls { defs: Vec::new() };
    for d in ds {
      self.d(d, &mut decls);
    }
    decls
  }
}

pub fn convert<T: Clone>(p: Program<T>) -> (core::Program<T, PTag, STag>, Vec<(T, String)>) {
  let mut b = Builder::new(p.symbols);
  // No need to do this but for convenience
  for index in 0..8 {
    b.name_from_index(&index);
  }
  let defs = b.ds(&p.decls).defs;
  (
    core::Program {
      defs,
      def_symbols: b
        .def_symbols
        .iter()
        .map(|(k, v)| {
          (
            *k,
            match v.name {
              Some(ref s) => format!("{}", s),
              None => format!("{:?}", k),
            },
          )
        })
        .collect(),
      bind_symbols: b
        .bind_symbols
        .iter()
        .map(|(k, v)| {
          (
            *k,
            match v.name {
              Some(ref s) => format!("{}", s),
              None => format!("{:?}", k),
            },
          )
        })
        .collect(),
      name_symbols: b
        .name_symbols
        .iter()
        .map(|(k, v)| {
          (
            *k,
            match v {
              Name::Def(i) => {
                if let Some(s) = b.symbols.get(&Id(*i)) {
                  format!("{}@{}", s, i)
                } else {
                  format!("@{}", i)
                }
              }
              Name::Str(s) => format!("{}#{}", k.0, s),
              Name::Ix(i) => format!("{}#{}", k.0, i),
            },
          )
        })
        .collect(),
    },
    b.errors,
  )
}
