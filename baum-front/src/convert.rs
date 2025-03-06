use crate::types::tree::*;
use baum_core::types::tree as core;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Entity {
  Bind(core::BindId),
  Mod(core::BindId),
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
enum BindType {
  Bind,
  Def,
  Mod,
}

struct Bind {
  name: Option<String>,
  ty: BindType,
}

enum Name {
  Str(String),
  Ix(u8),
  Def(u32),
}

struct Decls {
  defs: Vec<(core::BindId, Rc<core::Expr>)>,
}

struct Builder {
  symbols: HashMap<Id, String>,
  envs: Vec<Env>,
  names_id: HashMap<Id, core::NameId>,
  names_ix: HashMap<u8, core::NameId>,
  names_def: HashMap<u32, core::NameId>,
  bind_symbols: HashMap<core::BindId, Bind>,
  name_symbols: HashMap<core::NameId, Name>,
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
      names_def: HashMap::new(),
      bind_symbols: HashMap::new(),
      name_symbols: HashMap::new(),
      next_bind_id: 0,
      next_name_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_bind(&mut self, i: &Id, b: Bind) -> core::BindId {
    let id = core::BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self
      .envs
      .last_mut()
      .unwrap()
      .lookup
      .insert(i.clone(), Entity::Bind(id));
    self.bind_symbols.insert(id, b);
    id
  }

  fn add_bind_ty(&mut self, i: &Id, ty: BindType) -> core::BindId {
    self.add_bind(
      i,
      Bind {
        name: self.symbols.get(i).cloned(),
        ty,
      },
    )
  }

  fn add_local_bind(&mut self, i: &Id) -> core::BindId {
    self.add_bind_ty(i, BindType::Bind)
  }

  fn lookup_bind(&mut self, i: &Id) -> Option<core::BindId> {
    for env in self.envs.iter().rev() {
      if let Some(e) = env.lookup.get(i) {
        if let Entity::Bind(id) = e {
          return Some(*id);
        } else {
          self.errors.push(format!("Expected bind, found {:?}", e));
          return None;
        }
      }
    }
    None
  }

  fn lookup_mod(&mut self, m: &Id) -> Option<core::BindId> {
    for env in self.envs.iter().rev() {
      if let Some(e) = env.lookup.get(m) {
        if let Entity::Mod(bind) = e {
          return Some(*bind);
        } else {
          self.errors.push(format!("Expected mod, found {:?}", e));
          return None;
        }
      }
    }
    None
  }

  fn name_from_id(&mut self, i: &Id) -> core::NameId {
    if let Some(i) = self.names_id.get(i) {
      return *i;
    } else {
      let id = core::NameId(self.next_name_id);
      self.next_name_id += 1;
      self.names_id.insert(i.clone(), id);
      let s = self.symbols.get(i).unwrap();
      self.name_symbols.insert(id, Name::Str(s.clone()));
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

  fn e(&mut self, e: &Expr) -> core::Expr {
    use ExprF::*;
    core::Expr(match &e.0 {
      Hole => core::ExprF::Hole,
      Bind(i) => match self.lookup_bind(i) {
        Some(i) => {
          assert_eq!(
            self.bind_symbols.get(&i).map(|b| &b.ty).unwrap(),
            &BindType::Bind
          );
          core::ExprF::Bind(i)
        }
        None => {
          self.errors.push(format!("Unbound identifier: {:?}", i));
          core::ExprF::Hole
        }
      },
      Ann(e, ty) => core::ExprF::Ann(Rc::new(self.e(&e)), Rc::new(self.e(&ty))),
      Uni => core::ExprF::Uni,

      Ext(_, mod_name, i) => {
        if mod_name.is_empty() {
          match self.lookup_bind(i) {
            Some(i) => core::ExprF::Bind(i),
            None => {
              self.errors.push(format!("Variable not found: {:?}", i));
              core::ExprF::Hole
            }
          }
        } else {
          match self.lookup_mod(&mod_name[0]) {
            Some(bind) => {
              let mut e = core::ExprF::Bind(bind);
              for m in mod_name.iter().skip(1) {
                e = core::ExprF::Prop(
                  core::STag {
                    is_tuple: false,
                    is_mod: true,
                  },
                  self.name_from_def(m),
                  Rc::new(core::Expr(e)),
                );
              }
              core::ExprF::Prop(
                core::STag {
                  is_tuple: false,
                  is_mod: true,
                },
                self.name_from_def(i),
                Rc::new(core::Expr(e)),
              )
            }
            None => {
              self
                .errors
                .push(format!("Module not found: {:?}", mod_name[0]));
              core::ExprF::Hole
            }
          }
        }
      }
      Let(ds, e) => {
        self.envs.push(Env::new());
        let decls = self.ds(ds);
        let e = Rc::new(self.e(e));
        self.envs.pop();
        core::ExprF::Let(decls.defs, e)
      }
      Lit(l) => {
        self
          .errors
          .push(format!("Literal not yet supported: {:?}", l));
        core::ExprF::Hole
      }

      PiE(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = i.map(|i| self.add_local_bind(&i));
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, i, ty, e)
      }
      LamE(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = self.add_local_bind(i);
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, i, ty, e)
      }
      AppE(e1, e2) => {
        let tag = core::PTag {
          vis: core::Vis::Explicit,
          is_mod_param: false,
        };
        let e1 = Rc::new(self.e(&e1));
        let e2 = Rc::new(self.e(&e2));
        core::ExprF::App(tag, e1, e2)
      }

      PiI(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = i.map(|i| self.add_local_bind(&i));
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Pi(tag, i, ty, e)
      }
      LamI(i, ty, e) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
          is_mod_param: false,
        };
        self.envs.push(Env::new());
        let ty = Rc::new(self.e(&ty));
        let i = self.add_local_bind(i);
        let e = Rc::new(self.e(&e));
        self.envs.pop();
        core::ExprF::Lam(tag, i, ty, e)
      }
      AppI(e1, e2) => {
        let tag = core::PTag {
          vis: core::Vis::Implicit,
          is_mod_param: false,
        };
        let e1 = Rc::new(self.e(&e1));
        let e2 = Rc::new(self.e(&e2));
        core::ExprF::App(tag, e1, e2)
      }

      TupleTy(elems) => {
        let tag = core::STag {
          is_tuple: true,
          is_mod: false,
        };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (ix, (bind, e)) in elems.iter().enumerate() {
          let ix = ix as u8;
          let name = self.name_from_index(&ix);
          let bind = bind.map(|i| self.add_local_bind(&i));
          let e = Rc::new(self.e(&e));
          es.push((name, bind, e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      TupleCon(elems) => {
        let tag = core::STag {
          is_tuple: true,
          is_mod: false,
        };
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
        let tag = core::STag {
          is_tuple: true,
          is_mod: false,
        };
        let i = self.name_from_index(i);
        let e = Rc::new(self.e(&e));
        core::ExprF::Prop(tag, i, e)
      }

      ObjTy(elems) => {
        let tag = core::STag {
          is_tuple: false,
          is_mod: false,
        };
        self.envs.push(Env::new());
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let bind = self.add_local_bind(i);
          let e = Rc::new(self.e(&e));
          es.push((name, Some(bind), e));
        }
        self.envs.pop();
        core::ExprF::Sigma(tag, es)
      }
      ObjCon(elems) => {
        let tag = core::STag {
          is_tuple: false,
          is_mod: false,
        };
        let mut es = Vec::new();
        for (i, e) in elems {
          let name = self.name_from_id(i);
          let e = Rc::new(self.e(&e));
          es.push((name, e));
        }
        core::ExprF::Obj(tag, es)
      }
      Prop(i, e) => {
        let tag = core::STag {
          is_tuple: false,
          is_mod: false,
        };
        let name = self.name_from_id(i);
        let e = Rc::new(self.e(&e));
        core::ExprF::Prop(tag, name, e)
      }
    })
  }

  fn d(&mut self, d: &Decl, decls: &mut Decls) {
    match d {
      Decl::Mod(name, params, body) => {
        self.envs.push(Env::new());
        let mut ps = Vec::new();
        for (vis, i, ty) in params {
          let ty = self.e(ty);
          let i = self.add_local_bind(i);
          ps.push((vis.clone(), i, Rc::new(ty)));
        }
        let e = match body {
          ModBody::Decls(ds) => {
            self.envs.push(Env::new());
            let decls = self.ds(ds);
            let env = self.envs.pop().unwrap();
            let mut defs = Vec::new();
            for (i, e) in env.lookup {
              let name = self.name_from_def(&i);
              let bind = match e {
                Entity::Bind(bind) => bind,
                Entity::Mod(bind) => bind,
              };
              defs.push((name, Rc::new(core::Expr(core::ExprF::Bind(bind)))));
            }
            let obj = core::ExprF::Obj(
              core::STag {
                is_tuple: false,
                is_mod: true,
              },
              defs,
            );
            core::ExprF::Let(decls.defs, Rc::new(core::Expr(obj)))
          }
          ModBody::Import(_) => {
            self.errors.push("Import not yet supported".to_string());
            core::ExprF::Hole
          }
          ModBody::App(_, mod_name, args) => match self.lookup_mod(&mod_name[0]) {
            Some(bind) => {
              let mut e = core::ExprF::Bind(bind);
              for m in mod_name.iter().skip(1) {
                e = core::ExprF::Prop(
                  core::STag {
                    is_tuple: false,
                    is_mod: true,
                  },
                  self.name_from_def(m),
                  Rc::new(core::Expr(e)),
                );
              }
              for (vis, arg) in args {
                let vis = match vis {
                  Vis::Explicit => core::Vis::Explicit,
                  Vis::Implicit => core::Vis::Implicit,
                };
                let arg = self.e(arg);
                e = core::ExprF::App(
                  core::PTag {
                    vis,
                    is_mod_param: true,
                  },
                  Rc::new(core::Expr(e)),
                  Rc::new(arg),
                );
              }
              e
            }
            None => {
              self
                .errors
                .push(format!("Module not found: {:?}", mod_name[0]));
              core::ExprF::Hole
            }
          },
        };
        self.envs.pop();
        let mut e = e;
        for p in ps.into_iter().rev() {
          let (vis, i, ty) = p;
          let tag = core::PTag {
            vis: match vis {
              Vis::Explicit => core::Vis::Explicit,
              Vis::Implicit => core::Vis::Implicit,
            },
            is_mod_param: true,
          };
          e = core::ExprF::Lam(tag, i, ty, Rc::new(core::Expr(e)));
        }
        let bind = self.add_bind_ty(name, BindType::Mod);
        decls.defs.push((bind, Rc::new(core::Expr(e))));
        self
          .envs
          .last_mut()
          .unwrap()
          .lookup
          .insert(name.clone(), Entity::Mod(bind));
      }
      Decl::Def(i, e) => {
        let e = self.e(&e);
        let bind = self.add_bind_ty(&i, BindType::Def);
        decls.defs.push((bind, Rc::new(e)));
        self
          .envs
          .last_mut()
          .unwrap()
          .lookup
          .insert(i.clone(), Entity::Bind(bind));
      }
    };
  }

  fn ds(&mut self, ds: &Vec<Decl>) -> Decls {
    let mut decls = Decls { defs: Vec::new() };
    for d in ds {
      self.d(d, &mut decls);
    }
    decls
  }
}

pub fn convert(p: Program) -> (core::Program, Vec<String>) {
  let mut b = Builder::new(p.symbols);
  let defs = b.ds(&p.decls).defs;
  (
    core::Program {
      defs,
      bind_symbols: b
        .bind_symbols
        .iter()
        .map(|(k, v)| {
          (
            *k,
            match v.name {
              Some(ref s) => format!("{}{:?}", s, k),
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
              Name::Str(s) => format!("#{}", s),
              Name::Ix(i) => format!("#{}", i),
            },
          )
        })
        .collect(),
    },
    b.errors,
  )
}
