use crate::types::code::*;
use crate::types::runtime::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Global {
  pub defs: HashMap<DefLoc, D>,
  pub next_def: u32,
  pub lookup: HashMap<String, DefLoc>,
}

impl Global {
  fn define(&mut self, d: D) -> DefLoc {
    let l = DefLoc(self.next_def);
    self.next_def += 1;
    self.defs.insert(l, d);
    l
  }
}

#[derive(Debug, Clone)]
pub struct FunMap {
  pub funs: HashMap<FunLoc, Fun>,
  pub next_fun: u32,
}

impl FunMap {
  fn define(&mut self, fun: Fun) -> FunLoc {
    let l = FunLoc(self.next_fun);
    self.next_fun += 1;
    self.funs.insert(l, fun);
    l
  }
}

#[derive(Debug, Clone)]
pub struct Compiler {
  pub global: Global,
  pub funmap: FunMap,
}

#[derive(Debug, Clone)]
pub enum VarRef {
  Local(OpLoc),
  Module(HashMap<String, VarRef>),
}

#[derive(Debug)]
pub struct BodyEnv<'a> {
  pub global: &'a Global,
  pub funmap: &'a mut FunMap,
  pub env: HashMap<String, VarRef>,
  pub locals: Vec<E>,
  pub fv: Vec<Vec<Id>>,
  pub fv_env: HashMap<String, VarRef>,
}

impl<'a> BodyEnv<'a> {
  fn new(global: &'a Global, funmap: &'a mut FunMap) -> Self {
    BodyEnv {
      global,
      funmap,
      env: HashMap::new(),
      locals: Vec::new(),
      fv: Vec::new(),
      fv_env: HashMap::new(),
    }
  }

  fn define(&mut self, e: E) -> OpLoc {
    let l = OpLoc(u32::try_from(self.locals.len()).unwrap());
    self.locals.push(e);
    l
  }

  fn lookup_env(&self, ids: &Vec<Id>) -> Option<VarRef> {
    let mut env = &self.env;
    for id in ids.iter().take(ids.len() - 1) {
      if let Some(VarRef::Module(m)) = env.get(id.as_str()) {
        env = m;
      } else {
        return None;
      }
    }
    env.get(ids.last()?.as_str()).cloned()
  }

  fn lookup_fv_env(&self, ids: &Vec<Id>) -> Option<VarRef> {
    let mut env = &self.fv_env;
    for id in ids.iter().take(ids.len() - 1) {
      if let Some(VarRef::Module(m)) = env.get(id.as_str()) {
        env = m;
      } else {
        return None;
      }
    }
    env.get(ids.last()?.as_str()).cloned()
  }

  fn lookup_def(&self, ids: &Vec<Id>) -> Option<DefLoc> {
    let mut lookup = &self.global.lookup;
    for id in ids.iter().take(ids.len() - 1) {
      if let Some(l) = lookup.get(id.as_str()) {
        if let D::Mod(m) = self.global.defs.get(l)? {
          lookup = m;
        } else {
          return None;
        }
      } else {
        return None;
      }
    }
    lookup.get(ids.last()?.as_str()).cloned()
  }

  fn expr_val(&mut self, expr: Expr) -> OpLoc {
    match self.expr(expr) {
      VarRef::Local(l) => l,
      _ => {
        eprintln!("expected local variable");
        self.define(E::Hole)
      }
    }
  }

  fn define_fv(&mut self, ids: &Vec<Id>, loc: OpLoc) {
    let mut env = &mut self.fv_env;
    for id in ids.iter().take(ids.len() - 1) {
      let entry = env
        .entry(id.as_str().to_string())
        .or_insert_with(|| VarRef::Module(HashMap::new()));
      if let VarRef::Module(m) = entry {
        env = m;
      } else {
        eprintln!("expected module");
        return;
      }
    }
    env.insert(ids.last().unwrap().as_str().to_string(), VarRef::Local(loc));
  }

  fn expr(&mut self, expr: Expr) -> VarRef {
    let e = match expr {
      Expr::Lit(l) => E::Lit(match l {
        Literal::Num(n) => L::Num(n),
        Literal::Chr(c) => L::Chr(c),
        Literal::Str(s) => L::Str(s),
      }),
      Expr::Var(ids) => {
        if let Some(v) = self.lookup_env(&ids) {
          return v;
        }
        if let Some(v) = self.lookup_fv_env(&ids) {
          return v;
        }
        let e = if let Some(l) = self.lookup_def(&ids) {
          E::DefRef(l)
        } else {
          let env_loc = EnvLoc(u16::try_from(self.fv.len()).unwrap());
          self.fv.push(ids.clone());
          E::EnvRef(env_loc)
        };
        let loc = self.define(e);
        self.define_fv(&ids, loc);
        return VarRef::Local(loc);
      }
      Expr::Lam(args, e) => {
        let empty_global = Global {
          defs: HashMap::new(),
          next_def: 0,
          lookup: HashMap::new(),
        };
        let mut lam_env = BodyEnv::new(&empty_global, &mut self.funmap);
        let args_name = args
          .into_iter()
          .enumerate()
          .map(|(l, a)| {
            let l = ArgLoc(u16::try_from(l).unwrap());
            let op = lam_env.define(E::ArgRef(l));
            let n = a.as_str().to_string();
            lam_env.env.insert(n.clone(), VarRef::Local(op));
            n
          })
          .collect();
        let result = lam_env.expr_val(*e);
        let body = Body {
          ls: lam_env.locals,
          result,
        };
        let (envs, envs_name) = lam_env
          .fv
          .into_iter()
          .map(|ids| {
            // TODO: use expr, not expr_val...
            let e = self.expr_val(Expr::Var(ids.clone()));
            let name = ids
              .iter()
              .map(|id| id.as_str())
              .collect::<Vec<_>>()
              .join(".");
            (e, name)
          })
          .unzip();
        let fun = self.funmap.define(Fun {
          args: args_name,
          env: envs_name,
          body,
        });
        E::Cl(fun, envs)
      }
      Expr::App(f, args) => {
        let f = self.expr_val(*f);
        let args = args.into_iter().map(|a| self.expr_val(a)).collect();
        E::App(f, args)
      }
      Expr::Hole => {
        eprintln!("hole found in expression");
        E::Hole
      }
      Expr::Prim(p) => E::Prim(p),
      Expr::Let(decls, expr) => {
        let orig_env = self.env.clone();
        decls.into_iter().for_each(|decl| {
          if let Some((name, v)) = self.decl(decl) {
            self.env.insert(name.to_string(), v);
          }
        });
        let e = self.expr(*expr);
        self.env = orig_env;
        return e;
      }
    };
    VarRef::Local(self.define(e))
  }

  fn decl(&mut self, decl: Decl) -> Option<(String, VarRef)> {
    match decl {
      Decl::Def(id, expr) => {
        let v = self.expr(*expr);
        let n = id.as_str().to_string();
        self.env.insert(n.clone(), v.clone());
        Some((n, v))
      }
      Decl::Mod(id, decls) => {
        let orig_env = self.env.clone();
        let mut m = HashMap::new();
        decls.into_iter().for_each(|decl| {
          if let Some((name, v)) = self.decl(decl) {
            m.insert(name.to_string(), v);
          }
        });
        self.env = orig_env;
        let v = VarRef::Module(m);
        let n = id.as_str().to_string();
        self.env.insert(n.clone(), v.clone());
        Some((n, v))
      }
      Decl::Infix(_, _, _) => None,
    }
  }
}

impl Compiler {
  fn new() -> Self {
    Compiler {
      global: Global {
        defs: HashMap::new(),
        next_def: 0,
        lookup: HashMap::new(),
      },
      funmap: FunMap {
        funs: HashMap::new(),
        next_fun: 0,
      },
    }
  }

  fn body(&mut self, expr: Expr) -> Body {
    let mut env = BodyEnv::new(&self.global, &mut self.funmap);
    let result = env.expr_val(expr);
    if !env.fv.is_empty() {
      eprintln!("free variables in body");
    }
    Body {
      ls: env.locals,
      result,
    }
  }

  fn decl(&mut self, decl: Decl) -> Option<(String, DefLoc)> {
    let (id, d) = match decl {
      Decl::Def(id, expr) => {
        let d = self.body(*expr);
        (id, D::Def(d))
      }
      Decl::Mod(id, decls) => {
        let mut m = HashMap::new();
        for decl in decls {
          if let Some((name, dl)) = self.decl(decl) {
            if m.contains_key(&name) {
              eprintln!("duplicate declaration: {}", name);
            } else {
              m.insert(name.to_string(), dl);
            }
          }
        }
        (id, D::Mod(m))
      }
      Decl::Infix(_, _, _) => return None,
    };
    let l = self.global.define(d);
    Some((id.as_str().to_string(), l))
  }
}

pub fn compile(program: Vec<Decl>) -> Runtime {
  let mut compiler = Compiler::new();
  for decl in program {
    if let Some((name, dl)) = compiler.decl(decl) {
      compiler.global.lookup.insert(name, dl);
    }
  }
  Runtime {
    funs: compiler.funmap.funs,
    fun_count: compiler.funmap.next_fun,
    defs: compiler.global.defs,
    def_count: compiler.global.next_def,
    lookup: compiler.global.lookup,
  }
}
