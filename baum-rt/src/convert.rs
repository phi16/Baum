use crate::types::code::{Code, EnvIx, FunIx, Global, Op, OpIx, Ref};
use crate::types::common::{Id, Name};
use crate::types::tree::{Tree, TreeF};
use baum_core::types::common::{BindId, DefId};
use baum_core::types::val::{CExprF, RE};
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

struct Convert1 {
  next_id: u32,
  bind_map: HashMap<BindId, Id>,
  def_map: HashMap<DefId, Id>,
  used_ids: HashSet<Id>,
}

impl Convert1 {
  fn new() -> Convert1 {
    Convert1 {
      next_id: 0,
      bind_map: HashMap::new(),
      def_map: HashMap::new(),
      used_ids: HashSet::new(),
    }
  }

  fn bind(&mut self, i: BindId) -> Id {
    if let Some(id) = self.bind_map.get(&i) {
      self.used_ids.insert(*id);
      return *id;
    }
    let id = Id(self.next_id);
    self.next_id += 1;
    self.bind_map.insert(i, id);
    self.used_ids.insert(id);
    id
  }

  fn def(&mut self, i: DefId) -> Id {
    if let Some(id) = self.def_map.get(&i) {
      self.used_ids.insert(*id);
      return *id;
    }
    let id = Id(self.next_id);
    self.next_id += 1;
    self.def_map.insert(i, id);
    self.used_ids.insert(id);
    id
  }

  fn e<P, S>(&mut self, e: &RE<P, S>) -> Rc<Tree> {
    use CExprF::*;
    let t = match &e.0 {
      Hole(_) => unreachable!(),
      Bind(b) => TreeF::Var(self.bind(*b)),
      Def(d, _) => TreeF::Var(self.def(*d)),
      Ann(e, _) => return self.e(e),
      Uni(_) => TreeF::Unit,
      Prim(s) => TreeF::Prim(s.clone()),
      Lit(l) => TreeF::Lit(l.clone()),
      Let(defs, e) => {
        let outer_scope = std::mem::take(&mut self.used_ids);
        let mut ds = Vec::new();
        for (d, _, e) in defs {
          let e = self.e(e);
          let d = self.def(*d);
          ds.push((d, e));
        }
        let e = self.e(e);
        for (d, _) in &ds {
          self.used_ids.remove(d);
        }
        self.used_ids.extend(outer_scope);
        TreeF::Let(ds, e)
      }
      Pi(_, _, _, _, _) => TreeF::Unit,
      Lam(_, _, b, _, e) => {
        let outer_scope = std::mem::take(&mut self.used_ids);
        let e = self.e(e);
        let b = self.bind(*b);
        self.used_ids.remove(&b);
        let scope = self.used_ids.iter().cloned().collect::<Vec<_>>();
        self.used_ids.extend(outer_scope);
        TreeF::Lam(b, scope, e)
      }
      App(_, _, e1, e2) => TreeF::App(self.e(e1), self.e(e2)),

      Sigma0(_) => TreeF::Unit,
      Obj0(_) => TreeF::Unit,
      Sigma(_, _, _) => TreeF::Unit,
      Obj(_, (n0, e0), props) => {
        let mut ps = vec![(Name(n0.0), self.e(e0))];
        for (n, e) in props {
          ps.push((Name(n.0), self.e(e)));
        }
        TreeF::Obj(ps)
      }
      Prop(_, e, n) => TreeF::Prop(self.e(e), Name(n.0)),
    };
    Rc::new(Tree(t))
  }
}

struct Convert2 {
  funs: Vec<Code>,
  code: Vec<Op<OpIx>>,
  id_map: HashMap<Id, OpIx>,
}

impl Convert2 {
  fn new() -> Convert2 {
    Convert2 {
      funs: Vec::new(),
      code: Vec::new(),
      id_map: HashMap::new(),
    }
  }

  fn push(&mut self, op: Op<OpIx>) -> OpIx {
    let ix = self.code.len() as OpIx;
    self.code.push(op);
    ix
  }

  fn push_fun(&mut self, env_count: usize, ops: Vec<Op<OpIx>>, ret: OpIx) -> FunIx {
    let ix = self.funs.len() as FunIx;
    let code = Code {
      env_count: env_count as u32,
      ops,
      ret,
    };
    self.funs.push(code);
    ix
  }

  fn t(&mut self, t: Rc<Tree>) -> OpIx {
    match &t.0 {
      TreeF::Var(i) => self.id_map.get(i).unwrap().clone(),
      TreeF::Unit => self.push(Op::Unit),
      TreeF::Prim(s) => self.push(Op::Prim(s.clone())),
      TreeF::Lit(l) => self.push(Op::Lit(l.clone())),
      TreeF::Let(defs, e) => {
        for (d, e) in defs {
          let e = self.t(Rc::clone(e));
          let prev = self.id_map.insert(*d, e.clone());
          assert!(prev.is_none());
        }
        let e = self.t(Rc::clone(e));
        for (d, _) in defs {
          self.id_map.remove(d);
        }
        e
      }

      TreeF::Lam(arg, scope, body) => {
        let mut last_code = std::mem::take(&mut self.code);
        let last_id_map = std::mem::take(&mut self.id_map);
        let a = self.push(Op::Ref(Ref::Arg));
        self.id_map.insert(*arg, a);
        for (ix, id) in scope.iter().enumerate() {
          let e = self.push(Op::Ref(Ref::Env(ix as EnvIx)));
          self.id_map.insert(*id, e);
        }
        let body = self.t(Rc::clone(body));
        std::mem::swap(&mut self.code, &mut last_code);
        let ops = last_code;

        let fun = self.push_fun(scope.len(), ops, body);
        self.id_map = last_id_map;
        let mut env = Vec::new();
        for id in scope {
          env.push(self.id_map.get(id).unwrap().clone());
        }
        self.push(Op::Lam(fun, env))
      }
      TreeF::App(f, x) => {
        let f = self.t(Rc::clone(f));
        let x = self.t(Rc::clone(x));
        self.push(Op::App(f, x))
      }
      TreeF::Obj(props) => {
        let mut ps = Vec::new();
        for (n, e) in props {
          let e = self.t(Rc::clone(e));
          ps.push((*n, e));
        }
        self.push(Op::Obj(ps))
      }
      TreeF::Prop(o, n) => {
        let o = self.t(Rc::clone(o));
        self.push(Op::Prop(o, *n))
      }
    }
  }
}

pub fn convert<P, S>(e: &RE<P, S>) -> Global {
  let mut c = Convert1::new();
  let t = c.e(e);
  eprintln!("{}", crate::pretty::ppt(&t));
  eprintln!("--------");
  let mut c = Convert2::new();
  let ret = c.t(t);
  let ops = c.code;
  let main = Code {
    env_count: 0,
    ops,
    ret,
  };
  let g = Global { funs: c.funs, main };
  eprintln!("{}", crate::pretty::ppg(&g));
  eprintln!("--------");
  g
}
