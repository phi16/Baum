use crate::types::code::{FunIx, OpIx};
use crate::types::common::Name;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Action {
  pub run: Rc<dyn Fn() -> Thunk>,
}

impl std::fmt::Debug for Action {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<Action>")
  }
}

#[derive(Debug, Clone)]
pub enum Raw {
  U32(u32),
  F32(f32),
  Char(char),
  String(String),
  Action(Action),
  Done,
}

#[derive(Debug, Clone)]
pub enum ValF<T> {
  Unit,
  Raw(Raw),
  Prim(String, usize, Vec<T>),
  Cl(FunIx, Vec<T>),
  Obj(HashMap<Name, T>),
}

#[derive(Debug, Clone)]
pub struct Val(pub ValF<Rc<Val>>);
pub type V = Rc<Val>;

#[derive(Debug, Clone)]
pub enum ThunkOp {
  Val(V),
  Eval(FunIx, Vec<V>, V),
  Prim(String, Vec<V>),
}

#[derive(Debug, Clone)]
pub struct Cont {
  pub fun: FunIx,
  pub env: Vec<V>,
  pub res: Vec<V>,
  pub opix: OpIx,
}

#[derive(Debug, Clone)]
pub struct Thunk {
  op: ThunkOp,
  conts: Vec<Cont>,
}

impl Thunk {
  pub fn new(op: ThunkOp) -> Self {
    Self {
      op,
      conts: Vec::new(),
    }
  }

  pub fn into_inner(self) -> (ThunkOp, Vec<Cont>) {
    (self.op, self.conts)
  }

  pub fn val(v: V) -> Self {
    Self::new(ThunkOp::Val(v))
  }

  pub fn eval(fun: FunIx, env: Vec<V>, arg: V) -> Self {
    Self::new(ThunkOp::Eval(fun, env, arg))
  }

  pub fn prim(name: String, args: Vec<V>) -> Self {
    Self::new(ThunkOp::Prim(name, args))
  }

  pub fn push_cont(&mut self, k: Cont) {
    self.conts.push(k);
  }
}

pub fn app(f: V, x: V) -> Thunk {
  let f = Rc::unwrap_or_clone(f);
  match f.0 {
    ValF::Cl(fun, env) => Thunk::eval(fun, env, x),
    ValF::Prim(name, n, mut args) => {
      args.push(x);
      if n == args.len() {
        Thunk::prim(name, args)
      } else {
        Thunk::val(Rc::new(Val(ValF::Prim(name, n, args))))
      }
    }
    _ => unreachable!(),
  }
}

pub fn prop(o: V, n: &Name) -> Thunk {
  match &o.0 {
    ValF::Obj(ps) => Thunk::val(ps.get(n).unwrap().clone()),
    _ => unreachable!(),
  }
}
