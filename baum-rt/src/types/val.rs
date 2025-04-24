use crate::types::common::{Id, Name};
use crate::types::tree::Tree;
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
pub enum Val {
  Unit,
  Raw(Raw),
  Prim(String, usize, Vec<Val>),
  Cl(Id, Env, Rc<Tree>),
  Obj(HashMap<Name, Val>),
}

pub type Env = Rc<HashMap<Id, Val>>;

#[derive(Debug, Clone)]
pub enum Op {
  Val(Val),
  Eval(Rc<Tree>, Env),
  Prim(String, Vec<Val>),
}

#[derive(Debug, Clone)]
pub enum Cont {
  App0((), Box<Thunk>),
  App1(Val, ()),
  Prop(Name),
}

#[derive(Debug, Clone)]
pub struct Thunk {
  op: Op,
  conts: Vec<Cont>,
}

impl Thunk {
  pub fn new(op: Op) -> Self {
    Self {
      op,
      conts: Vec::new(),
    }
  }

  pub fn into_inner(self) -> (Op, Vec<Cont>) {
    (self.op, self.conts)
  }

  pub fn val(v: Val) -> Self {
    Self::new(Op::Val(v))
  }

  pub fn eval(t: Rc<Tree>, env: Env) -> Self {
    Self::new(Op::Eval(t, env))
  }

  pub fn prim(name: String, args: Vec<Val>) -> Self {
    Self::new(Op::Prim(name, args))
  }

  pub fn push_cont(&mut self, k: Cont) {
    self.conts.push(k);
  }
}

pub fn app(f: Val, x: Val) -> Thunk {
  match f {
    Val::Cl(i, mut e, body) => {
      Rc::make_mut(&mut e).insert(i, x);
      Thunk::eval(body, e)
    }
    Val::Prim(name, n, args) => {
      let mut args = args;
      args.push(x);
      if n == args.len() {
        Thunk::prim(name, args)
      } else {
        Thunk::val(Val::Prim(name, n, args))
      }
    }
    _ => unreachable!(),
  }
}

pub fn prop(o: Val, n: &Name) -> Thunk {
  match o {
    Val::Obj(ps) => Thunk::val(ps.get(n).unwrap().clone()),
    _ => unreachable!(),
  }
}
