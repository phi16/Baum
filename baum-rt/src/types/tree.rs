use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub u32);

impl std::fmt::Debug for Id {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "%{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(pub u32);

impl std::fmt::Debug for Name {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

pub type Scope = Vec<Id>;

#[derive(Debug, Clone)]
pub enum TreeF<T> {
  Var(Id),
  Unit,
  Prim(String),
  Let(Vec<(Id, T)>, T),

  Lam(Id, Scope, T),
  App(T, T),
  Obj(Vec<(Name, T)>),
  Prop(T, Name),
}

#[derive(Debug, Clone)]
pub struct Tree(pub TreeF<Rc<Tree>>);

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
  Eval(Rc<Tree>, Env),
  Prim(String, Vec<Val>),
}

#[derive(Debug, Clone)]
pub enum Cont {
  App(Val),
  Prop(Name),
}

#[derive(Debug, Clone)]
pub enum Thunk {
  Val(Val),
  Op(Op, Vec<Cont>),
}

pub fn app(f: Val, x: Val) -> Thunk {
  match f {
    Val::Cl(i, mut e, body) => {
      Rc::make_mut(&mut e).insert(i, x);
      Thunk::Op(Op::Eval(body, e), Vec::new())
    }
    Val::Prim(name, n, args) => {
      let mut args = args;
      args.push(x);
      if n == args.len() {
        Thunk::Op(Op::Prim(name, args), Vec::new())
      } else {
        Thunk::Val(Val::Prim(name, n, args))
      }
    }
    _ => unreachable!(),
  }
}

pub fn prop(o: Val, n: &Name) -> Thunk {
  match o {
    Val::Obj(ps) => Thunk::Val(ps.get(n).unwrap().clone()),
    _ => unreachable!(),
  }
}
