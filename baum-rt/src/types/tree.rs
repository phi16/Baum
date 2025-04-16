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

#[derive(Debug, Clone)]
pub enum TreeF<T> {
  Var(Id),
  Unit,
  Prim(String),
  Let(Vec<(Id, T)>, T),

  Lam(Id, T),
  App(T, T),
  Obj(Vec<(Name, T)>),
  Prop(T, Name),
}

#[derive(Debug, Clone)]
pub struct Tree(pub TreeF<Rc<Tree>>);

#[derive(Debug, Clone)]
pub enum Cont {
  App(Val),
  Prop(Name),
}

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
  Type,
  Raw(Raw),
  Prim(String, usize, Vec<Val>),
  Cl(Id, Env, Rc<Tree>),
  Unit,
  Obj(HashMap<Name, Val>),
}

pub type Env = Rc<HashMap<Id, Val>>;

#[derive(Debug, Clone)]
pub enum Thunk {
  Val(Val),
  Thunk(Rc<Tree>, Env, Vec<Cont>),
}
