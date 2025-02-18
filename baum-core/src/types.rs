use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub u32);

impl std::fmt::Debug for Id {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

type I = Id;
type E = Rc<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
  Hole,
  Var(I),
  Let(Vec<(I, E)>, E),

  Ann(E, E),
  Uni,

  PiE(I, E, E),
  LamE(I, E, E),
  AppE(E, E),

  PiI(I, E, E),
  LamI(I, E, E),
  AppI(E, E),

  TupleTy(Vec<(Option<I>, E)>),
  TupleCon(Vec<(I, E)>),
  Proj(u8, E),

  ObjTy(Vec<(I, E)>),
  Obj(Vec<(I, E)>),
  Prop(I, E),
}

#[derive(Debug, Clone)]
pub struct Program {
  pub decls: Vec<(I, E)>,
  pub symbols: HashMap<I, String>,
}
