use crate::types::literal::Literal;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub u32);

impl std::fmt::Debug for Id {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

#[derive(Debug, Clone)]
pub enum ExprF<I, E> {
  Hole,
  Var(I),
  Ann(E, E),
  Uni,

  Ext(Vec<I>, I),
  Let(Vec<Decl>, E),
  Lit(Literal),

  PiE(I, E, E),
  LamE(I, E, E),
  AppE(E, E),

  PiI(I, E, E),
  LamI(I, E, E),
  AppI(E, E),

  TupleTy(Vec<(Option<I>, E)>),
  TupelCon(Vec<E>),
  Proj(u8, E),

  ObjTy(Vec<(I, E)>),
  Obj(Vec<(I, E)>),
  Prop(I, E),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprF<Id, Rc<Expr>>);

type I = Id;
type E = Expr;

#[derive(Debug, Clone)]
pub enum Vis {
  Explicit,
  Implicit,
}

#[derive(Debug, Clone)]
pub enum ModBody {
  Decls(Vec<Decl>),
  Import(String),
  App(Vec<I>, Vec<(Vis, E)>),
}

#[derive(Debug, Clone)]
pub enum Decl {
  Mod(I, Vec<(Vis, I, Rc<E>)>, ModBody),
  Def(I, E),
}

#[derive(Debug, Clone)]
pub struct Program {
  pub decls: Vec<Decl>,
  pub symbols: HashMap<I, String>,
}
