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

pub type ModDepth = u8;
pub type ModLevel = i8;

#[derive(Debug, Clone)]
pub enum ExprF<T, I, E> {
  Hole,
  Bind(I),
  Ann(E, E),
  Uni,
  Wrap(E),

  Def(ModLevel, Vec<I>, I),
  Let(Vec<Decl<T>>, E),
  Lit(Literal),

  PiE(Option<I>, E, E),
  LamE(I, E, E),
  AppE(E, E),

  PiI(Option<I>, E, E),
  LamI(I, E, E),
  AppI(E, E),

  TupleTy(Vec<(Option<I>, E)>),
  TupleCon(Vec<E>),
  Proj(E, u8),

  ObjTy(Vec<(I, E)>),
  ObjCon(Vec<(I, E)>),
  Prop(E, I),
}

pub type ExprInternal<T> = ExprF<T, Id, Rc<Expr<T>>>;

#[derive(Debug, Clone)]
pub struct Expr<T>(pub ExprInternal<T>, pub T);

#[derive(Debug, Clone)]
pub enum Vis {
  Explicit,
  Implicit,
}

#[derive(Debug, Clone)]
pub enum ModBodyF<T> {
  Decls(Vec<Decl<T>>),
  Import(String),
  App(ModLevel, Vec<Id>, Vec<(Vis, Expr<T>)>),
}

#[derive(Debug, Clone)]
pub struct ModBody<T>(pub ModBodyF<T>, pub T);

#[derive(Debug, Clone)]
pub enum DeclF<T> {
  Mod(Id, Vec<(Vis, Id, Rc<Expr<T>>)>, ModBody<T>),
  Def(Id, Expr<T>),
}

#[derive(Debug, Clone)]
pub struct Decl<T>(pub DeclF<T>, pub T);

#[derive(Debug, Clone)]
pub struct Program<T> {
  pub decls: Vec<Decl<T>>,
  pub symbols: HashMap<Id, String>,
}
