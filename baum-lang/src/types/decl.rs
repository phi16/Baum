use crate::types::mixfix::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
  Explicit,
  Implicit,
}

pub type ArgF<I, E> = (Vis, Vec<I>, Option<E>);

#[derive(Debug, Clone)]
pub struct DefF<I, E> {
  pub name: I,
  pub args: Vec<ArgF<I, E>>,
  pub ty: Option<E>,
  pub body: E,
  // pub where_: Vec<DefF<I, E>>,
}

#[derive(Debug, Clone)]
pub struct ModDeclF<I, E> {
  pub name: I,
  pub params: Vec<ArgF<I, E>>,
}

#[derive(Debug, Clone)]
pub enum ModuleF<S, I, Ds, E> {
  Decls(Ds),
  Import(S),
  Ref(Vec<I>, Vec<(Vis, E)>),
}

#[derive(Debug, Clone)]
pub enum DeclF<I, Ds, E, M> {
  Local(Ds),
  Module(ModDeclF<I, E>, M),
  Use(M),
  Open(M),
  Def(DefF<I, E>),
  Syntax(Syntax /* , Vec<DefF<I, E>> */),
}
