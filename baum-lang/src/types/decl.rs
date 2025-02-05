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
}

#[derive(Debug, Clone)]
pub struct ModDeclF<I, E> {
  pub name: I,
  pub params: Vec<ArgF<I, E>>,
}

#[derive(Debug, Clone)]
pub enum ModRefF<S, I, E> {
  Import(S),
  App(Vec<I>, Vec<(Vis, E)>),
}

#[derive(Debug, Clone)]
pub enum ModDefF<S, I, Ds, E> {
  Decls(Ds),
  Ref(ModRefF<S, I, E>),
}

#[derive(Debug, Clone)]
pub enum DeclF<S, I, Ds, E, Md> {
  Local(Ds),
  ModDef(ModDeclF<I, E>, Md),
  Open(ModRefF<S, I, E>),
  Def(DefF<I, E> /* , Vec<DefF<I, E>> */),
  Syntax(Syntax /* , Vec<DefF<I, E>> */),
}
