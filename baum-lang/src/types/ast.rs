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
pub struct WhereF<I, E> {
  pub defs: Vec<DefF<I, E>>,
}

#[derive(Debug, Clone)]
pub enum SyntaxDefF<S, I> {
  Token(S),
  Ident(I),
  Expr(I),
}

#[derive(Debug, Clone)]
pub enum DeclF<S, I, Ds, E, Md> {
  Local(Ds),
  ModDef(ModDeclF<I, E>, Md),
  Open(ModRefF<S, I, E>),
  Def(DefF<I, E>, WhereF<I, E>),
  Syntax(Option<S>, Vec<SyntaxDefF<S, I>>, E, WhereF<I, E>),
}

#[derive(Debug, Clone)]
pub enum ExprF<I, X, Xe> {
  Hole,
  Var(I),
  Mod(Vec<I>),
  Ext(Vec<I>, I),
  Syntax(X, Vec<Xe>),
}
