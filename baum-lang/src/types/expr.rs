use crate::types::mixfix::*;
use crate::types::parse_base::*;

#[derive(Debug, Clone)]
pub enum Literal<S> {
  Nat(S),
  Rat(S),
  Chr(S),
  Str(S),
  Hole,
}

#[derive(Debug, Clone)]
pub enum Base<S, I, Ds, E, Se> {
  Lit(Literal<S>),
  Var(I),
  Prim(S),
  Ext(ModuleName<I>, I),
  Let(Ds, E),
  Syntax(Syntax, Se),
}
#[derive(Debug, Clone)]
pub enum FunctionE<I, E> {
  PiE(Vec<ArgF<I, E>>, E),
  LamE(Vec<ArgF<I, E>>, E),
  AppE(E, E),
}
#[derive(Debug, Clone)]
pub enum FunctionI<I, E> {
  PiI(Vec<ArgF<I, E>>, E),
  LamI(Vec<ArgF<I, E>>, E),
  AppI(E, E),
}
#[derive(Debug, Clone)]
pub enum Tuples<E> {
  TupleTy(Vec<E>),
  TupleCon(Vec<E>),
  Proj(u32, E),
}
#[derive(Debug, Clone)]
pub enum Objects<I, E> {
  Sigma(Vec<ArgF<I, E>>),
  Obj(Vec<DefF<I, E>>),
  Prop(I, E),
}
#[derive(Debug, Clone)]
pub enum Contextual<I, E> {
  Subst(Vec<DefF<I, E>>, E),
}
#[derive(Debug, Clone)]
pub enum Inductive<I, E> {
  Mu(I, E, Vec<(I, E)>),
}
#[derive(Debug, Clone)]
pub enum Coinductive<I, E> {
  Nu(I, E, Vec<(I, E)>),
}

#[derive(Debug, Clone)]
pub enum ExprF<S, I, Ds, E, Se> {
  Base(Base<S, I, Ds, E, Se>),
  FunctionE(FunctionE<I, E>),
  FunctionI(FunctionI<I, E>),
  Tuples(Tuples<E>),
  Objects(Objects<I, E>),
  Contextual(Contextual<I, E>),
  Inductive(Inductive<I, E>),
  Coinductive(Coinductive<I, E>),
}
