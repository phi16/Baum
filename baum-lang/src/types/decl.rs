use crate::types::mixfix::*;
use crate::types::parse::*;

type ModuleName<I> = Vec<I>;

#[derive(Debug, Clone)]
pub enum ContextF<I, E> {
  Ref(I, E),
}

#[derive(Debug, Clone)]
pub enum ModuleF<S, I, Ds> {
  Decls(Option<I>, Ds),
  Import(S),
  Ref(ModuleName<I>),
}

#[derive(Debug, Clone)]
pub enum Access {
  Open,
  Keep,
}

#[derive(Debug, Clone)]
pub enum DeclF<I, D, E, M> {
  Context(ContextF<I, E>, D),
  Module(Access, M),
  Def(DefF<I, E>),
  Syntax(Syntax),
}
