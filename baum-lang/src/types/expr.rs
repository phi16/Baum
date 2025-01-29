use crate::types::mixfix::*;
use crate::types::parse_base::*;

#[derive(Debug, Clone)]
pub enum Literal<S> {
  Nat(S),
  Rat(S),
  Chr(S),
  Str(S),
}

#[derive(Debug, Clone)]
pub enum ExprF<S, I, Ds, E, Se> {
  Hole,
  Lit(Literal<S>),
  Var(I),
  Prim(S),
  Ext(ModuleName<I>, I),
  Let(Ds, E),
  Syntax(Syntax, Se),
}
