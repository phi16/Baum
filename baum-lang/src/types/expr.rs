#[derive(Debug, Clone)]
pub enum Literal<S> {
  Nat(S),
  Rat(S),
  Chr(S),
  Str(S),
}

#[derive(Debug, Clone)]
pub enum ExprF<S, I, X> {
  Hole,
  Lit(Literal<S>),
  Var(I),
  Ext(Vec<I>, I),
  Syntax(X),
}
