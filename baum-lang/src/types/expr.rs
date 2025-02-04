#[derive(Debug, Clone)]
pub enum ExprF<I, X> {
  Hole,
  Var(I),
  Mod(Vec<I>),
  Ext(Vec<I>, I),
  Syntax(X),
}
