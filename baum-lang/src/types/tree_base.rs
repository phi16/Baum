use baum_front::types::tree as front;

pub type Vis = front::Vis;

pub type ArgF<I, E> = (Vis, Vec<I>, Option<E>);

#[derive(Debug, Clone)]
pub enum ModRefF<S, I, E> {
  Import(S),
  App(Vec<I>, Vec<(Vis, E)>),
}

#[derive(Debug, Clone)]
pub struct ModDefF<I, E> {
  pub name: I,
  pub params: Vec<ArgF<I, E>>,
}

#[derive(Debug, Clone)]
pub enum ModBodyF<S, I, Ds, E> {
  Decls(Ds),
  Ref(ModRefF<S, I, E>),
}

#[derive(Debug, Clone)]
pub struct DefF<I, E> {
  pub name: I,
  pub args: Vec<ArgF<I, E>>,
  pub ty: Option<E>,
  pub body: E,
}

#[derive(Debug, Clone)]
pub enum SynDefF<S, I> {
  Token(S),
  Ident(I),
  Expr(I),
}

#[derive(Debug, Clone)]
pub enum DeclF<S, I, Ds, E, X> {
  Local(Ds),
  Mod(ModDefF<I, E>, ModBodyF<S, I, Ds, E>),
  Open(ModRefF<S, I, E>),
  Use(ModRefF<S, I, E>),
  Def(DefF<I, E>),
  Syntax(X, Option<S>, Vec<SynDefF<S, I>>, E),
}

#[derive(Debug, Clone)]
pub enum SyntaxElemF<S, I, E> {
  Token(S),
  Ident(I),
  Nat(S),
  Rat(S),
  Chr(S),
  Str(S),
  Expr(E),
}

#[derive(Debug, Clone)]
pub enum ExprF<S, I, Ds, E, X> {
  Hole,
  Var(I),
  Mod(Vec<I>),
  Ext(Vec<I>, I),
  Let(Ds, E),
  Syntax(Vec<I>, X, Vec<SyntaxElemF<S, I, E>>),
}
