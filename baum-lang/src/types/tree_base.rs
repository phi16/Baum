use baum_front::types::tree as front;

pub type Vis = front::Vis;

pub type ArgF<I, E> = (Vis, Vec<I>, Option<E>);

#[derive(Debug, Clone)]
pub enum ModRefF<S, I, E> {
  Import(S),
  App(Vec<I>, Vec<(Vis, E)>),
}

#[derive(Debug, Clone)]
pub struct ModDefF<I, A> {
  pub name: I,
  pub params: Vec<A>,
}

#[derive(Debug, Clone)]
pub enum ModBodyF<Ds, Mr> {
  Decls(Ds),
  Ref(Mr),
}

#[derive(Debug, Clone)]
pub struct DefF<I, A, E> {
  pub name: I,
  pub args: Vec<A>,
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
pub enum DeclF<S, I, Ds, A, E, X, Sy, Mr> {
  Local(Ds),
  Mod(ModDefF<I, A>, ModBodyF<Ds, Mr>),
  Open(Mr),
  Use(Mr),
  Def(DefF<I, A, E>),
  Syntax(X, Option<S>, Sy, E),
}

#[derive(Debug, Clone)]
pub enum SynElemF<S, I, E> {
  Token(S),
  Ident(I),
  Dec(S),
  Num(S),
  Chr(S),
  Str(S),
  Expr(E),
}

#[derive(Debug, Clone)]
pub enum ExprF<I, Ds, E, X, Se> {
  Hole,
  Var(I),
  Mod(Vec<I>),
  Ext(Vec<I>, I),
  Let(Ds, E),
  Syntax(Vec<I>, X, Se),
}
