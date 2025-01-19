use crate::mixfix_types::*;

type ModRef<I> = Vec<I>;

#[derive(Debug, Clone)]
pub enum Literal<S> {
  Num(S),
  Chr(S),
  Str(S),
  Hole,
}

#[derive(Debug, Clone)]
pub enum ArgF<I, E> {
  Id(Vec<I>),
  Ty(E),
  IdTy(Vec<I>, E),
}

#[derive(Debug, Clone, Copy)]
pub enum Vis {
  Explicit,
  Implicit,
}

type ArgVis<I, E> = (Vis, ArgF<I, E>);

#[derive(Debug, Clone)]
pub struct DefF<I, E> {
  pub name: I,
  pub args: Vec<ArgVis<I, E>>,
  pub ty: Option<E>,
  pub body: E,
}

#[derive(Debug, Clone)]
pub enum ContextF<I, E> {
  Ref(I, E),
}

#[derive(Debug, Clone)]
pub enum DeclF<I, D, Ds, E> {
  Context(ContextF<I, E>, D),
  Module(Option<I>, Ds),
  Open(ModRef<I>),
  Def(DefF<I, E>),
  Syntax(SyntaxDecl),
}

#[derive(Debug, Clone)]
pub struct Id<'a>(&'a str);

impl<'a> Id<'a> {
  pub fn new(s: &'a str) -> Self {
    Id(s)
  }
  pub fn as_str(&self) -> &'a str {
    self.0
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenPos {
  Pos(u32, u16),
  EoL(u32),
  EoF,
}

impl TokenPos {
  pub fn to_string(&self) -> String {
    match self {
      TokenPos::Pos(ln, col) => format!("L{} C{}", ln + 1, col + 1),
      TokenPos::EoL(ln) => format!("End of L{}", ln + 1),
      TokenPos::EoF => "End of file".to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Base<S, I, Ds, E> {
  Lit(Literal<S>),
  Var(I),
  Prim(S),
  Extern(ModRef<I>, I),
  Let(Ds, E),
  Syntax(Syntax, E),
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
pub enum ExprF<S, I, Ds, E> {
  Base(Base<S, I, Ds, E>),
  FunctionE(FunctionE<I, E>),
  FunctionI(FunctionI<I, E>),
  Tuples(Tuples<E>),
  Objects(Objects<I, E>),
  Contextual(Contextual<I, E>),
  Inductive(Inductive<I, E>),
  Coinductive(Coinductive<I, E>),
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<&'a str, Id<'a>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Decl<'a>(
  pub DeclF<Id<'a>, Box<Decl<'a>>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>>,
  pub TokenPos,
);

pub type Context<'a> = ContextF<Id<'a>, Box<Expr<'a>>>;
pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
