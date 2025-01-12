type Mod<I> = Vec<I>;

#[derive(Debug, Clone)]
pub enum Literal<S> {
  Num(S),
  Chr(S),
  Str(S),
  Hole,
}

#[derive(Debug, Clone)]
pub enum Arg<I, E> {
  Id(Vec<I>),
  Ty(E),
  IdTy(Vec<I>, E),
}

#[derive(Debug, Clone)]
pub enum Vis {
  Explicit,
  Implicit,
}

type ArgVis<I, E> = (Vis, Arg<I, E>);

#[derive(Debug, Clone)]
pub struct Def<I, E> {
  pub name: I,
  pub args: Vec<ArgVis<I, E>>,
  pub ty: Option<E>,
  pub body: E,
}

#[derive(Debug, Clone)]
pub enum Context<I, E> {
  Ref(I, E),
}

type Syntax = ();

#[derive(Debug, Clone)]
pub enum ExprF<S, I, Ds, E> {
  Prim(S),
  Lit(Literal<S>),
  Var(I),

  PiE(Vec<Arg<I, E>>, E),
  LamE(Vec<Arg<I, E>>, E),
  AppE(E, E),

  PiI(Vec<Arg<I, E>>, E),
  LamI(Vec<Arg<I, E>>, E),
  AppI(E, E),

  TupleTy(Vec<E>),
  TupleCon(Vec<E>),
  Proj(E, u32),

  Sigma(Vec<Arg<I, E>>),
  Obj(Vec<Def<I, E>>),
  Prop(E, I),

  Subst(Vec<Def<I, E>>, E),
  Extern(Mod<I>, I),
  Mu(I, E, Vec<(I, E)>),
  Nu(I, E, Vec<(I, E)>),

  Let(Ds, E),
  Where(E, Ds),
}

#[derive(Debug, Clone)]
pub enum DeclF<I, D, Ds, E> {
  Context(Context<I, E>, D),
  Module(Option<I>, Ds),
  Def(Def<I, E>),
  Syntax(Syntax),
  Open(Mod<I>),
}

#[derive(Debug, Clone)]
pub struct Id<'a>(pub &'a str);

impl<'a> Id<'a> {
  pub fn new(s: &'a str) -> Self {
    Id(s)
  }
  pub fn as_str(&self) -> &'a str {
    self.0
  }
}

#[derive(Debug, Clone, Copy)]
pub enum TokenPos {
  Pos(u32, u16),
  EoL(u32),
  EoF,
}

impl TokenPos {
  pub fn to_string(&self) -> String {
    match self {
      TokenPos::Pos(ln, col) => format!("L{} C{}", ln, col),
      TokenPos::EoL(ln) => format!("End of L{}", ln),
      TokenPos::EoF => "EoF".to_string(),
    }
  }
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

pub type Ctx<'a> = Context<Id<'a>, Box<Expr<'a>>>;
