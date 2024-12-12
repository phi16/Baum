#[derive(Debug, Clone)]
pub enum Id {
  Name(String),
}

#[derive(Debug, Clone)]
pub enum Literal {
  Int(i64),
  Str(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Lit(Literal),
  Var(Id),
  Lam(Vec<Id>, Box<Expr>),
  App(Box<Expr>, Vec<Box<Expr>>),
  Hole,
  Prim(String),
  Let(Decl, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Associativity {
  L,
  R,
  N,
}
type Precedence = u32;

#[derive(Debug, Clone)]
pub enum Decl {
  Def(Id, Box<Expr>),
  Mod(Id, Vec<Box<Decl>>),
  Infix(Associativity, Precedence, Id),
}
