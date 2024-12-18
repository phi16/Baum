#[derive(Debug, Clone)]
pub struct Id(String);

impl Id {
  pub fn new(s: &str) -> Self {
    Id(s.to_string())
  }
  pub fn as_str(&self) -> &str {
    &self.0
  }
}

#[derive(Debug, Clone)]
pub enum Literal {
  Num(String), // TODO
  Chr(String), // TODO
  Str(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Lit(Literal),
  Var(Vec<Id>),
  Lam(Vec<Id>, Box<Expr>),
  App(Box<Expr>, Vec<Expr>),
  Hole,
  Prim(String),
  Let(Vec<Decl>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Associativity {
  L,
  R,
  N,
}
type Precedence = u32; // TODO: floating?

#[derive(Debug, Clone)]
pub enum Decl {
  Def(Id, Box<Expr>),
  Mod(Id, Vec<Decl>),
  Infix(Associativity, Precedence, Id),
}
