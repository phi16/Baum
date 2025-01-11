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
  Num(String),
  Chr(String),
  Str(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Lit(Literal),
  Var(Id),
  Ann(Box<Expr>, Box<Expr>),
  Fun(Box<Expr>, Box<Expr>),
  FunDep(Id, Box<Expr>, Box<Expr>),
  Lam(Id, Box<Expr>),
  LamTy(Id, Box<Expr>, Box<Expr>),
  Uni(),
  App(Box<Expr>, Box<Expr>),
  Hole,
  Prim(String),
  Let(Vec<Decl>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Decl {
  Def(Id, Box<Expr>),
}
