pub use crate::types::decl::*;
pub use crate::types::expr::*;
pub use crate::types::mixfix::*;
pub use crate::types::token::*;
pub use crate::types::tracker::*;

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

#[derive(Debug, Clone)]
pub enum SyntaxElem<'a> {
  Token(&'a str),
  Ident(Id<'a>),
  Nat(&'a str),
  Str(&'a str),
  Def(Box<Def<'a>>),
  Expr(Box<Expr<'a>>),
  Decls(Vec<Decl<'a>>),
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<&'a str, Id<'a>, (Syntax, Vec<SyntaxElem<'a>>)>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Module<'a>(
  pub ModuleF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Decl<'a>(
  pub DeclF<Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, Box<Module<'a>>>,
  pub TokenPos,
);

pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
