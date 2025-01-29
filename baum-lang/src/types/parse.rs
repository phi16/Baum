pub use crate::types::decl::*;
pub use crate::types::expr::*;
pub use crate::types::mixfix::*;
pub use crate::types::parse_base::*;
pub use crate::types::token::*;
pub use crate::types::tracker::*;

#[derive(Debug, Clone)]
pub enum SyntaxElem<'a> {
  Token(&'a str),
  Ident(Id<'a>),
  Nat(&'a str),
  Str(&'a str),
  Def(Def<'a>),
  Expr(Expr<'a>),
  Decls(Vec<Decl<'a>>),
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<&'a str, Id<'a>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>, Vec<SyntaxElem<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Module<'a>(
  pub ModuleF<&'a str, Id<'a>, Box<Vec<Decl<'a>>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Decl<'a>(
  pub DeclF<Id<'a>, Box<Decl<'a>>, Box<Expr<'a>>, Box<Module<'a>>>,
  pub TokenPos,
);

pub type Context<'a> = ContextF<Id<'a>, Box<Expr<'a>>>;
pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
