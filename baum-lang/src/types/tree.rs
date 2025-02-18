pub use crate::types::token::TokenPos;
use crate::types::tree_base::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct TokenRange {
  pub begin: TokenPos,
  pub end: TokenPos,
}

#[derive(Debug, Clone)]
pub enum SyntaxId {
  Sys(u8),
  User(u16),
}

pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
pub type ModRef<'a> = ModRefF<&'a str, Id<'a>, Box<Expr<'a>>>;
pub type ModDef<'a> = ModDefF<Id<'a>, Box<Expr<'a>>>;
pub type ModBody<'a> = ModBodyF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>>;
pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type SynDef<'a> = SynDefF<&'a str, Id<'a>>;
pub type SyntaxElem<'a> = SyntaxElemF<&'a str, Id<'a>, Box<Expr<'a>>>;

pub type DeclInternal<'a> = DeclF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, SyntaxId>;
pub type ExprInternal<'a> = ExprF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, SyntaxId>;

#[derive(Debug, Clone)]
pub struct Decl<'a>(pub DeclInternal<'a>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct Expr<'a>(pub ExprInternal<'a>, pub TokenRange);
