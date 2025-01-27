pub mod decl;
pub mod expr;
pub mod mixfix;
pub mod parse;
pub mod token;
pub mod tracker;

use crate::types::decl::*;
use crate::types::expr::*;
use crate::types::parse::*;
use crate::types::token::*;

#[derive(Debug, Clone)]
pub enum SyntaxElems<'a> {
  Token(&'a str),
  Ident(Id<'a>),
  Def(Def<'a>),
  Expr(Expr<'a>),
  Decls(Vec<Decl<'a>>),
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<&'a str, Id<'a>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>, Vec<SyntaxElems<'a>>>,
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
