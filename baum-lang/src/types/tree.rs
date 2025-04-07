use crate::types::token::TokenRange;
use crate::types::tree_base::*;

use super::token::TokenIx;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxId {
  User(u16),
  Dec,
  Num,
  Chr,
  Str,
  Hole,
  Uni,
  LamE,
  PiE,
  AppE,
  LamI,
  PiI,
  AppI,
  TupleTy,
  TupleCon,
  Proj,
  ObjTy,
  ObjCon,
  Prop,
}

pub type ModDef<'a> = ModDefF<Id<'a>, Arg<'a>>;
pub type Def<'a> = DefF<Id<'a>, Arg<'a>, Box<Expr<'a>>>;

pub type ArgInternal<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
pub type ModRefInternal<'a> = ModRefF<&'a str, Id<'a>, Box<Expr<'a>>>;
pub type ModBodyInternal<'a> = ModBodyF<Vec<Decl<'a>>, ModRef<'a>>;
pub type SynDefInternal<'a> = SynDefF<&'a str, Id<'a>>;
pub type SynElemInternal<'a> = SynElemF<&'a str, Id<'a>, Box<Expr<'a>>>;
pub type DeclInternal<'a> = DeclF<
  &'a str,
  Id<'a>,
  Vec<Decl<'a>>,
  Arg<'a>,
  Box<Expr<'a>>,
  SyntaxId,
  Vec<SynDef<'a>>,
  ModBody<'a>,
  ModRef<'a>,
>;
pub type ExprInternal<'a> = ExprF<Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, SyntaxId, Vec<SynElem<'a>>>;

#[derive(Debug, Clone)]
pub struct Arg<'a>(pub ArgInternal<'a>, pub TokenIx);

#[derive(Debug, Clone)]
pub struct Decl<'a>(pub DeclInternal<'a>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct Expr<'a>(pub ExprInternal<'a>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct SynDef<'a>(pub SynDefInternal<'a>, pub TokenIx);

#[derive(Debug, Clone)]
pub struct SynElem<'a>(pub SynElemInternal<'a>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct ModBody<'a>(pub ModBodyInternal<'a>, pub TokenRange);

#[derive(Debug, Clone)]
pub struct ModRef<'a>(pub ModRefInternal<'a>, pub TokenRange);
