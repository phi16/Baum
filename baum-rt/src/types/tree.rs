use crate::types::common::{Id, Name};
use baum_core::types::literal::Literal;
use std::rc::Rc;

pub type Scope = Vec<Id>;

#[derive(Debug, Clone)]
pub enum TreeF<T> {
  Var(Id),
  Unit,
  Prim(String),
  Lit(Literal),
  Let(Vec<(Id, T)>, T),

  Lam(Id, Scope, T),
  App(T, T),
  Obj(Vec<(Name, T)>),
  Prop(T, Name),
}

#[derive(Debug, Clone)]
pub struct Tree(pub TreeF<Rc<Tree>>);
