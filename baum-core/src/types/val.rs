use crate::types::common::*;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdF {
  Hole(HoleId),
  Bind(BindId),
  Def(DefId),
}

#[derive(Debug, Clone)]
pub enum ContF<PTag, STag, V> {
  App(PTag, Vis, V),
  Prop(STag, NameId),
}

#[derive(Debug, Clone)]
pub enum ValF<PTag, STag, C, V> {
  Id(IdF, Vec<C>),
  Uni,

  Pi(PTag, Vis, BindId, V, V),
  Lam(PTag, Vis, BindId, V, V),

  Sigma(STag, Vec<(NameId, BindId, V)>),
  Obj(STag, Vec<(NameId, V)>),
}

#[derive(Debug, Clone)]
pub struct Val<P, S>(pub ValF<P, S, ContF<P, S, Rc<Val<P, S>>>, Rc<Val<P, S>>>);

pub type Conts<P, S> = Vec<ContF<P, S, Rc<Val<P, S>>>>;

pub type RV<P, S> = Rc<Val<P, S>>;
pub type Term<P, S> = RV<P, S>;
pub type Type<P, S> = RV<P, S>;
