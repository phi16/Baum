use crate::types::common::*;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdF {
  Hole(HoleId),
  Bind(BindId),
  Def(DefId), // not a "neutral" term though...
}

#[derive(Debug, Clone)]
pub enum CExprF<PTag, STag, E> {
  Id(IdF),
  Ann(E, E),
  Uni,
  Let(Vec<(DefId, E)>, E),

  Pi(PTag, Vis, BindId, E, E),
  Lam(PTag, Vis, BindId, E, E),
  App(PTag, Vis, E, E),

  Sigma(STag, Vec<(NameId, BindId, E)>),
  Obj(STag, Vec<(NameId, E)>),
  Prop(STag, E, NameId),
}

// Checked Expr
#[derive(Debug, Clone)]
pub struct CE<P, S>(pub CExprF<P, S, Rc<CE<P, S>>>);

#[derive(Debug, Clone)]
pub enum ContF<PTag, STag, V> {
  App(PTag, Vis, V),
  Prop(STag, NameId),
}

#[derive(Debug, Clone)]
pub enum ValF<PTag, STag, V> {
  Neu(IdF, Vec<ContF<PTag, STag, V>>),
  Cl(Rc<HashMap<BindId, V>>, V),
  Uni,

  Pi(PTag, Vis, BindId, V, V),
  Lam(PTag, Vis, BindId, V, V),

  Sigma(STag, Vec<(NameId, BindId, V)>),
  Obj(STag, Vec<(NameId, V)>),
}

#[derive(Debug, Clone)]
pub struct Val<P, S>(pub ValF<P, S, Rc<Val<P, S>>>);

pub type Conts<P, S> = Vec<ContF<P, S, Rc<Val<P, S>>>>;

pub type RV<P, S> = Rc<Val<P, S>>;
pub type Term<P, S> = RV<P, S>;
pub type Type<P, S> = RV<P, S>;
