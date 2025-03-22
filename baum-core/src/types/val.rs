use crate::types::common::*;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum CExprF<PTag, STag, E> {
  Hole(HoleId),
  Bind(BindId),
  Def(DefId),
  Ann(E, E),
  Uni,
  Let(Vec<(DefId, E)>, E),

  Pi(PTag, Vis, BindId, E, E),
  Lam(PTag, Vis, BindId, E, E),
  App(PTag, Vis, E, E),

  Sigma0(STag),
  Obj0(STag),
  Sigma(STag, (NameId, BindId, E), Vec<(NameId, BindId, E)>),
  Obj(STag, (NameId, E), Vec<(NameId, E)>),
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
pub enum ValF<PTag, STag, V, E, D> {
  Hole(HoleId),
  Neu(BindId, Vec<ContF<PTag, STag, V>>),
  Lazy(DefId, Vec<ContF<PTag, STag, V>>),
  Uni,

  Pi(PTag, Vis, BindId, V, E, D),
  Lam(PTag, Vis, BindId, V, E, D),

  Sigma0(STag),
  Obj0(STag),
  Sigma(STag, (NameId, BindId, V), E, Vec<(NameId, BindId, D)>),
  Obj(STag, (NameId, V), Vec<(NameId, V)>),
}

pub type Env<P, S> = HashMap<BindId, RV<P, S>>;

#[derive(Debug, Clone)]
pub struct Val<P, S>(pub ValF<P, S, RV<P, S>, Env<P, S>, Rc<CE<P, S>>>);
pub type RV<P, S> = Rc<Val<P, S>>;

pub type Conts<P, S> = Vec<ContF<P, S, RV<P, S>>>;

pub type Term<P, S> = RV<P, S>;
pub type Type<P, S> = RV<P, S>;
