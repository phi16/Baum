use crate::types::common::*;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum ValF<PTag, STag, V> {
  Hole,
  Bind(BindId),
  Uni,

  Pi(PTag, Vis, BindId, V, V),
  Lam(PTag, Vis, BindId, V, V),
  App(PTag, Vis, BindId, V),

  Sigma(STag, Vec<(NameId, BindId, V)>),
  Obj(STag, Vec<(NameId, V)>),
  Prop(STag, BindId, NameId),
}

#[derive(Debug, Clone)]
pub struct Val<P, S>(pub ValF<P, S, Rc<Val<P, S>>>);

pub type Term<P, S> = Val<P, S>;
pub type Type<P, S> = Val<P, S>;
