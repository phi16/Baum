pub use crate::types::common::*;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum ExprF<PTag, STag, E> {
  Hole,
  Bind(BindId),
  Ann(E, E),
  Def(E),
  Uni,

  Let(Vec<(BindId, E)>, E),

  Pi(PTag, Vis, Option<BindId>, E, E),
  Lam(PTag, Vis, BindId, E, E),
  App(PTag, Vis, E, E),

  Sigma(STag, Vec<(NameId, Option<BindId>, E)>),
  Obj(STag, Vec<(NameId, E)>),
  Prop(STag, E, NameId),
}

#[derive(Debug, Clone)]
pub struct Expr<PTag, STag>(pub ExprF<PTag, STag, Rc<Expr<PTag, STag>>>);

#[derive(Debug, Clone)]
pub struct Program<PTag, STag> {
  pub defs: Vec<(BindId, Rc<Expr<PTag, STag>>)>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
