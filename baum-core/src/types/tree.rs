pub use crate::types::common::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ExprF<PTag, STag, H, E> {
  Hole(H),
  Bind(BindId),
  Def(DefId),
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

#[derive(Debug, Clone)]
pub struct Expr<PTag, STag>(pub ExprF<PTag, STag, (), Box<Expr<PTag, STag>>>);

#[derive(Debug, Clone)]
pub struct Program<PTag, STag> {
  pub defs: Vec<(DefId, Box<Expr<PTag, STag>>)>,
  pub def_symbols: HashMap<DefId, String>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
