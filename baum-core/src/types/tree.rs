pub use crate::types::common::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ExprF<T, PTag, STag, E> {
  Hole,
  Bind(BindId),
  Def(DefId),
  Ann(E, E),
  Uni,
  Prim(String),
  Let(Vec<(DefId, T, E)>, E),

  Pi(PTag, Vis, BindId, E, E),
  Lam(PTag, Vis, BindId, E, E),
  App(PTag, Vis, E, E),

  Sigma(STag, Vec<(NameId, BindId, E)>),
  Obj(STag, Vec<(NameId, E)>),
  Prop(STag, E, NameId),
}

#[derive(Debug, Clone)]
pub struct Expr<T, PTag, STag>(pub ExprF<T, PTag, STag, Box<Expr<T, PTag, STag>>>);

#[derive(Debug, Clone)]
pub struct Program<T, PTag, STag> {
  pub defs: Vec<(DefId, T, Box<Expr<T, PTag, STag>>)>,
  pub def_symbols: HashMap<DefId, String>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
