use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindId(pub u32);

impl std::fmt::Debug for BindId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "%{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId(pub u32);

impl std::fmt::Debug for NameId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

#[derive(Debug, Clone)]
pub enum Vis {
  Explicit,
  Implicit,
}

#[derive(Debug, Clone)]
pub enum ExprF<PTag, STag, E> {
  Hole,
  Bind(BindId),
  Ann(E, E),
  Synth(E),
  Uni,

  Let(Vec<(BindId, E)>, E),

  Pi(PTag, Vis, Option<BindId>, E, E),
  Lam(PTag, Vis, BindId, E, E),
  App(PTag, Vis, E, E),

  Sigma(STag, Vec<(NameId, Option<BindId>, E)>),
  Obj(STag, Vec<(NameId, E)>),
  Prop(STag, NameId, E),
}

#[derive(Debug, Clone)]
pub struct Expr<PTag, STag>(pub ExprF<PTag, STag, Rc<Expr<PTag, STag>>>);

#[derive(Debug, Clone)]
pub struct Program<PTag, STag> {
  pub defs: Vec<(BindId, Rc<Expr<PTag, STag>>)>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
