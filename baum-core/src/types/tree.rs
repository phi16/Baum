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
pub struct PTag {
  pub vis: Vis,
  pub is_mod_param: bool,
}

#[derive(Debug, Clone)]
pub struct STag {
  pub is_tuple: bool,
  pub is_mod: bool,
}

#[derive(Debug, Clone)]
pub enum ExprF<E> {
  Hole,
  Bind(BindId),
  Ann(E, E),
  Uni,

  Let(Vec<(BindId, E)>, E),

  Pi(PTag, Option<BindId>, E, E),
  Lam(PTag, BindId, E, E),
  App(PTag, E, E),

  Sigma(STag, Vec<(NameId, Option<BindId>, E)>),
  Obj(STag, Vec<(NameId, E)>),
  Prop(STag, NameId, E),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprF<Rc<Expr>>);

#[derive(Debug, Clone)]
pub struct Program {
  pub defs: Vec<(BindId, Rc<Expr>)>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
