use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

impl std::fmt::Debug for DefId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "@{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

impl std::fmt::Debug for LocalId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "%{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TagId(pub u32);

impl std::fmt::Debug for TagId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

#[derive(Debug, Clone)]
pub enum ExprF<DefI, LocI, TagI, E> {
  Hole,
  Var(LocI),
  Ann(E, E),
  Uni,

  Def(DefI),
  Let(Vec<(DefI, E)>, E),

  Pi(Option<LocI>, E, E),
  Lam(LocI, E, E),
  App(E, E),

  Sigma(Vec<(TagI, Option<LocI>, E)>),
  Obj(Vec<(TagI, E)>),
  Prop(TagI, E),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprF<DefId, LocalId, TagId, Rc<Expr>>);

#[derive(Debug, Clone)]
pub struct Program {
  pub decls: Vec<(DefId, Rc<Expr>)>,
  pub def_symbols: HashMap<DefId, String>,
  pub local_symbols: HashMap<LocalId, String>,
  pub tag_symbols: HashMap<TagId, String>,
}
