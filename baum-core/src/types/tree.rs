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
pub enum ExprF<DefI, LocI, NameI, E> {
  Hole,
  Var(LocI),
  Ann(E, E),
  Uni,

  Def(DefI),
  Let(Vec<(DefI, E)>, E),

  Pi(Option<LocI>, E, E),
  Lam(LocI, E, E),
  App(E, E),

  Sigma(Vec<(NameI, Option<LocI>, E)>),
  Obj(Vec<(NameI, E)>),
  Prop(NameI, E),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprF<DefId, BindId, NameId, Rc<Expr>>);

#[derive(Debug, Clone)]
pub struct Program {
  pub decls: Vec<(DefId, Rc<Expr>)>,
  pub def_symbols: HashMap<DefId, String>,
  pub bind_symbols: HashMap<BindId, String>,
  pub name_symbols: HashMap<NameId, String>,
}
