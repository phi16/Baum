use crate::types::common::*;
use crate::types::level::*;
use crate::types::literal::Literal;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum CExprF<PTag, STag, E> {
  Hole(HoleId),
  Bind(BindId),
  Def(DefId, Vec<Level>),
  Ann(E, E),
  Uni(Level),
  Prim(String),
  Lit(Literal),
  Let(Vec<(DefId, Solution, E)>, E),

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
pub struct CExpr<P, S>(pub CExprF<P, S, RE<P, S>>);
pub type RE<P, S> = Rc<CExpr<P, S>>;

#[derive(Debug, Clone)]
pub enum ContF<PTag, STag, V> {
  App(PTag, Vis, V),
  Prop(STag, NameId),
}

#[derive(Debug, Clone)]
pub enum ValF<PTag, STag, V, E, D> {
  Hole(HoleId),
  Neu(BindId, Vec<ContF<PTag, STag, V>>),
  Lazy(DefId, Vec<Level>, Vec<ContF<PTag, STag, V>>),
  Prim(String, Vec<ContF<PTag, STag, V>>),
  Lit(Literal),
  Uni(Level),

  Pi(PTag, Vis, BindId, V, E, D),
  Lam(PTag, Vis, BindId, V, E, D),

  Sigma0(STag),
  Obj0(STag),
  Sigma(STag, (NameId, BindId, V), E, Vec<(NameId, BindId, D)>),
  Obj(STag, (NameId, V), Vec<(NameId, V)>),
}

#[derive(Debug, Clone)]
pub struct Env<P, S> {
  // Note: lookup elements don't refer the binds in the lookup,
  //       but define elements may do so.
  pub lookup: HashMap<BindId, Term<P, S>>,
  pub define: HashMap<DefId, Term<P, S>>,
}

impl<P, S> Env<P, S> {
  pub fn new() -> Self {
    Self {
      lookup: HashMap::new(),
      define: HashMap::new(),
    }
  }

  pub fn add_bind(&mut self, i: BindId, val: Term<P, S>) {
    self.lookup.insert(i, val);
  }

  pub fn add_def(&mut self, i: DefId, val: Term<P, S>) {
    self.define.insert(i, val);
  }

  pub fn lookup_bind(&self, i: &BindId) -> Option<&RV<P, S>> {
    self.lookup.get(i)
  }

  pub fn lookup_def(&self, i: &DefId) -> Option<&RV<P, S>> {
    self.define.get(i)
  }
}

#[derive(Debug, Clone)]
pub struct Val<P, S>(pub ValF<P, S, RV<P, S>, Env<P, S>, RE<P, S>>);
pub type RV<P, S> = Rc<Val<P, S>>;

pub type Conts<P, S> = Vec<ContF<P, S, RV<P, S>>>;

pub type Term<P, S> = RV<P, S>;
pub type Type<P, S> = RV<P, S>;
