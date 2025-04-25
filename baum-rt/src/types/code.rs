use crate::types::common::Name;
use baum_core::types::literal::Literal;

pub type OpIx = u32;
pub type FunIx = u16;
pub type EnvIx = u16;

#[derive(Debug, Clone)]
pub enum Ref {
  Env(EnvIx),
  Arg,
}

#[derive(Debug, Clone)]
pub enum Op<T> {
  Ref(Ref),
  Unit,
  Prim(String),
  Lit(Literal),

  Lam(FunIx, Vec<T>),
  App(T, T),
  Obj(Vec<(Name, T)>),
  Prop(T, Name),
}

#[derive(Debug, Clone)]
pub struct Code {
  pub env_count: u32,
  pub ops: Vec<Op<OpIx>>,
  pub ret: OpIx,
}

#[derive(Debug, Clone)]
pub struct Global {
  pub funs: Vec<Code>,
  pub main: FunIx,
}
