use std::collections::HashMap;

pub struct Nat(u32); // TODOO: BigUint
pub struct Rat {
  // TODO: BigRational
  denom: u32,
  base: u8,
  exponent: i32,
}

type Id = String;
type I = u32;
type E = Box<Expr>;
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}
pub enum ModuleRef {
  Import(String),
  Sub(Box<ModuleRef>, Id),
}
pub enum Level {
  Nat(u8),
  Omega(u8),
  Infer,
}
pub enum Expr {
  Hole,
  Ann(E, E),
  Lit(Literal),
  Var(I),
  Ext(ModuleRef, Id),
  Let(Vec<(I, E)>, E), // um... needs module ref
  Uni(Level),

  PiE(I, E, E),
  LamE(I, E, E),
  AppE(E, E),

  PiI(I, E, E),
  LamI(I, E, E),
  AppI(E, E),

  TupleTy(Vec<E>),
  TupelCon(Vec<E>),
  Proj(u8, E),

  ObjTy(Vec<(I, E)>),
  Obj(Vec<(I, E)>),
  Prop(I, E),

  Mu(I, E, Vec<(I, E)>),

  Nu(I, E, Vec<(I, E)>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefRef(pub u32);

pub enum Vis {
  Explicit,
  Implicit,
}
pub struct Modules {
  params: Vec<(Vis, I, E)>,
  defs: HashMap<Id, DefRef>,
  subs: HashMap<Id, Modules>,
}
pub struct Program {
  defs: HashMap<DefRef, E>,
  mods: Modules,
  symbols: HashMap<I, Id>,
}
