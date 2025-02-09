use std::collections::HashMap;
use std::rc::Rc;

pub struct Nat(pub u32); // TODO: BigUint
pub struct Rat {
  // TODO: BigRational
  pub denom: u32,
  pub base: u8,
  pub exponent: i32,
}
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}

pub type Id = String;
pub type I = u32;

type E = Rc<Expr>;

// TODO: decompose all
pub enum Expr {
  Hole,
  Var(I),
  Ext(Vec<I>, I),

  Ann(E, E),
  Uni,

  Lit(Literal),
  Let(Vec<Decl>, E),

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

#[derive(Debug, Clone)]
pub enum Vis {
  Explicit,
  Implicit,
}
pub enum ModRef {
  Import(String),
  App(Vec<I>, Vec<(Vis, E)>),
}
pub enum ModDef {
  Decls(Vec<Decl>),
  Ref(ModRef),
}
pub struct ModDecl {
  pub name: I,
  pub params: Vec<(Vis, I, E)>,
}
pub enum Decl {
  Local(Vec<Decl>),
  ModDef(ModDecl, ModDef),
  Open(ModRef),
  Def(I, E),
}
pub struct Program {
  pub decls: Vec<Decl>,
  pub symbols: HashMap<I, Id>,
}
