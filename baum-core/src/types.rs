use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Nat(pub u32); // TODO: BigUint
#[derive(Debug, Clone)]
pub struct Rat {
  // TODO: BigRational
  pub denom: u32,
  pub base: u8,
  pub exponent: i32,
}
#[derive(Debug, Clone)]
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub u32);

impl std::fmt::Debug for Id {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

type I = Id;
type E = Rc<Expr>;

// TODO: decompose all
#[derive(Debug, Clone)]
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

  TupleTy(Vec<(Option<I>, E)>),
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
#[derive(Debug, Clone)]
pub enum ModRef {
  Import(String),
  App(Vec<I>, Vec<(Vis, E)>),
}
#[derive(Debug, Clone)]
pub enum ModDef {
  Decls(Vec<Decl>),
  Ref(ModRef),
}
#[derive(Debug, Clone)]
pub struct ModDecl {
  pub name: I,
  pub params: Vec<(Vis, I, E)>,
}
#[derive(Debug, Clone)]
pub enum Decl {
  ModDef(ModDecl, ModDef),
  Open(ModRef),
  Def(I, E),
}
#[derive(Debug, Clone)]
pub struct Program {
  pub decls: Vec<Decl>,
  pub symbols: HashMap<I, String>,
}
