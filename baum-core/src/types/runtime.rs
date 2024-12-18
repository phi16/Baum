use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunLoc(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefLoc(pub u32);
#[derive(Debug, Clone, Copy)]
pub struct OpLoc(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct ArgLoc(pub u16);
#[derive(Debug, Clone, Copy)]
pub struct EnvLoc(pub u16);

#[derive(Debug, Clone)]
pub enum L {
  Num(String),
  Chr(String),
  Str(String),
}

#[derive(Debug, Clone)]
pub enum E {
  DefRef(DefLoc),
  ArgRef(ArgLoc),
  EnvRef(EnvLoc),
  Lit(L),
  Cl(FunLoc, Vec<OpLoc>), // fun, env
  App(OpLoc, Vec<OpLoc>), // cl, args
  Hole,
  Prim(String),
}

#[derive(Debug, Clone)]
pub struct Body {
  pub ls: Vec<E>, // OpLoc -> E
  pub result: OpLoc,
}

#[derive(Debug, Clone)]
pub struct Fun {
  pub args: Vec<String>,
  pub env: Vec<String>, // may include "a.b.c"; just printing purpose
  pub body: Body,
}

#[derive(Debug, Clone)]
pub enum D {
  Def(Body),
  Mod(HashMap<String, DefLoc>),
}

#[derive(Debug, Clone)]
pub struct Runtime {
  pub funs: HashMap<FunLoc, Fun>,
  pub fun_count: u32,
  pub defs: HashMap<DefLoc, D>,
  pub def_count: u32,
  pub lookup: HashMap<String, DefLoc>,
}
