pub use crate::types::ast::*;
pub use crate::types::mixfix;
pub use crate::types::token::*;
pub use crate::types::tracker::*;
use baum_core::types as core;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id<'a>(&'a str);

impl<'a> Id<'a> {
  pub fn new(s: &'a str) -> Self {
    Id(s)
  }
  pub fn as_str(&self) -> &'a str {
    self.0
  }
}

#[derive(Debug, Clone)]
pub enum SyntaxElem<'a> {
  Token(&'a str),
  Ident(Id<'a>),
  Nat(&'a str),
  Rat(&'a str),
  Chr(&'a str),
  Str(&'a str),
  Expr(Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum CoreElem<'a> {
  Token(&'a str),
  Ident(core::Id),
  Nat(&'a str),
  Rat(&'a str),
  Chr(&'a str),
  Str(&'a str),
  Expr(core::Expr),
}

pub trait SyntaxHandler<'a> {
  fn convert_i(&mut self, id: &Id<'a>) -> core::Id;
  fn convert_e(&mut self, e: &Expr<'a>) -> core::Expr;
  fn convert_se(&mut self, e: &SyntaxElem<'a>) -> CoreElem<'a>;
}

pub type SyntaxInterpreter<'a> =
  Rc<dyn Fn(Vec<CoreElem<'a>>, &Vec<Id<'a>>, &mut dyn SyntaxHandler<'a>) -> core::Expr + 'a>;
pub type Syntax<'a> = mixfix::Syntax<SyntaxInterpreter<'a>>;
pub type SyntaxTable<'a> = mixfix::SyntaxTable<SyntaxInterpreter<'a>>;

// TODO
pub struct TokenRange {
  pub begin: TokenPos,
  pub end: TokenPos,
}

#[derive(Debug, Clone)]
pub struct ModDef<'a>(
  pub ModDefF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Decl<'a>(
  pub DeclF<&'a str, Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, Box<ModDef<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<Id<'a>, Vec<Decl<'a>>, Box<Expr<'a>>, Syntax<'a>, SyntaxElem<'a>>,
  pub TokenPos,
);

pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
pub type ModRef<'a> = ModRefF<&'a str, Id<'a>, Box<Expr<'a>>>;

#[derive(Debug, Clone)]
pub struct Env<'a> {
  pub syntax: SyntaxTable<'a>,
  pub modules: HashMap<Id<'a>, Rc<Env<'a>>>,
}

impl<'a> Env<'a> {
  pub fn new() -> Self {
    Env {
      syntax: SyntaxTable::new(),
      modules: HashMap::new(),
    }
  }

  pub fn from_syntax(syntax: SyntaxTable<'a>) -> Self {
    Env {
      syntax,
      modules: HashMap::new(),
    }
  }

  pub fn add_syntax(&mut self, syntax: Syntax<'a>) {
    self.syntax.add(syntax);
  }
  pub fn add_module(&mut self, name: Id<'a>, e: Rc<Env<'a>>) -> bool {
    self.modules.insert(name, e).is_none()
  }

  pub fn get_mod(&self, id: &'a str) -> Option<&Rc<Env<'a>>> {
    self.modules.get(&Id::new(id))
  }
  pub fn lookup(&self, ids: &[Id<'a>]) -> Option<&Rc<Env<'a>>> {
    let mut env = self.modules.get(&ids[0])?;
    for id in ids.iter().skip(1) {
      match env.modules.get(id) {
        Some(e) => {
          env = e;
        }
        None => return None,
      }
    }
    Some(env)
  }
  pub fn merge(&mut self, other: Rc<Env<'a>>) -> Result<(), Vec<Id<'a>>> {
    let mut conflicts = Vec::new();
    self.modules.keys().for_each(|id| {
      if other.modules.contains_key(id) {
        conflicts.push(id.clone());
      }
    });
    if !conflicts.is_empty() {
      return Err(conflicts);
    }
    self.modules.extend(other.modules.clone());
    self.syntax.merge(other.syntax.clone());
    Ok(())
  }

  pub fn is_modname(&self, id: &'a str) -> bool {
    self.modules.contains_key(&Id(id))
  }
  pub fn is_leading_opname(&self, id: &'a str) -> bool {
    self.syntax.is_pre_head(id)
  }
  pub fn is_trailing_opname(&self, id: &'a str) -> bool {
    self.syntax.is_ope_head(id)
  }
}

#[derive(Debug, Clone)]
pub enum Role {
  Expr,
  Ident,
}

pub fn fv<'a>(e: &Expr<'a>) -> Option<Vec<(Id<'a>, Role)>> {
  fn fv_internal<'a>(e: &Expr<'a>, v: &mut Vec<(Id<'a>, Role)>, invalid: &mut bool) {
    match e.0 {
      ExprF::Var(ref id) => {
        v.push((id.clone(), Role::Expr));
      }
      ExprF::Syntax(_, _, ref elems) => {
        for elem in elems {
          match elem {
            SyntaxElem::Ident(ref id) => {
              v.push((id.clone(), Role::Ident));
            }
            SyntaxElem::Expr(ref e) => {
              fv_internal(e, v, invalid);
            }
            _ => {}
          }
        }
      }
      ExprF::Let(_, _) => {
        *invalid = true;
      }
      _ => {}
    }
  }
  let mut v = Vec::new();
  let mut invalid = false;
  fv_internal(e, &mut v, &mut invalid);
  if invalid {
    None
  } else {
    Some(v)
  }
}
