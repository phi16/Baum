pub use crate::types::decl::*;
pub use crate::types::expr::*;
pub use crate::types::mixfix::*;
pub use crate::types::token::*;
pub use crate::types::tracker::*;
pub use std::collections::HashMap;
pub use std::rc::Rc;

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
  Def(Box<Def<'a>>),
  Expr(Box<Expr<'a>>),
  Decls(Vec<Decl<'a>>),
}

// TODO
pub struct TokenRange {
  pub begin: TokenPos,
  pub end: TokenPos,
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<Id<'a>, (Syntax, Vec<SyntaxElem<'a>>)>,
  pub TokenPos,
);

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

pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;
pub type ModRef<'a> = ModRefF<&'a str, Id<'a>, Box<Expr<'a>>>;

#[derive(Debug, Clone)]
pub struct Env<'a> {
  pub syntax: SyntaxTable,
  pub modules: HashMap<Id<'a>, Rc<Env<'a>>>,
}

impl<'a> Env<'a> {
  pub fn new() -> Self {
    Env {
      syntax: SyntaxTable::new(),
      modules: HashMap::new(),
    }
  }

  pub fn from_syntax(syntax: SyntaxTable) -> Self {
    Env {
      syntax,
      modules: HashMap::new(),
    }
  }

  pub fn add_syntax(&mut self, syntax: Syntax) {
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
