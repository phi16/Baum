use crate::types::syntax;
use crate::types::tree::{Id, SyntaxId};
use std::collections::HashMap;
use std::rc::Rc;

pub type Syntax = syntax::Syntax<SyntaxId>;
pub type SyntaxTable = syntax::SyntaxTable<SyntaxId>;

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
      env = env.modules.get(id)?;
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
    self.modules.contains_key(&Id::new(id))
  }
  pub fn is_opname(&self, id: &'a str) -> bool {
    self.is_leading_opname(id) || self.is_trailing_opname(id)
  }
  pub fn is_leading_opname(&self, id: &'a str) -> bool {
    self.syntax.is_pre_head(id)
  }
  pub fn is_trailing_opname(&self, id: &'a str) -> bool {
    self.syntax.is_ope_head(id)
  }
}
