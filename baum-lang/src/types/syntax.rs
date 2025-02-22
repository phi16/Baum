use crate::types::precedence::Precedence;
use crate::types::regex::{Regex, Terminal};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Syntax<T> {
  pub left: Precedence,
  pub right: Precedence,
  pub regex: Rc<Regex>,
  pub t: T,
}

impl<T> std::fmt::Debug for Syntax<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Syn({:?}, {:?}) {:?}", self.left, self.right, self.regex)
  }
}

impl<T> Syntax<T> {
  pub fn new(left: Precedence, right: Precedence, regex: Rc<Regex>, t: T) -> Self {
    Syntax {
      left,
      right,
      regex,
      t,
    }
  }
}

#[derive(Clone)]
pub struct SyntaxTable<T> {
  pres: HashMap<String, Vec<Syntax<T>>>, // starts from token
  lits: Vec<Syntax<T>>,                  // literals
  opes: HashMap<String, Vec<Syntax<T>>>, // starts from expr and token
  apps: Vec<Syntax<T>>,                  // expr expr
}

// Note: this is necessary to avoid T: Debug bound
impl<T> std::fmt::Debug for SyntaxTable<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("SyntaxTable")
      .field("pres", &self.pres)
      .field("lits", &self.lits)
      .field("opes", &self.opes)
      .field("apps", &self.apps)
      .finish()
  }
}

impl<T> SyntaxTable<T> {
  pub fn new() -> Self {
    SyntaxTable {
      pres: HashMap::new(),
      lits: Vec::new(),
      opes: HashMap::new(),
      apps: Vec::new(),
    }
  }

  pub fn def(&mut self, prec: &str, seqs: Rc<Regex>, t: T) {
    let (left, right) = Precedence::parse(prec).unwrap();
    self.add(Syntax::new(left, right, seqs, t));
  }

  pub fn add(&mut self, syn: Syntax<T>) {
    let seqs = &syn.regex;
    let v = match &**seqs {
      Regex::Terminal(Terminal::Token(ref s)) => {
        self.pres.entry(s.to_string()).or_insert(Vec::new())
      }
      Regex::Seq(first, cont) => match **first {
        Regex::Terminal(Terminal::Token(ref s)) => {
          self.pres.entry(s.to_string()).or_insert(Vec::new())
        }
        Regex::Expr => match **cont {
          Regex::Terminal(Terminal::Token(ref s)) => {
            self.opes.entry(s.to_string()).or_insert(Vec::new())
          }
          Regex::Expr => &mut self.apps,
          Regex::Seq(ref second, _) => match **second {
            Regex::Terminal(Terminal::Token(ref s)) => {
              self.opes.entry(s.to_string()).or_insert(Vec::new())
            }
            _ => panic!(),
          },
          _ => panic!(),
        },
        _ => &mut self.lits,
      },
      _ => &mut self.lits,
    };
    eprintln!("Adding {:?} to {:?}", syn, v);
    v.push(syn);
  }

  #[allow(dead_code)]
  pub fn dump(&self) {
    println!("[Prefixs]");
    for syn in &self.pres {
      println!("{:?}", syn);
    }
    println!("[Literals]");
    for syn in &self.lits {
      println!("{:?}", syn);
    }
    println!("[Operators]");
    for syn in &self.opes {
      println!("{:?}", syn);
    }
    println!("[Applications]");
    for syn in &self.apps {
      println!("{:?}", syn);
    }
  }

  pub fn choose_pre(&self, s: &str) -> Option<&Vec<Syntax<T>>> {
    self.pres.get(s)
  }
  pub fn lits(&self) -> &Vec<Syntax<T>> {
    &self.lits
  }
  pub fn choose_ope(&self, s: &str) -> Option<&Vec<Syntax<T>>> {
    self.opes.get(s)
  }
  pub fn apps(&self) -> &Vec<Syntax<T>> {
    &self.apps
  }

  pub fn is_pre_head(&self, s: &str) -> bool {
    self.pres.contains_key(s)
  }
  pub fn is_ope_head(&self, s: &str) -> bool {
    self.opes.contains_key(s)
  }

  pub fn merge(&mut self, other: SyntaxTable<T>) {
    for (k, v) in other.pres {
      self.pres.entry(k).or_insert(Vec::new()).extend(v);
    }
    self.lits.extend(other.lits);
    for (k, v) in other.opes {
      self.opes.entry(k).or_insert(Vec::new()).extend(v);
    }
    self.apps.extend(other.apps);
  }
}

use crate::types::tree::SyntaxElem;
use baum_front::types::tree as front;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ElemId(pub u16);

#[derive(Debug, Clone)]
pub enum ElemToken {
  Token,
  Ident(ElemId),
  Nat,
  Rat,
  Chr,
  Str,
  Expr(ElemId),
}

#[derive(Debug, Clone)]
pub enum LookupId {
  InSyntax(ElemId),
  General(front::Id),
}

#[derive(Debug, Clone)]
pub struct SyntaxExpr(pub front::ExprF<LookupId, Rc<SyntaxExpr>>);

pub type SyntaxHandler<'a> =
  Rc<dyn for<'b> Fn(&'b Vec<SyntaxElem<'a>>) -> Result<(Vec<ElemToken>, SyntaxExpr), String>>;
