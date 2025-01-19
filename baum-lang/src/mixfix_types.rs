use std::cmp::Ordering;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrecEps {
  NegEps,
  Zero,
  PosEps,
}

#[derive(Debug, Clone)]
pub struct PrecLevel(pub Vec<i16>);

impl PartialEq for PrecLevel {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl Eq for PrecLevel {}

impl PartialOrd for PrecLevel {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.cmp(other).into()
  }
}

impl Ord for PrecLevel {
  fn cmp(&self, other: &Self) -> Ordering {
    let mut i = 0;
    loop {
      match self.0[i].cmp(&other.0[i]) {
        Ordering::Equal => i += 1,
        o => return o,
      }

      if i == self.0.len() {
        match other.0[i..].iter().find(|&&x| x != 0) {
          Some(x) => return 0.cmp(&x),
          None => return Ordering::Equal,
        }
      }
      if i == other.0.len() {
        match self.0[i..].iter().find(|&&x| x != 0) {
          Some(x) => return x.cmp(&0),
          None => return Ordering::Equal,
        }
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
  Level(PrecLevel, PrecEps),
  Terminal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Syntax {
  Token(String),
  Nat,
  Str,
  Id,
  Expr,
  Defs,
  Decls,
  May(Rc<Syntax>),
  Rep1(Rc<Syntax>),
  Sep0(Rc<Syntax>, String),
  Sep1(Rc<Syntax>, String),
  Sep2(Rc<Syntax>, String),
  Seq(Rc<Syntax>, Rc<Syntax>),
  Seqs(Vec<Syntax>),
}

impl Syntax {
  pub fn get_first_token(&self) -> Option<String> {
    match self {
      Syntax::Token(s) => Some(s.clone()),
      Syntax::May(s) => None,
      Syntax::Rep1(s) => s.get_first_token(),
      Syntax::Sep0(s, _) => None,
      Syntax::Sep1(s, _) => s.get_first_token(),
      Syntax::Sep2(s, _) => s.get_first_token(),
      Syntax::Seq(s1, _) => s1.get_first_token(),
      Syntax::Seqs(ss) => ss[0].get_first_token(),
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxDecl {
  pub left: Precedence,
  pub right: Precedence,
  pub syntax: Syntax,
}

impl SyntaxDecl {
  pub fn new(left: Precedence, right: Precedence, syntax: Syntax) -> Self {
    SyntaxDecl {
      left,
      right,
      syntax,
    }
  }
}
