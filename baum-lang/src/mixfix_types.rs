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

impl Precedence {
  pub fn parse(s: &str) -> Option<(Self, Self)> {
    // 1.-2.3<
    unimplemented!()
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NonTerm {
  Expr,
  Def,
  Decls,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
  Token(String),
  Nat,
  Str,
  Id,
  Empty,
  Seq(Rc<Regex>, Rc<Regex>),
  Seqs(Vec<Rc<Regex>>),
  OrElse(Rc<Regex>, Rc<Regex>),
  Rep(Rc<Regex>),
  NonTerm(NonTerm),
}

impl Regex {
  pub fn id() -> Rc<Self> {
    Rc::new(Regex::Id)
  }
  pub fn e() -> Rc<Self> {
    Rc::new(Regex::NonTerm(NonTerm::Expr))
  }
  pub fn def() -> Rc<Self> {
    Rc::new(Regex::NonTerm(NonTerm::Def))
  }
  pub fn decls() -> Rc<Self> {
    Rc::new(Regex::NonTerm(NonTerm::Decls))
  }
  pub fn ids() -> Rc<Self> {
    Self::rep1(&Self::id())
  }
  pub fn token(s: &str) -> Rc<Self> {
    Rc::new(Regex::Token(s.to_string()))
  }

  pub fn seq(r1: &Rc<Regex>, r2: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Seq(r1.clone(), r2.clone()))
  }
  pub fn seqs(rs: Vec<&Rc<Regex>>) -> Rc<Self> {
    let rs = rs.into_iter().map(|r| r.clone()).collect();
    Rc::new(Regex::Seqs(rs))
  }
  pub fn rep1(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Seq(r.clone(), Rc::new(Regex::Rep(r.clone()))))
  }
  pub fn sep1(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Rc::new(Regex::Seq(
      r.clone(),
      Rc::new(Regex::Rep(Rc::new(Regex::Seq(
        Rc::new(Regex::Token(s.to_string())),
        r.clone(),
      )))),
    ))
  }
  pub fn sep2(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Rc::new(Regex::Seqs(vec![
      r.clone(),
      Rc::new(Regex::Token(s.to_string())),
      Self::sep1(r, s),
    ]))
  }
  pub fn may(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::OrElse(r.clone(), Rc::new(Regex::Empty)))
  }
}

#[derive(Debug, Clone)]
pub struct Syntax {
  pub left: Precedence,
  pub right: Precedence,
  pub regex: Regex,
}

impl Syntax {
  pub fn new(left: Precedence, right: Precedence, regex: Regex) -> Self {
    Syntax { left, right, regex }
  }
}
