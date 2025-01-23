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
    // example: "1.-2.3<"
    // contains only: -.<>[0-9]
    if s == "" {
      return Some((Precedence::Terminal, Precedence::Terminal));
    }
    let mut ss = s.chars().peekable();
    let mut nums = Vec::new();
    let mut left_eps = PrecEps::Zero;
    loop {
      let negated = if ss.peek() == Some(&'-') {
        ss.next();
        true
      } else {
        false
      };
      let mut nat = 0;
      let c = *ss.peek()?;
      if !c.is_ascii_digit() {
        return None;
      }
      loop {
        let c = match ss.peek() {
          Some(c) if c.is_ascii_digit() => *c,
          _ => break,
        };
        ss.next();
        nat = nat * 10 + c.to_digit(10).unwrap() as i16;
      }
      if negated {
        nat = -nat;
      }
      nums.push(nat);
      match ss.next() {
        Some('.') => {}
        Some('<') => {
          left_eps = PrecEps::NegEps;
          break;
        }
        Some('>') => {
          left_eps = PrecEps::PosEps;
          break;
        }
        Some(_) => return None,
        None => break,
      }
    }
    if ss.next().is_some() {
      return None;
    }
    if nums.len() == 0 {
      return None;
    }
    let right_eps = match left_eps {
      PrecEps::NegEps => PrecEps::PosEps,
      PrecEps::PosEps => PrecEps::NegEps,
      PrecEps::Zero => PrecEps::Zero,
    };
    let left = Precedence::Level(PrecLevel(nums.clone()), left_eps);
    let right = Precedence::Level(PrecLevel(nums), right_eps);
    Some((left, right))
  }
}

#[cfg(test)]
#[test]
fn prec_parse_test() {
  assert!(Precedence::parse("").is_some());
  assert!(Precedence::parse("0").is_some());
  assert!(Precedence::parse("1.2").is_some());
  assert!(Precedence::parse("1.2.3").is_some());
  assert!(Precedence::parse("1.-2.3<").is_some());
  assert!(Precedence::parse("-10.42.2048>").is_some());
  assert!(Precedence::parse("-10.42..2048>").is_none());
  assert!(Precedence::parse("--10.42.2048>").is_none());
  assert!(Precedence::parse("-10.42.2048>>").is_none());
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
  Fail,
  Eps,
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
  pub fn sep0(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Rc::new(Regex::OrElse(Rc::new(Regex::Eps), Regex::sep1(r, s)))
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
    Rc::new(Regex::OrElse(r.clone(), Rc::new(Regex::Eps)))
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
