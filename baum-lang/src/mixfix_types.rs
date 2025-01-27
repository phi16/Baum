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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
  Initial,
  Level(PrecLevel, PrecEps),
  Terminal,
}

impl std::fmt::Debug for Precedence {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Precedence::Initial => write!(f, "-∞"),
      Precedence::Level(l, e) => {
        let ls = l
          .0
          .iter()
          .map(|x| x.to_string())
          .collect::<Vec<_>>()
          .join(".");
        write!(f, "{}", ls)?;
        match e {
          PrecEps::NegEps => write!(f, "-ε")?,
          PrecEps::Zero => {
            if l.0.len() == 0 {
              write!(f, "0")?;
            }
          }
          PrecEps::PosEps => write!(f, "+ε")?,
        }
        Ok(())
      }
      Precedence::Terminal => write!(f, "∞"),
    }
  }
}

impl Precedence {
  pub fn parse(s: &str) -> Option<(Self, Self)> {
    // (-?[0-9]+)%.[<>]
    // example: "1.-2.3<"
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
  assert_eq!(
    Precedence::parse("1.2.3").unwrap().0,
    Precedence::Level(PrecLevel(vec![1, 2, 3]), PrecEps::Zero)
  );
  assert!(Precedence::parse("1.-2.3<").is_some());
  assert_eq!(
    Precedence::parse("-10.42.2048>").unwrap().0,
    Precedence::Level(PrecLevel(vec![-10, 42, 2048]), PrecEps::PosEps)
  );
  assert!(Precedence::parse("-10.42..2048>").is_none());
  assert!(Precedence::parse("--10.42.2048>").is_none());
  assert!(Precedence::parse("-10.42.2048>>").is_none());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonTerm {
  Def,
  Expr,
  Decls,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Regex {
  Token(String),
  Nat,
  Str,
  Id,
  Fail,
  Eps,
  Seq(Rc<Regex>, Rc<Regex>),
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
    let mut rs = rs.into_iter().rev();
    let mut r = match rs.next() {
      Some(r) => r.clone(),
      None => return Rc::new(Regex::Eps),
    };
    for r2 in rs {
      r = Self::seq(r2, &r);
    }
    r
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
    Regex::seqs(vec![&r, &Regex::token(s), &Self::sep1(r, s)])
  }
  pub fn may(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::OrElse(r.clone(), Rc::new(Regex::Eps)))
  }
}

impl std::fmt::Debug for Regex {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn is_simple(r: &Regex) -> bool {
      match r {
        Regex::Token(_)
        | Regex::Nat
        | Regex::Str
        | Regex::Id
        | Regex::Fail
        | Regex::Eps
        | Regex::Rep(_)
        | Regex::NonTerm(_) => true,
        Regex::OrElse(r1, _) => **r1 == Regex::Eps,
        _ => false,
      }
    }
    fn fmt_sub(r: &Regex, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      if is_simple(r) {
        fmt_rec(r, f)
      } else {
        write!(f, "(")?;
        fmt_rec(r, f)?;
        write!(f, ")")
      }
    }
    fn fmt_rec(r: &Regex, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match r {
        Regex::Token(s) => write!(f, "\"{}\"", s),
        Regex::Nat => write!(f, "n"),
        Regex::Str => write!(f, "s"),
        Regex::Id => write!(f, "i"),
        Regex::Fail => write!(f, "∅"),
        Regex::Eps => write!(f, "ε"),
        Regex::Seq(r1, r2) => {
          match **r1 {
            Regex::Seq(_, _) => fmt_rec(&r1, f)?,
            _ => fmt_sub(&r1, f)?,
          };
          write!(f, " ")?;
          match **r2 {
            Regex::Seq(_, _) => fmt_rec(&r2, f),
            _ => fmt_sub(&r2, f),
          }
        }
        Regex::OrElse(r1, r2) => {
          if **r1 == Regex::Eps {
            fmt_sub(&r2, f)?;
            write!(f, "?")
          } else {
            fmt_sub(&r1, f)?;
            write!(f, " | ")?;
            fmt_sub(&r2, f)
          }
        }
        Regex::Rep(r) => {
          fmt_sub(&r, f)?;
          write!(f, "*")
        }
        Regex::NonTerm(nt) => write!(f, "{:?}", nt),
      }
    }
    write!(f, "/")?;
    fmt_rec(self, f)?;
    write!(f, "/")?;
    Ok(())
  }
}

#[derive(Clone)]
pub struct Syntax {
  pub left: Precedence,
  pub right: Precedence,
  pub regex: Rc<Regex>,
}

impl std::fmt::Debug for Syntax {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Syn({:?}, {:?}) {:?}", self.left, self.right, self.regex)
  }
}

impl Syntax {
  pub fn new(left: Precedence, right: Precedence, regex: Rc<Regex>) -> Self {
    Syntax { left, right, regex }
  }
}
