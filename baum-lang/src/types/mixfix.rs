use crate::types::token::TokenType;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
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
    let mut left = Precedence::Level(PrecLevel(nums.clone()), left_eps);
    let right = Precedence::Level(PrecLevel(nums), right_eps);
    if right <= Precedence::Level(PrecLevel(vec![0]), PrecEps::Zero) {
      // allows `1 + π(0) x` for prefixs
      left = Precedence::Terminal;
    }
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
    Precedence::parse("10.42.2048>").unwrap().0,
    Precedence::Level(PrecLevel(vec![10, 42, 2048]), PrecEps::PosEps)
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
  Rat,
  Chr,
  Str,
  Id,
  NonTerm(NonTerm),
  Fail,
  Eps,
  Seq(Rc<Regex>, Rc<Regex>),
  Or(Rc<Regex>, Rc<Regex>),
  Rep(Rc<Regex>),
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
  pub fn sep0_(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Rc::new(Regex::Seq(
      Regex::sep0(r, s),
      Regex::may(&Rc::new(Regex::Token(s.to_string()))),
    ))
  }
  pub fn sep0(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Rc::new(Regex::Or(Rc::new(Regex::Eps), Regex::sep1(r, s)))
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
  pub fn may(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Or(r.clone(), Rc::new(Regex::Eps)))
  }
}

impl std::fmt::Debug for Regex {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn is_simple(r: &Regex) -> bool {
      match r {
        Regex::Token(_)
        | Regex::Nat
        | Regex::Rat
        | Regex::Chr
        | Regex::Str
        | Regex::Id
        | Regex::Fail
        | Regex::Eps
        | Regex::Rep(_)
        | Regex::NonTerm(_) => true,
        Regex::Or(r1, _) => **r1 == Regex::Eps,
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
        Regex::Rat => write!(f, "r"),
        Regex::Chr => write!(f, "c"),
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
        Regex::Or(r1, r2) => {
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

#[derive(Debug, Clone)]
pub struct SyntaxTable {
  pres: HashMap<String, Vec<Syntax>>, // starts from token
  lits: Vec<Syntax>,                  // literals
  opes: HashMap<String, Vec<Syntax>>, // starts from expr and token
  apps: Vec<Syntax>,                  // expr expr
}

impl SyntaxTable {
  pub fn new() -> Self {
    SyntaxTable {
      pres: HashMap::new(),
      lits: Vec::new(),
      opes: HashMap::new(),
      apps: Vec::new(),
    }
  }

  pub fn def(&mut self, prec: &str, seqs: Rc<Regex>) {
    let (left, right) = Precedence::parse(prec).unwrap();
    self.add(Syntax::new(left, right, seqs));
  }

  pub fn add(&mut self, syn: Syntax) {
    let seqs = &syn.regex;
    let v = match &**seqs {
      Regex::Token(ref s) => self.pres.entry(s.to_string()).or_insert(Vec::new()),
      Regex::Seq(first, cont) => match **first {
        Regex::Token(ref s) => self.pres.entry(s.to_string()).or_insert(Vec::new()),
        Regex::NonTerm(NonTerm::Expr) => match **cont {
          Regex::Token(ref s) => self.opes.entry(s.to_string()).or_insert(Vec::new()),
          Regex::NonTerm(NonTerm::Expr) => &mut self.apps,
          Regex::Seq(ref second, _) => match **second {
            Regex::Token(ref s) => self.opes.entry(s.to_string()).or_insert(Vec::new()),
            _ => panic!(),
          },
          _ => panic!(),
        },
        _ => &mut self.lits,
      },
      _ => &mut self.lits,
    };
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

  pub fn choose_pre(&self, s: &str) -> Option<&Vec<Syntax>> {
    self.pres.get(s)
  }
  pub fn lits(&self) -> &Vec<Syntax> {
    &self.lits
  }
  pub fn choose_ope(&self, s: &str) -> Option<&Vec<Syntax>> {
    self.opes.get(s)
  }
  pub fn apps(&self) -> &Vec<Syntax> {
    &self.apps
  }

  pub fn is_pre_head(&self, s: &str) -> bool {
    self.pres.contains_key(s)
  }
  pub fn is_ope_head(&self, s: &str) -> bool {
    self.opes.contains_key(s)
  }

  pub fn merge(&mut self, other: SyntaxTable) {
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

pub mod deriv {
  use super::*;

  pub enum Query<'a> {
    Token(TokenType, &'a str),
    NonTerm(NonTerm),
  }

  #[derive(PartialEq, Eq)]
  pub enum ElemType {
    Token,
    Nat,
    Rat,
    Chr,
    Str,
    Id,
  }

  #[derive(PartialEq, Eq)]
  pub enum QueryResult<'a> {
    Token(ElemType, &'a str), // TODO: Add module
    NonTerm,
  }

  pub fn d<'a>(regex: &Rc<Regex>, q: &Query<'a>) -> Option<(QueryResult<'a>, Rc<Regex>)> {
    match **regex {
      Regex::Token(ref s) => match q {
        Query::Token(ty, str) if ty == &TokenType::Ident || ty == &TokenType::Reserved => {
          if s == str {
            return Some((
              QueryResult::Token(ElemType::Token, str),
              Rc::new(Regex::Eps),
            ));
          }
        }
        _ => {}
      },
      Regex::Nat => match q {
        Query::Token(TokenType::Natural, str) => {
          return Some((QueryResult::Token(ElemType::Nat, str), Rc::new(Regex::Eps)))
        }
        _ => {}
      },
      Regex::Rat => match q {
        Query::Token(TokenType::Rational, str) => {
          return Some((QueryResult::Token(ElemType::Rat, str), Rc::new(Regex::Eps)))
        }
        _ => {}
      },
      Regex::Chr => match q {
        Query::Token(TokenType::Char, str) => {
          return Some((QueryResult::Token(ElemType::Chr, str), Rc::new(Regex::Eps)))
        }
        _ => {}
      },
      Regex::Str => match q {
        Query::Token(TokenType::String, str) => {
          return Some((QueryResult::Token(ElemType::Str, str), Rc::new(Regex::Eps)))
        }
        _ => {}
      },
      Regex::Id => match q {
        Query::Token(TokenType::Ident, str) => {
          return Some((QueryResult::Token(ElemType::Id, str), Rc::new(Regex::Eps)))
        }
        _ => {}
      },
      Regex::NonTerm(ref n) => match q {
        Query::NonTerm(qn) if n == qn => return Some((QueryResult::NonTerm, Rc::new(Regex::Eps))),
        _ => {}
      },
      Regex::Fail => {}
      Regex::Eps => {}
      Regex::Seq(ref left, ref right) => {
        let dl = d(left, q);
        let sl = match dl {
          Some((qr, dl)) => Some((qr, Rc::new(Regex::Seq(dl, right.clone())))),
          None => None,
        };
        if has_eps(&left) {
          let dr = d(right, q);
          return or(sl, dr);
        } else {
          return sl;
        }
      }
      Regex::Or(ref left, ref right) => {
        let dl = d(left, q);
        let dr = d(right, q);
        return or(dl, dr);
      }
      Regex::Rep(ref r) => {
        let (qr, dr) = d(r, q)?;
        return Some((qr, Rc::new(Regex::Seq(dr, regex.clone()))));
      }
    }
    None
  }

  fn or<'a>(
    left: Option<(QueryResult<'a>, Rc<Regex>)>,
    right: Option<(QueryResult<'a>, Rc<Regex>)>,
  ) -> Option<(QueryResult<'a>, Rc<Regex>)> {
    if left.is_none() {
      return right;
    }
    if right.is_none() {
      return left;
    }
    let (ql, l) = left.unwrap();
    let (qr, r) = right.unwrap();
    assert!(ql == qr);
    Some((ql, Rc::new(Regex::Or(l, r))))
  }

  pub fn has_eps(regex: &Regex) -> bool {
    match regex {
      Regex::Eps => true,
      Regex::Seq(left, right) => has_eps(left) && has_eps(right),
      Regex::Or(left, right) => has_eps(left) || has_eps(right),
      Regex::Rep(_) => true,
      _ => false, // Note: NonTerm::Decls may produce empty, but it doesn't matter here
    }
  }

  pub fn next_tokens(regex: &Regex) -> HashSet<String> {
    fn collect(regex: &Regex, set: &mut HashSet<String>) {
      match regex {
        Regex::Token(s) => {
          set.insert(s.clone());
        }
        Regex::Seq(left, right) => {
          collect(left, set);
          if has_eps(left) {
            collect(right, set);
          }
        }
        Regex::Or(left, right) => {
          collect(left, set);
          collect(right, set);
        }
        Regex::Rep(r) => collect(r, set),
        _ => {}
      }
    }
    let mut set = HashSet::new();
    collect(regex, &mut set);
    set
  }

  pub fn next_nonterm(regex: &Regex) -> Option<NonTerm> {
    type NonTermBits = u8;

    const DEF_BITS: NonTermBits = 0b001;
    const EXPR_BITS: NonTermBits = 0b010;
    const DECLS_BITS: NonTermBits = 0b100;

    fn rec(regex: &Regex) -> NonTermBits {
      match regex {
        Regex::Eps => 0,
        Regex::Seq(left, right) => {
          let ln = rec(left);
          let rn = if has_eps(left) { rec(right) } else { 0 };
          ln | rn
        }
        Regex::Or(left, right) => {
          let ln = rec(left);
          let rn = rec(right);
          ln | rn
        }
        Regex::Rep(r) => rec(r),
        Regex::NonTerm(NonTerm::Def) => DEF_BITS,
        Regex::NonTerm(NonTerm::Expr) => EXPR_BITS,
        Regex::NonTerm(NonTerm::Decls) => DECLS_BITS,
        _ => 0,
      }
    }

    let bits = rec(regex);
    if bits == 0 {
      return None;
    }
    if bits == DEF_BITS {
      return Some(NonTerm::Def);
    }
    if bits == EXPR_BITS {
      return Some(NonTerm::Expr);
    }
    if bits == DECLS_BITS {
      return Some(NonTerm::Decls);
    }
    panic!("Not allowed to have multiple non-terminals in a syntax");
  }

  pub fn skip_e(regex: &Rc<Regex>, skip: usize) -> Rc<Regex> {
    let mut r = regex.clone();
    for _ in 0..skip {
      match *r {
        Regex::Seq(ref left, ref right) => match **left {
          Regex::NonTerm(NonTerm::Expr) => {
            r = right.clone();
          }
          _ => panic!(),
        },
        _ => panic!(),
      }
    }
    r
  }
}
