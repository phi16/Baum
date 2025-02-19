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
    // (-?[0-9]+)%.[<>←→]
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
        Some('<') | Some('←') => {
          left_eps = PrecEps::NegEps;
          break;
        }
        Some('>') | Some('→') => {
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
  assert!(Precedence::parse("1.-2.3→").is_some());
  assert_eq!(
    Precedence::parse("10.42.2048>").unwrap().0,
    Precedence::Level(PrecLevel(vec![10, 42, 2048]), PrecEps::PosEps)
  );
  assert!(Precedence::parse("-10.42..2048>").is_none());
  assert!(Precedence::parse("--10.42.2048>").is_none());
  assert!(Precedence::parse("-10.42.2048>>").is_none());
}

#[derive(Clone, PartialEq, Eq)]
pub enum Terminal {
  Token(String),
  Nat,
  Rat,
  Chr,
  Str,
  Id,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Regex {
  Terminal(Terminal),
  Expr,
  Fail,
  Eps,
  Seq(Rc<Regex>, Rc<Regex>),
  Or(Rc<Regex>, Rc<Regex>),
  Rep(Rc<Regex>),
}

impl Regex {
  pub fn id() -> Rc<Self> {
    Rc::new(Regex::Terminal(Terminal::Id))
  }
  pub fn e() -> Rc<Self> {
    Rc::new(Regex::Expr)
  }
  pub fn token(s: &str) -> Rc<Self> {
    Rc::new(Regex::Terminal(Terminal::Token(s.to_string())))
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
  pub fn or(r1: &Rc<Regex>, r2: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Or(r1.clone(), r2.clone()))
  }
  pub fn rep0(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Rep(r.clone()))
  }
  pub fn rep1(r: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Seq(r.clone(), Rc::new(Regex::Rep(r.clone()))))
  }
  pub fn sep0_(r: &Rc<Regex>, s: &Rc<Regex>) -> Rc<Self> {
    // accepts: ε, s, r, rs, rsr, rsrs, ...
    Rc::new(Regex::Seq(Regex::sep0(r, s), Regex::may(s)))
  }
  pub fn sep0(r: &Rc<Regex>, s: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Or(Rc::new(Regex::Eps), Regex::sep1(r, s)))
  }
  pub fn sep1(r: &Rc<Regex>, s: &Rc<Regex>) -> Rc<Self> {
    Rc::new(Regex::Seq(
      r.clone(),
      Rc::new(Regex::Rep(Rc::new(Regex::Seq(s.clone(), r.clone())))),
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
        Regex::Terminal(_) | Regex::Fail | Regex::Eps | Regex::Rep(_) | Regex::Expr => true,
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
        Regex::Terminal(t) => match t {
          Terminal::Token(s) => write!(f, "\"{}\"", s),
          Terminal::Nat => write!(f, "n"),
          Terminal::Rat => write!(f, "r"),
          Terminal::Chr => write!(f, "c"),
          Terminal::Str => write!(f, "s"),
          Terminal::Id => write!(f, "i"),
        },
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
        Regex::Expr => write!(f, "Expr"),
      }
    }
    write!(f, "/")?;
    fmt_rec(self, f)?;
    write!(f, "/")?;
    Ok(())
  }
}

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

pub mod deriv {
  use super::*;

  pub enum Query<'a> {
    Token(TokenType, &'a str),
    Expr,
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
    Token(ElemType, &'a str),
    Expr,
  }

  pub fn d<'a>(regex: &Rc<Regex>, q: &Query<'a>) -> Option<(QueryResult<'a>, Rc<Regex>)> {
    match **regex {
      Regex::Terminal(ref t) => match q {
        Query::Token(ty, str) => {
          if let Some(ety) = match (t, ty) {
            (Terminal::Token(ref s), TokenType::Ident) if s == str => Some(ElemType::Token),
            (Terminal::Token(ref s), TokenType::Reserved) if s == str => Some(ElemType::Token),
            (Terminal::Nat, TokenType::Natural) => Some(ElemType::Nat),
            (Terminal::Rat, TokenType::Rational) => Some(ElemType::Rat),
            (Terminal::Chr, TokenType::Char) => Some(ElemType::Chr),
            (Terminal::Str, TokenType::String) => Some(ElemType::Str),
            (Terminal::Id, TokenType::Ident) => Some(ElemType::Id),
            _ => None,
          } {
            return Some((QueryResult::Token(ety, str), Rc::new(Regex::Eps)));
          }
        }
        _ => {}
      },
      Regex::Expr => match q {
        Query::Expr => return Some((QueryResult::Expr, Rc::new(Regex::Eps))),
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
      _ => false,
    }
  }

  pub fn next_tokens(regex: &Regex) -> HashSet<String> {
    fn collect(regex: &Regex, set: &mut HashSet<String>) {
      match regex {
        Regex::Terminal(Terminal::Token(s)) => {
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

  pub fn next_expr(regex: &Regex) -> bool {
    match regex {
      Regex::Eps => false,
      Regex::Seq(left, right) => {
        let ln = next_expr(left);
        let rn = if has_eps(left) {
          next_expr(right)
        } else {
          false
        };
        ln | rn
      }
      Regex::Or(left, right) => {
        let ln = next_expr(left);
        let rn = next_expr(right);
        ln | rn
      }
      Regex::Rep(r) => next_expr(r),
      Regex::Expr => true,
      _ => false,
    }
  }

  pub fn skip_e(regex: &Rc<Regex>, skip: usize) -> Rc<Regex> {
    let mut r = regex.clone();
    for _ in 0..skip {
      match *r {
        Regex::Seq(ref left, ref right) => match **left {
          Regex::Expr => {
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
