use crate::types::token::{Token, TokenType};
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
  Or(Rc<Regex>, Rc<Regex>),
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
  pub fn sep2(r: &Rc<Regex>, s: &str) -> Rc<Self> {
    Regex::seqs(vec![&r, &Regex::token(s), &Self::sep1(r, s)])
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
  pub pres: HashMap<String, Vec<Syntax>>, // starts from token
  pub opes: HashMap<String, Vec<Syntax>>, // starts from expr and token
  pub apps: Vec<Syntax>,                  // expr expr
}

macro_rules! syntax_elem {
  ($s:literal) => {
    (Regex::token($s))
  };
  (n) => {
    Rc::new(Regex::Nat)
  };
  (s) => {
    Rc::new(Regex::Str)
  };
  (id) => {
    Regex::id()
  };
  (e) => {
    Regex::e()
  };
  (def) => {
    Regex::def()
  };
  (decls) => {
    Regex::decls()
  };
  ($i:tt) => {
    $i.clone()
  };
}

macro_rules! syntax_elems {
  ($($e:tt),+) => {
    Regex::seqs(vec![$(&syntax_elem!($e)),+])
  };
}

#[cfg(test)]
#[test]
fn syntax_macro_test() {
  let elems = syntax_elems!["λ", id, ".", e];
  assert_eq!(
    elems,
    Regex::seqs(vec![
      &Regex::token("λ"),
      &Regex::id(),
      &Regex::token("."),
      &Regex::e(),
    ])
  );
}

impl SyntaxTable {
  pub fn default() -> Self {
    let mut db = SyntaxTable {
      pres: HashMap::new(),
      opes: HashMap::new(),
      apps: Vec::new(),
    };

    let id = Regex::id();
    let e = Regex::e();
    let ids = Regex::rep1(&id);
    let colon = Regex::token(":");

    // (id+ | id+: e)%1,
    let fun_args = Regex::sep1(&Regex::seq(&ids, &Regex::may(&Regex::seq(&colon, &e))), ",");
    // (e | id+: e)%0,
    let types = Regex::sep0(&Regex::seq(&Regex::may(&Regex::seq(&ids, &colon)), &e), ",");
    // e%2,
    let tuple_vals = Regex::sep2(&e, ",");
    // (id+: e)%0,
    let props = Regex::sep0(&Regex::seqs(vec![&ids, &colon, &e]), ",");
    // def%0,
    let defs = Regex::sep0(&Regex::def(), ",");

    // Base
    db.def("", syntax_elems!["(", e, ")"]);
    db.def("", syntax_elems!["prim", s]);
    db.def("0", syntax_elems!["let", decls, "in", e]);
    // Function
    db.def("0", syntax_elems!["λ", "(", fun_args, ")", e]);
    db.def("0", syntax_elems!["λ", "{", fun_args, "}", e]);
    db.def("0", syntax_elems!["Π", "(", types, ")", e]);
    db.def("0", syntax_elems!["Π", "{", types, "}", e]);
    db.def("4<", syntax_elems![e, e]);
    db.def("4<", syntax_elems![e, "{", e, "}"]);
    // Tuple/Object
    db.def("", syntax_elems!["(", ")"]);
    db.def("", syntax_elems!["(", tuple_vals, ")"]);
    db.def("", syntax_elems!["{", defs, "}"]);
    db.def("", syntax_elems!["Σ", "(", types, ")"]);
    db.def("", syntax_elems!["Σ", "{", props, "}"]);
    db.def("0", syntax_elems!["π", "(", n, ")", e]);
    db.def("0", syntax_elems!["π", "{", id, "}", e]);
    // Context
    db.def("0", syntax_elems!["σ", "{", defs, "}", e]);
    // Inductive/Coinductive
    let id_ty = Regex::seqs(vec![&id, &colon, &e]);
    db.def("", syntax_elems!["μ", "(", id_ty, ")", "{", defs, "}"]);
    db.def("", syntax_elems!["ν", "(", id_ty, ")", "{", defs, "}"]);

    // Demo
    db.def("2.1<", syntax_elems![e, "+", e]);
    db.def("2.1<", syntax_elems![e, "-", e]);
    db.def("2.2<", syntax_elems![e, "*", e]);
    db.def("2.3>", syntax_elems!["-", e]);
    db.def("2.4<", syntax_elems![e, "!"]);
    db.def("", syntax_elems!["[", e, "|", e, "]"]);
    db.def("", syntax_elems!["[", e, "?", e, "]"]);
    db
  }

  fn def(&mut self, prec: &str, seqs: Rc<Regex>) {
    let (mut left, right) = Precedence::parse(prec).unwrap();
    if right <= Precedence::Level(PrecLevel(vec![0]), PrecEps::Zero) {
      // allows `1 + π(0) x` for prefixs
      left = Precedence::Terminal;
    }
    let v = match &*seqs {
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
        _ => panic!(),
      },
      _ => panic!(),
    };
    v.push(Syntax::new(left, right, seqs));
  }

  pub fn dump(&self) {
    println!("[Prefixs]");
    for syn in &self.pres {
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
  pub fn choose_ope(&self, s: &str) -> Option<&Vec<Syntax>> {
    self.opes.get(s)
  }

  pub fn is_head(&self, s: &str) -> bool {
    self.pres.contains_key(s) || self.opes.contains_key(s)
  }
}

pub mod deriv {
  use super::*;

  pub fn d(regex: &Rc<Regex>, t: &Token) -> Rc<Regex> {
    match **regex {
      Regex::Token(ref s) => {
        if (t.ty == TokenType::Ident || t.ty == TokenType::Reserved) && s == t.str {
          return Rc::new(Regex::Eps);
        }
      }
      Regex::Nat => {
        if t.ty == TokenType::Natural {
          return Rc::new(Regex::Eps);
        }
      }
      Regex::Str => {
        if t.ty == TokenType::String {
          return Rc::new(Regex::Eps);
        }
      }
      Regex::Id => {
        if t.ty == TokenType::Ident {
          return Rc::new(Regex::Eps);
        }
      }
      Regex::Fail => {}
      Regex::Eps => {}
      Regex::Seq(ref left, ref right) => {
        let dl = d(left, t);
        let sl = if is_fail(&dl) {
          dl
        } else {
          Rc::new(Regex::Seq(dl, right.clone()))
        };
        if has_eps(&left) {
          let dr = d(right, t);
          return or(sl, dr);
        } else {
          return sl;
        }
      }
      Regex::Or(ref left, ref right) => {
        let dl = d(left, t);
        let dr = d(right, t);
        return or(dl, dr);
      }
      Regex::Rep(ref r) => {
        let dr = d(r, t);
        return Rc::new(Regex::Seq(dr, regex.clone()));
      }
      Regex::NonTerm(_) => {}
    }
    Rc::new(Regex::Fail)
  }

  pub fn d_nonterm(regex: &Rc<Regex>, nt: &NonTerm) -> Rc<Regex> {
    match **regex {
      Regex::NonTerm(ref n) => {
        if n == nt {
          return Rc::new(Regex::Eps);
        }
      }
      Regex::Fail => {}
      Regex::Eps => {}
      Regex::Seq(ref left, ref right) => {
        let dl = d_nonterm(left, nt);
        let sl = if is_fail(&dl) {
          dl
        } else {
          Rc::new(Regex::Seq(dl, right.clone()))
        };
        if has_eps(&left) {
          let dr = d_nonterm(right, nt);
          return or(sl, dr);
        } else {
          return sl;
        }
      }
      Regex::Or(ref left, ref right) => {
        let dl = d_nonterm(left, nt);
        let dr = d_nonterm(right, nt);
        return or(dl, dr);
      }
      Regex::Rep(ref r) => {
        let dr = d_nonterm(r, nt);
        return Rc::new(Regex::Seq(dr, regex.clone()));
      }
      _ => {}
    }
    Rc::new(Regex::Fail)
  }

  fn or(left: Rc<Regex>, right: Rc<Regex>) -> Rc<Regex> {
    if is_fail(&left) {
      return right;
    }
    if is_fail(&right) {
      return left;
    }
    Rc::new(Regex::Or(left, right))
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

  pub fn next_nonterm(regex: &Regex) -> Vec<NonTerm> {
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
    let mut v = Vec::new();
    if bits & DEF_BITS != 0 {
      v.push(NonTerm::Def);
    }
    if bits & EXPR_BITS != 0 {
      v.push(NonTerm::Expr);
    }
    if bits & DECLS_BITS != 0 {
      v.push(NonTerm::Decls);
    }
    v
  }

  pub fn is_fail(regex: &Regex) -> bool {
    match regex {
      Regex::Fail => true,
      _ => false,
    }
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
