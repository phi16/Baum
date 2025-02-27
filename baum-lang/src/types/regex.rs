use crate::types::token::TokenType;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq)]
pub enum Terminal {
  Token(String),
  Id,
  Dec,
  Num,
  Chr,
  Str,
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
          Terminal::Id => write!(f, "i"),
          Terminal::Dec => write!(f, "d"),
          Terminal::Num => write!(f, "n"),
          Terminal::Chr => write!(f, "c"),
          Terminal::Str => write!(f, "s"),
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

pub mod deriv {
  use super::*;

  pub enum Query<'a> {
    Token(TokenType, &'a str),
    Expr,
  }

  #[derive(PartialEq, Eq)]
  pub enum ElemType {
    Token,
    Id,
    Dec,
    Num,
    Chr,
    Str,
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
            (Terminal::Id, TokenType::Ident) => Some(ElemType::Id),
            (Terminal::Dec, TokenType::DecNat) => Some(ElemType::Dec),
            (Terminal::Num, TokenType::DecNat) => Some(ElemType::Num),
            (Terminal::Num, TokenType::Number) => Some(ElemType::Num),
            (Terminal::Chr, TokenType::Char) => Some(ElemType::Chr),
            (Terminal::Str, TokenType::String) => Some(ElemType::Str),
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

  pub enum SkipToken {
    Expr,
    Id,
  }

  pub fn skip_e(regex: &Rc<Regex>, skip: usize) -> (Rc<Regex>, Vec<SkipToken>) {
    let mut r = regex.clone();
    let mut skipped = Vec::new();
    for _ in 0..skip {
      match *r {
        Regex::Seq(ref left, ref right) => match **left {
          Regex::Expr => {
            skipped.push(SkipToken::Expr);
            r = right.clone();
          }
          Regex::Terminal(Terminal::Id) => {
            skipped.push(SkipToken::Id);
            r = right.clone();
          }
          _ => panic!(),
        },
        _ => panic!(),
      }
    }
    (r, skipped)
  }
}
