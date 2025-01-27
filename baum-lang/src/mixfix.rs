use crate::mixfix_types::*;
use crate::tokenize::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

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
          return or_else(sl, dr);
        } else {
          return sl;
        }
      }
      Regex::OrElse(ref left, ref right) => {
        let dl = d(left, t);
        let dr = d(right, t);
        return or_else(dl, dr);
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
          return or_else(sl, dr);
        } else {
          return sl;
        }
      }
      Regex::OrElse(ref left, ref right) => {
        let dl = d_nonterm(left, nt);
        let dr = d_nonterm(right, nt);
        return or_else(dl, dr);
      }
      Regex::Rep(ref r) => {
        let dr = d_nonterm(r, nt);
        return Rc::new(Regex::Seq(dr, regex.clone()));
      }
      _ => {}
    }
    Rc::new(Regex::Fail)
  }

  fn or_else(left: Rc<Regex>, right: Rc<Regex>) -> Rc<Regex> {
    if is_fail(&left) {
      return right;
    }
    if is_fail(&right) {
      return left;
    }
    Rc::new(Regex::OrElse(left, right))
  }

  pub fn has_eps(regex: &Regex) -> bool {
    match regex {
      Regex::Eps => true,
      Regex::Seq(left, right) => has_eps(left) && has_eps(right),
      Regex::OrElse(left, right) => has_eps(left) || has_eps(right),
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
        Regex::OrElse(left, right) => {
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
        Regex::OrElse(left, right) => {
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
