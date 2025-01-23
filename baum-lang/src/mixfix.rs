use crate::mixfix_types::*;
use crate::tokenize::*;
use crate::types::*;
use std::collections::HashMap;
use std::rc::Rc;

pub struct SyntaxDatabase {
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
    Regex::Seqs(vec![$(syntax_elem!($e)),+])
  };
}

#[cfg(test)]
#[test]
fn syntax_macro_test() {
  let elems = syntax_elems!["λ", id, ".", e];
  assert_eq!(
    elems,
    Regex::Seqs(vec![
      Regex::token("λ"),
      Regex::id(),
      Regex::token("."),
      Regex::e(),
    ])
  );
}

impl SyntaxDatabase {
  pub fn default() -> Self {
    let mut db = SyntaxDatabase {
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
    db
  }

  fn def(&mut self, prec: &str, seqs: Regex) {
    let (left, right) = Precedence::parse(prec).unwrap();
    let v = match &seqs {
      Regex::Seqs(seqs) => {
        let first = seqs.get(0).unwrap();
        match **first {
          Regex::Token(ref s) => self.pres.entry(s.to_string()).or_insert(Vec::new()),
          Regex::NonTerm(NonTerm::Expr) => {
            let second = seqs.get(1).unwrap();
            match **second {
              Regex::Token(ref s) => self.opes.entry(s.to_string()).or_insert(Vec::new()),
              Regex::NonTerm(NonTerm::Expr) => &mut self.apps,
              _ => panic!(),
            }
          }
          _ => panic!(),
        }
      }
      _ => panic!(),
    };
    v.push(Syntax::new(left, right, seqs));
  }

  pub fn dump(&self) {
    for syn in &self.pres {
      println!("{:?}", syn);
    }
    for syn in &self.opes {
      println!("{:?}", syn);
    }
    for syn in &self.apps {
      println!("{:?}", syn);
    }
  }
}
