use crate::types::mixfix::*;
use crate::types::parse::SyntaxInterpreter;
use std::rc::Rc;

macro_rules! regex_elem {
  ($s:literal) => {
    (Regex::token($s))
  };
  (n) => {
    Rc::new(Regex::Nat)
  };
  (r) => {
    Rc::new(Regex::Rat)
  };
  (c) => {
    Rc::new(Regex::Chr)
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

macro_rules! regex_elems {
  ($($e:tt),+) => {
    Regex::seqs(vec![$(&regex_elem!($e)),+])
  };
}

#[cfg(test)]
#[test]
fn regex_macro_test() {
  let elems = regex_elems!["λ", id, ".", e];
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

pub fn default_syntax_table() -> SyntaxTable<SyntaxInterpreter<'static>> {
  let mut syntax: SyntaxTable<SyntaxInterpreter> = SyntaxTable::new();

  let id = Regex::id();
  let e = Regex::e();
  let ids = Regex::rep1(&id);
  let colon = Regex::token(":");

  // (id+ | id+: e)%0,
  let fun_args = Regex::sep0_(&Regex::seq(&ids, &Regex::may(&Regex::seq(&colon, &e))), ",");
  // (e | id+: e)%0,
  let types = Regex::sep0_(&Regex::seq(&Regex::may(&Regex::seq(&ids, &colon)), &e), ",");
  // e%0,
  let vals = Regex::sep0_(&e, ",");
  // (id+: e)%0,
  let props = Regex::sep0_(&Regex::seqs(vec![&ids, &colon, &e]), ",");
  // def%0,
  let defs = Regex::sep0_(&Regex::def(), ","); // comma or...

  let t = unimplemented!();

  // Literal
  syntax.def("", regex_elems![n], t);
  syntax.def("", regex_elems![r], t);
  syntax.def("", regex_elems![c], t);
  syntax.def("", regex_elems![s], t);
  // Base
  syntax.def("", regex_elems!["prim", s], t);
  syntax.def("", regex_elems!["_"], t);
  syntax.def("0", regex_elems!["let", decls, "in", e], t);
  // Universe
  syntax.def("", regex_elems!["U"], t);
  // Function
  syntax.def("0", regex_elems!["λ", "(", fun_args, ")", e], t);
  syntax.def("0", regex_elems!["λ", "{", fun_args, "}", e], t);
  syntax.def("0", regex_elems!["Π", "(", types, ")", e], t);
  syntax.def("0", regex_elems!["Π", "{", types, "}", e], t);
  syntax.def("4<", regex_elems![e, e], t);
  syntax.def("4<", regex_elems![e, "{", e, "}"], t);
  // Tuple/Object
  syntax.def("", regex_elems!["(", vals, ")"], t); // or unit or usual parenthesis
  syntax.def("", regex_elems!["{", defs, "}"], t);
  syntax.def("", regex_elems!["Σ", "(", types, ")"], t);
  syntax.def("", regex_elems!["Σ", "{", props, "}"], t);
  syntax.def("0", regex_elems!["π", "(", n, ")", e], t);
  syntax.def("0", regex_elems!["π", "{", id, "}", e], t);
  // Inductive/Coinductive
  let id_ty = Regex::seqs(vec![&id, &colon, &e]);
  syntax.def("", regex_elems!["μ", "(", id_ty, ")", "{", defs, "}"], t);
  syntax.def("", regex_elems!["ν", "(", id_ty, ")", "{", defs, "}"], t);

  syntax
}
