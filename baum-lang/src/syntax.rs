use crate::types::env::SyntaxTable;
use crate::types::regex::{Regex, Terminal};
use crate::types::tree::SyntaxId;
use std::rc::Rc;

macro_rules! regex_elem {
  ($s:literal) => {
    (Regex::token($s))
  };
  (n) => {
    Rc::new(Regex::Terminal(Terminal::Nat))
  };
  (r) => {
    Rc::new(Regex::Terminal(Terminal::Rat))
  };
  (c) => {
    Rc::new(Regex::Terminal(Terminal::Chr))
  };
  (s) => {
    Rc::new(Regex::Terminal(Terminal::Str))
  };
  (id) => {
    Regex::id()
  };
  (e) => {
    Regex::e()
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

pub fn default_syntax_table<'a>() -> SyntaxTable {
  let mut syntax = SyntaxTable::new();

  let id = &Regex::id();
  let e = &Regex::e();
  let ids = &Regex::rep1(&id);
  let colon = &Regex::token(":");
  let comma = &Regex::token(",");

  // (id+ | id+: e)%0,
  let fun_args = &Regex::sep0_(&Regex::seq(ids, &Regex::may(&Regex::seq(colon, e))), comma);
  // (e | id+: e)%0,
  let types = &Regex::sep0_(&Regex::seq(&Regex::may(&Regex::seq(ids, colon)), e), comma);
  // e%0,
  let vals = &Regex::sep0_(e, comma);
  // (id+: e)%0,
  let props = &Regex::sep0_(&Regex::seqs(vec![ids, colon, e]), comma);
  // def%0,
  let may_type = &Regex::may(&Regex::seq(colon, e));
  let arg = &Regex::or(
    &Regex::or(
      &Regex::seqs(vec![&Regex::token("("), ids, may_type, &Regex::token(")")]),
      &Regex::seqs(vec![&Regex::token("{"), ids, may_type, &Regex::token("}")]),
    ),
    &id,
  );
  let args = &Regex::rep0(arg);
  let def = &Regex::seqs(vec![id, args, may_type, &Regex::token("="), &e]);
  let defs = &Regex::sep0_(def, comma); // comma or...

  let t = SyntaxId::Sys(0);

  syntax.def("", regex_elems![n], t.clone());
  syntax.def("", regex_elems![r], t.clone());
  syntax.def("", regex_elems![c], t.clone());
  syntax.def("", regex_elems![s], t.clone());
  // Base
  syntax.def("", regex_elems!["_"], t.clone());
  // Universe
  syntax.def("", regex_elems!["U"], t.clone());
  // Function
  syntax.def("0", regex_elems!["λ", "(", fun_args, ")", e], t.clone());
  syntax.def("0", regex_elems!["λ", "{", fun_args, "}", e], t.clone());
  syntax.def("0", regex_elems!["Π", "(", types, ")", e], t.clone());
  syntax.def("0", regex_elems!["Π", "{", types, "}", e], t.clone());
  syntax.def("4<", regex_elems![e, e], t.clone());
  syntax.def("4<", regex_elems![e, "{", e, "}"], t.clone());
  // Tuple/Object
  syntax.def("", regex_elems!["(", vals, ")"], t.clone());
  syntax.def("", regex_elems!["{", defs, "}"], t.clone());
  syntax.def("", regex_elems!["Σ", "(", types, ")"], t.clone());
  syntax.def("", regex_elems!["Σ", "{", props, "}"], t.clone());
  syntax.def("0", regex_elems!["π", "(", n, ")", e], t.clone());
  syntax.def("0", regex_elems!["π", "{", id, "}", e], t.clone());

  syntax
}
