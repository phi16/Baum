use crate::types::mixfix::*;
use crate::types::parse::{CoreElem, SyntaxInterpreter};
use baum_core::types as core;
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

struct MiniParser<'a> {
  input: Vec<CoreElem<'a>>,
  index: usize,
}

enum ElemType {
  Token,
  Ident,
  Nat,
  Rat,
  Chr,
  Str,
  Def,
  Expr,
  Decls,
}

fn match_elem(e: &CoreElem, ty: ElemType) -> bool {
  match (e, ty) {
    (CoreElem::Token(_), ElemType::Token) => true,
    (CoreElem::Ident(_), ElemType::Ident) => true,
    (CoreElem::Nat(_), ElemType::Nat) => true,
    (CoreElem::Rat(_), ElemType::Rat) => true,
    (CoreElem::Chr(_), ElemType::Chr) => true,
    (CoreElem::Str(_), ElemType::Str) => true,
    (CoreElem::Def(_, _), ElemType::Def) => true,
    (CoreElem::Expr(_), ElemType::Expr) => true,
    (CoreElem::Decls(_), ElemType::Decls) => true,
    _ => false,
  }
}

impl<'a> MiniParser<'a> {
  fn new(input: Vec<CoreElem<'a>>) -> Self {
    Self { input, index: 0 }
  }

  fn peek_if(&self, ty: ElemType) -> Option<&CoreElem<'a>> {
    let e = self.input.get(self.index)?;
    if match_elem(e, ty) {
      Some(e)
    } else {
      None
    }
  }

  fn take_if(&mut self, ty: ElemType) -> Option<&CoreElem<'a>> {
    let e = self.input.get(self.index)?;
    if match_elem(e, ty) {
      self.index += 1;
      Some(e)
    } else {
      None
    }
  }

  fn take_token(&mut self, s: &'a str) -> Option<&CoreElem<'a>> {
    let e = self.input.get(self.index)?;
    if let CoreElem::Token(t) = e {
      if *t == s {
        self.index += 1;
        return Some(e);
      }
    }
    None
  }
}

pub fn default_syntax_table<'a>() -> SyntaxTable<SyntaxInterpreter<'a>> {
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

  let t: SyntaxInterpreter<'a> = Rc::new(move |elems, _| {
    eprintln!("Interpreting: {:?}", elems);
    let res = unimplemented!();
    res
  });

  // Literal
  syntax.def(
    "",
    regex_elems![n],
    Rc::new(|elems, _| match elems[..] {
      [CoreElem::Nat(s)] => core::Expr::Lit(core::Literal::Nat(core::Nat(s.parse().unwrap()))),
      _ => panic!(),
    }),
  );
  syntax.def("", regex_elems![r], t.clone());
  syntax.def("", regex_elems![c], t.clone());
  syntax.def("", regex_elems![s], t.clone());
  // Base
  syntax.def("", regex_elems!["prim", s], t.clone());
  syntax.def("", regex_elems!["_"], Rc::new(|_, _| core::Expr::Hole));
  syntax.def("0", regex_elems!["let", decls, "in", e], Rc::new(|elems, _| match elems.as_slice() {
    [CoreElem::Token("let"), CoreElem::Decls(decls), CoreElem::Token("in"), CoreElem::Expr(e)] => {
      core::Expr::Let(decls.clone(), Rc::new(e.clone()))
    },
    _ => panic!(),
  }),);
  // Universe
  syntax.def("", regex_elems!["U"], Rc::new(|_, _| core::Expr::Uni));
  // Function
  syntax.def("0", regex_elems!["λ", "(", fun_args, ")", e], t.clone());
  syntax.def("0", regex_elems!["λ", "{", fun_args, "}", e], t.clone());
  syntax.def("0", regex_elems!["Π", "(", types, ")", e], t.clone());
  syntax.def("0", regex_elems!["Π", "{", types, "}", e], t.clone());
  syntax.def(
    "4<",
    regex_elems![e, e],
    Rc::new(|elems, _| match elems.as_slice() {
      [CoreElem::Expr(e1), CoreElem::Expr(e2)] => {
        core::Expr::AppE(Rc::new(e1.clone()), Rc::new(e2.clone()))
      }
      _ => panic!(),
    }),
  );
  syntax.def(
    "4<",
    regex_elems![e, "{", e, "}"],
    Rc::new(|elems, _| match elems.as_slice() {
      [CoreElem::Expr(e1), CoreElem::Token("{"), CoreElem::Expr(e2), CoreElem::Token("}")] => {
        core::Expr::AppI(Rc::new(e1.clone()), Rc::new(e2.clone()))
      }
      _ => panic!(),
    }),
  );
  // Tuple/Object
  syntax.def(
    "",
    regex_elems!["(", vals, ")"],
    Rc::new(|elems, _| match elems.as_slice() {
      [CoreElem::Token("("), CoreElem::Token(")")] => core::Expr::TupelCon(Vec::new()),
      [CoreElem::Token("("), CoreElem::Expr(e), CoreElem::Token(")")] => e.clone(),
      _ => panic!(),
    }),
  );
  syntax.def("", regex_elems!["{", defs, "}"], t.clone());
  syntax.def(
    "",
    regex_elems!["Σ", "(", types, ")"],
    Rc::new(|elems, _| {
      let mut v = Vec::new();
      let mut p = MiniParser::new(elems);
      p.take_token("Σ").unwrap();
      p.take_token("(").unwrap();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        if let Some(_) = p.peek_if(ElemType::Ident) {
          let mut ids = Vec::new();
          loop {
            if let Some(_) = p.take_token(":") {
              break;
            }
            let id = p.take_if(ElemType::Ident).unwrap();
            if let CoreElem::Ident(id) = id {
              ids.push(id.clone());
            } else {
              unreachable!();
            }
          }
          if let CoreElem::Expr(e) = p.take_if(ElemType::Expr).unwrap() {
            for id in ids {
              v.push((Some(id.clone()), Rc::new(e.clone())));
            }
          } else {
            unreachable!();
          }
        } else if let Some(e) = p.take_if(ElemType::Expr) {
          if let CoreElem::Expr(e) = e {
            v.push((None, Rc::new(e.clone())));
          } else {
            unreachable!();
          }
        }
        p.take_token(",");
      }
      core::Expr::TupleTy(v)
    }),
  );
  syntax.def("", regex_elems!["Σ", "{", props, "}"], t.clone());
  syntax.def("0", regex_elems!["π", "(", n, ")", e], t.clone());
  syntax.def("0", regex_elems!["π", "{", id, "}", e], t.clone());
  // Inductive/Coinductive
  let id_ty = Regex::seqs(vec![&id, &colon, &e]);
  syntax.def(
    "",
    regex_elems!["μ", "(", id_ty, ")", "{", defs, "}"],
    t.clone(),
  );
  syntax.def(
    "",
    regex_elems!["ν", "(", id_ty, ")", "{", defs, "}"],
    t.clone(),
  );

  syntax
}
