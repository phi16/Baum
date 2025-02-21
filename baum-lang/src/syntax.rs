use crate::types::env::SyntaxTable;
use crate::types::regex::{Regex, Terminal};
use crate::types::tree::SyntaxId;
use std::collections::HashMap;
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

  syntax.def("", regex_elems![n], SyntaxId::Nat);
  syntax.def("", regex_elems![r], SyntaxId::Rat);
  syntax.def("", regex_elems![c], SyntaxId::Chr);
  syntax.def("", regex_elems![s], SyntaxId::Str);
  // Base
  syntax.def("", regex_elems!["_"], SyntaxId::Hole);
  syntax.def("", regex_elems!["U"], SyntaxId::Uni);
  // Function
  syntax.def(
    "0",
    regex_elems!["λ", "(", fun_args, ")", e],
    SyntaxId::LamE,
  );
  syntax.def(
    "0",
    regex_elems!["λ", "{", fun_args, "}", e],
    SyntaxId::LamI,
  );
  syntax.def("0", regex_elems!["Π", "(", types, ")", e], SyntaxId::PiE);
  syntax.def("0", regex_elems!["Π", "{", types, "}", e], SyntaxId::PiI);
  syntax.def("4<", regex_elems![e, e], SyntaxId::AppE);
  syntax.def("4<", regex_elems![e, "{", e, "}"], SyntaxId::AppI);
  // Tuple/Object
  syntax.def("", regex_elems!["(", vals, ")"], SyntaxId::TupleCon);
  syntax.def("", regex_elems!["{", defs, "}"], SyntaxId::ObjCon);
  syntax.def("", regex_elems!["Σ", "(", types, ")"], SyntaxId::TupleTy);
  syntax.def("", regex_elems!["Σ", "{", props, "}"], SyntaxId::ObjTy);
  syntax.def("0", regex_elems!["π", "(", n, ")", e], SyntaxId::Proj);
  syntax.def("0", regex_elems!["π", "{", id, "}", e], SyntaxId::Proj);

  syntax
}

use crate::types::syntax::{FrontElem, SyntaxHandler};
use baum_front::types::literal as lit;
use baum_front::types::tree as front;

struct MiniParser<'a> {
  input: Vec<FrontElem<'a>>,
  index: usize,
}

enum ElemType {
  Token,
  Ident,
  Nat,
  Rat,
  Chr,
  Str,
  Expr,
}

fn match_elem(e: &FrontElem, ty: ElemType) -> bool {
  match (e, ty) {
    (FrontElem::Token(_), ElemType::Token) => true,
    (FrontElem::Ident(_), ElemType::Ident) => true,
    (FrontElem::Nat(_), ElemType::Nat) => true,
    (FrontElem::Rat(_), ElemType::Rat) => true,
    (FrontElem::Chr(_), ElemType::Chr) => true,
    (FrontElem::Str(_), ElemType::Str) => true,
    (FrontElem::Expr(_), ElemType::Expr) => true,
    _ => false,
  }
}

impl<'a> MiniParser<'a> {
  fn new(input: Vec<FrontElem<'a>>) -> Self {
    Self { input, index: 0 }
  }

  fn peek_if(&self, ty: ElemType) -> Option<&FrontElem<'a>> {
    let e = self.input.get(self.index)?;
    if match_elem(e, ty) {
      Some(e)
    } else {
      None
    }
  }

  fn take_if(&mut self, ty: ElemType) -> Option<&FrontElem<'a>> {
    let e = self.input.get(self.index)?;
    if match_elem(e, ty) {
      self.index += 1;
      Some(e)
    } else {
      None
    }
  }

  fn take_token(&mut self, s: &'a str) -> Option<&FrontElem<'a>> {
    let e = self.input.get(self.index)?;
    if let FrontElem::Token(t) = e {
      if *t == s {
        self.index += 1;
        return Some(e);
      }
    }
    None
  }
}

pub fn default_syntax_handlers<'a>() -> HashMap<SyntaxId, SyntaxHandler<'a>> {
  let mut handlers: HashMap<SyntaxId, SyntaxHandler> = HashMap::new();

  handlers.insert(
    SyntaxId::Nat,
    Box::new(|elems| match elems[..] {
      [FrontElem::Nat(s)] => {
        let n = s.parse().unwrap();
        let n = lit::Literal::Nat(lit::Nat(n));
        Ok(front::Expr(front::ExprF::Lit(n)))
      }
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::Rat,
    Box::new(|elems| match elems[..] {
      [FrontElem::Rat(s)] => {
        unimplemented!()
      }
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::Chr,
    Box::new(|elems| match elems[..] {
      [FrontElem::Chr(s)] => {
        let c = s.parse().unwrap();
        let c = lit::Literal::Chr(c);
        Ok(front::Expr(front::ExprF::Lit(c)))
      }
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::Str,
    Box::new(|elems| match elems[..] {
      [FrontElem::Str(s)] => {
        let s = lit::Literal::Str(s.to_string());
        Ok(front::Expr(front::ExprF::Lit(s)))
      }
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::Hole,
    Box::new(|elems| match elems[..] {
      [FrontElem::Token("_")] => Ok(front::Expr(front::ExprF::Hole)),
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::Uni,
    Box::new(|elems| match elems[..] {
      [FrontElem::Token("U")] => Ok(front::Expr(front::ExprF::Uni)),
      _ => unreachable!(),
    }),
  );
  handlers.insert(
    SyntaxId::TupleTy,
    Box::new(|elems| {
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
            if let FrontElem::Ident(id) = id {
              ids.push(id.clone());
            } else {
              unreachable!();
            }
          }
          if let FrontElem::Expr(e) = p.take_if(ElemType::Expr).unwrap() {
            for id in ids {
              v.push((Some(id.clone()), Rc::new(e.clone())));
            }
          } else {
            unreachable!();
          }
        } else if let Some(e) = p.take_if(ElemType::Expr) {
          if let FrontElem::Expr(e) = e {
            v.push((None, Rc::new(e.clone())));
          } else {
            unreachable!();
          }
        }
        p.take_token(",");
      }
      Ok(front::Expr(front::ExprF::TupleTy(v)))
    }),
  );
  handlers.insert(
    SyntaxId::TupleCon,
    Box::new(|elems| {
      let mut v = Vec::new();
      let mut p = MiniParser::new(elems);
      p.take_token("(").unwrap();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        if let FrontElem::Expr(e) = p.take_if(ElemType::Expr).unwrap() {
          v.push(Rc::new(e.clone()));
        } else {
          unreachable!();
        }
        p.take_token(",");
      }
      Ok(front::Expr(front::ExprF::TupleCon(v)))
    }),
  );

  handlers
}
