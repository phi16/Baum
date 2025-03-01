use crate::types::env::SyntaxTable;
use crate::types::regex::{Regex, Terminal};
use crate::types::tree::{SynElemInternal, SyntaxId};
use crate::types::tree_base::SynElemF;
use std::collections::HashMap;
use std::rc::Rc;

macro_rules! regex_elem {
  ($s:literal) => {
    (Regex::token($s))
  };
  (d) => {
    Rc::new(Regex::Terminal(Terminal::Dec))
  };
  (n) => {
    Rc::new(Regex::Terminal(Terminal::Num))
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

pub fn builtin_syntax_table<'a>() -> SyntaxTable {
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

  syntax.def("", regex_elems![n], SyntaxId::Num);
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
  syntax.def("0", regex_elems!["π", "{", id, "}", e], SyntaxId::Prop);
  syntax.def("5<", regex_elems![e, ".", d], SyntaxId::Proj);
  syntax.def("5<", regex_elems![e, ".", id], SyntaxId::Prop);

  syntax
}

use crate::types::syntax::{
  ElemId, ElemToken, LookupId, SyntaxExpr, SyntaxHandler, SyntaxInterpret,
};
use crate::types::tree::SynElem;
use baum_front::types::literal as lit;
use baum_front::types::tree as front;
use std::char::ParseCharError;
use std::num::ParseIntError;

#[derive(Debug, Clone)]
enum Vis {
  Explicit,
  Implicit,
}

struct MiniParser<'a, 'b> {
  input: &'b Vec<SynElem<'a>>,
  index: usize,
  next_elem_id: u16,
  tokens: Vec<ElemToken>,
}

enum LitType {
  Dec,
  Num,
  Chr,
  Str,
}

fn match_lit(e: &SynElemInternal, ty: LitType) -> bool {
  match (e, ty) {
    (SynElemF::Dec(_), LitType::Dec) => true,
    (SynElemF::Num(_), LitType::Num) => true,
    (SynElemF::Chr(_), LitType::Chr) => true,
    (SynElemF::Str(_), LitType::Str) => true,
    _ => false,
  }
}

impl<'a, 'b> MiniParser<'a, 'b> {
  fn new(input: &'b Vec<SynElem<'a>>) -> Self {
    Self {
      input,
      index: 0,
      next_elem_id: 0,
      tokens: Vec::new(),
    }
  }

  fn done(self) -> Option<Vec<ElemToken>> {
    if self.input.len() == self.index {
      Some(self.tokens)
    } else {
      None
    }
  }

  fn peek_token(&self, s: &'a str) -> Option<()> {
    let e = self.input.get(self.index)?;
    if let SynElemF::Token(t) = &e.0 {
      if *t == s {
        return Some(());
      }
    }
    None
  }

  fn take_token(&mut self, s: &'a str) -> Option<()> {
    self.peek_token(s)?;
    self.index += 1;
    self.tokens.push(ElemToken::Token);
    return Some(());
  }

  fn peek_id(&self) -> Option<()> {
    let e = self.input.get(self.index)?;
    if let SynElemF::Ident(_) = e.0 {
      Some(())
    } else {
      None
    }
  }

  fn take_id(&mut self) -> Option<LookupId> {
    self.peek_id()?;
    self.index += 1;
    let id = ElemId(self.next_elem_id);
    self.next_elem_id += 1;
    self.tokens.push(ElemToken::Ident(id));
    Some(LookupId::InSyntax(id))
  }

  fn take_lit(&mut self, ty: LitType) -> Option<&'a str> {
    let e = self.input.get(self.index)?;
    if !match_lit(&e.0, ty) {
      return None;
    }
    let (s, token) = match self.input.get(self.index).unwrap().0 {
      SynElemF::Token(_) => panic!(),
      SynElemF::Ident(_) => panic!(),
      SynElemF::Dec(s) => (s, ElemToken::Dec),
      SynElemF::Num(s) => (s, ElemToken::Num),
      SynElemF::Chr(s) => (s, ElemToken::Chr),
      SynElemF::Str(s) => (s, ElemToken::Str),
      SynElemF::Expr(_) => panic!(),
    };
    self.index += 1;
    self.tokens.push(token);
    Some(s)
  }

  fn take_expr(&mut self) -> Option<SyntaxExpr> {
    let e = self.input.get(self.index)?;
    if let SynElemF::Expr(_) = e.0 {
      self.index += 1;
      let id = ElemId(self.next_elem_id);
      self.next_elem_id += 1;
      self.tokens.push(ElemToken::Expr(id));
      return Some(SyntaxExpr(front::ExprF::Var(0, LookupId::InSyntax(id))));
    }
    None
  }
}

fn num_parse(s: &str) -> Result<lit::Literal, String> {
  let mut it = s.chars().peekable();
  let c0 = it.next().unwrap();
  enum State {
    Zero,    // 0
    Radix,   // 0b, 0o, 0x (non-accepting state)
    Nat,     // 123, 0x5c
    Frac,    // 123., 0x1.23
    Exp,     // 1e, 1p, 0x1.23p (non-accepting state)
    ExpSign, // 1e+ (non-accepting state)
    ExpNat,  // 1e+4
  }
  let mut hex = false;
  let mut s = if c0 == '0' { State::Zero } else { State::Nat };
  let mut coeff = c0.to_digit(10).unwrap();
  let mut frac_digits = 0;
  let mut nat_base = 10;
  let mut exp = 0;
  let mut exp_sign: i32 = 1;
  while let Some(c1) = it.peek() {
    let c1 = *c1;
    match s {
      State::Zero => {
        if c1 == 'b' || c1 == 'o' || c1 == 'x' {
          nat_base = match c1 {
            'b' => 2,
            'o' => 8,
            'x' => 16,
            _ => unreachable!(),
          };
          hex = c1 == 'x';
          s = State::Radix;
        } else if c1 == 'e' || c1 == 'E' {
          s = State::Exp;
        } else if c1 == '.' {
          s = State::Frac;
        } else if c1.is_ascii_digit() {
          let d = c1.to_digit(10).unwrap();
          coeff = d;
          s = State::Nat;
        } else {
          unreachable!();
        }
      }
      State::Radix | State::Nat => {
        if !hex && (c1 == 'e' || c1 == 'E') || hex && c1 == 'p' {
          s = State::Exp;
        } else if c1 == '.' {
          s = State::Frac;
        } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
          let d = c1
            .to_digit(nat_base)
            .ok_or(format!("invalid digit: {}", c1))?;
          coeff = coeff * nat_base + d;
          s = State::Nat;
        } else {
          unreachable!();
        }
      }
      State::Frac => {
        if !hex && (c1 == 'e' || c1 == 'E') || hex && c1 == 'p' {
          s = State::Exp;
        } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
          let d = c1
            .to_digit(nat_base)
            .ok_or(format!("invalid digit: {}", c1))?;
          coeff = coeff * nat_base + d;
          frac_digits += 1;
          s = State::Frac;
        } else {
          unreachable!();
        }
      }
      State::Exp => {
        if c1 == '+' || c1 == '-' {
          s = State::ExpSign;
          exp_sign = if c1 == '+' { 1 } else { -1 };
        } else if c1.is_ascii_digit() {
          let d = c1.to_digit(10).unwrap();
          exp = d;
          s = State::ExpNat;
        } else {
          unreachable!();
        }
      }
      State::ExpSign | State::ExpNat => {
        if c1.is_ascii_digit() {
          let d = c1.to_digit(10).unwrap();
          exp = exp * 10 + d;
          s = State::ExpNat;
        } else {
          unreachable!();
        }
      }
    }
    it.next();
  }
  let base: u8 = if hex { 2 } else { 10 };
  let mut exponent = exp_sign * exp as i32;
  exponent -= frac_digits as i32 * if hex { 4 } else { 1 };
  let lit = if exponent >= 0 {
    lit::Literal::Nat(lit::Nat {
      coeff,
      base,
      exponent: exponent as u32,
    })
  } else {
    lit::Literal::Rat(lit::Rat {
      coeff,
      base,
      exponent,
    })
  };
  Ok(lit)
}

pub fn builtin_syntax_handlers<'a>() -> HashMap<SyntaxId, SyntaxHandler<'a>> {
  let mut handlers: HashMap<SyntaxId, SyntaxHandler> = HashMap::new();

  fn make_handler<'a>(
    f: impl for<'b> Fn(&mut MiniParser<'a, 'b>) -> Result<SyntaxExpr, String> + 'static,
  ) -> SyntaxHandler<'a> {
    Rc::new(move |elems: &Vec<SynElem<'a>>| {
      let mut p = MiniParser::new(elems);
      let e = f(&mut p)?;
      let tokens = p.done().unwrap();
      Ok(SyntaxInterpret::new(tokens, e))
    })
  }

  handlers.insert(
    SyntaxId::Num,
    make_handler(|p| {
      let s = p.take_lit(LitType::Num).unwrap();
      let n = num_parse(s)?;
      Ok(SyntaxExpr(front::ExprF::Lit(n)))
    }),
  );
  handlers.insert(
    SyntaxId::Chr,
    make_handler(|p| {
      let s = p.take_lit(LitType::Chr).unwrap();
      let c = s.parse().map_err(|e: ParseCharError| e.to_string())?;
      let c = lit::Literal::Chr(c);
      Ok(SyntaxExpr(front::ExprF::Lit(c)))
    }),
  );
  handlers.insert(
    SyntaxId::Str,
    make_handler(|p| {
      let s = p.take_lit(LitType::Str).unwrap();
      // TODO: process escape
      let s = lit::Literal::Str(s.to_string());
      Ok(SyntaxExpr(front::ExprF::Lit(s)))
    }),
  );

  handlers.insert(
    SyntaxId::Hole,
    make_handler(|p| {
      p.take_token("_").unwrap();
      Ok(SyntaxExpr(front::ExprF::Hole))
    }),
  );
  handlers.insert(
    SyntaxId::Uni,
    make_handler(|p| {
      p.take_token("U").unwrap();
      Ok(SyntaxExpr(front::ExprF::Uni))
    }),
  );

  handlers.insert(
    SyntaxId::LamE,
    make_handler(|p| {
      p.take_token("λ").unwrap();
      p.take_token("(").unwrap();
      let mut args = Vec::new();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        let mut ids = Vec::new();
        while let Some(id) = p.take_id() {
          ids.push(id.clone());
        }
        let ty = if let Some(_) = p.take_token(":") {
          p.take_expr().unwrap()
        } else {
          SyntaxExpr(front::ExprF::Hole)
        };
        let ty = Rc::new(ty);
        for id in ids {
          args.push((id, ty.clone()));
        }
        p.take_token(",");
      }
      let body = p.take_expr().unwrap();
      let mut e = body;
      for (id, ty) in args.into_iter().rev() {
        e = SyntaxExpr(front::ExprF::LamE(id, ty, Rc::new(e)));
      }
      Ok(e)
    }),
  );
  handlers.insert(
    SyntaxId::PiE,
    make_handler(|p| {
      p.take_token("Π").unwrap();
      p.take_token("(").unwrap();
      let mut args = Vec::new();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        if let Some(_) = p.peek_id() {
          let mut ids = Vec::new();
          loop {
            if let Some(_) = p.take_token(":") {
              break;
            }
            let id = p.take_id().unwrap();
            ids.push(id.clone());
          }
          let e = Rc::new(p.take_expr().unwrap());
          for id in ids {
            args.push((Some(id.clone()), e.clone()));
          }
        } else {
          let e = p.take_expr().unwrap();
          args.push((None, Rc::new(e.clone())));
        }
        p.take_token(",");
      }
      let body = p.take_expr().unwrap();
      let mut e = body;
      for (id, ty) in args.into_iter().rev() {
        e = SyntaxExpr(front::ExprF::PiE(id, ty, Rc::new(e)));
      }
      Ok(e)
    }),
  );
  handlers.insert(
    SyntaxId::AppE,
    make_handler(|p| {
      let e1 = p.take_expr().unwrap();
      let e2 = p.take_expr().unwrap();
      Ok(SyntaxExpr(front::ExprF::AppE(Rc::new(e1), Rc::new(e2))))
    }),
  );

  handlers.insert(
    SyntaxId::LamI,
    make_handler(|p| {
      p.take_token("λ").unwrap();
      p.take_token("{").unwrap();
      let mut args = Vec::new();
      loop {
        if let Some(_) = p.take_token("}") {
          break;
        }
        let mut ids = Vec::new();
        while let Some(id) = p.take_id() {
          ids.push(id.clone());
        }
        let ty = if let Some(_) = p.take_token(":") {
          p.take_expr().unwrap()
        } else {
          SyntaxExpr(front::ExprF::Hole)
        };
        let ty = Rc::new(ty);
        for id in ids {
          args.push((id, ty.clone()));
        }
        p.take_token(",");
      }
      let body = p.take_expr().unwrap();
      let mut e = body;
      for (id, ty) in args.into_iter().rev() {
        e = SyntaxExpr(front::ExprF::LamI(id, ty, Rc::new(e)));
      }
      Ok(e)
    }),
  );
  handlers.insert(
    SyntaxId::PiI,
    make_handler(|p| {
      p.take_token("Π").unwrap();
      p.take_token("{").unwrap();
      let mut args = Vec::new();
      loop {
        if let Some(_) = p.take_token("}") {
          break;
        }
        if let Some(_) = p.peek_id() {
          let mut ids = Vec::new();
          loop {
            if let Some(_) = p.take_token(":") {
              break;
            }
            let id = p.take_id().unwrap();
            ids.push(id.clone());
          }
          let e = Rc::new(p.take_expr().unwrap());
          for id in ids {
            args.push((Some(id.clone()), e.clone()));
          }
        } else {
          let e = p.take_expr().unwrap();
          args.push((None, Rc::new(e.clone())));
        }
        p.take_token(",");
      }
      let body = p.take_expr().unwrap();
      let mut e = body;
      for (id, ty) in args.into_iter().rev() {
        e = SyntaxExpr(front::ExprF::PiI(id, ty, Rc::new(e)));
      }
      Ok(e)
    }),
  );
  handlers.insert(
    SyntaxId::AppI,
    make_handler(|p| {
      let e1 = p.take_expr().unwrap();
      p.take_token("{").unwrap();
      let e2 = p.take_expr().unwrap();
      p.take_token("}").unwrap();
      Ok(SyntaxExpr(front::ExprF::AppI(Rc::new(e1), Rc::new(e2))))
    }),
  );

  handlers.insert(
    SyntaxId::TupleTy,
    make_handler(|p| {
      let mut v = Vec::new();
      p.take_token("Σ").unwrap();
      p.take_token("(").unwrap();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        if let Some(_) = p.peek_id() {
          let mut ids = Vec::new();
          loop {
            if let Some(_) = p.take_token(":") {
              break;
            }
            let id = p.take_id().unwrap();
            ids.push(id.clone());
          }
          let e = Rc::new(p.take_expr().unwrap());
          for id in ids {
            v.push((Some(id.clone()), e.clone()));
          }
        } else {
          let e = p.take_expr().unwrap();
          v.push((None, Rc::new(e.clone())));
        }
        p.take_token(",");
      }
      Ok(SyntaxExpr(front::ExprF::TupleTy(v)))
    }),
  );
  handlers.insert(
    SyntaxId::TupleCon,
    make_handler(|p| {
      let mut v = Vec::new();
      p.take_token("(").unwrap();
      loop {
        if let Some(_) = p.take_token(")") {
          break;
        }
        let e = p.take_expr().unwrap();
        v.push(Rc::new(e.clone()));
        p.take_token(",");
      }
      Ok(SyntaxExpr(front::ExprF::TupleCon(v)))
    }),
  );
  handlers.insert(
    SyntaxId::Proj,
    make_handler(|p| {
      let (s, e) = if let Some(_) = p.take_token("π") {
        p.take_token("(").unwrap();
        let s = p.take_lit(LitType::Dec).unwrap();
        p.take_token(")").unwrap();
        let e = p.take_expr().unwrap();
        (s, e)
      } else {
        let e = p.take_expr().unwrap();
        p.take_token(".").unwrap();
        let s = p.take_lit(LitType::Dec).unwrap();
        (s, e)
      };
      let n = s.parse().map_err(|e: ParseIntError| e.to_string())?;
      Ok(SyntaxExpr(front::ExprF::Proj(n, Rc::new(e))))
    }),
  );

  handlers.insert(
    SyntaxId::ObjTy,
    make_handler(|p| {
      let mut v = Vec::new();
      p.take_token("Σ").unwrap();
      p.take_token("{").unwrap();
      loop {
        if let Some(_) = p.take_token("}") {
          break;
        }
        let mut ids = Vec::new();
        while let Some(id) = p.take_id() {
          ids.push(id.clone());
        }
        p.take_token(":").unwrap();
        let e = Rc::new(p.take_expr().unwrap());
        for id in ids {
          v.push((id, e.clone()));
        }
        p.take_token(",");
      }
      Ok(SyntaxExpr(front::ExprF::ObjTy(v)))
    }),
  );
  handlers.insert(
    SyntaxId::ObjCon,
    make_handler(|p| {
      let mut v = Vec::new();
      p.take_token("{").unwrap();
      loop {
        if let Some(_) = p.take_token("}") {
          break;
        }
        let id = p.take_id().unwrap();
        let mut args = Vec::new();
        loop {
          if p.peek_token(":").is_some() || p.peek_token("=").is_some() {
            break;
          }
          if let Some(i) = p.take_id() {
            args.push((Vis::Explicit, i, Rc::new(SyntaxExpr(front::ExprF::Hole))));
          } else {
            let explicit = p.peek_token("(").is_some();
            let implicit = p.peek_token("{").is_some();
            let vis = match (explicit, implicit) {
              (true, false) => Vis::Explicit,
              (false, true) => Vis::Implicit,
              _ => unreachable!(),
            };
            p.take_token(match vis {
              Vis::Explicit => "(",
              Vis::Implicit => "{",
            });
            let mut ids = Vec::new();
            while let Some(id) = p.take_id() {
              ids.push(id.clone());
            }
            let ty = if let Some(_) = p.take_token(":") {
              p.take_expr().unwrap()
            } else {
              SyntaxExpr(front::ExprF::Hole)
            };
            p.take_token(match vis {
              Vis::Explicit => ")",
              Vis::Implicit => "}",
            });
            let ty = Rc::new(ty);
            for id in ids {
              args.push((vis.clone(), id, ty.clone()));
            }
          }
        }
        let ty = if let Some(_) = p.take_token(":") {
          Some(p.take_expr().unwrap())
        } else {
          None
        };
        p.take_token("=");
        let e = p.take_expr().unwrap();
        let e = match ty {
          Some(ty) => SyntaxExpr(front::ExprF::Ann(Rc::new(e), Rc::new(ty))),
          None => e,
        };
        let mut e = e;
        for (vis, id, ty) in args.into_iter().rev() {
          match vis {
            Vis::Explicit => e = SyntaxExpr(front::ExprF::LamE(id, ty, Rc::new(e))),
            Vis::Implicit => e = SyntaxExpr(front::ExprF::LamI(id, ty, Rc::new(e))),
          }
        }
        v.push((id, Rc::new(e)));
        p.take_token(",");
      }
      Ok(SyntaxExpr(front::ExprF::ObjCon(v)))
    }),
  );
  handlers.insert(
    SyntaxId::Prop,
    make_handler(|p| {
      let (i, e) = if let Some(_) = p.take_token("π") {
        p.take_token("{").unwrap();
        let i = p.take_id().unwrap();
        p.take_token("}").unwrap();
        let e = p.take_expr().unwrap();
        (i, e)
      } else {
        let e = p.take_expr().unwrap();
        p.take_token(".").unwrap();
        let i = p.take_id().unwrap();
        (i, e)
      };
      Ok(SyntaxExpr(front::ExprF::Prop(i, Rc::new(e))))
    }),
  );

  handlers
}
