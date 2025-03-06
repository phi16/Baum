use crate::types::tree::*;
use std::collections::HashMap;
use std::rc::Rc;

struct Pretty<'a> {
  bind_symbols: &'a HashMap<BindId, String>,
  name_symbols: &'a HashMap<NameId, String>,
  indent: u32,
  str: Vec<String>,
  line: Vec<String>,
}

impl<'a> Pretty<'a> {
  fn new(
    bind_symbols: &'a HashMap<BindId, String>,
    name_symbols: &'a HashMap<NameId, String>,
  ) -> Self {
    Pretty {
      bind_symbols,
      name_symbols,
      indent: 0,
      str: Vec::new(),
      line: Vec::new(),
    }
  }

  fn i(&mut self, bind: &BindId) -> &mut Self {
    self.line.push(self.bind_symbols.get(bind).unwrap().clone());
    self
  }

  fn name(&mut self, name: &NameId) -> &mut Self {
    self.line.push(self.name_symbols.get(name).unwrap().clone());
    self
  }

  fn s(&mut self, s: &str) -> &mut Self {
    self.line.push(s.to_string());
    self
  }

  fn ln(&mut self) -> &mut Self {
    let pad = " ".repeat(self.indent as usize);
    let str = self.line.join("");
    self.str.push(pad + &str);
    self.line.clear();
    self
  }

  fn open(&mut self) -> &mut Self {
    self.ln();
    self.indent += 2;
    self
  }

  fn close(&mut self) -> &mut Self {
    self.indent -= 2;
    self
  }

  fn defs(&mut self, ds: &Vec<(BindId, Rc<Expr>)>) -> &mut Self {
    for (i, e) in ds {
      self.i(i).s(" = ").e(e).ln();
    }
    self
  }

  fn e(&mut self, e: &Expr) -> &mut Self {
    match &e.0 {
      ExprF::Hole => self.s("_"),
      ExprF::Bind(i) => self.i(i),
      ExprF::Ann(v, t) => self.e(v).s(" of ").e(t),
      ExprF::Synth(e) => self.s("synth ").e(e),
      ExprF::Uni => self.s("𝒰"),
      ExprF::Let(defs, e) => self.s("let").open().defs(defs).close().s("in ").e(e),

      ExprF::Pi(tag, i, t, e) => match i {
        Some(i) => self.s("Π(").i(i).s(": ").e(t).s(") ").e(e),
        None => self.s("Π(").e(t).s(") ").e(e),
      },
      ExprF::Lam(tag, i, t, e) => self.s("λ(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::App(tag, e1, e2) => match e2.0 {
        ExprF::Hole | ExprF::Bind(_) | ExprF::Uni | ExprF::Sigma(_, _) | ExprF::Obj(_, _) => {
          self.e(e1).s(" ").e(e2)
        }
        _ => self.e(e1).s(" (").e(e2).s(")"),
      },

      ExprF::Sigma(tag, es) => {
        self.s("Σ{");
        for (name, bind, t) in es {
          if let Some(bind) = bind {
            self.name(name).s("~").i(bind).s(": ").e(t).s(", ");
          } else {
            self.name(name).s(": ").e(t).s(", ");
          }
        }
        self.s("}")
      }
      ExprF::Obj(tag, es) => {
        self.s("{");
        for (name, e) in es {
          self.name(name).s(" = ").e(e).s(", ");
        }
        self.s("}")
      }
      ExprF::Prop(tag, name, e) => self.e(e).s(".").name(name),
    }
  }
}

pub fn pretty(program: &Program) -> String {
  let mut p = Pretty::new(&program.bind_symbols, &program.name_symbols);
  p.defs(&program.defs);
  p.str.join("\n")
}
