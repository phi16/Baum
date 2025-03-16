use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
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

  fn defs<P, S>(&mut self, ds: &Vec<(BindId, Rc<Expr<P, S>>)>) -> &mut Self {
    for (i, e) in ds {
      self.i(i).s(" = ").e(e).ln();
    }
    self
  }

  fn e<P, S>(&mut self, e: &Expr<P, S>) -> &mut Self {
    match &e.0 {
      ExprF::Hole => self.s("_"),
      ExprF::Bind(i) => self.i(i),
      ExprF::Ann(v, t) => self.e(v).s(" of ").e(t),
      ExprF::Def(e) => self.s("def ").e(e),
      ExprF::Uni => self.s("𝒰"),
      ExprF::Let(defs, e) => self.s("let").open().defs(defs).close().s("in ").e(e),

      ExprF::Pi(_, Vis::Explicit, i, t, e) => self.s("Π(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::Lam(_, Vis::Explicit, i, t, e) => self.s("λ(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::App(_, Vis::Explicit, e1, e2) => match e2.0 {
        ExprF::Hole | ExprF::Bind(_) | ExprF::Uni | ExprF::Sigma(_, _) | ExprF::Obj(_, _) => {
          self.e(e1).s(" ").e(e2)
        }
        _ => self.e(e1).s(" (").e(e2).s(")"),
      },

      ExprF::Pi(_, Vis::Implicit, i, t, e) => self.s("Π{").i(i).s(": ").e(t).s("} ").e(e),
      ExprF::Lam(_, Vis::Implicit, i, t, e) => self.s("λ{").i(i).s(": ").e(t).s("} ").e(e),
      ExprF::App(_, Vis::Implicit, e1, e2) => self.e(e1).s(" {").e(e2).s("}"),

      ExprF::Sigma(_, es) => {
        self.s("Σ{");
        for (name, bind, t) in es {
          self.name(name).s("~").i(bind).s(": ").e(t).s(", ");
        }
        self.s("}")
      }
      ExprF::Obj(_, es) => {
        self.s("{");
        for (name, e) in es {
          self.name(name).s(" = ").e(e).s(", ");
        }
        self.s("}")
      }
      ExprF::Prop(_, e, name) => self.e(e).s(".").name(name),
    }
  }

  fn v<P, S>(&mut self, v: &Val<P, S>) -> &mut Self {
    match &v.0 {
      ValF::Hole => self.s("_"),
      ValF::Bind(i) => self.i(i),
      ValF::Uni => self.s("𝒰"),

      ValF::Pi(_, Vis::Explicit, i, t, e) => self.s("Π(").i(i).s(": ").v(t).s(") ").v(e),
      ValF::Lam(_, Vis::Explicit, i, t, e) => self.s("λ(").i(i).s(": ").v(t).s(") ").v(e),
      ValF::App(_, Vis::Explicit, e1, e2) => match e2.0 {
        ValF::Hole | ValF::Bind(_) | ValF::Uni | ValF::Sigma(_, _) | ValF::Obj(_, _) => {
          self.i(e1).s(" ").v(e2)
        }
        _ => self.i(e1).s(" (").v(e2).s(")"),
      },

      ValF::Pi(_, Vis::Implicit, i, t, e) => self.s("Π{").i(i).s(": ").v(t).s("} ").v(e),
      ValF::Lam(_, Vis::Implicit, i, t, e) => self.s("λ{").i(i).s(": ").v(t).s("} ").v(e),
      ValF::App(_, Vis::Implicit, e1, e2) => self.i(e1).s(" {").v(e2).s("}"),

      ValF::Sigma(_, es) => {
        self.s("Σ{");
        for (name, bind, t) in es {
          self.name(name).s("~").i(bind).s(": ").v(t).s(", ");
        }
        self.s("}")
      }
      ValF::Obj(_, es) => {
        self.s("{");
        for (name, e) in es {
          self.name(name).s(" = ").v(e).s(", ");
        }
        self.s("}")
      }
      ValF::Prop(_, e, name) => self.i(e).s(".").name(name),
    }
  }
}

pub fn pretty<P, S>(program: &Program<P, S>) -> String {
  let mut p = Pretty::new(&program.bind_symbols, &program.name_symbols);
  p.defs(&program.defs);
  p.str.join("\n")
}

pub fn pretty_expr<P, S>(
  bind_symbols: &HashMap<BindId, String>,
  name_symbols: &HashMap<NameId, String>,
  e: &Expr<P, S>,
) -> String {
  let mut p = Pretty::new(bind_symbols, name_symbols);
  p.e(e).ln();
  p.str.join("\n")
}

pub fn pretty_val<P, S>(
  bind_symbols: &HashMap<BindId, String>,
  name_symbols: &HashMap<NameId, String>,
  v: &Val<P, S>,
) -> String {
  let mut p = Pretty::new(bind_symbols, name_symbols);
  p.v(v).ln();
  p.str.join("\n")
}
