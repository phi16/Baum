use crate::types::tree::*;
use std::rc::Rc;

struct Pretty {
  indent: u32,
  str: Vec<String>,
  line: Vec<String>,
}

impl Pretty {
  fn new() -> Self {
    Pretty {
      indent: 0,
      str: Vec::new(),
      line: Vec::new(),
    }
  }

  fn i(&mut self, i: &Id) -> &mut Self {
    self.line.push(format!("{:?}", i));
    self
  }

  fn name(&mut self, n: &Name) -> &mut Self {
    self.line.push(format!("{:?}", n));
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

  fn ds(&mut self, ds: &Vec<(Id, Rc<Tree>)>) -> &mut Self {
    for (i, e) in ds {
      self.i(i).s(" = ").t(e).ln();
    }
    self
  }

  fn t(&mut self, t: &Rc<Tree>) -> &mut Self {
    match &t.0 {
      TreeF::Var(i) => self.i(i),
      TreeF::Unit => self.s("()"),
      TreeF::Prim(s) => self.s("prim[").s(s).s("]"),
      TreeF::Let(ds, e) => self.s("let").open().ds(ds).close().s("in ").t(e),
      TreeF::Lam(b, e) => self.s("λ(").i(b).s(") ").t(e),
      TreeF::App(e1, e2) => match e2.0 {
        TreeF::Var(_) | TreeF::Unit | TreeF::Prim(_) | TreeF::Obj(_) => self.t(e1).s(" ").t(e2),
        _ => self.t(e1).s(" (").t(e2).s(")"),
      },
      TreeF::Obj(ps) => {
        self.s("{");
        let mut first = true;
        for (n, e) in ps {
          if !first {
            self.s(", ");
          }
          self.name(n).s(": ").t(e);
          first = false;
        }
        self.s("}")
      }
      TreeF::Prop(e, n) => self.t(e).s(".").name(n),
    }
  }
}

pub fn pretty(t: &Rc<Tree>) -> String {
  let mut p = Pretty::new();
  p.t(t).ln();
  p.str.join("\n")
}
