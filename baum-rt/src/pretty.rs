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
      TreeF::Lam(b, _, e) => self.s("λ(").i(b).s(") ").t(e),
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

  fn e(&mut self, e: &Env) -> &mut Self {
    let mut first = true;
    for (i, v) in e.iter() {
      if !first {
        self.s(", ");
      }
      self.i(i).s(" = ").v(v);
      first = false;
    }
    self
  }

  fn k(&mut self, k: &Thunk) -> &mut Self {
    /* match k {
      Thunk::Val(v) => self.s("Val[").v(v).s("]"),
      Thunk::Op(op, ks) => {
        match op {
          Op::Eval(e, env) => self.s("Eval[").t(e).s(" in ").e(env).s("]"),
          Op::Prim(name, args) => {
            self.s("Prim[").s(name).s("(");
            let mut first = true;
            for arg in args {
              if !first {
                self.s(", ");
              }
              self.v(arg);
              first = false;
            }
            self.s(")]")
          }
        };
        self.open();
        for k in ks {
          match k {
            Cont::App(v) => self.s("↑ App[").v(v).s("]").ln(),
            Cont::Prop(n) => self.s("↑ Prop[").name(n).s("]").ln(),
          };
        }
        self.close()
      }
    } */
    self.s("[Thunk]")
  }

  fn v(&mut self, v: &Val) -> &mut Self {
    match v {
      Val::Unit => self.s("()"),
      Val::Raw(r) => match r {
        Raw::U32(n) => self.s("U32[").s(&n.to_string()).s("]"),
        Raw::Action(_) => self.s("Action[]"),
        Raw::Done => self.s("Done"),
      },
      Val::Prim(name, l, args) => {
        self.s(&format!("{}<{:?}>", name, l)).s("(");
        let mut first = true;
        for arg in args {
          if !first {
            self.s(", ");
          }
          self.v(arg);
          first = false;
        }
        self.s(")")
      }
      Val::Cl(i, env, e) => self.s("λ(").i(i).s(") ").t(e),
      Val::Obj(ps) => {
        self.s("{");
        let mut first = true;
        for (n, v) in ps {
          if !first {
            self.s(", ");
          }
          self.name(n).s(": ").v(v);
          first = false;
        }
        self.s("}")
      }
    }
  }
}

pub fn ppt(t: &Rc<Tree>) -> String {
  let mut p = Pretty::new();
  p.t(t).ln();
  p.str.join("\n")
}

pub fn ppk(k: &Thunk) -> String {
  let mut p = Pretty::new();
  p.k(k).ln();
  p.str.join("\n")
}

pub fn ppv(v: &Val) -> String {
  let mut p = Pretty::new();
  p.v(v).ln();
  p.str.join("\n")
}
