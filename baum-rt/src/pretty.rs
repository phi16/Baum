use baum_core::types::literal::Literal;

use crate::types::code::{Code, Global, Op, OpIx, Ref};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
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

  fn lit(&mut self, l: &Literal) -> &mut Self {
    match l {
      Literal::Nat(n) => self.s(format!("{:?}", n).as_str()),
      Literal::Rat(r) => self.s(format!("{:?}", r).as_str()),
      Literal::Chr(c) => self.s(format!("{:?}", c).as_str()),
      Literal::Str(s) => self.s(format!("{:?}", s).as_str()),
    }
  }

  fn t(&mut self, t: &Rc<Tree>) -> &mut Self {
    match &t.0 {
      TreeF::Var(i) => self.i(i),
      TreeF::Unit => self.s("()"),
      TreeF::Prim(s) => self.s("prim[").s(s).s("]"),
      TreeF::Lit(l) => self.lit(l),
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

  fn r(&mut self, r: &OpIx) -> &mut Self {
    self.s(&format!("%{:?}", r))
  }

  fn op(&mut self, op: &Op<OpIx>) -> &mut Self {
    match op {
      Op::Ref(Ref::Arg) => self.s("arg"),
      Op::Ref(Ref::Env(i)) => self.s(&format!("env[{:?}]", i)),
      Op::Unit => self.s("()"),
      Op::Prim(s) => self.s("prim \"").s(s).s("\""),
      Op::Lit(l) => self.lit(l),
      Op::Lam(fun, env) => {
        self.s(&format!("Cl[fun {:?}", fun));
        for i in env {
          self.s(", ").r(i);
        }
        self.s("]")
      }
      Op::App(f, x) => self.r(f).s("(").r(x).s(")"),
      Op::Obj(ps) => {
        self.s("{");
        let mut first = true;
        for (n, e) in ps {
          if !first {
            self.s(", ");
          }
          self.name(n).s(": ").r(e);
          first = false;
        }
        self.s("}")
      }
      Op::Prop(o, n) => self.r(o).s(".").name(n),
    }
  }

  fn c(&mut self, c: &Code) -> &mut Self {
    for (i, op) in c.ops.iter().enumerate() {
      self.s(&format!("%{:?} = ", i)).op(op).ln();
    }
    self.s("return ").r(&c.ret).ln()
  }

  fn g(&mut self, g: &Global) -> &mut Self {
    for (i, c) in g.funs.iter().enumerate() {
      self
        .s(&format!("fun {:?} (envs: {:?}) {{", i, c.env_count))
        .open()
        .c(c)
        .close()
        .s("}")
        .ln();
    }
    self.s(&format!("main = fun {:?}", g.main));
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
    match &v.0 {
      ValF::Unit => self.s("()"),
      ValF::Raw(r) => match r {
        Raw::U32(n) => self.s("U32[").s(&n.to_string()).s("]"),
        Raw::F32(f) => self.s("F32[").s(&f.to_string()).s("]"),
        Raw::Char(c) => self.s("Char[").s(&c.to_string()).s("]"),
        Raw::String(s) => self.s("String[").s(&s.to_string()).s("]"),
        Raw::Action(_) => self.s("Action[]"),
        Raw::Done => self.s("Done"),
      },
      ValF::Prim(name, l, args) => {
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
      ValF::Cl(fun, env) => {
        self.s(&format!("fun {:?} [", fun));
        let mut first = true;
        for e in env {
          if !first {
            self.s(", ");
          }
          self.v(e);
          first = false;
        }
        self.s("]")
      }
      ValF::Obj(ps) => {
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

pub fn ppg(g: &Global) -> String {
  let mut p = Pretty::new();
  p.g(g).ln();
  p.str.join("\n")
}
