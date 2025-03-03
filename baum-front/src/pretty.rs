use crate::types::literal::*;
use crate::types::tree::*;
use std::collections::HashMap;

struct Pretty<'a> {
  symbols: &'a HashMap<Id, String>,
  indent: u32,
  str: Vec<String>,
  line: Vec<String>,
}

impl<'a> Pretty<'a> {
  fn new(symbols: &'a HashMap<Id, String>) -> Self {
    Pretty {
      symbols,
      indent: 0,
      str: Vec::new(),
      line: Vec::new(),
    }
  }

  fn id_to_str(&self, i: &Id) -> String {
    match self.symbols.get(i) {
      Some(s) => format!("{}{:?}", s, i),
      None => format!("{:?}", i),
    }
  }

  fn i(&mut self, i: &Id) -> &mut Self {
    self.line.push(self.id_to_str(i));
    self
  }

  fn is(&mut self, i: &Vec<Id>, sep: &str) -> &mut Self {
    let mid: Vec<String> = i.iter().map(|i| self.id_to_str(i)).collect();
    self.line.push(mid.join(sep));
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

  fn mb(&mut self, m: &ModBody) -> &mut Self {
    match &m {
      ModBody::Decls(ds) => self.s("{").open().ds(ds).close().s("}").ln(),
      ModBody::Import(path) => self.s("import ").s(path).ln(),
      ModBody::App(l, mod_name, args) => {
        self.s(&format!("[{}]", l)).is(mod_name, ".");
        for (vis, e) in args {
          self.s(" ");
          match vis {
            Vis::Explicit => self.s("(").e(e).s(")"),
            Vis::Implicit => self.s("{").e(e).s("}"),
          };
        }
        self.ln()
      }
    }
  }

  fn d(&mut self, d: &Decl) -> &mut Self {
    match d {
      Decl::Mod(name, params, body) => {
        self.s("module ").i(&name);
        for (vis, id, ty) in params {
          match vis {
            Vis::Explicit => self.s(" (").i(id).s(": ").e(ty).s(")"),
            Vis::Implicit => self.s(" {").i(id).s(": ").e(ty).s("}"),
          };
        }
        self.s(" = ").mb(body)
      }
      Decl::Def(i, e) => self.i(i).s(" = ").e(e).ln(),
    }
  }

  fn ds(&mut self, ds: &Vec<Decl>) -> &mut Self {
    for d in ds {
      self.d(d);
    }
    self
  }

  fn e(&mut self, e: &Expr) -> &mut Self {
    match &e.0 {
      ExprF::Hole => self.s("_"),
      ExprF::Bind(i) => self.i(i),
      ExprF::Ann(v, t) => self.e(v).s(" of ").e(t),
      ExprF::Uni => self.s("𝒰"),
      ExprF::Ext(l, m, i) => {
        self.s(&format!("[{}]", l));
        if m.is_empty() {
          self.is(m, ".").s(".").i(i)
        } else {
          self.i(i)
        }
      }
      ExprF::Let(ds, e) => self.s("let").open().ds(ds).close().s("in ").e(e),
      ExprF::Lit(Literal::Nat(n)) => self.s(&format!("{:?}", n)),
      ExprF::Lit(Literal::Rat(r)) => self.s(&format!("{:?}", r)),
      ExprF::Lit(Literal::Chr(c)) => self.s(&format!("{:?}", c)),
      ExprF::Lit(Literal::Str(s)) => self.s(&format!("{:?}", s)),

      ExprF::PiE(i, t, e) => match i {
        Some(i) => self.s("Π(").i(i).s(": ").e(t).s(") ").e(e),
        None => self.s("Π(").e(t).s(") ").e(e),
      },
      ExprF::LamE(i, t, e) => self.s("λ(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::AppE(e1, e2) => match e2.0 {
        ExprF::Hole
        | ExprF::Bind(_)
        | ExprF::Uni
        | ExprF::Ext(_, _, _)
        | ExprF::Lit(_)
        | ExprF::TupleTy(_)
        | ExprF::TupleCon(_)
        | ExprF::ObjTy(_)
        | ExprF::ObjCon(_) => self.e(e1).s(" ").e(e2),
        _ => self.e(e1).s(" (").e(e2).s(")"),
      },

      ExprF::PiI(i, t, e) => match i {
        Some(i) => self.s("Π{").i(i).s(": ").e(t).s("} ").e(e),
        None => self.s("Π{").e(t).s("} ").e(e),
      },
      ExprF::LamI(i, t, e) => self.s("λ{").i(i).s(": ").e(t).s("} ").e(e),
      ExprF::AppI(e1, e2) => self.e(e1).s(" {").e(e2).s("}"),

      ExprF::TupleTy(ts) => {
        self.s("Σ(");
        for (i, t) in ts {
          match i {
            Some(i) => self.i(i).s(": "),
            None => self,
          };
          self.e(t).s(", ");
        }
        self.s(")")
      }
      ExprF::TupleCon(es) => {
        self.s("(");
        for e in es {
          self.e(e).s(", ");
        }
        self.s(")")
      }
      ExprF::Proj(i, e) => self.s("π(").s(&format!("{}", i)).s(") ").e(e),

      ExprF::ObjTy(es) => {
        self.s("Σ{");
        for (i, t) in es {
          self.i(i).s(": ").e(t).s(", ");
        }
        self.s("}")
      }
      ExprF::ObjCon(es) => {
        self.s("{");
        for (i, e) in es {
          self.i(i).s(" = ").e(e).s(", ");
        }
        self.s("}")
      }
      ExprF::Prop(i, e) => self.s("π{").i(i).s("} ").e(e),
    }
  }
}

pub fn pretty(program: &Program) -> String {
  let mut p = Pretty::new(&program.symbols);
  p.ds(&program.decls);
  p.str.join("\n")
}
