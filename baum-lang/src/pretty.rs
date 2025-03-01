use crate::types::tree::*;
use crate::types::tree_base::*;

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
    self.line.push(i.as_str().to_string());
    self
  }

  fn is(&mut self, i: &Vec<Id>, sep: &str) -> &mut Self {
    let mid: Vec<&str> = i.iter().map(|i| i.as_str()).collect();
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

  fn arg(&mut self, arg: &Arg) -> &mut Self {
    let (vis, ids, ty) = arg;
    self.s(" ");
    match vis {
      Vis::Explicit => self.s("("),
      Vis::Implicit => self.s("{"),
    };
    self.is(ids, " ");
    if let Some(ty) = ty {
      self.s(": ").e(&*ty);
    }
    match vis {
      Vis::Explicit => self.s(")"),
      Vis::Implicit => self.s("}"),
    }
  }

  fn def(&mut self, def: &Def) -> &mut Self {
    self.i(&def.name);
    for arg in &def.args {
      self.arg(arg);
    }
    if let Some(ty) = &def.ty {
      self.s(": ").e(&*ty);
    }
    self.s(" = ").e(&*def.body)
  }

  fn mr(&mut self, m: &ModRef) -> &mut Self {
    match &m.0 {
      ModRefF::Import(s) => self.s("import ").s("\"").s(s).s("\"").ln(),
      ModRefF::App(is, params) => {
        self.is(is, ".");
        if !params.is_empty() {
          for (vis, e) in params {
            self.s(" ");
            match vis {
              Vis::Explicit => self.s("("),
              Vis::Implicit => self.s("{"),
            };
            self.e(&*e);
            match vis {
              Vis::Explicit => self.s(")"),
              Vis::Implicit => self.s("}"),
            };
          }
        }
        self.ln()
      }
    }
  }

  fn mb(&mut self, m: &ModBody) -> &mut Self {
    match &m {
      ModBodyF::Decls(ds) => self.s("{").open().ds(ds).close().s("}").ln(),
      ModBodyF::Ref(mr) => self.mr(mr),
    }
  }

  fn d(&mut self, d: &Decl) -> &mut Self {
    match &d.0 {
      DeclF::Local(ds) => self.s("local {").open().ds(ds).close().s("}").ln(),
      DeclF::Mod(md, mb) => {
        self.s("module ").i(&md.name);
        for arg in &md.params {
          self.arg(arg);
        }
        self.s(" = ").mb(mb)
      }
      DeclF::Open(mr) => self.s("open ").mr(mr),
      DeclF::Use(mr) => self.s("use ").mr(mr),
      DeclF::Def(def) => self.def(def).ln(),
      DeclF::Syntax(_, prec_str, defs, e) => {
        self.s("syntax ");
        if let Some(prec_str) = prec_str {
          self.s(prec_str).s(" ");
        }
        for def in defs {
          match &def.0 {
            SynDefF::Token(s) => self.s(s),
            SynDefF::Ident(i) => self.i(i),
            SynDefF::Expr(i) => self.i(i),
          };
          self.s(" ");
        }
        self.s("= ").e(e).ln()
      }
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
      ExprF::Var(i) => self.i(i),
      ExprF::Mod(m) => self.s("module ").is(m, "."),
      ExprF::Ext(m, i) => self.is(m, ".").s(".").i(i),
      ExprF::Let(ds, e) => self.s("let").open().ds(ds).close().s("in ").e(e),
      ExprF::Syntax(mod_name, _, se) => {
        let mut qualified = !mod_name.is_empty();
        self.s("[");
        for e in se.into_iter() {
          self.s(" ");
          match &e.0 {
            SynElemF::Token(s) => {
              if qualified {
                qualified = false;
                self.is(mod_name, ".").s(".").s(s)
              } else {
                self.s(s)
              }
            }
            SynElemF::Ident(i) => self.i(i),
            SynElemF::Dec(s) => self.s(s),
            SynElemF::Num(s) => self.s(s),
            SynElemF::Chr(s) => self.s("'").s(s).s("'"),
            SynElemF::Str(s) => self.s("\"").s(s).s("\""),
            SynElemF::Expr(e) => self.e(e),
          };
        }
        self.s(" ]");
        self
      }
    }
  }
}

pub fn pretty(ds: &Vec<Decl>) -> String {
  let mut p = Pretty::new();
  p.ds(ds);
  p.str.join("\n")
}
