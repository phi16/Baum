use crate::types::parse::*;

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

  fn m(&mut self, m: &Module) -> &mut Self {
    match &m.0 {
      ModuleF::Decls(ds) => self.s("{").open().ds(ds).close().s("}").ln(),
      ModuleF::Import(s) => self.s("import ").s("\"").s(s).s("\"").ln(),
      ModuleF::Ref(is, params) => {
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

  fn d(&mut self, d: &Decl) -> &mut Self {
    match &d.0 {
      DeclF::Local(ds) => self.s("local {").open().ds(ds).close().s("}").ln(),
      DeclF::Module(md, m) => {
        self.s("module ").i(&md.name);
        for arg in &md.params {
          self.arg(arg);
        }
        self.s(" = ").m(m)
      }
      DeclF::Use(m) => self.s("use ").m(m),
      DeclF::Open(m) => self.s("open ").m(m),
      DeclF::Def(def) => self.def(def).ln(),
      DeclF::Syntax(_) => self.s("syntax ").ln(),
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
      ExprF::Lit(l) => match l {
        Literal::Nat(n) => self.s(n),
        Literal::Rat(n) => self.s(n),
        Literal::Chr(c) => self.s("'").s(c).s("'"),
        Literal::Str(s) => self.s("\"").s(s).s("\""),
      },
      ExprF::Var(i) => self.i(i),
      ExprF::Ext(m, i) => self.is(m, ".").s(".").i(i),
      ExprF::Syntax((_, se)) => {
        self.s("[");
        for e in se.into_iter() {
          self.s(" ");
          match e {
            SyntaxElem::Token(s) => self.s(s),
            SyntaxElem::Ident(i) => self.i(i),
            SyntaxElem::Nat(s) => self.s(s),
            SyntaxElem::Str(s) => self.s("\"").s(s).s("\""),
            SyntaxElem::Def(def) => self.def(def),
            SyntaxElem::Expr(e) => self.e(e),
            SyntaxElem::Decls(ds) => self.open().ds(ds).close(),
          };
        }
        self.s(" ]");
        self
      }
      _ => unimplemented!(),
    }
  }
}

pub fn pretty(ds: &Vec<Decl>) -> String {
  let mut p = Pretty::new();
  p.ds(ds);
  p.str.join("\n")
}
