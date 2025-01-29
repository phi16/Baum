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

  fn c(&mut self, c: &Context) -> &mut Self {
    match c {
      ContextF::Ref(i, e) => self.i(i).s(": ").e(e),
    }
  }

  fn def(&mut self, def: &Def) -> &mut Self {
    self.i(&def.name);
    for (arg, vis) in &def.args {
      self.s(" ");
      match arg {
        ArgF::Ids(ids) => match vis {
          Vis::Explicit => self.is(&ids, " "),
          Vis::Implicit => self.s("{").is(&ids, " ").s("}"),
        },
        ArgF::Ty(e) => unreachable!(),
        ArgF::IdsTy(ids, e) => match vis {
          Vis::Explicit => self.s("(").is(ids, " ").s(": ").e(&*e).s(")"),
          Vis::Implicit => self.s("{").is(ids, " ").s(": ").e(&*e).s("}"),
        },
      };
    }
    if let Some(ty) = &def.ty {
      self.s(": ").e(&*ty);
    }
    self.s(" = ").e(&*def.body)
  }

  fn m(&mut self, m: &Module) -> &mut Self {
    match &m.0 {
      ModuleF::Decls(Some(i), ds) => self.i(i).s(" {").open().ds(ds).close().s("}"),
      ModuleF::Decls(None, ds) => self.s("{").open().ds(ds).close().s("}"),
      ModuleF::Import(s) => self.s("import ").s(s),
      ModuleF::Ref(is) => self.is(is, "."),
    }
  }

  fn d(&mut self, d: &Decl) -> &mut Self {
    match &d.0 {
      DeclF::Context(c, d) => self.s("[").c(c).s("] ").d(d),
      DeclF::Module(Access::Keep, m) => self.m(m),
      DeclF::Module(Access::Open, m) => self.s("open ").m(m),
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
      ExprF::Prim(n) => self.s("prim ").s(n),
      ExprF::Syntax(s, se) => {
        self.s("Syntax");
        for (i, e) in se.into_iter().enumerate() {
          self.s(" ");
          match e {
            SyntaxElems::Token(s) => self.s(s),
            SyntaxElems::Ident(i) => self.i(i),
            SyntaxElems::Def(def) => self.def(def),
            SyntaxElems::Expr(e) => self.e(e),
            SyntaxElems::Decls(ds) => self.ds(ds),
          };
        }
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
