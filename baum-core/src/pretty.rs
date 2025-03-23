use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

struct Pretty<'a> {
  def_symbols: &'a HashMap<DefId, String>,
  bind_symbols: &'a HashMap<BindId, String>,
  name_symbols: &'a HashMap<NameId, String>,
  indent: u32,
  str: Vec<String>,
  line: Vec<String>,
}

impl<'a> Pretty<'a> {
  fn new(
    def_symbols: &'a HashMap<DefId, String>,
    bind_symbols: &'a HashMap<BindId, String>,
    name_symbols: &'a HashMap<NameId, String>,
  ) -> Self {
    Pretty {
      def_symbols,
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

  fn di(&mut self, def: &DefId) -> &mut Self {
    self.line.push(self.def_symbols.get(def).unwrap().clone());
    self
  }

  fn hi(&mut self, hole: &HoleId) -> &mut Self {
    self.s("?");
    self.line.push(hole.0.to_string());
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

  fn defs<P, S>(&mut self, ds: &Vec<(DefId, Box<Expr<P, S>>)>) -> &mut Self {
    for (i, e) in ds {
      self.di(i).s(" = ").e(e).ln();
    }
    self
  }

  fn defs_c<P, S>(&mut self, ds: &Vec<(DefId, Rc<CExpr<P, S>>)>) -> &mut Self {
    for (i, e) in ds {
      self.di(i).s(" = ").c(e).ln();
    }
    self
  }

  fn e<P, S>(&mut self, e: &Expr<P, S>) -> &mut Self {
    match &e.0 {
      ExprF::Hole => self.s("_"),
      ExprF::Bind(i) => self.i(i),
      ExprF::Def(i) => self.di(i),
      ExprF::Ann(v, t) => self.e(v).s(" of ").e(t),
      ExprF::Uni => self.s("𝒰"),
      ExprF::Let(defs, e) => self.s("let").open().defs(defs).close().s("in ").e(e),

      ExprF::Pi(_, Vis::Explicit, i, t, e) => self.s("Π(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::Lam(_, Vis::Explicit, i, t, e) => self.s("λ(").i(i).s(": ").e(t).s(") ").e(e),
      ExprF::App(_, Vis::Explicit, e1, e2) => match e2.0 {
        ExprF::Hole
        | ExprF::Bind(_)
        | ExprF::Def(_)
        | ExprF::Uni
        | ExprF::Sigma(_, _)
        | ExprF::Obj(_, _) => self.e(e1).s(" ").e(e2),
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

  fn c<P, S>(&mut self, e: &CExpr<P, S>) -> &mut Self {
    match &e.0 {
      CExprF::Hole(i) => self.hi(i),
      CExprF::Bind(i) => self.i(i),
      CExprF::Def(i) => self.di(i),
      CExprF::Ann(v, t) => self.c(v).s(" of ").c(t),
      CExprF::Uni => self.s("𝒰"),
      CExprF::Let(defs, e) => self.s("let").open().defs_c(defs).close().s("in ").c(e),

      CExprF::Pi(_, Vis::Explicit, i, t, e) => self.s("Π(").i(i).s(": ").c(t).s(") ").c(e),
      CExprF::Lam(_, Vis::Explicit, i, t, e) => self.s("λ(").i(i).s(": ").c(t).s(") ").c(e),
      CExprF::App(_, Vis::Explicit, e1, e2) => match e2.0 {
        CExprF::Hole(_)
        | CExprF::Bind(_)
        | CExprF::Def(_)
        | CExprF::Uni
        | CExprF::Sigma0(_)
        | CExprF::Obj0(_)
        | CExprF::Sigma(_, _, _)
        | CExprF::Obj(_, _, _) => self.c(e1).s(" ").c(e2),
        _ => self.c(e1).s(" (").c(e2).s(")"),
      },

      CExprF::Pi(_, Vis::Implicit, i, t, e) => self.s("Π{").i(i).s(": ").c(t).s("} ").c(e),
      CExprF::Lam(_, Vis::Implicit, i, t, e) => self.s("λ{").i(i).s(": ").c(t).s("} ").c(e),
      CExprF::App(_, Vis::Implicit, e1, e2) => self.c(e1).s(" {").c(e2).s("}"),

      CExprF::Sigma0(_) => self.s("Σ{}"),
      CExprF::Obj0(_) => self.s("{}"),
      CExprF::Sigma(_, (n0, i0, ty0), es) => {
        self.s("Σ{").name(n0).s("~").i(i0).s(": ").c(ty0);
        for (name, bind, t) in es {
          self.s(", ").name(name).s("~").i(bind).s(": ").c(t);
        }
        self.s("}")
      }
      CExprF::Obj(_, (n0, e0), es) => {
        self.s("{").name(n0).s(" = ").c(e0);
        for (name, e) in es {
          self.s(", ").name(name).s(" = ").c(e);
        }
        self.s("}")
      }
      CExprF::Prop(_, e, name) => self.c(e).s(".").name(name),
    }
  }

  fn ks<P, S>(&mut self, ks: &Vec<ContF<P, S, Rc<Val<P, S>>>>) -> &mut Self {
    for k in ks {
      match k {
        ContF::App(_, Vis::Explicit, e) => match &e.0 {
          ValF::Neu(_, kks) if kks.is_empty() => {
            self.s(" ").v(e);
          }
          ValF::Uni
          | ValF::Sigma0(_)
          | ValF::Obj0(_)
          | ValF::Sigma(_, _, _, _)
          | ValF::Obj(_, _, _) => {
            self.s(" ").v(e);
          }
          _ => {
            self.s(" (").v(e).s(")");
          }
        },
        ContF::App(_, Vis::Implicit, e) => {
          self.s(" {").v(e).s("}");
        }
        ContF::Prop(_, name) => {
          self.s("#.").name(name);
        }
      }
    }
    self
  }

  fn g<P, S>(&mut self, g: &Env<P, S>) -> &mut Self {
    if !g.lookup.is_empty() {
      self.s("[");
      let mut gi = g.lookup.iter();
      let (i0, v0) = gi.next().unwrap();
      self.i(i0).s(" = ").v(v0);
      for (i, v) in gi {
        self.s(", ").i(i).s(" = ").v(v);
      }
      self.s("]");
    }
    if !g.define.is_empty() {
      self.s("[");
      let mut gi = g.define.iter();
      let (i0, v0) = gi.next().unwrap();
      self.di(i0).s(" = ").v(v0);
      for (i, v) in gi {
        self.s(", ").di(i).s(" = ").v(v);
      }
      self.s("]");
    }
    self
  }

  fn v<P, S>(&mut self, v: &Val<P, S>) -> &mut Self {
    match &v.0 {
      ValF::Hole(i) => self.hi(i),
      ValF::Neu(i, ks) => self.i(i).ks(ks),
      ValF::Lazy(i, ks) => self.di(i).ks(ks),
      ValF::Uni => self.s("𝒰"),

      ValF::Pi(_, Vis::Explicit, i, t, g, e) => self.s("Π(").i(i).s(": ").v(t).s(") ").g(g).c(e),
      ValF::Lam(_, Vis::Explicit, i, t, g, e) => self.s("λ(").i(i).s(": ").v(t).s(") ").g(g).c(e),

      ValF::Pi(_, Vis::Implicit, i, t, g, e) => self.s("Π{").i(i).s(": ").v(t).s("} ").g(g).c(e),
      ValF::Lam(_, Vis::Implicit, i, t, g, e) => self.s("λ{").i(i).s(": ").v(t).s("} ").g(g).c(e),

      ValF::Sigma0(_) => self.s("Σ{}"),
      ValF::Obj0(_) => self.s("{}"),
      ValF::Sigma(_, (n0, i0, ty0), g, es) => {
        self.s("Σ{").name(n0).s("~").i(i0).s(": ").v(ty0).g(g);
        for (name, bind, t) in es {
          self.s(", ").name(name).s("~").i(bind).s(": ").c(t);
        }
        self.s("}")
      }
      ValF::Obj(_, (n0, e0), es) => {
        self.s("{").name(n0).s(" = ").v(e0);
        for (name, e) in es {
          self.s(", ").name(name).s(" = ").v(e);
        }
        self.s("}")
      }
    }
  }
}

pub fn pretty<P, S>(program: &Program<P, S>) -> String {
  let mut p = Pretty::new(
    &program.def_symbols,
    &program.bind_symbols,
    &program.name_symbols,
  );
  p.defs(&program.defs);
  p.str.join("\n")
}

pub fn pretty_expr<P, S>(
  def_symbols: &HashMap<DefId, String>,
  bind_symbols: &HashMap<BindId, String>,
  name_symbols: &HashMap<NameId, String>,
  e: &CExpr<P, S>,
) -> String {
  let mut p = Pretty::new(def_symbols, bind_symbols, name_symbols);
  p.c(e).ln();
  p.str.join("\n")
}

pub fn pretty_val<P, S>(
  def_symbols: &HashMap<DefId, String>,
  bind_symbols: &HashMap<BindId, String>,
  name_symbols: &HashMap<NameId, String>,
  v: &Val<P, S>,
) -> String {
  let mut p = Pretty::new(def_symbols, bind_symbols, name_symbols);
  p.v(v).ln();
  p.str.join("\n")
}
