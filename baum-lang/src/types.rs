type Mod<I> = Vec<I>;

#[derive(Debug, Clone)]
pub enum Literal<S> {
  Prim(S),
  Num(S),
  Chr(S),
  Str(S),
  Hole,
}

#[derive(Debug, Clone)]
pub enum ArgF<I, E> {
  Id(Vec<I>),
  Ty(E),
  IdTy(Vec<I>, E),
}

#[derive(Debug, Clone, Copy)]
pub enum Vis {
  Explicit,
  Implicit,
}

type ArgVis<I, E> = (Vis, ArgF<I, E>);

#[derive(Debug, Clone)]
pub struct DefF<I, E> {
  pub name: I,
  pub args: Vec<ArgVis<I, E>>,
  pub ty: Option<E>,
  pub body: E,
}

#[derive(Debug, Clone)]
pub enum ContextF<I, E> {
  Ref(I, E),
}

pub type Precedence = Vec<u16>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fixity {
  InfixN,
  InfixL,
  InfixR,
  Prefix,
  Postfix,
  Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxElement {
  Str(String),
  Id(String),
  Expr(String),
  Special,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Syntax {
  pub prec: Precedence,
  pub fixity: Fixity,
  pub elems: Vec<SyntaxElement>,
}

impl Syntax {
  pub fn new(prec: Precedence, fixity: Fixity, elems: Vec<SyntaxElement>) -> Self {
    Syntax {
      prec,
      fixity,
      elems,
    }
  }
}

#[derive(Debug, Clone)]
pub enum ExprF<S, I, Ds, E> {
  Lit(Literal<S>),
  Var(I),

  PiE(Vec<ArgF<I, E>>, E),
  LamE(Vec<ArgF<I, E>>, E),
  AppE(E, E),

  PiI(Vec<ArgF<I, E>>, E),
  LamI(Vec<ArgF<I, E>>, E),
  AppI(E, E),

  TupleTy(Vec<E>),
  TupleCon(Vec<E>),
  Proj(E, u32),

  Sigma(Vec<ArgF<I, E>>),
  Obj(Vec<DefF<I, E>>),
  Prop(E, I),

  Subst(Vec<DefF<I, E>>, E),
  Extern(Mod<I>, I),
  Mu(I, E, Vec<(I, E)>),
  Nu(I, E, Vec<(I, E)>),

  Let(Ds, E),
  Where(E, Ds),
  Syntax(Syntax, E),
}

#[derive(Debug, Clone)]
pub enum DeclF<I, D, Ds, E> {
  Context(ContextF<I, E>, D),
  Module(Option<I>, Ds),
  Def(DefF<I, E>),
  Syntax(Syntax),
  Open(Mod<I>),
}

#[derive(Debug, Clone)]
pub struct Id<'a>(&'a str);

impl<'a> Id<'a> {
  pub fn new(s: &'a str) -> Self {
    Id(s)
  }
  pub fn as_str(&self) -> &'a str {
    self.0
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenPos {
  Pos(u32, u16),
  EoL(u32),
  EoF,
}

impl TokenPos {
  pub fn to_string(&self) -> String {
    match self {
      TokenPos::Pos(ln, col) => format!("L{} C{}", ln + 1, col + 1),
      TokenPos::EoL(ln) => format!("End of L{}", ln + 1),
      TokenPos::EoF => "End of file".to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct Expr<'a>(
  pub ExprF<&'a str, Id<'a>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>>,
  pub TokenPos,
);

#[derive(Debug, Clone)]
pub struct Decl<'a>(
  pub DeclF<Id<'a>, Box<Decl<'a>>, Box<Vec<Decl<'a>>>, Box<Expr<'a>>>,
  pub TokenPos,
);

pub type Context<'a> = ContextF<Id<'a>, Box<Expr<'a>>>;
pub type Def<'a> = DefF<Id<'a>, Box<Expr<'a>>>;
pub type Arg<'a> = ArgF<Id<'a>, Box<Expr<'a>>>;

struct Pretty {
  indent: u32,
  str: Vec<String>,
  line: Vec<String>,
}

impl Pretty {
  pub fn new() -> Self {
    Pretty {
      indent: 0,
      str: Vec::new(),
      line: Vec::new(),
    }
  }

  pub fn i(&mut self, i: &Id) -> &mut Self {
    self.line.push(i.as_str().to_string());
    self
  }

  pub fn is(&mut self, i: &Vec<Id>, sep: &str) -> &mut Self {
    let mid: Vec<&str> = i.iter().map(|i| i.as_str()).collect();
    self.line.push(mid.join(sep));
    self
  }

  pub fn s(&mut self, s: &str) -> &mut Self {
    self.line.push(s.to_string());
    self
  }

  pub fn ln(&mut self) -> &mut Self {
    let pad = " ".repeat(self.indent as usize);
    let str = self.line.join("");
    self.str.push(pad + &str);
    self.line.clear();
    self
  }

  pub fn open(&mut self) -> &mut Self {
    self.ln();
    self.indent += 2;
    self
  }

  pub fn close(&mut self) -> &mut Self {
    self.indent -= 2;
    self
  }

  pub fn c(&mut self, c: &Context) -> &mut Self {
    match c {
      ContextF::Ref(i, e) => self.i(i).s(": ").e(e),
    }
  }

  pub fn def(&mut self, def: &Def) -> &mut Self {
    self.i(&def.name);
    for (vis, arg) in &def.args {
      self.s(" ");
      match arg {
        ArgF::Id(ids) => match vis {
          Vis::Explicit => self.is(&ids, " "),
          Vis::Implicit => self.s("{").is(&ids, " ").s("}"),
        },
        ArgF::Ty(e) => unreachable!(),
        ArgF::IdTy(ids, e) => match vis {
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

  pub fn d(&mut self, d: &Decl) -> &mut Self {
    match &d.0 {
      DeclF::Context(c, d) => self.s("[").c(c).s("] ").d(d),
      DeclF::Module(None, ds) => self.s("{").open().ds(ds).close().s("}").ln(),
      DeclF::Module(Some(i), ds) => self.i(i).s(" {").open().ds(ds).close().s("}").ln(),
      DeclF::Def(def) => self.def(def).ln(),
      DeclF::Syntax(_) => self.s("syntax ").ln(),
      DeclF::Open(ms) => self.s("open ").is(ms, ".").ln(),
    }
  }

  pub fn ds(&mut self, ds: &Vec<Decl>) -> &mut Self {
    for d in ds {
      self.d(d);
    }
    self
  }

  pub fn e(&mut self, e: &Expr) -> &mut Self {
    match &e.0 {
      ExprF::Lit(l) => match l {
        Literal::Prim(n) => self.s("prim ").s(n),
        Literal::Num(n) => self.s(n),
        Literal::Chr(c) => self.s("'").s(c).s("'"),
        Literal::Str(s) => self.s("\"").s(s).s("\""),
        Literal::Hole => self.s("_"),
      },
      _ => unimplemented!(),
    }
  }
}

pub fn pretty(ds: &Vec<Decl>) -> String {
  let mut p = Pretty::new();
  p.ds(ds);
  p.str.join("\n")
}
