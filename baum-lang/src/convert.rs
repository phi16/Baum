use crate::types::parse::*;
use baum_core::types as core;
use std::collections::HashMap;
use std::rc::Rc;

impl From<&Vis> for core::Vis {
  fn from(v: &Vis) -> Self {
    match v {
      Vis::Explicit => core::Vis::Explicit,
      Vis::Implicit => core::Vis::Implicit,
    }
  }
}

struct Builder<'a> {
  id_map: HashMap<Id<'a>, core::Id>,
  next_i: u32,
  p: core::Program,
}

impl<'a> Builder<'a> {
  fn new() -> Self {
    Self {
      id_map: HashMap::new(),
      next_i: 0,
      p: core::Program {
        decls: Vec::new(),
        symbols: HashMap::new(),
      },
    }
  }

  fn id(&mut self, id: &Id<'a>) -> core::Id {
    match self.id_map.get(id) {
      Some(i) => *i,
      None => {
        let i = core::Id(self.next_i);
        self.next_i += 1;
        self.id_map.insert(id.clone(), i);
        self.p.symbols.insert(i, id.as_str().to_string());
        i
      }
    }
  }

  fn syntax_elem(&mut self, e: &SyntaxElem<'a>) -> CoreElem<'a> {
    match e {
      SyntaxElem::Token(s) => CoreElem::Token(s),
      SyntaxElem::Ident(id) => CoreElem::Ident(self.id(id)),
      SyntaxElem::Nat(s) => CoreElem::Nat(s),
      SyntaxElem::Rat(s) => CoreElem::Rat(s),
      SyntaxElem::Chr(s) => CoreElem::Chr(s),
      SyntaxElem::Str(s) => CoreElem::Str(s),
      SyntaxElem::Expr(e) => CoreElem::Expr(self.e(e)),
    }
  }

  fn e(&mut self, e: &Expr<'a>) -> core::Expr {
    match &e.0 {
      ExprF::Hole => core::Expr::Hole,
      ExprF::Var(i) => core::Expr::Var(self.id(i)),
      ExprF::Mod(_) => {
        // not allowed!
        // TODO: return error
        panic!()
      }
      ExprF::Ext(ids, i) => core::Expr::Ext(ids.iter().map(|i| self.id(i)).collect(), self.id(&i)),
      ExprF::Let(ds, e) => {
        let ds = self.ds(ds);
        let e = Rc::new(self.e(e));
        core::Expr::Let(ds, e)
      }
      ExprF::Syntax(x, mod_name, es) => {
        let es = es.iter().map(|e| self.syntax_elem(e)).collect();
        (x.t)(es, mod_name, self)
      }
    }
  }

  fn mr(&mut self, mr: &ModRef<'a>) -> core::ModRef {
    match mr {
      ModRef::Import(s) => core::ModRef::Import(s.to_string()),
      ModRef::App(name, params) => core::ModRef::App(
        name.iter().map(|i| self.id(i)).collect(),
        params
          .iter()
          .map(|(v, e)| (core::Vis::from(v), Rc::new(self.e(e))))
          .collect(),
      ),
    }
  }

  fn def(&mut self, def: &Def<'a>) -> (core::Id, core::Expr) {
    let name = self.id(&def.name);
    let e = self.e(&*def.body);
    let e = if let Some(ty) = &def.ty {
      core::Expr::Ann(Rc::new(e), Rc::new(self.e(&*ty)))
    } else {
      e
    };
    let mut e = e;
    for (v, is, ty) in &def.args {
      let ty = ty
        .as_ref()
        .map(|ty| self.e(&ty))
        .unwrap_or(core::Expr::Hole);
      let ty = Rc::new(ty);
      for i in is {
        match v {
          Vis::Explicit => e = core::Expr::PiI(self.id(i), ty.clone(), Rc::new(e)),
          Vis::Implicit => e = core::Expr::PiE(self.id(i), ty.clone(), Rc::new(e)),
        };
      }
    }
    (name, e)
  }

  fn d(&mut self, d: &Decl<'a>) -> Vec<core::Decl> {
    match &d.0 {
      DeclF::Local(ds) => self.ds(ds),
      DeclF::ModDef(md, def) => {
        let mut params = Vec::new();
        for p in &md.params {
          let (v, is, e) = p;
          let v = core::Vis::from(v);
          let e = e.as_ref().map(|e| self.e(&e)).unwrap_or(core::Expr::Hole);
          let e = Rc::new(e);
          for i in is {
            params.push((v.clone(), self.id(i), e.clone()));
          }
        }
        let decl = core::ModDecl {
          name: self.id(&md.name),
          params,
        };
        let def = match &def.0 {
          ModDefF::Decls(ds) => core::ModDef::Decls(self.ds(ds)),
          ModDefF::Ref(mr) => core::ModDef::Ref(self.mr(mr)),
        };
        vec![core::Decl::ModDef(decl, def)]
      }
      DeclF::Open(mr) => vec![core::Decl::Open(self.mr(mr))],
      DeclF::Def(def) => {
        let (name, e) = self.def(def);
        vec![core::Decl::Def(name, Rc::new(e))]
      }
      DeclF::Syntax(_, _, _) => Vec::new(),
    }
  }

  fn ds(&mut self, ds: &Vec<Decl<'a>>) -> Vec<core::Decl> {
    let mut rs = Vec::new();
    for d in ds {
      rs.extend(self.d(d));
    }
    rs
  }
}

impl<'a> SyntaxHandler<'a> for Builder<'a> {
  fn convert_i(&mut self, id: &Id<'a>) -> core::Id {
    self.id(id)
  }

  fn convert_e(&mut self, e: &Expr<'a>) -> core::Expr {
    self.e(e)
  }

  fn convert_se(&mut self, e: &SyntaxElem<'a>) -> CoreElem<'a> {
    self.syntax_elem(e)
  }
}

pub fn convert(ds: Vec<Decl>) -> core::Program {
  let mut b = Builder::new();
  b.p.decls = b.ds(&ds);
  b.p
}
