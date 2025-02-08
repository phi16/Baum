use crate::types::parse::*;
use baum_core::types as core;

type I = core::I;

impl From<&Vis> for core::Vis {
  fn from(v: &Vis) -> Self {
    match v {
      Vis::Explicit => core::Vis::Explicit,
      Vis::Implicit => core::Vis::Implicit,
    }
  }
}

struct Builder<'a> {
  id_map: HashMap<Id<'a>, I>,
  next_i: I,
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

  fn id(&mut self, id: &Id<'a>) -> I {
    match self.id_map.get(id) {
      Some(i) => *i,
      None => {
        let i = self.next_i;
        self.next_i += 1;
        self.id_map.insert(id.clone(), i);
        self.p.symbols.insert(i, id.as_str().to_string());
        i
      }
    }
  }

  fn fresh(&mut self) -> I {
    let i = self.next_i;
    self.next_i += 1;
    i
  }

  fn e(&mut self, e: &Expr<'a>) -> core::Expr {
    core::Expr::Hole
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

  fn def(&mut self, def: &Def<'a>, wh: &Where<'a>) -> core::Decl {
    let name = self.id(&def.name);
    let e = self.e(&*def.body);
    let e = if let Some(ty) = &def.ty {
      core::Expr::Ann(Rc::new(e), Rc::new(self.e(&*ty)))
    } else {
      e
    };
    let e = if !wh.defs.is_empty() {
      let whs = wh
        .defs
        .iter()
        .map(|def| self.def(def, &Where { defs: Vec::new() }))
        .collect();
      core::Expr::Let(whs, Rc::new(e))
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
    core::Decl::Def(name, Rc::new(e))
  }

  fn d(&mut self, d: &Decl<'a>) -> core::Decl {
    match &d.0 {
      DeclF::Local(ds) => core::Decl::Local(self.ds(ds)),
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
        core::Decl::ModDef(decl, def)
      }
      DeclF::Open(mr) => core::Decl::Open(self.mr(mr)),
      DeclF::Def(def, wh) => self.def(def, wh),
      DeclF::Syntax(_, ds, e, wh) => {
        // TODO!
        core::Decl::Local(vec![])
      }
    }
  }

  fn ds(&mut self, ds: &Vec<Decl<'a>>) -> Vec<core::Decl> {
    let mut rs = Vec::new();
    for d in ds {
      rs.push(self.d(d));
    }
    rs
  }
}

pub fn convert(ds: Vec<Decl>) -> core::Program {
  let mut b = Builder::new();
  b.p.decls = b.ds(&ds);
  b.p
}
