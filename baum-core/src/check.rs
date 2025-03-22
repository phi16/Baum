use crate::eval::*;
use crate::pretty::{pretty_expr, pretty_val};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

struct VarEnv<P, S> {
  lookup: HashMap<BindId, Type<P, S>>,
  define: HashMap<DefId, Option<Type<P, S>>>,
}

impl<P, S> VarEnv<P, S> {
  fn new() -> Self {
    VarEnv {
      lookup: HashMap::new(),
      define: HashMap::new(),
    }
  }

  fn add(&mut self, bind: BindId, ty: Type<P, S>) {
    self.lookup.insert(bind, ty);
  }

  fn def(&mut self, def: DefId, ty: Option<Type<P, S>>) {
    self.define.insert(def, ty);
  }
}

struct Solve<P, S> {
  holes: HashMap<HoleId, Type<P, S>>,
  constraints: HashMap<HoleId, Term<P, S>>,
}

impl<P, S> Solve<P, S> {
  fn new() -> Self {
    Solve {
      holes: HashMap::new(),
      constraints: HashMap::new(),
    }
  }

  fn add_hole(&mut self, hole: HoleId, ty: Type<P, S>) {
    self.holes.insert(hole, ty);
  }

  fn add_constraint(&mut self, hole: HoleId, v: Term<P, S>) {
    self.constraints.insert(hole, v);
  }
}

struct Checker<P, S> {
  def_symbols: HashMap<DefId, String>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  envs: Vec<VarEnv<P, S>>,
  solves: Vec<Solve<P, S>>,
  next_bind_id: u32,
  next_hole_id: u32,
  errors: Vec<String>,
}

impl<P, S> Checker<P, S>
where
  P: Tag,
  S: Tag,
{
  fn new(
    def_symbols: HashMap<DefId, String>,
    bind_symbols: HashMap<BindId, String>,
    name_symbols: HashMap<NameId, String>,
  ) -> Self {
    let next_bind_id = bind_symbols.keys().map(|i| i.0).max().map_or(0, |i| i + 1);
    Checker {
      def_symbols,
      bind_symbols,
      name_symbols,
      envs: Vec::new(),
      solves: Vec::new(),
      next_bind_id,
      next_hole_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, msg: &str) {
    self.errors.push(msg.to_string());
  }

  fn here(&mut self) -> &mut VarEnv<P, S> {
    self.envs.last_mut().unwrap()
  }

  fn solve(&mut self) -> &mut Solve<P, S> {
    self.solves.last_mut().unwrap()
  }

  fn fresh_id(&mut self, i: &BindId) -> BindId {
    let id = BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self.bind_symbols.insert(
      id,
      match self.bind_symbols.get(i) {
        Some(s) => s.clone(),
        None => format!("%%{}", id.0),
      },
    );
    id
  }

  fn fresh_id_new(&mut self) -> BindId {
    let id = BindId(self.next_bind_id);
    self.next_bind_id += 1;
    self.bind_symbols.insert(id, format!("%%{}", id.0));
    id
  }

  fn lookup_bind(&self, bind: BindId) -> Option<&Type<P, S>> {
    for env in self.envs.iter().rev() {
      if let Some(ty) = env.lookup.get(&bind) {
        return Some(ty);
      }
    }
    None
  }

  fn lookup_def(&self, def: DefId) -> Option<&Option<Type<P, S>>> {
    for env in self.envs.iter().rev() {
      if let Some(ty) = env.define.get(&def) {
        return Some(ty);
      }
    }
    None
  }

  fn lookup_constraint(&self, hole: HoleId) -> Option<&RV<P, S>> {
    for env in self.solves.iter().rev() {
      if let Some(v) = env.constraints.get(&hole) {
        return Some(v);
      }
    }
    None
  }

  fn fresh_hole(&mut self) -> HoleId {
    let id = HoleId(self.next_hole_id);
    self.next_hole_id += 1;
    id
  }

  fn ppe(&self, e: &Expr<P, S>) -> String {
    pretty_expr(&self.def_symbols, &self.bind_symbols, &self.name_symbols, e)
  }

  fn ppv(&self, v: &Val<P, S>) -> String {
    pretty_val(&self.def_symbols, &self.bind_symbols, &self.name_symbols, v)
  }

  pub fn app(&mut self, tag: P, vis: Vis, f: Term<P, S>, x: Term<P, S>) -> Term<P, S> {
    match &f.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lam(ftag, fvis, i, _, g, body) => {
        assert_eq!(tag, *ftag);
        assert_eq!(vis, *fvis);
        let mut g = g.clone();
        g.insert(*i, x);
        self.eval(&g, body)
      }
      _ => unreachable!(),
    }
  }

  pub fn prop(&mut self, tag: S, e: Term<P, S>, name: NameId) -> Term<P, S> {
    match &e.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Obj(otag, (n0, e0), props) => {
        assert_eq!(tag, *otag);
        if name == *n0 {
          return e0.clone();
        }
        for (n, e) in props {
          if name == *n {
            return e.clone();
          }
        }
        unreachable!()
      }
      _ => unreachable!(),
    }
  }

  fn eval(&mut self, g: &Env<P, S>, e: &CE<P, S>) -> Term<P, S> {
    // Note: e may contain unresolved bindings
    use CExprF::*;
    let v = match &e.0 {
      Hole(i) => ValF::Hole(i.clone()),
      Bind(i) if g.contains_key(i) => return g[i].clone(),
      Bind(i) => ValF::Neu(i.clone(), Vec::new()),
      Def(i) => ValF::Lazy(i.clone(), Vec::new()),
      Ann(t, _) => return self.eval(g, t),
      Uni => ValF::Uni,
      Let(defs, body) => {
        unimplemented!()
      }

      Pi(tag, vis, i, ty, body) => {
        let ty = self.eval(g, ty);
        ValF::Pi(tag.clone(), vis.clone(), *i, ty, g.clone(), body.clone())
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(g, ty);
        ValF::Lam(tag.clone(), vis.clone(), *i, ty, g.clone(), body.clone())
      }
      App(tag, vis, f, x) => {
        let f = self.eval(g, f);
        let x = self.eval(g, x);
        return self.app(tag.clone(), vis.clone(), f, x);
      }

      Sigma0(tag) => ValF::Sigma0(tag.clone()),
      Obj0(tag) => ValF::Obj0(tag.clone()),
      Sigma(tag, (n0, i0, ty0), props) => {
        let mut ps = Vec::new();
        let ty0 = self.eval(g, ty0);
        for (name, i, ty) in props {
          ps.push((*name, *i, ty.clone()));
        }
        ValF::Sigma(tag.clone(), (*n0, *i0, ty0), g.clone(), ps)
      }
      Obj(tag, (n0, e0), props) => {
        let mut ps = Vec::new();
        let e0 = self.eval(g, e0);
        for (name, e) in props {
          let e = self.eval(g, e);
          ps.push((*name, e));
        }
        ValF::Obj(tag.clone(), (*n0, e0), ps)
      }
      Prop(tag, e, name) => {
        let e = self.eval(g, e);
        return self.prop(tag.clone(), e, *name);
      }
    };
    Rc::new(Val(v))
  }

  fn eval0(&mut self, e: &CE<P, S>) -> Term<P, S> {
    self.eval(&Env::new(), e)
  }

  fn quote(&mut self, v: &Term<P, S>) -> CE<P, S> {
    use ValF::*;
    fn quote_ks<P: Tag, S: Tag>(
      this: &mut Checker<P, S>,
      ks: &Conts<P, S>,
      e: CE<P, S>,
    ) -> CE<P, S> {
      let mut e = e;
      for k in ks {
        match k {
          ContF::App(tag, vis, x) => {
            let x = this.quote(x);
            e = CE(CExprF::App(
              tag.clone(),
              vis.clone(),
              Rc::new(e),
              Rc::new(x),
            ));
          }
          ContF::Prop(tag, name) => {
            e = CE(CExprF::Prop(tag.clone(), Rc::new(e), *name));
          }
        }
      }
      e
    }
    match &v.0 {
      Hole(i) => CE(CExprF::Hole(i.clone())),
      Neu(i, ks) => {
        let mut e = CE(CExprF::Bind(i.clone()));
        quote_ks(self, ks, e)
      }
      Lazy(i, ks) => {
        let mut e = CE(CExprF::Def(i.clone()));
        quote_ks(self, ks, e)
      }
      Uni => CE(CExprF::Uni),
      Pi(tag, vis, i, ty, g, body) => {
        let ty = self.quote(ty);
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.insert(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let body = self.eval(&g, body);
        let body = self.quote(&body);
        CE(CExprF::Pi(
          tag.clone(),
          vis.clone(),
          fi,
          Rc::new(ty),
          Rc::new(body),
        ))
      }
      Lam(tag, vis, i, ty, g, body) => {
        let ty = self.quote(ty);
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.insert(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let body = self.eval(&g, body);
        let body = self.quote(&body);
        CE(CExprF::Lam(
          tag.clone(),
          vis.clone(),
          fi,
          Rc::new(ty),
          Rc::new(body),
        ))
      }
      Sigma0(tag) => CE(CExprF::Sigma0(tag.clone())),
      Obj0(tag) => CE(CExprF::Obj0(tag.clone())),
      Sigma(tag, (n0, i0, ty0), g, props) => {
        let fi0 = self.fresh_id(i0);
        let ty0 = self.quote(ty0);
        let mut g = g.clone();
        g.insert(*i0, Rc::new(Val(ValF::Neu(fi0, Vec::new()))));
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let body = self.eval(&g, ty);
          let ty = self.quote(&body);
          let fi = self.fresh_id(i);
          g.insert(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
          ps.push((*name, fi, Rc::new(ty)));
        }
        CE(CExprF::Sigma(tag.clone(), (*n0, fi0, Rc::new(ty0)), ps))
      }
      Obj(tag, (n0, e0), props) => {
        let e0 = self.quote(e0);
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.quote(e);
          ps.push((*name, Rc::new(e)));
        }
        CE(CExprF::Obj(tag.clone(), (*n0, Rc::new(e0)), ps))
      }
    }
  }

  fn unify(&mut self, v1: &Term<P, S>, v2: &Term<P, S>) -> Result<()> {
    let mut ev = Eval::new();
    let res = ev.unify(v1, v2);
    if let Ok(cs) = &res {
      for (hole, v) in cs.iter() {
        self.solve().add_constraint(*hole, v.clone());
      }
    }
    res.map(|_| ())
  }

  fn check(&mut self, e: Expr<P, S>, check_ty: &Type<P, S>) -> Result<CE<P, S>> {
    use ExprF::*;
    match e.0 {
      Hole => {
        let h = self.fresh_hole();
        self.solve().add_hole(h, check_ty.clone());
        Ok(CE(CExprF::Hole(h)))
      }
      Let(defs, body) => {
        unimplemented!()
      }
      Lam(tag, vis, i, ty, body) => match &check_ty.0 {
        ValF::Pi(ttag, tvis, ti, tty, tg, bty) => {
          if tag != *ttag || vis != *tvis {
            return Err("check: lam".to_string());
          }
          let c_ty = self.check_ty(*ty)?;
          let ty = self.eval0(&c_ty);
          self.unify(&ty, tty)?;
          self.envs.push(VarEnv::new());
          self.here().add(i, tty.clone());
          let mut tg = tg.clone();
          tg.insert(*ti, Rc::new(Val(ValF::Neu(i, Vec::new()))));
          let bty = self.eval(&tg, bty);
          let c_body = self.check(*body, &bty)?;
          self.envs.pop();
          Ok(CE(CExprF::Lam(tag, vis, i, Rc::new(c_ty), Rc::new(c_body))))
        }
        _ => Err("check: lam".to_string()),
      },
      Obj(tag, props) => match &check_ty.0 {
        ValF::Sigma0(ttag) => {
          if tag != *ttag {
            return Err("check: obj 0".to_string());
          }
          if !props.is_empty() {
            return Err("check: obj 0 (not length 0)".to_string());
          }
          return Ok(CE(CExprF::Obj0(tag)));
        }
        ValF::Sigma(ttag, (tn0, ti0, tty0), tg, tprops) => {
          if tag != *ttag {
            return Err("check: obj 1".to_string());
          }
          let len_match = props.len() == tprops.len() + 1;
          let mut pi = props.into_iter();
          let (n0, e0) = pi.next().unwrap();
          if n0 != *tn0 {
            eprintln!("{:?} != {:?}", n0, tn0);
            return Err("check: obj 2 head".to_string());
          }
          let c_e0 = self.check(*e0, &tty0)?;
          let e0 = self.eval0(&c_e0);
          self.envs.push(VarEnv::new());
          self.here().add(*ti0, tty0.clone());
          let mut tg = tg.clone();
          tg.insert(*ti0, e0);
          let mut ps = Vec::new();
          let mut c_ps = Vec::new();
          for ((n, e), (tn, ti, ty)) in pi.zip(tprops.iter()) {
            if n != *tn {
              eprintln!("{:?} != {:?}", n, tn);
              return Err("check: obj 2".to_string());
            }
            // ty may depend on previous props
            let ty = self.eval(&tg, ty);
            let c_e = self.check(*e, &ty)?;
            let e = self.eval0(&c_e);
            self.here().add(*ti, ty.clone());
            tg.insert(*ti, e.clone());
            ps.push((*ti, e));
            c_ps.push((n, Rc::new(c_e)));
          }
          self.envs.pop();
          if !len_match {
            // we defer this error to check the existing props as much as possible
            return Err("check: obj 3".to_string());
          }
          Ok(CE(CExprF::Obj(tag, (n0, Rc::new(c_e0)), c_ps)))
        }
        _ => return Err("check: obj 4".to_string()),
      },
      _ => {
        let (c_e, ty) = self.synth(e)?;
        self.unify(&ty, check_ty)?;
        Ok(c_e)
      }
    }
  }

  fn check_ty(&mut self, e: Expr<P, S>) -> Result<CE<P, S>> {
    self.check(e, &Rc::new(Val(ValF::Uni)))
  }

  fn synth(&mut self, e: Expr<P, S>) -> Result<(CE<P, S>, Type<P, S>)> {
    use ExprF::*;
    match e.0 {
      Hole => Err("synth: hole".to_string()),
      Bind(i) => match self.lookup_bind(i) {
        Some(ty) => Ok((CE(CExprF::Bind(i)), ty.clone())),
        None => Err("synth: bind".to_string()),
      },
      Def(i) => match self.lookup_def(i) {
        Some(Some(ty)) => Ok((CE(CExprF::Def(i)), ty.clone())),
        Some(None) => Err("synth: fail".to_string()),
        None => Err("synth: def".to_string()),
      },
      Ann(t, ty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        let c_t = self.check(*t, &ty)?;
        Ok((CE(CExprF::Ann(Rc::new(c_t), Rc::new(c_ty))), ty))
      }
      Uni => Ok(((CE(CExprF::Uni)), Rc::new(Val(ValF::Uni)))),

      Let(defs, body) => {
        unimplemented!()
      }

      Pi(tag, vis, i, ty, bty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        self.envs.push(VarEnv::new());
        self.here().add(i, ty.clone());
        let c_bty = self.check_ty(*bty)?;
        self.envs.pop();
        Ok((
          CE(CExprF::Pi(tag, vis, i, Rc::new(c_ty), Rc::new(c_bty))),
          Rc::new(Val(ValF::Uni)),
        ))
      }
      Lam(tag, vis, i, ty, body) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        self.envs.push(VarEnv::new());
        self.here().add(i, ty.clone());
        let (c_body, bty) = self.synth(*body)?;
        let bty = self.quote(&bty);
        self.envs.pop();
        Ok((
          (CE(CExprF::Lam(
            tag.clone(),
            vis.clone(),
            i,
            Rc::new(c_ty),
            Rc::new(c_body),
          ))),
          Rc::new(Val(ValF::Pi(tag, vis, i, ty, HashMap::new(), Rc::new(bty)))),
        ))
      }
      App(tag, vis, f, x) => {
        let (c_f, fty) = self.synth(*f)?;
        match &fty.0 {
          ValF::Pi(ftag, fvis, i, ty, fg, bty) => {
            if tag != *ftag {
              return Err("synth: app".to_string());
            }
            if vis != *fvis {
              return Err("synth: app (may implicit app)".to_string());
            }
            let c_x = self.check(*x, &ty)?;
            let x = self.eval0(&c_x);
            let mut fg = fg.clone();
            fg.insert(*i, x);
            let bty = self.eval(&fg, bty);
            Ok((CE(CExprF::App(tag, vis, Rc::new(c_f), Rc::new(c_x))), bty))
          }
          _ => Err(format!("synth: app / {:?}", fty)),
        }
      }

      Sigma(tag, props) => {
        if props.is_empty() {
          return Ok((CE(CExprF::Sigma0(tag)), Rc::new(Val(ValF::Uni))));
        }
        let mut pi = props.into_iter();
        let (n0, i0, ty0) = pi.next().unwrap();
        let c_ty0 = self.check_ty(*ty0)?;
        let ty0 = self.eval0(&c_ty0);
        self.envs.push(VarEnv::new());
        self.here().add(i0, ty0);
        let mut c_props = Vec::new();
        for (name, i, ty) in pi {
          let c_ty = self.check_ty(*ty)?;
          let ty = self.eval0(&c_ty);
          self.here().add(i, ty);
          c_props.push((name, i, Rc::new(c_ty)));
        }
        self.envs.pop();
        Ok((
          CE(CExprF::Sigma(tag, (n0, i0, Rc::new(c_ty0)), c_props)),
          Rc::new(Val(ValF::Uni)),
        ))
      }
      Obj(tag, props) => {
        if props.is_empty() {
          return Ok((
            CE(CExprF::Obj0(tag.clone())),
            Rc::new(Val(ValF::Sigma0(tag))),
          ));
        }
        let mut pi = props.into_iter();
        let (n0, e0) = pi.next().unwrap();
        let i0 = self.fresh_id_new();
        let (c_e0, ty0) = self.synth(*e0)?;
        let mut tys = Vec::new();
        let mut c_props = Vec::new();
        for (name, e) in pi {
          let (c_e, ty) = self.synth(*e)?;
          let ty = self.quote(&ty);
          let i = self.fresh_id_new();
          tys.push((name.clone(), i, Rc::new(ty)));
          c_props.push((name, Rc::new(c_e)));
        }
        Ok((
          CE(CExprF::Obj(tag.clone(), (n0, Rc::new(c_e0)), c_props)),
          Rc::new(Val(ValF::Sigma(tag, (n0, i0, ty0), HashMap::new(), tys))),
        ))
      }
      Prop(tag, e, name) => {
        let (c_e, ty) = self.synth(*e)?;
        match &ty.0 {
          ValF::Sigma(etag, (n0, i0, ty0), eg, props) => {
            if tag != *etag {
              return Err("synth: prop".to_string());
            }
            if name == *n0 {
              return Ok((CE(CExprF::Prop(tag, Rc::new(c_e), name)), ty0.clone()));
            }
            for (index, (n, _, ty)) in props.iter().enumerate() {
              if name == *n {
                let e = self.eval0(&c_e);
                let mut ty = ty.clone();
                let mut eg = eg.clone();
                // ty may depend on previous props
                let p0 = self.prop(tag.clone(), e.clone(), *n0); // e.n0
                eg.insert(*i0, p0);
                for (n, i, _) in props.iter().take(index) {
                  let p = self.prop(tag.clone(), e.clone(), *n); // e.n
                  eg.insert(*i, p);
                }
                let ty = self.eval(&eg, &ty);
                return Ok((CE(CExprF::Prop(tag, Rc::new(c_e), name)), ty));
              }
            }
            Err("synth: prop".to_string())
          }
          _ => Err("synth: prop".to_string()),
        }
      }
    }
  }

  fn defs(&mut self, defs: Vec<(DefId, Box<Expr<P, S>>)>) -> Vec<(DefId, Option<Term<P, S>>)> {
    self.envs.push(VarEnv::new());
    self.solves.push(Solve::new());
    let mut def_terms = Vec::new();
    for (id, expr) in defs {
      eprintln!("- Synth[ {} ]", self.def_symbols[&id]);
      let depth1 = (self.envs.len(), self.solves.len());
      let res = self.synth(*expr);
      let depth2 = (self.envs.len(), self.solves.len());
      match &res {
        Ok(_) => assert_eq!(depth1, depth2),
        Err(_) => {
          assert!(depth1.0 <= depth2.0);
          assert!(depth1.1 <= depth2.1);
          // rollback
          self.envs.truncate(depth1.0);
          self.solves.truncate(depth1.1);
        }
      }
      match &res {
        Ok((_, ty)) => eprintln!("[o] {}: {}", self.def_symbols[&id], self.ppv(ty)),
        Err(e) => eprintln!("[x] {}: {}", self.def_symbols[&id], e),
      }
      let (c_expr, tm, ty) = match res {
        Ok((c_expr, ty)) => {
          let tm = self.eval0(&c_expr);
          (Some(c_expr), Some(tm), Some(ty))
        }
        Err(e) => {
          self.add_error(&format!("{}: {}", self.def_symbols[&id], e));
          let h = self.fresh_hole();
          (None, None, None)
        }
      };
      self.here().def(id, ty);
      def_terms.push((id, tm));
    }
    self.solves.pop();
    self.envs.pop();
    def_terms
  }
}

pub fn check<P, S>(p: Program<P, S>) -> std::result::Result<(), Vec<String>>
where
  P: Tag,
  S: Tag,
{
  let mut c = Checker::new(p.def_symbols, p.bind_symbols, p.name_symbols);
  c.defs(p.defs);
  if c.errors.is_empty() {
    Ok(())
  } else {
    Err(c.errors)
  }
}
