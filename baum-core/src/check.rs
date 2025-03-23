use crate::pretty::{pretty_expr, pretty_val};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

struct VarEnv<P, S> {
  lookup: HashMap<BindId, Type<P, S>>,
}

impl<P, S> VarEnv<P, S> {
  fn new() -> Self {
    VarEnv {
      lookup: HashMap::new(),
    }
  }

  fn add(&mut self, bind: BindId, ty: Type<P, S>) {
    self.lookup.insert(bind, ty);
  }
}

struct DefEnv<P, S> {
  define: HashMap<DefId, (Term<P, S>, Type<P, S>)>,
}

impl<P, S> DefEnv<P, S> {
  fn new() -> Self {
    DefEnv {
      define: HashMap::new(),
    }
  }

  fn add(&mut self, def: DefId, tm: Term<P, S>, ty: Type<P, S>) {
    self.define.insert(def, (tm, ty));
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

pub enum Subst<'a, T> {
  Unchanged(&'a T),
  Changed(T),
}

impl<'a, T: Clone> Subst<'a, T> {
  fn is_changed(&self) -> bool {
    match self {
      Subst::Unchanged(_) => false,
      Subst::Changed(_) => true,
    }
  }
  pub fn into(self) -> T {
    match self {
      Subst::Unchanged(t) => t.clone(),
      Subst::Changed(t) => t,
    }
  }
}

fn subst_hole_e<'a, P: Tag, S: Tag>(
  m: &HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
  e: &'a RE<P, S>,
) -> Subst<'a, RE<P, S>> {
  let unchanged = Subst::Unchanged(e);
  match &e.0 {
    CExprF::Hole(i) => match m.get(&i) {
      Some((e, _)) => Subst::Changed(subst_hole_e(m, e).into()),
      None => unchanged,
    },
    CExprF::Bind(_) => unchanged,
    CExprF::Def(_) => unchanged,
    CExprF::Ann(e, ty) => {
      let e = subst_hole_e(m, e);
      let ty = subst_hole_e(m, ty);
      if e.is_changed() || ty.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::Ann(e.into(), ty.into()))))
      } else {
        unchanged
      }
    }
    CExprF::Uni => unchanged,
    CExprF::Let(defs, body) => {
      let mut changed = false;
      let mut rdefs = Vec::new();
      for (def, e) in defs {
        let e = subst_hole_e(m, e);
        if e.is_changed() {
          changed = true;
        }
        rdefs.push((*def, e.into()));
      }
      let body = subst_hole_e(m, body);
      if changed || body.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::Let(rdefs, body.into()))))
      } else {
        unchanged
      }
    }
    CExprF::Pi(tag, vis, i, ty, bty) => {
      let ty = subst_hole_e(m, ty);
      let bty = subst_hole_e(m, bty);
      if ty.is_changed() || bty.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::Pi(
          tag.clone(),
          vis.clone(),
          *i,
          ty.into(),
          bty.into(),
        ))))
      } else {
        unchanged
      }
    }
    CExprF::Lam(tag, vis, i, ty, body) => {
      let ty = subst_hole_e(m, ty);
      let body = subst_hole_e(m, body);
      if ty.is_changed() || body.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::Lam(
          tag.clone(),
          vis.clone(),
          *i,
          ty.into(),
          body.into(),
        ))))
      } else {
        unchanged
      }
    }
    CExprF::App(tag, vis, f, x) => {
      let f = subst_hole_e(m, f);
      let x = subst_hole_e(m, x);
      if f.is_changed() || x.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::App(
          tag.clone(),
          vis.clone(),
          f.into(),
          x.into(),
        ))))
      } else {
        unchanged
      }
    }
    CExprF::Sigma0(_) => unchanged,
    CExprF::Obj0(_) => unchanged,
    CExprF::Sigma(tag, (n0, i0, ty0), props) => {
      let ty0 = subst_hole_e(m, ty0);
      let mut changed = false;
      let mut rprops = Vec::new();
      for (n, i, ty) in props {
        let ty = subst_hole_e(m, ty);
        if ty.is_changed() {
          changed = true;
        }
        rprops.push((*n, *i, ty.into()));
      }
      if ty0.is_changed() || changed {
        Subst::Changed(Rc::new(CExpr(CExprF::Sigma(
          tag.clone(),
          (*n0, *i0, ty0.into()),
          rprops,
        ))))
      } else {
        unchanged
      }
    }
    CExprF::Obj(tag, (n0, e0), props) => {
      let e0 = subst_hole_e(m, e0);
      let mut changed = false;
      let mut rprops = Vec::new();
      for (n, e) in props {
        let e = subst_hole_e(m, e);
        if e.is_changed() {
          changed = true;
        }
        rprops.push((*n, e.into()));
      }
      if e0.is_changed() || changed {
        Subst::Changed(Rc::new(CExpr(CExprF::Obj(
          tag.clone(),
          (*n0, e0.into()),
          rprops,
        ))))
      } else {
        unchanged
      }
    }
    CExprF::Prop(tag, e, name) => {
      let e = subst_hole_e(m, e);
      if e.is_changed() {
        Subst::Changed(Rc::new(CExpr(CExprF::Prop(tag.clone(), e.into(), *name))))
      } else {
        unchanged
      }
    }
  }
}

fn subst_hole_v<'a, P: Tag, S: Tag>(
  m: &HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
  v: &'a RV<P, S>,
) -> Subst<'a, RV<P, S>> {
  let unchanged = Subst::Unchanged(v);
  match &v.0 {
    ValF::Hole(i) => match m.get(&i) {
      Some((_, v)) => Subst::Changed(subst_hole_v(m, v).into()),
      None => unchanged,
    },
    ValF::Neu(i, ks) => {
      let mut changed = false;
      let mut rks = Vec::new();
      for k in ks {
        match k {
          ContF::App(tag, vis, x) => {
            let x = subst_hole_v(m, x);
            if x.is_changed() {
              changed = true;
            }
            rks.push(ContF::App(tag.clone(), vis.clone(), x.into()));
          }
          ContF::Prop(tag, name) => {
            rks.push(ContF::Prop(tag.clone(), *name));
          }
        }
      }
      if changed {
        Subst::Changed(Rc::new(Val(ValF::Neu(*i, rks))))
      } else {
        unchanged
      }
    }
    ValF::Lazy(i, ks) => {
      let mut changed = false;
      let mut rks = Vec::new();
      for k in ks {
        match k {
          ContF::App(tag, vis, x) => {
            let x = subst_hole_v(m, x);
            if x.is_changed() {
              changed = true;
            }
            rks.push(ContF::App(tag.clone(), vis.clone(), x.into()));
          }
          ContF::Prop(tag, name) => {
            rks.push(ContF::Prop(tag.clone(), *name));
          }
        }
      }
      if changed {
        Subst::Changed(Rc::new(Val(ValF::Lazy(*i, rks))))
      } else {
        unchanged
      }
    }
    ValF::Uni => unchanged,
    ValF::Pi(tag, vis, i, ty, g, bty) => {
      let ty = subst_hole_v(m, ty);
      let bty = subst_hole_e(m, bty);
      if ty.is_changed() || bty.is_changed() {
        Subst::Changed(Rc::new(Val(ValF::Pi(
          tag.clone(),
          vis.clone(),
          *i,
          ty.into(),
          g.clone(),
          bty.into(),
        ))))
      } else {
        unchanged
      }
    }
    ValF::Lam(tag, vis, i, ty, g, body) => {
      let ty = subst_hole_v(m, ty);
      let body = subst_hole_e(m, body);
      if ty.is_changed() || body.is_changed() {
        Subst::Changed(Rc::new(Val(ValF::Lam(
          tag.clone(),
          vis.clone(),
          *i,
          ty.into(),
          g.clone(),
          body.into(),
        ))))
      } else {
        unchanged
      }
    }
    ValF::Sigma0(tag) => unchanged,
    ValF::Obj0(tag) => unchanged,
    ValF::Sigma(tag, (n0, i0, ty0), g, props) => {
      let ty0 = subst_hole_v(m, ty0);
      let mut changed = false;
      let mut rprops = Vec::new();
      for (n, i, ty) in props {
        let ty = subst_hole_e(m, ty);
        if ty.is_changed() {
          changed = true;
        }
        rprops.push((*n, *i, ty.into()));
      }
      if ty0.is_changed() || changed {
        Subst::Changed(Rc::new(Val(ValF::Sigma(
          tag.clone(),
          (*n0, *i0, ty0.into()),
          g.clone(),
          rprops,
        ))))
      } else {
        unchanged
      }
    }
    ValF::Obj(tag, (n0, e0), props) => {
      let e0 = subst_hole_v(m, e0);
      let mut changed = false;
      let mut rprops = Vec::new();
      for (n, e) in props {
        let e = subst_hole_v(m, e);
        if e.is_changed() {
          changed = true;
        }
        rprops.push((*n, e.into()));
      }
      if e0.is_changed() || changed {
        Subst::Changed(Rc::new(Val(ValF::Obj(
          tag.clone(),
          (*n0, e0.into()),
          rprops,
        ))))
      } else {
        unchanged
      }
    }
  }
}

fn contains_hole_e<P, S>(i: &HoleId, e: &RE<P, S>) -> bool {
  match &e.0 {
    CExprF::Hole(j) => i == j,
    CExprF::Bind(_) => false,
    CExprF::Def(_) => false,
    CExprF::Ann(e, _) => contains_hole_e(i, e),
    CExprF::Uni => false,
    CExprF::Let(defs, body) => {
      defs.iter().any(|(_, e)| contains_hole_e(i, e)) || contains_hole_e(i, body)
    }
    CExprF::Pi(_, _, _, ty, body) => contains_hole_e(i, ty) || contains_hole_e(i, body),
    CExprF::Lam(_, _, _, ty, body) => contains_hole_e(i, ty) || contains_hole_e(i, body),
    CExprF::App(_, _, f, x) => contains_hole_e(i, f) || contains_hole_e(i, x),
    CExprF::Sigma0(_) => false,
    CExprF::Obj0(_) => false,
    CExprF::Sigma(_, (_, _, ty), props) => {
      contains_hole_e(i, ty) || props.iter().any(|(_, _, ty)| contains_hole_e(i, ty))
    }
    CExprF::Obj(_, (_, e), props) => {
      contains_hole_e(i, e) || props.iter().any(|(_, e)| contains_hole_e(i, e))
    }
    CExprF::Prop(_, e, _) => contains_hole_e(i, e),
  }
}

fn contains_hole_v<P, S>(i: &HoleId, v: &Term<P, S>) -> bool {
  match &v.0 {
    ValF::Hole(j) => i == j,
    ValF::Neu(_, ks) => ks.iter().any(|k| match k {
      ContF::App(_, _, x) => contains_hole_v(i, x),
      ContF::Prop(_, _) => false,
    }),
    ValF::Lazy(_, ks) => ks.iter().any(|k| match k {
      ContF::App(_, _, x) => contains_hole_v(i, x),
      ContF::Prop(_, _) => false,
    }),
    ValF::Uni => false,
    ValF::Pi(_, _, _, ty, _, bty) => contains_hole_v(i, ty) || contains_hole_e(i, bty),
    ValF::Lam(_, _, _, ty, _, body) => contains_hole_v(i, ty) || contains_hole_e(i, body),
    ValF::Sigma0(_) => false,
    ValF::Obj0(_) => false,
    ValF::Sigma(_, (_, _, ty), _, props) => {
      contains_hole_v(i, ty) || props.iter().any(|(_, _, ty)| contains_hole_e(i, ty))
    }
    ValF::Obj(_, (_, e), props) => {
      contains_hole_v(i, e) || props.iter().any(|(_, e)| contains_hole_v(i, e))
    }
  }
}

struct Checker<P, S> {
  def_symbols: HashMap<DefId, String>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  varenvs: Vec<VarEnv<P, S>>,
  defenvs: Vec<DefEnv<P, S>>,
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
      varenvs: Vec::new(),
      defenvs: Vec::new(),
      solves: Vec::new(),
      next_bind_id,
      next_hole_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, msg: &str) {
    self.errors.push(msg.to_string());
  }

  fn varenv(&mut self) -> &mut VarEnv<P, S> {
    self.varenvs.last_mut().unwrap()
  }

  fn defenv(&mut self) -> &mut DefEnv<P, S> {
    self.defenvs.last_mut().unwrap()
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
    for env in self.varenvs.iter().rev() {
      if let Some(ty) = env.lookup.get(&bind) {
        return Some(ty);
      }
    }
    None
  }

  fn lookup_def(&self, def: DefId) -> Option<&(Term<P, S>, Type<P, S>)> {
    for env in self.defenvs.iter().rev() {
      if let Some(tmty) = env.define.get(&def) {
        return Some(tmty);
      }
    }
    None
  }

  fn lookup_constraint(&self, hole: HoleId) -> Option<&RV<P, S>> {
    for solve in self.solves.iter().rev() {
      if let Some(v) = solve.constraints.get(&hole) {
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

  fn ppe(&self, e: &RE<P, S>) -> String {
    pretty_expr(&self.def_symbols, &self.bind_symbols, &self.name_symbols, e)
  }

  fn ppv(&self, v: &Val<P, S>) -> String {
    pretty_val(&self.def_symbols, &self.bind_symbols, &self.name_symbols, v)
  }

  fn norm(&mut self, v: &Term<P, S>) -> Result<Term<P, S>> {
    match &v.0 {
      ValF::Hole(h) => match self.lookup_constraint(*h) {
        Some(v) => self.norm(&v.clone()),
        None => Ok(v.clone()),
      },
      ValF::Lazy(i, ks) => match self.lookup_def(*i) {
        Some((v, _)) => {
          let mut v = v.clone();
          for k in ks {
            match k {
              ContF::App(tag, vis, x) => {
                v = self.app(tag.clone(), vis.clone(), v, x.clone());
              }
              ContF::Prop(tag, name) => {
                v = self.prop(tag.clone(), v, *name);
              }
            }
          }
          self.norm(&v)
        }
        None => Err("weak_head: fail".to_string()),
      },
      _ => Ok(v.clone()),
    }
  }

  fn deep_norm(&mut self, v: &Term<P, S>) -> Result<Term<P, S>> {
    let v = match &self.norm(v)?.0 {
      ValF::Hole(i) => ValF::Hole(i.clone()),
      ValF::Neu(i, ks) => {
        let mut v = Rc::new(Val(ValF::Neu(i.clone(), Vec::new())));
        for k in ks {
          match k {
            ContF::App(tag, vis, x) => {
              let x = self.deep_norm(x)?;
              v = self.app(tag.clone(), vis.clone(), v, x);
            }
            ContF::Prop(tag, name) => {
              v = self.prop(tag.clone(), v, *name);
            }
          }
        }
        return Ok(v);
      }
      ValF::Uni => ValF::Uni,
      ValF::Pi(tag, vis, i, ty, g, bty) => {
        let ty = self.deep_norm(ty)?;
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let bty = self.eval(&g, bty);
        let bty = self.deep_norm(&bty)?;
        let bty = self.quote(&bty);
        ValF::Pi(tag.clone(), vis.clone(), fi, ty, g, bty)
      }
      ValF::Lam(tag, vis, i, ty, g, body) => {
        let ty = self.deep_norm(ty)?;
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let body = self.eval(&g, body);
        let body = self.deep_norm(&body)?;
        let body = self.quote(&body);
        ValF::Lam(tag.clone(), vis.clone(), fi, ty, g, body)
      }

      ValF::Sigma0(tag) => ValF::Sigma0(tag.clone()),
      ValF::Obj0(tag) => ValF::Obj0(tag.clone()),
      ValF::Sigma(tag, (n0, i0, ty0), g, props) => {
        let ty0 = self.deep_norm(ty0)?;
        let mut g = g.clone();
        let fi0 = self.fresh_id(i0);
        g.add_bind(*i0, Rc::new(Val(ValF::Neu(fi0, Vec::new()))));
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let ty = self.eval(&g, ty);
          let ty = self.deep_norm(&ty)?;
          let ty = self.quote(&ty);
          let fi = self.fresh_id(i);
          g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
          ps.push((*name, fi, ty));
        }
        ValF::Sigma(tag.clone(), (*n0, fi0, ty0), g, ps)
      }
      ValF::Obj(tag, (n0, e0), props) => {
        let e0 = self.deep_norm(&e0)?;
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.deep_norm(&e)?;
          ps.push((*name, e));
        }
        ValF::Obj(tag.clone(), (*n0, e0), ps)
      }
      _ => unreachable!(),
    };
    Ok(Rc::new(Val(v)))
  }

  fn app(&mut self, tag: P, vis: Vis, f: Term<P, S>, x: Term<P, S>) -> Term<P, S> {
    match &f.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lazy(d, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Lazy(*d, ks)))
      }
      ValF::Lam(ftag, fvis, i, _, g, body) => {
        assert_eq!(tag, *ftag);
        assert_eq!(vis, *fvis);
        let mut g = g.clone();
        g.add_bind(*i, x);
        self.eval(&g, body)
      }
      _ => unreachable!("{:?}", f.0),
    }
  }

  fn prop(&mut self, tag: S, e: Term<P, S>, name: NameId) -> Term<P, S> {
    match &e.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lazy(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Lazy(i.clone(), ks)))
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

  fn eval(&mut self, g: &Env<P, S>, e: &RE<P, S>) -> Term<P, S> {
    // Note: e may contain unresolved bindings
    use CExprF::*;
    let v = match &e.0 {
      Hole(i) => match self.lookup_constraint(*i) {
        Some(v) => return v.clone(),
        None => ValF::Hole(i.clone()),
      },
      Bind(i) => match g.lookup_bind(i) {
        Some(v) => return v.clone(),
        None => ValF::Neu(i.clone(), Vec::new()),
      },
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
        let ty0 = self.eval(g, ty0);
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          ps.push((*name, *i, ty.clone()));
        }
        ValF::Sigma(tag.clone(), (*n0, *i0, ty0), g.clone(), ps)
      }
      Obj(tag, (n0, e0), props) => {
        let e0 = self.eval(g, e0);
        let mut ps = Vec::new();
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

  fn eval0(&mut self, e: &RE<P, S>) -> Term<P, S> {
    self.eval(&Env::new(), e)
  }

  fn quote(&mut self, v: &Term<P, S>) -> RE<P, S> {
    use ValF::*;
    fn quote_ks<P: Tag, S: Tag>(
      this: &mut Checker<P, S>,
      ks: &Conts<P, S>,
      e: RE<P, S>,
    ) -> RE<P, S> {
      let mut e = e;
      for k in ks {
        match k {
          ContF::App(tag, vis, x) => {
            let x = this.quote(x);
            e = Rc::new(CExpr(CExprF::App(tag.clone(), vis.clone(), e, x.clone())));
          }
          ContF::Prop(tag, name) => {
            e = Rc::new(CExpr(CExprF::Prop(tag.clone(), e, *name)));
          }
        }
      }
      e
    }
    match &v.0 {
      Hole(i) => Rc::new(CExpr(CExprF::Hole(i.clone()))),
      Neu(i, ks) => {
        let e = Rc::new(CExpr(CExprF::Bind(i.clone())));
        quote_ks(self, ks, e)
      }
      Lazy(i, ks) => {
        let e = Rc::new(CExpr(CExprF::Def(i.clone())));
        quote_ks(self, ks, e)
      }
      Uni => Rc::new(CExpr(CExprF::Uni)),
      Pi(tag, vis, i, ty, g, bty) => {
        let ty = self.quote(ty);
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let bty = self.eval(&g, bty);
        let bty = self.quote(&bty);
        Rc::new(CExpr(CExprF::Pi(tag.clone(), vis.clone(), fi, ty, bty)))
      }
      Lam(tag, vis, i, ty, g, body) => {
        let ty = self.quote(ty);
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let body = self.eval(&g, body);
        let body = self.quote(&body);
        Rc::new(CExpr(CExprF::Lam(tag.clone(), vis.clone(), fi, ty, body)))
      }
      Sigma0(tag) => Rc::new(CExpr(CExprF::Sigma0(tag.clone()))),
      Obj0(tag) => Rc::new(CExpr(CExprF::Obj0(tag.clone()))),
      Sigma(tag, (n0, i0, ty0), g, props) => {
        let fi0 = self.fresh_id(i0);
        let ty0 = self.quote(ty0);
        let mut g = g.clone();
        g.add_bind(*i0, Rc::new(Val(ValF::Neu(fi0, Vec::new()))));
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let ty = self.eval(&g, ty);
          let ty = self.quote(&ty);
          let fi = self.fresh_id(i);
          g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
          ps.push((*name, fi, ty));
        }
        Rc::new(CExpr(CExprF::Sigma(tag.clone(), (*n0, fi0, ty0), ps)))
      }
      Obj(tag, (n0, e0), props) => {
        let e0 = self.quote(e0);
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.quote(e);
          ps.push((*name, e));
        }
        Rc::new(CExpr(CExprF::Obj(tag.clone(), (*n0, e0), ps)))
      }
    }
  }

  fn unify(&mut self, v1: &Term<P, S>, v2: &Term<P, S>) -> Result<()> {
    // eprintln!("unify: {} = {}", self.ppv(v1), self.ppv(v2));
    match (&v1.0, &v2.0) {
      (ValF::Hole(i1), ValF::Hole(i2)) if i1 == i2 => Ok(()),
      (ValF::Hole(i1), _) if self.lookup_constraint(*i1).is_some() => {
        let v1 = self.lookup_constraint(*i1).unwrap().clone();
        self.unify(&v1, v2)
      }
      (_, ValF::Hole(i2)) if self.lookup_constraint(*i2).is_some() => {
        let v2 = self.lookup_constraint(*i2).unwrap().clone();
        self.unify(v1, &v2)
      }
      (ValF::Hole(i1), _) => {
        if contains_hole_v(i1, v2) {
          return Err("unify: occurs check".to_string());
        }
        // eprintln!("add_constraint: {:?} = {}", i1, self.ppv(v2));
        self.solve().add_constraint(i1.clone(), v2.clone());
        Ok(())
      }
      (_, ValF::Hole(i2)) => {
        if contains_hole_v(i2, v1) {
          return Err("unify: occurs check".to_string());
        }
        // eprintln!("add_constraint: {:?} = {}", i2, self.ppv(v1));
        self.solve().add_constraint(i2.clone(), v1.clone());
        Ok(())
      }
      (ValF::Neu(i1, ks1), ValF::Neu(i2, ks2)) => {
        if i1 != i2 {
          return Err(format!("unify: {:?} != {:?}", i1, i2));
        }
        if ks1.len() != ks2.len() {
          return Err("unify: neu: len".to_string());
        }
        for (k1, k2) in ks1.iter().zip(ks2.iter()) {
          match (k1, k2) {
            (ContF::App(p1, v1, t1), ContF::App(p2, v2, t2)) => {
              if p1 == p2 && v1 == v2 {
                self.unify(t1, t2)?;
              } else {
                return Err("unify: app".to_string());
              }
            }
            (ContF::Prop(s1, n1), ContF::Prop(s2, n2)) => {
              if s1 == s2 && n1 == n2 {
                // ok
              } else {
                return Err("unify: prop".to_string());
              }
            }
            _ => return Err("unify: cont".to_string()),
          }
        }
        Ok(())
      }
      (ValF::Lazy(_, _), _) => {
        // TODO: optimize
        let v1 = self.norm(v1)?;
        self.unify(&v1, v2)
      }
      (_, ValF::Lazy(_, _)) => {
        // TODO: optimize
        let v2 = self.norm(v2)?;
        self.unify(v1, &v2)
      }
      (ValF::Uni, ValF::Uni) => Ok(()),
      (ValF::Pi(t1, v1, i1, ty1, g1, bty1), ValF::Pi(t2, v2, i2, ty2, g2, bty2)) => {
        if t1 == t2 && v1 == v2 {
          self.unify(ty1, ty2)?;
          let mut g1 = g1.clone();
          let mut g2 = g2.clone();
          let fi = self.fresh_id(&i1);
          g1.add_bind(*i1, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          g2.add_bind(*i2, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          let bty1 = self.eval(&g1, bty1);
          let bty2 = self.eval(&g2, bty2);
          self.unify(&bty1, &bty2)?;
          Ok(())
        } else {
          Err("unify: pi".to_string())
        }
      }
      (ValF::Lam(t1, v1, i1, ty1, g1, bty1), ValF::Lam(t2, v2, i2, ty2, g2, bty2)) => {
        if t1 == t2 && v1 == v2 {
          self.unify(ty1, ty2)?;
          let mut g1 = g1.clone();
          let mut g2 = g2.clone();
          let fi = self.fresh_id(&i1);
          g1.add_bind(*i1, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          g2.add_bind(*i2, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          let bty1 = self.eval(&g1, bty1);
          let bty2 = self.eval(&g2, bty2);
          self.unify(&bty1, &bty2)?;
          Ok(())
        } else {
          Err("unify: pi".to_string())
        }
      }
      (
        ValF::Sigma(t1, (n01, i01, ty01), g1, props1),
        ValF::Sigma(t2, (n02, i02, ty02), g2, props2),
      ) => {
        if t1 != t2 {
          return Err("unify: sigma".to_string());
        }
        self.unify(ty01, ty02)?;
        if n01 != n02 {
          return Err("unify: sigma".to_string());
        }
        let mut g1 = g1.clone();
        let mut g2 = g2.clone();
        let fi0 = self.fresh_id(&i01);
        g1.add_bind(*i01, Rc::new(Val(ValF::Neu(fi0.clone(), Vec::new()))));
        g2.add_bind(*i02, Rc::new(Val(ValF::Neu(fi0.clone(), Vec::new()))));
        for ((n1, i1, ty1), (n2, i2, ty2)) in props1.iter().zip(props2.iter()) {
          if n1 == n2 {
            let ty1 = self.eval(&g1, ty1);
            let ty2 = self.eval(&g2, ty2);
            self.unify(&ty1, &ty2)?;
            let fi = self.fresh_id(&i01);
            g1.add_bind(*i1, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
            g2.add_bind(*i2, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          } else {
            return Err("unify: sigma".to_string());
          }
        }
        if props1.len() == props2.len() {
          Ok(())
        } else {
          Err("unify: sigma".to_string())
        }
      }
      (ValF::Obj(t1, (n01, e01), props1), ValF::Obj(t2, (n02, e02), props2)) => {
        if t1 != t2 {
          return Err("unify: obj".to_string());
        }
        if n01 != n02 {
          return Err("unify: obj".to_string());
        }
        self.unify(e01, e02)?;
        for ((n1, e1), (n2, e2)) in props1.iter().zip(props2.iter()) {
          if n1 == n2 {
            self.unify(e1, e2)?;
          } else {
            return Err("unify: obj".to_string());
          }
        }
        if props1.len() == props2.len() {
          Ok(())
        } else {
          Err("unify: obj".to_string())
        }
      }
      _ => {
        eprintln!("unify: {:?} = {:?}", v1, v2);
        Err("unify: failed".to_string())
      }
    }
  }

  fn check(&mut self, e: Expr<P, S>, check_ty: &Type<P, S>) -> Result<RE<P, S>> {
    use ExprF::*;
    match e.0 {
      Hole => {
        let h = self.fresh_hole();
        self.solve().add_hole(h, check_ty.clone());
        Ok(Rc::new(CExpr(CExprF::Hole(h))))
      }
      Let(defs, body) => {
        let ds = self.defs(defs);
        unimplemented!()
      }
      Lam(tag, vis, i, ty, body) => match &self.norm(check_ty)?.0 {
        ValF::Pi(ttag, tvis, ti, tty, tg, bty) => {
          if tag != *ttag || vis != *tvis {
            return Err("check: lam".to_string());
          }
          let c_ty = self.check_ty(*ty)?;
          let ty = self.eval0(&c_ty);
          self.unify(&ty, tty)?;
          self.varenvs.push(VarEnv::new());
          self.varenv().add(i, tty.clone());
          let mut tg = tg.clone();
          tg.add_bind(*ti, Rc::new(Val(ValF::Neu(i, Vec::new()))));
          let bty = self.eval(&tg, bty);
          let c_body = self.check(*body, &bty)?;
          self.varenvs.pop();
          Ok(Rc::new(CExpr(CExprF::Lam(tag, vis, i, c_ty, c_body))))
        }
        _ => Err(format!("check: lam / {:?}", check_ty)),
      },
      Obj(tag, props) => match &self.norm(check_ty)?.0 {
        ValF::Sigma0(ttag) => {
          if tag != *ttag {
            return Err("check: obj 0".to_string());
          }
          if !props.is_empty() {
            return Err("check: obj 0 (not length 0)".to_string());
          }
          return Ok(Rc::new(CExpr(CExprF::Obj0(tag))));
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
          self.varenvs.push(VarEnv::new());
          self.varenv().add(*ti0, tty0.clone());
          let mut tg = tg.clone();
          tg.add_bind(*ti0, e0);
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
            self.varenv().add(*ti, ty.clone());
            tg.add_bind(*ti, e.clone());
            ps.push((*ti, e));
            c_ps.push((n, c_e));
          }
          self.varenvs.pop();
          if !len_match {
            // we defer this error to check the existing props as much as possible
            return Err("check: obj 3".to_string());
          }
          Ok(Rc::new(CExpr(CExprF::Obj(tag, (n0, c_e0), c_ps))))
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

  fn check_ty(&mut self, e: Expr<P, S>) -> Result<RE<P, S>> {
    self.check(e, &Rc::new(Val(ValF::Uni)))
  }

  fn resolve_implicits(
    &mut self,
    c_f: RE<P, S>,
    fty: Type<P, S>,
  ) -> Result<(RE<P, S>, Type<P, S>)> {
    match &self.norm(&fty)?.0 {
      ValF::Pi(ftag, Vis::Implicit, i, ty, fg, bty) => {
        // there should be implicit applications in between f and x
        let h = self.fresh_hole();
        self.solve().add_hole(h, ty.clone());
        let c_fh = Rc::new(CExpr(CExprF::App(
          ftag.clone(),
          Vis::Implicit,
          c_f,
          Rc::new(CExpr(CExprF::Hole(h))),
        )));
        let mut fg = fg.clone();
        fg.add_bind(*i, Rc::new(Val(ValF::Hole(h))));
        let bty = self.eval(&fg, bty);
        self.resolve_implicits(c_fh, bty)
      }
      _ => Ok((c_f, fty)),
    }
  }

  fn synth(&mut self, e: Expr<P, S>) -> Result<(RE<P, S>, Type<P, S>)> {
    use ExprF::*;
    match e.0 {
      Hole => Err("synth: hole".to_string()),
      Bind(i) => match self.lookup_bind(i) {
        Some(ty) => Ok((Rc::new(CExpr(CExprF::Bind(i))), ty.clone())),
        None => Err("synth: bind".to_string()),
      },
      Def(i) => match self.lookup_def(i) {
        Some((_, ty)) => Ok((Rc::new(CExpr(CExprF::Def(i))), ty.clone())),
        None => Err("synth: failed def".to_string()),
      },
      Ann(tm, ty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        let c_tm = self.check(*tm, &ty)?;
        Ok((Rc::new(CExpr(CExprF::Ann(c_tm, c_ty))), ty))
      }
      Uni => Ok(((Rc::new(CExpr(CExprF::Uni))), Rc::new(Val(ValF::Uni)))),

      Let(defs, body) => {
        let ds = self.defs(defs);
        unimplemented!()
      }

      Pi(tag, vis, i, ty, bty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i, ty.clone());
        let c_bty = self.check_ty(*bty)?;
        self.varenvs.pop();
        Ok((
          Rc::new(CExpr(CExprF::Pi(tag, vis, i, c_ty, c_bty))),
          Rc::new(Val(ValF::Uni)),
        ))
      }
      Lam(tag, vis, i, ty, body) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i, ty.clone());
        let (c_body, bty) = self.synth(*body)?;
        let bty = self.quote(&bty);
        self.varenvs.pop();
        Ok((
          (Rc::new(CExpr(CExprF::Lam(
            tag.clone(),
            vis.clone(),
            i,
            c_ty,
            c_body,
          )))),
          Rc::new(Val(ValF::Pi(tag, vis, i, ty, Env::new(), bty))),
        ))
      }
      App(tag, vis, f, x) => {
        let (c_f, fty) = self.synth(*f)?;
        let (c_f, fty) = match vis {
          Vis::Explicit => self.resolve_implicits(c_f, fty)?,
          Vis::Implicit => (c_f, fty),
        };
        match &self.norm(&fty)?.0 {
          ValF::Pi(ftag, fvis, i, ty, fg, bty) => {
            if tag != *ftag || vis != *fvis {
              return Err("synth: app".to_string());
            }
            let c_x = self.check(*x, &ty)?;
            let x = self.eval0(&c_x);
            let mut fg = fg.clone();
            fg.add_bind(*i, x);
            let bty = self.eval(&fg, bty);
            Ok((Rc::new(CExpr(CExprF::App(tag, vis, c_f, c_x))), bty))
          }
          _ => Err(format!("synth: app / {:?}", fty)),
        }
      }

      Sigma(tag, props) => {
        if props.is_empty() {
          return Ok((Rc::new(CExpr(CExprF::Sigma0(tag))), Rc::new(Val(ValF::Uni))));
        }
        let mut pi = props.into_iter();
        let (n0, i0, ty0) = pi.next().unwrap();
        let c_ty0 = self.check_ty(*ty0)?;
        let ty0 = self.eval0(&c_ty0);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i0, ty0);
        let mut c_props = Vec::new();
        for (name, i, ty) in pi {
          let c_ty = self.check_ty(*ty)?;
          let ty = self.eval0(&c_ty);
          self.varenv().add(i, ty);
          c_props.push((name, i, c_ty));
        }
        self.varenvs.pop();
        Ok((
          Rc::new(CExpr(CExprF::Sigma(tag, (n0, i0, c_ty0), c_props))),
          Rc::new(Val(ValF::Uni)),
        ))
      }
      Obj(tag, props) => {
        if props.is_empty() {
          return Ok((
            Rc::new(CExpr(CExprF::Obj0(tag.clone()))),
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
          tys.push((name.clone(), i, ty));
          c_props.push((name, c_e));
        }
        Ok((
          Rc::new(CExpr(CExprF::Obj(tag.clone(), (n0, c_e0), c_props))),
          Rc::new(Val(ValF::Sigma(tag, (n0, i0, ty0), Env::new(), tys))),
        ))
      }
      Prop(tag, e, name) => {
        let (c_e, ty) = self.synth(*e)?;
        match &self.norm(&ty)?.0 {
          ValF::Sigma(etag, (n0, i0, ty0), eg, props) => {
            if tag != *etag {
              return Err("synth: prop".to_string());
            }
            if name == *n0 {
              return Ok((Rc::new(CExpr(CExprF::Prop(tag, c_e, name))), ty0.clone()));
            }
            for (index, (n, _, ty)) in props.iter().enumerate() {
              if name == *n {
                let e = self.eval0(&c_e);
                let mut eg = eg.clone();
                // ty may depend on previous props
                let p0 = self.prop(tag.clone(), e.clone(), *n0); // e.n0
                eg.add_bind(*i0, p0);
                for (n, i, _) in props.iter().take(index) {
                  let p = self.prop(tag.clone(), e.clone(), *n); // e.n
                  eg.add_bind(*i, p);
                }
                let ty = self.eval(&eg, &ty);
                return Ok((Rc::new(CExpr(CExprF::Prop(tag, c_e, name))), ty));
              }
            }
            Err("synth: prop".to_string())
          }
          _ => Err("synth: prop".to_string()),
        }
      }
    }
  }

  fn hole_resolve(&mut self, solve: Solve<P, S>) -> Result<HashMap<HoleId, (RE<P, S>, RV<P, S>)>> {
    eprintln!("[Solve]");
    eprintln!("- holes:");
    for (i, ty) in &solve.holes {
      eprintln!("  - {:?}: {}", i, self.ppv(ty));
    }
    eprintln!("- constraints:");
    for (i, tm) in &solve.constraints {
      eprintln!("  - {:?} = {}", i, self.ppv(tm));
    }
    for (j, _) in &solve.holes {
      if !solve.constraints.contains_key(j) {
        return Err("unresolved hole".to_string());
      }
    }
    // TODO: resolve hole-in-hole constraints at this point
    let mut hole_map = HashMap::new();
    for (i, v) in solve.constraints {
      let e = self.quote(&v);
      hole_map.insert(i, (e, v));
    }
    Ok(hole_map)
  }

  fn def(&mut self, e: Box<Expr<P, S>>) -> Result<(RE<P, S>, Term<P, S>, Type<P, S>)> {
    self.solves.push(Solve::new());
    let depth1 = (self.varenvs.len(), self.defenvs.len(), self.solves.len());
    let res = self.synth(*e);
    let depth2 = (self.varenvs.len(), self.defenvs.len(), self.solves.len());
    match &res {
      Ok(_) => assert_eq!(depth1, depth2),
      Err(_) => {
        assert!(depth1.0 <= depth2.0);
        assert!(depth1.1 <= depth2.1);
        assert!(depth1.2 <= depth2.2);
        // rollback
        self.varenvs.truncate(depth1.0);
        self.defenvs.truncate(depth1.1);
        self.solves.truncate(depth1.2);
      }
    }
    let solve = self.solves.pop().unwrap();
    let (ce, ty) = res?;
    let hole_map = self.hole_resolve(solve)?;

    eprintln!("- From");
    eprintln!("  - {}: {}", self.ppe(&ce), self.ppv(&ty));
    let ce = subst_hole_e(&hole_map, &ce).into();
    let ty = subst_hole_v(&hole_map, &ty).into();
    eprintln!("- To");
    eprintln!("  - {}: {}", self.ppe(&ce), self.ppv(&ty));
    for i in hole_map.keys() {
      if contains_hole_e(i, &ce) || contains_hole_v(i, &ty) {
        return Err("hole remains unresolved".to_string());
      }
    }
    // TODO: scope check?

    let tm = self.eval0(&ce);
    Ok((ce, tm, ty))
  }

  fn defs(&mut self, defs: Vec<(DefId, Box<Expr<P, S>>)>) -> Vec<(DefId, RE<P, S>)> {
    self.defenvs.push(DefEnv::new());
    let mut def_terms = Vec::new();
    for (id, expr) in defs {
      eprintln!("- Def[ {} ]", self.def_symbols[&id]);
      let res = self.def(expr);
      match &res {
        Ok((_, _, ty)) => eprintln!("[o] {}: {}", self.def_symbols[&id], self.ppv(ty)),
        Err(e) => eprintln!("[x] {}: {}", self.def_symbols[&id], e),
      }
      match res {
        Ok((c_expr, tm, ty)) => {
          // let dtm = self.deep_norm(&tm).unwrap();
          // eprintln!("    {} = {}", self.def_symbols[&id], self.ppv(&dtm));
          self.defenv().add(id, tm, ty);
          def_terms.push((id, c_expr));
        }
        Err(e) => {
          self
            .errors
            .push(format!("def {}: {}", self.def_symbols[&id], e));
        }
      };
    }
    self.defenvs.pop();
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
