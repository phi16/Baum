use crate::levels::*;
use crate::pretty::{pretty_expr, pretty_val};
use crate::subst::SubstEnv;
use crate::types::common::*;
use crate::types::level::*;
use crate::types::tree::*;
use crate::types::val::*;
use colored::Colorize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Error {
  Loc(String),
  Fail,
}

type Result<T> = std::result::Result<T, Error>;

struct VarEnv<P, S> {
  lookup: HashMap<BindId, TyUni<P, S>>,
}

impl<P, S> VarEnv<P, S> {
  fn new() -> Self {
    VarEnv {
      lookup: HashMap::new(),
    }
  }

  fn add(&mut self, bind: BindId, ty: TyUni<P, S>) {
    self.lookup.insert(bind, ty);
  }
}

struct DefEnv<P, S> {
  define: HashMap<DefId, (Solution, RE<P, S>, TyUni<P, S>)>,
}

impl<P, S> DefEnv<P, S> {
  fn new() -> Self {
    DefEnv {
      define: HashMap::new(),
    }
  }

  fn add(&mut self, def: DefId, d: (Solution, RE<P, S>, TyUni<P, S>)) {
    self.define.insert(def, d);
  }
}

struct Solve<P, S> {
  holes: HashMap<HoleId, Type<P, S>>,
  hole_assign: HashMap<HoleId, RE<P, S>>,
  levels: HashSet<LevelId>,
  level_constraints: Vec<(LevelId, LevelRel, LevelId)>,
  constraints_accum: Option<Vec<(LevelId, LevelRel, LevelId)>>,
}

impl<P, S> Solve<P, S> {
  fn new() -> Self {
    Solve {
      holes: HashMap::new(),
      hole_assign: HashMap::new(),
      levels: HashSet::new(),
      level_constraints: Vec::new(),
      constraints_accum: None,
    }
  }

  fn add_hole(&mut self, hole: HoleId, ty: Type<P, S>) {
    self.holes.insert(hole, ty);
  }

  fn add_assign(&mut self, hole: HoleId, e: RE<P, S>) {
    self.hole_assign.insert(hole, e);
  }

  fn add_level(&mut self, level: LevelId) {
    self.levels.insert(level);
  }

  fn add_constraint(&mut self, l1: LevelId, rel: LevelRel, l2: LevelId) {
    self.level_constraints.push((l1, rel, l2));
    self.constraints_accum = None;
  }
}

fn contains_hole_e<P, S>(i: &HoleId, e: &RE<P, S>) -> bool {
  match &e.0 {
    CExprF::Hole(j) => i == j,
    CExprF::Bind(_) => false,
    CExprF::Def(_, _) => false,
    CExprF::Ann(e, _) => contains_hole_e(i, e),
    CExprF::Uni(_) => false,
    CExprF::Let(defs, body) => {
      defs.iter().any(|(_, _, e)| contains_hole_e(i, e)) || contains_hole_e(i, body)
    }
    CExprF::Pi(_, _, _, ty, body, _) => contains_hole_e(i, ty) || contains_hole_e(i, body),
    CExprF::Lam(_, _, _, ty, body) => contains_hole_e(i, ty) || contains_hole_e(i, body),
    CExprF::App(_, _, f, x) => contains_hole_e(i, f) || contains_hole_e(i, x),
    CExprF::Sigma0(_) => false,
    CExprF::Obj0(_) => false,
    CExprF::Sigma(_, (_, _, ty, _), props) => {
      contains_hole_e(i, ty) || props.iter().any(|(_, _, ty, _)| contains_hole_e(i, ty))
    }
    CExprF::Obj(_, (_, e), props) => {
      contains_hole_e(i, e) || props.iter().any(|(_, e)| contains_hole_e(i, e))
    }
    CExprF::Prop(_, e, _) => contains_hole_e(i, e),
  }
}

pub struct Checker<P, S> {
  def_symbols: HashMap<DefId, String>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  varenvs: Vec<VarEnv<P, S>>,
  defenvs: Vec<DefEnv<P, S>>,
  solves: Vec<Solve<P, S>>,
  next_bind_id: u32,
  next_hole_id: u32,
  next_level_id: u32,
  log_head: String,
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
      next_level_id: 0,
      log_head: String::new(),
      errors: Vec::new(),
    }
  }

  fn log(&self, msg: String) {
    eprintln!("{}{}", self.log_head, msg);
  }
  fn push_indent(&mut self) {
    self.log_head.push_str("  ");
  }
  fn pop_indent(&mut self) {
    self.log_head.pop();
    self.log_head.pop();
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

  fn fresh_level(&mut self) -> LevelId {
    let id = LevelId(self.next_level_id);
    self.next_level_id += 1;
    self.solve().add_level(id);
    id
  }

  fn lookup_bind(&self, bind: BindId) -> Option<&TyUni<P, S>> {
    for env in self.varenvs.iter().rev() {
      if let Some(ty) = env.lookup.get(&bind) {
        return Some(ty);
      }
    }
    None
  }

  fn lookup_def(&self, def: DefId) -> Option<&(Solution, RE<P, S>, TyUni<P, S>)> {
    for env in self.defenvs.iter().rev() {
      if let Some(d) = env.define.get(&def) {
        return Some(d);
      }
    }
    None
  }

  fn lookup_assign(&self, hole: HoleId) -> Option<&RE<P, S>> {
    for solve in self.solves.iter().rev() {
      if let Some(v) = solve.hole_assign.get(&hole) {
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

  fn fresh_levels(&mut self, sol: &Solution) -> Vec<LevelId> {
    let ls = (0..sol.groups)
      .map(|_| self.fresh_level())
      .collect::<Vec<_>>();
    for (r1, rel, r2) in &sol.constraints {
      let l1 = match r1 {
        LevelRef::Id(i1) => *i1,
        LevelRef::Group(g1) => ls[*g1 as usize],
      };
      let l2 = match r2 {
        LevelRef::Id(i2) => *i2,
        LevelRef::Group(g2) => ls[*g2 as usize],
      };
      self.solve().add_constraint(l1, rel.clone(), l2);
    }
    ls
  }

  pub fn map_solution(&self, sol: &Solution, ls: &Vec<LevelId>) -> HashMap<LevelId, LevelId> {
    let m = sol
      .scope
      .iter()
      .map(|i| (i.0, ls[i.1 as usize]))
      .collect::<HashMap<_, _>>();
    m
  }

  fn ppe(&self, e: &RE<P, S>) -> String {
    pretty_expr(&self.def_symbols, &self.bind_symbols, &self.name_symbols, e)
  }

  fn ppv(&self, v: &Val<P, S>) -> String {
    pretty_val(&self.def_symbols, &self.bind_symbols, &self.name_symbols, v)
  }

  fn norm(&mut self, v: &Term<P, S>) -> Result<Term<P, S>> {
    match &v.0 {
      ValF::Hole(h) => match self.lookup_assign(*h) {
        Some(e) => {
          let v = self.eval0(&e.clone());
          self.norm(&v)
        }
        None => Err(Error::Loc("Found hole in normalization".to_string())),
      },
      ValF::Lazy(i, ls, ks) => match self.lookup_def(*i).cloned() {
        Some((sol, e, _)) => {
          let ls_m = self.map_solution(&sol, &ls);
          let mut subst = SubstEnv::from_levels(ls_m, self);
          let e = subst.subst_e(&e).into();
          let mut v = self.eval0(&e.clone());
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
        None => Err(Error::Fail),
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
      ValF::Uni(i) => ValF::Uni(*i),
      ValF::Pi(tag, vis, i, ty, g, bty, l_bty) => {
        let ty = self.deep_norm(ty)?;
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let bty = self.eval(&g, bty);
        let bty = self.deep_norm(&bty)?;
        let bty = self.quote(&bty);
        ValF::Pi(tag.clone(), vis.clone(), fi, ty, g, bty, *l_bty)
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
      ValF::Sigma(tag, (n0, i0, ty0, l_ty0), g, props) => {
        let ty0 = self.deep_norm(ty0)?;
        let mut g = g.clone();
        let fi0 = self.fresh_id(i0);
        g.add_bind(*i0, Rc::new(Val(ValF::Neu(fi0, Vec::new()))));
        let mut ps = Vec::new();
        for (name, i, ty, l_ty) in props {
          let ty = self.eval(&g, ty);
          let ty = self.deep_norm(&ty)?;
          let ty = self.quote(&ty);
          let fi = self.fresh_id(i);
          g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
          ps.push((*name, fi, ty, *l_ty));
        }
        ValF::Sigma(tag.clone(), (*n0, fi0, ty0, *l_ty0), g, ps)
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

  pub fn app(&mut self, tag: P, vis: Vis, f: Term<P, S>, x: Term<P, S>) -> Term<P, S> {
    match &f.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lazy(d, ls, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Lazy(*d, ls.clone(), ks)))
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

  pub fn prop(&mut self, tag: S, e: Term<P, S>, name: NameId) -> Term<P, S> {
    match &e.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lazy(i, ls, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Lazy(i.clone(), ls.clone(), ks)))
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
      Hole(i) => match self.lookup_assign(*i) {
        Some(e) => return self.eval(g, &e.clone()),
        None => ValF::Hole(i.clone()),
      },
      Bind(i) => match g.lookup_bind(i) {
        Some(v) => return v.clone(),
        None => ValF::Neu(i.clone(), Vec::new()),
      },
      Def(i, ls) => match g.lookup_def(i) {
        Some(v) => return v.clone(),
        None => ValF::Lazy(i.clone(), ls.clone(), Vec::new()),
      },
      Ann(t, _) => return self.eval(g, t),
      Uni(i) => ValF::Uni(*i),
      Let(defs, body) => {
        let mut g = g.clone();
        for (i, sol, e) in defs {
          let ls = self.fresh_levels(&sol);
          let ls_m = self.map_solution(&sol, &ls);
          let mut subst = SubstEnv::from_levels(ls_m, self);
          let e = subst.subst_e(e).into();
          let v = self.eval(&g, &e);
          g.add_def(*i, v);
        }
        return self.eval(&g, body);
      }

      Pi(tag, vis, i, ty, bty, l_bty) => {
        let ty = self.eval(g, ty);
        ValF::Pi(
          tag.clone(),
          vis.clone(),
          *i,
          ty,
          g.clone(),
          bty.clone(),
          *l_bty,
        )
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
      Sigma(tag, (n0, i0, ty0, l_ty0), props) => {
        let ty0 = self.eval(g, ty0);
        let mut ps = Vec::new();
        for (name, i, ty, l_ty) in props {
          ps.push((*name, *i, ty.clone(), *l_ty));
        }
        ValF::Sigma(tag.clone(), (*n0, *i0, ty0, *l_ty0), g.clone(), ps)
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
      Lazy(i, ls, ks) => {
        let e = Rc::new(CExpr(CExprF::Def(i.clone(), ls.clone())));
        quote_ks(self, ks, e)
      }
      Uni(i) => Rc::new(CExpr(CExprF::Uni(i.clone()))),
      Pi(tag, vis, i, ty, g, bty, l_bty) => {
        let ty = self.quote(ty);
        let mut g = g.clone();
        let fi = self.fresh_id(i);
        g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
        let bty = self.eval(&g, bty);
        let bty = self.quote(&bty);
        Rc::new(CExpr(CExprF::Pi(
          tag.clone(),
          vis.clone(),
          fi,
          ty,
          bty,
          *l_bty,
        )))
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
      Sigma(tag, (n0, i0, ty0, l_ty0), g, props) => {
        let fi0 = self.fresh_id(i0);
        let ty0 = self.quote(ty0);
        let mut g = g.clone();
        g.add_bind(*i0, Rc::new(Val(ValF::Neu(fi0, Vec::new()))));
        let mut ps = Vec::new();
        for (name, i, ty, l_ty) in props {
          let ty = self.eval(&g, ty);
          let ty = self.quote(&ty);
          let fi = self.fresh_id(i);
          g.add_bind(*i, Rc::new(Val(ValF::Neu(fi, Vec::new()))));
          ps.push((*name, fi, ty, *l_ty));
        }
        Rc::new(CExpr(CExprF::Sigma(
          tag.clone(),
          (*n0, fi0, ty0, *l_ty0),
          ps,
        )))
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
    self.push_indent();
    let res = self.unify_internal(v1, v2);
    self.pop_indent();
    res
  }

  fn unify_internal(&mut self, v1: &Term<P, S>, v2: &Term<P, S>) -> Result<()> {
    let fail = |this: &Checker<P, S>, msg: &str| -> Result<_> {
      Err(Error::Loc(format!(
        "Failed to unify ({}): {} ≟ {}",
        msg,
        this.ppv(v1),
        this.ppv(v2)
      )))
    };
    // self.log(format!("unify: {} ≟ {}", self.ppv(v1), self.ppv(v2)));
    match (&v1.0, &v2.0) {
      (ValF::Hole(i1), ValF::Hole(i2)) if i1 == i2 => Ok(()),
      (ValF::Hole(i1), _) if self.lookup_assign(*i1).is_some() => {
        let e1 = self.lookup_assign(*i1).unwrap().clone();
        let v1 = self.eval0(&e1);
        self.unify(&v1, v2)
      }
      (_, ValF::Hole(i2)) if self.lookup_assign(*i2).is_some() => {
        let e2 = self.lookup_assign(*i2).unwrap().clone();
        let v2 = self.eval0(&e2);
        self.unify(v1, &v2)
      }
      (ValF::Hole(i1), _) => {
        let e2 = self.quote(v2);
        if contains_hole_e(i1, &e2) {
          return fail(self, "recursive hole");
        }
        // self.log(format!("add_assign: {:?} = {}", i1, self.ppe(&e2)));
        self.solve().add_assign(i1.clone(), e2);
        Ok(())
      }
      (_, ValF::Hole(i2)) => {
        let e1 = self.quote(v1);
        if contains_hole_e(i2, &e1) {
          return fail(self, "recursive hole");
        }
        // self.log(format!("add_assign: {:?} = {}", i2, self.ppe(&e1)));
        self.solve().add_assign(i2.clone(), e1);
        Ok(())
      }
      (ValF::Neu(i1, ks1), ValF::Neu(i2, ks2)) => {
        if ks1.len() != ks2.len() {
          return fail(self, &format!("continuation length mismatch (TODO)"));
        }
        if i1 != i2 {
          return fail(self, &format!("identifier mismatch: {:?} ≟ {:?}", i1, i2));
        }
        for (k1, k2) in ks1.iter().zip(ks2.iter()) {
          match (k1, k2) {
            (ContF::App(p1, v1, t1), ContF::App(p2, v2, t2)) => {
              if p1 != p2 {
                return fail(self, "app / role mismatch");
              }
              if v1 != v2 {
                return fail(self, "app / visibility mismatch");
              }
              self.unify(t1, t2)?;
            }
            (ContF::Prop(s1, n1), ContF::Prop(s2, n2)) => {
              if s1 != s2 {
                return fail(self, "prop / role mismatch");
              }
              if n1 != n2 {
                return fail(self, &format!("prop / name mismatch: {:?} ≟ {:?}", n1, n2));
              }
              // ok
            }
            _ => return fail(self, "continuation mismatch"),
          }
        }
        Ok(())
      }
      (ValF::Lazy(_, _, _), _) => {
        // TODO: optimize
        let v1 = self.norm(v1)?;
        self.unify(&v1, v2)
      }
      (_, ValF::Lazy(_, _, _)) => {
        // TODO: optimize
        let v2 = self.norm(v2)?;
        self.unify(v1, &v2)
      }
      (ValF::Uni(i1), ValF::Uni(i2)) => {
        self.solve().add_constraint(*i1, LevelRel::Eq, *i2);
        Ok(())
      }
      (
        ValF::Pi(t1, v1, i1, ty1, g1, bty1, l_bty1),
        ValF::Pi(t2, v2, i2, ty2, g2, bty2, l_bty2),
      ) => {
        if t1 != t2 {
          return fail(self, "Π / role mismatch");
        }
        if v1 != v2 {
          return fail(self, "Π / visibility mismatch");
        }
        self.unify(ty1, ty2)?;
        let mut g1 = g1.clone();
        let mut g2 = g2.clone();
        let fi = self.fresh_id(&i1);
        g1.add_bind(*i1, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
        g2.add_bind(*i2, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
        let bty1 = self.eval(&g1, bty1);
        let bty2 = self.eval(&g2, bty2);
        self.unify(&bty1, &bty2)?;
        self.solve().add_constraint(*l_bty1, LevelRel::Eq, *l_bty2);
        Ok(())
      }
      (ValF::Lam(t1, v1, i1, ty1, g1, bty1), ValF::Lam(t2, v2, i2, ty2, g2, bty2)) => {
        if t1 != t2 {
          return fail(self, "λ / role mismatch");
        }
        if v1 != v2 {
          return fail(self, "λ / visibility mismatch");
        }
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
      }
      (
        ValF::Sigma(t1, (n01, i01, ty01, l_ty01), g1, props1),
        ValF::Sigma(t2, (n02, i02, ty02, l_ty02), g2, props2),
      ) => {
        if t1 != t2 {
          return fail(self, "Σ / role mismatch");
        }
        self.unify(ty01, ty02)?;
        self.solve().add_constraint(*l_ty01, LevelRel::Eq, *l_ty02);
        if n01 != n02 {
          return fail(self, &format!("Σ / name mismatch: {:?} ≟ {:?}", n01, n02));
        }
        let mut g1 = g1.clone();
        let mut g2 = g2.clone();
        let fi0 = self.fresh_id(&i01);
        g1.add_bind(*i01, Rc::new(Val(ValF::Neu(fi0.clone(), Vec::new()))));
        g2.add_bind(*i02, Rc::new(Val(ValF::Neu(fi0.clone(), Vec::new()))));
        for ((n1, i1, ty1, l_ty1), (n2, i2, ty2, l_ty2)) in props1.iter().zip(props2.iter()) {
          if n1 != n2 {
            return fail(self, &format!("Σ / name mismatch: {:?} ≟ {:?}", n1, n2));
          }
          let ty1 = self.eval(&g1, ty1);
          let ty2 = self.eval(&g2, ty2);
          self.unify(&ty1, &ty2)?;
          self.solve().add_constraint(*l_ty1, LevelRel::Eq, *l_ty2);
          let fi = self.fresh_id(&i01);
          g1.add_bind(*i1, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
          g2.add_bind(*i2, Rc::new(Val(ValF::Neu(fi.clone(), Vec::new()))));
        }
        if props1.len() != props2.len() {
          return fail(self, "Σ / length mismatch");
        }
        Ok(())
      }
      (ValF::Obj(t1, (n01, e01), props1), ValF::Obj(t2, (n02, e02), props2)) => {
        if t1 != t2 {
          return fail(self, "obj / role mismatch");
        }
        if n01 != n02 {
          return fail(self, &format!("obj / name mismatch: {:?} ≟ {:?}", n01, n02));
        }
        self.unify(e01, e02)?;
        for ((n1, e1), (n2, e2)) in props1.iter().zip(props2.iter()) {
          if n1 != n2 {
            return fail(self, &format!("obj / name mismatch: {:?} ≟ {:?}", n1, n2));
          }
          self.unify(e1, e2)?;
        }
        if props1.len() != props2.len() {
          return fail(self, "obj / length mismatch");
        }
        Ok(())
      }
      _ => Err(Error::Loc(format!(
        "Failed to unify: {} ≟ {}",
        self.ppv(v1),
        self.ppv(v2)
      ))),
    }
  }

  fn check(&mut self, e: Expr<P, S>, check_ty: &Type<P, S>) -> Result<RE<P, S>> {
    let res = self.check_internal(e, check_ty);
    /* if let Ok(e) = &res {
      self.log(format!("CHECK {}: {}", self.ppe(e), self.ppv(check_ty)));
    } */
    res
  }

  fn check_internal(&mut self, e: Expr<P, S>, check_ty: &Type<P, S>) -> Result<RE<P, S>> {
    let ec = e.clone(); // TODO: redundant?
    let fail = |this: &Checker<P, S>, msg: &str| -> Result<_> {
      Err(Error::Loc(format!(
        "Failed to check type ({}): {:?}: {}",
        msg,
        ec,
        this.ppv(check_ty)
      )))
    };
    use ExprF::*;
    match e.0 {
      Hole => {
        let h = self.fresh_hole();
        self.solve().add_hole(h, check_ty.clone());
        Ok(Rc::new(CExpr(CExprF::Hole(h))))
      }
      Uni => match &self.norm(check_ty)?.0 {
        ValF::Uni(tyl) => {
          let tml = self.fresh_level();
          self.solve().add_constraint(tml, LevelRel::Lt, *tyl);
          Ok(Rc::new(CExpr(CExprF::Uni(tml))))
        }
        _ => fail(self, "𝒰 / not 𝒰"),
      },
      Let(defs, body) => {
        self.defenvs.push(DefEnv::new());
        let ds = self.defs(defs);
        let body = self.check(*body, check_ty)?;
        self.defenvs.pop();
        Ok(Rc::new(CExpr(CExprF::Let(ds, body))))
      }
      Lam(tag, vis, i, ty, body) => match &self.norm(check_ty)?.0 {
        ValF::Pi(ttag, tvis, ti, tty, tg, bty, _) => {
          if tag != *ttag {
            return fail(self, "λ / role mismatch");
          }
          if vis != *tvis {
            return fail(self, "λ / visibility mismatch");
          }
          let (c_ty, l_ty) = self.check_ty(*ty)?;
          let ty = self.eval0(&c_ty);
          self.unify(&ty, tty)?;
          self.varenvs.push(VarEnv::new());
          self.varenv().add(i, (tty.clone(), l_ty));
          let mut tg = tg.clone();
          tg.add_bind(*ti, Rc::new(Val(ValF::Neu(i, Vec::new()))));
          let bty = self.eval(&tg, bty);
          let c_body = self.check(*body, &bty)?;
          self.varenvs.pop();
          Ok(Rc::new(CExpr(CExprF::Lam(tag, vis, i, c_ty, c_body))))
        }
        _ => fail(self, "λ / not Π"),
      },
      Obj(tag, props) => match &self.norm(check_ty)?.0 {
        ValF::Sigma0(ttag) => {
          if tag != *ttag {
            return fail(self, "obj / role mismatch");
          }
          if !props.is_empty() {
            return fail(self, "obj / length mismatch");
          }
          return Ok(Rc::new(CExpr(CExprF::Obj0(tag))));
        }
        ValF::Sigma(ttag, (tn0, ti0, tty0, l_tty0), tg, tprops) => {
          if tag != *ttag {
            return fail(self, "obj / role mismatch");
          }
          let len_match = props.len() == tprops.len() + 1;
          let mut pi = props.into_iter();
          let (n0, e0) = pi.next().unwrap();
          if n0 != *tn0 {
            return fail(self, &format!("obj / name mismatch: {:?} ≟ {:?}", n0, tn0));
          }
          let c_e0 = self.check(*e0, &tty0)?;
          let e0 = self.eval0(&c_e0);
          self.varenvs.push(VarEnv::new());
          self.varenv().add(*ti0, (tty0.clone(), *l_tty0));
          let mut tg = tg.clone();
          tg.add_bind(*ti0, e0);
          let mut ps = Vec::new();
          let mut c_ps = Vec::new();
          for ((n, e), (tn, ti, ty, l_ty)) in pi.zip(tprops.iter()) {
            if n != *tn {
              return fail(self, &format!("obj / name mismatch: {:?} ≟ {:?}", n, tn));
            }
            // ty may depend on previous props
            let ty = self.eval(&tg, ty);
            let c_e = self.check(*e, &ty)?;
            let e = self.eval0(&c_e);
            self.varenv().add(*ti, (ty.clone(), *l_ty));
            tg.add_bind(*ti, e.clone());
            ps.push((*ti, e));
            c_ps.push((n, c_e));
          }
          self.varenvs.pop();
          if !len_match {
            // we defer this error to check the existing props as much as possible
            return fail(self, "obj / length mismatch");
          }
          Ok(Rc::new(CExpr(CExprF::Obj(tag, (n0, c_e0), c_ps))))
        }
        _ => fail(self, "obj / not Σ"),
      },
      _ => {
        let (c_e, ty) = self.synth(e)?;
        self.unify(&ty.0, check_ty)?;
        Ok(c_e)
      }
    }
  }

  fn check_ty(&mut self, e: Expr<P, S>) -> Result<(RE<P, S>, LevelId)> {
    let li = self.fresh_level();
    let e = self.check(e, &Rc::new(Val(ValF::Uni(li))))?;
    Ok((
      Rc::new(CExpr(CExprF::Ann(e, Rc::new(CExpr(CExprF::Uni(li)))))),
      li,
    ))
  }

  fn resolve_implicits(
    &mut self,
    c_f: RE<P, S>,
    fty: TyUni<P, S>,
  ) -> Result<(RE<P, S>, TyUni<P, S>)> {
    match &self.norm(&fty.0)?.0 {
      ValF::Pi(ftag, Vis::Implicit, i, ty, fg, bty, l_bty) => {
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
        self.resolve_implicits(c_fh, (bty, *l_bty))
      }
      _ => Ok((c_f, fty)),
    }
  }

  fn synth(&mut self, e: Expr<P, S>) -> Result<(RE<P, S>, TyUni<P, S>)> {
    let res = self.synth_internal(e);
    /* if let Ok((c_e, ty)) = &res {
      self.log(format!("SYNTH {}: {}", self.ppe(c_e), self.ppv(ty)));
    } */
    res
  }

  fn synth_internal(&mut self, e: Expr<P, S>) -> Result<(RE<P, S>, TyUni<P, S>)> {
    let ec = e.clone(); // TODO: redundant?
    let fail = |_this: &Checker<P, S>, msg: &str| -> Result<_> {
      Err(Error::Loc(format!(
        "Failed to synthesize type ({}): {:?}",
        msg, ec,
      )))
    };
    use ExprF::*;
    match e.0 {
      Hole => fail(self, "hole"),
      Bind(i) => match self.lookup_bind(i) {
        Some(ty) => Ok((Rc::new(CExpr(CExprF::Bind(i))), ty.clone())),
        None => unreachable!(),
      },
      Def(i) => match self.lookup_def(i).cloned() {
        Some((sol, _, (ty, l_ty))) => {
          let ls = self.fresh_levels(&sol);
          let ls_m = self.map_solution(&sol, &ls);
          let l_ty = ls_m.get(&l_ty).unwrap_or(&l_ty).clone();
          let mut subst = SubstEnv::from_levels(ls_m, self);
          let ty = subst.subst_v(&ty).into();
          Ok((Rc::new(CExpr(CExprF::Def(i, ls))), (ty, l_ty)))
        }
        None => Err(Error::Fail),
      },
      Ann(tm, ty) => {
        let (c_ty, l_ty) = self.check_ty(*ty)?;
        let ty = self.eval0(&c_ty);
        let c_tm = self.check(*tm, &ty)?;
        Ok((Rc::new(CExpr(CExprF::Ann(c_tm, c_ty))), (ty, l_ty)))
      }
      Uni => {
        let tml = self.fresh_level();
        let tyl = self.fresh_level();
        let unil = self.fresh_level();
        self.solve().add_constraint(tml, LevelRel::Lt, tyl);
        self.solve().add_constraint(tyl, LevelRel::Lt, unil);
        Ok((
          (Rc::new(CExpr(CExprF::Ann(
            Rc::new(CExpr(CExprF::Uni(tml))),
            Rc::new(CExpr(CExprF::Uni(tyl))),
          )))),
          (Rc::new(Val(ValF::Uni(tyl))), unil),
        ))
      }

      Let(defs, body) => {
        self.defenvs.push(DefEnv::new());
        let ds = self.defs(defs);
        let (body, (ty, l_ty)) = self.synth(*body)?;
        self.defenvs.pop();
        let mut def_map = HashMap::new();
        for (i, sol, e) in &ds {
          let ls = self.fresh_levels(&sol);
          let ls_m = self.map_solution(&sol, &ls);
          let mut subst = SubstEnv::from_levels(ls_m, self);
          let e = subst.subst_e(e).into();
          let v = self.eval0(&e);
          def_map.insert(*i, (sol.clone(), e.clone(), v));
        }
        let ty = SubstEnv::from_defs(def_map, self).subst_v(&ty).into();
        Ok((Rc::new(CExpr(CExprF::Let(ds, body))), (ty, l_ty)))
      }

      Pi(tag, vis, i, ty, bty) => {
        let li = self.fresh_level();
        let unil = self.fresh_level();
        self.solve().add_constraint(li, LevelRel::Lt, unil);
        let (c_ty, l_ty) = self.check_ty(*ty)?;
        self.solve().add_constraint(l_ty, LevelRel::Le, li);
        let ty = self.eval0(&c_ty);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i, (ty, l_ty));
        let (c_bty, l_bty) = self.check_ty(*bty)?;
        self.solve().add_constraint(l_bty, LevelRel::Le, li);
        self.varenvs.pop();
        Ok((
          Rc::new(CExpr(CExprF::Ann(
            Rc::new(CExpr(CExprF::Pi(tag, vis, i, c_ty, c_bty, l_bty))),
            Rc::new(CExpr(CExprF::Uni(li))),
          ))),
          (Rc::new(Val(ValF::Uni(li))), unil),
        ))
      }
      Lam(tag, vis, i, ty, body) => {
        let unil: LevelId = self.fresh_level();
        let (c_ty, l_ty) = self.check_ty(*ty)?;
        self.solve().add_constraint(l_ty, LevelRel::Le, unil);
        let ty = self.eval0(&c_ty);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i, (ty.clone(), l_ty));
        let (c_body, (bty, l_bty)) = self.synth(*body)?;
        self.solve().add_constraint(l_bty, LevelRel::Le, unil);
        let bty = self.quote(&bty.into());
        self.varenvs.pop();
        Ok((
          (Rc::new(CExpr(CExprF::Lam(
            tag.clone(),
            vis.clone(),
            i,
            c_ty,
            c_body,
          )))),
          (
            Rc::new(Val(ValF::Pi(tag, vis, i, ty, Env::new(), bty, l_bty))),
            unil,
          ),
        ))
      }
      App(tag, vis, f, x) => {
        let (c_f, fty) = self.synth(*f)?;
        let (c_f, fty) = match vis {
          Vis::Explicit => self.resolve_implicits(c_f, fty)?,
          Vis::Implicit => (c_f, fty),
        };
        match &self.norm(&fty.0)?.0 {
          ValF::Pi(ftag, fvis, i, ty, fg, bty, l_bty) => {
            if tag != *ftag {
              return fail(self, "app / role mismatch");
            }
            if vis != *fvis {
              return fail(self, "app / visibility mismatch");
            }
            let c_x = self.check(*x, &ty)?;
            let x = self.eval0(&c_x);
            let mut fg = fg.clone();
            fg.add_bind(*i, x);
            let bty = self.eval(&fg, bty);
            Ok((
              Rc::new(CExpr(CExprF::App(tag, vis, c_f, c_x))),
              (bty, *l_bty),
            ))
          }
          _ => fail(self, "app / not a function"),
        }
      }

      Sigma(tag, props) => {
        let li = self.fresh_level();
        let unil = self.fresh_level();
        self.solve().add_constraint(li, LevelRel::Lt, unil);
        if props.is_empty() {
          return Ok((
            Rc::new(CExpr(CExprF::Ann(
              Rc::new(CExpr(CExprF::Sigma0(tag))),
              Rc::new(CExpr(CExprF::Uni(li))),
            ))),
            (Rc::new(Val(ValF::Uni(li))), unil),
          ));
        }
        let mut pi = props.into_iter();
        let (n0, i0, ty0) = pi.next().unwrap();
        let (c_ty0, l_ty0) = self.check_ty(*ty0)?;
        self.solve().add_constraint(l_ty0, LevelRel::Le, li);
        let ty0 = self.eval0(&c_ty0);
        self.varenvs.push(VarEnv::new());
        self.varenv().add(i0, (ty0, l_ty0));
        let mut c_props = Vec::new();
        for (name, i, ty) in pi {
          let (c_ty, l_ty) = self.check_ty(*ty)?;
          self.solve().add_constraint(l_ty, LevelRel::Le, li);
          let ty = self.eval0(&c_ty);
          self.varenv().add(i, (ty, l_ty));
          c_props.push((name, i, c_ty, l_ty));
        }
        self.varenvs.pop();
        Ok((
          Rc::new(CExpr(CExprF::Ann(
            Rc::new(CExpr(CExprF::Sigma(tag, (n0, i0, c_ty0, l_ty0), c_props))),
            Rc::new(CExpr(CExprF::Uni(li))),
          ))),
          (Rc::new(Val(ValF::Uni(li))), unil),
        ))
      }
      Obj(tag, props) => {
        let unil = self.fresh_level();
        if props.is_empty() {
          return Ok((
            Rc::new(CExpr(CExprF::Obj0(tag.clone()))),
            (Rc::new(Val(ValF::Sigma0(tag))), unil),
          ));
        }
        let mut pi = props.into_iter();
        let (n0, e0) = pi.next().unwrap();
        let i0 = self.fresh_id_new();
        let (c_e0, (ty0, l_ty0)) = self.synth(*e0)?;
        self.solve().add_constraint(l_ty0, LevelRel::Le, unil);
        let mut tys = Vec::new();
        let mut c_props = Vec::new();
        for (name, e) in pi {
          let (c_e, (ty, l_ty)) = self.synth(*e)?;
          self.solve().add_constraint(l_ty, LevelRel::Le, unil);
          let ty = self.quote(&ty.into());
          let i = self.fresh_id_new();
          tys.push((name.clone(), i, ty, l_ty));
          c_props.push((name, c_e));
        }
        Ok((
          Rc::new(CExpr(CExprF::Obj(tag.clone(), (n0, c_e0), c_props))),
          (
            Rc::new(Val(ValF::Sigma(
              tag,
              (n0, i0, ty0.into(), l_ty0),
              Env::new(),
              tys,
            ))),
            unil,
          ),
        ))
      }
      Prop(tag, e, name) => {
        let (c_e, ty) = self.synth(*e)?;
        match &self.norm(&ty.0)?.0 {
          ValF::Sigma(etag, (n0, i0, ty0, l_ty0), eg, props) => {
            if tag != *etag {
              return fail(self, "prop / role mismatch");
            }
            if name == *n0 {
              return Ok((
                Rc::new(CExpr(CExprF::Prop(tag, c_e, name))),
                (ty0.clone(), *l_ty0),
              ));
            }
            for (index, (n, _, ty, l_ty)) in props.iter().enumerate() {
              if name == *n {
                let e = self.eval0(&c_e);
                let mut eg = eg.clone();
                // ty may depend on previous props
                let p0 = self.prop(tag.clone(), e.clone(), *n0); // e.n0
                eg.add_bind(*i0, p0);
                for (n, i, _, _) in props.iter().take(index) {
                  let p = self.prop(tag.clone(), e.clone(), *n); // e.n
                  eg.add_bind(*i, p);
                }
                let ty = self.eval(&eg, &ty);
                return Ok((Rc::new(CExpr(CExprF::Prop(tag, c_e, name))), (ty, *l_ty)));
              }
            }
            fail(self, &format!("prop / name not found: {:?}", name))
          }
          _ => fail(self, "prop / not an object"),
        }
      }
    }
  }

  fn resolve(
    &mut self,
    solve: Solve<P, S>,
  ) -> Result<(HashMap<HoleId, (RE<P, S>, RV<P, S>)>, Solution)> {
    eprintln!("[Resolve]");

    let hole_map = {
      /* eprintln!("- holes:");
      for (i, ty) in &solve.holes {
        eprintln!("  - {:?}: {}", i, self.ppv(ty));
      }
      eprintln!("- assignments:");
      for (i, tm) in &solve.hole_assign {
        eprintln!("  - {:?} = {}", i, self.ppe(tm));
      } */

      for (j, _) in &solve.holes {
        if !solve.hole_assign.contains_key(j) {
          return Err(Error::Loc(format!("Unresolved hole: {:?}", j)));
        }
      }
      // TODO: resolve hole-in-hole constraints at this point
      let mut hole_map = HashMap::new();
      for (i, e) in solve.hole_assign {
        let v = self.eval0(&e);
        hole_map.insert(i, (e, v));
      }
      hole_map
    };

    let level_solution = {
      let constraints = solve.constraints_accum.unwrap();
      eprintln!("- levels: {:?}", solve.levels);
      eprintln!("- constraints:");
      for (i1, rel, i2) in &constraints {
        match rel {
          LevelRel::Eq => eprintln!("  - {:?} = {:?}", i1, i2),
          LevelRel::Le => eprintln!("  - {:?} ≤ {:?}", i1, i2),
          LevelRel::Lt => eprintln!("  - {:?} < {:?}", i1, i2),
        }
      }
      // TODO: extract essential levels
      let scope = solve.levels;
      let scope = HashSet::new();
      // ^TODO: pickup all constraints in solve_levels
      let level_solution = solve_levels(&constraints, &scope)
        .map_err(|e| Error::Loc(format!("Failed to solve level constraints: {}", e)))?;
      level_solution
    };

    Ok((hole_map, level_solution))
  }

  fn def(&mut self, e: Box<Expr<P, S>>) -> Result<(Solution, RE<P, S>, TyUni<P, S>)> {
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
    {
      // accumulate constraints
      for i in 0..self.solves.len() {
        if self.solves[i].constraints_accum.is_none() {
          let parent = if i != 0 {
            self
              .solves
              .get(i - 1)
              .map(|s| s.constraints_accum.clone().unwrap())
          } else {
            None
          };
          let s = &mut self.solves[i];
          s.constraints_accum = Some(match parent {
            Some(mut ps) => {
              ps.extend(s.level_constraints.clone());
              ps
            }
            None => s.level_constraints.clone(),
          });
        }
      }
    }
    let solve = self.solves.pop().unwrap();
    let (ce, (ty, l_ty)) = res?;
    let (hole_map, level_solution) = self.resolve(solve)?;

    // eprintln!("- From");
    // eprintln!("  - {}: {}", self.ppe(&ce), self.ppv(&ty));
    let mut senv = SubstEnv::from_holes(hole_map.clone(), self);
    let ce = senv.subst_e(&ce).into();
    let ty = senv.subst_v(&ty).into();
    // eprintln!("- To");
    // eprintln!("  - {}: {}", self.ppe(&ce), self.ppv(&ty));
    for i in hole_map.keys() {
      if contains_hole_e(i, &ce) {
        return Err(Error::Loc(format!(
          "Hole {:?} remains unresolved in {}",
          i,
          self.ppe(&ce)
        )));
      }
    }
    // TODO: scope check?

    Ok((level_solution, ce, (ty, l_ty)))
  }

  fn defs(&mut self, defs: Vec<(DefId, Box<Expr<P, S>>)>) -> Vec<(DefId, Solution, RE<P, S>)> {
    let mut def_terms = Vec::new();
    for (id, expr) in defs {
      eprintln!("- Def[ {} ]", self.def_symbols[&id]);
      let res = self.def(expr);
      match &res {
        Ok((_, _, ty)) => eprintln!(
          "[{}] {}: {}: 𝒰{:?}",
          "o".green(),
          self.def_symbols[&id],
          self.ppv(&ty.0),
          ty.1
        ),
        Err(Error::Fail) => eprintln!(
          "[{}] {}",
          "_".yellow(),
          &format!("{} (failed def)", self.def_symbols[&id]).black()
        ),
        Err(Error::Loc(e)) => eprintln!("[{}] {}: {}", "x".red(), self.def_symbols[&id], e),
      }
      match res {
        Ok((sol, c_expr, ty)) => {
          eprintln!(
            "{}",
            format!("    {} = {}", self.def_symbols[&id], self.ppe(&c_expr)).black()
          );
          // let tm = self.eval0(&c_expr);
          // let dtm = self.deep_norm(&tm).unwrap();
          // eprintln!(
          //   "{}",
          //   format!("    {} = {}", self.def_symbols[&id], self.ppv(&dtm)).black()
          // );
          self.defenv().add(id, (sol.clone(), c_expr.clone(), ty));
          def_terms.push((id, sol, c_expr));
        }
        Err(Error::Fail) => {}
        Err(Error::Loc(e)) => {
          self
            .errors
            .push(format!("def {}: {}", self.def_symbols[&id], e));
        }
      }
    }
    def_terms
  }
}

pub fn check<P, S>(p: Program<P, S>) -> std::result::Result<(), Vec<String>>
where
  P: Tag,
  S: Tag,
{
  let mut c = Checker::new(p.def_symbols, p.bind_symbols, p.name_symbols);
  c.defenvs.push(DefEnv::new());
  c.defs(p.defs);
  c.defenvs.pop();
  if c.errors.is_empty() {
    Ok(())
  } else {
    Err(c.errors)
  }
}
