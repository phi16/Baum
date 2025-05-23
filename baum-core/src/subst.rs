use crate::check::Checker;
use crate::types::common::*;
use crate::types::level::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

pub enum Subst<'a, T> {
  Unchanged(&'a T),
  Changed(T),
}

impl<'a, T: Clone> Subst<'a, T> {
  pub fn is_changed(&self) -> bool {
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

pub struct SubstEnv<'b, T, P, S> {
  holes: HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
  defs: HashMap<DefId, (Solution, RE<P, S>, RV<P, S>)>,
  levels: HashMap<LevelId, Level>,
  checker: &'b mut Checker<T, P, S>,
}

impl<'b, T: Clone, P: Tag, S: Tag> SubstEnv<'b, T, P, S> {
  pub fn from_holes(
    holes: HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
    checker: &'b mut Checker<T, P, S>,
  ) -> Self {
    SubstEnv {
      holes,
      defs: HashMap::new(),
      levels: HashMap::new(),
      checker,
    }
  }

  pub fn from_defs(
    defs: HashMap<DefId, (Solution, RE<P, S>, RV<P, S>)>,
    checker: &'b mut Checker<T, P, S>,
  ) -> Self {
    SubstEnv {
      holes: HashMap::new(),
      defs,
      levels: HashMap::new(),
      checker,
    }
  }

  pub fn from_levels(levels: HashMap<LevelId, Level>, checker: &'b mut Checker<T, P, S>) -> Self {
    SubstEnv {
      holes: HashMap::new(),
      defs: HashMap::new(),
      levels,
      checker,
    }
  }

  fn extend(&mut self, ls_m: HashMap<LevelId, Level>) -> HashMap<LevelId, Level> {
    let old = self.levels.clone();
    for (i, js) in &ls_m {
      if self.levels.contains_key(i) {
        panic!("Level already exists in subst env: {:?}", i);
      }
      used_levels(js).iter().for_each(|j| {
        if self.levels.contains_key(j) {
          panic!("Level already exists in subst env: {:?}", j);
        }
      });
    }
    for (i, js) in &self.levels {
      if ls_m.contains_key(i) {
        panic!("Level already exists in subst env: {:?}", i);
      }
      used_levels(js).iter().for_each(|j| {
        if ls_m.contains_key(j) {
          panic!("Level already exists in subst env: {:?}", j);
        }
      });
    }
    self.levels.extend(ls_m);
    old
  }
  fn revert(&mut self, old: HashMap<LevelId, Level>) {
    self.levels = old;
  }

  fn subst_l<'a>(&mut self, l: &'a Level) -> Subst<'a, Level> {
    let unchanged = Subst::Unchanged(l);
    match l {
      Level::Id(i) => match self.levels.get(i).cloned() {
        Some(l) => Subst::Changed(l),
        None => unchanged,
      },
      Level::Max(is) => {
        let mut changed = false;
        let ris = is
          .iter()
          .flat_map(|(i, o)| match self.levels.get(i) {
            Some(l) => {
              changed = true;
              l.clone()
                .into()
                .iter()
                .map(|(j, jo)| (*j, *o + *jo))
                .collect::<Vec<_>>()
            }
            None => vec![(*i, *o)],
          })
          .collect::<Vec<_>>();
        if changed {
          Subst::Changed(Level::Max(ris))
        } else {
          unchanged
        }
      }
    }
  }

  fn subst_ls<'a>(&mut self, ls: &'a Vec<Level>) -> Subst<'a, Vec<Level>> {
    let mut changed = false;
    let mut rls = Vec::new();
    for l in ls {
      let l = self.subst_l(l);
      if l.is_changed() {
        changed = true;
      }
      rls.push(l.into());
    }
    if changed {
      Subst::Changed(rls)
    } else {
      Subst::Unchanged(ls)
    }
  }

  pub fn subst_e<'a>(&mut self, e: &'a RE<P, S>) -> Subst<'a, RE<P, S>> {
    let unchanged = Subst::Unchanged(e);
    match &e.0 {
      CExprF::Hole(i) => match self.holes.get(&i).cloned() {
        Some((e, _)) => Subst::Changed(self.subst_e(&e).into()),
        None => unchanged,
      },
      CExprF::Bind(_) => unchanged,
      CExprF::Def(i, ls) => match self.defs.get(&i).cloned() {
        Some((sol, e, _)) => {
          let ls = self.subst_ls(ls).into();
          let ls_m = self.checker.map_solution(&sol, &ls);
          let lm = self.extend(ls_m);
          let s = Subst::Changed(self.subst_e(&e).into());
          self.revert(lm);
          s
        }
        None => {
          let ls = self.subst_ls(ls);
          if ls.is_changed() {
            Subst::Changed(Rc::new(CExpr(CExprF::Def(*i, ls.into()))))
          } else {
            unchanged
          }
        }
      },
      CExprF::Ann(e, ty) => {
        let e = self.subst_e(e);
        let ty = self.subst_e(ty);
        if e.is_changed() || ty.is_changed() {
          Subst::Changed(Rc::new(CExpr(CExprF::Ann(e.into(), ty.into()))))
        } else {
          unchanged
        }
      }
      CExprF::Uni(l) => {
        let l = self.subst_l(l);
        if l.is_changed() {
          Subst::Changed(Rc::new(CExpr(CExprF::Uni(l.into()))))
        } else {
          unchanged
        }
      }
      CExprF::Prim(_) => unchanged,
      CExprF::Lit(_) => unchanged,
      CExprF::Let(defs, body) => {
        let mut changed = false;
        let mut rdefs = Vec::new();
        for (def, sol, e) in defs {
          let e = self.subst_e(e);
          if e.is_changed() {
            changed = true;
          }
          rdefs.push((*def, sol.clone(), e.into()));
        }
        let body = self.subst_e(body);
        if changed || body.is_changed() {
          Subst::Changed(Rc::new(CExpr(CExprF::Let(rdefs, body.into()))))
        } else {
          unchanged
        }
      }
      CExprF::Pi(tag, vis, i, ty, bty) => {
        let ty = self.subst_e(ty);
        let bty = self.subst_e(bty);
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
        let ty = self.subst_e(ty);
        let body = self.subst_e(body);
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
        let f = self.subst_e(f);
        let x = self.subst_e(x);
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
        let ty0 = self.subst_e(ty0);
        let mut changed = false;
        let mut rprops = Vec::new();
        for (n, i, ty) in props {
          let ty = self.subst_e(ty);
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
        let e0 = self.subst_e(e0);
        let mut changed = false;
        let mut rprops = Vec::new();
        for (n, e) in props {
          let e = self.subst_e(e);
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
        let e = self.subst_e(e);
        if e.is_changed() {
          Subst::Changed(Rc::new(CExpr(CExprF::Prop(tag.clone(), e.into(), *name))))
        } else {
          unchanged
        }
      }
    }
  }

  pub fn subst_v<'a>(&mut self, v: &'a RV<P, S>) -> Subst<'a, RV<P, S>> {
    let unchanged = Subst::Unchanged(v);
    match &v.0 {
      ValF::Hole(i) => match self.holes.get(&i).cloned() {
        Some((_, v)) => Subst::Changed(self.subst_v(&v).into()),
        None => unchanged,
      },
      ValF::Neu(i, ks) => {
        let mut changed = false;
        let mut rks = Vec::new();
        for k in ks {
          match k {
            ContF::App(tag, vis, x) => {
              let x = self.subst_v(x);
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
      ValF::Lazy(i, ls, ks) => match self.defs.get(&i).cloned() {
        Some((sol, _, v)) => {
          let ls_m = self.checker.map_solution(&sol, &ls);
          let lm = self.extend(ls_m);
          let mut v = self.subst_v(&v).into();
          for k in ks {
            match k {
              ContF::App(tag, vis, x) => {
                let x = self.subst_v(x).into();
                v = self.checker.app(tag.clone(), vis.clone(), v, x);
              }
              ContF::Prop(tag, name) => {
                v = self.checker.prop(tag.clone(), v, *name);
              }
            }
          }
          self.revert(lm);
          return Subst::Changed(v);
        }
        None => {
          let mut changed = false;
          let mut rks = Vec::new();
          for k in ks {
            match k {
              ContF::App(tag, vis, x) => {
                let x = self.subst_v(x);
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
          let mut rls = Vec::new();
          for l in ls {
            let l = self.subst_l(l);
            if l.is_changed() {
              changed = true;
            }
            rls.push(l.into());
          }
          if changed {
            Subst::Changed(Rc::new(Val(ValF::Lazy(*i, rls, rks))))
          } else {
            unchanged
          }
        }
      },
      ValF::Prim(s, ks) => {
        let mut changed = false;
        let mut rks = Vec::new();
        for k in ks {
          match k {
            ContF::App(tag, vis, x) => {
              let x = self.subst_v(x);
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
          Subst::Changed(Rc::new(Val(ValF::Prim(s.clone(), rks))))
        } else {
          unchanged
        }
      }
      ValF::Lit(_) => unchanged,
      ValF::Uni(l) => {
        let l = self.subst_l(l);
        if l.is_changed() {
          Subst::Changed(Rc::new(Val(ValF::Uni(l.into()))))
        } else {
          unchanged
        }
      }

      ValF::Pi(tag, vis, i, ty, g, bty) => {
        let ty = self.subst_v(ty);
        let g = self.subst_g(g);
        let bty = self.subst_e(bty);
        if ty.is_changed() || g.is_changed() || bty.is_changed() {
          Subst::Changed(Rc::new(Val(ValF::Pi(
            tag.clone(),
            vis.clone(),
            *i,
            ty.into(),
            g.into(),
            bty.into(),
          ))))
        } else {
          unchanged
        }
      }
      ValF::Lam(tag, vis, i, ty, g, body) => {
        let ty = self.subst_v(ty);
        let g = self.subst_g(g);
        let body = self.subst_e(body);
        if ty.is_changed() || g.is_changed() || body.is_changed() {
          Subst::Changed(Rc::new(Val(ValF::Lam(
            tag.clone(),
            vis.clone(),
            *i,
            ty.into(),
            g.into(),
            body.into(),
          ))))
        } else {
          unchanged
        }
      }
      ValF::Sigma0(_) => unchanged,
      ValF::Obj0(_) => unchanged,
      ValF::Sigma(tag, (n0, i0, ty0), g, props) => {
        let ty0 = self.subst_v(ty0);
        let g = self.subst_g(g);
        let mut changed = false;
        let mut rprops = Vec::new();
        for (n, i, ty) in props {
          let ty = self.subst_e(ty);
          if ty.is_changed() {
            changed = true;
          }
          rprops.push((*n, *i, ty.into()));
        }
        if ty0.is_changed() || g.is_changed() || changed {
          Subst::Changed(Rc::new(Val(ValF::Sigma(
            tag.clone(),
            (*n0, *i0, ty0.into()),
            g.into(),
            rprops,
          ))))
        } else {
          unchanged
        }
      }
      ValF::Obj(tag, (n0, e0), props) => {
        let e0 = self.subst_v(e0);
        let mut changed = false;
        let mut rprops = Vec::new();
        for (n, e) in props {
          let e = self.subst_v(e);
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

  fn subst_g<'a>(&mut self, g: &'a Env<P, S>) -> Subst<'a, Env<P, S>> {
    let mut changed = false;
    let mut rlookup = HashMap::new();
    for (i, v) in &g.lookup {
      let v = self.subst_v(v);
      if v.is_changed() {
        changed = true;
      }
      rlookup.insert(*i, v.into());
    }
    let mut rdefine = HashMap::new();
    for (i, v) in &g.define {
      let v = self.subst_v(v);
      if v.is_changed() {
        changed = true;
      }
      rdefine.insert(*i, v.into());
    }
    if changed {
      Subst::Changed(Env {
        lookup: rlookup,
        define: rdefine,
      })
    } else {
      Subst::Unchanged(g)
    }
  }
}
