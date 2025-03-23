use crate::check::Checker;
use crate::types::common::*;
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

pub struct SubstEnv<'b, P, S> {
  holes: HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
  defs: HashMap<DefId, (RE<P, S>, RV<P, S>)>,
  checker: &'b mut Checker<P, S>,
}

impl<'b, P: Tag, S: Tag> SubstEnv<'b, P, S> {
  pub fn from_holes(
    holes: HashMap<HoleId, (RE<P, S>, RV<P, S>)>,
    checker: &'b mut Checker<P, S>,
  ) -> Self {
    SubstEnv {
      holes,
      defs: HashMap::new(),
      checker,
    }
  }

  pub fn from_defs(
    defs: HashMap<DefId, (RE<P, S>, RV<P, S>)>,
    checker: &'b mut Checker<P, S>,
  ) -> Self {
    SubstEnv {
      holes: HashMap::new(),
      defs,
      checker,
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
      CExprF::Def(i) => match self.defs.get(&i).cloned() {
        Some((e, _)) => Subst::Changed(self.subst_e(&e).into()),
        None => unchanged,
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
      CExprF::Uni => unchanged,
      CExprF::Let(defs, body) => {
        let mut changed = false;
        let mut rdefs = Vec::new();
        for (def, e) in defs {
          let e = self.subst_e(e);
          if e.is_changed() {
            changed = true;
          }
          rdefs.push((*def, e.into()));
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
      ValF::Lazy(i, ks) => match self.defs.get(&i).cloned() {
        Some((_, v)) => {
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
          if changed {
            Subst::Changed(Rc::new(Val(ValF::Lazy(*i, rks))))
          } else {
            unchanged
          }
        }
      },
      ValF::Uni => unchanged,
      ValF::Pi(tag, vis, i, ty, g, bty) => {
        let ty = self.subst_v(ty);
        let bty = self.subst_e(bty);
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
        let ty = self.subst_v(ty);
        let body = self.subst_e(body);
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
      ValF::Sigma0(_) => unchanged,
      ValF::Obj0(_) => unchanged,
      ValF::Sigma(tag, (n0, i0, ty0), g, props) => {
        let ty0 = self.subst_v(ty0);
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
}
