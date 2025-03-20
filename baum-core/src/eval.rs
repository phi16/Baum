use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::rc::Rc;

// Checked Expr
#[derive(Debug, Clone)]
pub struct CE<PTag, STag>(pub ExprF<PTag, STag, HoleId, Box<CE<PTag, STag>>>);

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

pub struct Eval<P, S> {
  _p: std::marker::PhantomData<P>,
  _s: std::marker::PhantomData<S>,
}

impl<P, S> Eval<P, S>
where
  P: Tag,
  S: Tag,
{
  pub fn new() -> Self {
    Eval {
      _p: std::marker::PhantomData,
      _s: std::marker::PhantomData,
    }
  }

  fn subst_ks<'a>(&self, i: BindId, e: &RV<P, S>, ks: &'a Conts<P, S>) -> Subst<'a, Conts<P, S>> {
    let mut changed = false;
    let mut tks = Vec::new();
    for k in ks {
      match k {
        ContF::App(tag, vis, x) => {
          let x_s = self.subst(i, e, x);
          if x_s.is_changed() {
            changed = true;
          }
          tks.push(ContF::App(tag.clone(), vis.clone(), x_s.into()));
        }
        ContF::Prop(tag, name) => {
          tks.push(ContF::Prop(tag.clone(), *name));
        }
      }
    }
    if changed {
      Subst::Changed(tks)
    } else {
      Subst::Unchanged(ks)
    }
  }

  pub fn subst<'a>(&self, i: BindId, e: &RV<P, S>, v: &'a RV<P, S>) -> Subst<'a, RV<P, S>> {
    // TODO: escaping (e may contain "bounded" variables...)
    match &v.0 {
      ValF::Neu(ti, ks) => {
        if IdF::Bind(i) == *ti {
          let mut te = e.clone();
          for k in ks {
            match k {
              ContF::App(tag, vis, x) => {
                let x = self.subst(i, e, x).into();
                te = self.app(tag.clone(), vis.clone(), te, x);
              }
              ContF::Prop(tag, name) => {
                te = self.prop(tag.clone(), te, *name);
              }
            }
          }
          Subst::Changed(te.clone())
        } else {
          let ks_s = self.subst_ks(i, e, ks);
          if ks_s.is_changed() {
            Subst::Changed(Rc::new(Val(ValF::Neu(ti.clone(), ks_s.into()))))
          } else {
            Subst::Unchanged(v)
          }
        }
      }
      ValF::Pi(tag, vis, ti, ty, body) => {
        let ty_s = self.subst(i, e, ty);
        let body_s = if i == *ti {
          Subst::Unchanged(body)
        } else {
          self.subst(i, e, body)
        };
        if ty_s.is_changed() || body_s.is_changed() {
          Subst::Changed(Rc::new(Val(ValF::Pi(
            tag.clone(),
            vis.clone(),
            *ti,
            ty_s.into(),
            body_s.into(),
          ))))
        } else {
          Subst::Unchanged(v)
        }
      }
      ValF::Lam(tag, vis, ti, ty, body) => {
        let ty_s = self.subst(i, e, ty);
        let body_s = if i == *ti {
          Subst::Unchanged(body)
        } else {
          self.subst(i, e, body)
        };
        if ty_s.is_changed() || body_s.is_changed() {
          Subst::Changed(Rc::new(Val(ValF::Lam(
            tag.clone(),
            vis.clone(),
            *ti,
            ty_s.into(),
            body_s.into(),
          ))))
        } else {
          Subst::Unchanged(v)
        }
      }

      ValF::Sigma(tag, props) => {
        let mut changed = false;
        let mut hidden = false;
        let mut ps: Vec<(NameId, BindId, Rc<Val<P, S>>)> = Vec::new();
        for (name, ti, ty) in props {
          if hidden {
            ps.push((name.clone(), ti.clone(), ty.clone()));
            continue;
          } else {
            let ty_s = self.subst(i, e, ty);
            if ty_s.is_changed() {
              changed = true;
            }
            ps.push((name.clone(), ti.clone(), ty_s.into()));
            if i == *ti {
              hidden = true;
            }
          }
        }
        if changed {
          Subst::Changed(Rc::new(Val(ValF::Sigma(tag.clone(), ps))))
        } else {
          Subst::Unchanged(v)
        }
      }
      ValF::Obj(tag, props) => {
        let mut changed = false;
        let mut ps = Vec::new();
        for (name, el) in props {
          let el_s = self.subst(i, e, el);
          if el_s.is_changed() {
            changed = true;
          }
          ps.push((name.clone(), el_s.into()));
        }
        if changed {
          Subst::Changed(Rc::new(Val(ValF::Obj(tag.clone(), ps))))
        } else {
          Subst::Unchanged(v)
        }
      }
      _ => Subst::Unchanged(v),
    }
  }

  pub fn app(&self, tag: P, vis: Vis, f: Term<P, S>, x: Term<P, S>) -> Term<P, S> {
    match &f.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::App(tag, vis, x));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Lam(ftag, fvis, i, _, body) => {
        assert_eq!(tag, *ftag);
        assert_eq!(vis, *fvis);
        let g = vec![(*i, x)].into_iter().collect();
        Rc::new(Val(ValF::Cl(g, body.clone())))
      }
      _ => unreachable!(),
    }
  }

  pub fn prop(&self, tag: S, e: Term<P, S>, name: NameId) -> Term<P, S> {
    match &e.0 {
      ValF::Neu(i, ks) => {
        let mut ks = ks.clone();
        ks.push(ContF::Prop(tag, name));
        Rc::new(Val(ValF::Neu(i.clone(), ks)))
      }
      ValF::Obj(otag, props) => {
        assert_eq!(tag, *otag);
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

  pub fn eval(&self, e: &CE<P, S>) -> Term<P, S> {
    // Note: e may contain unresolved bindings
    use ExprF::*;
    let v = match &e.0 {
      Hole(h) => ValF::Neu(IdF::Hole(*h), Vec::new()),
      Bind(i) => ValF::Neu(IdF::Bind(*i), Vec::new()),
      Def(i) => ValF::Neu(IdF::Def(*i), Vec::new()),
      Ann(t, _) => return self.eval(t),
      Uni => ValF::Uni,
      Let(defs, body) => {
        unimplemented!()
      }

      Pi(tag, vis, i, ty, body) => {
        let ty = self.eval(ty);
        let body = self.eval(body);
        ValF::Pi(tag.clone(), vis.clone(), *i, ty, body)
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty);
        let body = self.eval(body);
        ValF::Lam(tag.clone(), vis.clone(), *i, ty, body)
      }
      App(tag, vis, f, x) => {
        let f = self.eval(f);
        let x = self.eval(x);
        return self.app(tag.clone(), vis.clone(), f, x);
      }

      Sigma(tag, props) => {
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let ty = self.eval(ty);
          ps.push((*name, *i, ty));
        }
        ValF::Sigma(tag.clone(), ps)
      }
      Obj(tag, props) => {
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.eval(e);
          ps.push((*name, e));
        }
        ValF::Obj(tag.clone(), ps)
      }
      Prop(tag, e, name) => {
        let e = self.eval(e);
        return self.prop(tag.clone(), e, *name);
      }
    };
    Rc::new(Val(v))
  }
}
