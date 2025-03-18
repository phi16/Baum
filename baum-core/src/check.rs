use crate::pretty::{pretty_expr, pretty_val};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

type RV<P, S> = Rc<Val<P, S>>;
type Term<P, S> = RV<P, S>;
type Type<P, S> = RV<P, S>;

enum Subst<'a, T> {
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
  fn into(self) -> T {
    match self {
      Subst::Unchanged(t) => t.clone(),
      Subst::Changed(t) => t,
    }
  }
}

pub trait Tag: Clone + std::fmt::Debug + PartialEq + Eq {}

struct Env<P, S> {
  lookup: HashMap<BindId, Type<P, S>>,
  define: HashMap<DefId, Type<P, S>>,
}

impl<P, S> Env<P, S> {
  fn new() -> Self {
    Env {
      lookup: HashMap::new(),
      define: HashMap::new(),
    }
  }

  fn add(&mut self, bind: BindId, ty: Type<P, S>) {
    self.lookup.insert(bind, ty);
  }

  fn def(&mut self, def: DefId, ty: Type<P, S>) {
    self.define.insert(def, ty);
  }
}

struct Solve<P, S> {
  holes: HashMap<HoleId, Type<P, S>>,
  constraints: HashMap<HoleId, RV<P, S>>,
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

  fn add_constraint(&mut self, hole: HoleId, v: RV<P, S>) {
    self.constraints.insert(hole, v);
  }
}

struct Checker<P, S> {
  def_symbols: HashMap<DefId, String>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  envs: Vec<Env<P, S>>,
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
      envs: vec![Env::new()],
      solves: vec![Solve::new()],
      next_bind_id,
      next_hole_id: 0,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, msg: &str) {
    self.errors.push(msg.to_string());
  }

  fn here(&mut self) -> &mut Env<P, S> {
    self.envs.last_mut().unwrap()
  }

  fn solve(&mut self) -> &mut Solve<P, S> {
    self.solves.last_mut().unwrap()
  }

  fn fresh_id(&mut self) -> BindId {
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

  fn lookup_def(&self, def: DefId) -> Option<&Type<P, S>> {
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

  fn resolve_let(&mut self, defs: Vec<(DefId, Rc<Term<P, S>>)>, e: Term<P, S>) -> Term<P, S> {
    eprintln!(
      "let_binding: escaping {:?} from {}",
      defs.iter().map(|(k, _)| *k).collect::<Vec<_>>(),
      self.ppv(&e)
    );
    unimplemented!()
  }

  fn unify(&mut self, v1: &RV<P, S>, v2: &RV<P, S>) -> Result<()> {
    if Rc::ptr_eq(v1, v2) {
      return Ok(());
    }
    eprintln!("- unify: {} ≟ {}", self.ppv(v1), self.ppv(v2));
    match (&v1.0, &v2.0) {
      (ValF::Fail, _) => Err("unify: fail".to_string()),
      (_, ValF::Fail) => Err("unify: fail".to_string()),
      (ValF::Hole(h1), ValF::Hole(h2)) if h1.0 == h2.0 => Ok(()),
      (ValF::Hole(h1), _) => {
        if let Some(v1) = self.lookup_constraint(*h1) {
          self.unify(&v1.clone(), v2)
        } else {
          self.solve().add_constraint(*h1, v2.clone());
          Ok(())
        }
      }
      (_, ValF::Hole(h2)) => {
        if let Some(v2) = self.lookup_constraint(*h2) {
          self.unify(v1, &v2.clone())
        } else {
          self.solve().add_constraint(*h2, v1.clone());
          Ok(())
        }
      }
      (ValF::Bind(i1), ValF::Bind(i2)) => {
        if i1 == i2 {
          Ok(())
        } else {
          Err("unify: bind".to_string())
        }
      }
      (ValF::Uni, ValF::Uni) => Ok(()),
      _ => Err("unify: unimplemented".to_string()),
    }
  }

  fn subst<'a>(&mut self, i: BindId, e: &Term<P, S>, v: &'a RV<P, S>) -> Subst<'a, RV<P, S>> {
    // TODO: escaping
    eprintln!("- subst: {:?} with {} in {}", i, self.ppv(e), self.ppv(v));
    let u = Subst::Unchanged(v);
    match &v.0 {
      ValF::Bind(j) => {
        if i == *j {
          // TODO: e may contain "bounded" variables...
          Subst::Changed(e.clone())
        } else {
          u
        }
      }
      ValF::Def(di) => unimplemented!(),

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
      ValF::App(tag, vis, ti, x) => {
        unimplemented!()
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
      ValF::Prop(tag, ti, name) => {
        unimplemented!()
      }
      _ => Subst::Unchanged(v),
    }
  }

  fn prop(&mut self, tag: &S, e: &Term<P, S>, name: &NameId) -> Term<P, S> {
    match &e.0 {
      ValF::Bind(i) => Rc::new(Val(ValF::Prop(tag.clone(), *i, *name))),
      ValF::Def(i) => unimplemented!(),
      ValF::Obj(otag, props) => {
        assert_eq!(tag, otag);
        for (n, e) in props {
          if n == name {
            return e.clone();
          }
        }
        unreachable!()
      }
      _ => unreachable!(),
    }
  }

  fn eval(&mut self, e: &Expr<P, S>) -> Result<Term<P, S>> {
    // Note: e may contain unresolved bindings
    use ExprF::*;
    let v = match &e.0 {
      Hole => {
        let htm = self.fresh_hole();
        let hty = self.fresh_hole();
        self.solve().add_hole(hty, Rc::new(Val(ValF::Uni)));
        self.solve().add_hole(htm, Rc::new(Val(ValF::Hole(hty))));
        ValF::Hole(htm)
      }
      Bind(i) => ValF::Bind(*i),
      Def(i) => ValF::Def(*i),
      Ann(t, _) => return self.eval(t),
      Uni => ValF::Uni,
      Let(defs, body) => {
        self.envs.push(Env::new());
        let defs = self.defs(defs);
        let body = self.eval(body)?;
        self.envs.pop();
        return Ok(self.resolve_let(defs, body));
      }

      Pi(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        let body = self.eval(body)?;
        ValF::Pi(tag.clone(), vis.clone(), i.clone(), ty, body)
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        let body = self.eval(body)?;
        ValF::Lam(tag.clone(), vis.clone(), *i, ty, body)
      }
      App(tag, vis, f, x) => {
        let f = self.eval(f)?;
        let x = self.eval(x)?;
        match &f.0 {
          ValF::Bind(i) => ValF::App(tag.clone(), vis.clone(), *i, x),
          ValF::Def(i) => unimplemented!(),
          ValF::Lam(ftag, fvis, i, ty, body) => {
            if tag != ftag || vis != fvis {
              return Err("eval: app".to_string());
            }
            let body = self.subst(*i, &x, &body).into();
            ValF::Lam(tag.clone(), vis.clone(), *i, ty.clone(), body)
          }
          _ => return Err("eval: app".to_string()),
        }
      }

      Sigma(tag, props) => {
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let ty = self.eval(ty)?;
          ps.push((name.clone(), i.clone(), ty));
        }
        ValF::Sigma(tag.clone(), ps)
      }
      Obj(tag, props) => {
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.eval(e)?;
          ps.push((name.clone(), e));
        }
        ValF::Obj(tag.clone(), ps)
      }
      Prop(tag, e, name) => {
        let e = self.eval(e)?;
        match &e.0 {
          ValF::Bind(i) => ValF::Prop(tag.clone(), *i, *name),
          ValF::Def(i) => unimplemented!(),
          ValF::Obj(otag, props) => {
            if tag != otag {
              return Err("eval: prop".to_string());
            }
            for (n, e) in props {
              if n == name {
                return Ok(e.clone());
              }
            }
            return Err("eval: prop".to_string());
          }
          _ => return Err("eval: prop".to_string()),
        }
      }
    };
    Ok(Rc::new(Val(v)))
  }

  fn check(&mut self, e: &Expr<P, S>, check_ty: &Type<P, S>) -> Result<()> {
    use ExprF::*;
    match &e.0 {
      Hole => Ok(()),
      Let(defs, body) => {
        self.envs.push(Env::new());
        let defs = self.defs(defs);
        self.check(body, check_ty)?;
        self.envs.pop();
        // TODO: resolve let?
        Ok(())
      }
      Lam(tag, vis, i, ty, body) => match &check_ty.0 {
        ValF::Pi(ttag, tvis, ti, tty, bty) => {
          if tag != ttag || vis != tvis {
            return Err("check: lam".to_string());
          }
          let ty = self.eval(ty)?;
          self.unify(&ty, tty)?;
          self.envs.push(Env::new());
          self.here().add(*i, ty.clone());
          // Note: bty should not depend on i
          let tbody = self.subst(*ti, &Rc::new(Val(ValF::Bind(*i))), bty).into();
          let bty = self.check(body, &tbody)?;
          self.envs.pop();
          Ok(())
        }
        _ => Err("check: lam".to_string()),
      },
      Obj(tag, props) => match &check_ty.0 {
        ValF::Sigma(ttag, tprops) => {
          if tag != ttag {
            return Err("check: obj 1".to_string());
          }
          let mut ps = Vec::new();
          self.envs.push(Env::new());
          eprintln!("- object match {}: {}", self.ppe(e), self.ppv(check_ty));
          for ((n, e), (tn, ti, ty)) in props.iter().zip(tprops.iter()) {
            if n != tn {
              eprintln!("{:?} != {:?}", n, tn);
              return Err("check: obj 2".to_string());
            }
            self.check(&e, &ty)?;
            self.here().add(*ti, ty.clone());
            ps.push((n.clone(), *ti, Rc::new(ty)));
          }
          if props.len() != tprops.len() {
            return Err("check: obj 3".to_string());
          }
          self.envs.pop();
          Ok(())
        }
        _ => return Err("check: obj 4".to_string()),
      },
      _ => {
        let ty = self.synth(e)?;
        self.unify(&ty, check_ty)?;
        Ok(())
      }
    }
  }

  fn synth(&mut self, e: &Expr<P, S>) -> Result<Type<P, S>> {
    use ExprF::*;
    match &e.0 {
      Hole => Err("synth: hole".to_string()),
      Bind(i) => match self.lookup_bind(*i) {
        Some(ty) => Ok(ty.clone()),
        None => Err("synth: bind".to_string()),
      },
      Def(i) => match self.lookup_def(*i) {
        Some(ty) => Ok(ty.clone()),
        None => Err("synth: def".to_string()),
      },
      Ann(t, ty) => {
        self.check(&ty, &Rc::new(Val(ValF::Uni)))?;
        let ty = self.eval(ty)?;
        self.check(t, &ty)?;
        Ok(ty)
      }
      Uni => Ok(Rc::new(Val(ValF::Uni))),

      Let(defs, body) => {
        self.envs.push(Env::new());
        let defs = self.defs(defs);
        let bty = self.synth(body)?;
        self.envs.pop();
        Ok(self.resolve_let(defs, bty))
      }

      Pi(_, _, i, ty, body) => {
        let ty = self.synth(ty)?;
        self.envs.push(Env::new());
        self.here().add(*i, ty.clone());
        self.synth(body)?;
        self.envs.pop();
        Ok(Rc::new(Val(ValF::Uni)))
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        self.envs.push(Env::new());
        self.here().add(*i, ty.clone());
        let bty = self.synth(body)?;
        self.envs.pop();
        Ok(Rc::new(Val(ValF::Pi(
          tag.clone(),
          vis.clone(),
          *i,
          ty,
          bty,
        ))))
      }
      App(tag, vis, f, x) => {
        let fty = self.synth(f)?;
        match &fty.0 {
          ValF::Pi(ftag, fvis, i, ty, bty) => {
            if tag != ftag || vis != fvis {
              return Err("synth: app".to_string());
            }
            self.check(x, &ty)?;
            let x = self.eval(x)?;
            // substitute i with x in bty
            let bty = self.subst(*i, &x, bty).into();
            Ok(bty)
          }
          _ => Err("synth: app".to_string()),
        }
      }

      Sigma(_, props) => {
        self.envs.push(Env::new());
        for (_, i, ty) in props {
          let ty = self.synth(ty)?;
          self.here().add(*i, ty.clone());
        }
        self.envs.pop();
        Ok(Rc::new(Val(ValF::Uni)))
      }
      Obj(tag, props) => {
        let mut tys = Vec::new();
        self.envs.push(Env::new());
        for (name, e) in props {
          let ty = self.synth(e)?;
          let i = self.fresh_id();
          tys.push((name.clone(), i, ty.clone()));
        }
        self.envs.pop();
        Ok(Rc::new(Val(ValF::Sigma(tag.clone(), tys))))
      }
      Prop(tag, e, name) => {
        let ty = self.synth(e)?;
        match &ty.0 {
          ValF::Sigma(etag, props) => {
            if tag != etag {
              return Err("synth: prop".to_string());
            }
            for (index, (n, _, ty)) in props.iter().enumerate() {
              if n == name {
                let e = self.eval(e)?;
                let mut ty = ty.clone();
                // ty may depend on previous props
                for (n, i, _) in props.iter().take(index).rev() {
                  let p = self.prop(tag, &e, n); // e.n
                  ty = self.subst(*i, &p, &ty).into();
                }
                return Ok(ty);
              }
            }
            Err("synth: prop".to_string())
          }
          _ => Err("synth: prop".to_string()),
        }
      }
    }
  }

  fn defs(&mut self, defs: &Vec<(DefId, Rc<Expr<P, S>>)>) -> Vec<(DefId, Rc<Term<P, S>>)> {
    let mut def_terms = Vec::new();
    let fail = Rc::new(Val(ValF::Fail));
    for (id, expr) in defs {
      let ty = self.synth(&*expr);
      match &ty {
        Ok(ty) => eprintln!("[o] {}: {}", self.def_symbols[&id], self.ppv(ty)),
        Err(e) => eprintln!("[x] {}: {}", self.def_symbols[&id], e),
      }
      let (tm, ty) = match ty {
        Ok(ty) => match self.eval(&expr) {
          Ok(tm) => (tm, ty),
          Err(e) => {
            self.add_error(&format!("{}: {}", self.def_symbols[&id], e));
            (fail.clone(), ty)
          }
        },
        Err(e) => {
          self.add_error(&format!("{}: {}", self.def_symbols[&id], e));
          (fail.clone(), fail.clone())
        }
      };
      self.here().def(*id, ty);
      def_terms.push((*id, Rc::new(tm)));
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
  c.defs(&p.defs);
  if c.errors.is_empty() {
    Ok(())
  } else {
    Err(c.errors)
  }
}
