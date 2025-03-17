use crate::pretty::{pretty_expr, pretty_val};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

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

struct Checker<P, S> {
  def_symbols: HashMap<DefId, String>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  envs: Vec<Env<P, S>>,
  next_bind_id: u32,
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
      next_bind_id,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, msg: &str) {
    self.errors.push(msg.to_string());
  }

  fn here(&mut self) -> &mut Env<P, S> {
    self.envs.last_mut().unwrap()
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

  fn ppv(&self, t: &Type<P, S>) -> String {
    pretty_val(&self.def_symbols, &self.bind_symbols, &self.name_symbols, t)
  }

  fn ppe(&self, e: &Expr<P, S>) -> String {
    pretty_expr(&self.def_symbols, &self.bind_symbols, &self.name_symbols, e)
  }

  fn resolve_let(&mut self, defs: Vec<(DefId, Rc<Term<P, S>>)>, e: Term<P, S>) -> Term<P, S> {
    eprintln!(
      "let_binding: escaping {:?} from {}",
      defs.iter().map(|(k, _)| *k).collect::<Vec<_>>(),
      self.ppv(&e)
    );
    unimplemented!()
  }

  fn unify(&self, t1: &Type<P, S>, t2: &Type<P, S>) -> Result<Type<P, S>> {
    // prioritize t2
    eprintln!("unify: {} ≟ {}", self.ppv(t1), self.ppv(t2));
    match (&t1.0, &t2.0) {
      (ValF::Hole, _) => Ok(t2.clone()),
      (_, ValF::Hole) => Ok(t1.clone()),
      (ValF::Bind(i1), ValF::Bind(i2)) => {
        if i1 == i2 {
          Ok(t2.clone())
        } else {
          // TODO...?
          Err("unify: bind".to_string())
        }
      }
      (ValF::Uni, ValF::Uni) => Ok(t1.clone()),
      _ => Err("unify: unimplemented".to_string()),
    }
  }

  fn eval(&mut self, e: &Expr<P, S>) -> Result<Term<P, S>> {
    use ExprF::*;
    let v = match &e.0 {
      Hole => ValF::Hole,
      Bind(i) => ValF::Bind(*i),
      Def(i) => ValF::Def(*i),
      Ann(t, _) => self.eval(t)?.0,
      Synth(e) => self.eval(e)?.0,
      Uni => ValF::Uni,
      Let(defs, body) => {
        self.envs.push(Env::new());
        let defs = self.defs(defs);
        let body = self.eval(body)?;
        self.envs.pop();
        self.resolve_let(defs, body).0
      }

      Pi(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        let body = self.eval(body)?;
        ValF::Pi(
          tag.clone(),
          vis.clone(),
          i.clone(),
          Rc::new(ty),
          Rc::new(body),
        )
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        let body = self.eval(body)?;
        ValF::Lam(tag.clone(), vis.clone(), *i, Rc::new(ty), Rc::new(body))
      }
      App(tag, vis, f, x) => {
        let f = self.eval(f)?;
        let x = self.eval(x)?;
        match f.0 {
          ValF::Bind(i) => ValF::App(tag.clone(), vis.clone(), i, Rc::new(x)),
          ValF::Lam(ftag, fvis, i, ty, body) => {
            if *tag != ftag || *vis != fvis {
              return Err("eval: app".to_string());
            }
            let body = self.subst(i, &x, &body);
            ValF::Lam(tag.clone(), vis.clone(), i, ty.clone(), Rc::new(body))
          }
          _ => return Err("eval: app".to_string()),
        }
      }

      Sigma(tag, props) => {
        let mut ps = Vec::new();
        for (name, i, ty) in props {
          let ty = self.eval(ty)?;
          ps.push((name.clone(), i.clone(), Rc::new(ty)));
        }
        ValF::Sigma(tag.clone(), ps)
      }
      Obj(tag, props) => {
        let mut ps = Vec::new();
        for (name, e) in props {
          let e = self.eval(e)?;
          ps.push((name.clone(), Rc::new(e)));
        }
        ValF::Obj(tag.clone(), ps)
      }
      Prop(tag, e, name) => {
        let e = self.eval(e)?;
        match e.0 {
          ValF::Bind(i) => ValF::Prop(tag.clone(), i, *name),
          ValF::Obj(otag, props) => {
            if *tag != otag {
              return Err("eval: prop".to_string());
            }
            for (n, e) in props {
              if n == *name {
                return Ok((*e).clone());
              }
            }
            return Err("eval: prop".to_string());
          }
          _ => return Err("eval: prop".to_string()),
        }
      }
    };
    Ok(Val(v))
  }

  fn subst(&mut self, i: BindId, e: &Term<P, S>, ty: &Type<P, S>) -> Type<P, S> {
    eprintln!("subst: {:?} with {} in {}", i, self.ppv(e), self.ppv(ty));
    let v = match &ty.0 {
      ValF::Hole => ValF::Hole,
      ValF::Bind(j) => {
        if i == *j {
          e.0.clone()
        } else {
          ValF::Bind(*j)
        }
      }
      ValF::Def(i) => ValF::Def(*i),
      ValF::Uni => ValF::Uni,

      ValF::Pi(tag, vis, ti, ty, body) => {
        if *ti == i {
          ty.0.clone()
        } else {
          let ty = self.subst(i, e, ty);
          let body = self.subst(i, e, body);
          ValF::Pi(
            tag.clone(),
            vis.clone(),
            ti.clone(),
            Rc::new(ty),
            Rc::new(body),
          )
        }
      }
      ValF::Lam(tag, vis, ti, ty, body) => {
        if *ti == i {
          ty.0.clone()
        } else {
          let ty = self.subst(i, e, ty);
          let body = self.subst(i, e, body);
          ValF::Lam(tag.clone(), vis.clone(), *ti, Rc::new(ty), Rc::new(body))
        }
      }
      ValF::App(tag, vis, ti, x) => {
        unimplemented!()
      }

      ValF::Sigma(tag, props) => {
        let mut ps = Vec::new();
        for (name, ti, ty) in props {
          if *ti == i {
            unimplemented!()
          } else {
            let ty = self.subst(i, e, ty);
            ps.push((name.clone(), ti.clone(), Rc::new(ty)));
          }
        }
        ValF::Sigma(tag.clone(), ps)
      }
      ValF::Obj(tag, props) => {
        let mut ps = Vec::new();
        for (name, el) in props {
          let el = self.subst(i, e, el);
          ps.push((name.clone(), Rc::new(el)));
        }
        ValF::Obj(tag.clone(), ps)
      }
      ValF::Prop(tag, ti, name) => {
        unimplemented!()
      }
    };
    Val(v)
  }

  fn valprop(&mut self, tag: &S, e: &Term<P, S>, name: &NameId) -> Result<Term<P, S>> {
    let v = match &e.0 {
      ValF::Bind(i) => ValF::Prop(tag.clone(), *i, *name),
      ValF::Obj(otag, props) => {
        if tag != otag {
          return Err("valprop: obj".to_string());
        }
        for (n, e) in props {
          if n == name {
            return Ok((**e).clone());
          }
        }
        return Err("valprop: obj".to_string());
      }
      _ => return Err("valprop".to_string()),
    };
    Ok(Val(v))
  }

  fn check(&mut self, e: &Expr<P, S>, check_ty: &Type<P, S>) -> Result<Type<P, S>> {
    use ExprF::*;
    match &e.0 {
      Hole => Ok(check_ty.clone()),
      Let(defs, body) => {
        self.envs.push(Env::new());
        let defs = self.defs(defs);
        let bty = self.check(body, check_ty)?;
        self.envs.pop();
        Ok(self.resolve_let(defs, bty))
      }
      Lam(tag, vis, i, ty, body) => match &check_ty.0 {
        ValF::Pi(ttag, tvis, ti, tty, bty) => {
          if tag != ttag || vis != tvis {
            return Err("check: lam".to_string());
          }
          let ty = self.eval(ty)?;
          let ty = self.unify(&ty, tty)?;
          self.envs.push(Env::new());
          self.here().add(*i, ty.clone());
          // Note: bty should not depend on i
          let tbody = self.subst(*ti, &Val(ValF::Bind(*i)), bty);
          let bty = self.check(body, &tbody)?;
          self.envs.pop();
          Ok(Val(ValF::Pi(
            ttag.clone(),
            tvis.clone(),
            *i, // hmm...
            Rc::new(ty),
            Rc::new(bty),
          )))
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
          eprintln!("match {} ~ {}", self.ppe(e), self.ppv(check_ty));
          for ((n, e), (tn, ti, ty)) in props.iter().zip(tprops.iter()) {
            if n != tn {
              eprintln!("{:?} != {:?}", n, tn);
              return Err("check: obj 2".to_string());
            }
            let ty = self.check(&e, &ty)?;
            self.here().add(*ti, ty.clone());
            ps.push((n.clone(), *ti, Rc::new(ty)));
          }
          if props.len() != tprops.len() {
            return Err("check: obj 3".to_string());
          }
          self.envs.pop();
          Ok(Val(ValF::Sigma(tag.clone(), ps)))
        }
        _ => return Err("check: obj 4".to_string()),
      },
      _ => {
        let ty = self.synth(e)?;
        self.unify(&ty, check_ty)
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
        let tyty = self.synth(ty)?;
        self.unify(&tyty, &Val(ValF::Uni))?;
        let ty = self.eval(ty)?;
        self.check(t, &ty)?;
        Ok(ty)
      }
      Synth(e) => {
        let ty = self.synth(e)?;
        // TODO: universe polymorphism
        Ok(ty)
      }
      Uni => Ok(Val(ValF::Uni)),

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
        Ok(Val(ValF::Uni))
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        self.envs.push(Env::new());
        self.here().add(*i, ty.clone());
        let bty = self.synth(body)?;
        self.envs.pop();
        Ok(Val(ValF::Pi(
          tag.clone(),
          vis.clone(),
          *i,
          Rc::new(ty),
          Rc::new(bty),
        )))
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
            let bty = self.subst(*i, &x, bty);
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
        Ok(Val(ValF::Uni))
      }
      Obj(tag, props) => {
        let mut tys = Vec::new();
        self.envs.push(Env::new());
        for (name, e) in props {
          let ty = self.synth(e)?;
          let i = self.fresh_id();
          tys.push((name.clone(), i, Rc::new(ty.clone())));
        }
        self.envs.pop();
        Ok(Val(ValF::Sigma(tag.clone(), tys)))
      }
      Prop(tag, e, name) => {
        let ty = self.synth(e)?;
        match ty.0 {
          ValF::Sigma(etag, props) => {
            if *tag != etag {
              return Err("synth: prop".to_string());
            }
            for (index, (n, _, ty)) in props.iter().enumerate() {
              if n == name {
                let e = self.eval(e)?;
                let mut ty = (**ty).clone();
                // ty may depend on previous props
                for (n, i, _) in props.iter().take(index).rev() {
                  let p = self.valprop(tag, &e, n)?;
                  ty = self.subst(*i, &p, &ty);
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
    for (id, expr) in defs {
      let ty = self.synth(&*expr);
      match &ty {
        Ok(ty) => eprintln!("{}: Ok({})", self.def_symbols[&id], self.ppv(ty)),
        Err(e) => eprintln!("{}: Err({})", self.def_symbols[&id], e),
      }
      let (tm, ty) = match ty {
        Ok(ty) => match self.eval(&expr) {
          Ok(tm) => (tm, ty),
          Err(e) => {
            self.add_error(&format!("{}: {}", self.def_symbols[&id], e));
            (Val(ValF::Hole), ty)
          }
        },
        Err(e) => {
          self.add_error(&format!("{}: {}", self.def_symbols[&id], e));
          (Val(ValF::Hole), Val(ValF::Hole))
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
