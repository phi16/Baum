use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

pub trait Tag: Clone + std::fmt::Debug + PartialEq + Eq {}

struct Env<P, S> {
  lookup: HashMap<BindId, Type<P, S>>,
}

impl<P, S> Env<P, S> {
  fn new() -> Self {
    Env {
      lookup: HashMap::new(),
    }
  }

  fn add(&mut self, bind: BindId, ty: Type<P, S>) {
    self.lookup.insert(bind, ty);
  }
}

struct Checker<P, S> {
  defs: HashMap<BindId, Option<(Term<P, S>, Type<P, S>)>>,
  bind_symbols: HashMap<BindId, String>,
  name_symbols: HashMap<NameId, String>,
  envs: Vec<Env<P, S>>,
  errors: Vec<String>,
}

impl<P, S> Checker<P, S>
where
  P: Tag,
  S: Tag,
{
  fn new(bind_symbols: HashMap<BindId, String>, name_symbols: HashMap<NameId, String>) -> Self {
    Checker {
      defs: HashMap::new(),
      bind_symbols,
      name_symbols,
      envs: vec![Env::new()],
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, msg: &str) {
    self.errors.push(msg.to_string());
  }

  fn here(&mut self) -> &mut HashMap<BindId, Type<P, S>> {
    &mut self.envs.last_mut().unwrap().lookup
  }

  fn lookup(&self, bind: BindId) -> Option<&Type<P, S>> {
    for env in self.envs.iter().rev() {
      if let Some(ty) = env.lookup.get(&bind) {
        return Some(ty);
      }
    }
    None
  }

  fn unify(&mut self, t1: &Type<P, S>, t2: &Type<P, S>) -> Result<Type<P, S>> {
    unimplemented!()
  }

  fn eval(&mut self, e: &Expr<P, S>) -> Result<Term<P, S>> {
    unimplemented!()
  }

  fn replace(&mut self, i: BindId, e: &Term<P, S>, ty: &Type<P, S>) -> Type<P, S> {
    unimplemented!()
  }

  fn valprop(&mut self, n: NameId, e: &Term<P, S>) -> Term<P, S> {
    unimplemented!()
  }

  fn check(&mut self, e: &Expr<P, S>, check_ty: &Type<P, S>) -> Result<Type<P, S>> {
    use ExprF::*;
    match &e.0 {
      Hole => Ok(check_ty.clone()),
      Let(defs, body) => {
        self.envs.push(Env::new());
        for (i, e) in defs {
          let ty = self.synth(e)?;
          self.here().insert(*i, ty);
        }
        let bty = self.check(body, check_ty)?;
        self.envs.pop();
        // TODO: bty may depend on defs...? ( => yes )
        Ok(bty)
      }
      Lam(tag, vis, i, ty, body) => match &check_ty.0 {
        ValF::Pi(ttag, tvis, ti, tty, bty) => {
          if tag != ttag || vis != tvis {
            return Err("check: lam".to_string());
          }
          let ty = self.eval(ty)?;
          let ty = self.unify(&ty, tty)?;
          self.envs.push(Env::new());
          self.here().insert(*i, ty);
          let tbody = if let Some(ti) = ti {
            // Note: bty should not depend on i
            self.replace(*ti, &Val(ValF::Bind(*i)), bty)
          } else {
            (**bty).clone()
          };
          let bty = self.check(body, &tbody)?;
          self.envs.pop();
          Ok(unimplemented!())
        }
        _ => Err("check: lam".to_string()),
      },
      Obj(tag, props) => {
        unimplemented!()
      }
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
      Bind(id) => match self.lookup(*id) {
        Some(ty) => Ok(ty.clone()),
        None => Err("synth: bind".to_string()),
      },
      Ann(t, ty) => {
        let tyty = self.synth(ty)?;
        self.unify(&tyty, &Val(ValF::Uni))?;
        let ty = self.eval(ty)?;
        self.check(t, &ty)?;
        Ok(ty)
      }
      Def(e) => {
        let ty = self.synth(e)?;
        // TODO: universe polymorphism
        Ok(ty)
      }
      Uni => Ok(Val(ValF::Uni)),

      Let(defs, body) => {
        self.envs.push(Env::new());
        for (i, e) in defs {
          let ty = self.synth(e)?;
          self.here().insert(*i, ty);
        }
        let bty = self.synth(body)?;
        self.envs.pop();
        // bty may depend on defs
        Ok(bty)
      }

      Pi(_, _, i, ty, body) => {
        let ty = self.synth(ty)?;
        self.envs.push(Env::new());
        if let Some(i) = i {
          self.here().insert(*i, ty.clone());
        }
        self.synth(body)?;
        self.envs.pop();
        Ok(Val(ValF::Uni))
      }
      Lam(tag, vis, i, ty, body) => {
        let ty = self.eval(ty)?;
        self.envs.push(Env::new());
        self.here().insert(*i, ty.clone());
        let bty = self.synth(body)?;
        self.envs.pop();
        Ok(Val(ValF::Pi(
          tag.clone(),
          vis.clone(),
          Some(*i),
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
            // replace i with x in bty
            let bty = if let Some(i) = i {
              self.replace(*i, &x, bty)
            } else {
              (**bty).clone()
            };
            Ok(bty)
          }
          _ => Err("synth: app".to_string()),
        }
      }

      Sigma(_, props) => {
        self.envs.push(Env::new());
        for (_, i, ty) in props {
          let ty = self.synth(ty)?;
          if let Some(i) = i {
            self.here().insert(*i, ty.clone());
          }
        }
        self.envs.pop();
        Ok(Val(ValF::Uni))
      }
      Obj(tag, props) => {
        let mut tys = Vec::new();
        self.envs.push(Env::new());
        for (name, e) in props {
          let ty = self.synth(e)?;
          tys.push((name.clone(), None, Rc::new(ty.clone())));
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
                  if let Some(i) = i {
                    let p = self.valprop(*n, &e);
                    ty = self.replace(*i, &p, &ty);
                  }
                }
                return Ok(ty);
              }
            }
            Err("synth: prop".to_string())
          }
          _ => Err("synth: prop".to_string()),
        }
      }
      _ => Err(format!("synth: unimplemented ({:?})", e.0)),
    }
  }
}

pub fn check<P, S>(p: Program<P, S>) -> std::result::Result<(), Vec<String>>
where
  P: Tag,
  S: Tag,
{
  let mut c = Checker::new(p.bind_symbols, p.name_symbols);
  for (id, expr) in p.defs {
    let ty = c.synth(&*expr);
    eprintln!("{}: {:?}", c.bind_symbols[&id], ty);
    let tmty = match ty {
      Ok(ty) => match c.eval(&expr) {
        Ok(tm) => Some((tm, ty)),
        Err(e) => {
          c.add_error(&format!("{}: {}", c.bind_symbols[&id], e));
          None
        }
      },
      Err(e) => {
        c.add_error(&format!("{}: {}", c.bind_symbols[&id], e));
        None
      }
    };
    c.defs.insert(id, tmty);
  }
  Ok(())
}
