use crate::eval::*;
use crate::pretty::{pretty_expr, pretty_val};
use crate::types::common::*;
use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

struct Env<P, S> {
  lookup: HashMap<BindId, Type<P, S>>,
  define: HashMap<DefId, Option<Type<P, S>>>,
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

  fn def(&mut self, def: DefId, ty: Option<Type<P, S>>) {
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

  fn add_constraint(&mut self, hole: HoleId, v: Term<P, S>) {
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
  ev: Eval<P, S>,
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
      ev: Eval::new(),
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

  fn resolve_let(&mut self, defs: Vec<(DefId, Rc<Term<P, S>>)>, e: Term<P, S>) -> Term<P, S> {
    eprintln!(
      "let_binding: escaping {:?} from {}",
      defs.iter().map(|(k, _)| *k).collect::<Vec<_>>(),
      self.ppv(&e)
    );
    unimplemented!()
  }

  fn unify(&mut self, v1: &RV<P, S>, v2: &RV<P, S>) -> Result<()> {
    struct BEnv {
      b1: HashMap<BindId, u32>,
      b2: HashMap<BindId, u32>,
    }
    struct B {
      benvs: Vec<BEnv>,
      bid: u32,
    }
    impl B {
      fn new() -> Self {
        B {
          benvs: Vec::new(),
          bid: 0,
        }
      }

      fn push(&mut self) {
        self.benvs.push(BEnv {
          b1: HashMap::new(),
          b2: HashMap::new(),
        });
      }

      fn pop(&mut self) {
        self.benvs.pop();
      }

      fn unify(&mut self, i1: BindId, i2: BindId) {
        let benv = self.benvs.last_mut().unwrap();
        let bid = self.bid;
        self.bid += 1;
        benv.b1.insert(i1, bid);
        benv.b2.insert(i2, bid);
      }

      fn find1(&self, i: &BindId) -> Option<u32> {
        for benv in self.benvs.iter().rev() {
          if let Some(bid) = benv.b1.get(i) {
            return Some(*bid);
          }
        }
        None
      }
      fn find2(&self, i: &BindId) -> Option<u32> {
        for benv in self.benvs.iter().rev() {
          if let Some(bid) = benv.b2.get(i) {
            return Some(*bid);
          }
        }
        None
      }

      fn unifiable(&self, i1: &BindId, i2: &BindId) -> bool {
        if i1 == i2 {
          true
        } else {
          match (self.find1(i1), self.find2(i2)) {
            (Some(b1), Some(b2)) if b1 == b2 => true,
            _ => false,
          }
        }
      }
    }

    fn rec<P: Tag, S: Tag>(
      this: &mut Checker<P, S>,
      v1: &RV<P, S>,
      v2: &RV<P, S>,
      b: &mut B,
    ) -> Result<()> {
      if Rc::ptr_eq(v1, v2) {
        return Ok(());
      }
      match (&v1.0, &v2.0) {
        (ValF::Neu(i1, ks1), ValF::Neu(i2, ks2)) => {
          // try to do one-to-one unification
          if ks1.len() == ks2.len() {
            let mut failed = false;
            for (k1, k2) in ks1.iter().zip(ks2.iter()) {
              match (k1, k2) {
                (ContF::App(tag1, vis1, x1), ContF::App(tag2, vis2, x2)) => {
                  if tag1 != tag2 || vis1 != vis2 {
                    failed = true;
                    break;
                  }
                  if let Err(_) = rec(this, x1, x2, b) {
                    failed = true;
                    break;
                  }
                }
                (ContF::Prop(tag1, n1), ContF::Prop(tag2, n2)) => {
                  if tag1 != tag2 || n1 != n2 {
                    failed = true;
                    break;
                  }
                }
                _ => {
                  failed = true;
                  break;
                }
              }
            }
            if !failed {
              if i1 == i2 {
                return Ok(());
              } else {
                match (i1, i2) {
                  _ => unimplemented!(),
                }
              }
            }
          }
          // do computation here
          unimplemented!()
        }
        /* (ValF::Hole(h1), _) => {
          if let Some(v1) = this.lookup_constraint(*h1) {
            rec(this, &v1.clone(), v2, b)
          } else {
            eprintln!("add_constraint: {:?} ~ {}", h1, this.ppv(v2));
            this.solve().add_constraint(*h1, v2.clone());
            Ok(())
          }
        }
        (_, ValF::Hole(h2)) => {
          if let Some(v2) = this.lookup_constraint(*h2) {
            rec(this, v1, &v2.clone(), b)
          } else {
            eprintln!("add_constraint: {:?} ~ {}", h2, this.ppv(v1));
            this.solve().add_constraint(*h2, v1.clone());
            Ok(())
          }
        }
        (ValF::Bind(i1), ValF::Bind(i2)) => {
          if b.unifiable(i1, i2) {
            Ok(())
          } else {
            Err(format!("unify: bind: {:?} ~ {:?}", i1, i2))
          }
        }
        (ValF::App(tag1, vis1, i1, x1), ValF::App(tag2, vis2, i2, x2)) => {
          if tag1 != tag2 || vis1 != vis2 {
            return Err(format!(
              "unify: app: tag/vis mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          if b.unifiable(i1, i2) {
            rec(this, x1, x2, b)
          } else {
            Err(format!("unify: app: mismatch: {:?} ~ {:?}", i1, i2))
          }
        }
        (ValF::Prop(tag1, i1, n1), ValF::Prop(tag2, i2, n2)) => {
          if tag1 != tag2 {
            return Err(format!(
              "unify: prop: tag mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          if b.unifiable(i1, i2) {
            if n1 != n2 {
              return Err(format!(
                "unify: prop: name mismatch: {:?} ~ {:?}",
                this.ppv(v1),
                this.ppv(v2)
              ));
            }
            Ok(())
          } else {
            Err(format!("unify: prop: mismatch: {:?} ~ {:?}", i1, i2))
          }
        } */
        (ValF::Uni, ValF::Uni) => Ok(()),
        (ValF::Pi(tag1, vis1, i1, ty1, bty1), ValF::Pi(tag2, vis2, i2, ty2, bty2)) => {
          if tag1 != tag2 || vis1 != vis2 {
            return Err(format!(
              "unify: pi: tag/vis mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          rec(this, ty1, ty2, b)?;
          b.push();
          b.unify(*i1, *i2);
          rec(this, bty1, bty2, b)?;
          b.pop();
          Ok(())
        }
        (ValF::Lam(tag1, vis1, i1, ty1, body1), ValF::Lam(tag2, vis2, i2, ty2, body2)) => {
          if tag1 != tag2 || vis1 != vis2 {
            return Err(format!(
              "unify: pi: tag/vis mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          rec(this, ty1, ty2, b)?;
          b.push();
          b.unify(*i1, *i2);
          rec(this, body1, body2, b)?;
          b.pop();
          Ok(())
        }

        (ValF::Sigma(tag1, props1), ValF::Sigma(tag2, props2)) => {
          if tag1 != tag2 {
            return Err(format!(
              "unify: sigma: tag mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          if props1.len() != props2.len() {
            return Err(format!(
              "unify: sigma: length mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          b.push();
          for ((n1, i1, ty1), (n2, i2, ty2)) in props1.iter().zip(props2.iter()) {
            if n1 != n2 {
              return Err(format!(
                "unify: sigma: name mismatch: {:?} ~ {:?}",
                this.ppv(v1),
                this.ppv(v2)
              ));
            }
            rec(this, ty1, ty2, b)?;
            b.unify(*i1, *i2);
          }
          b.pop();
          Ok(())
        }
        (ValF::Obj(tag1, props1), ValF::Obj(tag2, props2)) => {
          if tag1 != tag2 {
            return Err(format!(
              "unify: sigma: tag mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          if props1.len() != props2.len() {
            return Err(format!(
              "unify: sigma: length mismatch: {:?} ~ {:?}",
              this.ppv(v1),
              this.ppv(v2)
            ));
          }
          for ((n1, e1), (n2, e2)) in props1.iter().zip(props2.iter()) {
            if n1 != n2 {
              return Err(format!(
                "unify: obj: name mismatch: {:?} ~ {:?}",
                this.ppv(v1),
                this.ppv(v2)
              ));
            }
            rec(this, e1, e2, b)?;
          }
          Ok(())
        }
        _ => Err(format!(
          "unify: mismatch: {:?} ~ {:?}",
          this.ppv(v1),
          this.ppv(v2)
        )),
      }
    }
    rec(self, v1, v2, &mut B::new())
  }

  fn check(&mut self, e: Expr<P, S>, check_ty: &Type<P, S>) -> Result<CE<P, S>> {
    use ExprF::*;
    match e.0 {
      Hole(_) => {
        let h = self.fresh_hole();
        self.solve().add_hole(h, check_ty.clone());
        Ok(CE(Hole(h)))
      }
      Let(defs, body) => {
        unimplemented!()
      }
      Lam(tag, vis, i, ty, body) => match &check_ty.0 {
        ValF::Pi(ttag, tvis, ti, tty, bty) => {
          if tag != *ttag || vis != *tvis {
            return Err("check: lam".to_string());
          }
          let c_ty = self.check_ty(*ty)?;
          let ty = self.ev.eval(&c_ty);
          self.unify(&ty, tty)?;
          self.envs.push(Env::new());
          self.here().add(i, tty.clone());
          // currently bty depends on ti, not i
          let i_e = Rc::new(Val(ValF::Neu(IdF::Bind(i), Vec::new())));
          let bty = self.ev.subst(*ti, &i_e, bty).into();
          let c_body = self.check(*body, &bty)?;
          self.envs.pop();
          Ok(CE(Lam(tag, vis, i, Box::new(c_ty), Box::new(c_body))))
        }
        _ => Err("check: lam".to_string()),
      },
      Obj(tag, props) => match &check_ty.0 {
        ValF::Sigma(ttag, tprops) => {
          if tag != *ttag {
            return Err("check: obj 1".to_string());
          }
          let len_match = props.len() == tprops.len();
          self.envs.push(Env::new());
          let mut ps = Vec::new();
          let mut c_ps = Vec::new();
          for ((n, e), (tn, ti, ty)) in props.into_iter().zip(tprops.iter()) {
            if n != *tn {
              eprintln!("{:?} != {:?}", n, tn);
              return Err("check: obj 2".to_string());
            }
            // ty may depend on previous props
            let mut ty = ty.clone();
            for (ji, je) in &ps {
              ty = self.ev.subst(*ji, je, &ty).into();
            }
            let c_e = self.check(*e, &ty)?;
            let e = self.ev.eval(&c_e);
            self.here().add(*ti, ty.clone());
            ps.push((*ti, e));
            c_ps.push((n, Box::new(c_e)));
          }
          self.envs.pop();
          if !len_match {
            // we defer this error to check the existing props as much as possible
            return Err("check: obj 3".to_string());
          }
          Ok(CE(Obj(tag, c_ps)))
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
      Hole(_) => Err("synth: hole".to_string()),
      Bind(i) => match self.lookup_bind(i) {
        Some(ty) => Ok((CE(Bind(i)), ty.clone())),
        None => Err("synth: bind".to_string()),
      },
      Def(i) => match self.lookup_def(i) {
        Some(Some(ty)) => Ok((CE(Def(i)), ty.clone())),
        Some(None) => Err("synth: fail".to_string()),
        None => Err("synth: def".to_string()),
      },
      Ann(t, ty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.ev.eval(&c_ty);
        let c_t = self.check(*t, &ty)?;
        Ok((CE(Ann(Box::new(c_t), Box::new(c_ty))), ty))
      }
      Uni => Ok(((CE(Uni)), Rc::new(Val(ValF::Uni)))),

      Let(defs, body) => {
        unimplemented!()
      }

      Pi(tag, vis, i, ty, bty) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.ev.eval(&c_ty);
        self.envs.push(Env::new());
        self.here().add(i, ty.clone());
        let c_bty = self.check_ty(*bty)?;
        self.envs.pop();
        Ok((
          CE(Pi(tag, vis, i, Box::new(c_ty), Box::new(c_bty))),
          Rc::new(Val(ValF::Uni)),
        ))
      }
      Lam(tag, vis, i, ty, body) => {
        let c_ty = self.check_ty(*ty)?;
        let ty = self.ev.eval(&c_ty);
        self.envs.push(Env::new());
        self.here().add(i, ty.clone());
        let (c_body, bty) = self.synth(*body)?;
        self.envs.pop();
        Ok((
          (CE(Lam(
            tag.clone(),
            vis.clone(),
            i,
            Box::new(c_ty),
            Box::new(c_body),
          ))),
          Rc::new(Val(ValF::Pi(tag, vis, i, ty, bty))),
        ))
      }
      App(tag, vis, f, x) => {
        let (c_f, fty) = self.synth(*f)?;
        match &fty.0 {
          ValF::Pi(ftag, fvis, i, ty, bty) => {
            if tag != *ftag {
              return Err("synth: app".to_string());
            }
            if vis != *fvis {
              return Err("synth: app (may implicit app)".to_string());
            }
            let c_x = self.check(*x, &ty)?;
            let x = self.ev.eval(&c_x);
            // substitute i with x in bty
            let bty = self.ev.subst(*i, &x, bty).into();
            Ok((CE(App(tag, vis, Box::new(c_f), Box::new(c_x))), bty))
          }
          _ => Err(format!("synth: app / {:?}", fty)),
        }
      }

      Sigma(tag, props) => {
        self.envs.push(Env::new());
        let mut c_props = Vec::new();
        for (name, i, ty) in props {
          let c_ty = self.check_ty(*ty)?;
          let ty = self.ev.eval(&c_ty);
          self.here().add(i, ty);
          c_props.push((name, i, Box::new(c_ty)));
        }
        self.envs.pop();
        Ok((CE(Sigma(tag, c_props)), Rc::new(Val(ValF::Uni))))
      }
      Obj(tag, props) => {
        self.envs.push(Env::new());
        let mut tys = Vec::new();
        let mut c_props = Vec::new();
        for (name, e) in props {
          let (c_e, ty) = self.synth(*e)?;
          let i = self.fresh_id();
          tys.push((name.clone(), i, ty.clone()));
          c_props.push((name, Box::new(c_e)));
        }
        self.envs.pop();
        Ok((
          CE(Obj(tag.clone(), c_props)),
          Rc::new(Val(ValF::Sigma(tag, tys))),
        ))
      }
      Prop(tag, e, name) => {
        let (c_e, ty) = self.synth(*e)?;
        match &ty.0 {
          ValF::Sigma(etag, props) => {
            if tag != *etag {
              return Err("synth: prop".to_string());
            }
            for (index, (n, _, ty)) in props.iter().enumerate() {
              if name == *n {
                let e = self.ev.eval(&c_e);
                let mut ty = ty.clone();
                // ty may depend on previous props
                for (n, i, _) in props.iter().take(index).rev() {
                  let p = self.ev.prop(tag.clone(), e.clone(), *n); // e.n
                  ty = self.ev.subst(*i, &p, &ty).into();
                }
                return Ok((CE(Prop(tag, Box::new(c_e), name)), ty));
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
    self.envs.push(Env::new());
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
          let tm = self.ev.eval(&c_expr);
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
