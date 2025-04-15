use crate::types::common::{BindId, Tag};
use crate::types::tree::{Expr, ExprF, Vis};

type Result<T> = std::result::Result<T, String>;

pub struct PrimMap {
  next_bind_id: Option<u32>,
}

impl PrimMap {
  pub fn new() -> Self {
    PrimMap { next_bind_id: None }
  }

  pub fn set_bind_id(&mut self, id: u32) {
    if self.next_bind_id.is_some() {
      panic!()
    }
    self.next_bind_id = Some(id);
  }

  pub fn fresh_bind(&mut self) -> BindId {
    match &mut self.next_bind_id {
      Some(i) => {
        let id = *i;
        *i += 1;
        BindId(id)
      }
      None => panic!(),
    }
  }

  pub fn take_bind_id(&mut self) -> u32 {
    let id = std::mem::take(&mut self.next_bind_id).unwrap();
    id
  }

  pub fn prim<T, P, S>(&self, name: &str) -> Expr<T, P, S> {
    return Expr(ExprF::Prim(name.to_string()));
  }

  pub fn fun<T, P: Tag, S: Tag>(
    &mut self,
    from: Expr<T, P, S>,
    to: Expr<T, P, S>,
  ) -> Expr<T, P, S> {
    return Expr(ExprF::Pi(
      Default::default(),
      Vis::Explicit,
      self.fresh_bind(),
      Box::new(from),
      Box::new(to),
    ));
  }

  pub fn ty<T: Clone, P: Tag, S: Tag>(&mut self, name: &str) -> Result<Expr<T, P, S>> {
    if name == "rt/u32" {
      return Ok(Expr(ExprF::Uni));
    } else if name == "rt/u32/0" {
      return Ok(self.prim("rt/u32"));
    } else if name == "rt/u32/1" {
      return Ok(self.prim("rt/u32"));
    } else if name == "rt/u32/add" {
      let u = self.prim("rt/u32");
      let u2u = self.fun(u.clone(), u.clone());
      return Ok(self.fun(u, u2u));
    } else if name == "rt/!" {
      return Ok(Expr(ExprF::Uni));
    } else if name == "rt/print" {
      let u = self.prim("rt/u32");
      let zero = Expr(ExprF::Sigma(Default::default(), vec![]));
      let bang = self.prim("rt/!");
      let cb = self.fun(zero, bang.clone());
      let cb2bang = self.fun(cb, bang);
      return Ok(self.fun(u, cb2bang));
    } else if name == "rt/exit" {
      return Ok(self.prim("rt/!"));
    } else {
      return Err(format!("unknown prim: {}", name));
    }
  }
}
