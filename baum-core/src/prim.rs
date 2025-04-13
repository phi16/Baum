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

  pub fn ty<T, P: Tag, S: Tag>(&mut self, name: &str) -> Result<Expr<T, P, S>> {
    if name == "rt/u32" {
      return Ok(Expr(ExprF::Uni));
    } else if name == "rt/u32/0" {
      return Ok(self.prim("rt/u32"));
    } else if name == "rt/u32/1" {
      return Ok(self.prim("rt/u32"));
    } else if name == "rt/u32/2" {
      return Ok(self.prim("rt/u32"));
    } else if name == "rt/u32/add" {
      return Ok(Expr(ExprF::Pi(
        Default::default(),
        Vis::Explicit,
        self.fresh_bind(),
        Box::new(self.prim("rt/u32")),
        Box::new(Expr(ExprF::Pi(
          Default::default(),
          Vis::Explicit,
          self.fresh_bind(),
          Box::new(self.prim("rt/u32")),
          Box::new(self.prim("rt/u32")),
        ))),
      )));
    } else if name == "rt/!" {
      return Ok(Expr(ExprF::Uni));
    } else if name == "rt/test" {
      return Ok(self.prim("rt/!"));
    } else {
      return Err(format!("unknown prim: {}", name));
    }
  }
}
