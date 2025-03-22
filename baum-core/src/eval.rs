use crate::types::common::*;
use crate::types::val::*;

type Result<T> = std::result::Result<T, String>;

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

  pub fn unify(&mut self, v1: &Term<P, S>, v2: &Term<P, S>) -> Result<Vec<(HoleId, Term<P, S>)>> {
    match (&v1.0, &v2.0) {
      (ValF::Hole(i1), _) => Ok(vec![(*i1, v2.clone())]),
      (_, ValF::Hole(i2)) => Ok(vec![(*i2, v1.clone())]),
      (ValF::Neu(i1, ks1), ValF::Neu(i2, ks2)) if ks1.is_empty() && ks2.is_empty() => {
        if i1 == i2 {
          Ok(Vec::new())
        } else {
          Err("unify: different neutral terms".to_string()) // no.
        }
      }
      (ValF::Uni, ValF::Uni) => Ok(Vec::new()),
      _ => {
        eprintln!("unify: {:?} = {:?}", v1, v2);
        Err("unimplemented".to_string())
      }
    }
  }
}
