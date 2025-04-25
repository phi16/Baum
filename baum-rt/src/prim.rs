use baum_core::types::literal::Literal;

use crate::types::val::{app, Action, Raw, Thunk, Val, ValF, V};
use std::rc::Rc;

fn take_u32(v: Val) -> u32 {
  match v.0 {
    ValF::Raw(Raw::U32(x)) => x,
    _ => unreachable!(),
  }
}

fn take_str(v: Val) -> String {
  match v.0 {
    ValF::Raw(Raw::String(s)) => s,
    _ => unreachable!(),
  }
}

pub struct Prim {
  pub unit: V,
}

impl Prim {
  pub fn new() -> Self {
    Prim {
      unit: Rc::new(Val(ValF::Unit)),
    }
  }

  pub fn prim(&self, name: &str) -> Val {
    // eprintln!("Prim: {}", name);
    let v = match name {
      "rt/lit/nat" | "rt/lit/rat" | "rt/lit/chr" | "rt/lit/str" => ValF::Unit,
      "rt/u32" => ValF::Unit,
      "rt/u32/0" => ValF::Raw(Raw::U32(0)),
      "rt/u32/1" => ValF::Raw(Raw::U32(1)),
      "rt/u32/add" => ValF::Prim("rt/u32/add".to_string(), 2, vec![]),
      "rt/!" => ValF::Unit,
      "rt/print" => ValF::Prim("rt/print".to_string(), 2, vec![]),
      "rt/put" => ValF::Prim("rt/put".to_string(), 2, vec![]),
      "rt/concat" => ValF::Prim("rt/concat".to_string(), 2, vec![]),
      "rt/exit" => ValF::Raw(Raw::Done),
      _ => panic!(),
    };
    Val(v)
  }

  pub fn lit(&self, lit: Literal) -> Val {
    let r = match lit {
      Literal::Nat(n) => Raw::U32(n.into()),
      Literal::Rat(r) => Raw::F32(r.into()),
      Literal::Chr(c) => Raw::Char(c),
      Literal::Str(s) => Raw::String(s),
      _ => unreachable!(),
    };
    Val(ValF::Raw(r))
  }

  pub fn ev(&self, name: &str, args: Vec<V>) -> Thunk {
    // eprintln!("Ev: {:?}{:?}", name, args);
    let mut args = args.into_iter();
    let args_len = args.len();
    let mut pick = || Rc::unwrap_or_clone(args.next().unwrap());
    if name == "rt/u32/add" {
      assert_eq!(args_len, 2);
      let x = take_u32(pick());
      let y = take_u32(pick());
      let z = x + y;
      Thunk::val(Rc::new(Val(ValF::Raw(Raw::U32(z)))))
    } else if name == "rt/print" {
      assert_eq!(args_len, 2);
      let n = take_u32(pick());
      let k = args.next().unwrap();
      let u = Rc::clone(&self.unit);
      Thunk::val(Rc::new(Val(ValF::Raw(Raw::Action(Action {
        run: Rc::new(move || {
          println!("{}", n);
          app(Rc::clone(&k), Rc::clone(&u))
        }),
      })))))
    } else if name == "rt/concat" {
      assert_eq!(args_len, 2);
      let s1 = take_str(pick());
      let s2 = take_str(pick());
      let s3 = format!("{}{}", s1, s2);
      Thunk::val(Rc::new(Val(ValF::Raw(Raw::String(s3)))))
    } else if name == "rt/put" {
      assert_eq!(args_len, 2);
      let s = take_str(pick());
      let k = args.next().unwrap();
      let u = Rc::clone(&self.unit);
      Thunk::val(Rc::new(Val(ValF::Raw(Raw::Action(Action {
        run: Rc::new(move || {
          print!("{}", s);
          app(Rc::clone(&k), Rc::clone(&u))
        }),
      })))))
    } else {
      unreachable!()
    }
  }
}
