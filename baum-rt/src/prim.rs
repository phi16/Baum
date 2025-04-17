use baum_core::types::literal::Literal;

use crate::types::tree::{app, Action, Raw, Thunk, Val};
use std::rc::Rc;

pub fn prim(name: &str) -> Val {
  // eprintln!("Prim: {}", name);
  if name == "rt/lit/nat" || name == "rt/lit/rat" || name == "rt/lit/chr" || name == "rt/lit/str" {
    Val::Unit
  } else if name == "rt/u32" {
    Val::Unit
  } else if name == "rt/u32/0" {
    Val::Raw(Raw::U32(0))
  } else if name == "rt/u32/1" {
    Val::Raw(Raw::U32(1))
  } else if name == "rt/u32/add" {
    Val::Prim("rt/u32/add".to_string(), 2, vec![])
  } else if name == "rt/!" {
    Val::Unit
  } else if name == "rt/print" {
    Val::Prim("rt/print".to_string(), 2, vec![])
  } else if name == "rt/put" {
    Val::Prim("rt/put".to_string(), 2, vec![])
  } else if name == "rt/concat" {
    Val::Prim("rt/concat".to_string(), 2, vec![])
  } else if name == "rt/exit" {
    Val::Raw(Raw::Done)
  } else {
    panic!()
  }
}

pub fn lit(lit: Literal) -> Val {
  match lit {
    Literal::Nat(n) => Val::Raw(Raw::U32(n.into())),
    Literal::Rat(r) => Val::Raw(Raw::F32(r.into())),
    Literal::Chr(c) => Val::Raw(Raw::Char(c)),
    Literal::Str(s) => Val::Raw(Raw::String(s)),
    _ => unreachable!(),
  }
}

fn take_u32(v: Val) -> u32 {
  match v {
    Val::Raw(Raw::U32(x)) => x,
    _ => unreachable!(),
  }
}

fn take_str(v: Val) -> String {
  match v {
    Val::Raw(Raw::String(s)) => s,
    _ => unreachable!(),
  }
}

pub fn prim_ev(name: &str, args: Vec<Val>) -> Thunk {
  // eprintln!("Ev: {:?}{:?}", name, args);
  let mut args = args.into_iter();
  if name == "rt/u32/add" {
    assert_eq!(args.len(), 2);
    let x = take_u32(args.next().unwrap());
    let y = take_u32(args.next().unwrap());
    let z = x + y;
    Thunk::val(Val::Raw(Raw::U32(z)))
  } else if name == "rt/print" {
    assert_eq!(args.len(), 2);
    let n = take_u32(args.next().unwrap());
    let k = args.next().unwrap();
    Thunk::val(Val::Raw(Raw::Action(Action {
      run: Rc::new(move || {
        println!("{}", n);
        app(k.clone(), Val::Unit)
      }),
    })))
  } else if name == "rt/concat" {
    assert_eq!(args.len(), 2);
    let s1 = take_str(args.next().unwrap());
    let s2 = take_str(args.next().unwrap());
    let s3 = format!("{}{}", s1, s2);
    Thunk::val(Val::Raw(Raw::String(s3)))
  } else if name == "rt/put" {
    assert_eq!(args.len(), 2);
    let s = take_str(args.next().unwrap());
    let k = args.next().unwrap();
    Thunk::val(Val::Raw(Raw::Action(Action {
      run: Rc::new(move || {
        print!("{}", s);
        app(k.clone(), Val::Unit)
      }),
    })))
  } else {
    unreachable!()
  }
}
