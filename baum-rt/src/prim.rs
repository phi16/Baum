use crate::types::tree::{app, Action, Raw, Thunk, Val};
use std::rc::Rc;

pub fn prim(name: &str) -> Val {
  // eprintln!("Prim: {}", name);
  if name == "rt/u32" {
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
  } else if name == "rt/exit" {
    Val::Raw(Raw::Done)
  } else {
    panic!()
  }
}

fn take_u32(v: Val) -> u32 {
  match v {
    Val::Raw(Raw::U32(x)) => x,
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
    Thunk::Val(Val::Raw(Raw::U32(z)))
  } else if name == "rt/print" {
    assert_eq!(args.len(), 2);
    let n = take_u32(args.next().unwrap());
    let k = args.next().unwrap();
    Thunk::Val(Val::Raw(Raw::Action(Action {
      run: Rc::new(move || {
        println!("Print: {}", n);
        app(k.clone(), Val::Unit)
      }),
    })))
  } else {
    unreachable!()
  }
}
