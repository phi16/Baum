use crate::types::runtime::*;
use std::{
  cell::RefCell,
  io::{self, Write},
  rc::Rc,
};

fn fn_add(args: Vec<&Value>) -> Value {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Value::Unit();
  }
  match (&args[0], &args[1]) {
    (&Value::Int(i1), &Value::Int(i2)) => Value::Int(i1 + i2),
    (&Value::Float(f1), &Value::Float(f2)) => Value::Float(f1 + f2),
    _ => {
      eprintln!("expected int or float");
      Value::Unit()
    }
  }
}

fn fn_concat(args: Vec<&Value>) -> Value {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Value::Unit();
  }
  match (&args[0], &args[1]) {
    (&Value::String(ref s1), &Value::String(ref s2)) => Value::String(s1.clone() + s2),
    _ => {
      eprintln!("expected string");
      Value::Unit()
    }
  }
}

fn fn_is0(args: Vec<&Value>) -> Value {
  if args.len() != 3 {
    eprintln!("expected 3 args");
    return Value::Unit();
  }
  match args[0] {
    Value::Int(i) => {
      if *i == 0 {
        args[1].clone()
      } else {
        args[2].clone()
      }
    }
    _ => {
      eprintln!("expected int");
      Value::Unit()
    }
  }
}

fn fn_decr(args: Vec<&Value>) -> Value {
  if args.len() != 1 {
    eprintln!("expected 1 arg");
    return Value::Unit();
  }
  match args[0] {
    Value::Int(i) => Value::Int(i - 1),
    _ => {
      eprintln!("expected int");
      Value::Unit()
    }
  }
}

fn action_print(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  match args[0] {
    Value::Unit() => print!("()"),
    Value::Int(i) => print!("{}", i),
    Value::Float(f) => print!("{}", f),
    Value::Char(c) => print!("{}", c),
    Value::String(ref s) => print!("{}", s),
    Value::Cl(_, _) => print!("[cl]"),
    Value::Action(_) => print!("[action]"),
    Value::PrimFun(_) => print!("[prim]"),
    Value::Store(_) => print!("[store]"),
  };
  let ret = Value::Unit();
  Action::App(Box::new(args[1].clone()), vec![ret])
}

fn action_readline(args: &Vec<Value>) -> Action {
  if args.len() != 1 {
    eprintln!("expected 1 arg");
    return Action::Stop;
  }
  // read line from stdin
  io::stdout().flush().unwrap();
  let mut s = String::new();
  io::stdin().read_line(&mut s).unwrap();
  let ret = Value::String(s.trim().to_string());
  Action::App(Box::new(args[0].clone()), vec![ret])
}

fn action_fork(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  match (&args[0], &args[1]) {
    (Value::Action(a0), Value::Action(a1)) => Action::Fork(vec![a0.clone(), a1.clone()]),
    _ => {
      eprintln!("expected action");
      Action::Stop
    }
  }
}

fn action_sleep(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  match (&args[0], &args[1]) {
    (&Value::Int(delay), &Value::Action(ref a)) => Action::Delay(delay as u32, Box::new(a.clone())),
    _ => {
      eprintln!("expected int and action");
      Action::Stop
    }
  }
}

fn action_store(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  let store = Rc::new(RefCell::new(args[0].clone()));
  let ret = Value::Store(store);
  Action::App(Box::new(args[1].clone()), vec![ret])
}

fn action_read(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  match args[0] {
    Value::Store(ref store) => {
      let ret = store.borrow().clone();
      Action::App(Box::new(args[1].clone()), vec![ret])
    }
    _ => {
      eprintln!("expected store");
      Action::Stop
    }
  }
}

fn action_write(args: &Vec<Value>) -> Action {
  if args.len() != 3 {
    eprintln!("expected 3 args");
    return Action::Stop;
  }
  match args[0] {
    Value::Store(ref store) => {
      let mut store = store.borrow_mut();
      *store = args[1].clone();
      Action::App(Box::new(args[2].clone()), vec![Value::Unit()])
    }
    _ => {
      eprintln!("expected store");
      Action::Stop
    }
  }
}

macro_rules! action_wrap {
  ($f:expr) => {{
    fn f_wrap(args: Vec<&Value>) -> Value {
      Value::Action(Action::Call(
        $f,
        args.into_iter().map(|x| x.clone()).collect(),
      ))
    };
    Value::PrimFun(f_wrap)
  }};
}

pub fn prim_value(name: &str) -> Value {
  match name {
    "void" => Value::Action(Action::Stop),
    "add" => Value::PrimFun(fn_add),
    "concat" => Value::PrimFun(fn_concat),
    "is0" => Value::PrimFun(fn_is0),
    "decr" => Value::PrimFun(fn_decr),
    "print" => action_wrap!(action_print),
    "readline" => action_wrap!(action_readline),
    "fork" => action_wrap!(action_fork),
    "sleep" => action_wrap!(action_sleep),
    "store" => action_wrap!(action_store),
    "read" => action_wrap!(action_read),
    "write" => action_wrap!(action_write),
    // box, send, receive, peek
    _ => {
      eprintln!("unknown prim {:?}", name);
      Value::Action(Action::Stop)
    }
  }
}
