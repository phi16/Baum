use rustyline_async::{Readline, ReadlineEvent};

use crate::types::runtime::*;
use tokio::time::*;

use core::panic;
use std::{
  io::{self, stdout, Write},
  sync::{Arc, Mutex},
};
use tokio::sync::Mutex as TokioMutex;

macro_rules! def_action {
  ($f:expr, $n:ident) => {
    fn $n(&mut self, args: Vec<&Value>) -> Value {
      Value::Action(Action::Call(
        $f,
        args.into_iter().map(|x| x.clone()).collect(),
      ))
    }
  };
}

impl Prim {
  pub fn new() -> Self {
    Prim {
      some_rl: Arc::new(TokioMutex::new(None)),
      some_sw: Arc::new(TokioMutex::new(None)),
    }
  }

  fn fn_add(&mut self, args: Vec<&Value>) -> Value {
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

  fn fn_concat(&mut self, args: Vec<&Value>) -> Value {
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

  fn fn_is0(&mut self, args: Vec<&Value>) -> Value {
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

  fn fn_decr(&mut self, args: Vec<&Value>) -> Value {
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

  fn action_print(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 2 {
      eprintln!("expected 2 args");
      return Action::Stop;
    }
    let s = match args[0] {
      Value::Unit() => "()".to_string(),
      Value::Int(i) => format!("{}", i),
      Value::Float(f) => format!("{}", f),
      Value::Char(c) => format!("{}", c),
      Value::String(ref s) => s.clone(),
      Value::Cl(_, _) => "[cl]".to_string(),
      Value::Action(_) => "[action]".to_string(),
      Value::PrimFun(_) => "[prim]".to_string(),
      Value::Store(_) => "[store]".to_string(),
    };
    let k = Box::new(args[1].clone());
    let some_sw = Arc::clone(&self.some_sw);
    Action::Async(Async(Box::pin(async move {
      let mut some_sw = some_sw.lock().await;
      match some_sw.as_mut() {
        Some(mut sw) => {
          sw.write(s.as_bytes()).unwrap();
          Action::App(k, vec![Value::Unit()])
        }
        None => {
          print!("{}", s);
          stdout().flush().unwrap();
          Action::App(k, vec![Value::Unit()])
        }
      }
    })))
  }

  fn action_readline(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 2 {
      eprintln!("expected 2 arg");
      return Action::Stop;
    }
    let prompt = match args[0] {
      Value::String(ref s) => s.clone(),
      _ => {
        eprintln!("expected string");
        return Action::Stop;
      }
    };
    let k = Box::new(args[1].clone());
    let some_rl = Arc::clone(&self.some_rl);
    let some_sw = Arc::clone(&self.some_sw);
    Action::Async(Async(Box::pin(async move {
      let mut srl = some_rl.lock().await;
      if srl.is_some() {
        panic!("readline already in use");
      }
      let (mut rl, sw) = Readline::new(prompt).unwrap();
      srl.replace(rl);
      drop(srl);
      some_sw.lock().await.replace(sw);

      let s = {
        let mut some_rl = some_rl.lock().await;
        let rl = some_rl.as_mut().unwrap();
        let event = rl.readline().await.unwrap();
        rl.flush().unwrap();
        event
      };
      some_rl.lock().await.take();
      some_sw.lock().await.take();
      let s = match s {
        ReadlineEvent::Line(s) => s,
        _ => return Action::Stop,
      };
      let ret = Value::String(s.to_string());
      Action::App(k, vec![ret])
    })))
  }

  fn action_fork(&mut self, args: &Vec<Value>) -> Action {
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

  fn action_sleep(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 2 {
      eprintln!("expected 2 args");
      return Action::Stop;
    }
    match (&args[0], &args[1]) {
      (&Value::Int(delay), &Value::Action(ref a)) => {
        let ac = a.clone();
        Action::Async(Async(Box::pin(async move {
          sleep(Duration::from_millis(delay as u64)).await;
          ac
        })))
      }
      _ => {
        eprintln!("expected int and action");
        Action::Stop
      }
    }
  }

  fn action_store(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 2 {
      eprintln!("expected 2 args");
      return Action::Stop;
    }
    let store = Arc::new(Mutex::new(args[0].clone()));
    let ret = Value::Store(store);
    Action::App(Box::new(args[1].clone()), vec![ret])
  }

  fn action_read(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 2 {
      eprintln!("expected 2 args");
      return Action::Stop;
    }
    match args[0] {
      Value::Store(ref store) => {
        let ret = store.lock().unwrap();
        let v = ret.clone();
        Action::App(Box::new(args[1].clone()), vec![v])
      }
      _ => {
        eprintln!("expected store");
        Action::Stop
      }
    }
  }

  fn action_write(&mut self, args: &Vec<Value>) -> Action {
    if args.len() != 3 {
      eprintln!("expected 3 args");
      return Action::Stop;
    }
    match args[0] {
      Value::Store(ref store) => {
        let mut store = store.lock().unwrap();
        *store = args[1].clone();
        Action::App(Box::new(args[2].clone()), vec![Value::Unit()])
      }
      _ => {
        eprintln!("expected store");
        Action::Stop
      }
    }
  }

  def_action!(Prim::action_print, fn_print);
  def_action!(Prim::action_readline, fn_readline);
  def_action!(Prim::action_fork, fn_fork);
  def_action!(Prim::action_sleep, fn_sleep);
  def_action!(Prim::action_store, fn_store);
  def_action!(Prim::action_read, fn_read);
  def_action!(Prim::action_write, fn_write);

  pub fn prim_value(&self, name: &str) -> Value {
    match name {
      "void" => Value::Action(Action::Stop),
      _ => Value::PrimFun(match name {
        "add" => Prim::fn_add,
        "concat" => Prim::fn_concat,
        "is0" => Prim::fn_is0,
        "decr" => Prim::fn_decr,
        "print" => Prim::fn_print,
        "readline" => Prim::fn_readline,
        "fork" => Prim::fn_fork,
        "sleep" => Prim::fn_sleep,
        "store" => Prim::fn_store,
        "read" => Prim::fn_read,
        "write" => Prim::fn_write,
        // box, send, receive, peek
        _ => {
          eprintln!("unknown prim {:?}", name);
          return Value::Action(Action::Stop);
        }
      }),
    }
  }
}
