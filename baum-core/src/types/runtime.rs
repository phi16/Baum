use crate::types::code::*;
use core::panic;
use rustyline_async::{Readline, SharedWriter};
use std::{
  fmt,
  future::Future,
  pin::Pin,
  sync::{Arc, Mutex},
};
use tokio::sync::Mutex as TokioMutex;

pub struct Prim {
  pub some_rl: Arc<TokioMutex<Option<Readline>>>,
  pub some_sw: Arc<TokioMutex<Option<SharedWriter>>>,
}

pub struct Async(pub Pin<Box<dyn Future<Output = Action> + Send>>);

impl Clone for Async {
  fn clone(&self) -> Self {
    panic!("[async] cannot be cloned")
  }
}

impl fmt::Debug for Async {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "[async]")
  }
}

#[derive(Debug, Clone)]
pub enum Action {
  Stop,
  Fork(Vec<Action>),
  Call(fn(&mut Prim, &Vec<Value>) -> Action, Vec<Value>),
  App(Box<Value>, Vec<Value>),
  Async(Async),
}

#[derive(Debug, Clone)]
pub enum Value {
  Unit(),
  Int(i32),
  Float(f32),
  Char(char),
  String(String),
  Cl(FunLoc, Vec<Value>),
  Action(Action),
  PrimFun(fn(&mut Prim, Vec<&Value>) -> Value),
  Store(Arc<Mutex<Value>>),
}
