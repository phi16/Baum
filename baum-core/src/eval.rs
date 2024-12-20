use std::collections::BinaryHeap;
use std::os::windows::process;

use crate::eval_lib::*;
use crate::types::runtime::*;

#[derive(Debug, Clone)]
struct DelayedActions((u64, u32), Action);

impl Eq for DelayedActions {}

impl PartialEq for DelayedActions {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

impl Ord for DelayedActions {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    other.0.cmp(&self.0)
  }
}

impl PartialOrd for DelayedActions {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

struct Scheduler {
  actions: BinaryHeap<DelayedActions>, // hmm... very inefficient...
  next_id: u32,
  current_time: u64,
}

impl Scheduler {
  fn new(a: Action) -> Self {
    let mut actions = BinaryHeap::new();
    actions.push(DelayedActions((0, 0), a));
    Scheduler {
      actions,
      next_id: 1,
      current_time: 0,
    }
  }

  fn push(&mut self, delay: u32, a: Action) {
    self.actions.push(DelayedActions(
      (self.current_time + delay as u64, self.next_id),
      a,
    ));
    self.next_id += 1;
  }

  fn pop(&mut self) -> Option<Action> {
    let DelayedActions((d, _), a) = self.actions.pop()?;
    if d > self.current_time {
      std::thread::sleep(std::time::Duration::from_millis(d - self.current_time));
      self.current_time = d;
    }
    Some(a)
  }
}

struct Eval<'a> {
  runtime: &'a Runtime,
}

impl<'a> Eval<'a> {
  pub fn new(runtime: &'a Runtime) -> Self {
    Eval { runtime }
  }

  fn eval_lit(&self, lit: &L) -> Value {
    match lit {
      L::Num(s) => match s.parse() {
        Ok(i) => Value::Int(i),
        Err(_) => Value::Float(s.parse().unwrap()),
      },
      L::Chr(s) => Value::Char(s.chars().next().unwrap()),
      L::Str(s) => Value::String(s.clone()),
    }
  }

  fn apply(&self, fun: &Value, args: Vec<&Value>) -> Value {
    match fun {
      Value::Cl(loc, envs) => {
        let fun = self.runtime.funs.get(loc).unwrap();
        if args.len() != fun.args.len() {
          eprintln!("expected {} args ({:?})", fun.args.len(), fun.args);
        }
        self.run(&fun.body, &args, &envs)
      }
      Value::PrimFun(f) => f(args),
      _ => {
        eprintln!("not a function {:?}", fun);
        Value::Action(Action::Stop)
      }
    }
  }

  fn run(&self, body: &Body, args: &Vec<&Value>, envs: &Vec<Value>) -> Value {
    let mut locals: Vec<Value> = Vec::new();
    for (i, e) in body.ls.iter().enumerate() {
      let v = match e {
        E::DefRef(loc) => {
          let def = self.runtime.defs.get(loc).unwrap();
          match def {
            D::Def(body) => self.run(body, &Vec::new(), &Vec::new()),
            D::Mod(_) => unimplemented!(),
          }
        }
        E::ArgRef(loc) => args[loc.0 as usize].clone(),
        E::EnvRef(loc) => envs[loc.0 as usize].clone(),
        E::Lit(lit) => self.eval_lit(lit),
        E::Cl(loc, args) => {
          let args = args.iter().map(|a| locals[a.0 as usize].clone()).collect();
          Value::Cl(*loc, args)
        }
        E::App(cl, args) => {
          let fun = &locals[cl.0 as usize];
          let args = args.iter().map(|a| &locals[a.0 as usize]).collect();
          self.apply(fun, args)
        }
        E::Hole => {
          eprintln!("hole!");
          Value::Action(Action::Stop)
        }
        E::Prim(name) => prim_value(name),
      };
      locals.push(v);
    }
    locals[body.result.0 as usize].clone()
    // TODO: tail call optimization
  }

  fn process_action(&self, a: Action) {
    let mut q = Scheduler::new(a);
    while let Some(a) = q.pop() {
      match a {
        Action::Stop => {}
        Action::Fork(actions) => {
          for a in actions {
            q.push(0, a);
          }
        }
        Action::Call(f, args) => {
          let args = args.iter().map(|v| v.clone()).collect();
          q.push(0, f(&args));
        }
        Action::App(fun, args) => {
          let args = args.iter().collect();
          match self.apply(&*fun, args) {
            Value::Action(a) => q.push(0, a),
            _ => {}
          }
        }
        Action::Delay(delay, a) => q.push(delay, *a),
      }
    }
  }

  fn eval(&self, name: &str) {
    let main_loc = self.runtime.lookup.get("main").unwrap();
    let v = match self.runtime.defs.get(main_loc).unwrap() {
      D::Def(body) => self.run(body, &Vec::new(), &Vec::new()),
      D::Mod(_) => {
        eprintln!("{} is a module", name);
        return;
      }
    };
    fn default_print(args: &Vec<Value>) -> Action {
      let v = &args[0];
      match v {
        Value::Unit() => println!("Result: ()"),
        Value::Int(i) => println!("Result: {}", i),
        Value::Float(f) => println!("Result: {}", f),
        Value::Char(c) => println!("Result: {}", c),
        Value::String(s) => println!("Result: {}", s),
        Value::Cl(_, _) => println!("Result: [cl]"),
        Value::Action(a) => return a.clone(),
        Value::PrimFun(_) => println!("Result: [prim]"),
        Value::Store(_) => println!("Result: [store]"),
      }
      return Action::Stop;
    }
    self.process_action(Action::Call(default_print, vec![v]));
  }
}

pub fn eval_main(runtime: &Runtime) {
  let eval = Eval::new(runtime);
  eval.eval("main");
}
