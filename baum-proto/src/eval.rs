use std::collections::VecDeque;
use std::io::Write;
use std::sync::{mpsc, Arc, Mutex};

use crate::types::code::*;
use crate::types::runtime::*;

struct Scheduler {
  actions: VecDeque<Action>,
  waiting: u32,
  awake: Option<mpsc::Sender<()>>,
}

impl Scheduler {
  fn new() -> Self {
    Scheduler {
      actions: VecDeque::new(),
      waiting: 0,
      awake: None,
    }
  }

  fn push(&mut self, a: Action) {
    self.actions.push_back(a);
  }

  fn push_async(&mut self, a: Action) {
    self.waiting -= 1;
    self.actions.push_back(a);
    if let Some(tx) = &self.awake {
      tx.send(()).unwrap();
    }
  }

  fn pop(&mut self) -> Option<Action> {
    self.actions.pop_front()
  }

  fn add_task(&mut self) {
    self.waiting += 1;
  }

  fn has_action(&self) -> bool {
    !self.actions.is_empty()
  }

  fn has_waiting(&self) -> bool {
    self.waiting > 0
  }
}

struct Eval<'a> {
  code: &'a Code,
  prim: Prim,
}

impl<'a> Eval<'a> {
  pub fn new(code: &'a Code) -> Self {
    Eval {
      code,
      prim: Prim::new(),
    }
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

  fn apply(&mut self, fun: &Value, args: Vec<&Value>) -> Value {
    match fun {
      Value::Cl(loc, envs) => {
        let fun = self.code.funs.get(loc).unwrap();
        if args.len() != fun.args.len() {
          eprintln!("expected {} args ({:?})", fun.args.len(), fun.args);
        }
        self.run(&fun.body, &args, &envs)
      }
      Value::PrimFun(f) => f(&mut self.prim, args),
      _ => {
        eprintln!("not a function {:?}", fun);
        Value::Action(Action::Stop)
      }
    }
  }

  fn run(&mut self, body: &Body, args: &Vec<&Value>, envs: &Vec<Value>) -> Value {
    let mut locals: Vec<Value> = Vec::new();
    for (i, e) in body.ls.iter().enumerate() {
      let v = match e {
        E::DefRef(loc) => {
          let def = self.code.defs.get(loc).unwrap();
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
        E::Prim(name) => self.prim.prim_value(name),
      };
      locals.push(v);
    }
    locals[body.result.0 as usize].clone()
    // TODO: tail call optimization
  }

  fn run_action(&mut self, qu: Arc<Mutex<Scheduler>>, a: Action) {
    match a {
      Action::Stop => {}
      Action::Fork(actions) => {
        let mut q = qu.lock().unwrap();
        for a in actions {
          q.push(a);
        }
      }
      Action::Call(f, args) => {
        let args = args.iter().map(|v| v.clone()).collect();
        let a = f(&mut self.prim, &args);
        let mut q = qu.lock().unwrap();
        q.push(a);
      }
      Action::App(fun, args) => {
        let args = args.iter().collect();
        match self.apply(&*fun, args) {
          Value::Action(a) => {
            let mut q = qu.lock().unwrap();
            q.push(a)
          }
          _ => {}
        }
      }
      Action::Async(Async(f)) => {
        let qu2 = Arc::clone(&qu);
        let handle = tokio::task::spawn(async move {
          let a = f.await;
          let mut q = qu2.lock().unwrap();
          q.push_async(a);
        });
        let mut q = qu.lock().unwrap();
        q.add_task();
      }
    }
  }

  fn process_action(&mut self, a: Action) {
    let mut squ = Scheduler::new();
    squ.push(a);
    let qu = Arc::new(Mutex::new(squ));
    tokio::runtime::Runtime::new().unwrap().block_on(async {
      loop {
        loop {
          let aqu = Arc::clone(&qu);
          let sa = {
            let mut q = aqu.lock().unwrap();
            q.pop()
          };
          match sa {
            Some(a) => self.run_action(Arc::clone(&qu), a),
            None => break,
          }
        }
        let aqu = Arc::clone(&qu);
        let (has_action, has_waiting) = {
          let q = aqu.lock().unwrap();
          (q.has_action(), q.has_waiting())
        };
        if has_action {
          continue;
        }
        if !has_waiting {
          break;
        }
        let (tx, rx) = mpsc::channel();
        let mut q = aqu.lock().unwrap();
        q.awake = Some(tx);
        drop(q);
        rx.recv().unwrap();
        let mut q = aqu.lock().unwrap();
        q.awake = None;
      }
    })
  }

  fn eval(&mut self, name: &str) {
    let main_loc = self.code.lookup.get("main").unwrap();
    let v = match self.code.defs.get(main_loc).unwrap() {
      D::Def(body) => self.run(body, &Vec::new(), &Vec::new()),
      D::Mod(_) => {
        eprintln!("{} is a module", name);
        return;
      }
    };
    fn default_print(prim: &mut Prim, args: &Vec<Value>) -> Action {
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

pub fn eval_main(code: &Code) {
  let mut eval = Eval::new(code);
  eval.eval("main");
}
