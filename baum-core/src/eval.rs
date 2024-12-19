use std::os::windows::process;

use crate::eval_lib::*;
use crate::types::runtime::*;

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
  }

  fn process_action(&self, a: Action) {
    let mut a = a;
    loop {
      match a {
        Action::Stop => return,
        Action::Fork(actions) => {
          unimplemented!()
        }
        Action::Call(f, args) => {
          let args = args.iter().map(|v| v.clone()).collect();
          a = f(&args);
        }
        Action::App(fun, args) => {
          let args = args.iter().collect();
          self.process_value(self.apply(&*fun, args));
          return;
        }
      }
    }
  }

  fn process_value(&self, v: Value) {
    match v {
      Value::Unit() => println!("()"),
      Value::Int(i) => println!("{}", i),
      Value::Float(f) => println!("{}", f),
      Value::Char(c) => println!("{}", c),
      Value::String(s) => println!("{}", s),
      Value::Cl(_, _) => println!("[cl]"),
      Value::Action(a) => self.process_action(a),
      Value::PrimFun(_) => println!("[prim]"),
    }
  }

  fn eval(&self, name: &str) {
    let main_loc = self.runtime.lookup.get("main").unwrap();
    match self.runtime.defs.get(main_loc).unwrap() {
      D::Def(body) => {
        let v = self.run(body, &Vec::new(), &Vec::new());
        self.process_value(v);
      }
      D::Mod(_) => eprintln!("{} is a module", name),
    }
  }
}

pub fn eval_main(runtime: &Runtime) {
  let eval = Eval::new(runtime);
  eval.eval("main");
}
