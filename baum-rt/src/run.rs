use crate::pretty::{ppk, ppt, ppv};
use crate::prim::Prim;
use crate::types::code::{Code, FunIx, Global, Op, OpIx, Ref};
use crate::types::val::{app, prop, Cont, Raw, Thunk, ThunkOp, Val, ValF, V};
use std::collections::HashMap;
use std::rc::Rc;

struct Run {
  funs: Vec<Code>,
  prim: Prim,
}

impl Run {
  fn new(funs: Vec<Code>) -> Self {
    Run {
      funs,
      prim: Prim::new(),
    }
  }

  fn step(&self, fun: FunIx, env: Vec<V>) -> Thunk {
    self.step_cont(fun, env, Vec::new(), 0)
  }

  fn step_cont(&self, fun: FunIx, env: Vec<V>, res: Vec<V>, start_ix: OpIx) -> Thunk {
    let code = &self.funs[fun as usize];
    let mut res = res;
    for (opix, op) in code.ops.iter().enumerate().skip(start_ix as usize) {
      // eprintln!("* {:?}-{:?}", fun, opix);
      let r = match op {
        Op::Ref(Ref::Env(i)) => Rc::clone(&env[*i as usize]),
        Op::Ref(Ref::Arg) => Rc::clone(&env[env.len() - 1]),
        Op::Unit => Rc::clone(&self.prim.unit),
        Op::Prim(s) => Rc::new(self.prim.prim(s)), // ?
        Op::Lit(l) => Rc::new(self.prim.lit(l.clone())), // ?

        Op::Lam(f, e) => {
          let mut es = Vec::new();
          for i in e {
            es.push(Rc::clone(&res[*i as usize]));
          }
          Rc::new(Val(ValF::Cl(*f, es)))
        }
        Op::App(f, x) => {
          let f = Rc::clone(&res[*f as usize]);
          let x = Rc::clone(&res[*x as usize]);
          let mut t = app(f, x);
          t.push_cont(Cont {
            fun,
            env,
            res,
            opix: opix as OpIx,
          });
          return t;
        }
        Op::Obj(props) => {
          let mut ps = HashMap::new();
          for (n, e) in props {
            ps.insert(*n, Rc::clone(&res[*e as usize]));
          }
          Rc::new(Val(ValF::Obj(ps)))
        }
        Op::Prop(o, n) => {
          let o = Rc::clone(&res[*o as usize]);
          let mut t = prop(o, n);
          t.push_cont(Cont {
            fun,
            env,
            res,
            opix: opix as OpIx,
          });
          return t;
        }
      };
      res.push(r);
    }
    let result = Rc::clone(&res[code.ret as usize]);
    // eprintln!("* {:?}-end", fun);
    Thunk::val(result)
  }

  fn eval(&self, thunk: Thunk) -> V {
    let mut conts = Vec::new();
    let mut thunk = thunk;
    loop {
      // eprintln!("[Thunk] {}", ppk(&value));
      let (op, ks) = thunk.into_inner();
      conts.extend(ks.into_iter().rev());
      // eprintln!("[Cont: {:?}]", conts.len());
      match op {
        ThunkOp::Val(v) => match conts.pop() {
          None => return v,
          Some(k) => {
            let mut res = k.res;
            res.push(v);
            thunk = self.step_cont(k.fun, k.env, res, k.opix as OpIx + 1);
          }
        },
        ThunkOp::Eval(fun, env, arg) => {
          let mut env = env;
          env.push(arg);
          thunk = self.step(fun, env)
        }
        ThunkOp::Prim(name, args) => thunk = self.prim.ev(&name, args),
      }
    }
  }
}

pub fn run(g: &Global) {
  eprintln!("[Run]");
  eprintln!("--------");
  let run = Run::new(g.funs.clone());
  let mut thunk = run.step(g.main, Vec::new());
  loop {
    let v = Rc::unwrap_or_clone(run.eval(thunk));
    match v.0 {
      ValF::Raw(Raw::Action(a)) => {
        thunk = (a.run)();
      }
      ValF::Raw(Raw::Done) => {
        eprintln!("--------");
        eprintln!("[Done]");
        return;
      }
      _ => unreachable!(),
    }
  }
}
