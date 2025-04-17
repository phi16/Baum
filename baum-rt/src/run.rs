use crate::pretty::{ppk, ppt, ppv};
use crate::prim::{prim, prim_ev};
use crate::types::tree::{app, prop, Cont, Env, Op, Raw, Thunk, Tree, TreeF, Val};
use std::collections::HashMap;
use std::rc::Rc;

fn step(t: &Rc<Tree>, env: Env) -> Thunk {
  match &t.0 {
    TreeF::Var(i) => Thunk::Val(env.get(i).unwrap().clone()),
    TreeF::Unit => Thunk::Val(Val::Unit),
    TreeF::Prim(s) => Thunk::Val(prim(s)),
    TreeF::Let(ds, e) => {
      let mut env = env;
      for (i, d) in ds {
        let v = eval(step(d, env.clone()));
        Rc::make_mut(&mut env).insert(*i, v);
      }
      Thunk::Op(Op::Eval(e.clone(), env), Vec::new())
    }

    TreeF::Lam(i, scope, e) => {
      let env = scope
        .iter()
        .map(|i| (*i, env.get(i).unwrap().clone()))
        .collect::<HashMap<_, _>>();
      Thunk::Val(Val::Cl(*i, Rc::new(env), e.clone()))
    }
    TreeF::App(f, x) => {
      let f = step(f, env.clone());
      let x = eval(step(x, env.clone()));
      match f {
        Thunk::Val(f) => app(f, x),
        Thunk::Op(op, mut ks) => {
          ks.push(Cont::App(x));
          Thunk::Op(op, ks)
        }
      }
    }
    TreeF::Obj(props) => {
      let mut ps = HashMap::new();
      for (n, e) in props {
        let v = eval(step(e, env.clone()));
        ps.insert(*n, v);
      }
      Thunk::Val(Val::Obj(ps))
    }
    TreeF::Prop(o, n) => {
      let o = step(o, env);
      match o {
        Thunk::Val(o) => prop(o, n),
        Thunk::Op(op, mut ks) => {
          ks.push(Cont::Prop(*n));
          Thunk::Op(op, ks)
        }
      }
    }
  }
}

fn eval(value: Thunk) -> Val {
  let mut conts = Vec::new();
  let mut value = value;
  loop {
    // eprintln!("[Thunk] {}", ppk(&value));
    match value {
      Thunk::Val(v) => match conts.pop() {
        None => return v,
        Some(Cont::App(x)) => value = app(v, x),
        Some(Cont::Prop(n)) => value = prop(v, &n),
      },
      Thunk::Op(op, ks) => {
        value = match op {
          Op::Eval(t, env) => step(&t, env),
          Op::Prim(name, args) => prim_ev(&name, args),
        };
        conts.extend(ks.into_iter().rev());
      }
    }
  }
}

pub fn run(t: &Rc<Tree>) {
  eprintln!("[Run]");
  eprintln!("{}", ppt(t));
  eprintln!("--------");
  let mut thunk = Thunk::Op(Op::Eval(t.clone(), Rc::new(HashMap::new())), Vec::new());
  loop {
    let v = eval(thunk);
    match v {
      Val::Raw(Raw::Action(a)) => {
        thunk = (a.run)();
      }
      Val::Raw(Raw::Done) => {
        eprintln!("Done");
        return;
      }
      _ => unreachable!(),
    }
  }
}
