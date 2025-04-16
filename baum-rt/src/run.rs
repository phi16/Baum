use crate::pretty::pretty;
use crate::prim::{prim, prim_ev};
use crate::types::tree::{Cont, Env, Name, Raw, Thunk, Tree, TreeF, Val};
use std::collections::HashMap;
use std::rc::Rc;

pub fn app(f: Val, x: Val) -> Thunk {
  match f {
    Val::Cl(i, mut e, body) => {
      Rc::make_mut(&mut e).insert(i, x);
      Thunk::Thunk(body, e, Vec::new())
    }
    Val::Prim(name, n, args) => {
      let mut args = args;
      args.push(x);
      if n == args.len() {
        prim_ev(&name, args)
      } else {
        Thunk::Val(Val::Prim(name, n, args))
      }
    }
    _ => unreachable!(),
  }
}

fn prop(o: Val, n: &Name) -> Thunk {
  match o {
    Val::Obj(ps) => Thunk::Val(ps.get(n).unwrap().clone()),
    _ => unreachable!(),
  }
}

fn step(t: &Rc<Tree>, env: Env) -> Thunk {
  match &t.0 {
    TreeF::Var(i) => Thunk::Val(env.get(i).unwrap().clone()),
    TreeF::Unit => Thunk::Val(Val::Type),
    TreeF::Prim(s) => Thunk::Val(prim(s)),
    TreeF::Let(ds, e) => {
      let mut env = env;
      for (i, d) in ds {
        let v = eval(step(d, env.clone()));
        Rc::make_mut(&mut env).insert(*i, v);
      }
      Thunk::Thunk(e.clone(), env, Vec::new())
    }

    TreeF::Lam(i, e) => Thunk::Val(Val::Cl(*i, env.clone(), e.clone())),
    TreeF::App(f, x) => {
      let f = step(f, env.clone());
      let x = eval(step(x, env.clone()));
      match f {
        Thunk::Val(f) => app(f, x),
        Thunk::Thunk(t, e, mut ks) => {
          ks.push(Cont::App(x));
          Thunk::Thunk(t, e, ks)
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
        Thunk::Thunk(t, e, mut ks) => {
          ks.push(Cont::Prop(*n));
          Thunk::Thunk(t, e, ks)
        }
      }
    }
  }
}

fn eval(value: Thunk) -> Val {
  let mut conts = Vec::new();
  let mut value = value;
  loop {
    match value {
      Thunk::Val(v) => match conts.pop() {
        None => return v,
        Some(Cont::App(x)) => value = app(v, x),
        Some(Cont::Prop(n)) => value = prop(v, &n),
      },
      Thunk::Thunk(t, env, ks) => {
        value = step(&t, env);
        conts.extend(ks.into_iter().rev());
      }
    }
  }
}

pub fn run(t: &Rc<Tree>) {
  eprintln!("[Run]");
  eprintln!("{}", pretty(t));
  eprintln!("--------");
  let mut thunk = Thunk::Thunk(t.clone(), Rc::new(HashMap::new()), Vec::new());
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
