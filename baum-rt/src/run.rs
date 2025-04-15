use crate::pretty::pretty;
use crate::prim::{prim, prim_ev};
use crate::types::tree::{Cont, Env, Name, Raw, Thunk, Tree, TreeF, Val};
use std::collections::HashMap;
use std::rc::Rc;

fn app(f: Val, x: Val) -> Thunk {
  match f {
    Val::Lam(i, e, body) => {
      let mut e = e;
      e.insert(i, x);
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

fn step(t: &Rc<Tree>, env: &Env) -> Thunk {
  match &t.0 {
    TreeF::Var(i) => Thunk::Val(env.get(i).unwrap().clone()),
    TreeF::Type => Thunk::Val(Val::Type),
    TreeF::Prim(s) => Thunk::Val(prim(s)),
    TreeF::Let(ds, e) => {
      let mut env = env.clone();
      for (i, d) in ds {
        let v = eval(step(d, &env));
        env.insert(*i, v);
      }
      Thunk::Thunk(e.clone(), env.clone(), Vec::new())
    }

    TreeF::Lam(i, e) => Thunk::Val(Val::Lam(*i, env.clone(), e.clone())),
    TreeF::App(f, x) => {
      let f = step(f, env);
      let x = eval(step(x, env));
      match f {
        Thunk::Val(f) => app(f, x),
        Thunk::Thunk(t, e, mut ks) => {
          ks.push(Cont::App(x));
          Thunk::Thunk(t, e, ks)
        }
        _ => unreachable!(),
      }
    }
    TreeF::Obj0 => Thunk::Val(Val::Obj0),
    TreeF::Obj(props) => {
      let mut ps = HashMap::new();
      for (n, e) in props {
        let v = eval(step(e, env));
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
        _ => unreachable!(),
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
        value = step(&t, &env);
        conts.extend(ks.into_iter().rev());
      }
    }
  }
}

pub fn run(t: &Rc<Tree>) {
  eprintln!("[Run]");
  eprintln!("{}", pretty(t));
  eprintln!("--------");
  let mut thunk = Thunk::Thunk(t.clone(), HashMap::new(), Vec::new());
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
      _ => {
        eprintln!("Result: {:?}", v);
        return;
      }
    }
  }
}
