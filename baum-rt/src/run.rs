use crate::pretty::{ppk, ppt, ppv};
use crate::prim::{lit, prim, prim_ev};
use crate::types::tree::{app, prop, Cont, Env, Op, Raw, Thunk, Tree, TreeF, Val};
use std::collections::HashMap;
use std::rc::Rc;

fn step(t: &Rc<Tree>, env: Env) -> Thunk {
  match &t.0 {
    TreeF::Var(i) => Thunk::val(env.get(i).unwrap().clone()),
    TreeF::Unit => Thunk::val(Val::Unit),
    TreeF::Prim(s) => Thunk::val(prim(s)),
    TreeF::Lit(l) => Thunk::val(lit(l.clone())),
    TreeF::Let(ds, e) => {
      let mut env = env;
      for (i, d) in ds {
        let v = eval(step(d, env.clone()));
        Rc::make_mut(&mut env).insert(*i, v);
      }
      Thunk::eval(e.clone(), env)
    }

    TreeF::Lam(i, scope, e) => {
      let env = scope
        .iter()
        .map(|i| (*i, env.get(i).unwrap().clone()))
        .collect::<HashMap<_, _>>();
      Thunk::val(Val::Cl(*i, Rc::new(env), e.clone()))
    }
    TreeF::App(f, x) => {
      let mut f = step(f, env.clone());
      let x = step(x, env.clone());
      f.push_cont(Cont::App0((), Box::new(x)));
      f
    }
    TreeF::Obj(props) => {
      let mut ps = HashMap::new();
      for (n, e) in props {
        let v = eval(step(e, env.clone()));
        ps.insert(*n, v);
      }
      Thunk::val(Val::Obj(ps))
    }
    TreeF::Prop(o, n) => {
      let mut o = step(o, env);
      o.push_cont(Cont::Prop(*n));
      o
    }
  }
}

fn eval(value: Thunk) -> Val {
  let mut conts = Vec::new();
  let mut value = value;
  loop {
    // eprintln!("[Thunk] {}", ppk(&value));
    let (op, ks) = value.into_inner();
    conts.extend(ks.into_iter().rev());
    match op {
      Op::Val(v) => match conts.pop() {
        None => return v,
        Some(Cont::App0(_, x)) => {
          conts.push(Cont::App1(v, ()));
          value = *x;
        }
        Some(Cont::App1(f, _)) => value = app(f, v),
        Some(Cont::Prop(n)) => value = prop(v, &n),
      },
      Op::Eval(t, env) => value = step(&t, env),
      Op::Prim(name, args) => value = prim_ev(&name, args),
    }
  }
}

pub fn run(t: &Rc<Tree>) {
  eprintln!("[Run]");
  eprintln!("{}", ppt(t));
  eprintln!("--------");
  let mut thunk = Thunk::eval(t.clone(), Rc::new(HashMap::new()));
  loop {
    let v = eval(thunk);
    match v {
      Val::Raw(Raw::Action(a)) => {
        thunk = (a.run)();
      }
      Val::Raw(Raw::Done) => {
        eprintln!("--------");
        eprintln!("[Done]");
        return;
      }
      _ => unreachable!(),
    }
  }
}
