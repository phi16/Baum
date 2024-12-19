use crate::types::runtime::*;

fn fn_add(args: Vec<&Value>) -> Value {
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

fn action_print(args: &Vec<Value>) -> Action {
  if args.len() != 2 {
    eprintln!("expected 2 args");
    return Action::Stop;
  }
  match args[0] {
    Value::Unit() => print!("()"),
    Value::Int(i) => print!("{}", i),
    Value::Float(f) => print!("{}", f),
    Value::Char(c) => print!("{}", c),
    Value::String(ref s) => print!("{}", s),
    Value::Cl(_, _) => print!("[cl]"),
    Value::Action(_) => print!("[action]"),
    Value::PrimFun(_) => print!("[prim]"),
  };
  let ret = Value::Unit();
  Action::App(Box::new(args[1].clone()), vec![ret])
}

macro_rules! action_wrap {
  ($f:expr) => {{
    fn f_wrap(args: Vec<&Value>) -> Value {
      Value::Action(Action::Call(
        $f,
        args.into_iter().map(|x| x.clone()).collect(),
      ))
    };
    Value::PrimFun(f_wrap)
  }};
}

pub fn prim_value(name: &str) -> Value {
  match name {
    "void" => Value::Action(Action::Stop),
    "add" => Value::PrimFun(fn_add),
    "print" => action_wrap!(action_print),
    _ => {
      eprintln!("unknown prim {:?}", name);
      Value::Action(Action::Stop)
    }
  }
}
