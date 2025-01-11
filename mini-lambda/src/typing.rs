use crate::types::tree::*;
use crate::types::val::*;
use std::collections::HashMap;

type Ty = Val;
type ValTy = (Val, Ty);

#[derive(Debug, Clone)]
struct Global {
  pub lookup: HashMap<String, ValTy>,
}

struct Checker {
  global: Global,
}

struct CheckEnv<'a> {
  global: &'a Global,
  env: HashMap<String, ValTy>,
  indent: String,
}

impl<'a> CheckEnv<'a> {
  fn new(global: &'a Global) -> Self {
    CheckEnv {
      global,
      env: HashMap::new(),
      indent: "".to_string(),
    }
  }

  fn error(&self, msg: &str) {
    eprintln!("type error: {}", msg);
  }

  fn lookup_env(&self, id: &Id) -> Option<ValTy> {
    self.env.get(id.as_str()).cloned()
  }

  fn lookup_def(&self, id: &Id) -> Option<ValTy> {
    self.global.lookup.get(id.as_str()).cloned()
  }

  fn prim_type(&self, s: &str) -> Val {
    match s {
      "U" => Val(ValF::Uni()),
      "Int" => Val(ValF::Uni()),
      "Char" => Val(ValF::Uni()),
      "String" => Val(ValF::Uni()),
      _ => {
        self.error(&format!("unknown primitive: {}", s));
        Val(ValF::Hole)
      }
    }
  }

  fn subst(&self, v: Val, s: &str, t: &Val) -> Val {
    match v.0 {
      ValF::Lit(_) => v,
      ValF::Def(_) => v,
      ValF::Var(ref id) => {
        if id == s {
          t.clone()
        } else {
          v
        }
      }
      ValF::Fun(a, b) => Val(ValF::Fun(
        Box::new(self.subst(*a, s, t)),
        Box::new(self.subst(*b, s, t)),
      )),
      ValF::FunDep(n, a, b) => {
        if n == s {
          Val(ValF::FunDep(n, a, b))
        } else {
          Val(ValF::FunDep(
            n,
            Box::new(self.subst(*a, s, t)),
            Box::new(self.subst(*b, s, t)),
          ))
        }
      }
      ValF::Lam(n, a, b) => {
        if n == s {
          Val(ValF::Lam(n, a, b))
        } else {
          Val(ValF::Lam(
            n,
            Box::new(self.subst(*a, s, t)),
            Box::new(self.subst(*b, s, t)),
          ))
        }
      }
      ValF::Uni() => v,
      ValF::App(a, b) => Val(ValF::App(
        Box::new(self.subst(*a, s, t)),
        Box::new(self.subst(*b, s, t)),
      )),
      ValF::Hole => v,
      ValF::Prim(_) => v,
    }
  }

  fn equiv(&mut self, a: &Val, b: &Val) -> bool {
    println!("{} - equiv: {:?} == {:?}", self.indent, a, b);
    let indent = self.indent.clone();
    self.indent += "  ";
    let r = self.equiv_impl(a, b);
    self.indent = indent;
    r
  }

  fn equiv_impl(&mut self, a: &Val, b: &Val) -> bool {
    match (&a.0, &b.0) {
      (ValF::Def(a), ValF::Def(b)) if a == b => true,
      (ValF::Var(a), ValF::Var(b)) if a == b => true,
      (ValF::Def(a), _) => {
        if let Some((v, _)) = self.lookup_def(&Id::new(a)) {
          self.equiv(&v, b)
        } else {
          self.error(&format!("definition not found: {}", a));
          false
        }
      }
      (_, ValF::Def(b)) => {
        if let Some((v, _)) = self.lookup_def(&Id::new(b)) {
          self.equiv(a, &v)
        } else {
          self.error(&format!("definition not found: {}", b));
          false
        }
      }
      (ValF::Var(a), _) => {
        if let Some((v, _)) = self.lookup_env(&Id::new(a)) {
          self.equiv(&v, b)
        } else {
          self.error(&format!("variable not found: {}", a));
          false
        }
      }
      (_, ValF::Var(b)) => {
        if let Some((v, _)) = self.lookup_env(&Id::new(b)) {
          self.equiv(a, &v)
        } else {
          self.error(&format!("variable not found: {}", b));
          false
        }
      }
      (ValF::Lit(Lit::Num(a)), ValF::Lit(Lit::Num(b))) => a == b,
      (ValF::Lit(Lit::Chr(a)), ValF::Lit(Lit::Chr(b))) => a == b,
      (ValF::Lit(Lit::Str(a)), ValF::Lit(Lit::Str(b))) => a == b,
      (ValF::Fun(a1, b1), ValF::Fun(a2, b2)) => self.equiv(&*a1, &*a2) && self.equiv(&*b1, &*b2),
      (ValF::FunDep(n1, a1, b1), ValF::FunDep(n2, a2, b2)) => {
        unimplemented!()
      }
      // TODO: Fun may be equivalent to FunDep
      (ValF::Lam(n1, a1, b1), ValF::Lam(n2, a2, b2)) => {
        unimplemented!()
      }
      (ValF::Uni(), ValF::Uni()) => true,
      (ValF::App(a1, b1), ValF::App(a2, b2)) => {
        unimplemented!()
      }
      (ValF::Prim(a), ValF::Prim(b)) => a == b,
      (ValF::Hole, _) => true,
      (_, ValF::Hole) => true,
      _ => false,
    }
  }

  fn check(&mut self, expr: &Expr, ty: &Val) -> (Val, Val) {
    println!("{} - check: {:?}: {:?}", self.indent, expr, ty);
    let indent = self.indent.clone();
    self.indent += "  ";
    let (v, t) = self.check_impl(expr, ty);
    self.indent = indent;
    println!(
      "{} - CHECKED: {:?} = {:?}: {:?} = {:?}",
      self.indent, expr, v, t, ty
    );
    (v, t)
  }

  fn check_impl(&mut self, expr: &Expr, ty: &Val) -> (Val, Val) {
    match expr {
      Expr::Lam(arg, body) => match ty {
        Val(ValF::Fun(va, vb)) => {
          let env = self.env.clone();
          self.env.insert(
            arg.as_str().to_string(),
            (Val(ValF::Var(arg.as_str().to_string())), *va.clone()),
          );
          let (vb, _) = self.check(body, vb);
          self.env = env;
          (
            Val(ValF::Lam(
              arg.as_str().to_string(),
              Box::new(*va.clone()),
              Box::new(vb),
            )),
            ty.clone(),
          )
        }
        Val(ValF::FunDep(n, va, vb)) => {
          let env = self.env.clone();
          self.env.insert(
            arg.as_str().to_string(),
            (Val(ValF::Var(arg.as_str().to_string())), *va.clone()),
          );
          self.env.insert(n.clone(), (*va.clone(), Val(ValF::Uni())));
          let (vb, _) = self.check(body, vb);
          self.env = env;
          (
            Val(ValF::Lam(
              arg.as_str().to_string(),
              Box::new(*va.clone()),
              Box::new(vb),
            )),
            ty.clone(),
          )
        }
        _ => {
          self.error("not a function type");
          (Val(ValF::Hole), ty.clone())
        }
      },
      _ => {
        let (vt, tt) = self.synth(expr);
        if self.equiv(&tt, &ty) {
          (vt, ty.clone())
        } else {
          self.error(&format!("type mismatch ({:?} /= {:?}) of {:?}", tt, ty, vt));
          (vt, Val(ValF::Hole))
        }
      }
    }
  }

  fn synth(&mut self, expr: &Expr) -> (Val, Val) {
    println!("{} - synth: {:?}", self.indent, expr);
    let indent = self.indent.clone();
    self.indent += "  ";
    let (v, t) = self.synth_impl(expr);
    self.indent = indent;
    println!("{} - SYNTHED: {:?} = {:?}: {:?}", self.indent, expr, v, t);
    (v, t)
  }

  fn synth_impl(&mut self, expr: &Expr) -> (Val, Val) {
    match expr {
      Expr::Lit(lit) => match lit {
        Literal::Num(s) => (
          Val(ValF::Lit(Lit::Num(s.parse().unwrap()))),
          Val(ValF::Prim("Int".to_string())),
        ),
        Literal::Chr(s) => (
          Val(ValF::Lit(Lit::Chr(s.chars().next().unwrap()))),
          Val(ValF::Prim("Char".to_string())),
        ),
        Literal::Str(s) => (
          Val(ValF::Lit(Lit::Str(s.clone()))),
          Val(ValF::Prim("String".to_string())),
        ),
      },
      Expr::Var(id) => {
        if let Some(vt) = self.lookup_env(&id) {
          vt
        } else if let Some(vt) = self.lookup_def(&id) {
          (Val(ValF::Def(id.as_str().to_string())), vt.1.clone())
        } else {
          self.error(&format!("variable not found: {}", id.as_str()));
          (Val(ValF::Hole), Val(ValF::Hole))
        }
      }
      Expr::Ann(expr, ty) => {
        let (tv, _) = self.check(ty, &Val(ValF::Uni()));
        self.check(expr, &tv)
      }
      Expr::Fun(a, b) => {
        let (va, _) = self.check(a, &Val(ValF::Uni()));
        let (vb, _) = self.check(b, &Val(ValF::Uni()));
        (Val(ValF::Fun(Box::new(va), Box::new(vb))), Val(ValF::Uni()))
      }
      Expr::FunDep(n, a, b) => {
        let (va, ta) = self.check(a, &Val(ValF::Uni()));
        let env = self.env.clone();
        self.env.insert(
          n.as_str().to_string(),
          (Val(ValF::Var(n.as_str().to_string())), va.clone()),
        );
        let (vb, _) = self.check(b, &Val(ValF::Uni()));
        self.env = env;
        (
          Val(ValF::FunDep(
            n.as_str().to_string(),
            Box::new(va),
            Box::new(vb),
          )),
          Val(ValF::Uni()),
        )
      }
      Expr::Lam(_, _) => {
        self.error("lambda in type synthesis");
        (Val(ValF::Hole), Val(ValF::Hole))
      }
      Expr::LamTy(arg, ty, body) => {
        let (vt, tt) = self.check(ty, &Val(ValF::Uni()));
        let env = self.env.clone();
        self.env.insert(
          arg.as_str().to_string(),
          (Val(ValF::Var(arg.as_str().to_string())), vt.clone()),
        );
        let (vb, tb) = self.synth(body);
        self.env = env;
        (
          Val(ValF::Lam(
            arg.as_str().to_string(),
            Box::new(vt.clone()),
            Box::new(vb),
          )),
          // Val(ValF::Fun(Box::new(vt), Box::new(tb))), // TODO: minimize
          Val(ValF::FunDep(
            arg.as_str().to_string(),
            Box::new(vt),
            Box::new(tb),
          )),
        )
      }
      Expr::Uni() => (Val(ValF::Uni()), Val(ValF::Uni())),
      Expr::App(f, a) => {
        let (vf, tf) = self.synth(f);
        match tf.0 {
          ValF::Fun(ref va, ref vb) => {
            let (va, _) = self.check(a, va);
            (Val(ValF::App(Box::new(vf), Box::new(va))), *vb.clone())
          }
          ValF::FunDep(ref n, ref va, ref vb) => {
            let (va, _) = self.check(a, va);
            let tb = self.subst(*vb.clone(), n, &va);
            (Val(ValF::App(Box::new(vf), Box::new(va))), tb)
          }
          _ => {
            self.error("not a function");
            (Val(ValF::Hole), Val(ValF::Hole))
          }
        }
      }
      Expr::Hole => {
        self.error("hole in type synthesis");
        (Val(ValF::Hole), Val(ValF::Hole))
      }
      Expr::Prim(s) => {
        if s == "U" {
          return (Val(ValF::Uni()), Val(ValF::Uni()));
        }
        (Val(ValF::Prim(s.clone())), self.prim_type(s))
      }
      Expr::Let(decls, body) => {
        let env = self.env.clone();
        for decl in decls {
          let Decl::Def(id, expr) = decl;
          let (vt, tt) = self.synth(expr);
          self.env.insert(id.as_str().to_string(), (vt, tt));
        }
        let (vb, tb) = self.synth(body);
        self.env = env;
        (vb, tb)
      }
    }
  }
}

impl Checker {
  fn new() -> Self {
    Checker {
      global: Global {
        lookup: HashMap::new(),
      },
    }
  }

  fn compile_error(&mut self, msg: &str) {
    eprintln!("compile error: {}", msg);
  }

  fn body(&mut self, expr: &Expr) -> (Val, Val) {
    let mut env = CheckEnv::new(&self.global);
    env.synth(expr)
  }

  fn decl(&mut self, decl: &Decl) -> Option<(String, ValTy)> {
    match decl {
      Decl::Def(id, expr) => {
        let vt = self.body(expr);
        Some((id.as_str().to_string(), vt))
      }
    }
  }
}

pub fn type_check(decls: &Vec<Decl>) {
  let mut checker = Checker::new();
  for decl in decls {
    if let Some((name, dl)) = checker.decl(decl) {
      checker.global.lookup.insert(name, dl);
    }
  }
}
