use crate::compile::*;
use crate::eval::*;
use crate::parse::*;
use crate::types::code::*;
use crate::types::runtime::*;
use std::collections::HashMap;

fn traverse_decls(decls: &[Decl], name: &str) {
  for decl in decls {
    match decl {
      Decl::Def(id, e) => {
        let name = format!("{}.{}", name, id.as_str());
        println!("{} = {:?}", name, e);
      }
      Decl::Mod(id, ds) => {
        let name = format!("{}.{}", name, id.as_str());
        traverse_decls(ds, &name);
      }
      Decl::Infix(_, _, _) => {}
    }
  }
}

struct Dump<'a> {
  funs: &'a HashMap<FunLoc, Fun>,
}

impl<'a> Dump<'a> {
  fn dump_e(&self, e: &E) -> String {
    match e {
      E::DefRef(DefLoc(loc)) => format!("D{}", loc),
      E::ArgRef(ArgLoc(loc)) => format!("A{}", loc),
      E::EnvRef(EnvLoc(loc)) => format!("E{}", loc),
      E::Lit(l) => match l {
        L::Num(s) => s.clone(),
        L::Chr(s) => s.clone(),
        L::Str(s) => format!("{:?}", s),
      },
      E::Cl(fun_loc, env) => {
        let fun = self.funs.get(fun_loc).unwrap();
        let env: Vec<String> = env
          .iter()
          .zip(fun.env.iter())
          .map(|(e, n)| format!("{}:L{}", n, e.0))
          .collect();
        format!("F{}{{ {} }}", fun_loc.0, env.join(", "))
      }
      E::App(cl, args) => {
        let args: Vec<String> = args.into_iter().map(|a| format!("L{}", a.0)).collect();
        format!("L{}( {} )", cl.0, args.join(", "))
      }
      E::Hole => "_".to_string(),
      E::Prim(s) => format!("prim {:?}", s),
    }
  }

  fn traverse_body(&self, body: &Body) {
    for (loc, e) in body.ls.iter().enumerate() {
      println!("    L{} = {}", loc, self.dump_e(e));
    }
    println!("    result = L{}", body.result.0);
  }

  fn with_index(&self, v: &[String], p: &str) -> String {
    v.iter()
      .enumerate()
      .map(|(i, s)| format!("{}{}:{}", p, i, s))
      .collect::<Vec<String>>()
      .join(", ")
  }

  fn traverse_runtime(&self, runtime: &Runtime) {
    println!("[Fun]");
    for loc in 0..runtime.fun_count {
      if let Some(fun) = runtime.funs.get(&FunLoc(loc)) {
        println!(
          "- F{}( {} ) {{ // {}",
          loc,
          self.with_index(&fun.args, "A"),
          self.with_index(&fun.env, "E")
        );
        self.traverse_body(&fun.body);
        println!("  }}");
      }
    }
    println!("[Def]");
    for loc in 0..runtime.def_count {
      if let Some(def) = runtime.defs.get(&DefLoc(loc)) {
        match def {
          D::Def(body) => {
            println!("- D{} = {{", loc);
            self.traverse_body(&body);
            println!("  }}");
          }
          D::Mod(mods) => {
            println!("- D{} = module {{", loc);
            for (name, loc) in mods {
              println!("    {} = D{}", name, loc.0);
            }
            println!("  }}");
          }
        }
      }
    }
    println!("[LookUp]");
    for (name, loc) in &runtime.lookup {
      println!("- {} = D{}", name, loc.0);
    }
  }
}

pub fn run(code: &str) {
  let program = parse(code);
  traverse_decls(&program, "");
  println!("");
  let runtime = compile(program);
  let dumper = Dump {
    funs: &runtime.funs,
  };
  dumper.traverse_runtime(&runtime);
}
