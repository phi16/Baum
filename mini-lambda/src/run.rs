use crate::parse::*;
use crate::types::tree::*;
use crate::typing::*;

fn traverse_decls(decls: &[Decl], name: &str) {
  for Decl::Def(id, e) in decls {
    let name = format!("{}.{}", name, id.as_str());
    println!("{} = {:?}", name, e);
  }
}

pub fn run(code: &str) {
  let program = parse(code);
  traverse_decls(&program, "");
  let typed_program = type_check(&program);
}
