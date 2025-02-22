pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
mod syntax;
mod tokenize;
pub mod types;

#[cfg(test)]
#[test]
fn test() {
  let code = include_str!("../test8.baum");
  let tree = parse::parse(code).unwrap();
  eprintln!("--------");
  let core = convert::convert(&tree, syntax::default_syntax_handlers());
  eprintln!("--------");
  eprintln!("{}", baum_front::pretty::pretty(&core));
  assert!(false);
}
