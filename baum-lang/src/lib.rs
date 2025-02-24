pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
mod syntax;
mod tokenize;
pub mod types;

use baum_front::types::tree as front;

pub fn parse(code: &str) -> Result<front::Program, Vec<String>> {
  let tree = parse::parse(code)?;
  eprintln!("--------");
  eprintln!("{}", pretty::pretty(&tree));
  eprintln!("--------");
  let core = convert::convert(&tree, syntax::default_syntax_handlers())?;
  eprintln!("--------");
  eprintln!("{}", baum_front::pretty::pretty(&core));
  eprintln!("--------");
  Ok(core)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../test6.baum");
  match parse(code) {
    Ok(_) => {}
    Err(es) => {
      for e in es {
        eprintln!("{}", e);
      }
    }
  }
  assert!(false);
}
