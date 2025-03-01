pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
mod syntax;
pub mod tokenize;
pub mod types;

use baum_front::types::tree as front;

pub fn run(code: &str) -> Result<front::Program, Vec<String>> {
  let (tokens, _, errors) = tokenize::tokenize(code);
  if !errors.is_empty() {
    return Err(errors);
  }
  let (tree, errors) = parse::parse(tokens);
  if !errors.is_empty() {
    return Err(errors);
  }
  // eprintln!("--------");
  // eprintln!("{}", pretty::pretty(&tree));
  // eprintln!("--------");
  let front = convert::convert(&tree, syntax::default_syntax_handlers())?;
  // eprintln!("--------");
  // eprintln!("{}", baum_front::pretty::pretty(&front));
  // eprintln!("--------");
  Ok(front)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../examples/try.baum");
  match run(code) {
    Ok(_) => {}
    Err(es) => {
      for e in es {
        eprintln!("{}", e);
      }
    }
  }
  assert!(false);
}
