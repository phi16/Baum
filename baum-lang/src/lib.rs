mod builtin;
pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
pub mod tokenize;
pub mod types;

use baum_front::types::tree as front;
use types::token::ErrorPos;

pub fn run(code: &str) -> Result<front::Program, Vec<(ErrorPos, String)>> {
  let (tokens, _, errors) = tokenize::tokenize(code);
  if !errors.is_empty() {
    return Err(errors);
  }
  let (tree, errors) = parse::parse(tokens);
  if !errors.is_empty() {
    return Err(errors);
  }
  eprintln!("--------");
  eprintln!("{}", pretty::pretty(&tree));
  eprintln!("--------");
  let (front, errors) = convert::convert(&tree);
  if !errors.is_empty() {
    return Err(errors);
  }
  eprintln!("--------");
  eprintln!("{}", baum_front::pretty::pretty(&front));
  eprintln!("--------");
  let (core, errors) = baum_front::convert::convert(front.clone());
  eprintln!("--------");
  eprintln!("{}", baum_core::pretty::pretty(&core));
  eprintln!("--------");
  if !errors.is_empty() {
    return Err(
      errors
        .iter()
        .map(|e| {
          let pos = ErrorPos::EoF;
          (pos, e.clone())
        })
        .collect(),
    );
  }
  Ok(front)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../examples/test15.baum");
  match run(code) {
    Ok(_) => {}
    Err(es) => {
      for (pos, e) in es {
        eprintln!("{}: {}", pos.to_string(), e);
      }
    }
  }
  assert!(false);
}
