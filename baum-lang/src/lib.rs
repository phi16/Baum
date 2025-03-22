mod builtin;
pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
pub mod tokenize;
pub mod types;

use baum_front::types::tree as front;
use types::token::{ErrorPos, TokenRange};

pub fn run(code: &str) -> Result<front::Program<TokenRange>, Vec<(ErrorPos, String)>> {
  let (tokens, _, errors) = tokenize::tokenize(code);
  if !errors.is_empty() {
    return Err(errors);
  }
  eprintln!("--------");
  let (tree, errors) = parse::parse(tokens);
  if !errors.is_empty() {
    return Err(errors);
  }
  eprintln!("{}", pretty::pretty(&tree));
  eprintln!("--------");
  let (front, errors) = convert::convert(&tree);
  if !errors.is_empty() {
    return Err(errors);
  }
  eprintln!("{}", baum_front::pretty::pretty(&front));
  eprintln!("--------");
  let (core, errors) = baum_front::convert::convert(front.clone());
  eprintln!("{}", baum_core::pretty::pretty(&core));
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
  eprintln!("--------");
  let errors = baum_core::check::check(core.clone());
  match errors {
    Ok(_) => {
      eprintln!("[Passed]");
      eprintln!("--------");
    }
    Err(es) => {
      return Err(
        es.iter()
          .map(|e| {
            let pos = ErrorPos::EoF;
            (pos, e.clone())
          })
          .collect(),
      );
    }
  }
  Ok(front)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../examples/hurkens.baum");
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
