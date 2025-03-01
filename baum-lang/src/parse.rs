use crate::builtin::builtin_syntax_table;
use crate::decl::DeclParser;
use crate::types::env::Env;
use crate::types::token::{ErrorPos, Token};
use crate::types::tracker::Tracker;
use crate::types::tree::Decl;
use std::collections::HashSet;

pub fn parse<'a>(tokens: Vec<Token<'a>>) -> (Vec<Decl<'a>>, Vec<(ErrorPos, String)>) {
  let tracker = Tracker::new(tokens);
  let env = Env::from_syntax(builtin_syntax_table());
  let mut parser = DeclParser::new(tracker, env, 0, HashSet::new(), Vec::new());
  let ds = parser.program();
  let (_, _, _, _, errors) = parser.into_inner();
  (ds, errors)
}

#[cfg(test)]
#[test]
fn test_full_features() {
  use crate::tokenize::tokenize;

  fn test<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<(ErrorPos, String)>> {
    let (tokens, _, errors) = tokenize(code);
    if !errors.is_empty() {
      return Err(errors);
    }
    let (ds, errors) = parse(tokens);
    if !errors.is_empty() {
      return Err(errors);
    }
    Ok(ds)
  }
  assert!(test(include_str!("../examples/pass.baum")).is_ok());
  assert!(test(include_str!("../examples/fail.baum")).is_err()); // TODO: error count check?
}

#[cfg(test)]
#[test]
fn test() {
  /* assert!(parse("x= 1").is_err());
  assert!(parse("x = 1").is_ok());
  assert!(parse("syntax 1< a + b = add a b\nx = 1 + 2 + 3").is_ok());
  assert!(parse("syntax 1 a + b = add a b\nx = 1 + 2 + 3").is_err());
  assert!(parse(include_str!("../test1.baum")).is_ok());
  assert!(parse(include_str!("../test2.baum")).is_ok());
  assert!(parse(include_str!("../test3.baum")).is_ok());
  assert!(parse(include_str!("../test4.baum")).is_ok());
  assert!(parse("z = let x = 1; y = 2 in t").is_ok());
  assert!(parse("x: 1 = 1").is_ok());
  assert!(false); */
}
