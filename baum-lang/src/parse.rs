use crate::decl::DeclParser;
use crate::syntax::default_syntax_table;
use crate::tokenize::tokenize;
use crate::types::env::Env;
use crate::types::tracker::Tracker;
use crate::types::tree::Decl;
use std::collections::HashSet;

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let tracker = Tracker::new(tokens);
  let env = Env::from_syntax(default_syntax_table());
  let mut parser = DeclParser::new(tracker, env, 0, HashSet::new(), Vec::new());
  let ds = parser.program();
  let (_, _, _, _, errors) = parser.into_inner();
  if errors.is_empty() {
    Ok(ds)
  } else {
    Err(errors)
  }
}

#[cfg(test)]
#[test]
fn test_full_features() {
  assert!(parse(include_str!("../pass.baum")).is_ok());
  assert!(parse(include_str!("../fail.baum")).is_err()); // TODO: error count check?
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
