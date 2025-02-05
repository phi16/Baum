use crate::decl::DeclParser;
use crate::pretty::pretty;
use crate::syntax::default_syntax_table;
use crate::tokenize::tokenize;
use crate::types::parse::{Decl, Env};
use crate::types::tracker::Tracker;
use baum_core::types as core;
use std::collections::HashSet;

fn convert(ds: Vec<Decl>) {
  let h = core::Expr::Hole;
  // unimplemented!()
}

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let tracker = Tracker::new(tokens);
  let env = Env::from_syntax(default_syntax_table());
  let mut parser = DeclParser::new(tracker, env, HashSet::new(), Vec::new());
  let ds = parser.program();
  let (_, _, errors) = parser.into_inner();
  if errors.is_empty() {
    convert(ds.clone());
    Ok(ds)
  } else {
    eprintln!("{}", pretty(&ds));
    eprintln!("{:?}", errors);
    Err(errors)
  }
}

#[cfg(test)]
#[test]
fn test() {
  assert!(parse("x = 1 + 2 + 3").is_ok());
  assert!(parse("y = 1 + 2 +").is_err());
  assert!(parse("y = A.x; module A = { x = 1 }").is_err());
  assert!(parse("module A = { x = 1 }\ny = A.x").is_ok());
  assert!(parse("module A = { module B = {} }\nopen A\nopen A").is_ok());
}
