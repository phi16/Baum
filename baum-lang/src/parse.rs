use crate::decl::DeclParser;
use crate::tokenize::tokenize;
use crate::types::env::Env;
use crate::types::tracker::Tracker;
use std::collections::HashSet;

pub fn parse<'a>(code: &'a str) -> Result<(), Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let tracker = Tracker::new(tokens);
  let env = Env::new(); // Env::from_syntax(default_syntax_table());
  let mut parser = DeclParser::new(tracker, env, HashSet::new(), Vec::new());
  let ds = parser.program();
  let (_, _, errors) = parser.into_inner();
  if errors.is_empty() {
    // let core = convert(ds.clone());
    // eprintln!("{:?}", core);
    // Ok(ds)
  } else {
    // eprintln!("{}", pretty(&ds));
    eprintln!("{:?}", errors);
    // Err(errors)
  }
  Ok(())
}

#[cfg(test)]
#[test]
fn test() {
  assert!(parse("x = 1").is_ok());
}
