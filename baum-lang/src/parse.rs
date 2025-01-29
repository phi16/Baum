use crate::parse_decl::DeclParser;
use crate::pretty::pretty;
use crate::tokenize::tokenize;
use crate::types::parse::Decl;
use crate::types::parse::SyntaxTable;
use crate::types::tracker::Tracker;
use std::collections::HashSet;

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let tracker = Tracker::new(tokens);
  let syntax = SyntaxTable::default();
  let mut parser = DeclParser::new(tracker, syntax, HashSet::new(), Vec::new());
  let ds = parser.program();
  let (_, _, errors) = parser.into_inner();
  if errors.is_empty() {
    Ok(ds)
  } else {
    eprintln!("{}", pretty(&ds));
    Err(errors)
  }
}

#[cfg(test)]
#[test]
fn test() {
  assert!(parse("x = 1 + 2 + 3").is_ok());
  assert!(parse("y = 1 + 2 +").is_err());
}
