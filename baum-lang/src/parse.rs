use crate::convert::convert;
use crate::decl::DeclParser;
use crate::pretty::pretty;
use crate::syntax::default_syntax_table;
use crate::tokenize::tokenize;
use crate::types::parse::{Decl, Env};
use crate::types::tracker::Tracker;
use std::collections::HashSet;

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
    let _ = convert(ds.clone());
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
  assert!(parse("syntax 1 a + b = add a b\nx = 1 + 2 + 3").is_ok());
  assert!(parse("syntax 1 a + b = add a b\ny = 1 + 2 +").is_err());
  assert!(parse("y = A.z; module A = { x = 1 }").is_err());
  assert!(parse("module A = { x = 1 }\ny = A.z").is_ok());
  assert!(parse("module A = { module B = { module C = {} } }\n").is_ok());
  assert!(parse("module A = { module B = { module C = {} } }\nopen A\nu = B.x").is_ok());
  assert!(parse("module A = { module B = { module C = {} } }\nopen A\nu = C.x").is_err());
  assert!(parse("module A = { module B = { module C = {} } }\nopen A\nopen B\nu = C.x").is_ok());
  assert!(parse("module A = { local module B = { module C = {} } }\nu = A.B.x").is_err());
  assert!(parse(
    r#"
      module A = {
        local module B = {
          module D = {}
        }
        module C = B
      }
      u = A.C.D.x"#
  )
  .is_ok());
  assert!(parse(
    r#"
      module A = {
        local module B = {}
        module C = {
          module D = B
        }
      }
      u = A.C.D.x"#
  )
  .is_ok());
  assert!(parse("a = b; c = d").is_ok());
  assert!(parse("module A (n: Nat) = { module B = {} }\nopen A 1\nopen A 2").is_err());
  // assert!(parse("module A = { module B = {} }\nopen A\nopen A").is_ok());
}
