use crate::parse_decl::DeclParser;
use crate::pretty::pretty;
use crate::tokenize::tokenize;
use crate::types::parse::Decl;

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let mut parser = DeclParser::new(tokens);
  let ds = parser.program();
  if let Some(t) = parser.tracker.peek_raw() {
    let t = t.clone();
    parser.add_error(t.pos, "not all tokens consumed");
  }

  if parser.errors.is_empty() {
    Ok(ds)
  } else {
    eprintln!("{}", pretty(&ds));
    Err(parser.errors)
  }
}

#[cfg(test)]
#[test]
fn test() {
  assert!(parse("x = 1 + 2 + 3").is_ok());
}
