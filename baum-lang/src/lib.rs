mod builtin;
pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
pub mod tokenize;
pub mod types;

use baum_front::types::tree as front;
use types::token::{ErrorPos, TokenPos, TokenRange};

struct Ps(Vec<TokenPos>);

impl Ps {
  fn error_to_string(&self, epos: ErrorPos) -> String {
    match epos {
      ErrorPos::Ix(ix) => {
        let pos = self.0.get(ix.into_inner()).unwrap();
        format!("L{} C{}", pos.line + 1, pos.column + 1)
      }
      ErrorPos::Pos(l, c, _) => format!("L{} C{}", l + 1, c + 1),
      ErrorPos::EoL(l) => format!("End of L{}", l + 1),
      ErrorPos::EoF => "End of file".to_string(),
    }
  }
  fn convert(&self, errors: Vec<(ErrorPos, String)>) -> String {
    let mut s = String::new();
    for (pos, e) in errors {
      let pos = self.error_to_string(pos);
      s.push_str(&format!("{}: {}\n", pos, e));
    }
    s
  }
  fn convert_range(&self, errors: Vec<(TokenRange, String)>) -> String {
    let mut s = String::new();
    for (range, e) in errors {
      let ix = range.begin;
      let pos = self.0.get(ix.into_inner()).unwrap();
      let pos = format!("L{} C{}", pos.line + 1, pos.column + 1);
      s.push_str(&format!("{}: {}\n", pos, e));
    }
    s
  }
}

pub fn run(code: &str) -> Result<front::Program<TokenRange>, String> {
  let (tokens, _, errors) = tokenize::tokenize(code);
  let ps = Ps(tokens.iter().map(|t| t.pos).collect::<Vec<_>>());
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("--------");
  let (tree, errors) = parse::parse(tokens);
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("{}", pretty::pretty(&tree));
  eprintln!("--------");
  let (front, errors) = convert::convert(&tree);
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("{}", baum_front::pretty::pretty(&front));
  eprintln!("--------");
  let (core, errors) = baum_front::convert::convert(front.clone());
  eprintln!("{}", baum_core::pretty::pretty(&core));
  if !errors.is_empty() {
    return Err(ps.convert_range(errors));
  }
  eprintln!("--------");
  let errors = baum_core::check::check(core.clone());
  match errors {
    Ok(_) => {
      eprintln!("[Passed]");
      eprintln!("--------");
    }
    Err(errors) => {
      eprintln!("--------");
      return Err(ps.convert_range(errors));
    }
  }
  Ok(front)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../examples/test.baum");
  match run(code) {
    Ok(_) => {}
    Err(e) => {
      eprintln!("{}", e);
    }
  }
  eprintln!();
  assert!(false);
}
