pub mod convert;
mod decl;
mod expr;
pub mod parse;
mod pretty;
mod syntax;
mod tokenize;
pub mod types;

use baum_front::types::tree as front;

pub struct TokenData {
  pub line: u32,
  pub column: u32,
  pub length: u32,
  pub token_type: u32,
}

pub fn tokenize_example(code: &str) -> Result<Vec<TokenData>, Vec<String>> {
  let tokens = tokenize::tokenize(code)?
    .into_iter()
    .filter_map(|t| {
      if let types::tree::TokenPos::Pos(l, c) = t.pos {
        Some(TokenData {
          line: l,
          column: c as u32,
          length: t.str.chars().count() as u32,
          token_type: match t.ty {
            types::token::TokenType::Ident => 2,
            types::token::TokenType::DecNat => 7,
            types::token::TokenType::Number => 7,
            types::token::TokenType::Char => 6,
            types::token::TokenType::String => 6,
            types::token::TokenType::Precedence => 8,
            types::token::TokenType::Reserved => 8,
          },
        })
      } else {
        None
      }
    })
    .collect::<Vec<_>>();
  Ok(tokens)
}

pub fn parse(code: &str) -> Result<front::Program, Vec<String>> {
  let tree = parse::parse(code)?;
  // eprintln!("--------");
  // eprintln!("{}", pretty::pretty(&tree));
  // eprintln!("--------");
  let front = convert::convert(&tree, syntax::default_syntax_handlers())?;
  // eprintln!("--------");
  // eprintln!("{}", baum_front::pretty::pretty(&core));
  // eprintln!("--------");
  Ok(front)
}

#[cfg(test)]
#[test]
fn test_dev() {
  let code = include_str!("../examples/try.baum");
  match parse(code) {
    Ok(_) => {}
    Err(es) => {
      for e in es {
        eprintln!("{}", e);
      }
    }
  }
  assert!(false);
}
