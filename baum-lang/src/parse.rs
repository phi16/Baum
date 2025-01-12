use crate::tokenize::*;
use crate::types::*;

use core::iter::Peekable;
use std::vec::IntoIter;

struct Parser<'a> {
  iter: Peekable<IntoIter<Token<'a>>>,
  indent: Indent,
}

impl<'a, 'b> Parser<'a> {
  fn new(tokens: IntoIter<Token<'a>>) -> Self {
    Parser {
      iter: tokens.peekable(),
      indent: Indent::Base,
    }
  }

  fn peek(&mut self) -> Option<&Token<'a>> {
    self.iter.peek().filter(|t| t.indent > self.indent)
  }

  fn decl(&mut self) -> Option<Decl<'a>> {
    unimplemented!()
  }

  fn decls(&mut self) -> Vec<Decl<'a>> {
    let outer_indent = self.indent;
    let mut ds = Vec::new();
    loop {
      self.indent = outer_indent;
      match self.decl() {
        Some(d) => {
          ds.push(d);
        }
        None => {
          break;
        }
      }
    }
    self.indent = outer_indent;
    return ds;
  }
}

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  for t in &tokens {
    eprintln!("{:?}", t);
  }
  let mut parser = Parser::new(tokens.into_iter());
  let ds = parser.decls();
  return Ok(ds);
}
