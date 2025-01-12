use crate::tokenize::*;
use crate::types::*;

use core::iter::Peekable;
use std::vec::IntoIter;

struct Parser<'a> {
  iter: Peekable<IntoIter<Token<'a>>>,
  indent: Indent,
  errors: Vec<String>,
}

impl<'a> Parser<'a> {
  fn new(tokens: IntoIter<Token<'a>>) -> Self {
    Parser {
      iter: tokens.peekable(),
      indent: Indent::Base,
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}: {}", pos.to_string(), msg);
    eprintln!("{}", s);
    self.errors.push(s);
  }

  fn peek(&mut self) -> Option<&Token<'a>> {
    self.iter.peek().filter(|t| self.indent < t.indent)
  }

  fn skip_to_next_head(&mut self) {
    while let Some(t) = self.peek() {
      match t.indent {
        Indent::Head(_) => break,
        _ => self.iter.next(),
      };
    }
  }

  fn pos(&mut self) -> TokenPos {
    self.peek().map_or(TokenPos::EoF, |t| t.pos)
  }

  fn context(&mut self) -> Option<Ctx<'a>> {
    self.iter.next();
    unimplemented!()
  }

  fn expr(&mut self) -> Option<Expr<'a>> {
    unimplemented!()
  }

  fn expr_or_hole(&mut self) -> Expr<'a> {
    let pos = self.pos();
    match self.expr() {
      Some(e) => e,
      None => Expr(ExprF::Lit(Literal::Hole), pos),
    }
  }

  fn decl(&mut self) -> Option<Decl<'a>> {
    let indent = self.indent;
    let res = self.decl_internal();
    self.indent = indent;
    res
  }

  fn decl_internal(&mut self) -> Option<Decl<'a>> {
    let t = self.peek()?.clone();
    let indent = match t.indent {
      Indent::Head(i) => Indent::Head(i),
      _ => {
        self.add_error(t.pos, "decl: expected beginning of line");
        self.skip_to_next_head();
        return None;
      }
    };
    self.indent = indent;

    match t.ty {
      TokenType::Keyword => {
        if t.str == "syntax" {
          // syntax
          unimplemented!()
        }
        if t.str == "open" {
          // open
          unimplemented!()
        }
      }
      TokenType::Symbol => {
        if t.str == "[" {
          // context
          let ctx = self.context()?;
          let d = self.decl()?;
          return Some(Decl(DeclF::Context(ctx, Box::new(d)), t.pos));
        }
        if t.str == "{" {
          // anonymous module
          self.iter.next();
          let ds = self.decls();
          match self.peek() {
            Some(t) if t.ty == TokenType::Symbol && t.str == "}" => {
              self.iter.next();
            }
            _ => {
              self.add_error(t.pos, "decl: expected '}'");
              self.skip_to_next_head();
            }
          }
          return Some(Decl(DeclF::Module(None, Box::new(ds)), t.pos));
        }
      }
      TokenType::Ident => {
        self.iter.next();
        let t2 = match self.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "decl: expected module or definition");
            self.skip_to_next_head();
            return None;
          }
        };
        if t2.ty == TokenType::Symbol && t2.str == "{" {
          // named module
          self.iter.next();
          let ds = self.decls();
          match self.peek() {
            Some(t) if t.ty == TokenType::Symbol && t.str == "}" => {
              self.iter.next();
            }
            _ => {
              self.add_error(t.pos, "decl: expected '}'");
              self.skip_to_next_head();
            }
          }
          return Some(Decl(
            DeclF::Module(Some(Id::new(t.str)), Box::new(ds)),
            t.pos,
          ));
        }

        // definition
        unimplemented!("definition!")
      }
      _ => {}
    }

    self.add_error(t.pos, "decl: expected declaration");
    self.iter.next();
    self.skip_to_next_head();
    return None;
  }

  fn decls(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    while self.peek().is_some() {
      if let Some(d) = self.decl() {
        ds.push(d);
      }
    }
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
  if let Some(t) = parser.iter.peek() {
    let t = t.clone();
    parser.add_error(t.pos, "not all tokens consumed");
  }

  if parser.errors.is_empty() {
    Ok(ds)
  } else {
    Err(parser.errors)
  }
}
