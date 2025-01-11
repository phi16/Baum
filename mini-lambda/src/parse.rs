use crate::tokenize::*;
use crate::types::tree::*;
use core::iter::Peekable;

/*

e = literal
  | id
  | e.id
  | [id] e
  | [id: e] e
  | e -> e
  | (id: e) -> e
  | e e
  | e : e
  | (e)
  | _
  | prim "name"
  | let d* in e
  | e where d*

d = id = e
  | id = { d* }

*/

pub struct Parser<'a> {
  tokenizer: Peekable<Tokenizer<'a>>,
  next: Option<Token<'a>>,
  indent: Indent,
  last_indent: Indent,
}

impl<'a> Parser<'a> {
  fn new(input: Tokenizer<'a>) -> Self {
    let mut tokenizer = input.peekable();
    let next = tokenizer.next();
    let last_indent = next.as_ref().map(|t| t.indent).unwrap_or(Indent::Base);
    Parser {
      tokenizer,
      next,
      indent: Indent::Base,
      last_indent,
    }
  }

  fn peek(&mut self) -> Option<Token<'a>> {
    if self
      .next
      .as_ref()
      .map(|t| t.indent <= self.indent)
      .unwrap_or(false)
    {
      return None;
    }
    self.next.clone()
  }

  fn next_is(&mut self, ty: TokenType, s: &str) -> bool {
    self
      .peek()
      .map(|t| t.ty == ty && t.str == s)
      .unwrap_or(false)
  }
  fn next_is_exact(&mut self, i: Indent, ty: TokenType, s: &str) -> bool {
    if let Some(t) = self.next.as_ref() {
      return t.indent == i && t.ty == ty && t.str == s;
    } else {
      return false;
    }
  }

  fn next_is_keyword(&mut self, s: &str) -> bool {
    self.next_is(TokenType::Keyword, s)
  }
  fn next_is_symbol(&mut self, s: &str) -> bool {
    self.next_is(TokenType::Symbol, s)
  }
  fn next_is_string(&mut self) -> bool {
    self
      .peek()
      .map(|t| t.ty == TokenType::String)
      .unwrap_or(false)
  }

  fn advance(&mut self) {
    if self.next.is_none() {
      return;
    }
    self.next = self.tokenizer.next();
    if let Some(t) = self.next.as_ref() {
      if let Indent::Head(n) = t.indent {
        self.last_indent = Indent::Head(n);
      }
    }
  }

  fn look_ahead(&mut self, ty: TokenType, s: &str) -> bool {
    self
      .tokenizer
      .peek()
      .map(|t| t.ty == ty && t.str == s)
      .unwrap_or(false)
  }

  fn look_ahead_symbol(&mut self, s: &str) -> bool {
    self.look_ahead(TokenType::Symbol, s)
  }

  fn parse_error(&mut self, msg: &str) {
    if let Some(t) = self.next.as_ref() {
      eprintln!("L{} C{}: {}", t.line, t.column, msg);
    } else {
      eprintln!("EOF: {}", msg);
    }
  }

  fn id_list(&mut self) -> Vec<Id> {
    let mut ids = Vec::new();
    loop {
      match self.peek() {
        Some(t) if t.ty == TokenType::Identifier => {
          ids.push(Id::new(t.str));
          self.advance();
        }
        _ => break,
      }
      if self.next_is_symbol(".") {
        self.advance();
      } else {
        break;
      }
    }
    ids
  }

  fn prim(&mut self) -> Option<Expr> {
    let t = self.peek()?;
    if t.ty == TokenType::Number {
      self.advance();
      return Some(Expr::Lit(Literal::Num(t.str.to_string())));
    }
    if t.ty == TokenType::Char {
      self.advance();
      return Some(Expr::Lit(Literal::Chr(t.str.to_string())));
    }
    if t.ty == TokenType::String {
      self.advance();
      let s = t.str.replace("\\n", "\n");
      return Some(Expr::Lit(Literal::Str(s)));
    }
    if t.ty == TokenType::Identifier {
      return Some(if t.str == "_" {
        self.advance();
        Expr::Hole
      } else {
        let ids = self.id_list();
        assert!(ids.len() == 1);
        Expr::Var(ids[0].clone())
      });
    }
    if t.ty == TokenType::Symbol && t.str == "(" {
      self.advance();
      let e = self.expr_or_hole();
      if self.next_is_symbol(")") {
        self.advance();
      } else {
        self.parse_error("missing ')'");
      }
      return Some(e);
    }
    if t.ty == TokenType::Keyword && t.str == "prim" {
      self.advance();
      if self.next_is_string() {
        let t = self.peek().unwrap();
        self.advance();
        return Some(Expr::Prim(t.str.to_string()));
      } else {
        self.parse_error("missing string literal");
        return Some(Expr::Prim("".to_string()));
      }
    }
    if t.ty == TokenType::Keyword && t.str == "let" {
      self.advance();
      let decls = self.decls();
      if self.next_is_keyword("in") {
        self.advance();
      } else {
        self.parse_error("missing 'in'");
      }
      let e = self.expr_or_hole();
      return Some(Expr::Let(decls, Box::new(e)));
    }
    if t.ty == TokenType::Symbol && t.str == "[" {
      self.advance();
      let t = self.peek()?;
      if t.ty != TokenType::Identifier {
        self.parse_error("missing identifier");
      }
      let id = Id::new(t.str);
      self.advance();
      let tt = if self.next_is_symbol(":") {
        self.advance();
        Some(self.expr_or_hole())
      } else {
        None
      };
      if self.next_is_symbol("]") {
        self.advance();
      } else {
        self.parse_error("missing ']'");
      }
      let e = self.expr_or_hole();
      return Some(match tt {
        Some(tt) => Expr::LamTy(id, Box::new(tt), Box::new(e)),
        None => Expr::Lam(id, Box::new(e)),
      });
    }
    None
  }

  fn expr(&mut self) -> Option<Expr> {
    let mut e = self.prim()?;
    while self.peek().is_some() {
      if self.next_is_keyword("where") {
        self.advance();
        let decls = self.decls();
        return Some(Expr::Let(decls, Box::new(e)));
      }
      if self.next_is_symbol("->") {
        self.advance();
        let e2 = self.expr_or_hole();
        if let Expr::Ann(na, ty) = e.clone() {
          if let Expr::Var(id) = *na {
            return Some(Expr::FunDep(id.clone(), ty, Box::new(e2)));
          }
        }
        return Some(Expr::Fun(Box::new(e), Box::new(e2)));
      }
      if self.next_is_symbol(":") {
        self.advance();
        let e2 = self.expr_or_hole();
        return Some(Expr::Ann(Box::new(e), Box::new(e2)));
      }
      if let Some(a) = self.prim() {
        e = Expr::App(Box::new(e), Box::new(a));
      } else {
        break;
      }
    }
    Some(e)
  }

  fn expr_or_hole(&mut self) -> Expr {
    match self.expr() {
      Some(e) => e,
      None => {
        self.parse_error("missing expression");
        Expr::Hole
      }
    }
  }

  fn decl(&mut self) -> Option<Decl> {
    let t = self.peek()?;
    let indent = self.last_indent;
    self.indent = indent;
    if self.next_is_keyword("infix") {
      unimplemented!()
    }
    if t.ty != TokenType::Identifier {
      return None;
    }
    if !self.look_ahead_symbol("=") {
      return None;
    }
    let id = Id::new(t.str);
    self.advance();
    self.advance();
    if self.next_is_symbol("{") {
      // module
      self.advance();
      let decls = self.decls();
      if self.next_is_symbol("}") || self.next_is_exact(indent, TokenType::Symbol, "}") {
        self.advance();
      } else {
        self.parse_error("missing '}'");
      }
      // Some(Decl::Mod(id, decls))
      unimplemented!()
    } else {
      // definition
      let e = self.expr_or_hole();
      Some(Decl::Def(id, Box::new(e)))
    }
  }

  fn decls(&mut self) -> Vec<Decl> {
    let outer_indent = self.indent;
    let mut decls = Vec::new();
    loop {
      let indent = self.indent;
      match self.decl() {
        Some(decl) => {
          decls.push(decl);
          if self.next_is_symbol(";") {
            self.advance();
          }
        }
        None => break,
      }
      self.indent = indent;
    }
    self.indent = outer_indent;
    decls
  }

  fn program(&mut self) -> Vec<Decl> {
    let mut decls = Vec::new();
    loop {
      self.indent = Indent::Base;
      if self.peek().is_none() {
        break;
      }
      let ds = self.decls();
      if ds.is_empty() {
        self.advance();
      } else {
        decls.extend(ds);
      }
    }
    decls
  }
}

pub fn parse(code: &str) -> Vec<Decl> {
  let tokenizer = Tokenizer::new(code);
  let mut parser = Parser::new(tokenizer);
  parser.program()
}
