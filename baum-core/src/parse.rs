use crate::code::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum CharType {
  Alpha,
  Number,
  Symbol,
  Special,  // _'"
  Reserved, // .,:;(){}[]|
  Space,
  Newline,
}

fn get_char_type(c: char) -> CharType {
  match c {
    'a'..='z' | 'A'..='Z' => CharType::Alpha,
    '0'..='9' => CharType::Number,
    '_' | '\'' | '"' => CharType::Special,
    '.' | ',' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | ']' | '|' => CharType::Reserved,
    '\r' | '\n' => CharType::Newline,
    _ if c.is_whitespace() => CharType::Space,
    _ => CharType::Symbol,
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
  Identifier, // a, xs, _+_, p0', if_then_else, ex_*_
  Keyword,    // let, in, where, infix, prim
  Number,     // 0, 3.14, 1e-2, 0x1F, 0xcc
  Char,       // 'a', '\n', '\x1F', '\u3082'
  String,     // "Hello, World!", "a\nb"
  Symbol,     // ., =, [, }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Copy)]
enum Indent {
  Base,
  Head(u16),
  Inf,
}

#[derive(Debug, Clone)]
struct Token<'a> {
  str: &'a str,
  ty: TokenType,
  line: u32,
  column: u16,
  indent: Indent,
}

struct Tokenizer<'a> {
  input: &'a str,
  char_indices: core::iter::Peekable<std::str::CharIndices<'a>>,
  next: Option<(usize, char)>, // look ahead 2 characters
  line: u32,
  column: u16,
  indent: u16,
  token_state: (bool, u32, u16, u16),
  head: bool,
}

impl<'a> Tokenizer<'a> {
  fn new(input: &'a str) -> Self {
    let mut char_indices = input.char_indices().peekable();
    let next = char_indices.next();
    Tokenizer {
      input,
      char_indices,
      next,
      line: 1,
      column: 1,
      indent: 0,
      token_state: (true, 0, 0, 0),
      head: true,
    }
  }

  fn peek(&mut self) -> Option<(usize, char)> {
    self.next
  }

  fn look_ahead(&mut self, c: char) -> bool {
    self.char_indices.peek().map(|(_, c1)| *c1) == Some(c)
  }

  fn advance(&mut self) {
    if self.next.is_none() {
      return;
    }
    let c = self.next.unwrap().1;
    if c == '\r' && !self.look_ahead('\n') || c == '\n' {
      self.line += 1;
      self.column = 1;
      self.indent = 0;
      self.head = true;
    } else if c.is_whitespace() {
      self.column += 1;
      if self.head {
        self.indent += 1;
      }
    } else {
      self.column += 1;
      self.head = false;
    }
    self.next = self.char_indices.next();
  }

  fn tokenize_error(&mut self, msg: &str) {
    if let Some(_) = self.peek() {
      eprintln!("L{} C{}: {}", self.line, self.column, msg);
    } else {
      eprintln!("EOF: {}", msg);
    }
  }

  fn skip_line(&mut self) {
    while let Some((_, c)) = self.peek() {
      if c == '\r' {
        if self.look_ahead('\n') {
          self.advance();
        }
        self.advance();
        return;
      }
      if c == '\n' {
        self.advance();
        return;
      }
      self.advance();
    }
  }

  fn skip_space(&mut self) {
    while let Some((_, c)) = self.peek() {
      if c.is_whitespace() {
        if c != ' ' && c != '\r' && c != '\n' {
          self.tokenize_error(&format!("invalid whitespace character: {:?}", c));
        }
        self.advance();
      } else if c == '-' && self.look_ahead('-') {
        // single line comment
        // TODO: multiline comment (---)
        self.skip_line();
      } else {
        break;
      }
    }
  }

  fn skip_escape(&mut self) {
    self.advance();
    if let Some((_, c2)) = self.peek() {
      if c2 == 'x' || c2 == 'u' {
        // hexadecimal or unicode escape
        unimplemented!()
      } else {
        self.advance();
      }
    } else {
      self.tokenize_error("unterminated escape character");
    }
  }

  fn set_token_state(&mut self) {
    self.token_state = (self.head, self.line, self.column, self.indent);
  }

  fn make_token_internal(&mut self, ty_base: TokenType, str: &'a str) -> Token<'a> {
    let (head, line, column, indent) = self.token_state;
    let indent = if head {
      Indent::Head(indent)
    } else {
      Indent::Inf
    };
    let ty = if ty_base == TokenType::Identifier {
      match str {
        "let" | "in" | "where" | "infix" | "prim" => TokenType::Keyword,
        "=" => TokenType::Symbol,
        _ => TokenType::Identifier,
      }
    } else {
      ty_base
    };
    Token {
      str,
      ty,
      line,
      column,
      indent,
    }
  }

  fn make_token(&mut self, ty: TokenType, i0: usize) -> Token<'a> {
    let (i1, c1) = self.peek().unwrap();
    let str = &self.input[i0..i1 + c1.len_utf8()];
    let token = self.make_token_internal(ty, str);
    self.advance();
    token
  }

  fn make_token_overlooked(&mut self, ty: TokenType, i0: usize) -> Token<'a> {
    let str = match self.peek() {
      Some((i1, _)) => &self.input[i0..i1],
      None => &self.input[i0..],
    };
    self.make_token_internal(ty, str)
  }

  fn string_literal(&mut self) -> Option<Token<'a>> {
    let (i0, c0) = self.peek()?;
    let ty = if c0 == '\'' {
      TokenType::Char
    } else {
      TokenType::String
    };
    let i = i0 + c0.len_utf8();
    self.advance();
    while let Some((_, c1)) = self.peek() {
      if c1 == '\r' || c1 == '\n' {
        self.tokenize_error("unterminated character or string literal");
        return Some(self.make_token_overlooked(ty, i));
      }
      if c1 == c0 {
        let t = self.make_token_overlooked(ty, i);
        self.advance();
        return Some(t);
      }
      if c1 == '\\' {
        self.skip_escape()
      } else {
        self.advance();
      }
    }
    self.tokenize_error("unterminated character or string literal");
    return Some(self.make_token_overlooked(ty, i));
  }

  fn number_literal(&mut self) -> Option<Token<'a>> {
    let (i0, c0) = self.peek()?;
    self.advance();
    enum State {
      Zero, // 0
      Int,  // 123, 0x5c
      Frac, // 1.23
      Exp,  // 1e
      Last, // 1e+4
    }
    let mut hex = false;
    let mut s = if c0 == '0' { State::Zero } else { State::Int };
    while let Some((_, c1)) = self.peek() {
      match s {
        State::Zero => {
          if c1 == 'b' || c1 == 'o' || c1 == 'x' {
            hex = c1 == 'x';
            s = State::Int;
          } else if c1 == 'e' || c1 == 'E' {
            s = State::Exp;
          } else if c1 == '.' {
            s = State::Frac;
          } else if c1.is_ascii_digit() {
            s = State::Int;
          } else {
            break;
          }
        }
        State::Int => {
          if c1 == '.' {
            s = State::Frac;
          } else if c1 == 'e' || c1 == 'E' {
            s = State::Exp;
          } else if hex && c1 == 'p' {
            s = State::Exp;
          } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
            // do nothing
          } else {
            break;
          }
        }
        State::Frac => {
          if c1 == 'e' || c1 == 'E' {
            s = State::Exp;
          } else if hex && c1 == 'p' {
            s = State::Exp;
          } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
            // do nothing
          } else {
            break;
          }
        }
        State::Exp => {
          if c1 == '+' || c1 == '-' {
            s = State::Last;
          } else if !c1.is_ascii_digit() {
            break;
          }
        }
        State::Last => {
          if !c1.is_ascii_digit() {
            break;
          }
        }
      }
      self.advance();
    }
    return Some(self.make_token_overlooked(TokenType::Number, i0));
  }
}

impl<'a> Iterator for Tokenizer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip_space();
    let (i0, c0) = self.peek()?;
    self.set_token_state();
    let ty = get_char_type(c0);
    assert!(ty != CharType::Space && ty != CharType::Newline);
    if ty == CharType::Reserved {
      return Some(self.make_token(TokenType::Symbol, i0));
    }
    if ty == CharType::Special {
      if c0 == '\'' || c0 == '"' {
        return self.string_literal();
      }
    }
    if ty == CharType::Number {
      return self.number_literal();
    }

    // identifier rule
    #[derive(PartialEq, Eq)]
    enum State {
      AlphaNum,
      Symbol,
      Underscore,
    }
    let mut s = match ty {
      CharType::Alpha => State::AlphaNum,
      CharType::Symbol => State::Symbol,
      CharType::Special => {
        assert!(c0 == '_');
        State::Underscore
      }
      _ => unreachable!(),
    };
    self.advance();
    while let Some((_, c1)) = self.peek() {
      let ty1 = get_char_type(c1);
      let pass = match ty1 {
        CharType::Alpha => s == State::AlphaNum || s == State::Underscore,
        CharType::Number => s == State::AlphaNum,
        CharType::Symbol => s == State::Symbol || s == State::Underscore,
        CharType::Special if c1 == '_' => s == State::AlphaNum || s == State::Symbol,
        CharType::Special if c1 == '\'' => s == State::AlphaNum || s == State::Symbol,
        _ => false,
      };
      if pass {
        self.advance();
        s = match ty1 {
          CharType::Alpha | CharType::Number => State::AlphaNum,
          CharType::Symbol => State::Symbol,
          CharType::Special if c1 == '_' => State::Underscore,
          CharType::Special if c1 == '\'' => s,
          _ => unreachable!(),
        };
      } else {
        break;
      }
    }
    return Some(self.make_token_overlooked(TokenType::Identifier, i0));
  }
}

#[cfg(test)]
#[test]
fn test_tokenizer() {
  let input = r#"1 "Hello, World!" 3.14 0x1F mochi if_then_else_ 2+3"#;
  let mut tokenizer = Tokenizer::new(input);
  assert_eq!(tokenizer.next().unwrap().str, "1");
  assert_eq!(tokenizer.next().unwrap().str, "Hello, World!");
  assert_eq!(tokenizer.next().unwrap().str, "3.14");
  assert_eq!(tokenizer.next().unwrap().str, "0x1F");
  assert_eq!(tokenizer.next().unwrap().str, "mochi");
  assert_eq!(tokenizer.next().unwrap().str, "if_then_else_");
  assert_eq!(tokenizer.next().unwrap().str, "2");
  assert_eq!(tokenizer.next().unwrap().str, "+");
  assert_eq!(tokenizer.next().unwrap().str, "3");
  assert!(tokenizer.next().is_none());
}

/*

e = literal
  | id
  | |id| e
  | e e*
  | operators
  | (e)
  | _
  | prim "name"
  | let d* in e
  | e where d*

d = id = e
  | id = { d* }
  | infix L num id
  | infix R num id
  | infix num id

*/

struct Parser<'a> {
  tokenizer: core::iter::Peekable<Tokenizer<'a>>,
  next: Option<Token<'a>>,
  indent: Indent,
  last_indent: Indent,
}

impl<'a> Parser<'a> {
  fn new(input: &'a str) -> Self {
    let mut tokenizer = Tokenizer::new(input).peekable();
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
          ids.push(Id::Name(t.str.to_string()));
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
      return Some(Expr::Lit(Literal::Str(t.str.to_string())));
    }
    if t.ty == TokenType::Identifier {
      return Some(if t.str == "_" {
        self.advance();
        Expr::Hole
      } else {
        let ids = self.id_list();
        Expr::Var(ids)
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
    if t.ty == TokenType::Symbol && t.str == "|" {
      self.advance();
      let mut ids = Vec::new();
      loop {
        let t = self.peek()?;
        if t.ty != TokenType::Identifier {
          break;
        }
        let id = Id::Name(t.str.to_string());
        ids.push(id);
        self.advance();
      }
      if self.next_is_symbol("|") {
        self.advance();
      } else {
        self.parse_error("missing '|'");
      }
      let e = self.expr_or_hole();
      return Some(Expr::Lam(ids, Box::new(e)));
    }
    None
  }

  fn app(&self, e: Expr, args: Vec<Box<Expr>>) -> Expr {
    if args.is_empty() {
      e
    } else {
      Expr::App(Box::new(e), args)
    }
  }

  fn expr(&mut self) -> Option<Expr> {
    let e = self.prim()?;
    let mut args = Vec::new();
    while self.peek().is_some() {
      if self.next_is_keyword("where") {
        self.advance();
        let e = self.app(e, args);
        let decls = self.decls();
        return Some(Expr::Let(decls, Box::new(e)));
      }
      if let Some(a) = self.prim() {
        args.push(Box::new(a));
      } else {
        break;
      }
    }
    Some(self.app(e, args))
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
    let id = Id::Name(t.str.to_string());
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
      Some(Decl::Mod(id, decls))
    } else {
      // definition
      let e = self.expr_or_hole();
      Some(Decl::Def(id, Box::new(e)))
    }
  }

  fn decls(&mut self) -> Vec<Box<Decl>> {
    let outer_indent = self.indent;
    let mut decls = Vec::new();
    loop {
      let indent = self.indent;
      match self.decl() {
        Some(decl) => {
          decls.push(Box::new(decl));
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

  fn program(&mut self) -> Vec<Box<Decl>> {
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

fn traverse_decls(decls: &[Box<Decl>], name: &str) {
  for decl in decls {
    match decl.as_ref() {
      Decl::Def(Id::Name(n), e) => {
        let name = format!("{}.{}", name, n);
        println!("{} = {:?}", name, e);
      }
      Decl::Mod(Id::Name(n), ds) => {
        let name = format!("{}.{}", name, n);
        traverse_decls(ds, &name);
      }
      Decl::Infix(_, _, _) => {}
    }
  }
}

pub fn parse(input: &str) -> Vec<Box<Decl>> {
  let mut parser = Parser::new(input);
  let p = parser.program();
  traverse_decls(&p, "");
  p
}
