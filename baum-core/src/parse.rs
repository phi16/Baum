use crate::code::*;

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

#[derive(Debug, Clone, PartialEq, Eq)]
enum CharType {
  Alpha,
  Number,
  Symbol,
  Special,  // _'"
  Reserved, // .,=:;(){}[]|
  Space,
  Newline,
}

fn get_char_type(c: char) -> CharType {
  match c {
    'a'..='z' | 'A'..='Z' => CharType::Alpha,
    '0'..='9' => CharType::Number,
    '_' | '\'' | '"' => CharType::Special,
    '.' | ',' | '=' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | ']' | '|' => CharType::Reserved,
    '\r' | '\n' => CharType::Newline,
    _ if c.is_whitespace() => CharType::Space,
    _ => CharType::Symbol,
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
  Identifier, // a, xs, _+_, p0', if_then_else, ex_*_
  Number,     // 0, 3.14, 1e-2, 0x1F, 0xcc
  Char,       // 'a', '\n', '\x1F', '\u3082'
  String,     // "Hello, World!", "a\nb"
  Symbol,     // ., =, [, }
}

#[derive(Debug, Clone)]
struct Token<'a> {
  str: &'a str,
  ty: TokenType,
  line: usize,
  column: usize,
  indent: usize,
}

struct Tokenizer<'a> {
  input: &'a str,
  char_indices: core::iter::Peekable<std::str::CharIndices<'a>>,
  next: Option<(usize, char)>, // look ahead 2 characters
  line: usize,
  column: usize,
  indent: usize,
  token_state: (usize, usize, usize),
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
      token_state: (0, 0, 0),
      head: true,
    }
  }

  fn peek(&mut self) -> Option<(usize, char)> {
    self.next
  }

  fn look_ahead(&mut self, c: char) -> bool {
    self.char_indices.peek().map(|(_, c1)| *c1) == Some(c)
  }

  fn next(&mut self) {
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

  fn skip_line(&mut self) {
    while let Some((_, c)) = self.peek() {
      if c == '\r' {
        if self.look_ahead('\n') {
          self.next();
        }
        self.next();
        return;
      }
      if c == '\n' {
        self.next();
        return;
      }
      self.next();
    }
  }

  fn skip_space(&mut self) {
    while let Some((_, c)) = self.peek() {
      if c.is_whitespace() {
        if c != ' ' && c != '\r' && c != '\n' {
          eprintln!("invalid whitespace character: {:?}", c);
        }
        self.next();
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
    self.next();
    if let Some((_, c2)) = self.peek() {
      if c2 == 'x' || c2 == 'u' {
        // hexadecimal or unicode escape
        unimplemented!()
      } else {
        self.next();
      }
    } else {
      eprintln!("unterminated escape character");
    }
  }

  fn set_token_state(&mut self) {
    self.token_state = (self.line, self.column, self.indent);
  }

  fn make_token(&mut self, ty: TokenType, i0: usize) -> Token<'a> {
    let (i1, c1) = self.peek().unwrap();
    let (line, column, indent) = self.token_state;
    let token = Token {
      str: &self.input[i0..i1 + c1.len_utf8()],
      ty,
      line,
      column,
      indent,
    };
    self.next();
    token
  }

  fn make_token_overlooked(&mut self, ty: TokenType, i0: usize) -> Token<'a> {
    let str = match self.peek() {
      Some((i1, _)) => &self.input[i0..i1],
      None => &self.input[i0..],
    };
    let (line, column, indent) = self.token_state;
    Token {
      str,
      ty,
      line,
      column,
      indent,
    }
  }

  fn string_literal(&mut self) -> Option<Token<'a>> {
    let (i0, c0) = self.peek()?;
    self.next();
    while let Some((i1, c1)) = self.peek() {
      if c1 == c0 {
        let ty = if c0 == '\'' {
          TokenType::Char
        } else {
          TokenType::String
        };
        return Some(self.make_token(ty, i0));
      }
      if c1 == '\\' {
        self.skip_escape()
      } else {
        self.next();
      }
    }
    eprintln!("unterminated character or string literal");
    return None;
  }

  fn number_literal(&mut self) -> Option<Token<'a>> {
    let (i0, _) = self.peek()?;
    self.next();
    while let Some((_, c1)) = self.peek() {
      if !c1.is_ascii_digit() {
        break;
      }
      // TODO: decimal and scientific notation, hexadecimal, etc
      self.next();
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
    self.next();
    while let Some((_, c1)) = self.peek() {
      let ty1 = get_char_type(c1);
      let pass = match ty1 {
        CharType::Alpha => s == State::AlphaNum || s == State::Underscore,
        CharType::Number => s == State::AlphaNum,
        CharType::Symbol => s == State::Symbol || s == State::Underscore,
        CharType::Special if c1 == '_' => s == State::AlphaNum || s == State::Symbol,
        _ => false,
      };
      if pass {
        self.next();
        s = match ty1 {
          CharType::Alpha | CharType::Number => State::AlphaNum,
          CharType::Symbol => State::Symbol,
          _ => State::Underscore,
        };
      } else {
        break;
      }
    }
    return Some(self.make_token_overlooked(TokenType::Identifier, i0));
  }
}

pub fn parse(input: &str) -> Decl {
  let tokenizer = Tokenizer::new(input);
  for token in tokenizer {
    println!("{:?}", token);
  }
  unimplemented!()
}
