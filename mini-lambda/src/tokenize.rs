use core::iter::Peekable;

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
pub enum TokenType {
  Identifier, // a, xs, _+_, p0', if_then_else, ex_*_
  Keyword,    // let, in, where, infix, prim
  Number,     // 0, 3.14, 1e-2, 0x1F, 0xcc
  Char,       // 'a', '\n', '\x1F', '\u3082'
  String,     // "Hello, World!", "a\nb"
  Symbol,     // ., =, [, }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Copy)]
pub enum Indent {
  Base,
  Head(u16),
  Inf,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
  pub str: &'a str,
  pub ty: TokenType,
  pub line: u32,
  pub column: u16,
  pub indent: Indent,
}

pub struct Tokenizer<'a> {
  input: &'a str,
  char_indices: Peekable<std::str::CharIndices<'a>>,
  next: Option<(usize, char)>, // look ahead 2 characters
  line: u32,
  column: u16,
  indent: u16,
  token_state: (bool, u32, u16, u16),
  head: bool,
}

impl<'a> Tokenizer<'a> {
  pub fn new(input: &'a str) -> Self {
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
        "=" | "->" => TokenType::Symbol,
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
