use crate::types::TokenPos;
use core::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq)]
enum CharType {
  Alpha,
  Number,
  Quote,
  Reserved,
  Symbol,
}

fn get_char_type(c: char) -> CharType {
  assert!(!c.is_whitespace());
  match c {
    'a'..='z' | 'A'..='Z' => CharType::Alpha,
    '0'..='9' => CharType::Number,
    '\'' | '"' => CharType::Quote,
    '.' | ',' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | ']' => CharType::Reserved,
    _ => CharType::Symbol,
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
  Ident,      // a, xs, +, p0', if_then_else_
  Number,     // 0, 3.14, 1e-2, 0x1F, 0xcc
  Char,       // 'a', '\n', '\x1F', '\u3082'
  String,     // "Hello, World!", "a\nb"
  Precedence, // 1.-2.3<
  Reserved,   // :, =, [, }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Indent {
  Base,
  Head(u16),
  Cont(u16),
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
  pub str: &'a str,
  pub ty: TokenType,
  pub pos: TokenPos,
  pub indent: Indent,
}

#[derive(Debug, Clone)]
struct Loc<'a> {
  str: &'a str,
  ln: u32,
  col: u16,
  indent: u16,
}

type CharLoc<'a> = (Loc<'a>, char);

struct Tokenizer<'a, I: Iterator<Item = CharLoc<'a>>> {
  iter: Peekable<I>,
  errors: Vec<String>,
}

impl<'a, I: Iterator<Item = CharLoc<'a>>> Tokenizer<'a, I> {
  fn new(input: I) -> Self {
    Tokenizer {
      iter: input.peekable(),
      errors: Vec::new(),
    }
  }

  fn make_token_internal(&self, loc: &Loc<'a>, str: &'a str, ty_base: TokenType) -> Token<'a> {
    let ty = match str {
      "=" | "_" => TokenType::Reserved,
      _ => ty_base,
    };
    if str == "syntax" {
      todo!("precedence");
    }
    Token {
      str,
      ty,
      pos: TokenPos::Pos(loc.ln, loc.col + loc.indent),
      indent: if loc.col == 0 {
        Indent::Head(loc.indent)
      } else {
        Indent::Cont(loc.indent)
      },
    }
  }

  fn make_token(&mut self, l0: &Loc<'a>, ty: TokenType) -> Token<'a> {
    let (l1, c1) = self.iter.next().unwrap();
    self.make_token_internal(
      l0,
      &l0.str[l0.col as usize..l1.col as usize + c1.len_utf8()],
      ty,
    )
  }

  fn make_token_addr(&mut self, addr: usize, l0: &Loc<'a>, ty: TokenType) -> Token<'a> {
    let str = match self.iter.peek() {
      Some((l1, c1)) if l0.ln == l1.ln => &l0.str[addr..l1.col as usize],
      _ => &l0.str[addr..],
    };
    self.make_token_internal(l0, str, ty)
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}: {}", pos.to_string(), msg);
    eprintln!("{}", s);
    self.errors.push(s);
  }

  fn add_error_loc(&mut self, l: &Loc<'a>, msg: &str) {
    self.add_error(TokenPos::Pos(l.ln, l.col), msg);
  }

  fn skip_space(&mut self) {
    while let Some((l, c)) = self.iter.peek() {
      if c.is_whitespace() {
        let c = *c;
        if c != ' ' {
          let l = l.clone();
          self.add_error_loc(&l, &format!("invalid whitespace character: {:?}", c));
        }
        self.iter.next();
      } else {
        break;
      }
    }
  }

  fn skip_escape(&mut self) {
    let (l, _) = self.iter.next().unwrap();
    if let Some((_, c2)) = self.iter.peek() {
      if *c2 == 'x' || *c2 == 'u' {
        // hexadecimal or unicode escape
        unimplemented!()
      } else {
        self.iter.next();
      }
    } else {
      self.add_error_loc(&l, "unterminated escape character");
    }
  }

  fn string_literal(&mut self) -> Token<'a> {
    let (l0, c0) = self.iter.next().unwrap();
    let ty = if c0 == '\'' {
      TokenType::Char
    } else {
      TokenType::String
    };
    let l = l0.col as usize + c0.len_utf8();
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln {
        self.add_error(
          TokenPos::EoL(l0.ln),
          if c0 == '\'' {
            "unterminated character literal"
          } else {
            "unterminated string literal"
          },
        );
        return self.make_token_addr(l, &l0, ty);
      }
      if c0 == *c1 {
        let t = self.make_token_addr(l, &l0, ty);
        self.iter.next();
        return t;
      }
      if *c1 == '\\' {
        self.skip_escape()
      } else {
        self.iter.next();
      }
    }
    self.add_error(TokenPos::EoF, "unterminated character or string literal");
    return self.make_token_addr(l, &l0, ty);
  }

  fn number_literal(&mut self) -> Token<'a> {
    let (l0, c0) = self.iter.peek().unwrap().clone();
    self.iter.next();
    enum State {
      Zero,    // 0
      Int,     // 123, 0x5c
      Frac,    // 1.23
      Exp,     // 1e
      ExpSign, // 1e+
      Last,    // 1e+4
    }
    let mut hex = false;
    let mut s = if c0 == '0' { State::Zero } else { State::Int };
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln {
        break;
      }
      let c1 = *c1;
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
            s = State::ExpSign;
          } else if c1.is_ascii_digit() {
            s = State::Last;
          } else {
            break;
          }
        }
        State::ExpSign => {
          if c1.is_ascii_digit() {
            s = State::Last;
          } else {
            break;
          }
        }
        State::Last => {
          if !c1.is_ascii_digit() {
            break;
          }
        }
      }
      self.iter.next();
    }
    match s {
      State::Exp | State::ExpSign => {
        self.add_error_loc(&l0, "incomplete number literal");
      }
      _ => {}
    }
    return self.make_token_addr(l0.col as usize, &l0, TokenType::Number);
  }
}

impl<'a, I: Iterator<Item = (Loc<'a>, char)>> Iterator for Tokenizer<'a, I> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip_space();
    let (l0, c0) = self.iter.peek()?.clone();
    let ty = get_char_type(c0);
    if ty == CharType::Reserved {
      return Some(self.make_token(&l0, TokenType::Reserved));
    }
    if ty == CharType::Quote {
      return Some(self.string_literal());
    }
    if ty == CharType::Number {
      return Some(self.number_literal());
    }

    // identifier
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln {
        break;
      }
      if c1.is_whitespace() {
        let c1 = *c1;
        if c1 != ' ' {
          self.add_error_loc(&l0, &format!("invalid whitespace character: {:?}", c1));
        }
        break;
      }
      // prime is allowed to use in identifier
      if *c1 != '\'' {
        let ty1 = get_char_type(*c1);
        if ty1 == CharType::Reserved || ty1 == CharType::Quote {
          break;
        }
      }
      self.iter.next();
    }

    Some(self.make_token_addr(l0.col as usize, &l0, TokenType::Ident))
  }
}

pub fn tokenize<'a>(code: &'a str) -> Result<Vec<Token<'a>>, Vec<String>> {
  let iter = code
    .lines()
    .enumerate()
    .map(|(ln, line)| {
      let ln = ln as u32;
      let (spaces, rest) = line.split_at(line.chars().take_while(|c| *c == ' ').count());
      let indent = spaces.len() as u16;

      // remove comments
      let rest = match rest.find(" --") {
        Some(i) => &rest[..i],
        None => {
          if rest.len() >= 2 && &rest[0..2] == "--" {
            ""
          } else {
            rest
          }
        }
      };

      rest.char_indices().map(move |(col, c)| {
        let col = col as u16;
        (
          Loc {
            str: &rest,
            ln,
            col,
            indent,
          },
          c,
        )
      })
    })
    .flatten();
  let mut tokenizer = Tokenizer::new(iter);
  let mut tokens = Vec::new();
  while let Some(t) = tokenizer.next() {
    tokens.push(t);
  }
  if tokenizer.errors.is_empty() {
    Ok(tokens)
  } else {
    Err(tokenizer.errors)
  }
}

#[cfg(test)]
#[test]
fn test() {
  let str0 = r#"
    x
    d "a\nb"
    da p + λ(x: X) → x = x -- mochi
    -- comment
-- head comment
----long
    141 -------long2
      312 0x56c 0b1010 0o7
      1e+6 1e-6 1.12e+6 1.g
    32. 0.32 0.32e4 0.32e+6 0.32e-6
    0x1.23ap32 0x1.23ap32 0x1.23ap+32 0x1.23p-32
    12"#;
  assert!(tokenize(str0).is_ok());
  assert!(tokenize(r#"1 2 3"#).is_ok());
  assert!(tokenize(r#"1 2e+ 3"#).is_err());

  fn splitter(s: &str) -> Vec<(&str, TokenType)> {
    tokenize(s)
      .unwrap_or(vec![])
      .into_iter()
      .map(|t| (t.str, t.ty))
      .collect()
  }
  assert_eq!(
    splitter("1.2 a.3"),
    vec![
      ("1.2", TokenType::Number),
      ("a", TokenType::Ident),
      (".", TokenType::Reserved),
      ("3", TokenType::Number),
    ]
  );
  assert_eq!(
    splitter("x += y .= z αβ"),
    vec![
      ("x", TokenType::Ident),
      ("+=", TokenType::Ident),
      ("y", TokenType::Ident),
      (".", TokenType::Reserved),
      ("=", TokenType::Reserved),
      ("z", TokenType::Ident),
      ("αβ", TokenType::Ident),
    ]
  );
  assert_eq!(
    splitter("1.2.3.4.5"),
    vec![
      ("1.2", TokenType::Number),
      (".", TokenType::Reserved),
      ("3.4", TokenType::Number),
      (".", TokenType::Reserved),
      ("5", TokenType::Number),
    ]
  );
  assert_eq!(
    splitter("syntax 1.-2.3.4< x"),
    vec![
      ("syntax", TokenType::Ident),
      ("1.-2.3.4<", TokenType::Precedence),
      ("x", TokenType::Ident),
    ]
  );
  assert_eq!(
    splitter("syntax -x"),
    vec![
      ("syntax", TokenType::Ident),
      ("-", TokenType::Ident),
      ("x", TokenType::Ident),
    ]
  );
}
