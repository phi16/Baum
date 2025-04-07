use crate::types::token::{ErrorPos, Indent, Token, TokenIx, TokenPos, TokenType};
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
    '.' | ',' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | '|' | ']' => CharType::Reserved,
    _ => CharType::Symbol,
  }
}

#[derive(Debug, Clone)]
struct Loc<'a> {
  str: &'a str,
  ln: u32,
  col_chars: u32,
  col_bytes: u32,
  indent: u32,
}

type CharLoc<'a> = (Loc<'a>, char);

struct Tokenizer<'a, I: Iterator<Item = CharLoc<'a>>> {
  iter: Peekable<I>,
  errors: Vec<(ErrorPos, String)>,
  in_syntax: bool,
  after_dot: bool,
  token_index: usize,
}

impl<'a, I: Iterator<Item = CharLoc<'a>>> Tokenizer<'a, I> {
  fn new(input: I) -> Self {
    Tokenizer {
      iter: input.peekable(),
      errors: Vec::new(),
      in_syntax: false,
      after_dot: false,
      token_index: 0,
    }
  }

  fn make_token_internal(&mut self, loc: &Loc<'a>, str: &'a str, ty_base: TokenType) -> Token<'a> {
    let ty = match str {
      "=" => TokenType::Reserved,
      _ => ty_base,
    };
    self.in_syntax = str == "syntax";
    self.after_dot = str == ".";
    let mut len = str.chars().count();
    match ty {
      TokenType::Char | TokenType::String => {
        len += 2;
      }
      _ => {}
    }
    let token_index = self.token_index;
    self.token_index += 1;
    Token {
      str,
      ty,
      pos: TokenPos {
        line: loc.ln,
        column: loc.col_chars + loc.indent,
        length: len as u32,
      },
      ix: TokenIx::new(token_index),
      indent: if loc.col_chars == 0 {
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
      &l0.str[l0.col_bytes as usize..l1.col_bytes as usize + c1.len_utf8()],
      ty,
    )
  }

  fn make_token_addr(&mut self, addr: usize, l0: &Loc<'a>, ty: TokenType) -> Token<'a> {
    let str = match self.iter.peek() {
      Some((l1, _)) if l0.ln == l1.ln => &l0.str[addr..l1.col_bytes as usize],
      _ => &l0.str[addr..],
    };
    self.make_token_internal(l0, str, ty)
  }

  fn add_error(&mut self, pos: ErrorPos, msg: &str) {
    self.errors.push((pos, msg.to_string()));
  }

  fn add_error_loc(&mut self, l: &Loc<'a>, len: u32, msg: &str) {
    self.add_error(ErrorPos::Pos(l.ln, l.col_chars, len), msg);
  }

  fn skip_space(&mut self) {
    while let Some((l, c)) = self.iter.peek() {
      if c.is_whitespace() {
        let c = *c;
        if c != ' ' {
          let l = l.clone();
          self.add_error_loc(&l, 1, &format!("invalid whitespace character: {:?}", c));
        }
        self.iter.next();
      } else {
        break;
      }
    }
  }

  fn string_literal(&mut self) -> Token<'a> {
    let (l0, c0) = self.iter.next().unwrap();
    let ty = if c0 == '\'' {
      TokenType::Char
    } else {
      TokenType::String
    };
    let l = l0.col_bytes as usize + c0.len_utf8();
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln {
        self.add_error(
          ErrorPos::EoL(l0.ln),
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
        self.iter.next();
        if let Some((l1, _)) = self.iter.peek() {
          if l0.ln != l1.ln {
            continue;
          }
          // we don't need to care about the escape-sequence length,
          // because we are just waiting for the closing quote
          self.iter.next();
        } else {
          continue;
        }
      } else {
        self.iter.next();
      }
    }
    self.add_error(ErrorPos::EoF, "unterminated character or string literal");
    return self.make_token_addr(l, &l0, ty);
  }

  fn number_literal(&mut self) -> Token<'a> {
    // dec: [0-9]+(.[0-9]*)?([eE](+-)?[0-9]+)?
    // bin: 0b[01]*(.[01]*)? but not 0b
    // oct: 0o[0-7]*(.[0-7]*)? but not 0o
    // hex: 0x[0-9a-fA-F]*(.[0-9a-fA-F]*)?(p(+-)?[0-9]+)? but not 0x(.p(+-)?[0-9]+)?

    let (l0, c0) = self.iter.peek().unwrap().clone();
    self.iter.next();
    let mut len = 1;
    enum State {
      Zero,    // 0
      Radix,   // 0b, 0o, 0x (non-accepting state)
      Nat,     // 123, 0x5c
      Frac,    // 123., 0x1.23
      Exp,     // 1e, 1p, 0x1.23p (non-accepting state)
      ExpSign, // 1e+ (non-accepting state)
      ExpNat,  // 1e+4
    }
    let mut dec = true;
    let mut hex = false;
    let mut s = if c0 == '0' { State::Zero } else { State::Nat };
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln {
        break;
      }
      let c1 = *c1;
      match s {
        State::Zero => {
          if c1 == 'b' || c1 == 'o' || c1 == 'x' {
            hex = c1 == 'x';
            dec = false;
            s = State::Radix;
          } else if c1 == 'e' || c1 == 'E' {
            s = State::Exp;
          } else if c1 == '.' {
            s = State::Frac;
          } else if c1.is_ascii_digit() {
            s = State::Nat;
          } else {
            break;
          }
        }
        State::Radix | State::Nat => {
          if !hex && (c1 == 'e' || c1 == 'E') || hex && c1 == 'p' {
            s = State::Exp;
          } else if c1 == '.' {
            s = State::Frac;
          } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
            s = State::Nat;
          } else {
            break;
          }
        }
        State::Frac => {
          if !hex && (c1 == 'e' || c1 == 'E') || hex && c1 == 'p' {
            s = State::Exp;
          } else if c1.is_ascii_digit() || hex && c1.is_ascii_hexdigit() {
            s = State::Frac;
          } else {
            break;
          }
        }
        State::Exp => {
          if c1 == '+' || c1 == '-' {
            s = State::ExpSign;
          } else if c1.is_ascii_digit() {
            s = State::ExpNat;
          } else {
            break;
          }
        }
        State::ExpSign | State::ExpNat => {
          if c1.is_ascii_digit() {
            s = State::ExpNat;
          } else {
            break;
          }
        }
      }
      self.iter.next();
      len += 1;
    }
    match s {
      State::Radix | State::Exp | State::ExpSign => {
        self.add_error_loc(&l0, len, "incomplete number literal");
      }
      _ => {}
    }
    let ty = match s {
      State::Zero | State::Nat if dec => TokenType::DecNat,
      _ => TokenType::Number,
    };
    return self.make_token_addr(l0.col_bytes as usize, &l0, ty);
  }

  fn dec_number(&mut self, l0: &Loc<'a>) {
    while let Some((l1, c1)) = self.iter.peek() {
      if l0.ln != l1.ln || !c1.is_ascii_digit() {
        break;
      }
      self.iter.next();
    }
  }
}

impl<'a, I: Iterator<Item = (Loc<'a>, char)>> Iterator for Tokenizer<'a, I> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip_space();
    let (l0, c0) = self.iter.peek()?.clone();
    if self.in_syntax {
      let precedence = if c0 == '-' {
        self.iter.next();
        match self.iter.peek() {
          Some((_, c1)) if c1.is_ascii_digit() => true,
          _ => {
            return Some(self.make_token_addr(l0.col_bytes as usize, &l0, TokenType::Reserved));
          }
        }
      } else {
        c0.is_ascii_digit()
      };
      if precedence {
        while let Some((l1, c1)) = self.iter.peek() {
          if l0.ln != l1.ln {
            break;
          }
          if c1.is_whitespace() {
            let c1 = *c1;
            if c1 != ' ' {
              self.add_error_loc(&l0, 1, &format!("invalid whitespace character: {:?}", c1));
            }
            break;
          }
          self.iter.next();
        }
        return Some(self.make_token_addr(l0.col_bytes as usize, &l0, TokenType::Precedence));
      }
    }
    let ty = get_char_type(c0);
    if ty == CharType::Reserved {
      return Some(self.make_token(&l0, TokenType::Reserved));
    }
    if ty == CharType::Quote {
      return Some(self.string_literal());
    }
    if ty == CharType::Number {
      if self.after_dot {
        // pure decimal natural number
        self.dec_number(&l0);
        return Some(self.make_token_addr(l0.col_bytes as usize, &l0, TokenType::DecNat));
      }
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
          self.add_error_loc(&l0, 1, &format!("invalid whitespace character: {:?}", c1));
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

    Some(self.make_token_addr(l0.col_bytes as usize, &l0, TokenType::Ident))
  }
}

pub fn tokenize<'a>(code: &'a str) -> (Vec<Token<'a>>, Vec<(u32, u32)>, Vec<(ErrorPos, String)>) {
  let (comments, lines): (Vec<_>, Vec<_>) = code
    .lines()
    .enumerate()
    .map(|(ln, line)| {
      let ln = ln as u32;
      let (spaces, rest) = line.split_at(line.chars().take_while(|c| *c == ' ').count());
      let indent = spaces.chars().count() as u32;

      // remove comments
      let (comment, rest) = if rest.chars().take(2).collect::<String>() == "--" {
        (Some(0), "")
      } else {
        match rest.find(" --") {
          Some(i) => (Some(i), &rest[..i]),
          None => (None, rest),
        }
      };

      (
        comment.map(|i| (ln, indent + i as u32)),
        rest
          .char_indices()
          .enumerate()
          .map(move |(col_chars, (col_bytes, c))| {
            let col_chars = col_chars as u32;
            let col_bytes = col_bytes as u32;
            (
              Loc {
                str: &rest,
                ln,
                col_chars,
                col_bytes,
                indent,
              },
              c,
            )
          }),
      )
    })
    .unzip();
  let comments = comments.into_iter().filter_map(|c| c).collect::<Vec<_>>();
  let iter = lines.into_iter().flatten();
  let mut tokenizer = Tokenizer::new(iter);
  let mut tokens = Vec::new();
  while let Some(t) = tokenizer.next() {
    tokens.push(t);
  }
  (tokens, comments, tokenizer.errors)
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
  fn test<'a>(code: &'a str) -> Result<Vec<Token<'a>>, Vec<(ErrorPos, String)>> {
    let (tokens, _, errors) = tokenize(code);
    if errors.is_empty() {
      Ok(tokens)
    } else {
      Err(errors)
    }
  }

  assert!(test(str0).is_ok());
  assert!(test("1 2 3").is_ok());
  assert!(test("1 2e+ 3").is_err());
  assert!(test("0x").is_err());
  assert!(test("0x.12p").is_err());
  assert!(test("0x.12p+").is_err());
  assert!(test("0x.12p+4").is_ok());
  assert!(test("0x.p+4").is_ok());

  fn splitter(s: &str) -> Vec<(&str, TokenType)> {
    test(s)
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
      ("3", TokenType::DecNat),
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
      ("3", TokenType::DecNat),
      (".", TokenType::Reserved),
      ("4", TokenType::DecNat),
      (".", TokenType::Reserved),
      ("5", TokenType::DecNat),
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
      ("-", TokenType::Reserved),
      ("x", TokenType::Ident),
    ]
  );
  assert_eq!(
    splitter("k= 1"),
    vec![("k=", TokenType::Ident), ("1", TokenType::DecNat),]
  );
}
