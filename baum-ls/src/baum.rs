use baum_lang::types;

pub enum TokenType {
  Def,
  Bind,
  Unknown,
  Prop,
  Module,

  Syntax,
  Keyword,
  Symbol,
  Precedence,

  String,
  Number,

  Comment,
}

pub struct TokenData {
  pub line: u32,
  pub column: u32,
  pub length: u32,
  pub token_type: TokenType,
}

pub fn tokenize_example(code: &str) -> Result<Vec<TokenData>, Vec<String>> {
  let lines = code.lines().collect::<Vec<_>>();
  let (tokens, comments, _) = baum_lang::tokenize::tokenize(code);
  let tokens_iter = tokens.into_iter().map(|t| {
    let column = t.pos.column;
    let length = t.pos.length;
    let mut l = lines.get(t.pos.line as usize).unwrap().chars();
    let utf16_column = l
      .by_ref()
      .take(column as usize)
      .map(|c| c.len_utf16())
      .sum::<usize>() as u32;
    let utf16_length = l
      .take(length as usize)
      .map(|c| c.len_utf16())
      .sum::<usize>() as u32;
    TokenData {
      line: t.pos.line,
      column: utf16_column,
      length: utf16_length,
      token_type: match t.ty {
        types::token::TokenType::Ident => match t.str {
          "syntax" | "local" | "module" | "let" | "in" => TokenType::Keyword,
          _ => TokenType::Unknown,
        },
        types::token::TokenType::DecNat => TokenType::Number,
        types::token::TokenType::Number => TokenType::Number,
        types::token::TokenType::Char => TokenType::String,
        types::token::TokenType::String => TokenType::String,
        types::token::TokenType::Precedence => TokenType::Precedence,
        types::token::TokenType::Reserved => TokenType::Symbol,
      },
    }
  });
  let comments_iter = comments.into_iter().map(|(line, column)| {
    let mut l = lines.get(line as usize).unwrap().chars();
    let utf16_column = l
      .by_ref()
      .take(column as usize)
      .map(|c| c.len_utf16())
      .sum::<usize>() as u32;
    let utf16_length = l.map(|c| c.len_utf16()).sum::<usize>() as u32;
    TokenData {
      line,
      column: utf16_column,
      length: utf16_length,
      token_type: TokenType::Comment,
    }
  });
  let mut tokens_iter = tokens_iter.peekable();
  let mut comments_iter = comments_iter.peekable();
  let mut res = Vec::new();
  loop {
    match (tokens_iter.peek(), comments_iter.peek()) {
      (Some(t), Some(c)) => {
        let token_first = if t.line < c.line {
          true
        } else if t.line > c.line {
          false
        } else {
          t.column < c.column
        };
        if token_first {
          res.push(tokens_iter.next().unwrap());
        } else {
          res.push(comments_iter.next().unwrap());
        }
      }
      (Some(_), None) => {
        res.extend(tokens_iter);
        break;
      }
      (None, Some(_)) => {
        res.extend(comments_iter);
        break;
      }
      (None, None) => {
        break;
      }
    }
  }
  Ok(res)
}
