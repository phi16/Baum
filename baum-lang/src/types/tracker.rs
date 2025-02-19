use crate::types::token::{Indent, Token, TokenPos, TokenType};

#[derive(Clone)]
pub struct TrackerState {
  current_pos: usize,
  indent: Indent,
  last_eol: TokenPos,
}

pub struct Tracker<'a> {
  token_list: Vec<Token<'a>>,
  state: TrackerState,
}

impl<'a> Tracker<'a> {
  pub fn new(tokens: Vec<Token<'a>>) -> Self {
    Tracker {
      token_list: tokens,
      state: TrackerState {
        current_pos: 0,
        indent: Indent::Base,
        last_eol: TokenPos::EoL(0),
      },
    }
  }

  pub fn set_indent(&mut self, indent: Indent) {
    self.state.indent = match indent {
      Indent::Base => unreachable!(),
      Indent::Head(i) => Indent::Head(i),
      Indent::Cont(i) => Indent::Head(i),
    };
  }

  fn peek_raw(&self) -> Option<&Token<'a>> {
    self.token_list.get(self.state.current_pos)
  }

  pub fn peek(&self) -> Option<&Token<'a>> {
    let i = self.state.indent;
    self.peek_raw().filter(|t| i < t.indent)
  }

  pub fn peek_ty(&self) -> Option<&TokenType> {
    self.peek().map(|t| &t.ty)
  }

  pub fn peek_str(&self) -> Option<&'a str> {
    self.peek().map(|t| t.str)
  }

  pub fn peek_ty_str(&self) -> Option<(&TokenType, &'a str)> {
    self.peek().map(|t| (&t.ty, t.str))
  }

  pub fn next(&mut self) -> Option<&Token<'a>> {
    self.state.last_eol = match self.peek_raw() {
      Some(t) => match t.pos {
        TokenPos::Pos(l, _) => TokenPos::EoL(l),
        _ => unreachable!(),
      },
      _ => TokenPos::EoF,
    };
    self.state.current_pos += 1;
    self.peek_raw()
  }

  pub fn skip_to_next_head(&mut self) {
    while let Some(t) = self.peek() {
      match t.indent {
        Indent::Head(_) => break,
        _ => self.next(),
      };
    }
  }

  pub fn end_of_line(&self) -> TokenPos {
    self.state.last_eol
  }

  pub fn pos(&self) -> TokenPos {
    let last_pos = self.end_of_line();
    self.peek().map_or(last_pos, |t| t.pos)
  }

  pub fn save_indent(&self) -> Indent {
    self.state.indent
  }

  pub fn restore_indent(&mut self, indent: Indent) {
    self.state.indent = indent;
  }

  pub fn save_state(&self) -> TrackerState {
    self.state.clone()
  }

  pub fn restore_state(&mut self, state: TrackerState) {
    self.state = state;
  }

  pub fn is_done(&self) -> bool {
    self.peek_raw().is_none()
  }
}
