use crate::types::token::*;

pub struct Tracker<'a> {
  token_list: Vec<Token<'a>>,
  current_pos: usize,
  indent: Indent,
  last_eol: TokenPos,
}

impl<'a> Default for Tracker<'a> {
  fn default() -> Self {
    Tracker {
      token_list: Vec::new(),
      current_pos: 0,
      indent: Indent::Base,
      last_eol: TokenPos::EoF,
    }
  }
}

pub type TrackerState = (usize, Indent, TokenPos);

impl<'a> Tracker<'a> {
  pub fn new(tokens: Vec<Token<'a>>) -> Self {
    Tracker {
      token_list: tokens,
      current_pos: 0,
      indent: Indent::Base,
      last_eol: TokenPos::EoL(0),
    }
  }

  pub fn set_indent(&mut self, indent: Indent) {
    self.indent = match indent {
      Indent::Base => unreachable!(),
      Indent::Head(i) => Indent::Head(i),
      Indent::Cont(i) => Indent::Head(i),
    };
  }

  pub fn peek_raw(&self) -> Option<&Token<'a>> {
    self.token_list.get(self.current_pos)
  }

  pub fn peek(&self) -> Option<&Token<'a>> {
    let i = self.indent;
    self.peek_raw().filter(|t| i < t.indent)
  }

  pub fn next(&mut self) -> Option<&Token<'a>> {
    self.last_eol = match self.peek_raw() {
      Some(t) => match t.pos {
        TokenPos::Pos(l, _) => TokenPos::EoL(l),
        _ => unreachable!(),
      },
      _ => TokenPos::EoF,
    };
    self.current_pos += 1;
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
    self.last_eol
  }

  pub fn pos(&self) -> TokenPos {
    let last_pos = self.end_of_line();
    self.peek().map_or(last_pos, |t| t.pos)
  }

  pub fn save_indent(&self) -> Indent {
    self.indent
  }

  pub fn restore_indent(&mut self, indent: Indent) {
    self.indent = indent;
  }

  pub fn save_state(&self) -> TrackerState {
    (self.current_pos, self.indent, self.last_eol)
  }

  pub fn restore_state(&mut self, state: TrackerState) {
    self.current_pos = state.0;
    self.indent = state.1;
    self.last_eol = state.2;
  }
}
