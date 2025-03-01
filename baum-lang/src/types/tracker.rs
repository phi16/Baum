use crate::types::token::{ErrorPos, Indent, Token, TokenLoc, TokenRange, TokenType};

#[derive(Clone)]
pub struct TrackerState {
  current_pos: usize,
  indent: Indent,
  last_eol: ErrorPos,
  last_pos: (u32, u32),
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
        last_eol: ErrorPos::EoL(0),
        last_pos: (0, 0),
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

  pub fn next(&mut self) {
    match self.peek_raw() {
      Some(t) => {
        let line = t.pos.line;
        let column = t.pos.column;
        let length = t.pos.length;
        self.state.last_eol = ErrorPos::Pos(line, column);
        self.state.last_pos = (line, column + length);
        self.state.current_pos += 1;
      }
      _ => {
        self.state.last_eol = ErrorPos::EoF;
      }
    };
  }

  pub fn skip_to_next_head(&mut self) {
    while let Some(t) = self.peek() {
      match t.indent {
        Indent::Head(_) => break,
        _ => self.next(),
      };
    }
  }

  pub fn epos(&self) -> ErrorPos {
    let last_pos = self.state.last_eol;
    self
      .peek()
      .map_or(last_pos, |t| ErrorPos::Pos(t.pos.line, t.pos.column))
  }

  pub fn get_location(&self) -> TokenLoc {
    TokenLoc::new(self.state.current_pos)
  }

  pub fn make_range(&self, begin: TokenLoc, end: TokenLoc) -> TokenRange {
    let begin_pos = match self.token_list.get(begin.clone().into_inner()) {
      Some(t) => (t.pos.line, t.pos.column),
      None => match self.token_list.last() {
        Some(t) => (t.pos.line, t.pos.column + t.pos.length),
        None => (0, 0),
      },
    };
    TokenRange {
      begin_pos,
      begin,
      end,
    }
  }

  pub fn range_from(&self, begin: TokenLoc) -> TokenRange {
    self.make_range(begin, self.get_location())
  }

  pub fn range_extend(&self, base: TokenRange) -> TokenRange {
    self.make_range(base.begin, self.get_location())
  }

  pub fn range_here(&self) -> TokenRange {
    self.make_range(self.get_location(), self.get_location())
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
