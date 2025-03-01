use crate::types::token::{ErrorPos, Indent, Token, TokenRange, TokenType};

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

  pub fn next(&mut self) -> Option<&Token<'a>> {
    match self.peek_raw() {
      Some(t) => {
        let line = t.pos.line;
        let column = t.pos.column;
        let length = t.pos.length;
        self.state.last_eol = ErrorPos::Pos(line, column);
        self.state.last_pos = (line, column + length);
      }
      _ => {
        self.state.last_eol = ErrorPos::EoF;
      }
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

  pub fn epos(&self) -> ErrorPos {
    let last_pos = self.state.last_eol;
    self
      .peek()
      .map_or(last_pos, |t| ErrorPos::Pos(t.pos.line, t.pos.column))
  }

  pub fn pos(&self) -> Option<(u32, u32)> {
    let pos = self.peek()?.pos;
    Some((pos.line, pos.column))
  }

  pub fn range_from(&self, begin: Option<(u32, u32)>) -> TokenRange {
    let begin = begin.unwrap_or(self.state.last_pos);
    TokenRange {
      begin_line: begin.0,
      begin_column: begin.1,
      end_line: self.state.last_pos.0,
      end_column: self.state.last_pos.1,
    }
  }

  pub fn range_extend(&self, base: TokenRange) -> TokenRange {
    TokenRange {
      begin_line: base.begin_line,
      begin_column: base.begin_column,
      end_line: self.state.last_pos.0,
      end_column: self.state.last_pos.1,
    }
  }

  pub fn range_here(&self) -> TokenRange {
    let pos = self.pos().unwrap_or(self.state.last_pos);
    TokenRange {
      begin_line: pos.0,
      begin_column: pos.1,
      end_line: pos.0,
      end_column: pos.1,
    }
  }

  pub fn get_location(&self) -> usize {
    self.state.current_pos
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
