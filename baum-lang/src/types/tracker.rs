use crate::types::token::{ErrorPos, Indent, Token, TokenIx, TokenRange, TokenType};

#[derive(Clone)]
pub struct TrackerState {
  current_pos: usize,
  indent: Indent,
  last_eol: ErrorPos,
  last_pos: TokenIx,
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
        last_pos: TokenIx::new(0),
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
        self.state.last_eol = ErrorPos::EoL(line);
        self.state.last_pos = TokenIx::new(self.state.current_pos);
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
    self.peek().map_or(last_pos, |t| ErrorPos::Ix(t.ix))
  }

  pub fn get_location(&self) -> TokenIx {
    TokenIx::new(self.state.current_pos)
  }

  pub fn make_range(&self, begin: TokenIx, end: TokenIx) -> TokenRange {
    TokenRange { begin, end }
  }

  pub fn range_single(&self, loc: TokenIx) -> TokenRange {
    self.make_range(loc.clone(), TokenIx::new(loc.into_inner() + 1))
  }

  pub fn range_from(&self, begin: TokenIx) -> TokenRange {
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
