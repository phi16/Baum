#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
  Ident,      // a, xs, +, p0', _
  DecNat,     // 0, 1, 2, ...
  Number,     // 0, 0x1F, 0xcc, 3.14, 1e-2, 0x1.23ap32
  Char,       // 'a', '\n', '\x1F', '\u3082'
  String,     // "Hello, World!", "a\nb"
  Precedence, // 1.-2.3<
  Reserved,   // :, =, [, }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPos {
  pub line: u32,
  pub column: u32,
  pub length: u32,
}

#[derive(Debug, Clone)]
pub struct TokenRange {
  pub begin_line: u32,
  pub begin_column: u32,
  pub end_line: u32,
  pub end_column: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorPos {
  Pos(u32, u32),
  EoL(u32),
  EoF,
}

impl From<TokenPos> for ErrorPos {
  fn from(pos: TokenPos) -> Self {
    ErrorPos::Pos(pos.line, pos.column)
  }
}

impl From<TokenRange> for ErrorPos {
  fn from(pos: TokenRange) -> Self {
    ErrorPos::Pos(pos.begin_line, pos.begin_column)
  }
}

impl ErrorPos {
  pub fn to_string(&self) -> String {
    match self {
      ErrorPos::Pos(ln, col) => format!("L{} C{}", ln + 1, col + 1),
      ErrorPos::EoL(ln) => format!("End of L{}", ln + 1),
      ErrorPos::EoF => "End of file".to_string(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Indent {
  Base,
  Head(u32),
  Cont(u32),
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
  pub str: &'a str,
  pub ty: TokenType,
  pub pos: TokenPos,
  pub indent: Indent,
}
