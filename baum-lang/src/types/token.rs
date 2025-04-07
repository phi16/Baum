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
pub struct TokenIx(usize);

impl TokenIx {
  pub fn new(ix: usize) -> Self {
    TokenIx(ix)
  }
  pub fn into_inner(self) -> usize {
    self.0
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenPos {
  pub line: u32,
  pub column: u32,
  pub length: u32,
}

#[derive(Debug, Clone)]
pub struct TokenRange {
  pub begin: TokenIx,
  pub end: TokenIx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorPos {
  Ix(TokenIx),
  Pos(u32, u32, u32),
  EoL(u32),
  EoF,
}

impl From<TokenIx> for ErrorPos {
  fn from(loc: TokenIx) -> Self {
    ErrorPos::Ix(loc)
  }
}

impl From<TokenRange> for ErrorPos {
  fn from(range: TokenRange) -> Self {
    ErrorPos::Ix(range.begin)
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
  pub ix: TokenIx,
  pub indent: Indent,
}
