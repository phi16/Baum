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
pub enum TokenPos {
  Pos(u32, u16),
  EoL(u32),
  EoF,
}

impl TokenPos {
  pub fn to_string(&self) -> String {
    match self {
      TokenPos::Pos(ln, col) => format!("L{} C{}", ln + 1, col + 1),
      TokenPos::EoL(ln) => format!("End of L{}", ln + 1),
      TokenPos::EoF => "End of file".to_string(),
    }
  }
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
