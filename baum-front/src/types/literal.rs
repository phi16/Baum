#[derive(Debug, Clone)]
pub struct Nat(pub u32);

#[derive(Debug, Clone)]
pub struct Rat {
  pub denom: u32,
  pub base: u8,
  pub exponent: i32,
}

#[derive(Debug, Clone)]
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}
