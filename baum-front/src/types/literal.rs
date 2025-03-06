#[derive(Clone)]
pub struct Nat {
  pub coeff: u32,
  pub base: u8,
  pub exponent: u32,
}

impl std::fmt::Debug for Nat {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.exponent == 0 {
      write!(f, "{}", self.coeff)
    } else {
      write!(f, "{}×{}^{}", self.coeff, self.base, self.exponent)
    }
  }
}

#[derive(Clone)]
pub struct Rat {
  pub coeff: u32,
  pub base: u8,
  pub exponent: i32,
}

impl std::fmt::Debug for Rat {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.exponent == 0 {
      write!(f, "{}", self.coeff)
    } else {
      write!(f, "{}×{}^{}", self.coeff, self.base, self.exponent)
    }
  }
}

#[derive(Debug, Clone)]
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}
