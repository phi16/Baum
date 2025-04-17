#[derive(Clone)]
pub struct BigNat {
  pub digits_rev: Vec<u32>,
}

impl BigNat {
  pub fn new() -> Self {
    Self { digits_rev: vec![] }
  }

  pub fn from_u32(n: u32) -> Self {
    Self {
      digits_rev: vec![n],
    }
  }

  pub fn add(&mut self, rhs: u32) {
    let mut carry = rhs as u32;
    for d in &mut self.digits_rev {
      let sum = *d as u64 + carry as u64;
      *d = sum as u32;
      carry = (sum >> 32) as u32;
      if carry == 0 {
        return;
      }
    }
    if carry > 0 {
      self.digits_rev.push(carry);
    }
  }

  pub fn multiply(&mut self, rhs: u32) {
    let mut carry = 0;
    for d in &mut self.digits_rev {
      let product = *d as u64 * rhs as u64 + carry as u64;
      *d = product as u32;
      carry = (product >> 32) as u32;
    }
    if carry > 0 {
      self.digits_rev.push(carry);
    }
  }

  pub fn divide(&mut self, rhs: u32) -> u32 {
    assert!(rhs != 0);
    let mut carry = 0;
    for d in self.digits_rev.iter_mut().rev() {
      let dividend = ((carry as u64) << 32) + *d as u64;
      *d = (dividend / rhs as u64) as u32;
      carry = (dividend % rhs as u64) as u32;
    }
    self.normalize();
    carry
  }

  pub fn from_vec(digits: Vec<u32>, base: u32) -> Self {
    let mut n = Self::new();
    for d in digits {
      n.multiply(base);
      n.add(d);
    }
    n
  }

  pub fn into_vec(self, base: u32) -> Vec<u32> {
    let mut n = self;
    let mut digits = Vec::new();
    while !n.is_zero() {
      let r = n.divide(base);
      digits.push(r);
    }
    digits
  }

  pub fn is_zero(&self) -> bool {
    self.digits_rev.is_empty()
  }

  fn normalize(&mut self) {
    while let Some(&0) = self.digits_rev.last() {
      self.digits_rev.pop();
    }
  }
}

impl std::fmt::Debug for BigNat {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    if self.digits_rev.is_empty() {
      write!(f, "0")?;
      return Ok(());
    }
    let mut it = self.digits_rev.iter().rev();
    write!(f, "{}", it.next().unwrap())?;
    for d in it {
      write!(f, ";{}", d)?;
    }
    Ok(())
  }
}

impl std::fmt::Display for BigNat {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let digits = self.clone().into_vec(1000000000);
    if digits.is_empty() {
      write!(f, "0")?;
      return Ok(());
    }
    let mut it = digits.iter().rev();
    write!(f, "{}", it.next().unwrap())?;
    for d in it {
      write!(f, "{:09}", d)?;
    }
    Ok(())
  }
}

#[derive(Clone)]
pub struct BigInt {
  pub negative: bool,
  pub nat: BigNat,
}

impl BigInt {
  pub fn new() -> Self {
    Self {
      negative: false,
      nat: BigNat::new(),
    }
  }

  pub fn from_nat(negative: bool, nat: BigNat) -> Self {
    Self { negative, nat }
  }

  pub fn into_inner(self) -> (bool, BigNat) {
    (self.negative, self.nat)
  }

  pub fn subtract(&mut self, rhs: u32) {
    if rhs == 0 {
      return;
    }
    if self.nat.is_zero() {
      self.negative = true;
      self.nat.add(rhs);
      return;
    }
    if self.negative {
      self.nat.add(rhs);
      return;
    }
    if self.nat.digits_rev.len() == 1 && self.nat.digits_rev[0] < rhs {
      self.nat.digits_rev[0] = rhs - self.nat.digits_rev[0];
      self.negative = true;
      return;
    }
    let mut carry = rhs as u32;
    for d in &mut self.nat.digits_rev {
      if *d < carry {
        *d = ((1u64 << 32) + *d as u64 - carry as u64) as u32;
        carry = 1;
      } else {
        *d -= carry;
        carry = 0;
        break;
      }
    }
    assert_eq!(carry, 0);
    self.nat.normalize();
  }
}

impl std::fmt::Debug for BigInt {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    if self.negative {
      write!(f, "-")?;
    }
    write!(f, "{:?}", self.nat)
  }
}

impl std::fmt::Display for BigInt {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    if self.negative {
      write!(f, "-")?;
    }
    write!(f, "{}", self.nat)
  }
}

#[cfg(test)]
#[test]
fn big_nat_test() {
  fn test(n: &BigNat, s: &str) {
    assert_eq!(&format!("{:?}", n), s);
  }
  test(&BigNat::new(), "0");
  test(&BigNat::from_vec(vec![1, 2, 3], 10), "123");
  test(&BigNat::from_u32(123456789), "123456789");
  let big_nat = BigNat::from_vec(vec![99999999, 99999999, 99999999], 100000000);
  test(&big_nat, "54210;466537709;2701131775");
  assert_eq!(big_nat.to_string(), "999999999999999999999999");
}

#[cfg(test)]
#[test]
fn big_int_test() {
  fn test(n: &BigInt, s: &str) {
    assert_eq!(&format!("{:?}", n), s);
  }
  let a = BigNat::from_vec(vec![99999999, 99999999, 99999999], 100000000);
  let a_pos = BigInt::from_nat(false, a.clone());
  let a_neg = BigInt::from_nat(true, a.clone());
  test(&a_pos, "54210;466537709;2701131775");
  test(&a_neg, "-54210;466537709;2701131775");

  let mut a = a_pos.clone();
  a.subtract(1111111111);
  test(&a, "54210;466537709;1590020664");
  let mut a = a_neg.clone();
  a.subtract(1111111111);
  test(&a, "-54210;466537709;3812242886");
  let mut a = a_pos.clone();
  a.subtract(3333333333);
  test(&a, "54210;466537708;3662765738");
  let mut a = a_neg.clone();
  a.subtract(3333333333);
  test(&a, "-54210;466537710;1739497812");

  let b = BigNat::from_vec(vec![1, 0, 0, 0, 0], 65536);
  let mut b = BigInt::from_nat(false, b);
  test(&b, "1;0;0");
  b.subtract(1);
  test(&b, "4294967295;4294967295");
}

#[derive(Clone)]
pub struct Nat {
  pub coeff: BigNat,
  pub base: u8,
  pub exponent: BigNat,
}

impl std::fmt::Debug for Nat {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.exponent.is_zero() {
      write!(f, "{}", self.coeff)
    } else {
      write!(f, "{}×{}^{}", self.coeff, self.base, self.exponent)
    }
  }
}

#[derive(Clone)]
pub struct Rat {
  pub coeff: BigNat,
  pub base: u8,
  pub negative_exponent: BigNat,
}

impl std::fmt::Debug for Rat {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}×{}^-{}",
      self.coeff, self.base, self.negative_exponent
    )
  }
}

#[derive(Debug, Clone)]
pub enum Literal {
  Nat(Nat),
  Rat(Rat),
  Chr(char),
  Str(String),
}

// Note: These are temporary definitions...

impl Into<u32> for Nat {
  fn into(self) -> u32 {
    assert_eq!(self.base, 10);
    let mut n = 0;
    for d in self.coeff.digits_rev {
      n = n * 10 + d;
    }
    n
  }
}

impl Into<f32> for Rat {
  fn into(self) -> f32 {
    assert_eq!(self.base, 10);
    let mut n = 0;
    for d in self.coeff.digits_rev {
      n = n * 10 + d;
    }
    let mut d = 1;
    for e in self.negative_exponent.digits_rev {
      d *= 10u32.pow(e);
    }
    n as f32 / d as f32
  }
}
