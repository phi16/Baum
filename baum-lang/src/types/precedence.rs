use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrecEps {
  NegEps,
  Zero,
  PosEps,
}

#[derive(Debug, Clone)]
pub struct PrecLevel(pub Vec<i16>);

impl PartialEq for PrecLevel {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl Eq for PrecLevel {}

impl PartialOrd for PrecLevel {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.cmp(other).into()
  }
}

impl Ord for PrecLevel {
  fn cmp(&self, other: &Self) -> Ordering {
    let mut i = 0;
    loop {
      match self.0[i].cmp(&other.0[i]) {
        Ordering::Equal => i += 1,
        o => return o,
      }

      if i == self.0.len() {
        match other.0[i..].iter().find(|&&x| x != 0) {
          Some(x) => return 0.cmp(&x),
          None => return Ordering::Equal,
        }
      }
      if i == other.0.len() {
        match self.0[i..].iter().find(|&&x| x != 0) {
          Some(x) => return x.cmp(&0),
          None => return Ordering::Equal,
        }
      }
    }
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
  Initial,
  Level(PrecLevel, PrecEps),
  Terminal,
}

impl std::fmt::Debug for Precedence {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Precedence::Initial => write!(f, "-∞"),
      Precedence::Level(l, e) => {
        let ls = l
          .0
          .iter()
          .map(|x| x.to_string())
          .collect::<Vec<_>>()
          .join(".");
        write!(f, "{}", ls)?;
        match e {
          PrecEps::NegEps => write!(f, "-ε")?,
          PrecEps::Zero => {
            if l.0.len() == 0 {
              write!(f, "0")?;
            }
          }
          PrecEps::PosEps => write!(f, "+ε")?,
        }
        Ok(())
      }
      Precedence::Terminal => write!(f, "∞"),
    }
  }
}

impl Precedence {
  pub fn parse(s: &str) -> Option<(Self, Self)> {
    // (-?[0-9]+)%.[<>←→]
    // example: "1.-2.3<"
    if s == "" {
      return Some((Precedence::Terminal, Precedence::Terminal));
    }
    let mut ss = s.chars().peekable();
    let mut nums = Vec::new();
    let mut left_eps = PrecEps::Zero;
    loop {
      let negated = if ss.peek() == Some(&'-') {
        ss.next();
        true
      } else {
        false
      };
      let mut nat = 0;
      let c = *ss.peek()?;
      if !c.is_ascii_digit() {
        return None;
      }
      loop {
        let c = match ss.peek() {
          Some(c) if c.is_ascii_digit() => *c,
          _ => break,
        };
        ss.next();
        nat = nat * 10 + c.to_digit(10).unwrap() as i16;
      }
      if negated {
        nat = -nat;
      }
      nums.push(nat);
      match ss.next() {
        Some('.') => {}
        Some('<') | Some('←') => {
          left_eps = PrecEps::NegEps;
          break;
        }
        Some('>') | Some('→') => {
          left_eps = PrecEps::PosEps;
          break;
        }
        Some(_) => return None,
        None => break,
      }
    }
    if ss.next().is_some() {
      return None;
    }
    if nums.len() == 0 {
      return None;
    }
    let right_eps = match left_eps {
      PrecEps::NegEps => PrecEps::PosEps,
      PrecEps::PosEps => PrecEps::NegEps,
      PrecEps::Zero => PrecEps::Zero,
    };
    let mut left = Precedence::Level(PrecLevel(nums.clone()), left_eps);
    let right = Precedence::Level(PrecLevel(nums), right_eps);
    if right <= Precedence::Level(PrecLevel(vec![0]), PrecEps::Zero) {
      // allows `1 + π(0) x` for prefixs
      left = Precedence::Terminal;
    }
    Some((left, right))
  }
}

#[cfg(test)]
#[test]
fn prec_parse_test() {
  assert!(Precedence::parse("").is_some());
  assert!(Precedence::parse("0").is_some());
  assert!(Precedence::parse("1.2").is_some());
  assert_eq!(
    Precedence::parse("1.2.3").unwrap().0,
    Precedence::Level(PrecLevel(vec![1, 2, 3]), PrecEps::Zero)
  );
  assert!(Precedence::parse("1.-2.3<").is_some());
  assert!(Precedence::parse("1.-2.3→").is_some());
  assert_eq!(
    Precedence::parse("10.42.2048>").unwrap().0,
    Precedence::Level(PrecLevel(vec![10, 42, 2048]), PrecEps::PosEps)
  );
  assert!(Precedence::parse("-10.42..2048>").is_none());
  assert!(Precedence::parse("--10.42.2048>").is_none());
  assert!(Precedence::parse("-10.42.2048>>").is_none());
}
