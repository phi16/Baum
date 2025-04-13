#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

impl std::fmt::Debug for DefId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "@{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindId(pub u32);

impl std::fmt::Debug for BindId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "%{}", self.0)
  }
}
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameId(pub u32);

impl std::fmt::Debug for NameId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct HoleId(pub u32);

impl std::fmt::Debug for HoleId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "?{}", self.0)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LevelId(pub u32);

impl std::fmt::Debug for LevelId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "_{}", self.0)
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Vis {
  Explicit,
  Implicit,
}

pub trait Tag: Clone + std::fmt::Debug + PartialEq + Eq + Default {}
