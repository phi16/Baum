use crate::types::common::LevelId;

#[derive(Debug, Clone)]
pub enum LevelRel {
  Eq,
  Le,
  Lt,
}

#[derive(Debug, Clone)]
pub enum LevelRef {
  Id(LevelId),
  Group(u32),
}

pub type Constraints = Vec<(LevelId, LevelRel, LevelId, String)>;

#[derive(Debug, Clone)]
pub struct Solution {
  pub groups: u32,
  pub scope: Vec<(LevelId, u32)>,
  pub constraints: Vec<(LevelRef, LevelRel, LevelRef, String)>,
}
