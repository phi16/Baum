use crate::types::common::LevelId;

#[derive(Debug, Clone)]
pub enum LevelRel {
  Eq,
  Le,
  Lt,
}

pub type GroupIndex = u32;

#[derive(Debug, Clone)]
pub enum LevelRef {
  Id(LevelId),
  Group(GroupIndex),
}

pub type Constraints = Vec<(LevelId, LevelRel, LevelId, String)>;

#[derive(Debug, Clone)]
pub struct Solution {
  pub group_count: usize,
  pub replacer: Vec<(LevelId, GroupIndex)>,
  pub constraints: Vec<(LevelRef, LevelRel, LevelRef, String)>,
}
