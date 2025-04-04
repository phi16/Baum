use crate::types::common::LevelId;

#[derive(Debug, Clone)]
pub enum LevelRel {
  Eq,
  Le,
  Lt,
}

#[derive(Debug, Clone)]
pub enum Level {
  Zero,
  Id(LevelId),
  Max(Vec<LevelId>),
}

pub fn max_level(levels: Vec<Level>) -> Level {
  if levels.is_empty() {
    Level::Zero
  } else if levels.len() == 1 {
    levels.into_iter().next().unwrap()
  } else {
    let mut ids = Vec::new();
    for l in levels {
      match l {
        Level::Zero => {}
        Level::Id(i) => ids.push(i),
        Level::Max(is) => ids.extend(is),
      }
    }
    if ids.is_empty() {
      Level::Zero
    } else if ids.len() == 1 {
      Level::Id(ids[0])
    } else {
      Level::Max(ids)
    }
  }
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
