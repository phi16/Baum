use crate::types::common::LevelId;

#[derive(Debug, Clone)]
pub enum LevelRel {
  Eq,
  Le,
  Lt,
}

#[derive(Debug, Clone)]
pub enum Level {
  Id(LevelId),
  Max(Vec<LevelId>, Vec<LevelId>), // le, lt
}

pub fn max_level(levels: Vec<Level>) -> Level {
  if levels.is_empty() {
    Level::Max(Vec::new(), Vec::new())
  } else if levels.len() == 1 {
    levels.into_iter().next().unwrap()
  } else {
    let mut les = Vec::new();
    let mut lts = Vec::new();
    for l in levels {
      match l {
        Level::Id(i) => les.push(i),
        Level::Max(eis, tis) => {
          les.extend(eis);
          lts.extend(tis);
        }
      }
    }
    if les.len() == 1 && lts.is_empty() {
      Level::Id(les[0])
    } else {
      Level::Max(les, lts)
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
