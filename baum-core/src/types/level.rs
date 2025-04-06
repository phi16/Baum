use crate::types::common::LevelId;

pub type LevelOffset = u8;

#[derive(Debug, Clone)]
pub enum LevelRel {
  Eq,
  Le(LevelOffset),
}

#[derive(Debug, Clone)]
pub enum Level {
  Id(LevelId),
  Max(Vec<(LevelId, LevelOffset)>), // Max(ls) = x ⇔ ∀(l,i) ∈ ls: l+i ≤ x
}

impl Level {
  pub fn into(self) -> Vec<(LevelId, LevelOffset)> {
    match self {
      Level::Id(i) => vec![(i, 0)],
      Level::Max(ls) => ls,
    }
  }
}

pub fn used_levels(level: &Level) -> Vec<LevelId> {
  match level {
    Level::Id(i) => vec![*i],
    Level::Max(ls) => ls.iter().map(|(i, _)| *i).collect(),
  }
}

pub fn offset_level(level: Level, offset: LevelOffset) -> Level {
  if offset == 0 {
    return level;
  }
  match level {
    Level::Id(i) => Level::Max(vec![(i, offset)]),
    Level::Max(ls) => {
      let mut new_ls = Vec::new();
      for (i, o) in ls {
        new_ls.push((i, o + offset));
      }
      Level::Max(new_ls)
    }
  }
}

pub fn max_level(levels: Vec<Level>) -> Level {
  if levels.is_empty() {
    Level::Max(Vec::new())
  } else if levels.len() == 1 {
    levels.into_iter().next().unwrap()
  } else {
    let mut ls = Vec::new();
    for l in levels {
      match l {
        Level::Id(i) => ls.push((i, 0)),
        Level::Max(is) => {
          ls.extend(is);
        }
      }
    }
    if ls.len() == 1 && ls[0].1 == 0 {
      Level::Id(ls[0].0)
    } else {
      Level::Max(ls)
    }
  }
}

pub type GroupIndex = u32;

#[derive(Debug, Clone)]
pub enum LevelRef {
  Ext(LevelId),
  Group(GroupIndex),
}

pub type Constraints = Vec<(LevelId, LevelRel, LevelId)>;

#[derive(Debug, Clone)]
pub struct Solution {
  pub group_count: usize,
  pub replacer: Vec<(LevelId, Vec<(LevelRef, LevelOffset)>)>,
  pub externals: Vec<(LevelId, Vec<(LevelRef, LevelOffset)>)>,
}
