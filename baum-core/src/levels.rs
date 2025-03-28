use crate::types::common::LevelId;
use std::collections::{HashMap, HashSet};
use union_find::{QuickFindUf, UnionBySize, UnionFind};

type Result<T> = std::result::Result<T, String>;

pub enum LevelRel {
  Eq,
  Le,
  Lt,
}

pub fn solve_levels(
  levels: &HashSet<LevelId>,
  constraints: &Vec<(LevelId, LevelRel, LevelId)>,
) -> Result<()> {
  let level_map: HashMap<LevelId, usize> =
    levels.iter().enumerate().map(|(i, &l)| (l, i)).collect();
  let mut uf = QuickFindUf::<UnionBySize>::new(levels.len());
  let mut constrs = Vec::new();
  for (i1, rel, i2) in constraints {
    let g1 = level_map.get(i1);
    let g2 = level_map.get(i2);
    let (g1, g2) = match (g1, g2) {
      (Some(g1), Some(g2)) => (g1, g2),
      _ => {
        let rel_str = match rel {
          LevelRel::Eq => "=",
          LevelRel::Le => "≤",
          LevelRel::Lt => "<",
        };
        eprintln!("Raw: {:?} {} {:?}", i1, rel_str, i2);
        continue;
      }
    };
    match rel {
      LevelRel::Eq => {
        uf.union(*g1, *g2);
      }
      LevelRel::Le => {
        constrs.push((*g1, LevelRel::Le, *g2));
      }
      LevelRel::Lt => {
        constrs.push((*g1, LevelRel::Lt, *g2));
      }
    }
  }
  eprintln!("- groups:");
  for i in levels {
    eprintln!("  - {:?} -> #{:?}", i, uf.find(level_map[i]));
  }
  eprintln!("- group constraints:");
  for (g1, rel, g2) in &constrs {
    let g1 = uf.find(*g1);
    let g2 = uf.find(*g2);
    match rel {
      LevelRel::Eq => eprintln!("  - #{:?} = #{:?}", g1, g2),
      LevelRel::Le => eprintln!("  - #{:?} ≤ #{:?}", g1, g2),
      LevelRel::Lt => eprintln!("  - #{:?} < #{:?}", g1, g2),
    }
  }
  Ok(())
}
