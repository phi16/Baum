use crate::types::common::LevelId;
use crate::types::level::*;
use crate::types::val::*;
use petgraph::{
  algo::bellman_ford::{bellman_ford, find_negative_cycle},
  prelude::NodeIndex,
  Graph,
};
use std::collections::{HashMap, HashSet};
use union_find::{QuickFindUf, UnionBySize, UnionFind};

type Result<T> = std::result::Result<T, String>;

pub fn traverse_levels<P, S>(e: &RE<P, S>) -> HashSet<LevelId> {
  fn rec<P, S>(e: &RE<P, S>, bounded: &HashSet<LevelId>, scope: &mut HashSet<LevelId>) {
    use CExprF::*;
    match &e.0 {
      Hole(_) => {}
      Bind(_) => {}
      Def(_, ls) => {
        ls.iter().for_each(|l| {
          if !bounded.contains(l) {
            scope.insert(*l);
          }
        });
      }
      Ann(e1, e2) => {
        rec(e1, bounded, scope);
        rec(e2, bounded, scope);
      }
      Uni(l) => {
        if !bounded.contains(l) {
          scope.insert(*l);
        }
      }
      Let(defs, body) => {
        for (_, sol, e) in defs {
          let mut b = bounded.clone();
          sol.scope.iter().for_each(|(l, _)| {
            b.insert(*l);
          });
          rec(e, &b, scope);
        }
        rec(body, bounded, scope);
      }

      Pi(_, _, _, ty, bty) => {
        rec(ty, bounded, scope);
        rec(bty, bounded, scope);
      }
      Lam(_, _, _, ty, body) => {
        rec(ty, bounded, scope);
        rec(body, bounded, scope);
      }
      App(_, _, e1, e2) => {
        rec(e1, bounded, scope);
        rec(e2, bounded, scope);
      }

      Sigma0(_) => (),
      Obj0(_) => (),
      Sigma(_, (_, _, ty0), props) => {
        rec(ty0, bounded, scope);
        for (_, _, e) in props {
          rec(e, bounded, scope);
        }
      }
      Obj(_, (_, e0), props) => {
        rec(e0, bounded, scope);
        for (_, e) in props {
          rec(e, bounded, scope);
        }
      }
      Prop(_, e, _) => {
        rec(e, bounded, scope);
      }
    }
  }
  let mut scope = HashSet::new();
  rec(e, &HashSet::new(), &mut scope);
  scope
}

enum Edge {
  Le,
  Lt,
}

pub fn solve_levels(constraints: &Constraints, scope: &HashSet<LevelId>) -> Result<Solution> {
  eprintln!("[Constraint Solver]");
  let mut level_map = HashMap::new();
  let mut uf = QuickFindUf::<UnionBySize>::new(0);
  for l in scope {
    level_map.insert(*l, uf.insert(Default::default()));
  }
  let mut constrs = Vec::new();
  let mut has_lt = false;
  for (i1, rel, i2, reason) in constraints {
    let g1 = level_map
      .entry(*i1)
      .or_insert(uf.insert(Default::default()))
      .clone();
    let g2 = level_map
      .entry(*i2)
      .or_insert(uf.insert(Default::default()))
      .clone();
    match rel {
      LevelRel::Eq => {
        uf.union(g1, g2);
      }
      LevelRel::Le => {
        constrs.push((g1, Edge::Le, g2, reason));
      }
      LevelRel::Lt => {
        constrs.push((g1, Edge::Lt, g2, reason));
        has_lt = true;
      }
    }
  }

  let mut groups = HashMap::new();
  for (l, i) in &level_map {
    let g = uf.find(*i);
    groups.entry(g).or_insert_with(Vec::new).push(l);
  }
  /* eprintln!("- groups:");
  for g in groups {
    eprintln!("  - #{:?} => {:?}", g.0, g.1);
  }
  eprintln!("- group constraints:");
  for (g1, rel, g2, reason) in &constrs {
    let g1 = uf.find(*g1);
    let g2 = uf.find(*g2);
    match rel {
      Edge::Le => eprintln!("  - #{:?} ≤ #{:?} ({})", g1, g2, reason),
      Edge::Lt => eprintln!("  - #{:?} < #{:?} ({})", g1, g2, reason),
    }
  } */

  let mut gs_count = 0;
  let mut gs = HashMap::new();
  for l in scope {
    let g = level_map.get(l).unwrap();
    let g = uf.find(*g);
    gs.entry(g).or_insert_with(|| {
      let idx = gs_count;
      gs_count += 1;
      idx
    });
  }

  let sol = Solution {
    groups: gs_count,
    scope: scope
      .iter()
      .map(|l| {
        let g = level_map.get(l).unwrap();
        let g = uf.find(*g);
        (*l, *gs.get(&g).unwrap())
      })
      .collect(),
    constraints: constraints
      .iter()
      .filter_map(|(l1, rel, l2, reason)| {
        let g1 = level_map.get(l1).unwrap();
        let g2 = level_map.get(l2).unwrap();
        let g1 = uf.find(*g1);
        let g2 = uf.find(*g2);
        if g1 == g2 {
          // Eq: ignore
          // Le: ignore
          // Lt: unreachable
          return None;
        }
        Some((
          if let Some(g) = gs.get(&g1) {
            LevelRef::Group(*g)
          } else {
            LevelRef::Id(*l1)
          },
          rel.clone(),
          if let Some(g) = gs.get(&g2) {
            LevelRef::Group(*g)
          } else {
            LevelRef::Id(*l2)
          },
          reason.clone(),
        ))
      })
      .collect(),
  };

  if !has_lt {
    return Ok(sol);
  }
  assert!(!constrs.is_empty());

  let mut edges = Vec::new();
  let mut verts = Vec::new();
  let mut vert_map = HashMap::new();
  let mut next_node_index: u32 = 0;
  for (g1, rel, g2, _) in &constrs {
    let n1 = *vert_map.entry(uf.find(*g1)).or_insert_with_key(|k| {
      let idx = next_node_index;
      verts.push(*k);
      next_node_index += 1;
      idx
    });
    let n2 = *vert_map.entry(uf.find(*g2)).or_insert_with_key(|k| {
      let idx = next_node_index;
      verts.push(*k);
      next_node_index += 1;
      idx
    });
    edges.push((
      n1,
      n2,
      match rel {
        Edge::Le => 0.,
        Edge::Lt => -1.,
      },
    ));
  }
  let root = next_node_index;
  for (_, n) in &vert_map {
    edges.push((root, *n, 0.));
  }

  let g = Graph::<(), f32>::from_edges(&edges);
  // eprintln!("- graph:");
  // eprintln!("{:?}", g);
  if let Some(cycle) = find_negative_cycle(&g, NodeIndex::new(root as usize)) {
    let cycle = cycle.iter().map(|i| verts[i.index()]).collect::<Vec<_>>();
    return Err(format!("Cycle detected: #{:?}", cycle));
  }
  /* if let Ok(dists) = bellman_ford(&g, NodeIndex::new(root as usize)) {
    eprintln!("- assigned levels:");
    let test_levels = vec![881, 918, 950, 1113, 948, 947, 916, 783];
    for l in test_levels {
      if let Some(g) = level_map.get(&LevelId(l)) {
        let g = uf.find(*g);
        if let Some(idx) = vert_map.get(&g) {
          let d = dists.distances.get(*idx as usize).unwrap();
          let ul = -*d as u32;
          eprintln!("  - 𝒰_{:?} = U{}", l, ul);
        } else {
          eprintln!("  - 𝒰_{:?} = (unconstrained)", l);
        }
      }
    }
  } else {
    return Err("Failed to compute distances".to_string());
  } */
  Ok(sol)
}
