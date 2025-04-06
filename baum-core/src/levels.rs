use crate::types::common::LevelId;
use crate::types::level::*;
use crate::types::val::*;
use petgraph::{algo::bellman_ford::find_negative_cycle, prelude::NodeIndex, Graph};
use std::collections::{HashMap, HashSet};
use union_find::{QuickFindUf, UnionBySize, UnionFind};

type Result<T> = std::result::Result<T, String>;

pub fn traverse_levels_e<P, S>(e: &RE<P, S>, scope: &mut HashSet<LevelId>) {
  fn rec<P, S>(e: &RE<P, S>, bounded: &HashSet<LevelId>, scope: &mut HashSet<LevelId>) {
    use CExprF::*;
    match &e.0 {
      Hole(_) => {}
      Bind(_) => {}
      Def(_, ls) => {
        ls.iter().flat_map(|l| used_levels(l)).for_each(|i| {
          if !bounded.contains(&i) {
            scope.insert(i);
          }
        });
      }
      Ann(e1, e2) => {
        rec(e1, bounded, scope);
        rec(e2, bounded, scope);
      }
      Uni(l) => {
        used_levels(l).iter().for_each(|i| {
          if !bounded.contains(i) {
            scope.insert(*i);
          }
        });
      }
      Let(defs, body) => {
        for (_, sol, e) in defs {
          let mut b = bounded.clone();
          sol.replacer.iter().for_each(|(l, _)| {
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
  rec(e, &HashSet::new(), scope);
}

pub fn traverse_levels_g<P, S>(g: &Env<P, S>, scope: &mut HashSet<LevelId>) {
  let mut s = HashSet::new();
  for (_, v) in &g.lookup {
    traverse_levels_v(v, &mut s);
  }
  for (_, v) in &g.define {
    traverse_levels_v(v, &mut s);
  }
  if !s.is_empty() {
    eprintln!("[Warning] Found levels in the environment: {:?}", s);
  }
  scope.extend(s.iter().cloned());
}

pub fn traverse_levels_v<P, S>(v: &RV<P, S>, scope: &mut HashSet<LevelId>) {
  fn rec<P, S>(v: &RV<P, S>, scope: &mut HashSet<LevelId>) {
    use ValF::*;
    match &v.0 {
      Hole(_) => {}
      Neu(_, cs) => {
        for c in cs {
          match c {
            ContF::App(_, _, e) => rec(e, scope),
            ContF::Prop(_, _) => {}
          }
        }
      }
      Lazy(_, ls, cs) => {
        ls.iter().flat_map(|l| used_levels(l)).for_each(|i| {
          scope.insert(i);
        });
        for c in cs {
          match c {
            ContF::App(_, _, e) => rec(e, scope),
            ContF::Prop(_, _) => {}
          }
        }
      }
      Uni(l) => {
        used_levels(l).iter().for_each(|i| {
          scope.insert(*i);
        });
      }

      Pi(_, _, _, ty, g, bty) => {
        rec(ty, scope);
        traverse_levels_g(g, scope);
        traverse_levels_e(bty, scope);
      }
      Lam(_, _, _, ty, g, body) => {
        rec(ty, scope);
        traverse_levels_g(g, scope);
        traverse_levels_e(body, scope);
      }

      Sigma0(_) => {}
      Obj0(_) => {}
      Sigma(_, (_, _, ty0), g, props) => {
        rec(ty0, scope);
        traverse_levels_g(g, scope);
        for (_, _, ty) in props {
          traverse_levels_e(ty, scope);
        }
      }
      Obj(_, (_, ty0), props) => {
        rec(ty0, scope);
        for (_, ty) in props {
          rec(ty, scope);
        }
      }
    }
  }
  rec(v, scope);
}

type Edge = LevelOffset;

pub fn resolve_constraints(
  levels: &HashSet<LevelId>,
  constraints: &Constraints,
  target: &HashSet<LevelId>,
) -> Result<Solution> {
  /* eprintln!("[Constraint Solver]");
  eprintln!("- target: {:?}", target);
  eprintln!("- constraints:");
  for (i1, rel, i2, reason) in constraints {
    match rel {
      LevelRel::Eq => eprintln!("  - {:?} = {:?} ({})", i1, i2, reason),
      LevelRel::Le(0) => eprintln!("  - {:?} ≤ {:?} ({})", i1, i2, reason),
      LevelRel::Le(o) => eprintln!("  - {:?}+{:?} ≤ {:?} ({})", i1, o, i2, reason),
    }
  } */
  let (group_count, level_map) = {
    let mut uf = QuickFindUf::<UnionBySize>::new(0);
    let mut level_map = HashMap::new();
    for t in target {
      level_map.insert(*t, uf.insert(Default::default()));
    }
    for (l1, rel, l2, _) in constraints {
      let g1 = level_map
        .entry(*l1)
        .or_insert(uf.insert(Default::default()))
        .clone();
      let g2 = level_map
        .entry(*l2)
        .or_insert(uf.insert(Default::default()))
        .clone();
      match rel {
        LevelRel::Eq => {
          uf.union(g1, g2);
        }
        _ => {}
      }
    }

    let mut group_map = HashMap::new();
    let mut count = 0;
    for (_, g) in level_map.iter_mut() {
      let n = uf.find(*g);
      *g = *group_map.entry(n).or_insert_with(|| {
        let index = count;
        count += 1;
        index
      });
    }
    (count, level_map)
  };

  let mut constrs = HashMap::new();
  for (l1, rel, l2, reason) in constraints {
    let g1 = level_map[l1];
    let g2 = level_map[l2];
    match rel {
      LevelRel::Eq => {}
      LevelRel::Le(o) => {
        constrs
          .entry((g1, g2))
          .and_modify(|(e, r): &mut (Edge, String)| {
            if *e < *o {
              *e = *o;
              *r = reason.clone();
            } else if *e == *o {
              r.push_str(&format!(", {}", reason));
            }
          })
          .or_insert_with(|| (*o, reason.clone()));
      }
    }
  }

  // Cycle detection

  if !constrs.is_empty() {
    /* {
      let mut groups = HashMap::new();
      for (l, g) in &level_map {
        groups.entry(g).or_insert_with(Vec::new).push(l);
      }
      eprintln!("- groups:");
      for g in groups {
        eprintln!("  - #{:?} => {:?}", g.0, g.1);
      }
      eprintln!("- group constraints:");
      for ((g1, g2), (rel, reason)) in &constrs {
        match rel {
          Edge::Le => eprintln!("  - #{:?} ≤ #{:?} ({})", g1, g2, reason),
          Edge::Lt => eprintln!("  - #{:?} < #{:?} ({})", g1, g2, reason),
        }
      }
    } */

    let mut edges = Vec::new();
    for ((g1, g2), (e, _)) in &constrs {
      edges.push((*g1 as u32, *g2 as u32, -(*e as f32)));
    }
    let root = group_count as u32;
    for n in 0..group_count {
      edges.push((root, n as u32, 0.));
    }

    let g = Graph::<(), f32>::from_edges(&edges);
    if let Some(cycle) = find_negative_cycle(&g, NodeIndex::new(root as usize)) {
      return Err(format!("Cycle detected: #{:?}", cycle));
    }
  }

  let mut ascending = vec![HashMap::new(); group_count];
  let mut descending = vec![HashMap::new(); group_count];
  for ((g1, g2), c) in &constrs {
    ascending[*g1].insert(*g2, c);
    descending[*g2].insert(*g1, c);
  }

  // Accessible groups

  let mut accessible = HashSet::new();
  fn access(
    g: usize,
    ascending: &Vec<HashMap<usize, &(Edge, String)>>,
    accessible: &mut HashSet<usize>,
  ) {
    if accessible.contains(&g) {
      return;
    }
    accessible.insert(g);
    for (g2, _) in &ascending[g] {
      access(*g2, ascending, accessible);
    }
  }
  for t in target {
    if let Some(g) = level_map.get(t) {
      access(*g, &ascending, &mut accessible);
    }
  }

  // Minimize constraints

  fn descend(
    g: usize,
    descending: &Vec<HashMap<usize, &(Edge, String)>>,
    accessible: &HashSet<usize>,
    next_root_index: &mut u32,
    processed: &mut HashMap<usize, Vec<(LevelRef, Edge)>>,
  ) {
    if processed.contains_key(&g) {
      return;
    }
    let mut constrs = Vec::new();
    for (g1, (e, _)) in &descending[g] {
      if accessible.contains(g1) {
        descend(*g1, descending, accessible, next_root_index, processed);
        let mut cs = processed.get(g1).unwrap().clone();
        for (_, o) in cs.iter_mut() {
          *o += *e;
        }
        constrs.extend(cs);
      }
    }
    if constrs.is_empty() {
      let index = *next_root_index;
      *next_root_index += 1;
      constrs.push((LevelRef::Group(index), 0));
    }
    processed.insert(g, constrs);
  }
  let mut next_root_index = 0;
  let mut processed = HashMap::new();
  let mut replacer = Vec::new();
  for t in target {
    let g = level_map.get(t).unwrap();
    descend(
      *g,
      &descending,
      &accessible,
      &mut next_root_index,
      &mut processed,
    );
    replacer.push((*t, processed.get(g).unwrap().clone()));
  }

  let sol = Solution {
    group_count: next_root_index as usize,
    replacer,
    externals: vec![],
  };
  let external_levels = {
    let mut ls = HashSet::new();
    for (l1, _, l2, _) in constraints {
      if !levels.contains(l1) {
        ls.insert(*l1);
      }
      if !levels.contains(l2) {
        ls.insert(*l2);
      }
    }
    ls
  };
  if !external_levels.is_empty() {
    eprintln!("constraints: {:?}", constraints);
    eprintln!("levels: {:?}", levels);
    eprintln!(
      "[Constraint Solver] Found {} external levels: {:?}",
      external_levels.len(),
      external_levels
    );
  }
  let mut sol = sol;
  for l in external_levels {
    let g = level_map.get(&l).unwrap();
    if let Some(ls) = processed.get(g) {
      sol.externals.push((l, ls.clone()));
    }
  }
  Ok(sol)
}
