use crate::types::tree::{Id, Name, Tree, TreeF};
use baum_core::types::common::{BindId, DefId};
use baum_core::types::val::{CExprF, RE};
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

struct Convert {
  next_id: u32,
  bind_map: HashMap<BindId, Id>,
  def_map: HashMap<DefId, Id>,
  used_ids: HashSet<Id>,
}

impl Convert {
  fn new() -> Convert {
    Convert {
      next_id: 0,
      bind_map: HashMap::new(),
      def_map: HashMap::new(),
      used_ids: HashSet::new(),
    }
  }

  fn bind(&mut self, i: BindId) -> Id {
    if let Some(id) = self.bind_map.get(&i) {
      self.used_ids.insert(*id);
      return *id;
    }
    let id = Id(self.next_id);
    self.next_id += 1;
    self.bind_map.insert(i, id);
    self.used_ids.insert(id);
    id
  }

  fn def(&mut self, i: DefId) -> Id {
    if let Some(id) = self.def_map.get(&i) {
      self.used_ids.insert(*id);
      return *id;
    }
    let id = Id(self.next_id);
    self.next_id += 1;
    self.def_map.insert(i, id);
    self.used_ids.insert(id);
    id
  }

  fn e<P, S>(&mut self, e: &RE<P, S>) -> Rc<Tree> {
    use CExprF::*;
    let t = match &e.0 {
      Hole(_) => unreachable!(),
      Bind(b) => TreeF::Var(self.bind(*b)),
      Def(d, _) => TreeF::Var(self.def(*d)),
      Ann(e, _) => return self.e(e),
      Uni(_) => TreeF::Unit,
      Prim(s) => TreeF::Prim(s.clone()),
      Lit(l) => TreeF::Lit(l.clone()),
      Let(defs, e) => {
        let outer_scope = std::mem::take(&mut self.used_ids);
        let mut ds = Vec::new();
        for (d, _, e) in defs {
          let e = self.e(e);
          let d = self.def(*d);
          ds.push((d, e));
        }
        let e = self.e(e);
        for (d, _) in &ds {
          self.used_ids.remove(d);
        }
        self.used_ids.extend(outer_scope);
        TreeF::Let(ds, e)
      }
      Pi(_, _, _, _, _) => TreeF::Unit,
      Lam(_, _, b, _, e) => {
        let outer_scope = std::mem::take(&mut self.used_ids);
        let e = self.e(e);
        let b = self.bind(*b);
        self.used_ids.remove(&b);
        let scope = self.used_ids.iter().cloned().collect::<Vec<_>>();
        self.used_ids.extend(outer_scope);
        TreeF::Lam(b, scope, e)
      }
      App(_, _, e1, e2) => TreeF::App(self.e(e1), self.e(e2)),

      Sigma0(_) => TreeF::Unit,
      Obj0(_) => TreeF::Unit,
      Sigma(_, _, _) => TreeF::Unit,
      Obj(_, (n0, e0), props) => {
        let mut ps = vec![(Name(n0.0), self.e(e0))];
        for (n, e) in props {
          ps.push((Name(n.0), self.e(e)));
        }
        TreeF::Obj(ps)
      }
      Prop(_, e, n) => TreeF::Prop(self.e(e), Name(n.0)),
    };
    Rc::new(Tree(t))
  }
}

pub fn convert<P, S>(e: &RE<P, S>) -> Rc<Tree> {
  let mut c = Convert::new();
  c.e(e)
}
