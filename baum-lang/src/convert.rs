use crate::types::syntax::{
  ElemId, ElemToken, LookupId, SyntaxExpr, SyntaxHandler, SyntaxInterpret,
};
use crate::types::tree::*;
use crate::types::tree_base::*;
use baum_front::types::tree as front;
use baum_front::types::tree::{ModDepth, ModLevel};
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, (TokenPos, String)>;

fn ext_name(mod_name: &Vec<Id>, i: &Id) -> String {
  let mut s = String::new();
  for name in mod_name {
    s.push_str(name.as_str());
    s.push_str(".");
  }
  s.push_str(i.as_str());
  s
}

type Resolver = Vec<Vec<front::Id>>;

#[derive(Debug, Clone)]
enum Entity<'a> {
  Def(front::Id),
  Mod(front::Id, Resolver, Env<'a>),
}

#[derive(Debug, Clone)]
struct Env<'a> {
  lookup: HashMap<Id<'a>, Entity<'a>>,
}

impl<'a> Env<'a> {
  fn new() -> Self {
    Self {
      lookup: HashMap::new(),
    }
  }
}

struct Builder<'a> {
  envs: Vec<(front::ModDepth, Env<'a>)>,
  cur_depth: front::ModDepth,
  cur_mod_name: Vec<front::Id>,
  next_id: u32,
  syntax: HashMap<SyntaxId, SyntaxHandler<'a>>,
  symbols: HashMap<front::Id, String>,
  errors: Vec<String>,
}

impl<'a> Builder<'a> {
  fn new(syntax: HashMap<SyntaxId, SyntaxHandler<'a>>) -> Self {
    Self {
      envs: vec![(0, Env::new())],
      cur_depth: 0,
      cur_mod_name: Vec::new(),
      next_id: 0,
      syntax,
      symbols: HashMap::new(),
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn here(&mut self) -> &mut HashMap<Id<'a>, Entity<'a>> {
    &mut self.envs.last_mut().unwrap().1.lookup
  }

  fn new_id(&mut self, id: &Id<'a>) -> front::Id {
    let i = front::Id(self.next_id);
    self.next_id += 1;
    self.symbols.insert(i, id.as_str().to_string());
    i
  }

  fn fresh_id(&mut self) -> front::Id {
    let i = front::Id(self.next_id);
    self.next_id += 1;
    i
  }

  fn lookup_mod(&self, n: &Vec<Id<'a>>) -> Option<(ModDepth, Vec<front::Id>, Resolver, Env<'a>)> {
    'k: for env in self.envs.iter().rev() {
      let mut cur_env = &env.1;
      let mut mod_name = Vec::new();
      let mut mod_res = Vec::new();
      for name in n {
        match cur_env.lookup.get(name) {
          Some(Entity::Mod(i, res, env)) => {
            mod_name.push(*i);
            mod_res = res.clone(); // a bit inefficient
            cur_env = env;
          }
          _ => continue 'k,
        }
      }
      return Some((env.0, mod_name, mod_res, cur_env.clone()));
    }
    None
  }

  fn lookup_id(&self, i: &Id<'a>, env: &Env<'a>) -> Option<front::Id> {
    match env.lookup.get(i) {
      Some(Entity::Def(i)) => Some(*i),
      _ => None,
    }
  }

  fn level_from(&self, depth: ModDepth) -> ModLevel {
    if self.cur_depth < depth {
      0
    } else {
      (self.cur_depth - depth) as ModLevel
    }
  }

  fn to_name(&self, mod_ids: &Vec<front::Id>) -> String {
    let mut s = String::new();
    for i in mod_ids {
      s.push_str(self.symbols.get(i).unwrap_or(&"".to_string()));
      s.push_str("#");
      s.push_str(&i.0.to_string());
      s.push_str(".");
    }
    s
  }

  fn e_internal(&mut self, e: &Expr<'a>) -> Result<front::Expr> {
    match &e.0 {
      ExprF::Hole => Ok(front::Expr(front::ExprF::Hole)),
      ExprF::Var(i) => {
        for env in self.envs.iter().rev() {
          if let Some(Entity::Def(i)) = env.1.lookup.get(i) {
            return Ok(front::Expr(front::ExprF::Var(self.level_from(env.0), *i)));
          }
        }
        Err((e.1.begin, format!("symbol not found: {}", i.as_str())))
      }
      ExprF::Mod(_) => Err((
        e.1.begin,
        "module reference is not allowed in expression".to_string(),
      )),
      ExprF::Ext(mod_name, i) => {
        let (depth, m, res, env) = self.lookup_mod(mod_name).ok_or((
          e.1.begin,
          format!("module not found: {}", ext_name(mod_name, i)),
        ))?;
        let i = self.lookup_id(i, &env).ok_or((
          e.1.begin,
          format!("symbol not found: {}", ext_name(mod_name, i)),
        ))?;
        Ok(front::Expr(front::ExprF::Ext(
          self.level_from(depth),
          m,
          i.clone(),
        )))
      }
      ExprF::Let(ds, e) => {
        self.envs.push((self.cur_depth, Env::new()));
        let mut decls = Vec::new();
        self.ds(ds, &mut Env::new(), &mut decls);
        let e = Rc::new(self.e(e));
        self.envs.pop();
        Ok(front::Expr(front::ExprF::Let(decls, e)))
      }
      ExprF::Syntax(mod_name, sid, elems) => {
        let handler = self
          .syntax
          .get(sid)
          .ok_or(format!("unimplemented syntax handler: {:?}", sid))
          .unwrap()
          .clone();

        let resolver = if mod_name.is_empty() {
          None // hmmmmmmmmmmm
        } else {
          let (md, mn, mres, _) = self
            .lookup_mod(mod_name)
            .ok_or(format!("module not found: {:?}", mod_name))
            .unwrap(); // is this correct for empty mod...?

          if match sid {
            SyntaxId::User(_) => true,
            _ => false,
          } {
            eprintln!(
              "at {:?}, Syntax {:?} called with {:?} ({:?}) / {:?}",
              e.1.begin.to_string(),
              sid,
              self.to_name(&mn),
              md,
              mres.iter().map(|m| self.to_name(m)).collect::<Vec<_>>()
            );
            eprintln!(
              "Current mod: {:?} ({:?})",
              self.to_name(&self.cur_mod_name),
              self.cur_depth
            );
            let here = &self.cur_mod_name;
            let mut res = Vec::new();
            for r in &mres {
              // remove common prefix with here, from r
              let mut rr = Vec::new();
              let mut common = true;
              for (index, m) in r.iter().enumerate() {
                if common {
                  if index < here.len() && m == &here[index] {
                    continue;
                  }
                }
                common = false;
                rr.push(m.clone());
              }
              res.push(rr);
            }

            eprintln!(
              "Res: {:?}",
              res
                .iter()
                .rev()
                .map(|m| self.to_name(m))
                .enumerate()
                .collect::<Vec<_>>()
            );
            eprintln!();
            Some(res)
          } else {
            None
          }
        };
        let (tokens, e, deps) = handler(elems).map_err(|err| (e.1.begin, err))?.into_inner();

        let mut i_map = HashMap::new();
        for (elem, t) in elems.iter().zip(tokens.iter()) {
          match (elem, t) {
            (SyntaxElem::Ident(i), ElemToken::Ident(eid)) => {
              let id = self.new_id(i);
              i_map.insert(eid.clone(), (i.clone(), id));
            }
            _ => {}
          }
        }

        let mut e_map = HashMap::new();
        for (elem, t) in elems.iter().zip(tokens.into_iter()) {
          match (elem, t) {
            (SyntaxElem::Expr(e), ElemToken::Expr(eid)) => {
              self.envs.push((self.cur_depth, Env::new()));
              // add dependencies
              for i_eid in deps.get(&eid).unwrap() {
                let (i, id) = i_map.get(i_eid).unwrap();
                self.here().insert(i.clone(), Entity::Def(id.clone()));
              }
              let e = self.e(e);
              self.envs.pop();
              e_map.insert(eid, e);
            }
            _ => {}
          }
        }

        struct E<'a> {
          i_map: HashMap<ElemId, (Id<'a>, front::Id)>,
          e_map: HashMap<ElemId, front::Expr>,
          resolver: Option<Resolver>,
        }
        let env = E {
          i_map,
          e_map,
          resolver,
        };

        fn replace_id(id: &LookupId, env: &E) -> front::Id {
          match id {
            LookupId::InSyntax(i) => env.i_map.get(i).unwrap().1,
            LookupId::General(i) => *i,
          }
        }

        fn resolve_ext(
          l: &ModLevel,
          mod_name: &Vec<LookupId>,
          i: &front::Id,
          env: &E,
        ) -> front::Expr {
          let mod_name = mod_name
            .iter()
            .map(|m| {
              if let LookupId::General(m) = m {
                *m
              } else {
                unreachable!()
              }
            })
            .collect::<Vec<_>>();
          let mod_name = match &env.resolver {
            Some(res) => {
              let l = *l as usize;
              assert!(l < res.len());
              let mut m = res[res.len() - 1 - l].clone();
              m.extend(mod_name);
              m
            }
            None => mod_name,
          };
          // TODO: Level ?
          front::Expr(if mod_name.is_empty() {
            front::ExprF::Var(-1, *i)
          } else {
            front::ExprF::Ext(-1, mod_name, *i)
          })
        }

        fn replace(e: &SyntaxExpr, env: &E) -> front::Expr {
          use front::ExprF::*;
          front::Expr(match &e.0 {
            Hole => Hole,
            Var(l, LookupId::General(i)) => resolve_ext(l, &Vec::new(), i, env).0,
            Var(_, LookupId::InSyntax(i)) => env.e_map.get(i).unwrap().0.clone(), // expr
            Ann(e1, e2) => Ann(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),
            Uni => Uni,
            Ext(l, mod_name, LookupId::General(i)) => resolve_ext(l, mod_name, i, env).0,
            Ext(_, _, LookupId::InSyntax(_)) => unreachable!(),
            Let(_, _) => unreachable!(),
            Lit(lit) => Lit(lit.clone()),

            PiE(i, e1, e2) => PiE(
              i.clone().map(|i| replace_id(&i, env)),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            LamE(i, e1, e2) => LamE(
              replace_id(i, env),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            AppE(e1, e2) => AppE(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),
            PiI(i, e1, e2) => PiI(
              i.clone().map(|i| replace_id(&i, env)),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            LamI(i, e1, e2) => LamI(
              replace_id(i, env),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            AppI(e1, e2) => AppI(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),

            TupleTy(tys) => TupleTy(
              tys
                .into_iter()
                .map(|(i, e)| {
                  (
                    i.clone().map(|i| replace_id(&i, env)),
                    Rc::new(replace(&e, env)),
                  )
                })
                .collect(),
            ),
            TupleCon(es) => TupleCon(es.into_iter().map(|e| Rc::new(replace(&e, env))).collect()),
            Proj(i, e) => Proj(*i, Rc::new(replace(&e, env))),

            ObjTy(es) => ObjTy(
              es.into_iter()
                .map(|(i, e)| (replace_id(i, env), Rc::new(replace(&e, env))))
                .collect(),
            ),
            ObjCon(es) => ObjCon(
              es.into_iter()
                .map(|(i, e)| (replace_id(i, env), Rc::new(replace(&e, env))))
                .collect(),
            ),
            Prop(i, e) => Prop(replace_id(i, env), Rc::new(replace(&e, env))),
          })
        }
        let e = replace(&e, &env);
        Ok(e)
      }
    }
  }

  fn e(&mut self, e: &Expr<'a>) -> front::Expr {
    match self.e_internal(e) {
      Ok(e) => e,
      Err((pos, msg)) => {
        self.add_error(pos, &msg);
        front::Expr(front::ExprF::Hole)
      }
    }
  }

  fn define_mod(
    &mut self,
    mod_id: &front::Id,
    m_params: &Vec<(Vis, Vec<Id<'a>>, Option<Box<Expr<'a>>>)>,
    m_body: &ModBody<'a>,
  ) -> (
    Vec<(Vis, front::Id, Rc<front::Expr>)>,
    front::ModBody,
    Resolver,
    Env<'a>,
  ) {
    self.envs.push((self.cur_depth, Env::new()));
    let mut param_names = Vec::new();
    let mut params = Vec::new();
    for (vis, names, ty) in m_params {
      let ty = if let Some(ty) = ty {
        self.e(&ty)
      } else {
        front::Expr(front::ExprF::Hole)
      };
      let ty = Rc::new(ty);
      for name in names {
        let i = self.new_id(name);
        self.here().insert(name.clone(), Entity::Def(i));
        param_names.push(name);
        params.push((vis.clone(), i, ty.clone()));
      }
    }
    let mut mod_env = Env::new();
    let mut mod_decls = Vec::new();
    let (mod_body, mod_res) = match m_body {
      ModBodyF::Decls(ds) => {
        // overwrite the module parameter name references to actual definitions
        for ((_, i, _), name) in params.iter().zip(param_names.into_iter()) {
          let j = self.new_id(name);
          let def_i = front::Expr(front::ExprF::Var(0, i.clone()));
          mod_decls.push(front::Decl::Def(j, def_i));
          self.here().insert(name.clone(), Entity::Def(j));
        }
        self.cur_depth += 1;
        self.cur_mod_name.push(mod_id.clone());
        self.envs.push((self.cur_depth, Env::new()));
        self.ds(ds, &mut mod_env, &mut mod_decls);
        self.envs.pop();
        self.cur_mod_name.pop();
        self.cur_depth -= 1;

        let res = {
          let mut res = Vec::new();
          let mut m = Vec::new();
          res.push(m.clone());
          for mi in &self.cur_mod_name {
            m.push(mi.clone());
            res.push(m.clone());
          }
          m.push(mod_id.clone());
          res.push(m);
          res
        };
        (front::ModBody::Decls(mod_decls), res)
      }
      ModBodyF::Ref(ModRefF::App(m, m_args)) => {
        let (depth, m, res, env) = self.lookup_mod(&m).unwrap();
        let mut args = Vec::new();
        for (vis, e) in m_args {
          args.push((vis.clone(), self.e(e)));
        }

        // Rewriting resolvers: replace occurrence of m with relative mod_name
        struct Replacer<'c> {
          from: &'c Vec<front::Id>,
          to: &'c Vec<front::Id>,
        }

        fn replace_res(res: &Resolver, repl: &Replacer) -> Resolver {
          enum ModMatch {
            Matched(Vec<front::Id>, Vec<front::Id>),
            Unmatched(Vec<front::Id>),
          }
          fn last_match(r: &Vec<front::Id>, cut: &Vec<front::Id>) -> ModMatch {
            let mut c = Vec::new();
            for (i, m) in r.iter().enumerate() {
              if m == &cut[0] && r[i..].starts_with(cut) {
                return ModMatch::Matched(c, r[i + cut.len()..].to_vec());
              }
              c.push(m.clone());
            }
            ModMatch::Unmatched(c)
          }

          let mut ret_res = Vec::new();
          for r in res {
            match last_match(&r, &repl.from) {
              ModMatch::Matched(mut rr, tail) => {
                rr.extend(repl.to);
                rr.extend(tail);
                ret_res.push(rr);
              }
              ModMatch::Unmatched(r) => {
                ret_res.push(r);
              }
            }
          }
          ret_res
        }

        fn replace_res_for_env<'a>(env: &Env<'a>, repl: &Replacer) -> Env<'a> {
          let lookup = env
            .lookup
            .iter()
            .map(|(name, entity)| {
              let e = match entity {
                Entity::Mod(i, res, env) => Entity::Mod(
                  i.clone(),
                  replace_res(res, repl),
                  replace_res_for_env(env, repl),
                ),
                e => e.clone(),
              };
              (name.clone(), e)
            })
            .collect();
          Env { lookup }
        }

        let mut repl_to = self.cur_mod_name[depth as usize..].to_vec();
        repl_to.push(mod_id.clone());
        let repl = Replacer {
          from: &m,
          to: &repl_to,
        };
        mod_env = replace_res_for_env(&env, &repl);
        let mod_res = replace_res(&res, &repl);

        (
          front::ModBody::App(self.level_from(depth), m, args),
          mod_res,
        )
      }
      ModBodyF::Ref(ModRefF::Import(_)) => unimplemented!(),
    };
    self.envs.pop();
    (params, mod_body, mod_res, mod_env)
  }

  fn d(&mut self, d: &Decl<'a>, cur_mod: &mut Env<'a>, decls: &mut Vec<front::Decl>) {
    match &d.0 {
      DeclF::Local(ds) => {
        let mut empty_env = Env::new();
        self.ds(ds, &mut empty_env, decls);
      }
      DeclF::Mod(md, mb) => {
        let mod_id = self.new_id(&md.name);
        let (params, body, res, env) = self.define_mod(&mod_id, &md.params, mb);
        decls.push(front::Decl::Mod(mod_id, params, body));
        self.here().insert(
          md.name.clone(),
          Entity::Mod(mod_id, res.clone(), env.clone()),
        ); // uh
        cur_mod
          .lookup
          .insert(md.name.clone(), Entity::Mod(mod_id, res, env));
      }
      DeclF::Open(mr) => {
        // define a dummy module to consume m_args
        let mb = ModBody::Ref(mr.clone());
        let mod_id = self.fresh_id();
        let (params, body, _, env) = self.define_mod(&mod_id, &Vec::new(), &mb);
        decls.push(front::Decl::Mod(mod_id, params, body));

        // inserts all entities from the dummy module to current module
        env.lookup.into_iter().for_each(|(name, entity)| {
          decls.push(match entity {
            Entity::Def(i) => {
              front::Decl::Def(i, front::Expr(front::ExprF::Ext(0, vec![mod_id], i)))
            }
            Entity::Mod(i, _, _) => {
              let mn = vec![mod_id, i];
              front::Decl::Mod(i, Vec::new(), front::ModBody::App(0, mn, Vec::new()))
            }
          });
          self.here().insert(name, entity);
        });
      }
      DeclF::Use(mr) => {
        let d = Decl(
          DeclF::Local(vec![Decl(DeclF::Open(mr.clone()), d.1.clone())]),
          d.1.clone(),
        );
        self.d(&d, cur_mod, decls);
      }
      DeclF::Def(def) => {
        self.envs.push((self.cur_depth, Env::new()));
        let mut conts = Vec::new();
        for (vis, names, ty) in &def.args {
          let ty = if let Some(ty) = ty {
            self.e(&ty)
          } else {
            front::Expr(front::ExprF::Hole)
          };
          let ty = Rc::new(ty);
          for name in names {
            let i = self.new_id(name);
            self.here().insert(name.clone(), Entity::Def(i));
            conts.push((vis.clone(), i, ty.clone()));
          }
        }
        let e = self.e(&def.body);
        let e = if let Some(ty) = &def.ty {
          let ty = self.e(&ty);
          front::Expr(front::ExprF::Ann(Rc::new(e), Rc::new(ty)))
        } else {
          e
        };
        let mut e = e;
        for (vis, i, ty) in conts.into_iter().rev() {
          match vis {
            Vis::Explicit => e = front::Expr(front::ExprF::LamE(i, ty, Rc::new(e))),
            Vis::Implicit => e = front::Expr(front::ExprF::LamI(i, ty, Rc::new(e))),
          }
        }
        self.envs.pop();
        let i = self.new_id(&def.name);
        decls.push(front::Decl::Def(i, e));
        self.here().insert(def.name.clone(), Entity::Def(i));
        cur_mod.lookup.insert(def.name.clone(), Entity::Def(i));
      }
      DeclF::Syntax(sid, _, syndefs, e) => {
        let mut next_elem_id = 0;
        let mut i_env = HashMap::new();
        let mut tokens = Vec::new();
        for s in syndefs {
          match s {
            SynDef::Token(_) => tokens.push(ElemToken::Token),
            SynDef::Ident(i) => {
              let eid = ElemId(next_elem_id);
              next_elem_id += 1;
              tokens.push(ElemToken::Ident(eid));
              i_env.insert(i.clone(), eid.clone());
            }
            SynDef::Expr(_) => {
              let eid = ElemId(next_elem_id);
              next_elem_id += 1;
              tokens.push(ElemToken::Expr(eid));
            }
          }
        }
        self.envs.push((self.cur_depth, Env::new()));
        let mut exprs = HashMap::new();
        for (s, t) in syndefs.iter().zip(tokens.iter()) {
          match s {
            SynDef::Token(_) => {}
            SynDef::Ident(_) => {}
            SynDef::Expr(i) => {
              let id = self.new_id(i);
              self.here().insert(i.clone(), Entity::Def(id));
              let eid = match t {
                ElemToken::Expr(eid) => eid.clone(),
                _ => panic!(),
              };
              exprs.insert(id, eid);
            }
          }
        }
        let id_since = self.next_id;
        let e = match self.e_internal(e) {
          Ok(e) => e,
          Err((pos, msg)) => {
            self.add_error(pos, &msg);
            return;
          }
        };
        self.envs.pop();

        struct E<'a> {
          id_since: u32,
          symbols: HashMap<front::Id, String>,
          i_env: HashMap<Id<'a>, ElemId>,
          exprs: HashMap<front::Id, ElemId>,
        }
        let symbols = std::mem::take(&mut self.symbols);
        let env = E {
          id_since,
          symbols,
          i_env,
          exprs,
        };

        fn replace_id(id: &front::Id, env: &E) -> LookupId {
          fn replace_id_internal(id: &front::Id, env: &E) -> Option<ElemId> {
            if id.0 < env.id_since {
              return None;
            }
            let name = env.symbols.get(id).unwrap();
            let eid = env.i_env.get(&Id::new(name))?;
            Some(eid.clone())
          }
          match replace_id_internal(id, env) {
            Some(eid) => LookupId::InSyntax(eid),
            None => LookupId::General(id.clone()),
          }
        }

        fn replace(e: &front::Expr, env: &E) -> SyntaxExpr {
          use front::ExprF::*;
          SyntaxExpr(match &e.0 {
            Hole => Hole,
            Var(l, i) => match env.exprs.get(i) {
              Some(eid) => Var(0, LookupId::InSyntax(*eid)),
              None => Var(l.clone(), LookupId::General(*i)),
            },
            Ann(e1, e2) => Ann(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),
            Uni => Uni,

            Ext(l, mod_name, i) => Ext(
              l.clone(),
              mod_name.iter().map(|i| LookupId::General(*i)).collect(),
              LookupId::General(*i),
            ),
            Let(_, _) => unreachable!(),
            Lit(lit) => Lit(lit.clone()),

            PiE(i, e1, e2) => PiE(
              i.clone().map(|i| replace_id(&i, env)),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            LamE(i, e1, e2) => LamE(
              replace_id(i, env),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            AppE(e1, e2) => AppE(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),

            PiI(i, e1, e2) => PiI(
              i.clone().map(|i| replace_id(&i, env)),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            LamI(i, e1, e2) => LamI(
              replace_id(i, env),
              Rc::new(replace(&e1, env)),
              Rc::new(replace(&e2, env)),
            ),
            AppI(e1, e2) => AppI(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),

            TupleTy(tys) => TupleTy(
              tys
                .into_iter()
                .map(|(i, e)| (i.map(|i| replace_id(&i, env)), Rc::new(replace(&e, env))))
                .collect(),
            ),
            TupleCon(es) => TupleCon(es.into_iter().map(|e| Rc::new(replace(&e, env))).collect()),
            Proj(i, e) => Proj(*i, Rc::new(replace(&e, env))),

            ObjTy(es) => ObjTy(
              es.into_iter()
                .map(|(i, e)| (replace_id(i, env), Rc::new(replace(&e, env))))
                .collect(),
            ),
            ObjCon(es) => ObjCon(
              es.into_iter()
                .map(|(i, e)| (replace_id(i, env), Rc::new(replace(&e, env))))
                .collect(),
            ),
            Prop(i, e) => Prop(replace_id(i, env), Rc::new(replace(&e, env))),
          })
        }

        let e = replace(&e, &env);
        self.symbols = env.symbols;

        let interpret = SyntaxInterpret::new(tokens, e);
        let handler: SyntaxHandler<'a> = Rc::new(move |_| Ok(interpret.clone()));
        self.syntax.insert(sid.clone(), handler);
      }
    }
  }

  fn ds(&mut self, ds: &Vec<Decl<'a>>, cur_mod: &mut Env<'a>, decls: &mut Vec<front::Decl>) {
    for d in ds {
      self.d(d, cur_mod, decls);
    }
  }
}

pub fn convert<'a>(
  ds: &Vec<Decl<'a>>,
  syntax_handlers: HashMap<SyntaxId, SyntaxHandler<'a>>,
) -> std::result::Result<front::Program, Vec<String>> {
  let mut b = Builder::new(syntax_handlers);
  let mut cur_mod = Env::new();
  let mut decls = Vec::new();
  b.ds(ds, &mut cur_mod, &mut decls);
  if b.errors.is_empty() {
    let symbols = b.symbols;
    Ok(front::Program { decls, symbols })
  } else {
    Err(b.errors)
  }
}
