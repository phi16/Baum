use crate::builtin::builtin_syntax_handlers;
use crate::types::syntax::{
  ElemId, ElemToken, LookupId, SyntaxExpr, SyntaxHandler, SyntaxInterpret,
};
use crate::types::token::{ErrorPos, TokenIx, TokenRange};
use crate::types::tree::*;
use crate::types::tree_base::*;
use baum_front::types::tree as front;
use baum_front::types::tree::{ModDepth, ModLevel};
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, (ErrorPos, String)>;
type Tag = TokenRange;
type FExpr = front::Expr<Tag>;
type FExprInternal = front::ExprInternal<Tag>;
type FModBody = front::ModBody<Tag>;
type FDecl = front::Decl<Tag>;

fn range_at(loc: &TokenIx) -> TokenRange {
  TokenRange {
    begin: loc.clone(),
    end: loc.clone(),
  }
}

fn range_merge(r1: &TokenRange, r2: &TokenRange) -> TokenRange {
  TokenRange {
    begin: r1.begin.clone(),
    end: r2.end.clone(),
  }
}

fn ext_name(mod_name: &Vec<Id>, i: &Id) -> String {
  let mut s = String::new();
  for name in mod_name {
    s.push_str(name.as_str());
    s.push_str(".");
  }
  s.push_str(i.as_str());
  s
}

#[derive(Debug, Clone)]
enum Entity<'a> {
  Def(front::Id),
  Bind(front::Id),
  Mod(front::Id, Env<'a>),
}

type Resolver = Vec<Vec<front::Id>>;

#[derive(Debug, Clone)]
struct Env<'a> {
  lookup: HashMap<Id<'a>, (Entity<'a>, TokenIx)>,
  syntax: HashMap<SyntaxId, Resolver>,
}

impl<'a> Env<'a> {
  fn new() -> Self {
    Self {
      lookup: HashMap::new(),
      syntax: HashMap::new(),
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
  errors: Vec<(ErrorPos, String)>,
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

  fn add_error(&mut self, pos: ErrorPos, msg: &str) {
    let s = format!("{}", msg);
    self.errors.push((pos, s));
  }

  fn here(&mut self) -> &mut HashMap<Id<'a>, (Entity<'a>, TokenIx)> {
    &mut self.envs.last_mut().unwrap().1.lookup
  }

  fn here_syntax(&mut self) -> &mut HashMap<SyntaxId, Resolver> {
    &mut self.envs.last_mut().unwrap().1.syntax
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

  fn lookup_mod(&self, n: &Vec<Id<'a>>) -> Option<(ModDepth, Vec<front::Id>, Env<'a>)> {
    assert!(!n.is_empty());
    for (depth, env) in self.envs.iter().rev() {
      match env.lookup.get(&n[0]) {
        Some((Entity::Mod(i, env), _)) => {
          let mut cur_env = env;
          let mut mod_name = vec![*i];
          for name in n.iter().skip(1) {
            match cur_env.lookup.get(name) {
              Some((Entity::Mod(i, env), _)) => {
                mod_name.push(*i);
                cur_env = env;
              }
              _ => return None,
            }
          }
          return Some((*depth, mod_name, cur_env.clone()));
        }
        _ => {}
      }
    }
    None
  }

  fn lookup_def(&self, i: &Id<'a>, env: &Env<'a>) -> Option<front::Id> {
    match env.lookup.get(i) {
      Some((Entity::Def(i), _)) => Some(*i),
      _ => None,
    }
  }

  fn lookup_syntax(&self, sid: &SyntaxId) -> Option<Resolver> {
    for env in self.envs.iter().rev() {
      if let Some(s) = env.1.syntax.get(sid) {
        return Some(s.clone());
      }
    }
    None
  }

  fn level_from(&self, depth: ModDepth) -> ModLevel {
    if self.cur_depth < depth {
      0
    } else {
      (self.cur_depth - depth) as ModLevel
    }
  }

  fn to_name(&self, mod_ids: &Vec<front::Id>) -> String {
    let mut v = Vec::new();
    for i in mod_ids {
      let mut s = String::new();
      s.push_str(self.symbols.get(i).unwrap_or(&"".to_string()));
      s.push_str("#");
      s.push_str(&i.0.to_string());
      v.push(s);
    }
    v.join(".")
  }

  fn rez_to_str(&self, rez: &Resolver) -> String {
    format!(
      "{:?}",
      rez
        .iter()
        .map(|m| self.to_name(m).to_string())
        .collect::<Vec<_>>()
    )
  }

  fn e_internal(&mut self, e: &Expr<'a>) -> Result<FExprInternal> {
    let epos: ErrorPos = e.1.clone().into();
    match &e.0 {
      ExprF::Hole => Ok(front::ExprF::Hole),
      ExprF::Bind(i) => {
        for env in self.envs.iter().rev() {
          match env.1.lookup.get(i) {
            Some((Entity::Def(i), _)) => {
              return Ok(front::ExprF::Def(self.level_from(env.0), vec![], *i))
            }
            Some((Entity::Bind(i), _)) => return Ok(front::ExprF::Bind(*i)),
            _ => {}
          }
        }
        Err((epos, format!("symbol not found: {}", i.as_str())))
      }
      ExprF::Mod(_) => Err((
        epos,
        "module reference is not allowed in expression".to_string(),
      )),
      ExprF::Def(mod_name, i) => {
        let (depth, m, env) = self
          .lookup_mod(mod_name)
          .ok_or((epos, format!("module not found: {}", ext_name(mod_name, i))))?;
        let i = self
          .lookup_def(i, &env)
          .ok_or((epos, format!("symbol not found: {}", ext_name(mod_name, i))))?;
        Ok(front::ExprF::Def(self.level_from(depth), m, i.clone()))
      }
      ExprF::Let(ds, e) => {
        self.envs.push((self.cur_depth, Env::new()));
        let mut decls = Vec::new();
        self.ds(ds, &mut Env::new(), &mut decls);
        let e = Rc::new(self.e(e));
        self.envs.pop();
        Ok(front::ExprF::Let(decls, e))
      }
      ExprF::Syntax(mod_name, sid, elems) => {
        let handler = self
          .syntax
          .get(sid)
          .ok_or(format!("unimplemented syntax handler: {:?}", sid))
          .unwrap()
          .clone();

        let rez = if mod_name.is_empty() {
          self.lookup_syntax(sid)
        } else {
          self
            .lookup_mod(mod_name)
            .unwrap()
            .2
            .syntax
            .get(sid)
            .cloned()
        };
        /* if let SyntaxId::User(_) = sid {
          eprintln!("at {}, query: mod_name = {:?}", epos.to_string(), mod_name);
          eprintln!(
            "resolver: {}",
            rez
              .clone()
              .map(|rez| format!("rez = {}", self.rez_to_str(&rez)))
              .unwrap_or("None".to_string())
          );
        } */

        let resolver = rez.map(|rez| {
          let here = &self.cur_mod_name;
          rez
            .iter()
            .map(|r| {
              // extract common prefix
              let mut common = 0;
              for (a, b) in r.iter().zip(here) {
                if a != b {
                  break;
                }
                common += 1;
              }
              let here_rest = here.len() - common;

              /* eprintln!(
                "RESOLVE: r = {:?}, here = {:?} / common = {:?}, here_rest = {:?}",
                r, here, common, here_rest
              ); */
              let relative = r[common..].to_vec();
              (here_rest as ModLevel, relative)
            })
            .collect::<Vec<_>>()
        });

        let (tokens, e, deps) = handler(elems)
          .map_err(|err| (e.1.clone().into(), err))?
          .into_inner();

        let mut i_map = HashMap::new();
        for (elem, t) in elems.iter().zip(tokens.iter()) {
          match (&elem.0, t) {
            (SynElemF::Ident(i), ElemToken::Ident(eid)) => {
              let id = self.new_id(i);
              i_map.insert(eid.clone(), (i.clone(), id));
            }
            _ => {}
          }
        }

        let mut e_map = HashMap::new();
        for (elem, t) in elems.iter().zip(tokens.into_iter()) {
          let loc = elem.1.begin.clone();
          match (&elem.0, t) {
            (SynElemF::Expr(e), ElemToken::Expr(eid)) => {
              self.envs.push((self.cur_depth, Env::new()));
              // add dependencies
              for i_eid in deps.get(&eid).unwrap() {
                let (i, id) = i_map.get(i_eid).unwrap();
                self
                  .here()
                  .insert(i.clone(), (Entity::Bind(id.clone()), loc.clone()));
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
          e_map: HashMap<ElemId, FExpr>,
          resolver: Option<Vec<(ModLevel, Vec<front::Id>)>>,
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
        ) -> FExprInternal {
          let base_mod_name = mod_name
            .iter()
            .map(|m| {
              if let LookupId::General(m) = m {
                *m
              } else {
                unreachable!()
              }
            })
            .collect::<Vec<_>>();

          let (level, relative) = match &env.resolver {
            Some(res) => {
              let l = *l as usize;
              assert!(l < res.len());
              res[l].clone()
            }
            None => (*l, Vec::new()),
          };

          let mut mod_name = relative;
          mod_name.extend(base_mod_name);

          front::ExprF::Def(level, mod_name, *i)
        }

        fn replace(e: &SyntaxExpr, env: &E) -> FExpr {
          use front::ExprF::*;
          let ei = match &e.0 {
            Hole => Hole,
            Bind(LookupId::General(i)) => Bind(i.clone()),
            Bind(LookupId::InSyntax(i)) => env.e_map.get(i).unwrap().0.clone(), // expr
            Ann(e1, e2) => Ann(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),
            Uni => Uni,
            Prim(s) => Prim(s.clone()),
            Wrap(e) => Wrap(Rc::new(replace(&e, env))),
            Def(l, mod_name, LookupId::General(i)) => resolve_ext(l, mod_name, i, env),
            Def(_, _, LookupId::InSyntax(_)) => unreachable!(),
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
            Proj(e, i) => Proj(Rc::new(replace(&e, env)), *i),

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
            Prop(e, i) => Prop(Rc::new(replace(&e, env)), replace_id(i, env)),
          };
          front::Expr(ei, e.1.clone())
        }
        let e = replace(&e, &env);
        Ok(front::ExprF::Wrap(Rc::new(e)))
      }
    }
  }

  fn e(&mut self, e: &Expr<'a>) -> FExpr {
    let tag = e.1.clone();
    match self.e_internal(e) {
      Ok(fe) => front::Expr(fe, tag),
      Err((pos, msg)) => {
        self.add_error(pos, &msg);
        front::Expr(front::ExprF::Hole, tag)
      }
    }
  }

  fn define_mod(
    &mut self,
    mod_id: &front::Id,
    m_params: &Vec<Arg<'a>>,
    m_body: &ModBody<'a>,
  ) -> (Vec<(Vis, front::Id, Rc<FExpr>)>, FModBody, Env<'a>) {
    self.envs.push((self.cur_depth, Env::new()));
    let mut param_infos = Vec::new();
    let mut params = Vec::new();
    for Arg((vis, names, ty), loc) in m_params {
      let ty = if let Some(ty) = ty {
        self.e(&ty)
      } else {
        front::Expr(front::ExprF::Hole, range_at(loc))
      };
      let ty = Rc::new(ty);
      for (index, name) in names.iter().enumerate() {
        let i = self.new_id(name);
        let l = TokenIx::new(loc.clone().into_inner() + index);
        self
          .here()
          .insert(name.clone(), (Entity::Bind(i), l.clone()));
        param_infos.push((name, l));
        params.push((vis.clone(), i, ty.clone()));
      }
    }
    let mut mod_env = Env::new();
    let mut mod_decls = Vec::new();
    let mod_body = match &m_body.0 {
      ModBodyF::Decls(ds) => {
        self.cur_depth += 1;
        self.cur_mod_name.push(mod_id.clone());
        self.envs.push((self.cur_depth, Env::new()));
        // overwrite the module parameter name references to actual definitions
        for ((_, i, _), (name, loc)) in params.iter().zip(param_infos.into_iter()) {
          let j = self.new_id(name);
          let def_i = front::Expr(front::ExprF::Def(0, vec![], i.clone()), range_at(&loc));
          mod_decls.push(front::Decl(
            front::DeclF::Def(j, def_i),
            range_at(&loc),
            range_at(&loc),
          ));
          self
            .here()
            .insert(name.clone(), (Entity::Def(j), loc.clone()));
        }
        self.envs.push((self.cur_depth, Env::new()));
        self.ds(ds, &mut mod_env, &mut mod_decls);
        self.envs.pop();
        self.envs.pop();
        self.cur_mod_name.pop();
        self.cur_depth -= 1;
        front::ModBody(front::ModBodyF::Decls(mod_decls), m_body.1.clone())
      }
      ModBodyF::Ref(ModRef(ModRefF::App(m, m_args), _)) => {
        let (depth, m, env) = self.lookup_mod(&m).unwrap();
        let mut args = Vec::new();
        for (vis, e) in m_args {
          args.push((vis.clone(), self.e(e)));
        }
        mod_env = env;

        // Rewriting resolvers: replace occurrence of m with relative mod_name
        struct Replacer<'c> {
          from: &'c Vec<front::Id>,
          to: &'c Vec<front::Id>,
        }

        fn replace(m: &mut Vec<front::Id>, repl: &Replacer) {
          if m.starts_with(&repl.from) {
            m.splice(0..repl.from.len(), repl.to.iter().cloned());
          }
        }

        fn replace_rez_for_env<'a>(env: &mut Env<'a>, repl: &Replacer) {
          for (_, entity) in env.lookup.iter_mut() {
            match &mut entity.0 {
              Entity::Mod(_, env) => replace_rez_for_env(env, repl),
              _ => {}
            }
          }
          for (_, rez) in env.syntax.iter_mut() {
            for m in rez.iter_mut() {
              replace(m, repl);
            }
          }
        }

        let mut repl_from = self.cur_mod_name[..depth as usize].to_vec();
        repl_from.extend(&m);
        let mut repl_to = self.cur_mod_name.clone();
        repl_to.push(mod_id.clone());

        let repl = Replacer {
          from: &repl_from,
          to: &repl_to,
        };

        replace_rez_for_env(&mut mod_env, &repl);
        front::ModBody(
          front::ModBodyF::App(self.level_from(depth), m, args),
          m_body.1.clone(),
        )
      }
      ModBodyF::Ref(ModRef(ModRefF::Import(_), _)) => unimplemented!(),
    };
    self.envs.pop();
    (params, mod_body, mod_env)
  }

  fn d(&mut self, d: &Decl<'a>, cur_mod: &mut Env<'a>, decls: &mut Vec<FDecl>) {
    let tag = d.1.clone();
    match &d.0 {
      DeclF::Local(ds) => {
        let mut empty_env = Env::new();
        self.ds(ds, &mut empty_env, decls);
      }
      DeclF::Mod(md, mb) => {
        let mod_id = self.new_id(&md.name);
        let (params, body, env) = self.define_mod(&mod_id, &md.params, mb);
        let loc = tag.begin.clone();
        decls.push(front::Decl(
          front::DeclF::Mod(mod_id, params, body),
          tag,
          mb.1.clone(),
        ));
        self.here().insert(
          md.name.clone(),
          (Entity::Mod(mod_id, env.clone()), loc.clone()),
        ); // uh
        cur_mod
          .lookup
          .insert(md.name.clone(), (Entity::Mod(mod_id, env), loc));
      }
      DeclF::Open(mr) => {
        // define a dummy module to consume m_args
        let mb = ModBody(ModBodyF::Ref(mr.clone()), mr.1.clone());
        let mod_id = self.fresh_id();
        let (params, body, env) = self.define_mod(&mod_id, &Vec::new(), &mb);
        decls.push(front::Decl(
          front::DeclF::Mod(mod_id, params, body),
          tag,
          mr.1.clone(),
        ));

        // inserts all entities from the dummy module to current module
        env.lookup.into_iter().for_each(|(name, (entity, loc))| {
          let tag = range_at(&loc);
          decls.push(front::Decl(
            match entity {
              Entity::Def(i) => front::DeclF::Def(
                i,
                front::Expr(front::ExprF::Def(0, vec![mod_id], i), tag.clone()),
              ),
              Entity::Bind(_) => unreachable!(),
              Entity::Mod(i, _) => {
                let mn = vec![mod_id, i];
                front::DeclF::Mod(
                  i,
                  Vec::new(),
                  front::ModBody(front::ModBodyF::App(0, mn, Vec::new()), tag.clone()),
                )
              }
            },
            tag.clone(),
            tag,
          ));
          self.here().insert(name, (entity, loc));
        });
        self.here_syntax().extend(env.syntax);
      }
      DeclF::Use(mr) => {
        let d = Decl(
          DeclF::Local(vec![Decl(DeclF::Open(mr.clone()), tag.clone())]),
          tag,
        );
        self.d(&d, cur_mod, decls);
      }
      DeclF::Def(def) => {
        self.envs.push((self.cur_depth, Env::new()));
        let mut conts = Vec::new();
        for Arg((vis, names, ty), loc) in &def.args {
          let ty = if let Some(ty) = ty {
            self.e(&ty)
          } else {
            front::Expr(front::ExprF::Hole, range_at(loc))
          };
          let ty = Rc::new(ty);
          for (index, name) in names.iter().enumerate() {
            let i = self.new_id(name);
            let l = TokenIx::new(loc.clone().into_inner() + index);
            self
              .here()
              .insert(name.clone(), (Entity::Bind(i), l.clone()));
            conts.push((vis.clone(), i, ty.clone(), l));
          }
        }
        let e = self.e(&def.body);
        let e = if let Some(ty) = &def.ty {
          let ty = self.e(&ty);
          let tag = range_merge(&e.1, &ty.1);
          front::Expr(front::ExprF::Ann(Rc::new(e), Rc::new(ty)), tag)
        } else {
          e
        };
        let mut e = e;
        for (vis, i, ty, loc) in conts.into_iter().rev() {
          match vis {
            Vis::Explicit => e = front::Expr(front::ExprF::LamE(i, ty, Rc::new(e)), range_at(&loc)),
            Vis::Implicit => e = front::Expr(front::ExprF::LamI(i, ty, Rc::new(e)), range_at(&loc)),
          }
        }
        self.envs.pop();
        let i = self.new_id(&def.name);
        let loc = tag.begin.clone();
        decls.push(front::Decl(
          front::DeclF::Def(i, e),
          tag,
          def.body.1.clone(),
        ));
        self
          .here()
          .insert(def.name.clone(), (Entity::Def(i), loc.clone()));
        cur_mod
          .lookup
          .insert(def.name.clone(), (Entity::Def(i), loc));
      }
      DeclF::Syntax(sid, _, syndefs, e) => {
        let mut next_elem_id = 0;
        let mut i_env = HashMap::new();
        let mut tokens = Vec::new();
        for s in syndefs {
          match &s.0 {
            SynDefF::Token(_) => tokens.push(ElemToken::Token),
            SynDefF::Ident(i) => {
              let eid = ElemId(next_elem_id);
              next_elem_id += 1;
              tokens.push(ElemToken::Ident(eid));
              i_env.insert(i.clone(), eid.clone());
            }
            SynDefF::Expr(_) => {
              let eid = ElemId(next_elem_id);
              next_elem_id += 1;
              tokens.push(ElemToken::Expr(eid));
            }
          }
        }
        self.envs.push((self.cur_depth, Env::new()));
        let mut exprs = HashMap::new();
        for (s, t) in syndefs.iter().zip(tokens.iter()) {
          match &s.0 {
            SynDefF::Token(_) => {}
            SynDefF::Ident(_) => {}
            SynDefF::Expr(i) => {
              let id = self.new_id(i);
              self
                .here()
                .insert(i.clone(), (Entity::Bind(id), s.1.clone()));
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
          Ok(fe) => front::Expr(fe, e.1.clone()),
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

        fn replace(e: &FExpr, env: &E) -> SyntaxExpr {
          use front::ExprF::*;
          let se = match &e.0 {
            Hole => Hole,
            Bind(i) => match env.exprs.get(i) {
              Some(eid) => Bind(LookupId::InSyntax(*eid)),
              None => Bind(LookupId::General(*i)),
            },
            Ann(e1, e2) => Ann(Rc::new(replace(&e1, env)), Rc::new(replace(&e2, env))),
            Uni => Uni,
            Prim(s) => Prim(s.clone()),
            Wrap(e) => Wrap(Rc::new(replace(&e, env))),

            Def(l, mod_name, i) => Def(
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
            Proj(e, i) => Proj(Rc::new(replace(&e, env)), *i),

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
            Prop(e, i) => Prop(Rc::new(replace(&e, env)), replace_id(i, env)),
          };
          SyntaxExpr(se, e.1.clone())
        }

        // eprintln!("SYNTAX: {:?}", e);
        let e = replace(&e, &env);
        self.symbols = env.symbols;
        // eprintln!("SYNTAX: {:?}", e);

        let interpret = SyntaxInterpret::new(tokens, e);
        let handler: SyntaxHandler<'a> = Rc::new(move |_| Ok(interpret.clone()));
        self.syntax.insert(sid.clone(), handler);
        let mut rez = Vec::new();
        let mut r = self.cur_mod_name.clone();
        while !r.is_empty() {
          rez.push(r.clone());
          r.pop();
        }
        rez.push(r);
        self.here_syntax().insert(sid.clone(), rez.clone());
        cur_mod.syntax.insert(sid.clone(), rez);
      }
    }
  }

  fn ds(&mut self, ds: &Vec<Decl<'a>>, cur_mod: &mut Env<'a>, decls: &mut Vec<FDecl>) {
    for d in ds {
      self.d(d, cur_mod, decls);
    }
  }
}

pub fn convert<'a>(ds: &Vec<Decl<'a>>) -> (front::Program<Tag>, Vec<(ErrorPos, String)>) {
  let mut b = Builder::new(builtin_syntax_handlers());
  let mut cur_mod = Env::new();
  let mut decls = Vec::new();
  b.ds(ds, &mut cur_mod, &mut decls);
  let symbols = b.symbols;
  (front::Program { decls, symbols }, b.errors)
}
