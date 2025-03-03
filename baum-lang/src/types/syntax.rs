use crate::types::precedence::Precedence;
use crate::types::regex::{Regex, Terminal};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Syntax<T> {
  pub left: Precedence,
  pub right: Precedence,
  pub regex: Rc<Regex>,
  pub t: T,
}

impl<T> std::fmt::Debug for Syntax<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Syn({:?}, {:?}) {:?}", self.left, self.right, self.regex)
  }
}

impl<T> Syntax<T> {
  pub fn new(left: Precedence, right: Precedence, regex: Rc<Regex>, t: T) -> Self {
    Syntax {
      left,
      right,
      regex,
      t,
    }
  }
}

#[derive(Clone)]
pub struct SyntaxTable<T> {
  pres: HashMap<String, Vec<Syntax<T>>>, // starts from token
  lits: Vec<Syntax<T>>,                  // literals
  opes: HashMap<String, Vec<Syntax<T>>>, // starts from expr (or ident) and token
  apps: Vec<Syntax<T>>,                  // expr expr
}

// Note: this is necessary to avoid T: Debug bound
impl<T> std::fmt::Debug for SyntaxTable<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("SyntaxTable")
      .field("pres", &self.pres)
      .field("lits", &self.lits)
      .field("opes", &self.opes)
      .field("apps", &self.apps)
      .finish()
  }
}

impl<T> SyntaxTable<T> {
  pub fn new() -> Self {
    SyntaxTable {
      pres: HashMap::new(),
      lits: Vec::new(),
      opes: HashMap::new(),
      apps: Vec::new(),
    }
  }

  pub fn def(&mut self, prec: &str, seqs: Rc<Regex>, t: T) {
    let (left, right) = Precedence::parse(prec).unwrap();
    self.add(Syntax::new(left, right, seqs, t));
  }

  pub fn add(&mut self, syn: Syntax<T>) {
    let seqs = &syn.regex;
    let v = match &**seqs {
      Regex::Terminal(Terminal::Token(ref s)) => {
        self.pres.entry(s.to_string()).or_insert(Vec::new())
      }
      Regex::Seq(first, cont) => match **first {
        Regex::Terminal(Terminal::Token(ref s)) => {
          self.pres.entry(s.to_string()).or_insert(Vec::new())
        }
        Regex::Expr | Regex::Terminal(Terminal::Id) => match **cont {
          Regex::Terminal(Terminal::Token(ref s)) => {
            self.opes.entry(s.to_string()).or_insert(Vec::new())
          }
          Regex::Expr => &mut self.apps,
          Regex::Seq(ref second, _) => match **second {
            Regex::Terminal(Terminal::Token(ref s)) => {
              self.opes.entry(s.to_string()).or_insert(Vec::new())
            }
            _ => panic!(),
          },
          _ => panic!(),
        },
        _ => &mut self.lits,
      },
      _ => &mut self.lits,
    };
    v.push(syn);
  }

  #[allow(dead_code)]
  pub fn dump(&self) {
    println!("[Prefixs]");
    for syn in &self.pres {
      println!("{:?}", syn);
    }
    println!("[Literals]");
    for syn in &self.lits {
      println!("{:?}", syn);
    }
    println!("[Operators]");
    for syn in &self.opes {
      println!("{:?}", syn);
    }
    println!("[Applications]");
    for syn in &self.apps {
      println!("{:?}", syn);
    }
  }

  pub fn choose_pre(&self, s: &str) -> Option<&Vec<Syntax<T>>> {
    self.pres.get(s)
  }
  pub fn lits(&self) -> &Vec<Syntax<T>> {
    &self.lits
  }
  pub fn choose_ope(&self, s: &str) -> Option<&Vec<Syntax<T>>> {
    self.opes.get(s)
  }
  pub fn apps(&self) -> &Vec<Syntax<T>> {
    &self.apps
  }

  pub fn is_pre_head(&self, s: &str) -> bool {
    self.pres.contains_key(s)
  }
  pub fn is_ope_head(&self, s: &str) -> bool {
    self.opes.contains_key(s)
  }

  pub fn merge(&mut self, other: SyntaxTable<T>) {
    for (k, v) in other.pres {
      self.pres.entry(k).or_insert(Vec::new()).extend(v);
    }
    self.lits.extend(other.lits);
    for (k, v) in other.opes {
      self.opes.entry(k).or_insert(Vec::new()).extend(v);
    }
    self.apps.extend(other.apps);
  }
}

use crate::types::tree::SynElem;
use baum_front::types::tree as front;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ElemId(pub u16);

#[derive(Debug, Clone)]
pub enum ElemToken {
  Token,
  Ident(ElemId),
  Dec,
  Num,
  Chr,
  Str,
  Expr(ElemId),
}

#[derive(Debug, Clone)]
pub enum LookupId {
  InSyntax(ElemId),
  General(front::Id),
}

#[derive(Debug, Clone)]
pub struct SyntaxExpr(pub front::ExprF<LookupId, Rc<SyntaxExpr>>);

fn dependency_from(e: &SyntaxExpr) -> HashMap<ElemId, Vec<ElemId>> {
  fn add(id: &LookupId, env: &mut Vec<ElemId>) {
    if let LookupId::InSyntax(eid) = id {
      env.push(eid.clone());
    }
  }

  fn lcs(a: &Vec<ElemId>, b: &Vec<ElemId>) -> Vec<ElemId> {
    let mut res = Vec::new();
    for x in a {
      if b.contains(x) {
        res.push(x.clone());
      }
    }
    res
  }

  fn rec(e: &SyntaxExpr, env: &Vec<ElemId>, map: &mut HashMap<ElemId, Vec<ElemId>>) {
    use front::ExprF::*;
    match &e.0 {
      Hole => {}
      Bind(LookupId::General(_)) => {}
      Bind(LookupId::InSyntax(eid)) => {
        if map.contains_key(eid) {
          // take intersection
          let map_env = map.get_mut(eid).unwrap();
          *map_env = lcs(env, map_env);
        } else {
          map.insert(eid.clone(), env.clone());
        }
      }
      Ann(e1, e2) => {
        rec(&e1, env, map);
        rec(&e2, env, map);
      }
      Uni => {}

      Ext(_, _, _) => {}
      Let(_, _) => {}
      Lit(_) => {}

      PiE(i, e1, e2) => {
        let mut env = env.clone();
        rec(&e1, &env, map);
        if let Some(i) = i {
          add(i, &mut env);
        }
        rec(&e2, &env, map);
      }
      LamE(i, e1, e2) => {
        let mut env = env.clone();
        rec(&e1, &env, map);
        add(i, &mut env);
        rec(&e2, &env, map);
      }
      AppE(e1, e2) => {
        rec(&e1, env, map);
        rec(&e2, env, map);
      }

      PiI(i, e1, e2) => {
        let mut env = env.clone();
        rec(&e1, &env, map);
        if let Some(i) = i {
          add(i, &mut env);
        }
        rec(&e2, &env, map);
      }
      LamI(i, e1, e2) => {
        let mut env = env.clone();
        rec(&e1, &env, map);
        add(i, &mut env);
        rec(&e2, &env, map);
      }
      AppI(e1, e2) => {
        rec(&e1, env, map);
        rec(&e2, env, map);
      }

      TupleTy(tys) => {
        let mut env = env.clone();
        for (i, e) in tys {
          rec(&e, &env, map);
          if let Some(i) = i {
            add(i, &mut env);
          }
        }
      }
      TupleCon(es) => {
        for e in es {
          rec(&e, env, map);
        }
      }
      Proj(_, e) => {
        rec(&e, env, map);
      }

      ObjTy(es) => {
        let mut env = env.clone();
        for (i, e) in es {
          rec(&e, &env, map);
          add(i, &mut env);
        }
      }
      ObjCon(es) => {
        for (_, e) in es {
          rec(&e, env, map);
        }
      }
      Prop(_, e) => {
        rec(&e, env, map);
      }
    }
  }
  let env = Vec::new();
  let mut map = HashMap::new();
  rec(e, &env, &mut map);
  map
}

#[derive(Debug, Clone)]
pub struct SyntaxInterpret {
  tokens: Vec<ElemToken>,
  e: SyntaxExpr,
  deps: HashMap<ElemId, Vec<ElemId>>,
}

impl SyntaxInterpret {
  pub fn new(tokens: Vec<ElemToken>, e: SyntaxExpr) -> Self {
    let deps = dependency_from(&e);
    SyntaxInterpret { tokens, e, deps }
  }

  pub fn into_inner(self) -> (Vec<ElemToken>, SyntaxExpr, HashMap<ElemId, Vec<ElemId>>) {
    (self.tokens, self.e, self.deps)
  }
}

pub type SyntaxHandler<'a> =
  Rc<dyn for<'b> Fn(&'b Vec<SynElem<'a>>) -> Result<SyntaxInterpret, String>>;
