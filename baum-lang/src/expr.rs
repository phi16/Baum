use crate::decl::DeclParser;
use crate::types::mixfix::{deriv, NonTerm, Precedence, Regex};
use crate::types::parse::*;
use std::collections::HashSet;
use std::rc::Rc;

macro_rules! log {
  ($($arg:tt)*) => {
    // eprintln!($($arg)*);
  };
}

#[derive(Debug, Clone)]
enum AdvanceResult {
  Remain(Rc<Regex>),
  Pass(),
  Await(NonTerm, Rc<Regex>),
}

#[derive(Debug, Clone)]
struct SyntaxState<'a, 'b> {
  syntax: &'b Syntax,
  read: usize,
  elems: Vec<SyntaxElem<'a>>,
}

impl<'a, 'b> SyntaxState<'a, 'b> {
  fn new(syntax: &'b Syntax) -> Self {
    SyntaxState {
      syntax,
      read: 0,
      elems: Vec::new(),
    }
  }

  fn advance(&self, e: SyntaxElem<'a>) -> Self {
    SyntaxState {
      syntax: self.syntax,
      read: self.read + 1,
      elems: {
        let mut elems = self.elems.clone();
        elems.push(e);
        elems
      },
    }
  }
}

#[derive(Debug, Clone)]
struct SyntaxCont<'b> {
  syntax: &'b Syntax,
  cont: Rc<Regex>,
}

#[derive(Debug, Clone)]
enum ReadStatus<'b> {
  Pass(&'b Syntax),
  Await(NonTerm, Vec<SyntaxCont<'b>>),
}

pub struct ExprParser<'a, 'b> {
  tracker: Tracker<'a>,
  env: &'b Env<'a>,
  known_ops: HashSet<String>,
  errors: Vec<String>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
  pub fn new(
    tracker: Tracker<'a>,
    env: &'b Env<'a>,
    known_ops: HashSet<String>,
    errors: Vec<String>,
  ) -> Self {
    ExprParser {
      tracker,
      env,
      known_ops,
      errors,
    }
  }

  pub fn into_inner(self) -> (Tracker<'a>, HashSet<String>, Vec<String>) {
    (self.tracker, self.known_ops, self.errors)
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, expr: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn advance(
    &self,
    regex: Rc<Regex>,
    t: &Token<'a>,
  ) -> Option<(SyntaxElem<'a>, Vec<AdvanceResult>)> {
    let (qr, cont) = match deriv::d(&regex, &deriv::Query::Token(t.ty.clone(), t.str)) {
      Some(res) => res,
      None => return None,
    };
    let e = match qr {
      deriv::QueryResult::Token(ty, s) => {
        use deriv::ElemType;
        match ty {
          ElemType::Token => SyntaxElem::Token(s),
          ElemType::Nat => SyntaxElem::Nat(s),
          ElemType::Rat => SyntaxElem::Rat(s),
          ElemType::Chr => SyntaxElem::Chr(s),
          ElemType::Str => SyntaxElem::Str(s),
          ElemType::Id => SyntaxElem::Ident(Id::new(s)),
        }
      }
      _ => unreachable!(),
    };
    let mut res = Vec::new();
    res.push(AdvanceResult::Remain(cont.clone()));
    if deriv::has_eps(&cont) {
      res.push(AdvanceResult::Pass());
    }
    if let Some(nt) = deriv::next_nonterm(&cont) {
      let (_, cont) = deriv::d(&cont, &deriv::Query::NonTerm(nt)).unwrap();
      res.push(AdvanceResult::Await(nt, cont));
    }
    Some((e, res))
  }

  fn read_until_nonterm(
    &mut self,
    ss: Vec<(SyntaxState<'a, 'b>, Rc<Regex>)>,
  ) -> Result<(Vec<SyntaxElem<'a>>, ReadStatus<'b>), ()> {
    let mut ss = ss;
    for (_, r) in &ss {
      assert!(deriv::next_nonterm(r).is_none());
    }
    let pos = self.tracker.pos();
    let mut last_tracker_state = None;
    let mut passed_ss: Vec<SyntaxState> = Vec::new();
    let mut awaiting_ss: Vec<(SyntaxState, NonTerm, Rc<Regex>)> = Vec::new();

    // longest match
    loop {
      let t = match self.tracker.peek() {
        Some(t) => {
          // TODO: parse module names here?
          let t = t.clone();
          self.tracker.next();
          t
        }
        None => break,
      };
      log!("READ {:?}", t);
      let mut next_ss: Vec<(SyntaxState, Rc<Regex>)> = Vec::new();
      let mut first = true;
      for s in ss {
        log!("- Advance: s = {:?}", s);
        if let Some((e, conts)) = self.advance(s.1, &t) {
          for cont in conts {
            let ns = s.0.advance(e.clone());
            log!("- Cont: {:?}", cont);
            match cont {
              AdvanceResult::Remain(r) => next_ss.push((ns, r)),
              AdvanceResult::Pass() => {
                if first {
                  first = false;
                  passed_ss.clear();
                  awaiting_ss.clear();
                  last_tracker_state = Some(self.tracker.save_state());
                }
                passed_ss.push(ns)
              }
              AdvanceResult::Await(nt, r) => {
                if first {
                  first = false;
                  passed_ss.clear();
                  awaiting_ss.clear();
                  last_tracker_state = Some(self.tracker.save_state());
                }
                awaiting_ss.push((ns, nt, r))
              }
            }
          }
        }
      }
      if next_ss.is_empty() {
        break;
      }
      ss = next_ss;
    }
    log!("----------");
    log!("[Passed]");
    for s in &passed_ss {
      log!("- {:?}", s);
    }
    log!("[Awaiting]");
    for s in &awaiting_ss {
      log!("- {:?}", s);
    }
    log!("----------");

    let passes = passed_ss.len();
    let awaits = awaiting_ss.len();
    if passes == 0 && awaits == 0 {
      self.add_error(pos, "failed to parse (no candidate)");
      return Err(());
    }
    let ts = last_tracker_state.unwrap();
    self.tracker.restore_state(ts);
    log!("Restored: {:?}", self.tracker.peek());

    if passes >= 1 && awaits >= 1 {
      self.add_error(pos, "ambiguous parse (pass/await)");
      return Err(());
    }
    if passes >= 1 {
      // pass
      assert!(awaits == 0);
      if passes >= 2 {
        self.add_error(pos, "ambiguous parse (pass/pass)");
        return Err(());
      }
      let s = passed_ss.pop().unwrap();
      Ok((s.elems, ReadStatus::Pass(s.syntax)))
    } else {
      // await
      assert!(passes == 0);
      let mut fail = false;
      let nt = awaiting_ss[0].1;
      for s in &awaiting_ss {
        if s.1 != nt {
          fail = true;
          break;
        }
      }
      if fail {
        self.add_error(pos, "ambiguous parse (await/await)");
        return Err(());
      }

      let last_awaits = awaiting_ss.last().unwrap();
      let elems = last_awaits.0.elems.clone();
      let read = last_awaits.0.read;
      let nt = last_awaits.1;

      let awaits = awaiting_ss
        .into_iter()
        .filter(|(s, _, _)| s.read == read)
        .map(|(s, _, k)| SyntaxCont {
          syntax: s.syntax,
          cont: k,
        })
        .collect();
      Ok((elems, ReadStatus::Await(nt, awaits)))
    }
  }

  fn parse_by_regex(
    &mut self,
    ss: Vec<&'b Syntax>,
    heads: Vec<SyntaxElem<'a>>,
  ) -> Option<(&'b Syntax, Vec<SyntaxElem<'a>>)> {
    if ss.is_empty() {
      return None;
    }
    let mut ss = ss
      .into_iter()
      .map(|s| {
        assert!(!deriv::has_eps(&s.regex));
        (SyntaxState::new(s), deriv::skip_e(&s.regex, heads.len()))
      })
      .collect::<Vec<_>>();
    let mut elems = heads;
    loop {
      let (es, status) = self.read_until_nonterm(ss).ok()?;
      elems.extend(es);
      match status {
        ReadStatus::Pass(s) => return Some((s, elems)),
        ReadStatus::Await(nt, conts) => {
          // Rule: the last non-term can be predicted by the syntax, to use precedence correctly
          let last_count = conts.iter().filter(|sc| deriv::has_eps(&sc.cont)).count();
          if last_count >= 2 {
            self.add_error(self.tracker.pos(), "ambiguous parse (ambiguous non-term)");
            return None;
          }
          if last_count == 1 && conts.len() >= 2 {
            self.add_error(self.tracker.pos(), "ambiguous parse (multiple non-term)");
            return None;
          }
          let is_last = last_count == 1;
          // conts may be multiple. so we need to add every next-tokens to the known names.
          let last_known_ops = self.known_ops.clone();
          for sc in &conts {
            let nexts = deriv::next_tokens(&sc.cont);
            log!("adding to known ops: {:?}", nexts);
            self.known_ops.extend(nexts);
          }
          log!("Execute Await: {:?}", nt);
          let e = if nt == NonTerm::Expr {
            let p = if is_last {
              &conts[0].syntax.right
            } else {
              &Precedence::Initial
            };
            let pos = self.tracker.pos();
            let e = match self.expr_p(p) {
              Some(e) => e,
              None => {
                self.add_error(pos, "missing expr");
                Expr(ExprF::Hole, pos)
              }
            };
            SyntaxElem::Expr(Box::new(e))
          } else {
            let tracker = std::mem::take(&mut self.tracker);
            let known_ops = std::mem::take(&mut self.known_ops);
            let errors = std::mem::take(&mut self.errors);
            let mut d = DeclParser::new(tracker, self.env.clone(), known_ops, errors);
            let e = match nt {
              NonTerm::Def => {
                let def = match d.def() {
                  Some(def) => def,
                  None => {
                    let (tracker, _, errors) = d.into_inner();
                    self.tracker = tracker;
                    self.errors = errors;
                    return None;
                  }
                };
                SyntaxElem::Def(Box::new(def))
              }
              NonTerm::Decls => {
                let mut empty_env = Env::new();
                let ds = d.decls(&mut empty_env);
                log!("Decls: {:?}", ds);
                SyntaxElem::Decls(ds)
              }
              _ => unreachable!(),
            };
            let (tracker, _, errors) = d.into_inner();
            self.tracker = tracker;
            self.errors = errors;
            e
          };
          log!("Execute Await: completed ({:?})", e);
          elems.push(e);
          self.known_ops = last_known_ops;
          for sc in &conts {
            if deriv::has_eps(&sc.cont) {
              // TODO: need to check: no other syntaxes have eps...
              log!("EPS: {:?}, {:?}", sc.syntax, elems);
              return Some((sc.syntax, elems));
            }
          }
          ss = conts
            .into_iter()
            .map(|s| {
              if deriv::has_eps(&s.cont) {
                // TODO
                panic!();
              }
              (SyntaxState::new(s.syntax), s.cont)
            })
            .collect();
        }
      }
    }
  }

  fn filter_p(&self, ss: &'b Vec<Syntax>, base_p: &Precedence) -> Vec<&'b Syntax> {
    ss.into_iter().filter(|p| *base_p <= p.left).collect()
  }

  fn make_syntax(&self, s: &Syntax, elems: Vec<SyntaxElem<'a>>, pos: TokenPos) -> Expr<'a> {
    Expr(ExprF::Syntax(s.clone(), elems), pos)
  }

  fn module_name(&mut self) -> Option<(Vec<Id<'a>>, Option<&'b Env<'a>>)> {
    // module: identifier or operator, or module itself
    let mut mod_name = Vec::new();
    let mut env = self.env;
    loop {
      let t = self.tracker.peek()?;
      let pos = t.pos;
      let name = t.str;
      if !env.is_modname(name) {
        break;
      }
      mod_name.push(Id::new(name));
      self.tracker.next();
      match self.tracker.peek() {
        Some(t) if t.ty == TokenType::Reserved && t.str == "." => {
          self.tracker.next();
        }
        _ => {
          // module itself
          return Some((mod_name, None));
        }
      }
      match env.get_mod(name) {
        Some(e) => env = e,
        None => {
          self.add_error(pos, &format!("module not found: {:?}.{}", mod_name, name));
          return None;
        }
      }
    }
    Some((mod_name, Some(env)))
  }

  fn expr_leading(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    log!(
      "- expr1: base_p = {:?}, t = {:?}",
      base_p,
      self.tracker.peek()
    );
    let t = self.tracker.peek()?;
    let pos = t.pos;
    let (t, env) = if self.env.is_modname(t.str) {
      let (mod_name, env) = self.module_name()?;
      match env {
        Some(env) => {
          match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Ident && !env.is_leading_opname(t.str) => {
              // external identifier
              let id = Id::new(t.str);
              self.tracker.next();
              log!("- expr1: ext = {:?}", id);
              return Some(Expr(ExprF::Ext(mod_name, id), pos));
            }
            _ => (self.tracker.peek()?, env),
          }
        }
        None => {
          // module itself
          return Some(Expr(ExprF::Mod(mod_name), pos));
        }
      }
    } else {
      if t.ty == TokenType::Ident
        && !self.env.is_leading_opname(t.str)
        && !self.known_ops.contains(t.str)
      {
        // identifier
        let id = Id::new(t.str);
        self.tracker.next();
        log!("- expr1: var = {:?}", id);
        return Some(Expr(ExprF::Var(id), pos));
      }
      // as-is
      (t, self.env)
    };
    if t.ty != TokenType::Ident && t.ty != TokenType::Reserved {
      let lits: Vec<&'b Syntax> = self.filter_p(env.syntax.lits(), base_p);
      log!("- expr1: lits = {:?}", lits);
      let e = self.parse_by_regex(lits, Vec::new())?;
      return Some(self.make_syntax(e.0, e.1, pos));
    }
    let pres: Vec<&'b Syntax> = self.filter_p(env.syntax.choose_pre(t.str)?, base_p);
    log!("- expr1: pres = {:?}", pres);
    if pres.is_empty() && t.ty == TokenType::Ident {
      let id = Id::new(t.str);
      self.tracker.next();
      return Some(Expr(ExprF::Var(id), pos));
    }
    let e = self.parse_by_regex(pres, Vec::new())?;
    log!("- expr1 result: {:?}", e);
    return Some(self.make_syntax(e.0, e.1, pos));
  }

  fn expr_trailing(&mut self, base_p: &Precedence, e: Expr<'a>) -> Result<(bool, Expr<'a>), ()> {
    log!(
      "- expr2: base_p = {:?}, t = {:?}",
      base_p,
      self.tracker.peek()
    );
    let t = match self.tracker.peek() {
      Some(t) => t,
      None => return Ok((true, e)),
    };
    let pos = t.pos;
    let state = self.tracker.save_state();
    let (t, env) = if self.env.is_modname(t.str) {
      match self.module_name() {
        Some((_, Some(env))) => match self.tracker.peek() {
          Some(t)
            if (t.ty == TokenType::Ident || t.ty == TokenType::Reserved)
              && env.is_trailing_opname(t.str) =>
          {
            (t, env)
          }
          _ => {
            self.tracker.restore_state(state.clone());
            (self.tracker.peek().unwrap(), self.env)
          }
        },
        _ => {
          self.tracker.restore_state(state.clone());
          (self.tracker.peek().unwrap(), self.env)
        }
      }
    } else {
      (t, self.env)
    };
    let e = match env.syntax.choose_ope(t.str) {
      Some(opes) => {
        let opes = self.filter_p(opes, base_p);
        log!("- expr2: opes = {:?}", opes);
        match self.parse_by_regex(opes, vec![SyntaxElem::Expr(Box::new(e.clone()))]) {
          Some(e) => e,
          None => {
            // TODO: try apps?
            self.tracker.restore_state(state);
            return Ok((true, e));
          }
        }
      }
      None => {
        let apps = self.filter_p(env.syntax.apps(), base_p);
        log!("- expr2: apps = {:?}", apps);
        if apps.is_empty() {
          return Ok((true, e));
        }
        if apps.len() >= 2 {
          self.add_error(pos, "ambiguous parse (apps)");
          return Err(());
        }
        let p = apps[0].right.clone();
        match self.expr_p(&p) {
          Some(e2) => {
            log!("- expr2: e2 = {:?}", e2);
            (
              apps[0],
              vec![
                SyntaxElem::Expr(Box::new(e)),
                SyntaxElem::Expr(Box::new(e2)),
              ],
            )
          }
          None => return Ok((true, e)),
        }
      }
    };
    log!("- expr2: e = {:?}", e);
    return Ok((false, self.make_syntax(e.0, e.1, pos)));
  }

  fn expr_p(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    log!("- EXPR_P: {:?}", base_p);
    let mut e = self.expr_leading(base_p)?;
    loop {
      match self.expr_trailing(base_p, e).ok()? {
        (true, e) => {
          log!("- EXPR_P result: {:?}", e);
          return Some(e);
        }
        (false, e2) => e = e2,
      }
    }
  }

  pub fn expr(&mut self) -> Option<Expr<'a>> {
    log!("");
    log!("EXPR: t = {:?}", self.tracker.peek());
    self.expr_p(&Precedence::Initial)
  }
}
