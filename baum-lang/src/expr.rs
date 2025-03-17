use crate::decl::DeclParser;
use crate::types::env::{Env, Syntax};
use crate::types::precedence::Precedence;
use crate::types::regex::{deriv, Regex};
use crate::types::token::{ErrorPos, Token, TokenRange, TokenType};
use crate::types::tracker::Tracker;
use crate::types::tree::{Expr, Id, SynElem, SynElemInternal, SyntaxId};
use crate::types::tree_base::{ExprF, SynElemF};
use std::collections::HashSet;
use std::rc::Rc;

type Result<T> = std::result::Result<T, (ErrorPos, String)>;

#[derive(Debug, Clone)]
enum AdvanceResult {
  Remain(Rc<Regex>),
  Pass(),
  Await(Rc<Regex>),
}

#[derive(Debug, Clone)]
struct SyntaxState<'a, 'b> {
  syntax: &'b Syntax,
  read: usize,
  elems: Vec<SynElem<'a>>,
}

impl<'a, 'b> SyntaxState<'a, 'b> {
  fn new(syntax: &'b Syntax) -> Self {
    SyntaxState {
      syntax,
      read: 0,
      elems: Vec::new(),
    }
  }

  fn advance(&self, e: SynElem<'a>) -> Self {
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
  Await(Vec<SyntaxCont<'b>>),
}

enum ModQualifier<'a, 'b> {
  Itself(Vec<Id<'a>>),
  Qualified(Vec<Id<'a>>, &'b Env<'a>),
}

enum Trailing<'a> {
  Done(Expr<'a>),
  Continue(Expr<'a>),
  Applicand(Expr<'a>),
}

pub struct ExprParser<'a, 'b> {
  tracker: Tracker<'a>,
  env: &'b Env<'a>,
  next_syntax_id: u16,
  in_syntax: bool,
  known_ops: HashSet<String>,
  errors: Vec<(ErrorPos, String)>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
  pub fn new(
    tracker: Tracker<'a>,
    env: &'b Env<'a>,
    next_syntax_id: u16,
    in_syntax: bool,
    known_ops: HashSet<String>,
    errors: Vec<(ErrorPos, String)>,
  ) -> Self {
    ExprParser {
      tracker,
      env,
      next_syntax_id,
      in_syntax,
      known_ops,
      errors,
    }
  }

  pub fn into_inner(self) -> (Tracker<'a>, u16, HashSet<String>, Vec<(ErrorPos, String)>) {
    (
      self.tracker,
      self.next_syntax_id,
      self.known_ops,
      self.errors,
    )
  }

  fn add_error(&mut self, pos: ErrorPos, msg: &str) {
    let s = format!("expr: {}", msg);
    self.errors.push((pos, s));
  }

  fn advance(
    &self,
    regex: Rc<Regex>,
    t: &Token<'a>,
  ) -> Option<(SynElemInternal<'a>, Vec<AdvanceResult>)> {
    let (qr, cont) = match deriv::d(&regex, &deriv::Query::Token(t.ty.clone(), t.str)) {
      Some(res) => res,
      None => return None,
    };
    let e = match qr {
      deriv::QueryResult::Token(ty, s) => {
        use deriv::ElemType;
        match ty {
          ElemType::Token => SynElemF::Token(s),
          ElemType::Dec => SynElemF::Dec(s),
          ElemType::Num => SynElemF::Num(s),
          ElemType::Chr => SynElemF::Chr(s),
          ElemType::Str => SynElemF::Str(s),
          ElemType::Id => SynElemF::Ident(Id::new(s)),
        }
      }
      _ => unreachable!(),
    };
    let mut res = Vec::new();
    res.push(AdvanceResult::Remain(cont.clone()));
    if deriv::has_eps(&cont) {
      res.push(AdvanceResult::Pass());
    }
    if deriv::next_expr(&cont) {
      let (_, cont) = deriv::d(&cont, &deriv::Query::Expr).unwrap();
      res.push(AdvanceResult::Await(cont));
    }
    Some((e, res))
  }

  fn read_until_expr(
    &mut self,
    ss: Vec<(SyntaxState<'a, 'b>, Rc<Regex>)>,
  ) -> Result<(Vec<SynElem<'a>>, ReadStatus<'b>)> {
    let mut ss = ss;
    for (_, r) in &ss {
      assert!(!deriv::next_expr(r));
    }
    let mut last_tracker_state = None;
    let mut passed_ss: Vec<SyntaxState> = Vec::new();
    let mut awaiting_ss: Vec<(SyntaxState, Rc<Regex>)> = Vec::new();

    // longest match
    loop {
      let (t, loc) = match self.tracker.peek() {
        Some(t) => {
          let t = t.clone();
          let loc = self.tracker.get_location();
          self.tracker.next();
          (t, loc)
        }
        None => break,
      };
      let mut next_ss: Vec<(SyntaxState, Rc<Regex>)> = Vec::new();
      let mut first = true;
      for s in ss {
        if let Some((e, conts)) = self.advance(s.1, &t) {
          for cont in conts {
            let ns = s
              .0
              .advance(SynElem(e.clone(), self.tracker.range_single(loc.clone())));
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
              AdvanceResult::Await(r) => {
                if first {
                  first = false;
                  passed_ss.clear();
                  awaiting_ss.clear();
                  last_tracker_state = Some(self.tracker.save_state());
                }
                awaiting_ss.push((ns, r))
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

    let epos = self.tracker.epos();
    let passes = passed_ss.len();
    let awaits = awaiting_ss.len();
    if passes == 0 && awaits == 0 {
      return Err((epos, "failed to parse (no candidate)".to_string()));
    }
    let ts = last_tracker_state.unwrap();
    self.tracker.restore_state(ts);

    if passes >= 1 && awaits >= 1 {
      return Err((epos, "ambiguous parse (pass/await)".to_string()));
    }
    if passes >= 1 {
      // pass
      assert!(awaits == 0);
      if passes >= 2 {
        return Err((epos, "ambiguous parse (pass/pass)".to_string()));
      }
      let s = passed_ss.pop().unwrap();
      Ok((s.elems, ReadStatus::Pass(s.syntax)))
    } else {
      // await
      assert!(passes == 0);
      let last_awaits = awaiting_ss.last().unwrap();
      let elems = last_awaits.0.elems.clone();
      let read = last_awaits.0.read;

      let awaits = awaiting_ss
        .into_iter()
        .filter(|(s, _)| s.read == read)
        .map(|(s, k)| SyntaxCont {
          syntax: s.syntax,
          cont: k,
        })
        .collect();
      Ok((elems, ReadStatus::Await(awaits)))
    }
  }

  fn parse_by_regex(
    &mut self,
    ss: Vec<&'b Syntax>,
    heads: usize,
  ) -> Option<(SyntaxId, Vec<deriv::SkipToken>, Vec<SynElem<'a>>)> {
    fn skips_from(s: &Syntax, heads: usize) -> Vec<deriv::SkipToken> {
      deriv::skip_e(&s.regex, heads).1
    }
    if ss.is_empty() {
      return None;
    }
    let mut ss = ss
      .into_iter()
      .map(|s| {
        assert!(!deriv::has_eps(&s.regex));
        (SyntaxState::new(s), deriv::skip_e(&s.regex, heads).0)
      })
      .collect::<Vec<_>>();
    let mut elems = Vec::new();
    loop {
      let (es, status) = self.read_until_expr(ss).ok()?;
      elems.extend(es);
      match status {
        ReadStatus::Pass(s) => return Some((s.t.clone(), skips_from(s, heads), elems)),
        ReadStatus::Await(conts) => {
          let pos = self.tracker.epos();

          // Rule: the last non-term can be predicted by the syntax, to use precedence correctly
          let last_count = conts.iter().filter(|sc| deriv::has_eps(&sc.cont)).count();
          if last_count >= 2 {
            self.add_error(pos, "ambiguous parse (ambiguous non-term)");
          }
          if last_count == 1 && conts.len() >= 2 {
            self.add_error(pos, "ambiguous parse (multiple non-term)");
          }
          let is_last = last_count == 1;
          // conts may be multiple. so we need to add every next-tokens to the known names.
          let last_known_ops = self.known_ops.clone();
          for sc in &conts {
            let nexts = deriv::next_tokens(&sc.cont);
            self.known_ops.extend(nexts);
          }
          let p = if is_last {
            &conts[0].syntax.right
          } else {
            &Precedence::Initial
          };
          let e = self.expr_p(p)?;
          let range = e.1.clone();
          elems.push(SynElem(SynElemF::Expr(Box::new(e)), range));
          self.known_ops = last_known_ops;
          for sc in &conts {
            if deriv::has_eps(&sc.cont) {
              // TODO: need to check: no other syntaxes have eps...
              return Some((sc.syntax.t.clone(), skips_from(sc.syntax, heads), elems));
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

  fn make_syntax(
    &mut self,
    mod_name: Vec<Id<'a>>,
    sid: SyntaxId,
    elems: Vec<SynElem<'a>>,
    range: TokenRange,
  ) -> Expr<'a> {
    Expr(ExprF::Syntax(mod_name, sid, elems), range)
  }

  fn module_name(&mut self) -> Option<ModQualifier<'a, 'b>> {
    // module: identifier or operator, or module itself
    let state = self.tracker.save_state();
    let mut mod_name = Vec::new();
    let mut env = self.env;
    loop {
      match self.tracker.peek_str() {
        None => {
          self.tracker.restore_state(state);
          return None;
        }
        Some(name) if env.is_modname(name) => {
          mod_name.push(Id::new(name));
          self.tracker.next();
          match self.tracker.peek_str() {
            Some(".") => {
              self.tracker.next();
            }
            _ => return Some(ModQualifier::Itself(mod_name)),
          }
          env = env.get_mod(name).unwrap();
        }
        Some(_) => return Some(ModQualifier::Qualified(mod_name, env)),
      }
    }
  }

  fn expr_leading(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    let begin_pos = self.tracker.get_location();
    let (mod_name, env) = match self.tracker.peek_ty_str() {
      None => return None,
      Some((TokenType::Ident, s)) if self.env.is_modname(s) => {
        match self.module_name()? {
          ModQualifier::Itself(mod_name) => {
            return Some(Expr(
              ExprF::Mod(mod_name),
              self.tracker.range_from(begin_pos),
            ));
          }
          ModQualifier::Qualified(mod_name, env) => {
            match self.tracker.peek_ty_str() {
              Some((TokenType::Ident, s)) if !env.is_opname(s) => {
                // module qualified identifier
                let id = Id::new(s);
                self.tracker.next();
                return Some(Expr(
                  ExprF::Def(mod_name, id),
                  self.tracker.range_from(begin_pos),
                ));
              }
              _ => {
                // module qualified operator
                (mod_name, env)
              }
            }
          }
        }
      }
      Some((TokenType::Ident, s)) if !self.env.is_opname(s) => {
        // identifier
        let id = Id::new(s);
        self.tracker.next();
        return Some(Expr(ExprF::Bind(id), self.tracker.range_from(begin_pos)));
      }
      _ => {
        // leading op
        (Vec::new(), self.env)
      }
    };
    match self.tracker.peek_ty()? {
      TokenType::DecNat | TokenType::Number | TokenType::Char | TokenType::String => {
        // literal
        let lits = self.filter_p(env.syntax.lits(), base_p);
        let (sid, _, elems) = self.parse_by_regex(lits, 0)?;
        Some(self.make_syntax(mod_name, sid, elems, self.tracker.range_from(begin_pos)))
      }
      TokenType::Ident | TokenType::Reserved => {
        let s = self.tracker.peek().unwrap().str;
        let pres: Vec<&'b Syntax> = self.filter_p(env.syntax.choose_pre(s)?, base_p);
        let (sid, _, elems) = self.parse_by_regex(pres, 0)?;
        Some(self.make_syntax(mod_name, sid, elems, self.tracker.range_from(begin_pos)))
      }
      _ => None,
    }
  }

  fn expr_trailing(&mut self, base_p: &Precedence, e: Expr<'a>) -> Trailing<'a> {
    let (mod_name, env) = match self.tracker.peek_ty_str() {
      None => return Trailing::Done(e),
      Some((TokenType::Ident, s)) if self.env.is_modname(s) => {
        match self.module_name() {
          Some(ModQualifier::Qualified(mod_name, env)) => {
            match self.tracker.peek_ty_str() {
              Some((TokenType::Ident, s)) if !env.is_trailing_opname(s) => {
                return Trailing::Applicand(e);
              }
              _ => {
                // module qualified operator
                (mod_name, env)
              }
            }
          }
          _ => return Trailing::Applicand(e),
        }
      }
      Some(_) => {
        // trailing op
        (Vec::new(), self.env)
      }
    };
    match self.tracker.peek_str() {
      None => return Trailing::Applicand(e),
      Some(s) => {
        let opes = match env.syntax.choose_ope(s) {
          None => return Trailing::Applicand(e),
          Some(opes) => self.filter_p(opes, base_p),
        };
        match self.parse_by_regex(opes, 1) {
          None => Trailing::Applicand(e),
          Some((sid, skip, elems_tail)) => {
            let range = e.1.clone();
            let begin = e.1.clone();
            let mut elems = match skip[..] {
              [deriv::SkipToken::Expr] => vec![SynElem(SynElemF::Expr(Box::new(e)), range)],
              [deriv::SkipToken::Id] => match e {
                Expr(ExprF::Bind(id), _) => vec![SynElem(SynElemF::Ident(id), range)],
                _ => {
                  self.add_error(range.clone().into(), "expected identifier for syntax");
                  vec![SynElem(SynElemF::Ident(Id::new("_")), range)]
                }
              },
              _ => unreachable!(),
            };
            elems.extend(elems_tail);
            let s = self.make_syntax(mod_name, sid, elems, self.tracker.range_extend(begin));
            Trailing::Continue(s)
          }
        }
      }
    }
  }

  fn expr_p(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    match self.tracker.peek_str() {
      Some(s) if self.known_ops.contains(s) => return None,
      Some("let") => {
        let let_pos = self.tracker.get_location();
        self.tracker.next();

        let (ds, d_env) = {
          let mut tracker = Tracker::new(Vec::new());
          std::mem::swap(&mut self.tracker, &mut tracker);
          let mut in_ops = HashSet::new();
          in_ops.insert("in".to_string());
          let next_syntax_id = self.next_syntax_id;
          let errors = std::mem::take(&mut self.errors);
          let mut d = DeclParser::new(tracker, self.env.clone(), next_syntax_id, in_ops, errors);

          let mut empty_env = Env::new();
          let ds = d.decls(&mut empty_env);

          let (tracker, d_env, next_syntax_id, _, errors) = d.into_inner();
          self.tracker = tracker;
          self.next_syntax_id = next_syntax_id;
          self.errors = errors;
          (ds, d_env)
        };

        match self.tracker.peek_str() {
          Some("in") => {
            self.tracker.next();
          }
          _ => {
            self.add_error(self.tracker.epos(), "expected 'in'");
            return None;
          }
        };

        let e = {
          let mut tracker = Tracker::new(Vec::new());
          std::mem::swap(&mut self.tracker, &mut tracker);
          let known_ops = std::mem::take(&mut self.known_ops);
          let next_syntax_id = self.next_syntax_id;
          let errors = std::mem::take(&mut self.errors);

          let mut ep = ExprParser::new(
            tracker,
            &d_env,
            next_syntax_id,
            self.in_syntax,
            known_ops,
            errors,
          );
          let e = ep.expr();

          let (tracker, next_syntax_id, known_ops, errors) = ep.into_inner();
          self.tracker = tracker;
          self.next_syntax_id = next_syntax_id;
          self.known_ops = known_ops;
          self.errors = errors;
          e?
        };

        return Some(Expr(
          ExprF::Let(ds, Box::new(e)),
          self.tracker.range_from(let_pos),
        ));
      }
      _ => {}
    }
    let mut e = self.expr_leading(base_p)?;
    loop {
      let state = self.tracker.save_state();
      match self.expr_trailing(base_p, e) {
        Trailing::Done(e) => return Some(e),
        Trailing::Continue(new_e) => e = new_e,
        Trailing::Applicand(e1) => {
          self.tracker.restore_state(state);
          let apps = self.filter_p(self.env.syntax.apps(), base_p);
          if apps.is_empty() {
            return Some(e1);
          }
          if apps.len() >= 2 {
            panic!();
          }
          let app = apps[0];
          let p = app.right.clone();
          let e2 = match self.expr_p(&p) {
            Some(e) => e,
            None => return Some(e1),
          };
          let e1_range = e1.1.clone();
          let e2_range = e2.1.clone();
          let elems = vec![
            SynElem(SynElemF::Expr(Box::new(e1)), e1_range.clone()),
            SynElem(SynElemF::Expr(Box::new(e2)), e2_range),
          ];
          e = self.make_syntax(
            Vec::new(),
            app.t.clone(),
            elems,
            self.tracker.range_extend(e1_range),
          );
        }
      }
    }
  }

  pub fn expr(&mut self) -> Option<Expr<'a>> {
    let e = self.expr_p(&Precedence::Initial);
    e
  }
}
