use crate::mixfix::*;
use crate::mixfix_types::*;
use crate::pretty::*;
use crate::tokenize::*;
use crate::types::*;
use std::collections::HashSet;
use std::rc::Rc;

struct Tracker<'a> {
  token_list: Vec<Token<'a>>,
  current_pos: usize,
  indent: Indent,
  last_eol: TokenPos,
}

impl<'a> Default for Tracker<'a> {
  fn default() -> Self {
    Tracker {
      token_list: Vec::new(),
      current_pos: 0,
      indent: Indent::Base,
      last_eol: TokenPos::EoF,
    }
  }
}

type TrackerState = (usize, Indent, TokenPos);

impl<'a> Tracker<'a> {
  fn new(tokens: Vec<Token<'a>>) -> Self {
    Tracker {
      token_list: tokens,
      current_pos: 0,
      indent: Indent::Base,
      last_eol: TokenPos::EoL(0),
    }
  }

  fn set_indent(&mut self, indent: Indent) {
    self.indent = match indent {
      Indent::Base => unreachable!(),
      Indent::Head(i) => Indent::Head(i),
      Indent::Cont(i) => Indent::Head(i),
    };
  }

  fn peek_raw(&self) -> Option<&Token<'a>> {
    self.token_list.get(self.current_pos)
  }

  fn peek(&self) -> Option<&Token<'a>> {
    let i = self.indent;
    self.peek_raw().filter(|t| i < t.indent)
  }

  fn next(&mut self) -> Option<&Token<'a>> {
    self.last_eol = match self.peek_raw() {
      Some(t) => match t.pos {
        TokenPos::Pos(l, _) => TokenPos::EoL(l),
        _ => unreachable!(),
      },
      _ => TokenPos::EoF,
    };
    self.current_pos += 1;
    self.peek_raw()
  }

  fn skip_to_next_head(&mut self) {
    while let Some(t) = self.peek() {
      match t.indent {
        Indent::Head(_) => break,
        _ => self.next(),
      };
    }
  }

  fn end_of_line(&self) -> TokenPos {
    self.last_eol
  }

  fn pos(&self) -> TokenPos {
    let last_pos = self.end_of_line();
    self.peek().map_or(last_pos, |t| t.pos)
  }

  fn save_indent(&self) -> Indent {
    self.indent
  }

  fn restore_indent(&mut self, indent: Indent) {
    self.indent = indent;
  }

  fn save_state(&self) -> TrackerState {
    (self.current_pos, self.indent, self.last_eol)
  }

  fn restore_state(&mut self, state: TrackerState) {
    self.current_pos = state.0;
    self.indent = state.1;
    self.last_eol = state.2;
  }
}

#[derive(Debug, Clone)]
enum AdvanceResult {
  Pass(),
  Remain(Rc<Regex>),
  Await(NonTerm, Rc<Regex>),
}

#[derive(Debug, Clone)]
struct SyntaxState<'a> {
  syntax: &'a Syntax,
  read: usize,
}

impl<'a> SyntaxState<'a> {
  fn new(syntax: &'a Syntax) -> Self {
    SyntaxState { syntax, read: 0 }
  }

  fn advance(&self) -> Self {
    SyntaxState {
      syntax: self.syntax,
      read: self.read + 1,
    }
  }
}

#[derive(Debug, Clone)]
struct SyntaxCont<'a> {
  syntax: &'a Syntax,
  cont: Rc<Regex>,
}

#[derive(Debug, Clone)]
enum ReadStatus<'a> {
  Pass(&'a Syntax),
  Await(NonTerm, Vec<SyntaxCont<'a>>),
}

struct ExprParser<'a, 'b> {
  tracker: Tracker<'a>,
  syntax: &'b SyntaxTable,
  errors: Vec<String>,
  known_ops: HashSet<String>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
  fn new(tracker: Tracker<'a>, syntax: &'b SyntaxTable) -> Self {
    ExprParser {
      tracker,
      syntax,
      errors: Vec::new(),
      known_ops: HashSet::new(),
    }
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, expr: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn is_opname(&self, s: &str) -> bool {
    self.syntax.is_head(s) || self.known_ops.contains(s)
  }

  fn advance(&self, regex: Rc<Regex>, t: &Token) -> Vec<AdvanceResult> {
    let cont: Rc<Regex> = deriv::d(&regex, t);
    if deriv::is_fail(&cont) {
      return Vec::new();
    }
    let mut res = Vec::new();
    res.push(AdvanceResult::Remain(cont.clone()));
    if deriv::has_eps(&cont) {
      res.push(AdvanceResult::Pass());
    }
    for nt in deriv::next_nonterm(&cont) {
      res.push(AdvanceResult::Await(nt, deriv::d_nonterm(&cont, &nt)));
    }
    res
  }

  fn read_until_nonterm(
    &mut self,
    ss: Vec<(SyntaxState<'b>, Rc<Regex>)>,
  ) -> Option<ReadStatus<'b>> {
    let mut ss = ss;
    // TODO: ss may contain EXPR at the head
    let pos = self.tracker.pos();
    let mut passed_ss: Vec<(SyntaxState, TrackerState)> = Vec::new();
    let mut awaiting_ss: Vec<(SyntaxState, TrackerState, NonTerm, Rc<Regex>)> = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => {
          let t = t.clone();
          self.tracker.next();
          t
        }
        None => break,
      };
      eprintln!("READ {:?}", t);
      let mut next_ss: Vec<(SyntaxState, Rc<Regex>)> = Vec::new();
      for s in ss {
        eprintln!("- Advance: s = {:?}", s);
        let conts = self.advance(s.1, &t);
        for cont in conts {
          let ns = s.0.advance();
          eprintln!("- Cont: {:?}", cont);
          match cont {
            AdvanceResult::Pass() => {
              let ts = self.tracker.save_state();
              passed_ss.push((ns, ts))
            }
            AdvanceResult::Remain(r) => next_ss.push((ns, r)),
            AdvanceResult::Await(nt, r) => {
              let ts = self.tracker.save_state();
              awaiting_ss.push((ns, ts, nt, r))
            }
          }
        }
      }
      if next_ss.is_empty() {
        break;
      }
      ss = next_ss;
    }
    eprintln!("----------");
    eprintln!("[Passed]");
    for s in &passed_ss {
      eprintln!("- {:?}", s);
    }
    eprintln!("[Awaiting]");
    for s in &awaiting_ss {
      eprintln!("- {:?}", s);
    }
    eprintln!("----------");

    let passes = passed_ss.len();
    let awaits = awaiting_ss.len();
    if passes == 0 && awaits == 0 {
      self.add_error(pos, "failed to parse (no candidate)");
      return None;
    }
    if passes >= 1 && awaits >= 1 {
      self.add_error(pos, "ambiguous parse (pass/await)");
      return None;
    }
    if passes >= 1 {
      // pass
      assert!(awaits == 0);
      if passes >= 2 {
        self.add_error(pos, "ambiguous parse (pass/pass)");
        return None;
      }
      let syntax = passed_ss[0].0.syntax;

      let ts = passed_ss[0].1;
      self.tracker.restore_state(ts);

      Some(ReadStatus::Pass(syntax))
    } else {
      // await
      assert!(passes == 0);
      let mut fail = false;
      let nt = awaiting_ss[0].2;
      for s in &awaiting_ss {
        if s.2 != nt {
          fail = true;
          break;
        }
      }
      if fail {
        self.add_error(pos, "ambiguous parse (await/await)");
        return None;
      }

      let last_awaits = awaiting_ss.last().unwrap();
      let read = last_awaits.0.read;
      let ts = last_awaits.1;
      self.tracker.restore_state(ts);

      let awaits = awaiting_ss
        .into_iter()
        .filter(|(s, _, _, _)| s.read == read)
        .map(|(s, _, _, k)| SyntaxCont {
          syntax: s.syntax,
          cont: k,
        })
        .collect();
      Some(ReadStatus::Await(nt, awaits))
    }
  }

  fn parse_by_regex(
    &mut self,
    ss: Vec<&'b Syntax>,
    heads: Vec<SyntaxElems<'a>>,
  ) -> Option<(&'b Syntax, Vec<SyntaxElems<'a>>)> {
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
      match self.read_until_nonterm(ss)? {
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
            eprintln!("Next Tokens: {:?}", nexts);
            self.known_ops.extend(nexts);
          }
          eprintln!("Execute Await: {:?}", nt);
          if nt == NonTerm::Expr {
            let p = if is_last {
              &conts[0].syntax.right
            } else {
              &Precedence::Initial
            };
            let e = match self.expr_p(p) {
              Some(e) => e,
              None => unimplemented!(),
            };
            eprintln!("Execute Await: completed ({:?})", e);
            elems.push(SyntaxElems::Expr(e));
          } else {
            let tracker = std::mem::take(&mut self.tracker);
            let mut d = DeclParser {
              tracker: tracker,
              syntax: self.syntax.clone(),
              errors: Vec::new(),
              // TODO: add known_names
            };
            match nt {
              NonTerm::Def => {
                let def = match d.def() {
                  Some(def) => def,
                  None => unimplemented!(),
                };
                elems.push(SyntaxElems::Def(def));
              }
              NonTerm::Decls => {
                let ds = d.decls();
                elems.push(SyntaxElems::Decls(ds));
              }
              _ => unreachable!(),
            }
          }
          self.known_ops = last_known_ops;
          for sc in &conts {
            if deriv::has_eps(&sc.cont) {
              // TODO: need to check: no other syntaxes have eps...
              eprintln!("EPS: {:?}, {:?}", sc.syntax, elems);
              return Some((sc.syntax, elems));
            }
          }
          ss = conts
            .into_iter()
            .map(|s| {
              if deriv::has_eps(&s.cont) {
                todo!();
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

  fn make_syntax(&self, s: &Syntax, elems: Vec<SyntaxElems<'a>>, pos: TokenPos) -> Expr<'a> {
    Expr(ExprF::Base(Base::Syntax(s.clone(), elems)), pos)
  }

  fn expr1(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    eprintln!(
      "- expr1: base_p = {:?}, t = {:?}",
      base_p,
      self.tracker.peek()
    );
    let t = self.tracker.peek()?;
    let pos = t.pos;
    if let Some(lit) = match t.ty {
      TokenType::Natural => Some(Literal::Nat(t.str)),
      TokenType::Rational => Some(Literal::Rat(t.str)),
      TokenType::Char => Some(Literal::Chr(t.str)),
      TokenType::String => Some(Literal::Str(t.str)),
      TokenType::Precedence => unreachable!(),
      TokenType::Ident | TokenType::Reserved => None,
    } {
      self.tracker.next();
      eprintln!("- expr1: lit = {:?}", lit);
      return Some(Expr(ExprF::Base(Base::Lit(lit)), pos));
    }
    if !self.is_opname(t.str) && t.ty == TokenType::Ident {
      // identifier
      let id = Id::new(t.str);
      self.tracker.next();
      eprintln!("- expr1: var = {:?}", id);
      return Some(Expr(ExprF::Base(Base::Var(id)), pos));
    }
    let pres: Vec<&'b Syntax> = self.filter_p(self.syntax.choose_pre(t.str)?, base_p);
    eprintln!("- expr1: pres = {:?}", pres);
    if pres.is_empty() && t.ty == TokenType::Ident {
      let id = Id::new(t.str);
      self.tracker.next();
      return Some(Expr(ExprF::Base(Base::Var(id)), pos));
    }
    let e = self.parse_by_regex(pres, Vec::new())?;
    eprintln!("- expr1 result: {:?}", e);
    return Some(self.make_syntax(e.0, e.1, pos));
  }

  fn expr2(&mut self, base_p: &Precedence, e: Expr<'a>) -> Option<(bool, Expr<'a>)> {
    eprintln!(
      "- expr2: base_p = {:?}, t = {:?}",
      base_p,
      self.tracker.peek()
    );
    let t = match self.tracker.peek() {
      Some(t) => t,
      None => return Some((true, e)),
    };
    let pos = t.pos;
    let state = self.tracker.save_state();
    let e = match self.syntax.choose_ope(t.str) {
      Some(opes) => {
        let opes = self.filter_p(opes, base_p);
        eprintln!("- expr2: opes = {:?}", opes);
        match self.parse_by_regex(opes, vec![SyntaxElems::Expr(e.clone())]) {
          Some(e) => e,
          None => {
            self.tracker.restore_state(state);
            return Some((true, e));
          }
        }
      }
      None => {
        let apps = self.filter_p(&self.syntax.apps, base_p);
        eprintln!("- expr2: apps = {:?}", apps);
        if apps.is_empty() {
          return Some((true, e));
        }
        if apps.len() >= 2 {
          self.add_error(pos, "ambiguous parse (apps)");
          return None;
        }
        let p = apps[0].right.clone();
        match self.expr_p(&p) {
          Some(e2) => {
            eprintln!("- expr2: e2 = {:?}", e2);
            (apps[0], vec![SyntaxElems::Expr(e), SyntaxElems::Expr(e2)])
          }
          None => return Some((true, e)),
        }
      }
    };
    eprintln!("- expr2: e = {:?}", e);
    return Some((false, self.make_syntax(e.0, e.1, pos)));
  }

  fn expr_p(&mut self, base_p: &Precedence) -> Option<Expr<'a>> {
    eprintln!("- EXPR_P: {:?}", base_p);
    let mut e = self.expr1(base_p)?;
    loop {
      match self.expr2(base_p, e)? {
        (true, e) => {
          eprintln!("- EXPR_P result: {:?}", e);
          return Some(e);
        }
        (false, e2) => e = e2,
      }
    }
  }

  fn expr(&mut self) -> Option<Expr<'a>> {
    eprintln!("");
    eprintln!("EXPR: t = {:?}", self.tracker.peek());
    self.expr_p(&Precedence::Initial)
  }
}

struct DeclParser<'a> {
  tracker: Tracker<'a>,
  syntax: SyntaxTable,
  errors: Vec<String>,
}

impl<'a> DeclParser<'a> {
  fn new(tokens: Vec<Token<'a>>) -> Self {
    DeclParser {
      tracker: Tracker::new(tokens),
      syntax: SyntaxTable::default(),
      errors: Vec::new(),
    }
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, decl: {}", pos.to_string(), msg);
    self.errors.push(s);
  }

  fn expect_reserved(&mut self, str: &str) -> Option<()> {
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == str => {
        self.tracker.next();
        return Some(());
      }
      _ => {
        let pos = self.tracker.pos();
        self.add_error(pos, &format!("expected '{}'", str));
        self.tracker.skip_to_next_head();
        return None;
      }
    }
  }

  fn peek_is(&mut self, ty: TokenType) -> Option<()> {
    match self.tracker.peek() {
      Some(t) if t.ty == ty => {
        return Some(());
      }
      _ => {
        let pos = self.tracker.pos();
        self.add_error(
          pos,
          match ty {
            TokenType::Ident => "expected identifier",
            TokenType::String => "expected string",
            _ => unimplemented!(),
          },
        );
        self.tracker.skip_to_next_head();
        return None;
      }
    }
  }

  fn expect_id(&mut self) -> Option<Id<'a>> {
    self.peek_is(TokenType::Ident)?;
    let s = self.tracker.peek().unwrap().str;
    self.tracker.next();
    Some(Id::new(s))
  }

  fn ids(&mut self) -> Vec<Id<'a>> {
    let mut ids = Vec::new();
    loop {
      match self.tracker.peek() {
        Some(t) if t.ty == TokenType::Ident => {
          ids.push(Id::new(t.str));
          self.tracker.next();
        }
        _ => break,
      }
    }
    if ids.is_empty() {
      let last_pos = self.tracker.pos();
      self.add_error(last_pos, "expected identifier list");
    }
    ids
  }

  fn context(&mut self) -> Option<Context<'a>> {
    let id = self.expect_id()?;
    self.expect_reserved(":")?;
    let e = self.expr_or_hole();
    Some(Context::Ref(id, Box::new(e)))
  }

  fn def(&mut self) -> Option<Def<'a>> {
    let outer_indent = self.tracker.save_indent();
    let indent = match self.tracker.peek() {
      Some(t) => t.indent,
      None => return None,
    };
    let id = self.expect_id()?;
    self.tracker.set_indent(indent);
    let def = self.def_rest(id);
    self.tracker.restore_indent(outer_indent);
    def
  }

  fn def_rest(&mut self, name: Id<'a>) -> Option<Def<'a>> {
    let mut args = Vec::new();
    loop {
      let t = match self.tracker.peek() {
        Some(t) => t.clone(),
        None => {
          let pos = self.tracker.end_of_line();
          self.add_error(pos, "expected definition");
          self.tracker.skip_to_next_head();
          return None;
        }
      };
      if t.ty == TokenType::Reserved {
        if t.str == ":" || t.str == "=" {
          break;
        }
        if t.str == "(" || t.str == "{" {
          let vis = if t.str == "(" {
            Vis::Explicit
          } else {
            Vis::Implicit
          };
          let mut hasType = false;
          self.tracker.next();
          let ids = self.ids();
          let arg = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
              hasType = true;
              self.tracker.next();
              let e = self.expr_or_hole();
              (Arg::IdsTy(ids, Box::new(e)), vis)
            }
            _ => (Arg::Ids(ids), vis),
          };
          let (pos, cl) = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Reserved && (t.str == ")" || t.str == "}") => {
              let pos = t.pos;
              let str = t.str;
              self.tracker.next();
              (pos, str)
            }
            _ => {
              let msg = if hasType {
                "expected ')' or '}'"
              } else {
                "expected ')', '}' or ':'"
              };
              let pos = self.tracker.pos();
              self.add_error(pos, msg);
              self.tracker.skip_to_next_head();
              return None;
            }
          };
          let match_cl = match vis {
            Vis::Explicit => ")",
            Vis::Implicit => "}",
          };
          if cl != match_cl {
            self.add_error(pos, &format!("expected '{}', not '{}'", match_cl, cl));
          }
          args.push(arg);
        }
      } else {
        let ids = self.ids();
        args.push((Arg::Ids(ids), Vis::Explicit));
      }
    }
    let ty = match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
        self.tracker.next();
        let e = self.expr_or_hole();
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_reserved("=")?;
    let body = self.expr_or_hole();
    match self.tracker.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ";" => {
        self.tracker.next();
      }
      _ => {}
    }
    Some(Def {
      name,
      args,
      ty,
      body: Box::new(body),
    })
  }

  fn expr(&mut self) -> Option<Expr<'a>> {
    let tracker = std::mem::take(&mut self.tracker);
    let mut e = ExprParser::new(tracker, &self.syntax);
    let res = e.expr();
    eprintln!("EXPR RES: {:?}", res);
    self.tracker = e.tracker;
    self.errors.extend(e.errors);
    res
  }

  fn expr_or_hole(&mut self) -> Expr<'a> {
    let pos = self.tracker.pos();
    match self.expr() {
      Some(e) => e,
      None => Expr(ExprF::Base(Base::Lit(Literal::Hole)), pos),
    }
  }

  fn module(&mut self) -> Option<Module<'a>> {
    let t = self.tracker.peek()?.clone();
    match t.ty {
      TokenType::Reserved => {
        if t.str == "{" {
          // anonymous module
          self.tracker.next();
          let ds = self.decls();
          self.expect_reserved("}")?;
          return Some(Module(ModuleF::Decls(None, Box::new(ds)), t.pos));
        }
      }
      TokenType::Ident => {
        if t.str == "import" {
          // import
          self.tracker.next();
          self.peek_is(TokenType::String)?;
          let s = self.tracker.peek().unwrap();
          return Some(Module(ModuleF::Import(s.str), t.pos));
        }
        // TODO: t is a known module name
        self.tracker.next();
        let t2 = match self.tracker.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "expected module or definition");
            self.tracker.skip_to_next_head();
            return None;
          }
        };
        if t2.ty == TokenType::Reserved && t2.str == "{" {
          // named module
          self.tracker.next();
          let ds = self.decls();
          self.expect_reserved("}")?;
          return Some(Module(
            ModuleF::Decls(Some(Id::new(t.str)), Box::new(ds)),
            t.pos,
          ));
        }
      }
      _ => {}
    }
    None
  }

  fn decl(&mut self) -> Option<Decl<'a>> {
    let t = self.tracker.peek()?.clone();
    match t.ty {
      TokenType::Reserved => {
        if t.str == "[" {
          // context
          self.tracker.next();
          let ctx = self.context()?;
          self.expect_reserved("]")?;
          let d = self.decl()?;
          return Some(Decl(DeclF::Context(ctx, Box::new(d)), t.pos));
        }
        if t.str == "{" {
          // anonymous module
          let m = self.module()?;
          return Some(Decl(DeclF::Module(Access::Keep, Box::new(m)), t.pos));
        }
      }
      TokenType::Ident => {
        if t.str == "syntax" {
          let outer_indent = self.tracker.save_indent();
          self.tracker.set_indent(t.indent);
          // syntax
          self.tracker.next();
          let precs = match self.tracker.peek() {
            Some(t) if t.ty == TokenType::Precedence => match Precedence::parse(t.str) {
              Some(precs) => {
                self.tracker.next();
                precs
              }
              None => {
                self.tracker.restore_indent(outer_indent);
                let pos = self.tracker.pos();
                self.add_error(pos, "failed to parse precedence");
                self.tracker.skip_to_next_head();
                return None;
              }
            },
            _ => (Precedence::Terminal, Precedence::Terminal),
          };
          let regex = unimplemented!();
          let syntax = DeclF::Syntax(Syntax::new(precs.0, precs.1, regex));
          self.tracker.restore_indent(outer_indent);
          return Some(Decl(syntax, t.pos));
        }
        if t.str == "open" {
          // open
          self.tracker.next();
          let m = self.module()?;
          return Some(Decl(DeclF::Module(Access::Open, Box::new(m)), t.pos));
        }
        if t.str == "import" {
          // import
          let m = self.module()?;
          return Some(Decl(DeclF::Module(Access::Keep, Box::new(m)), t.pos));
        }
        // TODO: t is a known module name
        self.tracker.next();
        let t2 = match self.tracker.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "expected module or definition");
            self.tracker.skip_to_next_head();
            return None;
          }
        };
        if t2.ty == TokenType::Reserved && t2.str == "{" {
          // named module
          let mut m = self.module()?; // parse as an anonymous module
          m.0 = match m.0 {
            ModuleF::Decls(None, ds) => ModuleF::Decls(Some(Id::new(t.str)), ds),
            _ => unreachable!(),
          };
          return Some(Decl(DeclF::Module(Access::Keep, Box::new(m)), t.pos));
        }
        // definition
        let outer_indent = self.tracker.save_indent();
        self.tracker.set_indent(t.indent);
        let def = self.def_rest(Id::new(t.str))?;
        self.tracker.restore_indent(outer_indent);
        return Some(Decl(DeclF::Def(def), t.pos));
      }
      _ => {}
    }
    return None;
  }

  fn decls(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    while self.tracker.peek().is_some() {
      let orig_pos = self.tracker.pos();
      if let Some(d) = self.decl() {
        ds.push(d);
      }
      let cur_pos = self.tracker.pos();
      if orig_pos == cur_pos {
        break;
      }
    }
    return ds;
  }

  fn program(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    loop {
      let orig_pos = self.tracker.pos();
      match self.tracker.peek() {
        Some(_) => ds.extend(self.decls()),
        None => break,
      }
      let cur_pos = self.tracker.pos();
      if orig_pos == cur_pos {
        self.add_error(cur_pos, "expected declaration");
        self.tracker.next();
        self.tracker.skip_to_next_head();
      }
    }
    ds
  }
}

pub fn parse<'a>(code: &'a str) -> Result<Vec<Decl<'a>>, Vec<String>> {
  let tokens = match tokenize(code) {
    Ok(tokens) => tokens,
    Err(e) => return Err(e),
  };
  let mut parser = DeclParser::new(tokens);
  let ds = parser.program();
  if let Some(t) = parser.tracker.peek_raw() {
    let t = t.clone();
    parser.add_error(t.pos, "not all tokens consumed");
  }

  if parser.errors.is_empty() {
    Ok(ds)
  } else {
    eprintln!("{}", pretty(&ds));
    Err(parser.errors)
  }
}

#[cfg(test)]
#[test]
fn test() {
  assert!(parse("x = 1 + 2 + 3").is_ok());
}
