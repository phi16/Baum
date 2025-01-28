use crate::parse_expr::ExprParser;
use crate::types::parse::*;

pub struct DeclParser<'a> {
  pub tracker: Tracker<'a>,
  pub syntax: SyntaxTable,
  pub errors: Vec<String>,
}

impl<'a> DeclParser<'a> {
  pub fn new(tokens: Vec<Token<'a>>) -> Self {
    DeclParser {
      tracker: Tracker::new(tokens),
      syntax: SyntaxTable::default(),
      errors: Vec::new(),
    }
  }

  pub fn add_error(&mut self, pos: TokenPos, msg: &str) {
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

  pub fn def(&mut self) -> Option<Def<'a>> {
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

  pub fn decls(&mut self) -> Vec<Decl<'a>> {
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

  pub fn program(&mut self) -> Vec<Decl<'a>> {
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
