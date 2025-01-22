use crate::mixfix::*;
use crate::mixfix_types::*;
use crate::pretty::*;
use crate::tokenize::*;
use crate::types::*;

struct Parser<'a> {
  token_list: Vec<Token<'a>>,
  current_pos: usize,
  indent: Indent,
  last_eol: TokenPos,
  errors: Vec<String>,
  syntax: SyntaxDatabase,
}

impl<'a> Parser<'a> {
  fn new(tokens: Vec<Token<'a>>) -> Self {
    Parser {
      token_list: tokens,
      current_pos: 0,
      indent: Indent::Base,
      last_eol: TokenPos::EoL(0),
      errors: Vec::new(),
      syntax: SyntaxDatabase::default(),
    }
  }

  fn add_error(&mut self, pos: TokenPos, msg: &str) {
    let s = format!("{}, {}", pos.to_string(), msg);
    self.errors.push(s);
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

  fn expect_reserved(&mut self, str: &str) -> Option<()> {
    match self.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == str => {
        self.next();
        return Some(());
      }
      _ => {
        let pos = self.pos();
        self.add_error(pos, &format!("decl: expected '{}'", str));
        self.skip_to_next_head();
        return None;
      }
    }
  }

  fn peek_is(&mut self, tt: TokenType) -> Option<()> {
    match self.peek() {
      Some(t) if t.ty == tt => {
        return Some(());
      }
      _ => {
        let pos = self.pos();
        self.add_error(
          pos,
          match tt {
            TokenType::Ident => "decl: expected identifier",
            TokenType::String => "decl: expected string",
            _ => unimplemented!(),
          },
        );
        self.skip_to_next_head();
        return None;
      }
    }
  }

  fn expect_id(&mut self) -> Option<Id<'a>> {
    self.peek_is(TokenType::Ident)?;
    let s = self.peek().unwrap().str;
    self.next();
    Some(Id::new(s))
  }

  fn ids(&mut self) -> Vec<Id<'a>> {
    let mut ids = Vec::new();
    loop {
      match self.peek() {
        Some(t) if t.ty == TokenType::Ident => {
          ids.push(Id::new(t.str));
          self.next();
        }
        _ => break,
      }
    }
    if ids.is_empty() {
      let last_pos = self.pos();
      self.add_error(last_pos, "decl: expected identifier list");
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
    let outer_indent = self.indent;
    let indent = match self.peek() {
      Some(t) => t.indent,
      None => return None,
    };
    let id = self.expect_id()?;
    self.set_indent(indent);
    let def = self.def_rest(id);
    self.indent = outer_indent;
    def
  }

  fn def_rest(&mut self, name: Id<'a>) -> Option<Def<'a>> {
    let mut args = Vec::new();
    loop {
      let t = match self.peek() {
        Some(t) => t.clone(),
        None => {
          let pos = self.end_of_line();
          self.add_error(pos, "decl: expected definition");
          self.skip_to_next_head();
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
          self.next();
          let ids = self.ids();
          let arg = match self.peek() {
            Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
              hasType = true;
              self.next();
              let e = self.expr_or_hole();
              (Arg::IdsTy(ids, Box::new(e)), vis)
            }
            _ => (Arg::Ids(ids), vis),
          };
          let (pos, cl) = match self.peek() {
            Some(t) if t.ty == TokenType::Reserved && (t.str == ")" || t.str == "}") => {
              let pos = t.pos;
              let str = t.str;
              self.next();
              (pos, str)
            }
            _ => {
              let msg = if hasType {
                "decl: expected ')' or '}'"
              } else {
                "decl: expected ')', '}' or ':'"
              };
              let pos = self.pos();
              self.add_error(pos, msg);
              self.skip_to_next_head();
              return None;
            }
          };
          let match_cl = match vis {
            Vis::Explicit => ")",
            Vis::Implicit => "}",
          };
          if cl != match_cl {
            self.add_error(pos, &format!("decl: expected '{}', not '{}'", match_cl, cl));
          }
          args.push(arg);
        }
      } else {
        let ids = self.ids();
        args.push((Arg::Ids(ids), Vis::Explicit));
      }
    }
    let ty = match self.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ":" => {
        self.next();
        let e = self.expr_or_hole();
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_reserved("=")?;
    let body = self.expr_or_hole();
    match self.peek() {
      Some(t) if t.ty == TokenType::Reserved && t.str == ";" => {
        self.next();
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
    let t = self.peek()?;
    println!("expr from: {:?}", &self.peek());
    self.syntax.dump();
    // self.syntax.get_candidates(t);
    unimplemented!()
  }

  fn expr_or_hole(&mut self) -> Expr<'a> {
    let pos = self.pos();
    match self.expr() {
      Some(e) => e,
      None => Expr(ExprF::Base(Base::Lit(Literal::Hole)), pos),
    }
  }

  fn module(&mut self) -> Option<Module<'a>> {
    let t = self.peek()?.clone();
    match t.ty {
      TokenType::Reserved => {
        if t.str == "{" {
          // anonymous module
          self.next();
          let ds = self.decls();
          self.expect_reserved("}")?;
          return Some(Module(ModuleF::Decls(None, Box::new(ds)), t.pos));
        }
      }
      TokenType::Ident => {
        if t.str == "import" {
          // import
          self.next();
          self.peek_is(TokenType::String)?;
          let s = self.peek().unwrap();
          return Some(Module(ModuleF::Import(s.str), t.pos));
        }
        // TODO: t is a known module name
        self.next();
        let t2 = match self.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "decl: expected module or definition");
            self.skip_to_next_head();
            return None;
          }
        };
        if t2.ty == TokenType::Reserved && t2.str == "{" {
          // named module
          self.next();
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
    let t = self.peek()?.clone();
    match t.ty {
      TokenType::Reserved => {
        if t.str == "[" {
          // context
          self.next();
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
          let outer_indent = self.indent;
          self.set_indent(t.indent);
          // syntax
          self.next();
          let precs = match self.peek() {
            Some(t) if t.ty == TokenType::Precedence => match Precedence::parse(t.str) {
              Some(precs) => {
                self.next();
                precs
              }
              None => {
                let pos = t.pos;
                self.add_error(pos, "decl: failed to parse precedence");
                self.skip_to_next_head();
                return None;
              }
            },
            _ => (Precedence::Terminal, Precedence::Terminal),
          };
          let regex = unimplemented!();
          let syntax = DeclF::Syntax(Syntax::new(precs.0, precs.1, regex));
          self.indent = outer_indent;
          return Some(Decl(syntax, t.pos));
        }
        if t.str == "open" {
          // open
          self.next();
          let m = self.module()?;
          return Some(Decl(DeclF::Module(Access::Open, Box::new(m)), t.pos));
        }
        if t.str == "import" {
          // import
          let m = self.module()?;
          return Some(Decl(DeclF::Module(Access::Keep, Box::new(m)), t.pos));
        }
        // TODO: t is a known module name
        self.next();
        let t2 = match self.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "decl: expected module or definition");
            self.skip_to_next_head();
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
        let outer_indent = self.indent;
        self.set_indent(t.indent);
        let def = self.def_rest(Id::new(t.str))?;
        self.indent = outer_indent;
        return Some(Decl(DeclF::Def(def), t.pos));
      }
      _ => {}
    }
    return None;
  }

  fn decls(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    while self.peek().is_some() {
      let orig_pos = self.pos();
      if let Some(d) = self.decl() {
        ds.push(d);
      }
      let cur_pos = self.pos();
      if orig_pos == cur_pos {
        break;
      }
    }
    return ds;
  }

  fn program(&mut self) -> Vec<Decl<'a>> {
    let mut ds = Vec::new();
    loop {
      let orig_pos = self.pos();
      match self.peek() {
        Some(_) => ds.extend(self.decls()),
        None => break,
      }
      let cur_pos = self.pos();
      if orig_pos == cur_pos {
        self.add_error(cur_pos, "decl: expected declaration");
        self.next();
        self.skip_to_next_head();
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
  let mut parser = Parser::new(tokens);
  let ds = parser.program();
  if let Some(t) = parser.peek_raw() {
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
