use crate::tokenize::*;
use crate::types::*;

use core::iter::Peekable;
use std::collections::BTreeMap;
use std::vec::IntoIter;

struct SyntaxDatabase {
  precMap: BTreeMap<Precedence, Vec<Box<Syntax>>>,
}

macro_rules! syntax_elem {
  ($s:literal) => {
    SyntaxElement::Str($s.to_string())
  };
  (_) => {
    SyntaxElement::Special
  };
  ($i:tt) => {
    SyntaxElement::Expr(stringify!($i).to_string())
  };
}

macro_rules! syntax_elems {
  ($($e:tt),+) => {
    vec![$(syntax_elem!($e)),+]
  };
}

#[cfg(test)]
#[test]
fn syntax_macro_test() {
  let elems = syntax_elems!["λ", x, ".", e, _];
  assert_eq!(
    elems,
    vec![
      SyntaxElement::Str("λ".to_string()),
      SyntaxElement::Expr("x".to_string()),
      SyntaxElement::Str(".".to_string()),
      SyntaxElement::Expr("e".to_string()),
      SyntaxElement::Special
    ]
  );
}

impl SyntaxDatabase {
  fn default() -> Self {
    let mut db = SyntaxDatabase {
      precMap: BTreeMap::new(),
    };
    let root = vec![1, 0];
    db.def(&root, Fixity::Prefix, syntax_elems!["λ", "(", _, ")", e]);
    db.def(&root, Fixity::Prefix, syntax_elems!["λ", "{", _, "}", e]);
    db.def(&root, Fixity::Prefix, syntax_elems!["Π", "(", _, ")", e]);
    db.def(&root, Fixity::Prefix, syntax_elems!["Π", "{", _, "}", e]);
    db.def(&root, Fixity::Closed, syntax_elems!["(", _, ")"]);
    db.def(&root, Fixity::Closed, syntax_elems!["{", _, "}"]);
    db.def(&root, Fixity::Closed, syntax_elems!["Σ", "(", _, ")"]);
    db.def(&root, Fixity::Closed, syntax_elems!["Σ", "{", _, "}"]);
    db.def(&root, Fixity::Prefix, syntax_elems!["σ", "{", _, "}", e]);
    db.def(&root, Fixity::Closed, syntax_elems!["μ", "(", _, ")", _]);
    db.def(&root, Fixity::Closed, syntax_elems!["ν", "(", _, ")", _]);
    db.def(&root, Fixity::Prefix, syntax_elems!["let", _, "in", e]);
    db.def(&vec![1, 1], Fixity::Postfix, syntax_elems![e, "where", _]);
    db.def(&vec![4, 0], Fixity::InfixL, syntax_elems![e, e]);
    db.def(&vec![4, 1], Fixity::Postfix, syntax_elems![e, ".", _]);

    db.def(&vec![2, 1], Fixity::InfixL, syntax_elems![e, "+", e]);
    db.def(&vec![2, 2], Fixity::InfixL, syntax_elems![e, "*", e]);
    db.def(&vec![2, 3], Fixity::Prefix, syntax_elems!["-", e]);
    db.def(&vec![2, 4], Fixity::Postfix, syntax_elems![e, "!"]);
    db
  }

  fn def(&mut self, prec: &Precedence, fixity: Fixity, elems: Vec<SyntaxElement>) {
    self.add(Syntax::new(prec.clone(), fixity, elems));
  }

  fn add(&mut self, s: Syntax) {
    let prec = s.prec.clone();
    let vec = self.precMap.entry(prec).or_insert(Vec::new());
    vec.push(Box::new(s));
  }
}

struct Parser<'a> {
  iter: Peekable<IntoIter<Token<'a>>>,
  indent: Indent,
  last_eol: TokenPos,
  errors: Vec<String>,
  syntax: SyntaxDatabase,
}

impl<'a> Parser<'a> {
  fn new(tokens: IntoIter<Token<'a>>) -> Self {
    Parser {
      iter: tokens.peekable(),
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

  fn peek(&mut self) -> Option<&Token<'a>> {
    self.iter.peek().filter(|t| self.indent < t.indent)
  }

  fn next(&mut self) -> Option<Token<'a>> {
    self.last_eol = match self.iter.peek() {
      Some(t) => match t.pos {
        TokenPos::Pos(l, _) => TokenPos::EoL(l),
        _ => unreachable!(),
      },
      _ => TokenPos::EoF,
    };
    self.iter.next()
  }

  fn skip_to_next_head(&mut self) {
    while let Some(t) = self.peek() {
      match t.indent {
        Indent::Head(_) => break,
        _ => self.next(),
      };
    }
  }

  fn end_of_line(&mut self) -> TokenPos {
    self.last_eol
  }

  fn pos(&mut self) -> TokenPos {
    let last_pos = self.end_of_line();
    self.peek().map_or(last_pos, |t| t.pos)
  }

  fn expect_symbol(&mut self, str: &str) -> Option<()> {
    match self.peek() {
      Some(t) if t.ty == TokenType::Symbol && t.str == str => {
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

  fn expect_id(&mut self) -> Option<Id<'a>> {
    match self.peek() {
      Some(t) if t.ty == TokenType::Ident => {
        let s = t.str;
        self.next();
        return Some(Id::new(s));
      }
      _ => {
        let pos = self.pos();
        self.add_error(pos, "decl: expected identifier");
        self.skip_to_next_head();
        return None;
      }
    }
  }

  fn prim(&mut self) -> Option<Expr<'a>> {
    let t = self.peek()?;
    let pos = t.pos;
    let lit = match t.ty {
      TokenType::Number => Literal::Num(t.str),
      TokenType::Char => Literal::Chr(t.str),
      TokenType::String => Literal::Str(t.str),
      TokenType::Keyword => {
        if t.str == "prim" {
          self.next();
          match self.peek() {
            Some(t) if t.ty == TokenType::String => Literal::Prim(t.str),
            _ => {
              let pos = self.pos();
              self.add_error(pos, "prim: expected primitive name");
              Literal::Prim("")
            }
          }
        } else if t.str == "_" {
          Literal::Hole
        } else {
          return None;
        }
      }
      _ => return None,
    };
    self.next();
    return Some(Expr(ExprF::Lit(lit), pos));
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
    self.expect_symbol(":")?;
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
      if t.ty == TokenType::Symbol {
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
            Some(t) if t.ty == TokenType::Symbol && t.str == ":" => {
              hasType = true;
              self.next();
              let e = self.expr_or_hole();
              (vis, Arg::IdTy(ids, Box::new(e)))
            }
            _ => (vis, Arg::Id(ids)),
          };
          let (pos, cl) = match self.peek() {
            Some(t) if t.ty == TokenType::Symbol && (t.str == ")" || t.str == "}") => {
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
        args.push((Vis::Explicit, Arg::Id(ids)));
      }
    }
    let ty = match self.peek() {
      Some(t) if t.ty == TokenType::Symbol && t.str == ":" => {
        self.next();
        let e = self.expr_or_hole();
        Some(Box::new(e))
      }
      _ => None,
    };
    self.expect_symbol("=")?;
    let body = self.expr_or_hole();
    match self.peek() {
      Some(t) if t.ty == TokenType::Symbol && t.str == ";" => {
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
    // P' = Closed
    //    | P Non P
    //    | (Prefix | P Right)+ P
    //    | P (Postfix | Left P)+

    // P' = Closed
    //    | Prefix (Prefix* P) (Right Prefix* P)*
    //    | P Non P
    //    | P Right (Prefix* P) (Right Prefix* P)*
    //    | P Left P (Postfix | Left P)*
    //    | P Postfix (Postfix | Left P)*
    //    | Db

    for syn in &self.syntax.precMap {
      println!("{:?}", syn);
    }
    unimplemented!()

    // self.prim()
  }

  fn expr_or_hole(&mut self) -> Expr<'a> {
    let pos = self.pos();
    match self.expr() {
      Some(e) => e,
      None => Expr(ExprF::Lit(Literal::Hole), pos),
    }
  }

  fn decl(&mut self) -> Option<Decl<'a>> {
    let t = self.peek()?.clone();
    match t.ty {
      TokenType::Keyword => {
        if t.str == "syntax" {
          let outer_indent = self.indent;
          self.set_indent(t.indent);
          // syntax
          unimplemented!();
          self.indent = outer_indent;
          unimplemented!()
        }
        if t.str == "open" {
          // open
          unimplemented!()
        }
      }
      TokenType::Symbol => {
        if t.str == "[" {
          // context
          self.next();
          let ctx = self.context()?;
          self.expect_symbol("]")?;
          let d = self.decl()?;
          return Some(Decl(DeclF::Context(ctx, Box::new(d)), t.pos));
        }
        if t.str == "{" {
          // anonymous module
          self.next();
          let ds = self.decls();
          self.expect_symbol("}")?;
          return Some(Decl(DeclF::Module(None, Box::new(ds)), t.pos));
        }
      }
      TokenType::Ident => {
        self.next();
        let t2 = match self.peek() {
          Some(t) => t.clone(),
          None => {
            self.add_error(t.pos, "decl: expected module or definition");
            self.skip_to_next_head();
            return None;
          }
        };
        if t2.ty == TokenType::Symbol && t2.str == "{" {
          // named module
          self.next();
          let ds = self.decls();
          self.expect_symbol("}")?;
          return Some(Decl(
            DeclF::Module(Some(Id::new(t.str)), Box::new(ds)),
            t.pos,
          ));
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
  for t in &tokens {
    eprintln!("{:?}", t);
  }
  let mut parser = Parser::new(tokens.into_iter());
  let ds = parser.program();
  if let Some(t) = parser.iter.peek() {
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
