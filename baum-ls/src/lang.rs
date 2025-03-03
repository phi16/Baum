use baum_lang::types::{self, token::TokenLoc};

#[derive(Debug, Clone)]
pub enum TokenType {
  Def,
  Ext,
  Bind,
  Unknown,
  Prop,
  Module,

  SynDefIdent,
  SynDefExpr,
  BuiltinSyntax,
  UserSyntax,
  Keyword,
  Symbol,
  Precedence,

  String,
  Number,

  Comment,
}

pub struct Diagnostic {
  pub begin_line: u32,
  pub begin_column: u32,
  pub end_line: u32,
  pub end_column: u32,
  pub message: String,
}

pub struct TokenData {
  pub line: u32,
  pub column: u32,
  pub length: u32,
  pub token_type: TokenType,
}

struct Context {
  tokens: Vec<TokenData>,
}

impl Context {
  fn new(tokens: Vec<TokenData>) -> Self {
    Self { tokens }
  }

  fn into_inner(self) -> Vec<TokenData> {
    self.tokens
  }

  fn mark_as(&mut self, loc: &TokenLoc, offset: i32, t: TokenType) {
    let index = loc.clone().into_inner() as i32 + offset;
    let d = match self.tokens.get_mut(index as usize) {
      Some(token) => token,
      None => return,
    };
    d.token_type = t;
  }

  fn e(&mut self, e: &types::tree::Expr) {
    let loc = &e.1.begin;
    use types::tree_base::*;
    match &e.0 {
      ExprF::Hole => {
        self.mark_as(loc, 0, TokenType::BuiltinSyntax);
      }
      ExprF::Var(_) => {
        self.mark_as(loc, 0, TokenType::Unknown);
      }
      ExprF::Mod(mod_name) => {
        for i in 0..mod_name.len() {
          self.mark_as(loc, i as i32 * 2, TokenType::Module);
        }
      }
      ExprF::Ext(mod_name, _) => {
        for i in 0..mod_name.len() {
          self.mark_as(loc, i as i32 * 2, TokenType::Module);
        }
        self.mark_as(loc, mod_name.len() as i32 * 2, TokenType::Ext);
      }
      ExprF::Let(ds, e) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.ds(&ds);
        self.mark_as(&e.1.begin, -1, TokenType::Keyword);
        self.e(&e);
      }
      ExprF::Syntax(mod_name, sid, elems) => {
        let mut first_token = true;
        use types::tree::SyntaxId;
        let syn_type = match sid {
          SyntaxId::User(_) => TokenType::UserSyntax,
          _ => TokenType::BuiltinSyntax,
        };
        for e in elems {
          let loc = &e.1.begin;
          match &e.0 {
            SynElemF::Token(_) => {
              if first_token {
                first_token = false;
                for i in 0..mod_name.len() {
                  self.mark_as(
                    loc,
                    (i as i32 - mod_name.len() as i32) * 2,
                    TokenType::Module,
                  );
                }
              }
              self.mark_as(loc, 0, syn_type.clone());
            }
            SynElemF::Ident(_) => {
              self.mark_as(
                loc,
                0,
                match sid {
                  SyntaxId::Prop => TokenType::Prop,
                  _ => TokenType::Bind,
                },
              );
            }
            SynElemF::Dec(_) | SynElemF::Num(_) => {
              self.mark_as(
                loc,
                0,
                match sid {
                  SyntaxId::Dec | SyntaxId::Num => TokenType::Number,
                  SyntaxId::Proj => TokenType::Prop,
                  _ => TokenType::UserSyntax,
                },
              );
            }
            SynElemF::Chr(_) | SynElemF::Str(_) => {
              self.mark_as(loc, 0, TokenType::String);
            }
            SynElemF::Expr(e) => {
              self.e(&e);
            }
          }
        }
      }
    }
  }

  fn mr(&mut self, mr: &types::tree::ModRef) {
    let loc = &mr.1.begin;
    use types::tree_base::*;
    match &mr.0 {
      ModRefF::Import(_) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.mark_as(loc, 1, TokenType::String);
      }
      ModRefF::App(mod_name, args) => {
        for i in 0..mod_name.len() {
          self.mark_as(loc, i as i32 * 2, TokenType::Module);
        }
        for (_, e) in args {
          self.e(e);
        }
      }
    }
  }

  fn d(&mut self, d: &types::tree::Decl) {
    let loc = &d.1.begin;
    use types::tree::Arg;
    use types::tree_base::*;
    match &d.0 {
      DeclF::Local(ds) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.ds(&ds)
      }
      DeclF::Mod(md, body) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.mark_as(loc, 1, TokenType::Module);
        for Arg(arg, loc) in &md.params {
          for i in 0..arg.1.len() {
            self.mark_as(loc, i as i32, TokenType::Bind);
          }
          if let Some(e) = &arg.2 {
            self.e(e);
          }
        }
        match &body {
          ModBodyF::Decls(ds) => {
            self.ds(ds);
          }
          ModBodyF::Ref(mr) => {
            self.mr(mr);
          }
        }
      }
      DeclF::Open(mr) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.mr(mr);
      }
      DeclF::Use(mr) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        self.mr(mr);
      }
      DeclF::Def(def) => {
        self.mark_as(loc, 0, TokenType::Def);
        for Arg(arg, loc) in &def.args {
          for i in 0..arg.1.len() {
            self.mark_as(loc, i as i32, TokenType::Bind);
          }
          if let Some(e) = &arg.2 {
            self.e(e);
          }
        }
        if let Some(e) = &def.ty {
          self.e(e);
        }
        self.e(&def.body);
      }
      DeclF::Syntax(_, prec, syndefs, e) => {
        self.mark_as(loc, 0, TokenType::Keyword);
        if let Some(_) = prec {
          self.mark_as(loc, 1, TokenType::Precedence);
        }
        for syndef in syndefs {
          let t = match syndef.0 {
            SynDefF::Token(_) => TokenType::UserSyntax,
            SynDefF::Ident(_) => TokenType::SynDefIdent,
            SynDefF::Expr(_) => TokenType::SynDefExpr,
          };
          self.mark_as(&syndef.1, 0, t);
        }
        self.e(e);
      }
    }
  }

  fn ds(&mut self, ds: &Vec<types::tree::Decl>) {
    for d in ds {
      self.d(d);
    }
  }
}

pub fn tokenize_example(code: &str) -> (Vec<TokenData>, Vec<Diagnostic>) {
  let lines = code.lines().collect::<Vec<_>>();

  let mut diags = Vec::new();
  let mut add_diag = |pos: &types::token::ErrorPos, msg: &str| {
    use types::token::*;
    let (begin_line, begin_column) = match pos {
      ErrorPos::Pos(line, column) => (*line, *column),
      ErrorPos::EoL(line) => (*line, lines[*line as usize].len() as u32),
      ErrorPos::EoF => (lines.len() as u32, 0),
    };
    let end_line = begin_line;
    let end_column = begin_column + 1;
    diags.push(Diagnostic {
      begin_line,
      begin_column,
      end_line,
      end_column,
      message: msg.to_string(),
    });
  };

  let (tokens, comments, errors) = baum_lang::tokenize::tokenize(code);
  let mut x = Context::new(
    tokens
      .iter()
      .map(|t| {
        let column = t.pos.column;
        let length = t.pos.length;
        let mut l = lines.get(t.pos.line as usize).unwrap().chars();
        let utf16_column = l
          .by_ref()
          .take(column as usize)
          .map(|c| c.len_utf16())
          .sum::<usize>() as u32;
        let utf16_length = l
          .take(length as usize)
          .map(|c| c.len_utf16())
          .sum::<usize>() as u32;
        TokenData {
          line: t.pos.line,
          column: utf16_column,
          length: utf16_length,
          token_type: TokenType::Unknown,
        }
      })
      .collect::<Vec<_>>(),
  );
  tokens.iter().enumerate().for_each(|(loc, t)| {
    x.mark_as(
      &TokenLoc::new(loc),
      0,
      match t.ty {
        types::token::TokenType::Ident => match t.str {
          "syntax" | "local" | "module" | "let" | "in" => TokenType::Keyword,
          _ => TokenType::Unknown,
        },
        types::token::TokenType::DecNat => TokenType::Number,
        types::token::TokenType::Number => TokenType::Number,
        types::token::TokenType::Char => TokenType::String,
        types::token::TokenType::String => TokenType::String,
        types::token::TokenType::Precedence => TokenType::Precedence,
        types::token::TokenType::Reserved => TokenType::Symbol,
      },
    );
  });
  for (pos, e) in &errors {
    add_diag(pos, e);
  }
  if errors.is_empty() {
    let (tree, errors) = baum_lang::parse::parse(tokens);
    x.ds(&tree);
    for (pos, e) in &errors {
      add_diag(pos, e);
    }
    if errors.is_empty() {
      let (front, errors) = baum_lang::convert::convert(&tree);
      // TODO: use front to coloring
      for (pos, e) in &errors {
        add_diag(pos, e);
      }
    }
  }
  let comments_iter = comments.into_iter().map(|(line, column)| {
    let mut l = lines.get(line as usize).unwrap().chars();
    let utf16_column = l
      .by_ref()
      .take(column as usize)
      .map(|c| c.len_utf16())
      .sum::<usize>() as u32;
    let utf16_length = l.map(|c| c.len_utf16()).sum::<usize>() as u32;
    TokenData {
      line,
      column: utf16_column,
      length: utf16_length,
      token_type: TokenType::Comment,
    }
  });
  let mut tokens_iter = x.into_inner().into_iter().peekable();
  let mut comments_iter = comments_iter.peekable();
  let mut res = Vec::new();
  loop {
    match (tokens_iter.peek(), comments_iter.peek()) {
      (Some(t), Some(c)) => {
        let token_first = if t.line < c.line {
          true
        } else if t.line > c.line {
          false
        } else {
          t.column < c.column
        };
        if token_first {
          res.push(tokens_iter.next().unwrap());
        } else {
          res.push(comments_iter.next().unwrap());
        }
      }
      (Some(_), None) => {
        res.extend(tokens_iter);
        break;
      }
      (None, Some(_)) => {
        res.extend(comments_iter);
        break;
      }
      (None, None) => {
        break;
      }
    }
  }
  (res, diags)
}
