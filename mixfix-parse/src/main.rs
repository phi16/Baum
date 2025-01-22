#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Epsilon {
  NegEps,
  Zero,
  PosEps,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
  Initial,
  Level(i16, Epsilon),
  Terminal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Syntax {
  Token(char),
  Expr,
}

macro_rules! syntax_elem {
  ($s:literal) => {
    Syntax::Token($s)
  };
  (e) => {
    Syntax::Expr
  };
  ($i:tt) => {
    $i.clone()
  };
}

macro_rules! syntax_elems {
  ($($e:tt),+) => {
    vec![$(syntax_elem!($e)),+]
  };
}

#[derive(Debug, Clone)]
struct SyntaxDecl {
  left: Precedence,
  right: Precedence,
  syntax: Vec<Syntax>,
}

#[derive(Debug, Clone)]
struct SyntaxTable {
  pres: Vec<SyntaxDecl>, // Starts with terminal
  ops: Vec<SyntaxDecl>,  // Starts with e
  apps: Vec<SyntaxDecl>, // e e
}

impl SyntaxTable {
  fn default() -> Self {
    let mut table = Self {
      pres: Vec::new(),
      ops: Vec::new(),
      apps: Vec::new(),
    };
    let term = Precedence::Terminal;
    let root = Precedence::Level(0, Epsilon::Zero);
    let p1l = Precedence::Level(1, Epsilon::PosEps);
    let p1r = Precedence::Level(1, Epsilon::NegEps);
    let p2l = Precedence::Level(2, Epsilon::NegEps);
    let p2r = Precedence::Level(2, Epsilon::PosEps);
    let p3l = Precedence::Level(3, Epsilon::NegEps);
    let p3r = Precedence::Level(3, Epsilon::PosEps);
    let p4l = Precedence::Level(4, Epsilon::PosEps);
    let p4r = Precedence::Level(4, Epsilon::NegEps);
    let p5l = Precedence::Level(5, Epsilon::PosEps);
    let p5r = Precedence::Level(5, Epsilon::NegEps);
    let p6l = Precedence::Level(6, Epsilon::NegEps);
    let p6r = Precedence::Level(6, Epsilon::PosEps);
    let p7l = Precedence::Level(7, Epsilon::NegEps);
    let p7r = Precedence::Level(7, Epsilon::PosEps);
    let p8l = Precedence::Level(8, Epsilon::NegEps);
    let p8r = Precedence::Level(8, Epsilon::PosEps);
    table.add(&term, &term, syntax_elems!['0']);
    table.add(&term, &term, syntax_elems!['1']);
    table.add(&term, &term, syntax_elems!['2']);
    table.add(&term, &term, syntax_elems!['3']);
    table.add(&term, &term, syntax_elems!['4']);
    table.add(&term, &term, syntax_elems!['5']);
    table.add(&term, &term, syntax_elems!['6']);
    table.add(&term, &term, syntax_elems!['7']);
    table.add(&term, &term, syntax_elems!['8']);
    table.add(&term, &term, syntax_elems!['9']);
    table.add(&term, &term, syntax_elems!['(', e, ')']);
    table.add(&term, &root, syntax_elems!['λ', e]);
    table.add(&p1l, &p1r, syntax_elems![e, '-', '>', e]);
    table.add(&p2l, &p2r, syntax_elems![e, '+', e]);
    table.add(&p2l, &p2r, syntax_elems![e, '-', e]);
    table.add(&p3l, &p3r, syntax_elems![e, '*', e]);
    table.add(&p3l, &p3r, syntax_elems![e, '/', e]);
    table.add(&p4l, &p4r, syntax_elems![e, '^', e]);
    table.add(&p5l, &p5r, syntax_elems!['+', e]);
    table.add(&p5l, &p5r, syntax_elems!['-', e]);
    table.add(&p6l, &p6r, syntax_elems![e, '!']);
    table.add(&p6l, &p6r, syntax_elems![e, '[', e, ']']);
    table.add(&p7l, &p7r, syntax_elems![e, e]);
    table.add(&p8l, &p8r, syntax_elems![e, '.', 'x']);
    table
  }

  fn add(&mut self, left: &Precedence, right: &Precedence, syntax: Vec<Syntax>) {
    let s = SyntaxDecl {
      left: left.clone(),
      right: right.clone(),
      syntax: syntax.clone(),
    };
    if syntax.get(0) != Some(&Syntax::Expr) {
      self.pres.push(s);
    } else if syntax.get(1) != Some(&Syntax::Expr) {
      self.ops.push(s);
    } else {
      self.apps.push(s);
    }
  }

  fn choose_pre(&self, c: char, base_p: &Precedence) -> Vec<SyntaxDecl> {
    self
      .pres
      .iter()
      .filter(|decl| *base_p <= decl.left)
      .filter(|decl| match decl.syntax.first() {
        Some(Syntax::Token(s)) if *s == c => true,
        _ => false,
      })
      .cloned()
      .collect()
  }
  fn choose_op(&self, c: char, base_p: &Precedence) -> Vec<SyntaxDecl> {
    self
      .ops
      .iter()
      .filter(|decl| *base_p <= decl.left)
      .filter(|decl| match decl.syntax.iter().nth(1) {
        Some(Syntax::Token(s)) if *s == c => true,
        _ => false,
      })
      .cloned()
      .collect()
  }
  fn choose_app(&self, base_p: &Precedence) -> Vec<SyntaxDecl> {
    self
      .apps
      .iter()
      .filter(|decl| *base_p <= decl.left)
      .cloned()
      .collect()
  }
}

#[derive(Debug, Clone)]
struct Parser {
  input: Vec<char>,
  pos: usize,
  table: SyntaxTable,
  header: String,
}

#[derive(Debug, Clone)]
struct Tree {
  pub syntax: SyntaxDecl,
  pub elems: Vec<Tree>,
}

impl Parser {
  fn peek(&self) -> Option<char> {
    self.input.get(self.pos).cloned()
  }

  fn log(&self, s: &str) {
    // println!("{}- {}", self.header, s);
  }

  fn push(&mut self) {
    self.header.push_str("  ");
  }

  fn pop(&mut self) {
    self.header.pop();
    self.header.pop();
  }

  fn e(&mut self, base_p: &Precedence) -> Option<Tree> {
    // First candidates

    let c = self.peek()?;
    self.log(&format!("choose_pre: {}, {:?}", c, base_p));
    let pres = self.table.choose_pre(c, base_p);
    self.log(&format!("{:?}", pres));
    if pres.is_empty() {
      return None;
    }

    // First parse

    let pos = self.pos;
    let mut trees = Vec::new();
    let mut parsed_pos = 0;
    for s in pres {
      self.pos = pos;
      match self.decl_try(&s, Vec::new()) {
        Some(tree) => trees.push(tree),
        None => continue,
      }
      parsed_pos = self.pos;
    }
    if trees.is_empty() {
      self.pos = pos;
      return None;
    }
    self.pos = parsed_pos;
    if trees.len() != 1 {
      self.log("Ambiguous parse");
    }
    let mut tree = trees.last().unwrap().clone();
    self.log(&format!("e1 result: {}", tree_simp(&tree)));

    loop {
      // Second candidates

      let c = match self.peek() {
        Some(c) => c,
        None => return Some(tree),
      };
      self.log(&format!("choose_op: {}, {:?}", c, base_p));
      let mut ops = self.table.choose_op(c, &base_p);
      if ops.is_empty() {
        ops = self.table.choose_app(&base_p);
      }
      if ops.is_empty() {
        return Some(tree);
      }

      // Second parse

      let pos = self.pos;
      let mut trees = Vec::new();
      let mut parsed_pos = 0;
      for s in ops {
        self.pos = pos;
        match self.decl_try(&s, vec![tree.clone()]) {
          Some(t) => trees.push(t),
          None => continue,
        }
        parsed_pos = self.pos;
      }
      if trees.is_empty() {
        self.pos = pos;
        return Some(tree);
      }
      self.pos = parsed_pos;
      if trees.len() != 1 {
        self.log("Ambiguous parse");
      }
      tree = trees.last().unwrap().clone();
      self.log(&format!("e2 result: {}", tree_simp(&tree)));
    }
  }

  fn decl_try(&mut self, s: &SyntaxDecl, read_elems: Vec<Tree>) -> Option<Tree> {
    self.log(&format!("decl_try: {}", decl_simp(s)));
    self.push();
    let res = self.decl_try_internal(s, read_elems);
    self.pop();
    let s = match &res {
      Some(tree) => format!("decl_try result: {:?} {:?}", decl_simp(s), tree_simp(&tree)),
      None => "decl_try result: None".to_string(),
    };
    self.log(&s);
    res
  }

  fn decl_try_internal(&mut self, s: &SyntaxDecl, read_elems: Vec<Tree>) -> Option<Tree> {
    let skip_count = read_elems.len();
    let mut tree = Tree {
      syntax: s.clone(),
      elems: read_elems,
    };
    let right = s.right.clone();
    let last_index = s.syntax.len() - 1;
    for (i, s) in s.syntax.iter().enumerate().skip(skip_count) {
      let last = i == last_index;
      let elem_base_p = if last {
        right.clone()
      } else {
        Precedence::Initial
      };
      match self.syntax_try(s, &elem_base_p)? {
        Some(t) => tree.elems.push(t),
        None => {}
      }
    }
    Some(tree)
  }

  fn syntax_try(&mut self, s: &Syntax, base_p: &Precedence) -> Option<Option<Tree>> {
    let cur = self.peek()?;
    let res = match s {
      Syntax::Token(c) => {
        if cur != *c {
          return None;
        }
        self.pos += 1;
        None
      }
      Syntax::Expr => {
        let t = self.e(base_p)?;
        Some(t)
      }
    };
    Some(res)
  }
}

fn tree_simp(tree: &Tree) -> String {
  let mut index = 0;
  let mut str = String::new();
  for s in tree.syntax.syntax.iter() {
    match s {
      Syntax::Token(c) => str.push(*c),
      Syntax::Expr => {
        str.push_str("(");
        str.push_str(&tree_simp(&tree.elems[index]));
        index += 1;
        str.push_str(")");
      }
    }
  }
  str
}

fn prec_simp(p: &Precedence) -> String {
  match p {
    Precedence::Initial => "-∞".to_string(),
    Precedence::Level(n, Epsilon::NegEps) => format!("{}-", n),
    Precedence::Level(n, Epsilon::Zero) => format!("{}", n),
    Precedence::Level(n, Epsilon::PosEps) => format!("{}+", n),
    Precedence::Terminal => "∞".to_string(),
  }
}

fn decl_simp(decl: &SyntaxDecl) -> String {
  let mut str = format!("[{}, {}] ", prec_simp(&decl.left), prec_simp(&decl.right));
  for s in decl.syntax.iter() {
    match s {
      Syntax::Token(c) => str.push(*c),
      Syntax::Expr => str.push_str("e"),
    }
  }
  str
}

fn tree_dump(tree: &Tree, indent: &str) {
  println!("{}- {}", indent, decl_simp(&tree.syntax));
  for t in &tree.elems {
    tree_dump(&t, &format!("{}  ", indent));
  }
}

fn parse(e: &str) {
  let table = SyntaxTable::default();
  let mut parser = Parser {
    input: e.chars().filter(|c| !c.is_whitespace()).collect(),
    pos: 0,
    table,
    header: String::new(),
  };
  println!("{:?}", e);
  println!("{:?}", parser.input);
  let tree = parser.e(&Precedence::Initial);
  println!("");
  println!("[ Result ]");
  match tree {
    Some(tree) => {
      println!("{}", tree_simp(&tree));
      tree_dump(&tree, "");
    }
    None => println!("No parse"),
  }
  println!("");
}

fn main() {
  parse("1 + 2 * 3");
  parse("1 * 2 + 3");
  parse("- 1 + 2 + 3 * 4 ! -> 5 ^ 6 ^ 7");
  parse("1 + 2 [ 3 * 4 ] - 5 - 6 ^ 7 ^ 8");
  parse("1.x.x");
  parse("1.x 2.x 3");
  parse("1 + (2 + 3) + λ1 + 2");
  parse("+-1.x 2![3!]!");
}
