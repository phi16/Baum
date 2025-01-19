use crate::mixfix_types::*;
use crate::tokenize::*;
use crate::types::*;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;

pub struct SyntaxDatabase {
  pub prec_map: BTreeMap<Precedence, Vec<Box<SyntaxDecl>>>,
  pub terminals: HashMap<String, Vec<Box<SyntaxDecl>>>,
}

macro_rules! syntax_elem {
  ($s:literal) => {
    Syntax::Token($s.to_string())
  };
  (n) => {
    Syntax::Nat
  };
  (s) => {
    Syntax::Str
  };
  (i) => {
    Syntax::Id
  };
  (e) => {
    Syntax::Expr
  };
  (defs) => {
    Syntax::Defs
  };
  (decls) => {
    Syntax::Decls
  };
  ($i:tt) => {
    $i.clone()
  };
}

macro_rules! syntax_elems {
  ($($e:tt),+) => {
    Syntax::Seqs(vec![$(syntax_elem!($e)),+])
  };
}

#[cfg(test)]
#[test]
fn syntax_macro_test() {
  let elems = syntax_elems!["λ", i, ".", e];
  assert_eq!(
    elems,
    Syntax::Seqs(vec![
      Syntax::Token("λ".to_string()),
      Syntax::Id,
      Syntax::Token(".".to_string()),
      Syntax::Expr,
    ])
  );
}

impl SyntaxDatabase {
  pub fn default() -> Self {
    let mut db = SyntaxDatabase {
      prec_map: BTreeMap::new(),
      terminals: HashMap::new(),
    };
    use PrecEps::*;
    use Precedence::*;
    use Syntax::*;
    let root = &Level(PrecLevel(vec![0]), Zero);
    let appl = &Level(PrecLevel(vec![5]), NegEps);
    let appr = &Level(PrecLevel(vec![5]), PosEps);
    let term = &Terminal;

    let i = Rc::new(Id);
    let e = Rc::new(Expr);
    let ids = Rc::new(Rep1(i.clone()));
    let colon = Rc::new(Token(":".to_string()));

    let lam_args = Sep1(
      Rc::new(Seq(
        ids.clone(),
        Rc::new(May(Rc::new(Seq(colon.clone(), e.clone())))),
      )),
      ",".to_string(),
    );
    let types = Sep1(
      Rc::new(Seq(
        Rc::new(May(Rc::new(Seq(ids.clone(), colon.clone())))),
        e.clone(),
      )),
      ",".to_string(),
    );
    let types0 = Sep0(
      Rc::new(Seq(
        Rc::new(May(Rc::new(Seq(ids.clone(), colon.clone())))),
        e.clone(),
      )),
      ",".to_string(),
    );
    let tuple_vals = Sep2(e.clone(), ",".to_string());
    let sigma_types = Sep0(
      Rc::new(Seq(Rc::new(Seq(ids.clone(), colon.clone())), e.clone())),
      ",".to_string(),
    );
    let id_ty = Seq(i.clone(), colon.clone());
    let id_ty_list = Sep0(Rc::new(id_ty.clone()), ";".to_string()); // TODO: allow ln

    db.def(term, term, syntax_elems!["(", e, ")"]);
    db.def(term, term, syntax_elems!["prim", s]);
    db.def(term, root, syntax_elems!["let", decls, "in", e]);
    db.def(term, root, syntax_elems!["λ", "(", lam_args, ")", e]);
    db.def(term, root, syntax_elems!["λ", "{", lam_args, "}", e]);
    db.def(term, root, syntax_elems!["Π", "(", types, ")", e]);
    db.def(term, root, syntax_elems!["Π", "{", types, "}", e]);
    db.def(appl, appr, syntax_elems![e, e]);
    db.def(appl, term, syntax_elems![e, "{", e, "}"]);
    db.def(term, term, syntax_elems!["(", ")"]);
    db.def(term, term, syntax_elems!["(", tuple_vals, ")"]);
    db.def(term, term, syntax_elems!["{", defs, "}"]);
    db.def(term, term, syntax_elems!["Σ", "(", types0, ")"]);
    db.def(term, term, syntax_elems!["Σ", "{", sigma_types, "}"]);
    db.def(term, root, syntax_elems!["π", "(", n, ")", e]);
    db.def(term, root, syntax_elems!["π", "{", i, "}", e]);
    db.def(term, root, syntax_elems!["σ", "{", defs, "}", e]);
    db.def(
      term,
      term,
      syntax_elems!["μ", "(", id_ty, ")", "{", id_ty_list, "}"],
    );
    db.def(
      term,
      term,
      syntax_elems!["ν", "(", id_ty, ")", "{", id_ty_list, "}"],
    );

    let p21l = &Level(PrecLevel(vec![2, 1]), NegEps);
    let p21r = &Level(PrecLevel(vec![2, 1]), PosEps);
    let p22l = &Level(PrecLevel(vec![2, 2]), NegEps);
    let p22r = &Level(PrecLevel(vec![2, 2]), PosEps);
    let p23 = &Level(PrecLevel(vec![2, 3]), Zero);
    let p24 = &Level(PrecLevel(vec![2, 4]), Zero);
    db.def(p21l, p21r, syntax_elems![e, "+", e]);
    db.def(p22l, p22r, syntax_elems![e, "*", e]);
    db.def(term, p23, syntax_elems!["-", e]);
    db.def(p24, term, syntax_elems![e, "!"]);
    db
  }

  fn def(&mut self, left: &Precedence, right: &Precedence, syntax: Syntax) {
    self.add(SyntaxDecl::new(left.clone(), right.clone(), syntax));
  }

  fn add(&mut self, s: SyntaxDecl) {
    if s.left == Precedence::Terminal {
      let first_token = s.syntax.get_first_token().unwrap();
      let vec = self.terminals.entry(first_token).or_insert(Vec::new());
      vec.push(Box::new(s));
      return;
    }
    let prec = s.left.clone();
    let vec = self.prec_map.entry(prec).or_insert(Vec::new());
    vec.push(Box::new(s));
  }

  pub fn dump(&self) {
    for syn in &self.prec_map {
      println!("{:?}", syn);
    }
    for syn in &self.terminals {
      println!("{:?}", syn);
    }
  }

  pub fn get_candidates(&self, t: &Token) {
    unimplemented!()
  }
}
