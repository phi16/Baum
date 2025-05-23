use baum_lang::types::token::{ErrorPos, TokenPos, TokenRange};

struct Ps(Vec<TokenPos>);

impl Ps {
  fn error_to_string(&self, epos: ErrorPos) -> String {
    match epos {
      ErrorPos::Ix(ix) => {
        let pos = self.0.get(ix.into_inner()).unwrap();
        format!("L{} C{}", pos.line + 1, pos.column + 1)
      }
      ErrorPos::Pos(l, c, _) => format!("L{} C{}", l + 1, c + 1),
      ErrorPos::EoL(l) => format!("End of L{}", l + 1),
      ErrorPos::EoF => "End of file".to_string(),
    }
  }
  fn convert(&self, errors: Vec<(ErrorPos, String)>) -> String {
    let mut s = String::new();
    for (pos, e) in errors {
      let pos = self.error_to_string(pos);
      s.push_str(&format!("{}: {}\n", pos, e));
    }
    s
  }
  fn convert_range(&self, errors: Vec<(TokenRange, String)>) -> String {
    let mut s = String::new();
    for (range, e) in errors {
      let ix = range.begin;
      let pos = self.0.get(ix.into_inner()).unwrap();
      let pos = format!("L{} C{}", pos.line + 1, pos.column + 1);
      s.push_str(&format!("{}: {}\n", pos, e));
    }
    s
  }
}

pub fn run(code: &str) -> Result<(), String> {
  let (tokens, _, errors) = baum_lang::tokenize::tokenize(code);
  let ps = Ps(tokens.iter().map(|t| t.pos).collect::<Vec<_>>());
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("--------");
  let (tree, errors) = baum_lang::parse::parse(tokens);
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("{}", baum_lang::pretty::pretty(&tree));
  eprintln!("--------");
  let (front, errors) = baum_lang::convert::convert(&tree);
  if !errors.is_empty() {
    return Err(ps.convert(errors));
  }
  eprintln!("{}", baum_front::pretty::pretty(&front));
  eprintln!("--------");
  let (core, errors) = baum_front::convert::convert(front.clone());
  eprintln!("{}", baum_core::pretty::pretty(&core));
  if !errors.is_empty() {
    return Err(ps.convert_range(errors));
  }
  eprintln!("--------");
  let entrypoint = "main";
  let errors = baum_core::check::check_main(core.clone(), entrypoint);
  match errors {
    Ok(m) => {
      eprintln!("[Passed]");
      eprintln!("--------");
      if let Some(m) = m {
        let g = baum_rt::convert::convert(&m);
        baum_rt::run::run(&g);
      } else {
        eprintln!("End of file: entrypoint {:?} not found", entrypoint);
      }
    }
    Err(errors) => {
      eprintln!("--------");
      return Err(ps.convert_range(errors));
    }
  }
  Ok(())
}

fn main() {
  let code = include_str!("../examples/test.baum");
  match run(code) {
    Ok(_) => {}
    Err(e) => {
      eprintln!("{}", e);
    }
  }
  eprintln!();
}
