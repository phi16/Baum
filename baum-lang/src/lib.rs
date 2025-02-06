mod decl;
mod expr;
pub mod parse;
pub mod pretty;
mod syntax;
mod tokenize;
mod types;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let str = include_str!("../test.baum");
    // let str = include_str!("../test2.baum");
    let res = parse::parse(str);
    eprintln!("");
    match res {
      Ok(ds) => {
        eprintln!("[Ok]");
        eprintln!("{}", pretty::pretty(&ds));
      }
      Err(errs) => {
        eprintln!("[Err]");
        for err in errs {
          eprintln!("{}", err);
        }
        assert!(false);
      }
    }
  }
}
