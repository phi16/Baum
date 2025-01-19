mod mixfix;
mod mixfix_types;
pub mod parse;
pub mod pretty;
mod tokenize;
pub mod types;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let str = r#"
      x = 1 + 2 * 3
    "#;
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
      }
    }
    eprintln!("");
  }
}
