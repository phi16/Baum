pub mod parse;
mod tokenize;
pub mod types;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let str = r#"
      x = 1
      [x: 1] z = 1
      f {
        p = 1
      }
      g {
      }
      f { a = 2; b = 5 }
      f { a = 2
          b' = _
          c_f = 8 }
      g {}
      [x: 1] {
        z = 1
        w = "m\no \" \"chi"
        x a = 'abb'
        x (p: 1) = 1
        x m o (p q: 1) {r s: 1} t u: 1 = 1
        y = 1
      }
      y = 1
      a
        b 
          c = 
        1
    "#;
    let res = parse::parse(str);
    eprintln!("");
    match res {
      Ok(ds) => {
        eprintln!("[Ok]");
        eprintln!("{}", types::pretty(&ds));
      }
      Err(errs) => {
        eprintln!("[Err]");
        for err in errs {
          eprintln!("{}", err);
        }
      }
    }
    eprintln!("");
    assert!(false);
  }
}
