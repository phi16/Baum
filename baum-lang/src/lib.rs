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
      y = 1
    "#;
    let res = parse::parse(str);
    eprintln!("Final Result: {:?}", res);
    assert!(false);
  }
}
