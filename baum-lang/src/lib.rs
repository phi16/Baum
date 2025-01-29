pub mod parse;
mod parse_decl;
mod parse_expr;
pub mod pretty;
mod tokenize;
mod types;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let str = r#"
      f = - 2
      u = Σ(x y: Nat, Nat, z: Q x y, P x y z)
      v = Σ(x y: Nat, Nat, z: Q x y, P x y z,)
      x = 1 + 2 * 3
      y = 1 * 2 + 3
      z = 1 + [ 1 + 2 | 3 + 4 ] * 5
      w = 1 + [ 1 + 2 ? 3 + 4 ] * 5
      a = prim "a"
      b = g λ() x
      b = g λ(a) x
      b = g λ(a: b) x
      b = Σ() λ(a: b c, ) λ{x: y} x
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
        assert!(false);
      }
    }
    eprintln!("");
    assert!(false);
  }
}
