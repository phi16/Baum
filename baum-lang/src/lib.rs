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
      u = A.x
      module A = {
        x = 1
      }
      module C (x: A) {y: B} = {
        use A
        open B c d
        use import "mochi"
        local local open import "mochi"
        x = 1
        local {
          y = 2
        }
        module E = D x y z (a b) {x} {y z} A.p.r B.q
        local module D {r: s t} (x: a) = {
          local open A
        }
      }
      k = let module X = {
          x = 1
        } in X.x
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
      g = 1
      k = let x = 1; y = 2 in t
      g = k
      ot = Σ{x: Π(Nat) Nat, y: Nat}
      o = { x v: Nat = 1, y = 2 }
      v = π(1) x
      v = π(1 + 2) x -- fail
      w = π{prop} x
      x = (x) + () + (x,y) + (x,y,z)
      y = {}
      z = x {y}
      z = x {y = 2} -- hmm...
      y = {y = 1} + 2
      y = {y} + 1 -- fail
      u = 1 + 5
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
