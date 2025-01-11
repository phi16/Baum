#[derive(Debug, Clone)]
pub enum Lit {
  Num(i32),
  Chr(char),
  Str(String),
}

type Lift<V> = V;

#[derive(Debug, Clone)]
pub enum ValF<V> {
  Lit(Lit),
  Def(String),
  Var(String), // Id
  Fun(V, V),
  FunDep(String, V, Lift<V>),
  Lam(String, V, Lift<V>),
  Uni(),
  App(V, V),
  Hole,
  Prim(String),
}

#[derive(Debug, Clone)]
pub struct Val(pub ValF<Box<Val>>);
