enum Literal {
  Nat(u32),      // TODO: BigInt
  Rat(u32, u32), // TODO: BigRational
  Chr(char),
  Str(String),
}
type Id = String;
type I = Id;
type ModuleName = Vec<Id>;
enum Expr {
  Hole,
  Ann(E, E),
  Lit(Literal),
  Var(ModuleName, I),
  Let(Vec<Decl>, E),
  Pi(I, E, E),
  Lam(I, E, E),
  App(E, E),
  TupleTy(Vec<E>),
  TupelCon(Vec<E>),
  Proj(u8, E),
  Sigma(Vec<(I, E)>),
  Obj(Vec<(I, E)>),
  Prop(I, E),
  Mu(I, E, Vec<(I, E)>),
  Nu(I, E, Vec<(I, E)>),
}
enum Module {
  Decls(Vec<Decl>),
  Import(String),
}
enum Decl {
  Module(I, Module),
  Def(I, E),
}

type E = Box<Expr>;
