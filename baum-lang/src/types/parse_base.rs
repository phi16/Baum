pub type ModuleName<I> = Vec<I>;

#[derive(Debug, Clone)]
pub enum ArgF<I, E> {
  Ids(Vec<I>),
  Ty(E),
  IdsTy(Vec<I>, E),
}

#[derive(Debug, Clone, Copy)]
pub enum Vis {
  Explicit,
  Implicit,
}

pub type ArgVis<I, E> = (ArgF<I, E>, Vis);

#[derive(Debug, Clone)]
pub struct DefF<I, E> {
  pub name: I,
  pub args: Vec<ArgVis<I, E>>,
  pub ty: Option<E>,
  pub body: E,
}

#[derive(Debug, Clone)]
pub struct Id<'a>(&'a str);

impl<'a> Id<'a> {
  pub fn new(s: &'a str) -> Self {
    Id(s)
  }
  pub fn as_str(&self) -> &'a str {
    self.0
  }
}
