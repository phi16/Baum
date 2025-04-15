use crate::types::common::Tag;
use crate::types::val::RE;

pub fn run<P: Tag, S: Tag>(e: RE<P, S>) {
  eprintln!("RUN {:?}", e);
  unimplemented!()
}
