use baum_core::types::common::Tag;
use baum_core::types::val::RE;

pub fn run<P: Tag, S: Tag>(e: RE<P, S>) {
  eprintln!("RUN {:?}", e);
  unimplemented!()
}
