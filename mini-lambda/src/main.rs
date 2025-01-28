use mini_lambda::run::*;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    eprintln!("Usage: {} <file>", args[0]);
    return;
  }

  let path = &args[1];
  if let Ok(mut file) = File::open(path) {
    let mut content = String::new();
    if file.read_to_string(&mut content).is_ok() {
      let res = baum_lang::parse::parse(&content);
      println!("{:?}", res);
      run(&content);
    } else {
      eprintln!("Could not read file content: {}", path);
    }
  } else {
    eprintln!("Could not open file: {}", path);
  }
}
