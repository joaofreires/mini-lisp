mod ast;
mod builtins;
mod parser;
mod prelude;
mod utils;
use std::env;
use std::fs;

use prelude::LispEngine;
use utils::read_input;

fn main() {
    let mut engine = LispEngine::default();
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        loop {
            match read_input("mini-lisp> ") {
                Ok(input) => {
                    if input == "exit" {
                        break;
                    }
                    match engine.parse(&input) {
                        Ok(result) => println!("{}", result),
                        Err(err) => eprintln!("Parse error: {:?}", err),
                    }
                }
                Err(err) => {
                    eprintln!("Read error: {:?}", err);
                    break;
                }
            }
        }
    } else {
        let filename = &args[1];
        if let Ok(file_contents) = fs::read_to_string(filename) {
            match engine.parse(&file_contents) {
                Ok(result) => println!("{}", result),
                Err(err) => eprintln!("Parse error: {:?}", err),
            }
        }
    }
}
