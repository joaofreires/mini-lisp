mod ast;
mod builtins;
mod parser;
mod prelude;
mod utils;

use ast::evaluate;
use parser::parse;
use prelude::Environment;
use utils::read_input;

fn main() {
    let mut env = Environment::default();
    env.init();

    loop {
        match read_input("lisp> ") {
            Ok(input) => {
                if input == "exit" {
                    break;
                }

                match parse(&input) {
                    Ok(ast) => match evaluate(ast, &mut env) {
                        Ok(result) => println!("{}", result),
                        Err(err) => eprintln!("Error: {:?}", err),
                    },
                    Err(err) => eprintln!("Parse error: {:?}", err),
                }
            }
            Err(err) => {
                eprintln!("Read error: {:?}", err);
                break;
            }
        }
    }
}
