use std::{env, fs};

use aqa_pseudocode::{interpreter::Interpreter, parser::Parser};

fn main() {
    let filename = env::args().nth(1).unwrap();
    let mut contents = fs::read_to_string(&filename).unwrap();
    contents.push('\n');
    let stmts = Parser::new(&contents).parse();
    let mut interpreter = Interpreter::new(&contents);
    interpreter.run(&stmts);
}
