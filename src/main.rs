mod tac;
mod lexer;
mod parser;

use tac::*;
use lexer::*;
use std::env;
use parser::*;
use std::process;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
	println!("Expected filename as argument");
	process::exit(1);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    let mut lexer = Lexer::lex(contents);

    for token in &lexer.tokens.tokens {
	println!("{}", token.display(&lexer.file_contents));
    }

    println!();

    let mut parser = Parser::parse(&mut lexer);
    parser.ast.print();

    println!();

    // typecheck here?

    let tac = TAC::generate(&mut parser);
    for line in &tac.large_literals {
	println!("{:?}", line);
    }
    println!();
    for line in &tac.code {
	println!("{:?}", line);
    }
}
