mod lexer;

use lexer::*;
use std::env;
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

    let lexer = Lexer::lex(contents);

    for token in lexer.tokens {
	println!("{}", token.display(&lexer.file_contents));
    }
}
