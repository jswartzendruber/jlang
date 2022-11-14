mod lexer;

use lexer::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut file = File::open(&args[0]).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    let lexer = Lexer::lex(contents);

    for token in lexer.tokens {
	token.display(&lexer.file_contents);
    }
}
