mod asm;
mod tac;
mod lexer;
mod parser;

use asm::*;
use tac::*;
use lexer::*;
use std::env;
use parser::*;
use std::process;
use std::process::Command;
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
    // for token in &lexer.tokens.tokens {
    // 	println!("{}", token.display(&lexer.file_contents));
    // }
    // println!();

    let mut parser = Parser::parse(&mut lexer);
    // parser.ast.print();
    // println!();

    // typecheck here?

    let mut tac = TAC::generate(&mut parser);
    for line in &tac.large_literals {
	println!("{:?}", line);
    }
    println!();
    for line in &tac.code {
	println!("{:?}", line);
    }
    println!();

    let asm = ASM::generate(&mut tac);
    // for line in &asm.data_output {
    // 	println!("{}", line);
    // }
    // println!();
    // for line in &asm.text_output {
    // 	println!("{}", line);
    // }

    let mut file = File::create("../out.asm").unwrap();
    for line in &asm.data_output {
	file.write(line.as_bytes()).expect("Failed to write assembly to file");
	file.write(b"\n").expect("Failed to write assembly to file");
    }
    for line in &asm.text_output {
	file.write(line.as_bytes()).expect("Failed to write assembly to file");
	file.write(b"\n").expect("Failed to write assembly to file");
    }

    Command::new("as")
        .arg("../out.asm")
        .arg("-o")
        .arg("../out.o")
        .output()
        .expect("Error assembling code.");
    Command::new("as")
        .arg("../io.asm")
        .arg("-o")
        .arg("../io.o")
        .output()
        .expect("Error assembling lib.");
    Command::new("ld")
	.arg("-m")
	.arg("elf_x86_64")
	.arg("../out.o")
	.arg("../io.o")
	.arg("-o")
	.arg("../a.out")
	.output()
	.expect("Error linking code");
}
