// mod asm;
mod lexer;
mod parser;
mod tac;

// use asm::*;
use lexer::*;
use parser::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process;
use std::process::Command;
use tac::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expected filename as argument");
        process::exit(1);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file");

    let mut lexer = Lexer::lex(contents);
    println!("Lexer:");
    println!("-=------------=-");
    for token in &lexer.tokens.tokens {
        println!("{}", token.display(&lexer.file_contents));
    }
    println!("-=------------=-");
    println!();

    let parser = Parser::parse(&mut lexer);
    println!("Parser:");
    println!("-=------------=-");
    println!("{}", parser.ast);
    println!("-=------------=-");
    println!();

    // typecheck here?

    let tac = Tac::generate(parser);
    println!("TAC:");
    println!("-=------------=-");
    for line in &tac.code {
        println!("{}", line);
    }
    println!("-=------------=-");
    println!();

    // Codegen is frozen for now. Focusing on intermediate representation.
    // let asm = Asm::generate(&mut tac);
    // println!("ASM:");
    // println!("-=------------=-");
    // for line in &asm.data_output {
    //     println!("{}", line);
    // }
    // for line in &asm.text_output {
    //     println!("{}", line);
    // }
    // println!("-=------------=-");

    // let mut file = File::create("../out.asm").unwrap();
    // for line in &asm.data_output {
    //     file.write_all(line.as_bytes())
    //         .expect("Failed to write assembly to file");
    //     file.write_all(b"\n")
    //         .expect("Failed to write assembly to file");
    // }
    // for line in &asm.text_output {
    //     file.write_all(line.as_bytes())
    //         .expect("Failed to write assembly to file");
    //     file.write_all(b"\n")
    //         .expect("Failed to write assembly to file");
    // }

    // Command::new("as")
    //     .arg("../out.asm")
    //     .arg("-o")
    //     .arg("../out.o")
    //     .output()
    //     .expect("Error assembling code.");
    // Command::new("as")
    //     .arg("../io.asm")
    //     .arg("-o")
    //     .arg("../io.o")
    //     .output()
    //     .expect("Error assembling lib.");
    // Command::new("ld")
    //     .arg("-m")
    //     .arg("elf_x86_64")
    //     .arg("../out.o")
    //     .arg("../io.o")
    //     .arg("-o")
    //     .arg("../a.out")
    //     .output()
    //     .expect("Error linking code");
    // let result = Command::new("./../a.out")
    //     .output()
    //     .expect("Error running code");
    // println!();
    // println!("Program results:");
    // println!("-=------------=-");
    // print!("{}", String::from_utf8(result.stdout).unwrap());
    // println!("-=------------=-");
}
