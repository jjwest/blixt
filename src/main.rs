#![feature(nll)]

extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate log;
extern crate itertools;
extern crate pretty_env_logger;

mod ast;
mod builtins;
mod interpreter;
mod lexer;
mod parser;
mod traits;

use failure::err_msg;

use interpreter::Interpreter;
use traits::Visitable;

use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

fn main() {
    pretty_env_logger::init().unwrap();

    let source = match parse_args() {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    };

    let tokens = lexer::generate_tokens(&source).unwrap_or_else(|e| {
        print_error_message(e, &env::args().nth(1).unwrap(), &source);
        process::exit(1);
    });

    let mut ast = parser::parse_ast(tokens).unwrap_or_else(|e| {
        print_error_message(e, &env::args().nth(1).unwrap(), &source);
        process::exit(1);
    });

    println!("{:#?}", ast);
    let mut interp = Interpreter::new();
    ast.accept(&mut interp);
}

fn parse_args() -> Result<Vec<char>, failure::Error> {
    let filename = env::args()
        .nth(1)
        .ok_or_else(|| err_msg("Missing file name"))?;
    let mut file = File::open(&filename)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;

    Ok(buf.into_iter().map(|byte| byte as char).collect())
}

fn print_error_message(error: failure::Error, filename: &str, source: &[char]) {
    if let Ok(err) = error.downcast::<lexer::Error>() {
        let source: Vec<u8> = source.iter().map(|c| *c as u8).collect();
        let source = String::from_utf8(source).unwrap();

        let line = source.lines().nth(err.line - 1).expect("Invalid line");
        println!("|  Error in {} on line {}:", filename, err.line);
        println!("|");
        println!("|  {}", line);
        print!("|  {:>start$}", "", start = err.span.start - 1);

        for _ in 0..err.span.len {
            print!("^");
        }

        println!("\n|  {}", err.message);
    }
}
