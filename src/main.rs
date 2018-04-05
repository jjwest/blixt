#![feature(nll)]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate log;
extern crate itertools;
extern crate pretty_env_logger;

use failure::err_msg;

use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

mod ast;
mod builtins;
mod lexer;
mod parser;

fn main() {
    pretty_env_logger::init().unwrap();

    let source = match parse_args() {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    };

    let _tokens = lexer::generate_tokens(&source).unwrap_or_else(|e| {
        print_error_message(e, &source);
        process::exit(1);
    });
}

fn parse_args() -> Result<Vec<char>, failure::Error> {
    let file_name = env::args()
        .nth(1)
        .ok_or_else(|| err_msg("Missing file name"))?;
    let mut file = File::open(&file_name)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;

    Ok(buf.into_iter().map(|byte| byte as char).collect())
}

fn print_error_message(error: failure::Error, source: &[char]) {
    if let Ok(err) = error.downcast::<lexer::Error>() {
        let source: Vec<u8> = source.iter().map(|c| *c as u8).collect();
        let source = String::from_utf8(source).unwrap();

        let line = source.lines().nth(err.line - 1).expect("Invalid line");
        println!("Error on line {}:", err.line);
        println!("{}", line);
        print!("{:>start$}", "", start = err.span.start - 1);

        for _ in 0..err.span.len {
            print!("^");
        }

        println!();
        println!("{}", err.message);
    }
}
