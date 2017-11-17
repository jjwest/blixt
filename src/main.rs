#![feature(slice_patterns)]
#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate nom_locate;
extern crate pretty_env_logger;

use std::env;
use std::fs::File;
use std::io::{self, Read};

#[macro_use]
mod macros;
mod ast;
mod builtins;
mod errors;
mod parser;
mod lexer;

use errors::*;
use parser::Parser;

fn main() {
    pretty_env_logger::init().unwrap();

    if let Err(e) = parse_args().and_then(|src| run(&src)) {
        eprintln!("error: {}", e);
    }
}

fn run(source: &[u8]) -> Result<()> {
    let tokens = lexer::generate_tokens(source)?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    program.eval()?;

    Ok(())
}

fn parse_args() -> Result<Vec<u8>> {
    let file_name = env::args().nth(1).ok_or(io::Error::new(
        io::ErrorKind::InvalidInput,
        "No file selected".to_string(),
    ))?;

    let mut file = File::open(&file_name)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    Ok(buf)
}
