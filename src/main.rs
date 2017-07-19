#![feature(slice_patterns)]
#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate pretty_env_logger;

use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

mod ast;
mod builtins;
mod errors;
mod lexer;
#[macro_use]
mod macros;
mod parser;

use errors::*;
use parser::Parser;

fn main() {
    pretty_env_logger::init().unwrap();

    if let Err(e) = parse_args().and_then(|src| run(&src)) {
        eprintln!("error: {}", e);
        for e in e.iter().skip(1) {
            eprintln!("caused by: {}", e);
        }
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
    let file_name = env::args().nth(1).ok_or("No file provided")?;

    let mut file = BufReader::new(File::open(Path::new(&file_name)).chain_err(
        || "Could not open file",
    )?);

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).chain_err(
        || "Failed reading from file",
    )?;

    Ok(buf)
}
