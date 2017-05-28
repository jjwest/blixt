#![feature(plugin, trace_macros)]

#![plugin(clippy)]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;

use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

pub mod errors;
pub mod parser;

use errors::*;
use parser::Parser;

fn main() {
    match parse_args().and_then(|src| Parser::parse(&src)) {
        Ok(result) => {
            println!("Result: {}", result);
        }
        Err(e) => {
            println!("error: {}", e);

            for e in e.iter().skip(1) {
                println!("caused by: {}", e);
            }
        }
    }
}

#[allow(or_fun_call)]
fn parse_args() -> Result<Vec<u8>> {
    let file_name = env::args()
        .nth(1)
        .ok_or(io::Error::new(io::ErrorKind::Other, "No file provided"))
        .chain_err(|| "Could not open file")?;

    let mut file = File::open(Path::new(&file_name))
        .chain_err(|| "Could not open file")?;

    let mut buf = Vec::new();
    file.read_to_end(&mut buf)
        .chain_err(|| "Failed reading from file")?;

    Ok(buf)
}
