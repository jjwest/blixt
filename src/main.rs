#![feature(nll)]

extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate log;
extern crate itertools;
extern crate pretty_env_logger;

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod primitives;
mod scope;
mod traits;
mod typecheck;

use common::Context;

use std::env;
use std::process;

fn main() {
    pretty_env_logger::init().unwrap();

    let source_file = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Missing arg FILE");
        process::exit(1);
    });

    let mut context = Context::new();

    if let Ok(var) = env::var("PHOTON_DEBUG") {
        if var == "1" {
            env::set_var("RUST_BACKTRACE", "1");
            context.debug_mode = true;
        }
    }

    info!("Starting lexing");
    let tokens = lexer::generate_tokens(&source_file, &mut context).unwrap_or_else(|err| {
        eprintln!("Error lexing: {}", err);
        process::exit(1);
    });

    info!("Starting parsing");
    let ast = parser::parse_ast(tokens, &mut context).unwrap_or_else(|_| {
        eprintln!("ERROR PARSING");
        process::exit(1);
    });

    info!("Starting typechecking");
    if let Err(_) = typecheck::typecheck(&ast, &mut context) {
        eprintln!("Typechecking failed");
        process::exit(1);
    }

    interpreter::interpret(&ast, &mut context);
}
