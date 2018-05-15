#![feature(nll)]

extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate log;
extern crate itertools;
extern crate pretty_env_logger;
extern crate termcolor;

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

fn main() {
    pretty_env_logger::init().unwrap();

    if let Err(e) = run() {
        eprintln!("{}", e);
    }
}

fn run() -> Result<(), failure::Error> {
    let source_file = env::args()
        .nth(1)
        .ok_or(failure::err_msg("Missing argument FILE"))?;

    let mut context = Context::new();

    if let Ok(var) = env::var("PHOTON_DEBUG") {
        if var == "1" {
            env::set_var("RUST_BACKTRACE", "1");
            context.debug_mode = true;
        }
    }

    info!("Starting lexing");
    let tokens = lexer::generate_tokens(&source_file, &mut context)?;

    info!("Starting parsing");
    let ast = parser::parse_ast(tokens, &mut context)?;

    info!("Starting typechecking");
    typecheck::typecheck(&ast, &mut context)?;

    interpreter::interpret(&ast, &mut context);

    Ok(())
}
