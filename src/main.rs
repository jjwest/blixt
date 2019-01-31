mod arena;
mod ast;
mod common;
// mod interpreter;
mod lexer;
mod location;
mod options;
mod parser;
mod primitives;
mod scope;
mod token;
mod typecheck;

use std::env;
use std::fs;

use log::info;

use common::Context;
use options::Options;

fn main() {
    env_logger::init();
    let _ = run();
}

fn run() -> Result<(), ()> {
    let options = Options::parse();
    let mut context = Context::new();

    if let Ok(var) = env::var("BLIXT_DEBUG") {
        if var == "1" {
            env::set_var("RUST_BACKTRACE", "1");
            context.debug_mode = true;
        }
    }

    let interned_file = context.interner.intern(&options.file);
    let source = fs::read(&options.file).expect("Cant open file");

    info!("Starting lexing");
    let tokens = lexer::generate_tokens(&source, interned_file, &mut context)?;

    info!("Starting parsing");
    let ast = parser::parse_ast(tokens, &mut context)?;

    info!("Starting typechecking");
    typecheck::typecheck(&ast, &mut context)?;
    info!("Typechecking passed!");

    // interpreter::interpret(&ast, &mut context);

    Ok(())
}
