// mod ast;
mod common;
// mod interpreter;
mod lexer;
mod location;
// mod parser;
mod options;
mod primitives;
mod token;
// mod scope;
// mod traits;
// mod typecheck;

use std::env;

use crate::common::Context;
use crate::options::Options;

fn main() -> Result<(), ()> {
    pretty_env_logger::init().unwrap();

    let options = Options::parse();
    let mut context = Context::new();

    if let Ok(var) = env::var("BLIXT_DEBUG") {
        if var == "1" {
            env::set_var("RUST_BACKTRACE", "1");
            context.debug_mode = true;
        }
    }

    println!("Starting lexing");
    let tokens = lexer::generate_tokens(&options.file, &mut context)?;

    // info!("Starting parsing");
    // let ast = parser::parse_ast(tokens, &mut context)?;

    // info!("Starting typechecking");
    // typecheck::typecheck(&ast, &mut context)?;

    // interpreter::interpret(&ast, &mut context);

    Ok(())
}
