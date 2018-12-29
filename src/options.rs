use clap::{App, Arg};

pub struct Options {
    pub file: String,
}

impl Options {
    pub fn parse() -> Options {
        let matches = App::new("Blixt")
            .version("0.1")
            .author("Jonas Westlund <jonaswestlund101@gmail.com>")
            .about("A toy programming language")
            .arg(
                Arg::with_name("INPUT")
                    .help("File to run")
                    .required(true)
                    .index(1),
            )
            .get_matches();

        Options {
            file: matches.value_of("INPUT").unwrap().to_string(),
        }
    }
}
