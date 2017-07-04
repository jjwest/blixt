use nom;

error_chain! {
    foreign_links {
        Nom(nom::ErrorKind);
    }

    errors {
        ParseError(t: String) {
            description("Parse error")
            display("Error while parsing: {}", t)
        }
    }
}
