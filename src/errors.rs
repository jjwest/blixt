#![recursion_limit = "1024"]

use nom;

error_chain!{
    foreign_links {
        Nom(nom::ErrorKind);
    }
}
