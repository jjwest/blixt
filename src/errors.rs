use nom;

error_chain!{
    foreign_links {
        Nom(nom::ErrorKind);
    }
}
