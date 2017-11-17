// use nom;
// use std::str;

// error_chain!{
//     foreign_links {
//         Nom(nom::ErrorKind);
//         Utf8(str::Utf8Error);
//     }
// }

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;
pub use failure::err_msg;
