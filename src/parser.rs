use std::str;

use errors::*;
use nom::{alphanumeric, double, digit, float};

#[derive(Default)]
pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    pub fn parse(source: &[u8]) -> Result<i64> {
        if source.len() > 1 {
            Ok(30)
        } else {
            Err("File is empty".into())
        }
    }
}

#[derive(Debug, PartialEq)]
enum Type {
    Bool(bool),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
    String(String),
    Uint32(u32),
    Uint64(u64),
}


named!(identifier<String>,
       map_res!(
           many1!(alt!(ws!(alphanumeric) | ws!(tag!("_")))),
           |parts: Vec<&[u8]>| {
               String::from_utf8(parts
                                 .into_iter()
                                 .flat_map(|part| part.iter().cloned())
                                 .collect())
           } 
       )
);

named!(parens, ws!(delimited!(tag!("("), is_not!(")"), tag!(")"))));

named!(string<Type>,
       do_parse!(
           str: map_res!(
               ws!(delimited!(tag!("\""), is_not!("\""), tag!("\""))),
               |bytes: &[u8]| String::from_utf8(bytes.to_vec())
           ) >>
           (Type::String(str))
       )
);

named!(float64<Type>,
       do_parse!(
           res: ws!(double) >>
           (Type::Float64(res))
       )
);

named!(float32<Type>,
       do_parse!(
           res: ws!(float) >>
           (Type::Float32(res))
       )
);

named!(int64<Type>,
       do_parse!(
           res: map_res!(
               map_res!(
                   ws!(digit),
                   str::from_utf8
               ),
               str::parse
           ) >>
           (Type::Int64(res))
       )
);

named!(uint32<Type>,
       do_parse!(
           res: map_res!(
               map_res!(
                   ws!(digit),
                   str::from_utf8
               ),
               str::parse
           ) >>
           (Type::Uint32(res))
       )
);

named!(uint64<Type>,
       do_parse!(
           res: map_res!(
               map_res!(
                   ws!(digit),
                   str::from_utf8
               ),
               str::parse
           ) >>
           (Type::Uint64(res))
       )
);

named!(int32<Type>,
       do_parse!(
           res: map_res!(
               map_res!(
                   ws!(digit),
                   str::from_utf8
               ),
               str::parse
           ) >>
           (Type::Int32(res))
       )
);

named!(boolean<Type>,
       map!(
           ws!(alt!(tag!("true") | tag!("false"))),
           |tag: &[u8]| {
               if tag[0] as char == 't' { Type::Bool(true) } else { Type::Bool(false) }
           }
       )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_good_identifier() {
        let source = b" hello_There";
        let result = identifier(source);
        assert!(result.is_done());
        assert_eq!("hello_There", result.unwrap().1.as_str());
    }

    #[test]
    fn test_parse_bool() {
        let source_true = b" true";
        let result_true = boolean(source_true);
        assert!(result_true.is_done());
        assert_eq!(Type::Bool(true), result_true.unwrap().1);

        let source_false = b" false ";
        let result_false = boolean(source_false);
        assert!(result_false.is_done());
        assert_eq!(Type::Bool(false), result_false.unwrap().1);

    }

    #[test]
    fn test_parse_f64() {
        let source = b"   45.843 ";
        let result = float64(source);

        assert!(result.is_done());
        assert_eq!(Type::Float64(45.843), result.unwrap().1);
    }

    #[test]
    fn test_parse_i32() {
        let source = b"   45";
        let result = int32(source);

        assert!(result.is_done());
        assert_eq!(Type::Int32(45), result.unwrap().1);
        
    }

    #[test]
    fn test_parse_i64() {
        let source = b"   45";
        let result = int64(source);

        assert!(result.is_done());
        assert_eq!(Type::Int64(45), result.unwrap().1);
        
    }

    #[test]
    fn test_parse_u32() {
        let source = b"   45";
        let result = uint32(source);

        assert!(result.is_done());
        assert_eq!(Type::Uint32(45), result.unwrap().1);
    }

    #[test]
    fn test_parse_u64() {
        let source = b"   45";
        let result = uint64(source);

        assert!(result.is_done());
        assert_eq!(Type::Uint64(45), result.unwrap().1);
    }

    #[test]
    fn test_parse_f32() {
        let source = b"   45.843 ";
        let result = float32(source);

        assert!(result.is_done());
        assert_eq!(Type::Float32(45.843), result.unwrap().1);
    }

    #[test]
    fn test_string() {
        let source = b"  \"Hey man!\"";
        let result = string(source);

        assert!(result.is_done());
        assert_eq!(Type::String("Hey man!".to_string()), result.unwrap().1);
    }
}
