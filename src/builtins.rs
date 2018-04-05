use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueKind {
    Bool,
    Float,
    Int,
    String,
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(Rc<String>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Nil => write!(f, "nil")?,
            Value::Bool(val) => write!(f, "{}", val)?,
            Value::Int32(val) => write!(f, "{}", val)?,
            Value::Float32(val) => write!(f, "{}", val)?,
            Value::String(ref val) => write!(f, "{}", val)?,
        }

        Ok(())
    }
}
