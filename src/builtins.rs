use std::fmt;
use std::rc::Rc;

use errors::*;


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

impl ::std::ops::Add for Value {
    type Output = Result<Value>;

    fn add(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a + b)),
            (Float32(a), Float32(b)) => Ok(Float32(a + b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err(err_msg("Cannot add an integer with a float")),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err(err_msg("Cannot add an integer with a bool")),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err(err_msg("Cannot add a float with a bool")),
            (Bool(_), Bool(_)) => Err(err_msg("Cannot add bools")),
            _ => Err(err_msg("Cannot add with nil")),
        }
    }
}

impl ::std::ops::Sub for Value {
    type Output = Result<Value>;

    fn sub(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a - b)),
            (Float32(a), Float32(b)) => Ok(Float32(a - b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err(err_msg("Cannot subtract an int from a bool")),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err(err_msg("Cannot subtract an integer with a bool")),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err(err_msg("Cannot subtract a float with a bool")),
            (Bool(_), Bool(_)) => Err(err_msg("Cannot subtract bools")),
            _ => Err(err_msg("Cannot subtract with nil")),
        }
    }
}

impl ::std::ops::Mul for Value {
    type Output = Result<Value>;

    fn mul(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a * b)),
            (Float32(a), Float32(b)) => Ok(Float32(a * b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err(err_msg("Cannot multiply an int with a float")),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err(err_msg("Cannot multiply an integer with a bool")),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err(err_msg("Cannot multiply a float with a bool")),
            (Bool(_), Bool(_)) => Err(err_msg("Cannot multiply bools")),
            _ => Err(err_msg("Cannot multiply with nil")),
        }
    }
}

impl ::std::ops::Div for Value {
    type Output = Result<Value>;

    fn div(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a / b)),
            (Float32(a), Float32(b)) => Ok(Float32(a / b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err(err_msg("Cannot divide an int with a float")),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err(err_msg("Cannot divide an integer with a bool")),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err(err_msg("Cannot divide a float with a bool")),
            (Bool(_), Bool(_)) => Err(err_msg("Cannot divide bools")),
            _ => Err(err_msg("Cannot divide with nil")),
        }
    }
}

impl ::std::ops::Rem for Value {
    type Output = Result<Value>;

    fn rem(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a % b)),
            (Float32(a), Float32(b)) => Ok(Float32(a % b)),
            (Int32(_), Float32(_)) => Err(err_msg("Cannot divide a int with an float")),
            (Float32(_), Int32(_)) => Err(err_msg("Cannot divide a float with an int")),
            (Int32(_), Bool(_)) => Err(err_msg("Cannot divide an int with a bool")),
            (Bool(_), Int32(_)) => Err(err_msg("Cannot divide a bool with a float")),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) |
            (Bool(_), Bool(_)) => Err(err_msg("Cannot use modulo with bools")),
            _ => Err(err_msg("Cannot modulo with nil")),
        }
    }
}
