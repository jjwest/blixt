use std::fmt;

use errors::*;


#[derive(Debug, PartialEq)]
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
    String(String),
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
            (Float32(_), Int32(_)) => Err("Cannot add an integer with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot add an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot add a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot add bools".into()),
            _ => Err("Cannot add with nil".into()),
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
            (Float32(_), Int32(_)) => Err("Cannot subtract an int from a bool".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot subtract an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot subtract a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot subtract bools".into()),
            _ => Err("Cannot subtract with nil".into()),
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
            (Float32(_), Int32(_)) => Err("Cannot multiply an int with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot multiply an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot multiply a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot multiply bools".into()),
            _ => Err("Cannot multiply with nil".into()),
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
            (Float32(_), Int32(_)) => Err("Cannot divide an int with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot divide an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot divide a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot divide bools".into()),
            _ => Err("Cannot divide with nil".into()),
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
            (Int32(_), Float32(_)) => Err("Cannot divide a int with an float".into()),
            (Float32(_), Int32(_)) => Err("Cannot divide a float with an int".into()),
            (Int32(_), Bool(_)) => Err("Cannot divide an int with a bool".into()),
            (Bool(_), Int32(_)) => Err("Cannot divide a bool with a float".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) |
            (Bool(_), Bool(_)) => Err("Cannot use modulo with bools".into()),
            _ => Err("Cannot modulo with nil".into()),
        }
    }
}
