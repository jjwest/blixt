use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueKind {
    Bool,
    String,
    Integer,
    Float,
    Nil,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(Rc<String>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Nil => write!(f, "nil")?,
            Value::Bool(val) => write!(f, "{}", val)?,
            Value::Int(val) => write!(f, "{}", val)?,
            Value::Float(val) => write!(f, "{}", val)?,
            Value::String(ref val) => write!(f, "{}", val)?,
        }

        Ok(())
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f32),
            (Value::String(a), Value::String(b)) => {
                let mut value = (*a).clone();
                value.push_str(&*b);
                Value::String(Rc::new(value))
            }
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, other: Value) {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => *a = *a + b,
            (Value::Float(a), Value::Float(b)) => *a = *a + b,
            value @ (Value::Float(_), Value::Int(_)) => {
                if let (Value::Float(a), Value::Int(b)) = (&value.0, &value.1) {
                    *value.0 = Value::Float(a + *b as f32);
                }
            }
            value @ (Value::Int(_), Value::Float(_)) => {
                if let (Value::Int(a), Value::Float(b)) = (&value.0, &value.1) {
                    *value.0 = Value::Float(*a as f32 + b);
                }
            }
            value @ (Value::String(_), Value::String(_)) => {
                if let (Value::String(a), Value::String(b)) = (&value.0, &value.1) {
                    let mut new = (**a).clone();
                    new.extend(b.chars());
                    *value.0 = Value::String(Rc::new(new));
                }
            }
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f32),
            (a, b) => panic!("Cannot sub a {:?} with a {:?}", a, b),
        }
    }
}

impl SubAssign for Value {
    fn sub_assign(&mut self, other: Value) {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => *a = *a - b,
            (Value::Float(a), Value::Float(b)) => *a = *a - b,
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}

impl Mul for Value {
    type Output = Value;
    fn mul(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f32),
            (a, b) => panic!("Cannot multiply a {:?} with a {:?}", a, b),
        }
    }
}

impl MulAssign for Value {
    fn mul_assign(&mut self, other: Value) {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => *a = *a * b,
            (Value::Float(a), Value::Float(b)) => *a = *a * b,
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}

impl Div for Value {
    type Output = Value;
    fn div(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 / b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a / b as f32),
            (a, b) => panic!("Cannot divide a {:?} with a {:?}", a, b),
        }
    }
}

impl DivAssign for Value {
    fn div_assign(&mut self, other: Value) {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => *a = *a / b,
            (Value::Float(a), Value::Float(b)) => *a = *a / b,
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}

impl Rem for Value {
    type Output = Value;
    fn rem(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 % b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b as f32),
            (a, b) => panic!("Cannot rem a {:?} with a {:?}", a, b),
        }
    }
}

impl RemAssign for Value {
    fn rem_assign(&mut self, other: Value) {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => *a = *a % b,
            (Value::Float(a), Value::Float(b)) => *a = *a % b,
            (a, b) => panic!("Cannot add a {:?} with a {:?}", a, b),
        }
    }
}
