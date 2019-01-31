use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub,
    SubAssign,
};

use crate::common::Symbol;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ValueKind {
    Bool,
    String,
    Integer,
    Float,
    Struct(Symbol),
    Nil,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(Symbol),
    Struct(Symbol),
    Nil,
}

impl Add for Value {
    type Output = Value;
    fn add(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f32 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f32),
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
