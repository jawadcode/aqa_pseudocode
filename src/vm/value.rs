use super::{Chunk, VMError, VM};
use std::{
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Subroutine(Subroutine),
    BuiltinFn(NativeFn),
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone)]
pub enum Type {
    Subroutine,
    BuiltinFn,
    String,
    Number,
    Bool,
    Null,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Subroutine => write!(f, "Subroutine"),
            Type::BuiltinFn => write!(f, "Builtin"),
            Type::String => write!(f, "String"),
            Type::Number => write!(f, "Number"),
            Type::Bool => write!(f, "Boolean"),
            Type::Null => write!(f, "Null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Subroutine {
    pub ident: String,
    pub params: Vec<String>,
    pub chunk: Chunk,
}

#[derive(Clone)]
pub struct NativeFn {
    pub ident: String,
    pub params: Vec<String>,
    pub body: fn(&mut VM, &[Value]) -> Value,
}

impl PartialEq for Subroutine {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.params == other.params
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.params == other.params
    }
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "NativeFn {{ ident: {:?}, params: {:?} }}",
            self.ident, self.params
        )
    }
}

macro_rules! cannot_perform {
    ($lhs:expr, $rhs:expr, $op:expr) => {
        Err(VMError::CannotPerform {
            op: $op,
            lhs: $lhs.into(),
            rhs: $rhs.into(),
        })
    };
}

impl From<&Value> for Type {
    fn from(other: &Value) -> Self {
        match other {
            Value::Subroutine(_) => Type::Subroutine,
            Value::BuiltinFn(_) => Type::BuiltinFn,
            Value::String(_) => Type::String,
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Bool,
            Value::Null => Type::Null,
        }
    }
}

pub type ValueResult = Result<Value, VMError>;

impl Add for Value {
    type Output = ValueResult;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1 + s2)),
            (v1, v2) => cannot_perform!(&v1, v2, "+"),
        }
    }
}

impl Sub for Value {
    type Output = ValueResult;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            (v1, v2) => cannot_perform!(&v1, v2, "-"),
        }
    }
}

impl Mul for Value {
    type Output = ValueResult;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(*n as usize))),
            (v1, v2) => cannot_perform!(&v1, v2, "*"),
        }
    }
}

impl Div for Value {
    type Output = ValueResult;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            (v1, v2) => cannot_perform!(&v1, v2, "/"),
        }
    }
}

impl Neg for Value {
    type Output = ValueResult;
    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            _ => Err(VMError::IncorrectType {
                expected: Type::Number,
                got: Type::from(&self),
            }),
        }
    }
}

impl Not for Value {
    type Output = ValueResult;
    fn not(self) -> Self::Output {
        Ok(Value::Bool(!bool::from(self)))
    }
}

impl From<Value> for bool {
    fn from(other: Value) -> Self {
        match other {
            Value::Subroutine(_) => true,
            Value::BuiltinFn(_) => true,
            Value::String(s) => !s.is_empty(),
            Value::Number(n) => n != 0.0,
            Value::Bool(b) => b,
            Value::Null => false,
        }
    }
}

pub type VMResult<T> = Result<T, VMError>;

impl From<Value> for VMResult<f64> {
    fn from(other: Value) -> Self {
        match other {
            Value::Number(f) => Ok(f),
            _ => Err(VMError::IncorrectType {
                expected: Type::Number,
                got: (&other).into(),
            }),
        }
    }
}

impl From<Value> for VMResult<bool> {
    fn from(other: Value) -> Self {
        match other {
            Value::Bool(b) => Ok(b),
            _ => Err(VMError::IncorrectType {
                expected: Type::Bool,
                got: (&other).into(),
            }),
        }
    }
}

impl From<Value> for VMResult<String> {
    fn from(other: Value) -> Self {
        match other {
            Value::String(s) => Ok(s),
            _ => Err(VMError::IncorrectType {
                expected: Type::String,
                got: (&other).into(),
            }),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Subroutine(sub) => {
                write!(f, "<SUBROUTINE {}({})>", sub.ident, sub.params.join(", "))
            }
            Value::BuiltinFn(bf) => write!(f, "<BUILTIN {}({})>", bf.ident, bf.params.join(", ")),
            Value::String(s) => write!(f, "{s}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Null => write!(f, "NULL"),
        }
    }
}
