use std::ops;

use crate::{
    ast::Op,
    interpreter::{errors::RuntimeError, Type, ValueResult},
};

use super::Value;

impl From<&Value> for Type {
    fn from(other: &Value) -> Self {
        match other {
            Value::Subroutine(_) => Type::Subroutine,
            Value::BuiltinFn(_) => Type::BuiltinFn,
            Value::String(_) => Type::String,
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Boolean,
            Value::Null => Type::Null,
        }
    }
}

macro_rules! cannot_perform {
    ($lhs:expr, $rhs:expr, $op:expr) => {
        Err(RuntimeError::CannotPerform {
            lhs: $lhs.into(),
            rhs: $rhs.into(),
            op: $op,
        })
    };
}

impl ops::Add for Value {
    type Output = ValueResult;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1 + s2)),
            (v1, v2) => cannot_perform!(&v1, v2, Op::Add),
        }
    }
}

impl ops::Sub for Value {
    type Output = ValueResult;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            (v1, v2) => cannot_perform!(&v1, v2, Op::Minus),
        }
    }
}

impl ops::Mul for Value {
    type Output = ValueResult;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(*n as usize))),
            (v1, v2) => cannot_perform!(&v1, v2, Op::Multiply),
        }
    }
}

impl ops::Div for Value {
    type Output = ValueResult;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, &rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            (v1, v2) => cannot_perform!(&v1, v2, Op::Divide),
        }
    }
}

impl ops::Neg for Value {
    type Output = ValueResult;
    fn neg(self) -> Self::Output {
        if let Value::Number(n) = self {
            Ok(Value::Number(-n))
        } else {
            Err(RuntimeError::WrongType {
                expected: Type::Number,
                got: Type::from(&self),
            })
        }
    }
}

impl ops::Not for Value {
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            _ => None,
        }
    }
}
