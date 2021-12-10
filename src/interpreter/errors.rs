use std::fmt;

use crate::{ast::Op, interpreter::Type};

pub enum RuntimeError {
    Undefined {
        ident: String,
    },
    WrongType {
        expected: Type,
        got: Type,
    },
    CannotPerform {
        lhs: Type,
        rhs: Type,
        op: Op,
    },
    CannotCompare {
        lhs: Type,
        rhs: Type,
    },
    NotCallable {
        typ: Type,
    },
    ArityMismatch {
        ident: String,
        expected: usize,
        got: usize,
    },
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Undefined { ident } => write!(f, "{} is undefined", ident),
            RuntimeError::WrongType { expected, got } => {
                write!(f, "Expected {} but got {}", expected, got)
            }
            RuntimeError::CannotPerform { lhs, rhs, op } => {
                write!(f, "Cannot perform '{}' on types {} and {}", op, lhs, rhs)
            }
            RuntimeError::CannotCompare { lhs, rhs } => write!(
                f,
                "Cannot perform comparison between types {} and {}",
                lhs, rhs
            ),
            RuntimeError::NotCallable { typ } => write!(f, "Cannot call {} as subroutine", typ),
            RuntimeError::ArityMismatch {
                ident,
                expected,
                got,
            } => write!(
                f,
                "Subroutine {} expected {} arguments but got {}",
                ident, expected, got
            ),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Subroutine => "subroutine",
                Type::BuiltinFn => "builtin function",
                Type::String => "string",
                Type::Number => "number",
                Type::Boolean => "boolean",
                Type::Null => "null",
            }
        )
    }
}
