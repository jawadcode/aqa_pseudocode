pub mod impls;

use std::fmt;

use crate::ast::Stmts;

use super::Interpreter;

#[derive(Clone, PartialEq)]
pub enum Value {
    Subroutine(Subroutine),
    BuiltinFn(BuiltinFn),
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

#[derive(Clone, PartialEq)]
pub struct Subroutine {
    pub ident: String,
    pub params: Vec<String>,
    pub body: Stmts,
}

#[derive(Clone)]
pub struct BuiltinFn {
    pub ident: String,
    pub params: Vec<String>,
    pub body: fn(&mut Interpreter, &[Value]) -> Value,
}

impl PartialEq for BuiltinFn {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.params == other.params
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Subroutine(sr) => sr.to_string(),
                Value::BuiltinFn(bf) => bf.to_string(),
                Value::String(s) => s.clone(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => if *b { "True" } else { "False" }.to_string(),
                Value::Null => "NULL".to_string(),
            }
        )
    }
}

impl fmt::Display for Subroutine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<SUBROUTINE {}({})>", self.ident, self.params.join(", "))
    }
}

impl fmt::Display for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<BUILTIN {}({})>", self.ident, self.params.join(", "))
    }
}
