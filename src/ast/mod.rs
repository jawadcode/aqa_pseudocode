use std::ops::Range;

use crate::parser::Spanned;

pub mod impls;

type SpanExpr = Spanned<Expr>;
pub type Boxpr = Box<SpanExpr>;
type Stmts = Vec<Spanned<Stmt>>;

pub enum Stmt {
    Assign {
        ident: String,
        expr: Expr,
    },
    SubDef {
        ident: String,
        params: Vec<String>,
        body: Stmts,
    },
    SubCall {
        ident: String,
        args: Vec<SpanExpr>,
    },
    While {
        cond: SpanExpr,
        body: Stmts,
    },
    RepeatUntil {
        body: Stmts,
        until_cond: SpanExpr,
    },
    For {
        counter: String,
        range: Range<SpanExpr>,
        body: Stmts,
    },
}

pub enum Expr {
    Ident(String),
    Literal(Literal),
    UnaryOp {
        op: Op,
        expr: Boxpr,
    },
    BinaryOp {
        op: Op,
        lhs: Boxpr,
        rhs: Boxpr,
    },
    FnCall {
        fun: Boxpr,
        args: Vec<Spanned<Expr>>,
    },
}

/// A hardcoded value
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
}

/// A better way to encode all the operations in the language,
/// as opposed to just using `TokenKind`s
pub enum Op {
    /* ARITHMETIC OPERATORS */
    Add,
    Minus,
    Multiply,
    Divide,
    IntDiv,
    Modulus,
    /* RELATIONAL OPERATORS */
    Equals,
    NotEq,
    LessThan,
    GreaterThan,
    LessOrEq,
    GreaterOrEq,
    /* BOOLEAN OPERATORS */
    And,
    Or,
    Not,
}
