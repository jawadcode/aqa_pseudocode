use std::ops::Range;

use crate::parser::Spanned;

pub mod impls;

pub type SpanExpr = Spanned<Expr>;
pub type SpanStmt = Spanned<Stmt>;
pub type Boxpr = Box<SpanExpr>;
pub type Stmts = Vec<Spanned<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Output(SpanExpr),
    Assign {
        ident: String,
        value: SpanExpr,
    },
    ListAssign {
        list_ident: String,
        indices: Vec<SpanExpr>,
        value: SpanExpr,
    },
    SubDef {
        ident: String,
        params: Vec<String>,
        body: Stmts,
    },
    Return {
        expr: SpanExpr,
    },
    SubCall {
        ident: String,
        args: Vec<SpanExpr>,
    },
    If {
        cond: SpanExpr,
        body: Stmts,
        else_ifs: Vec<(SpanExpr, Stmts)>,
        else_: Option<Stmts>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Literal(Literal),
    List(Vec<SpanExpr>),
    Userinput,
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
        ident: String,
        args: Vec<Spanned<Expr>>,
    },
    Index {
        list: Boxpr,
        index: Boxpr,
    },
}

/// A hardcoded value
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
}

/// A better way to encode all the operations in the language,
/// as opposed to just using `TokenKind`s
#[derive(Debug, Clone, PartialEq)]
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
