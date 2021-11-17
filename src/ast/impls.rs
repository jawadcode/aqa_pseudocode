use std::fmt;

use crate::lexer::token::TokenKind;

use super::*;

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Output(value) => format!("(output! {})", value),
                Stmt::Assign { ident, value } => format!("(set! {} {})", ident, value),
                Stmt::ListAssign {
                    list_ident,
                    indices,
                    value,
                } => format!(
                    "(list_set! {} [{}] {})",
                    list_ident,
                    join_things(indices),
                    value
                ),
                Stmt::SubDef {
                    ident,
                    params,
                    body,
                } => format!(
                    "(define! {} [{}] ({}))",
                    ident,
                    join_things(params),
                    join_things(body)
                ),
                Stmt::SubCall { ident, args } => format!("({} {})", ident, join_things(args)),
                Stmt::While { cond, body } => format!("(while! {} {})", cond, join_things(body)),
                Stmt::RepeatUntil { body, until_cond } =>
                    format!("(repeat! ({}) {})", join_things(body), until_cond),
                Stmt::For {
                    counter,
                    range,
                    body,
                } => format!(
                    "(for! {} (.. {} {}) ({}))",
                    counter,
                    range.start,
                    range.end,
                    join_things(body)
                ),
            }
        )
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Ident(i) => i.to_string(),
                Expr::Literal(l) => l.to_string(),
                Expr::List(l) => format!("(list! {})", join_things(l)),
                Expr::Userinput => "(userinput!)".to_string(),
                Expr::UnaryOp { op, expr } => format!("({} {})", op, expr),
                Expr::BinaryOp { op, lhs, rhs } => format!("({} {} {})", op, lhs, rhs),
                Expr::FnCall { ident, args } => format!("({} {})", ident, join_things(args)),
                Expr::Index { list, index } => format!("(index! {} {})", list, index),
            }
        )
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Literal::String(s) => format!(r#""{}""#, s),
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.to_string(),
                Literal::Bool(b) => b.to_string(),
                Literal::Null => "null".to_string(),
            }
        )
    }
}

impl From<TokenKind> for Op {
    fn from(other: TokenKind) -> Self {
        match other {
            TokenKind::Add => Op::Add,
            TokenKind::Minus => Op::Minus,
            TokenKind::Multiply => Op::Multiply,
            TokenKind::Divide => Op::Divide,
            TokenKind::IntDiv => Op::IntDiv,
            TokenKind::Modulus => Op::Modulus,
            TokenKind::Equals => Op::Equals,
            TokenKind::NotEq => Op::NotEq,
            TokenKind::LessThan => Op::LessThan,
            TokenKind::GreaterThan => Op::GreaterThan,
            TokenKind::LessOrEq => Op::LessOrEq,
            TokenKind::GreaterOrEq => Op::GreaterOrEq,
            TokenKind::And => Op::And,
            TokenKind::Or => Op::Or,
            TokenKind::Not => Op::Not,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => "+",
                Op::Minus => "-",
                Op::Multiply => "*",
                Op::Divide => "/",
                Op::IntDiv => "//",
                Op::Modulus => "%",
                Op::Equals => "==",
                Op::NotEq => "!=",
                Op::LessThan => "<",
                Op::GreaterThan => ">",
                Op::LessOrEq => "<=",
                Op::GreaterOrEq => ">=",
                Op::And => "and",
                Op::Or => "or",
                Op::Not => "not",
            }
        )
    }
}

fn join_things<T: ToString>(things: &[T]) -> String {
    things
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}
