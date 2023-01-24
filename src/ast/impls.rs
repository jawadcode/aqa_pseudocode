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
                Stmt::Return { expr } => format!("(return! {})", expr),
                Stmt::SubCall { ident, args } => format!("({} {})", ident, join_things(args)),
                Stmt::If {
                    cond,
                    body,
                    else_ifs,
                    else_,
                } => format!(
                    "(if! {} ({}){}{})",
                    cond,
                    join_things(body),
                    fmt_else_ifs(else_ifs),
                    if let Some(e) = else_ {
                        format!(" :else ({})", join_things(e))
                    } else {
                        "".to_string()
                    }
                ),
                Stmt::While { cond, body } => format!("(while! {} {})", cond, join_things(body)),
                Stmt::RepeatUntil { body, until_cond } =>
                    format!("(repeat! ({}) :until {})", join_things(body), until_cond),
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

fn fmt_else_ifs(else_ifs: &[(SpanExpr, Stmts)]) -> String {
    if !else_ifs.is_empty() {
        format!(
            " :else_ifs ({})",
            else_ifs
                .iter()
                .map(|(cond, body)| format!("({} {})", cond, join_things(body)))
                .collect::<Vec<_>>()
                .join(" ")
        )
    } else {
        "".to_string()
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

impl From<TokenKind> for BinOp {
    fn from(other: TokenKind) -> Self {
        match other {
            TokenKind::Add => BinOp::Add,
            TokenKind::Minus => BinOp::Minus,
            TokenKind::Multiply => BinOp::Multiply,
            TokenKind::Divide => BinOp::Divide,
            TokenKind::IntDiv => BinOp::IntDiv,
            TokenKind::Modulus => BinOp::Modulus,
            TokenKind::Equals => BinOp::Equals,
            TokenKind::NotEq => BinOp::NotEq,
            TokenKind::LessThan => BinOp::LessThan,
            TokenKind::GreaterThan => BinOp::GreaterThan,
            TokenKind::LessOrEq => BinOp::LessOrEq,
            TokenKind::GreaterOrEq => BinOp::GreaterOrEq,
            TokenKind::And => BinOp::And,
            TokenKind::Or => BinOp::Or,
            TokenKind::Not => BinOp::Not,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Minus => "-",
                BinOp::Multiply => "*",
                BinOp::Divide => "/",
                BinOp::IntDiv => "//",
                BinOp::Modulus => "%",
                BinOp::Equals => "==",
                BinOp::NotEq => "!=",
                BinOp::LessThan => "<",
                BinOp::GreaterThan => ">",
                BinOp::LessOrEq => "<=",
                BinOp::GreaterOrEq => ">=",
                BinOp::And => "and",
                BinOp::Or => "or",
                BinOp::Not => "not",
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
