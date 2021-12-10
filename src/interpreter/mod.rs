pub mod expr;
pub mod stmt;

use crate::{
    ast::{Expr, SpanExpr, SpanStmt, Stmt},
    parser::Spanned,
    visitor::Visitor,
};

use self::{env::Env, errors::RuntimeError, value::Value};

pub mod env;
pub mod errors;
pub mod value;

pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type ValueResult = RuntimeResult<Value>;
pub type SpannedRuntimeResult<T> = Result<T, Spanned<RuntimeError>>;
pub type SpannedValueResult = SpannedRuntimeResult<Value>;

pub enum Type {
    Subroutine,
    BuiltinFn,
    String,
    Number,
    Boolean,
    Null,
}

pub struct Interpreter<'input> {
    source: &'input str,
    env: Env,
}

impl<'input> Interpreter<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            source,
            env: Default::default(),
        }
    }
}

impl<'input> Visitor<SpannedValueResult, SpannedRuntimeResult<()>> for Interpreter<'input> {
    fn visit_stmt(&mut self, s: &SpanStmt) -> SpannedRuntimeResult<()> {
        match &s.node {
            Stmt::Output(value) => self.visit_output(value),
            Stmt::Assign { ident, value } => self.visit_assign(ident, value),
            Stmt::ListAssign {
                list_ident,
                indices,
                value,
            } => todo!(),
            Stmt::SubDef {
                ident,
                params,
                body,
            } => self.visit_sub_def(ident, params, body),
            Stmt::SubCall { ident, args } => todo!(),
            Stmt::If {
                cond,
                body,
                else_ifs,
                else_,
            } => todo!(),
            Stmt::While { cond, body } => todo!(),
            Stmt::RepeatUntil { body, until_cond } => todo!(),
            Stmt::For {
                counter,
                range,
                body,
            } => todo!(),
            Stmt::Return { expr } => todo!(),
        }
    }

    fn visit_expr(&mut self, e: &SpanExpr) -> SpannedValueResult {
        match &e.node {
            Expr::Ident(_) => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Userinput => todo!(),
            Expr::UnaryOp { op, expr } => todo!(),
            Expr::BinaryOp { op, lhs, rhs } => todo!(),
            Expr::FnCall { ident, args } => todo!(),
            Expr::Index { list, index } => todo!(),
        }
    }
}
