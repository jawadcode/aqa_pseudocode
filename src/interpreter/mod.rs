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

#[derive(Debug)]
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

    pub fn run(&mut self, ast: &[SpanStmt]) {
        for stmt in ast {
            if let Err(e) = self.visit_stmt(stmt) {
                eprintln!("{:#?}", &self.env);
                panic!("{:#?}", e);
            }
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
            Stmt::SubCall { ident, args } => self.visit_sub_call(s.span, ident, args),
            Stmt::If {
                cond,
                body,
                else_ifs,
                else_,
            } => self.visit_if_stmt(cond, body, else_ifs, else_),
            Stmt::While { cond, body } => self.visit_while_loop(cond, body),
            Stmt::RepeatUntil { body, until_cond } => self.visit_repeat_until(body, until_cond),
            Stmt::For {
                counter,
                range,
                body,
            } => self.visit_for_loop(counter, range, body),
            Stmt::Return { expr } => Err(Spanned {
                span: s.span,
                node: RuntimeError::Return {
                    value: self.visit_expr(expr)?,
                },
            }),
        }
    }

    fn visit_expr(&mut self, e: &SpanExpr) -> SpannedValueResult {
        match &e.node {
            Expr::Ident(ident) => self.visit_ident(e.span, ident),
            Expr::Literal(lit) => Ok(self.visit_literal(lit)),
            Expr::List(_) => todo!(),
            Expr::Userinput => Ok(self.visit_userinput()),
            Expr::UnaryOp { op, expr } => self.visit_unary_op(e.span, op, expr),
            Expr::BinaryOp { op, lhs, rhs } => self.visit_binary_op(e.span, op, lhs, rhs),
            Expr::FnCall { ident, args } => self.visit_fncall(e.span, ident, args),
            Expr::Index { list, index } => todo!(),
        }
    }
}
