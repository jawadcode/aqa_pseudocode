use std::{
    cmp::Ordering,
    io,
    ops::{Neg, Not},
};

use crate::{
    ast::{Literal, Op, SpanExpr, Stmt},
    interpreter::{value::Value, Type},
    lexer::types::Span,
    parser::Spanned,
    visitor::Visitor,
};

use super::{
    errors::RuntimeError,
    value::{BuiltinFn, Subroutine},
    Interpreter, SpannedValueResult,
};

impl<'input> Interpreter<'_> {
    pub fn visit_ident(&mut self, span: Span, ident: &str) -> SpannedValueResult {
        self.env
            .get_variable(ident)
            .map_err(|e| Spanned { span, node: e })
    }

    pub fn visit_literal(&mut self, lit: &Literal) -> Value {
        match lit {
            Literal::String(s) => Value::String(s.to_string()),
            Literal::Int(i) => Value::Number(*i as f64),
            Literal::Float(f) => Value::Number(*f),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Null => Value::Null,
        }
    }

    pub fn visit_userinput(&mut self) -> Value {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        Value::String(input.trim().into())
    }

    pub fn visit_unary_op(&mut self, span: Span, op: &Op, expr: &SpanExpr) -> SpannedValueResult {
        let value = self.visit_expr(expr)?;
        match op {
            Op::Not => !value,
            Op::Minus => -value,
            _ => unreachable!(),
        }
        .map_err(|e| Spanned { span, node: e })
    }

    pub fn visit_binary_op(
        &mut self,
        span: Span,
        op: &Op,
        lhs: &SpanExpr,
        rhs: &SpanExpr,
    ) -> SpannedValueResult {
        let lhs = self.visit_expr(lhs)?;
        use Op::*;
        match op {
            LessThan | GreaterThan | LessOrEq | GreaterOrEq | Equals | NotEq => {
                let rhs = self.visit_expr(rhs)?;
                self.visit_comparison(span, op, &lhs, &rhs)
            }
            Add | Minus | Multiply | Divide => {
                let rhs = self.visit_expr(rhs)?;
                self.visit_arithmetic_op(span, op, lhs, rhs)
            }
            And | Or => self.visit_short_circuiting_op(op, lhs, rhs),
            _ => unreachable!(),
        }
    }

    pub fn visit_comparison(
        &mut self,
        span: Span,
        op: &Op,
        lhs: &Value,
        rhs: &Value,
    ) -> SpannedValueResult {
        let ordering = lhs.partial_cmp(rhs).ok_or(Spanned {
            span,
            node: RuntimeError::CannotCompare {
                lhs: lhs.into(),
                rhs: rhs.into(),
            },
        })?;

        use Op::*;
        Ok(Value::Bool(matches!(
            (op, ordering),
            (LessThan | LessOrEq, Ordering::Less)
                | (GreaterThan | GreaterOrEq, Ordering::Greater)
                | (Equals | LessOrEq | GreaterOrEq, Ordering::Equal)
                | (NotEq, Ordering::Less | Ordering::Greater)
        )))
    }

    pub fn visit_arithmetic_op(
        &mut self,
        span: Span,
        op: &Op,
        lhs: Value,
        rhs: Value,
    ) -> SpannedValueResult {
        match op {
            Op::Add => lhs + rhs,
            Op::Minus => lhs - rhs,
            Op::Multiply => lhs * rhs,
            Op::Divide => lhs / rhs,
            _ => unreachable!(),
        }
        .map_err(|e| Spanned { span, node: e })
    }

    pub fn visit_short_circuiting_op(
        &mut self,
        op: &Op,
        lhs: Value,
        rhs: &SpanExpr,
    ) -> SpannedValueResult {
        Ok(Value::Bool(match (op, bool::from(lhs)) {
            (Op::And, false) => false,
            (Op::And, true) => bool::from(self.visit_expr(rhs)?),
            (Op::Or, true) => true,
            (Op::Or, false) => bool::from(self.visit_expr(rhs)?),
            _ => unreachable!(),
        }))
    }

    pub fn visit_fncall(
        &mut self,
        span: Span,
        ident: &str,
        args: &[SpanExpr],
    ) -> SpannedValueResult {
        let fun = self
            .env
            .get_variable(ident)
            .map_err(|e| Spanned { span, node: e })?;

        match fun {
            Value::Subroutine(sr) => self.visit_subroutine(sr, args),
            Value::BuiltinFn(bf) => self.visit_builtin_fn(bf, args),
            _ => Err(Spanned {
                span,
                node: RuntimeError::WrongType {
                    expected: Type::Subroutine,
                    got: (&fun).into(),
                },
            }),
        }
    }

    pub fn visit_subroutine(&mut self, sr: Subroutine, args: &[SpanExpr]) -> SpannedValueResult {
        self.env.push_scope();
        for (param, arg) in sr.params.iter().zip(args) {
            let value = self.visit_expr(arg)?;
            self.env.set_variable(param, &value);
        }

        let mut return_value = Value::Null;
        for stmt in sr.body {
            if let Stmt::Return { expr } = stmt.node {
                return_value = self.visit_expr(&expr)?;
                break;
            } else {
                self.visit_stmt(&stmt)?;
            }
        }

        Ok(return_value)
    }

    pub fn visit_builtin_fn(&mut self, bf: BuiltinFn, args: &[SpanExpr]) -> SpannedValueResult {
        let mut args_values = vec![];
        for arg in args {
            args_values.push(self.visit_expr(arg)?);
        }

        self.env.push_scope();
        let return_value = (bf.body)(self, &args_values);
        self.env.pop_scope();
        Ok(return_value)
    }
}
