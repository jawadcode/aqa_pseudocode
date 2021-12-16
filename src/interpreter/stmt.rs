use std::ops::Range;

use crate::{
    ast::{SpanExpr, SpanStmt},
    interpreter::RuntimeResult,
    lexer::types::Span,
    parser::Spanned,
    visitor::Visitor,
};

use super::{
    value::{Subroutine, Value},
    Interpreter, SpannedRuntimeResult,
};

impl<'input> Interpreter<'_> {
    pub fn visit_output(&mut self, value: &SpanExpr) -> SpannedRuntimeResult<()> {
        let value = self.visit_expr(value)?;
        println!("{}", value);
        Ok(())
    }

    pub fn visit_assign(&mut self, ident: &str, value: &SpanExpr) -> SpannedRuntimeResult<()> {
        let value = self.visit_expr(value)?;
        self.env.set_variable(ident, &value);
        Ok(())
    }

    pub fn visit_sub_def(
        &mut self,
        ident: &str,
        params: &[String],
        body: &[SpanStmt],
    ) -> SpannedRuntimeResult<()> {
        self.env.set_variable(
            ident,
            &Value::Subroutine(Subroutine {
                ident: ident.to_string(),
                params: params.to_vec(),
                body: body.to_vec(),
            }),
        );
        Ok(())
    }

    pub fn visit_sub_call(
        &mut self,
        span: Span,
        ident: &str,
        args: &[SpanExpr],
    ) -> SpannedRuntimeResult<()> {
        self.visit_fncall(span, ident, args).map(|_| ())
    }

    pub fn visit_if_stmt(
        &mut self,
        cond: &SpanExpr,
        body: &[SpanStmt],
        else_ifs: &[(SpanExpr, Vec<SpanStmt>)],
        else_: &Option<Vec<SpanStmt>>,
    ) -> SpannedRuntimeResult<()> {
        self.env.push_scope();
        let cond = self.visit_expr(cond)?;
        if cond.into() {
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            self.env.pop_scope();
            return Ok(());
        }

        self.env.pop_scope();

        for (cond, body) in else_ifs {
            self.env.push_scope();
            let cond = self.visit_expr(cond)?;
            if cond.into() {
                for stmt in body {
                    self.visit_stmt(stmt)?;
                }
                self.env.pop_scope();
                return Ok(());
            }
            self.env.pop_scope();
        }

        if let Some(body) = else_ {
            self.env.push_scope();
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            self.env.pop_scope();
        }

        Ok(())
    }

    pub fn visit_for_loop(
        &mut self,
        counter: &str,
        range: &Range<SpanExpr>,
        body: &[SpanStmt],
    ) -> SpannedRuntimeResult<()> {
        let start = self.visit_expr(&range.start)?;
        let end = self.visit_expr(&range.end)?;

        let start = RuntimeResult::<f64>::from(&start)
            .map_err(|e| Spanned {
                span: range.start.span,
                node: e,
            })?
            .floor() as i64;
        let end = RuntimeResult::<f64>::from(&end)
            .map_err(|e| Spanned {
                span: range.end.span,
                node: e,
            })?
            .ceil() as i64;

        for i in start..=end {
            self.env.set_variable(counter, &Value::Number(i as f64));
            self.env.push_scope();
            for stmt in body {
                self.visit_stmt(stmt)?;
            }
            self.env.pop_scope();
        }

        Ok(())
    }
}
