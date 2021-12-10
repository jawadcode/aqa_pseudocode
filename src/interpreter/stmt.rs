use crate::{
    ast::{SpanExpr, Stmts},
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
        body: &Stmts,
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
}
