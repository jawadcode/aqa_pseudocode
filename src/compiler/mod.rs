use crate::{
    ast::{BinOp, Expr, SpanExpr, Stmt, Stmts},
    lexer::types::Token,
    vm::{value::Value, Chunk, Op},
};

struct Compiler {
    ast: Stmts,
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    chunk: Chunk,
}

struct Local {
    name: Token,
    depth: usize,
}

impl Compiler {
    fn new(ast: Stmts) -> Self {
        Self {
            ast,
            locals: vec![],
            local_count: 0,
            scope_depth: 0,
            chunk: Chunk::default(),
        }
    }

    fn compile(&mut self) -> Chunk {
        for stmt in &self.ast {
            match &stmt.node {
                Stmt::Output(expr) => todo!(),
                Stmt::Assign { ident, value } => {
                    self.chunk.consts.push(Value::String(ident.to_string()));
                    let constidx = self.chunk.consts.len() - 1;
                }
                Stmt::ListAssign {
                    list_ident,
                    indices,
                    value,
                } => todo!(),
                Stmt::SubDef {
                    ident,
                    params,
                    body,
                } => todo!(),
                Stmt::Return { expr } => todo!(),
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
            }
        }

        self.chunk.clone()
    }

    fn compile_expr(&mut self, expr: &SpanExpr) {
        match &expr.node {
            Expr::Ident(ident) => todo!(),
            Expr::Literal(lit) => todo!(),
            Expr::List(list) => todo!(),
            Expr::Userinput => todo!(),
            Expr::UnaryOp { op, expr } => {
                self.compile_expr(expr);
                match op {
                    // Not actually Binary Ops but don't worry about it :)
                    BinOp::Not => self.push_op(Op::Not),
                    BinOp::Minus => self.push_op(Op::Neg),
                    _ => unreachable!(),
                }
            }
            Expr::BinaryOp { op, lhs, rhs } => todo!(),
            Expr::FnCall { ident, args } => todo!(),
            Expr::Index { list, index } => todo!(),
        }
    }

    #[inline]
    fn push_op(&mut self, op: Op) {
        self.chunk.ops.push(op);
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
    }
}
