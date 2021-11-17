use crate::{
    ast::{Expr, Literal, SpanExpr},
    lexer::token::TokenKind,
};

use super::{ParseResult, Parser, Spanned, SyntaxError, SyntaxResult};

// const EXPR_TERMINATORS: [TokenKind; 7] = [
//     TokenKind::RightParen,
//     TokenKind::RightSquare,
//     TokenKind::Newline,
//     TokenKind::Comma,
//     TokenKind::Then,
//     TokenKind::Else,
//     TokenKind::Eof,
// ];

/// Trait with methods that return the binding power(s) for the operator it is called on
trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Minus => ((), 51),
            TokenKind::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        use TokenKind::*;
        Some(match self {
            Or => (1, 2),
            And => (3, 4),
            Equals | NotEq => (5, 6),
            LessThan | GreaterThan | LessOrEq | GreaterOrEq => (7, 8),
            Add | Minus => (9, 10),
            Multiply | Divide => (11, 12),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        // Some(match self {
        //     TokenKind::Question => (101, ()),
        //     _ => return None,
        // })
        None
    }
}

impl<'input> Parser<'input> {
    fn parse_expr(&mut self, binding_power: u8) -> ParseResult<Expr> {
        use TokenKind::*;
        let mut lhs = match self.peek() {
            lit @ IntLiteral
            | lit @ FloatLiteral
            | lit @ StringLiteral
            | lit @ True
            | lit @ False
            | lit @ Null => self.parse_lit(lit)?,
            Userinput => self.parse_userinput()?,
            LeftSquare => self.parse_list()?,

            op @ Minus | op @ Not => self.parse_prefix_op(op)?,

            Ident => self.parse_ident()?,
            LeftParen => self.parse_grouping()?,

            _ => {
                let token = self.next_token()?;
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    token,
                });
            }
        };

        if self.at(TokenKind::LeftSquare) {
            self.advance();
            let idx = self.expr()?;
            let rsquare = self.consume_next(TokenKind::RightSquare)?;
            lhs = Spanned {
                span: (lhs.span.start..rsquare.span.end).into(),
                node: Expr::Index {
                    list: Box::new(lhs),
                    index: Box::new(idx),
                },
            }
        }

        loop {
            let op = match self.peek() {
                op @ Add
                | op @ Minus
                | op @ Multiply
                | op @ Divide
                | op @ And
                | op @ Or
                | op @ LessThan
                | op @ GreaterThan
                | op @ Not
                | op @ LessOrEq
                | op @ GreaterOrEq
                | op @ NotEq
                | op @ Equals => op,
                RightParen | RightSquare | Newline | Comma | Then | Else | Eof | To => break,

                _ => {
                    let token = self.next_token()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "operator or expression terminator".to_string(),
                        token,
                    });
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    break;
                }

                let op_token = self.consume_next(op)?;
                lhs = Spanned {
                    span: (lhs.span.start..op_token.span.end).into(),
                    node: Expr::UnaryOp {
                        op: op.into(),
                        expr: Box::new(lhs),
                    },
                };
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    break;
                }

                self.consume(op)?;

                let rhs = self.parse_expr(right_binding_power)?;
                lhs = Spanned {
                    span: (lhs.span.start..rhs.span.end).into(),
                    node: Expr::BinaryOp {
                        op: op.into(),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                };
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    fn parse_lit(&mut self, lit: TokenKind) -> ParseResult<Expr> {
        // We can use .unwrap() because we've already checked the token before calling this method
        let token = self.next_token().unwrap();
        let text = self.text(token);

        Ok(Spanned {
            span: token.span,
            node: Expr::Literal(match lit {
                TokenKind::IntLiteral => Literal::Int(
                    text.parse()
                        .map_err(|_| SyntaxError::InvalidLiteral(token))?,
                ),
                TokenKind::FloatLiteral => Literal::Float(
                    text.parse()
                        .map_err(|_| SyntaxError::InvalidLiteral(token))?,
                ),
                TokenKind::StringLiteral => Literal::String(text[1..(text.len() - 1)].to_string()),
                TokenKind::True => Literal::Bool(true),
                TokenKind::False => Literal::Bool(false),
                TokenKind::Null => Literal::Null,
                _ => unreachable!(),
            }),
        })
    }

    fn parse_userinput(&mut self) -> ParseResult<Expr> {
        // Can be unwrapped because it's checked before calling
        let token = self.next_token().unwrap();
        Ok(Spanned {
            span: token.span,
            node: Expr::Userinput,
        })
    }

    fn parse_list(&mut self) -> ParseResult<Expr> {
        // We can .unwrap() because the next token is
        // guaranteed to be `LeftSquare`
        let lsquare = self.next_token().unwrap();
        let args = self.parse_csv(TokenKind::RightSquare)?;
        let rsquare = self.consume_next(TokenKind::RightSquare)?;

        Ok(Spanned {
            span: (lsquare.span.start..rsquare.span.end).into(),
            node: Expr::List(args),
        })
    }

    fn parse_prefix_op(&mut self, op: TokenKind) -> ParseResult<Expr> {
        let token = self.next_token()?;
        // Get right binding power of the operator,
        // we can unwrap because `op` is guaranteed to be a valid prefix operator
        // because of where it is called in `self.parse_expr`
        let ((), right_binding_power) = op.prefix_binding_power().unwrap();

        let expr = Box::new(self.parse_expr(right_binding_power)?);
        Ok(Spanned {
            span: (token.span.start..expr.span.end).into(),
            node: Expr::UnaryOp {
                op: op.into(),
                expr,
            },
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Expr> {
        // Can unwrap because we already know there is a token
        let token = self.next_token().unwrap();
        let text = self.text(token);

        Ok(if self.at(TokenKind::LeftParen) {
            self.advance();
            let args = self.parse_csv(TokenKind::RightParen)?;
            let rparen = self.consume_next(TokenKind::RightParen)?;

            Spanned {
                span: (token.span.start..rparen.span.end).into(),
                node: Expr::FnCall {
                    ident: text.to_string(),
                    args,
                },
            }
        } else {
            Spanned {
                span: token.span,
                node: Expr::Ident(text.to_string()),
            }
        })
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        self.advance();
        let expr = self.expr()?;
        self.consume(TokenKind::RightParen)?;
        Ok(expr)
    }

    pub(crate) fn parse_csv(&mut self, terminator: TokenKind) -> SyntaxResult<Vec<SpanExpr>> {
        let mut args = vec![];
        while !self.at(terminator) {
            let arg = self.expr()?;
            args.push(arg);

            if self.at(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_expr {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            println!("Sample: '{}'", $sample);
            match Parser::new(test).expr() {
                Ok(expr) => {
                    println!("\nGot:    {}\nWanted: {}\n", expr, $sexpr);
                    assert_eq!(expr.to_string(), $sexpr);
                }
                Err(err) => {
                    eprintln!("{:#?}", err);
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_op() {
        assert_expr!(
            "123 - 4.32e24 / 7893 + 3 * -789",
            "(+ (- 123 (/ 4320000000000000000000000 7893)) (* 3 (- 789)))"
        );
    }

    fn parse_userinput() {
        assert_expr!("(((USERINPUT)))", "(userinput!)");
    }

    #[test]
    fn parse_list() {
        assert_expr!(
            "[0, 1, 2 * 8 + 3, 89 * -79]",
            "(list! 0 1 (+ (* 2 8) 3) (* 89 (- 79)))"
        );
    }

    #[test]
    fn parse_ident() {
        assert_expr!(
            r#"hello + world == "hello world""#,
            r#"(== (+ hello world) "hello world")"#
        );
    }

    #[test]
    fn parse_grouping() {
        assert_expr!(
            "123 - 4.32e24 / (7893 + 3) * -789",
            "(- 123 (* (/ 4320000000000000000000000 (+ 7893 3)) (- 789)))"
        );
    }

    #[test]
    fn parse_fncall() {
        assert_expr!(
            r#"say_hello("Jawad", "[REDACTED]") == "Hello Jawad [REDACTED]!""#,
            r#"(== (say_hello "Jawad" "[REDACTED]") "Hello Jawad [REDACTED]!")"#
        );
    }

    #[test]
    fn parse_indexing() {
        assert_expr!(
            "create_range(0, 123)[64 + 3] == 67",
            "(== (index! (create_range 0 123) (+ 64 3)) 67)"
        );
    }
}
