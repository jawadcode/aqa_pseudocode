use crate::{
    ast::{Boxpr, Expr, Literal},
    lexer::token::TokenKind,
};

use super::{ParseResult, Parser, Spanned, SyntaxError, SyntaxResult};

const EXPR_TERMINATORS: [TokenKind; 6] = [
    TokenKind::RightParen,
    TokenKind::Newline,
    TokenKind::Comma,
    TokenKind::Then,
    TokenKind::Else,
    TokenKind::Eof,
];

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
            op @ Minus | op @ Not => self.parse_prefix_op(op)?,

            // Ident => self.parse_ident()?,
            // Fn => self.parse_closure()?,
            // If => self.parse_if_expr()?,
            // Match => self.parse_match_expr()?,
            // Do => self.parse_block_expr()?,
            // LeftParen => self.parse_grouping()?,
            _ => {
                let token = self.next_token()?;
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    token,
                });
            }
        };

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
                RightParen | Newline | Comma | Then | Else | Eof => break,

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

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr(0)
    }

    pub(crate) fn boxed_expr(&mut self) -> SyntaxResult<Boxpr> {
        self.parse_expr(0).map(Box::new)
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
                    println!(
                        "\nGot:    {}\nWanted: {}\n",
                        expr, $sexpr
                    );
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
}
