use crate::{
    ast::{Stmt, Stmts},
    lexer::{token::TokenKind, types::Token},
};

use super::{ParseResult, Parser, Spanned, SyntaxError, SyntaxResult};

impl<'input> Parser<'input> {
    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek() {
            TokenKind::Ident => self.parse_ident_stmt(),
            TokenKind::Output => self.parse_output(),
            TokenKind::Subroutine => self.parse_subroutine(),
            TokenKind::While => self.parse_while(),
            TokenKind::Repeat => self.parse_repeat_until(),
            TokenKind::For => self.parse_for(),
            TokenKind::Newline => {
                self.advance();
                self.stmt()
            }
            __ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "statement".to_string(),
                    token,
                })
            }
        }
    }

    fn parse_ident_stmt(&mut self) -> ParseResult<Stmt> {
        // Can .unwrap() because the token is guaranteed to be `Ident`
        let token = self.next_token().unwrap();
        let text = self.text(token);
        match self.peek() {
            TokenKind::Assign => {
                self.advance();
                let value = self.expr()?;
                self.consume(TokenKind::Newline)?;

                Ok(Spanned {
                    span: (token.span.start..value.span.end).into(),
                    node: Stmt::Assign {
                        ident: text.to_string(),
                        value,
                    },
                })
            }
            TokenKind::LeftSquare => self.parse_list_assign(token, text),
            _ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "'←', '[' or '('".to_string(),
                    token,
                })
            }
        }
    }

    fn parse_list_assign(&mut self, token: Token, text: &str) -> ParseResult<Stmt> {
        self.advance();
        let index = self.expr()?;
        self.consume(TokenKind::RightSquare)?;
        let indices = match self.peek() {
            TokenKind::Assign => vec![index],
            TokenKind::LeftSquare => {
                let mut idxs = vec![index];
                while self.at(TokenKind::LeftSquare) {
                    self.advance();
                    let idx = self.expr()?;
                    self.consume(TokenKind::RightSquare)?;
                    idxs.push(idx);
                }
                idxs
            }
            __ => {
                let token = self.next_token()?;
                return Err(SyntaxError::UnexpectedToken {
                    expected: "'←' or '['".to_string(),
                    token,
                });
            }
        };
        dbg!(self.peek());
        self.consume(TokenKind::Assign)?;
        let value = self.expr()?;
        self.consume(TokenKind::Newline)?;

        Ok(Spanned {
            span: (token.span.start..value.span.end).into(),
            node: Stmt::ListAssign {
                list_ident: text.to_string(),
                indices,
                value,
            },
        })
    }

    fn parse_output(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let value = self.expr()?;
        self.consume(TokenKind::Newline)?;
        Ok(Spanned {
            span: value.span,
            node: Stmt::Output(value),
        })
    }

    fn parse_subroutine(&mut self) -> ParseResult<Stmt> {
        self.advance();

        let ident = self.consume_next(TokenKind::Ident)?;
        let text = self.text(ident);
        self.consume(TokenKind::LeftParen)?;

        let mut params = vec![];
        while !self.at(TokenKind::RightParen) {
            let param = {
                let tok = self.consume_next(TokenKind::Ident)?;
                self.text(tok).to_string()
            };
            params.push(param);

            if self.at(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.consume(TokenKind::RightParen)?;
        self.consume(TokenKind::Newline)?;

        let body = self.parse_stmts(TokenKind::EndSub)?;
        let endsub = self.consume_next(TokenKind::EndSub)?;
        self.consume(TokenKind::Newline)?;

        Ok(Spanned {
            span: (ident.span.start..endsub.span.end).into(),
            node: Stmt::SubDef {
                ident: text.to_string(),
                params,
                body,
            },
        })
    }

    fn parse_while(&mut self) -> ParseResult<Stmt> {
        let token = self.next_token()?;
        let cond = self.expr()?;
        self.consume(TokenKind::Newline)?;
        let body = self.parse_stmts(TokenKind::EndWhile)?;
        let end = self.consume_next(TokenKind::EndWhile)?;
        self.consume(TokenKind::Newline)?;

        Ok(Spanned {
            span: (token.span.start..end.span.end).into(),
            node: Stmt::While { cond, body },
        })
    }

    fn parse_repeat_until(&mut self) -> ParseResult<Stmt> {
        let token = self.next_token()?;
        self.consume(TokenKind::Newline)?;
        let body = self.parse_stmts(TokenKind::Until)?;
        self.consume(TokenKind::Until)?;
        let until_cond = self.expr()?;
        self.consume(TokenKind::Newline)?;

        Ok(Spanned {
            span: (token.span.start..until_cond.span.end).into(),
            node: Stmt::RepeatUntil { body, until_cond },
        })
    }

    fn parse_for(&mut self) -> ParseResult<Stmt> {
        let token = self.next_token()?;
        let counter = {
            let tok = self.consume_next(TokenKind::Ident)?;
            self.text(tok).to_string()
        };
        self.consume(TokenKind::Assign)?;
        let start = self.expr()?;
        self.consume(TokenKind::To)?;
        let end = self.expr()?;
        self.consume(TokenKind::Newline)?;
        let body = self.parse_stmts(TokenKind::EndFor)?;
        let endfor = self.consume_next(TokenKind::EndFor)?;
        self.consume(TokenKind::Newline)?;

        Ok(Spanned {
            span: (token.span.start..endfor.span.end).into(),
            node: Stmt::For {
                counter,
                range: start..end,
                body,
            },
        })
    }

    fn parse_stmts(&mut self, terminator: TokenKind) -> SyntaxResult<Stmts> {
        let mut body = vec![];
        while !self.at(terminator) {
            body.push(self.stmt()?);
        }

        Ok(body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_stmt {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            println!("Sample: '{}'", $sample);
            match Parser::new(test).stmt() {
                Ok(stmt) => {
                    println!("\nGot:    {}\nWanted: {}\n", stmt, $sexpr);
                    assert_eq!(stmt.to_string(), $sexpr);
                }
                Err(err) => {
                    eprintln!("{:#?}", err);
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_assign() {
        assert_stmt!(
            "a_thing ← thing(stuff) + more_stuff\n",
            "(set! a_thing (+ (thing stuff) more_stuff))"
        );
    }

    #[test]
    fn parse_list_assign() {
        assert_stmt!(
            "matrix[a][b] ← list2[c]\n",
            "(list_set! matrix [a b] (index! list2 c))"
        );
    }

    #[test]
    fn parse_subdef() {
        assert_stmt!(
            r#"
SUBROUTINE hello(firstname)
    surname ← USERINPUT
    name ← firstname + " " + surname
    OUTPUT "Hello " + name
ENDSUBROUTINE
"#,
            r#"(define! hello [firstname] ((set! surname (userinput!)) (set! name (+ (+ firstname " ") surname)) (output! (+ "Hello " name))))"#
        );
    }

    #[test]
    fn parse_while() {
        assert_stmt!(
            "
SUBROUTINE main()
    number ← STRING_TO_INT(USERINPUT)
    WHILE number < 0 AND number > 10
        number ← STRING_TO_INT(USERINPUT)
    ENDWHILE
    OUTPUT number
ENDSUBROUTINE
",
            "(define! main [] ((set! number (STRING_TO_INT (userinput!))) (while! (and (< number 0) (> number 10)) (set! number (STRING_TO_INT (userinput!)))) (output! number)))"
        );
    }
}
