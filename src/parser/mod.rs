pub mod expr;

use crate::lexer::{Lexer, token::TokenKind, types::{Span, Token}};

use std::{fmt, iter::Peekable};

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T: fmt::Display> {
    pub span: Span,
    pub node: T,
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken {
        expected: String,
        token: Token,
    },
    InvalidLiteral(Token),
    UnexpectedEndOfInput(Token),
    InvalidToken(Token),
    /// Not actually an error 🤫
    End,
}

type SyntaxResult<T> = Result<T, SyntaxError>;
pub type ParseResult<T> = Result<Spanned<T>, SyntaxError>;

pub struct Parser<'input> {
    input: &'input str,
    tokens: Peekable<Lexer<'input>>,
}

impl<'input> Parser<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            tokens: Lexer::new(input).peekable(),
        }
    }

    /// Proceed by one token
    pub fn advance(&mut self) {
        self.tokens.next();
    }

    /// Get next token, returning an `UnexpectedEndOfInput` if the lexer returns `None`
    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.tokens.next().ok_or_else(|| {
            SyntaxError::UnexpectedEndOfInput(Token {
                kind: TokenKind::Eof,
                span: (self.input.len()..self.input.len()).into(),
            })
        })
    }

    /// Get the source text of a given token
    pub fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }

    /// Look ahead to the next token without consuming it
    pub fn peek(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::Eof)
    }

    /// Peek at the next token and check if its `TokenKind` is `kind`
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Peek at the next token and check if its `kind` is `a` or `b`
    pub fn at_either(&mut self, a: TokenKind, b: TokenKind) -> bool {
        let x = self.peek();
        x == a || x == b
    }

    /// Peek at the next token and check if it is one of many tokenkinds
    pub fn at_any(&mut self, kinds: &'static [TokenKind]) -> bool {
        kinds.contains(&self.peek())
    }

    /// Consume token and check that it's `TokenKind` is as `expected`
    pub fn consume(&mut self, expected: TokenKind) -> SyntaxResult<()> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                token,
            })
        } else {
            Ok(())
        }
    }

    /// Returns the next token but also checks that it is as `expected`
    pub fn consume_next(&mut self, expected: TokenKind) -> SyntaxResult<Token> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                token,
            })
        } else {
            Ok(token)
        }
    }
}
