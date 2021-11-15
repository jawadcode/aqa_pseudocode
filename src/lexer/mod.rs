pub mod token;
pub mod types;

use token::*;
use types::*;

use logos::Logos;

pub struct Lexer<'input> {
    input: &'input str,
    generated: logos::SpannedIter<'input, LogosToken>,
    eof: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.generated.next() {
            Some((token, span)) => Some(Token {
                kind: TokenKind::from(token),
                span: span.into(),
            }),
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Token {
                    kind: TokenKind::Eof,
                    span: (self.input.len() - 1..self.input.len() - 1).into(),
                })
            }
        }
    }
}

/// This is just for demonstrational purposes
/// and by no means exhaustive, too lazy for that :)
#[test]
fn lexer_test() {
    use TokenKind::*;

    let input = r#"# This subroutine does things and stuff
SUBROUTINE hello(name)
    hello <- "Hello"
    space <- " "
    OUTPUT hello + space + name
ENDSUBROUTINE"#;
    let tokens = Lexer::new(input).collect::<Vec<_>>();
    let wanted = [
        Comment,
        Newline,
        Subroutine,
        Ident,
        LeftParen,
        Ident,
        RightParen,
        Newline,
        Ident,
        Assign,
        StringLiteral,
        Newline,
        Ident,
        Assign,
        StringLiteral,
        Newline,
        Output,
        Ident,
        Add,
        Ident,
        Add,
        Ident,
        Newline,
        EndSub,
        Eof,
    ];
    assert_eq!(
        wanted.to_vec(),
        tokens.iter().map(|t| t.kind).collect::<Vec<_>>()
    );
}
