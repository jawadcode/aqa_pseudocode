use logos::Logos;
use std::fmt;

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum LogosToken {
    /* KEYWORDS */
    #[token("SUBROUTINE")]
    #[token("subroutine")]
    Subroutine,
    #[token("ENDSUBROUTINE")]
    #[token("endsubroutine")]
    EndSub,
    // #[token("FUNCTION")]
    // #[token("function")]
    // Function,
    // #[token("ENDFUNCTION")]
    // #[token("endfunction")]
    // EndFun,
    #[token("RETURN")]
    #[token("return")]
    Return,
    #[token("IF")]
    #[token("if")]
    If,
    #[token("THEN")]
    #[token("then")]
    Then,
    #[token("ELSE")]
    #[token("else")]
    Else,
    #[token("ELSE IF")]
    #[token("else if")]
    ElseIf,
    #[token("ENDIF")]
    #[token("endif")]
    Endif,
    #[token("WHILE")]
    #[token("while")]
    While,
    #[token("ENDWHILE")]
    #[token("endwhile")]
    EndWhile,
    #[token("FOR")]
    #[token("for")]
    For,
    #[token("TO")]
    #[token("to")]
    To,
    #[token("ENDFOR")]
    #[token("endfor")]
    EndFor,
    #[token("REPEAT")]
    #[token("repeat")]
    Repeat,
    #[token("UNTIL")]
    #[token("until")]
    Until,
    #[token("CONSTANT")]
    #[token("constant")]
    Constant,
    #[token("OUTPUT")]
    #[token("output")]
    Output,
    #[token("USERINPUT")]
    #[token("userinput")]
    Userinput,
    /* ARITHMETIC OPERATORS */
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("DIV")]
    IntDiv,
    #[token("MOD")]
    Modulus,
    /* RELATIONAL OPERATORS */
    #[token("=")]
    #[token("==")]
    Equals,
    #[token("!=")]
    #[token("≠")]
    NotEq,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    #[token("≤")]
    #[token("⩽")]
    LessOrEq,
    #[token(">=")]
    #[token("≥")]
    #[token("⩾")]
    GreaterOrEq,
    /* BOOLEAN OPERATORS */
    #[token("AND")]
    And,
    #[token("OR")]
    Or,
    #[token("NOT")]
    Not,
    /* VALUES */
    #[regex(r"([A-Za-z]|_)([A-Za-z]|_|\d)*")]
    Ident,
    #[regex(r"[0-9]+", priority = 2)]
    IntLiteral,
    #[regex(r"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?", priority = 1)]
    FloatLiteral,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    #[regex(r#"'((\\'|\\\\)|[^\\'])*'"#)]
    StringLiteral,
    #[token("True")]
    #[token("true")]
    True,
    #[token("False")]
    #[token("false")]
    False,
    #[token("Null")]
    #[token("null")]
    Null,
    /* MISCELLANEOUS */
    #[regex(r"#.*")]
    Comment,
    #[token("←")]
    #[token("🠔")]
    #[token("🠐")]
    #[token("⟵")]
    #[token("<-")]
    Assign,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,
    #[token(",")]
    Comma,
    /* SPECIAL */
    #[regex("(\r\n|\n)+")]
    Newline,
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    /* KEYWORDS */
    Subroutine,
    EndSub,
    // Function,
    // EndFun,
    Return,
    If,
    Then,
    Else,
    ElseIf,
    Endif,
    While,
    EndWhile,
    For,
    To,
    EndFor,
    Repeat,
    Until,
    Constant,
    Output,
    Userinput,
    /* ARITHMETIC OPERATORS */
    Add,
    Minus,
    Multiply,
    Divide,
    IntDiv,
    Modulus,
    /* RELATIONAL OPERATORS */
    Equals,
    NotEq,
    LessThan,
    GreaterThan,
    LessOrEq,
    GreaterOrEq,
    /* BOOLEAN OPERATORS */
    And,
    Or,
    Not,
    /* VALUES */
    Ident,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    True,
    False,
    Null,
    /* MISCELLANEOUS */
    Comment,
    Assign,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Comma,
    /* SPECIAL */
    Newline,
    Error,
    Eof,
}

impl From<LogosToken> for TokenKind {
    fn from(other: LogosToken) -> Self {
        use LogosToken::*;
        match other {
            Subroutine => Self::Subroutine,
            EndSub => Self::EndSub,
            // Function => Self::Function,
            // EndFun => Self::EndFun,
            Return => Self::Return,
            If => Self::If,
            Then => Self::Then,
            Else => Self::Else,
            ElseIf => Self::ElseIf,
            Endif => Self::Endif,
            While => Self::While,
            EndWhile => Self::EndWhile,
            For => Self::For,
            To => Self::To,
            EndFor => Self::EndFor,
            Repeat => Self::Repeat,
            Until => Self::Until,
            Constant => Self::Constant,
            Output => Self::Output,
            Userinput => Self::Userinput,
            Equals => Self::Equals,
            NotEq => Self::NotEq,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessOrEq => Self::LessOrEq,
            GreaterOrEq => Self::GreaterOrEq,
            Plus => Self::Add,
            Minus => Self::Minus,
            Multiply => Self::Multiply,
            Divide => Self::Divide,
            IntDiv => Self::IntDiv,
            Modulus => Self::Modulus,
            And => Self::And,
            Or => Self::Or,
            Not => Self::Not,
            Ident => Self::Ident,
            IntLiteral => Self::IntLiteral,
            FloatLiteral => Self::FloatLiteral,
            StringLiteral => Self::StringLiteral,
            True => Self::True,
            False => Self::False,
            Null => Self::Null,
            Comment => Self::Comment,
            Assign => Self::Assign,
            LeftParen => Self::LeftParen,
            RightParen => Self::RightParen,
            LeftSquare => Self::LeftSquare,
            RightSquare => Self::RightSquare,
            Comma => Self::Comma,
            Newline => Self::Newline,
            Error => Self::Error,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        write!(
            f,
            "{}",
            match self {
                Subroutine => "'SUBROUTINE'",
                EndSub => "'ENDSUB'",
                // Function => "'FUNCTION'",
                // EndFun => "'ENDFUNCTION'",
                Return => "'RETURN'",
                If => "'IF'",
                Then => "'THEN'",
                Else => "'ELSE'",
                ElseIf => "'ELSE IF'",
                Endif => "'ENDIF'",
                While => "'WHILE'",
                EndWhile => "'ENDWHILE'",
                For => "'FOR'",
                To => "'TO'",
                EndFor => "'ENDFOR'",
                Repeat => "'REPEAT'",
                Until => "'UNTIL'",
                Constant => "'CONSTANT'",
                Output => "'OUTPUT'",
                Userinput => "'USERINPUT'",
                Add => "'+'",
                Minus => "'-'",
                Multiply => "'*'",
                Divide => "'/'",
                IntDiv => "'DIV'",
                Modulus => "'MOD'",
                Equals => "==",
                NotEq => "≠",
                LessThan => "<",
                GreaterThan => ">",
                LessOrEq => "≤",
                GreaterOrEq => "≥",
                And => "'AND'",
                Or => "'OR'",
                Not => "'NOT'",
                Ident => "identifier",
                IntLiteral => "integer literal",
                FloatLiteral => "float literal",
                StringLiteral => "string literal",
                True => "'True'",
                False => "'False'",
                Null => "'Null'",
                Comment => "comment",
                Assign => "←",
                LeftParen => "'('",
                RightParen => "')'",
                LeftSquare => "'['",
                RightSquare => "']'",
                Comma => ",",
                Newline => "newline",
                Error => "invalid token",
                Eof => "EOF",
            }
        )
    }
}
