use super::CharGroup;
use crate::source::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum Kind {
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Expected {}, found `{}`", .expected, .found)]
    ExpectedChar { expected: char, found: char },
    #[error("Expected {}", .expected)]
    ExpectedExact { expected: &'static str },
    #[error("Expected a {}, found `{}`", .expected, .found)]
    ExpectedCharGroup { expected: CharGroup, found: char },
    #[error("Unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("Invalid escape sequence")]
    InvalidEscapeSequence,
    #[error("Unrecognized character")]
    UnrecognizedCharacter,
}
impl Kind {
    pub fn with_length(self, length: usize) -> TokenError {
        TokenError { kind: self, length }
    }
    pub fn spanning(self, span: Span) -> Error {
        Error { kind: self, span }
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
#[error("{} from {} to {}", .kind, .span.start, .span.end)]
pub struct Error {
    pub kind: Kind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenError {
    pub kind: Kind,
    pub length: usize,
}
impl TokenError {
    pub fn at(self, index: usize) -> Error {
        Error {
            kind: self.kind,
            span: Span::start_at(index).with_length(self.length),
        }
    }
}
