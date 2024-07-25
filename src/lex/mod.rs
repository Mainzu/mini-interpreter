use std::{fmt, result};

use thiserror::Error;

use crate::{
    source::{export::SourceCursor, Span, TokenStarting},
    token::{
        self,
        export::{TokenCore, TokenFull},
    },
};

pub mod error;
pub type Result<T> = result::Result<T, Error>;
pub use error::{Error, TokenError};

mod implementations;
mod iter;

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum LexingState {
//     BeforeFirstToken,
//     TokenStart,
//     TokenEnded,
//     FileEnded,
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharGroup {
    IdentStart,
    IdentCont,
    Digit,
}
impl fmt::Display for CharGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IdentStart => write!(f, "identifier start"),
            Self::IdentCont => write!(f, "identifier continuation"),
            Self::Digit => write!(f, "digit"),
        }
    }
}

pub trait Lex: Sized {
    type Value: fmt::Debug + Clone + PartialEq;

    fn can_start_with(lead: char) -> bool;
    fn try_lex(src: TokenStarting<'_>) -> Result<()>;

    unsafe fn lex_value(src: TokenStarting<'_>) -> Self::Value {
        Self::try_lex_value(src).unwrap_unchecked()
    }
    unsafe fn lex_length(src: TokenStarting<'_>) -> usize {
        Self::try_lex_length(src).unwrap_unchecked()
    }

    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>>;
    fn try_lex_value(src: TokenStarting<'_>) -> Result<Self::Value> {
        Self::try_lex_content(src).map(|fragment| fragment.value)
    }
    fn try_lex_length(src: TokenStarting<'_>) -> Result<usize> {
        Self::try_lex_content(src).map(|fragment| fragment.length)
    }
}

pub trait StaticLex {
    const BYTES: &[u8];
    const STR: &'static str = unsafe { std::str::from_utf8_unchecked(Self::BYTES) };
    fn length() -> usize {
        Self::BYTES.len()
    }
}

impl<T: StaticLex> Lex for T {
    type Value = ();

    fn can_start_with(lead: char) -> bool {
        Self::STR.starts_with(lead)
    }
    fn try_lex(src: TokenStarting<'_>) -> Result<()> {
        if src.text.starts_with(Self::STR) {
            Ok(())
        } else {
            Err(Error {
                kind: error::Kind::ExpectedExact {
                    expected: Self::STR,
                },
                span: Span::end_at(Self::length()),
            })
        }
    }

    unsafe fn lex_value(_src: TokenStarting<'_>) -> Self::Value {
        ()
    }
    unsafe fn lex_length(_src: TokenStarting<'_>) -> usize {
        Self::length()
    }

    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>> {
        Self::try_lex(src).map(|_| token::Content {
            value: (),
            length: Self::length(),
        })
    }
}

pub struct Lexer<'src> {
    cursor: SourceCursor<'src>,
}

impl<'src> Lexer<'src> {
    pub fn new(cursor: SourceCursor<'src>) -> Self {
        Self { cursor }
    }

    pub fn lex_token_core(&mut self) -> Result<TokenCore<'src>> {
        self.skip_whitespace();

        match self.cursor.is_eof() {
            true => Ok(token::Kind::EOF.at(self.cursor.index())),
            false => self.lex_core_content(),
        }
    }
    pub fn lex_token_full(&mut self) -> Result<TokenFull<'src>> {
        self.skip_whitespace();

        match self.cursor.is_eof() {
            true => Ok(token::Kind::EOF.spanning(Span::start_at(self.cursor.index()))),
            false => self.lex_full_content(),
        }
    }
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.cursor.peek_char() {
            if ch.is_whitespace() {
                self.cursor.next_char();
            } else {
                break;
            }
        }
    }
    fn lex_core_content(&mut self) -> Result<TokenCore<'src>> {
        let index = self.cursor.index();

        let token::Content {
            value: kind,
            length,
        } = self.cursor.on_token_start(token::Kind::try_lex_content)?;

        self.cursor = self.cursor.seek_forward(length);

        Ok(TokenCore::new(kind, index))
    }
    fn lex_full_content(&mut self) -> Result<TokenFull<'src>> {
        let index = self.cursor.index();

        let token::Content {
            value: kind,
            length,
        } = self.cursor.on_token_start(token::Kind::try_lex_content)?;

        self.cursor = self.cursor.seek_forward(length);

        Ok(TokenFull::new(
            kind,
            Span::start_at(index).with_length(length),
        ))
    }
}

pub trait ResultExt: Sized {
    fn start(&self) -> usize;
    fn is_eof(&self) -> bool;
    fn eof_as_none(self) -> Option<Self> {
        (!self.is_eof()).then(|| self)
    }
}
impl ResultExt for Result<TokenCore<'_>> {
    fn start(&self) -> usize {
        match self {
            Ok(token) => token.start,
            Err(error) => error.span.start,
        }
    }
    fn is_eof(&self) -> bool {
        matches!(
            self,
            Ok(TokenCore {
                kind: token::Kind::EOF,
                ..
            }) | Err(Error {
                kind: error::Kind::UnexpectedEOF,
                ..
            })
        )
    }
}
impl ResultExt for Result<TokenFull<'_>> {
    fn start(&self) -> usize {
        match self {
            Ok(token) => token.span.start,
            Err(error) => error.span.start,
        }
    }
    fn is_eof(&self) -> bool {
        matches!(
            self,
            Ok(TokenFull {
                kind: token::Kind::EOF,
                ..
            }) | Err(Error {
                kind: error::Kind::UnexpectedEOF,
                ..
            })
        )
    }
}

impl<'a, 'src> From<&'a Result<TokenFull<'src>>> for Span {
    fn from(result: &'a Result<TokenFull<'src>>) -> Self {
        match result {
            Ok(token) => token.span,
            Err(error) => error.span,
        }
    }
}

impl<'src> Lexer<'src> {
    /// Returns an iterator over [TokenCore] that replaces [UnexpectedEOF](error::Kind::UnexpectedEOF) with [None]
    pub fn iter_core<'a>(&'a mut self) -> iter::CoreNoneEOF<'a, 'src> {
        iter::CoreNoneEOF(self)
    }
    /// Returns an iterator over [TokenFull] that replaces [UnexpectedEOF](error::Kind::UnexpectedEOF) with [None]
    pub fn iter_full<'a>(&'a mut self) -> iter::FullNoneEOF<'a, 'src> {
        iter::FullNoneEOF(self)
    }
    /// Returns an iterator over [TokenCore] that always returns [Some]
    pub fn iter_core_eof_as_some<'a>(&'a mut self) -> iter::CoreSomeEOF<'a, 'src> {
        iter::CoreSomeEOF(self)
    }
    /// Returns an iterator over [TokenFull] that always returns [Some]
    pub fn iter_full_eof_as_some<'a>(&'a mut self) -> iter::FullSomeEOF<'a, 'src> {
        iter::FullSomeEOF(self)
    }
}

pub mod export {
    pub use super::Error as LexError;
}
