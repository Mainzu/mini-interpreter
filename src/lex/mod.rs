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
/// Defines the interface for lexing tokens.
///
/// This trait provides methods for lexing (tokenizing) input text into specific tokens.
/// Implementors of this trait can define how to recognize and extract tokens of a particular type.
// TODO: Examples
///
/// # Safety
///
/// Some methods in this trait are marked as `unsafe`. These methods assume that
/// `try_lex` has been called successfully before they are invoked. Calling these
/// methods without proper checks may lead to undefined behavior.
pub trait Lex {
    /// The type of value corresponding to this token type.
    type Value: Sized + fmt::Debug + Clone + PartialEq;

    /// Checks if the token can start with the given character.
    ///
    /// This method is used for quick checks to determine if a token of this type
    /// can possibly start with the given character.
    fn can_start_with(lead: char) -> bool;

    /// Attempts to lex a token, returning Ok(()) if successful.
    ///
    /// This method should perform all necessary checks to determine if the input
    /// can be lexed as a valid token of this type.
    fn try_lex(src: TokenStarting<'_>) -> Result<()>;

    /// Lexes the value of the token, assuming it's valid.
    ///
    /// # Safety
    ///
    /// This method assumes that a [`try_lex*`](Lex::try_lex) method has been called successfully.
    /// Calling it without proper checks may lead to undefined behavior.
    unsafe fn lex_value(src: TokenStarting<'_>) -> Self::Value {
        Self::try_lex_value(src).unwrap_unchecked()
    }
    /// Lexes the length of the token, assuming it's valid.
    ///
    /// # Safety
    ///
    /// This method assumes that a [`try_lex*`](Lex::try_lex) method has been called successfully.
    /// Calling it without proper checks may lead to undefined behavior.
    unsafe fn lex_length(src: TokenStarting<'_>) -> usize {
        Self::try_lex_length(src).unwrap_unchecked()
    }

    /// Attempts to lex the content of the token.
    ///
    /// Return both the value and the length of the lexed token.
    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>>;
    /// Attempts to lex only the value of the token.
    fn try_lex_value(src: TokenStarting<'_>) -> Result<Self::Value> {
        Self::try_lex_content(src).map(|fragment| fragment.value)
    }
    /// Attempts to lex only the length of the token.
    fn try_lex_length(src: TokenStarting<'_>) -> Result<usize> {
        Self::try_lex_content(src).map(|fragment| fragment.length)
    }
}

/// Defines static lexing for fixed strings.
///
/// This trait is useful for tokens that have a fixed representation, such as
/// keywords or specific symbols.
pub trait StaticLex {
    /// The fixed string representation of this token.
    const STR: &str;
    /// The byte representation of the fixed string.
    const BYTES: &[u8] = Self::STR.as_bytes();
    /// Returns the length of the fixed string.
    fn length() -> usize {
        Self::STR.len()
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
    fn try_lex_length(_src: TokenStarting<'_>) -> Result<usize> {
        Ok(Self::length())
    }
    fn try_lex_value(_src: TokenStarting<'_>) -> Result<Self::Value> {
        Ok(())
    }
}

/// Represents a lexer for tokenizing input text.
///
/// This struct provides methods for lexing tokens from the input source.
///
/// # Examples
///
/// ```rust
/// use mini_interpreter::{lex::Lexer, source::{Span, SourceFile}, token};
///
/// let source = SourceFile::new("example.txt", "let x = 42;");
/// let mut lexer = Lexer::new(source.cursor());
///
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::Let.spanning(Span::from(0..3)))
/// );
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::Ident.spanning(Span::from(4..5)))
/// );
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::Equal.spanning(Span::from(6..7)))
/// );
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::NumberLiteral.spanning(Span::from(8..10)))
/// );
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::Semicolon.spanning(Span::from(10..11)))
/// );
/// assert_eq!(
///     lexer.lex_token_full(),
///     Ok(token::Kind::EOF.spanning(Span::from(11..11)))
/// );
/// ```
pub struct Lexer<'src> {
    cursor: SourceCursor<'src>,
}

impl<'src> Lexer<'src> {
    /// Creates a new Lexer from a [`SourceCursor`].
    pub fn new(cursor: SourceCursor<'src>) -> Self {
        Self { cursor }
    }
    /// Lexes the next token, returning its [core](token::Core) information.
    pub fn lex_token_core(&mut self) -> Result<TokenCore<'src>> {
        self.skip_whitespace();

        match self.cursor.is_eof() {
            true => Ok(token::Kind::EOF.at(self.cursor.index())),
            false => self.lex_core_content(),
        }
    }
    /// Lexes the next token, returning its [full](token::Full) information.
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
impl<'src> Lexer<'src> {
    /// Returns an iterator over [TokenCore] that replaces
    /// [EOF](token::Kind::EOF) and [UnexpectedEOF](error::Kind::UnexpectedEOF)
    /// with [None]
    pub fn iter_core<'a>(&'a mut self) -> iter::CoreNoneEOF<'a, 'src> {
        iter::CoreNoneEOF(self)
    }
    /// Returns an iterator over [TokenFull] that replaces
    /// [EOF](token::Kind::EOF) and [UnexpectedEOF](error::Kind::UnexpectedEOF)
    /// with [None]
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

pub use export::*;
pub mod export {
    pub use super::Error as LexError;
}
