use core::fmt;
use std::marker::PhantomData;

use crate::{
    lex::{Lex, StaticLex},
    source::{
        export::{SourceFile, SourceLocation},
        Span, TokenEnded, TokenStarting,
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Core<'src> {
    pub kind: Kind,
    /// Byte index of the first character of the token
    /// in the full source string
    pub start: usize,
    _src: PhantomData<&'src str>,
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Full<'src> {
    pub kind: Kind,
    pub span: Span,
    _src: PhantomData<&'src str>,
}

impl<'src> Core<'src> {
    pub(crate) fn new(kind: Kind, start: usize) -> Self {
        Self {
            kind,
            start,
            _src: PhantomData,
        }
    }
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn get_location(&self, file: &'src SourceFile) -> SourceLocation<'src> {
        file.locate(self.start)
    }

    /// Get the index of the starting byte of the token
    ///
    /// This function exists for consistency with [`Self::get_end`] and [`Self::get_length`].
    /// For the safe version, use [`Self::start`].
    ///
    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn get_start(&self, _file: &'src SourceFile) -> usize {
        self.start
    }
    /// Get the index of the ending byte of the token
    ///
    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn get_end(&self, file: &'src SourceFile) -> usize {
        self.start + self.get_length(file)
    }
    /// Get the byte length of the token
    ///
    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn get_length(&self, file: &'src SourceFile) -> usize {
        let src = self.src_token_starting(file);
        unsafe { self.kind.get_length(src) }
    }

    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn src_token_starting(&self, file: &'src SourceFile) -> TokenStarting<'src> {
        file.token_starting_at(self.get_start(file))
    }

    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn src_token_ended(&self, file: &'src SourceFile) -> TokenEnded<'src> {
        file.token_ended_at(self.get_end(file))
    }
}

impl<'src> fmt::Debug for Core<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("index", &self.start)
            .finish()
    }
}

impl<'src> Full<'src> {
    pub(crate) fn new(kind: Kind, span: Span) -> Self {
        Self {
            kind,
            span,
            _src: PhantomData,
        }
    }
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn get_location(&self, file: &'src SourceFile) -> SourceLocation<'src> {
        file.locate(self.span.start)
    }

    pub fn get_start(&self) -> usize {
        self.span.start
    }
    pub fn get_end(&self) -> usize {
        self.span.end
    }
    pub fn get_length(&self) -> usize {
        self.span.length()
    }

    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn src_token_starting(&self, file: &'src SourceFile) -> TokenStarting<'src> {
        file.token_starting_at(self.span.start)
    }

    /// # Safety
    /// The source file must be the corresponding source file of the token
    pub unsafe fn src_token_ended(&self, file: &'src SourceFile) -> TokenEnded<'src> {
        file.token_ended_at(self.span.end)
    }
}

impl<'src> fmt::Debug for Full<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Ident,
    NumberLiteral,
    StringLiteral,
    /// +
    Plus,
    /// -
    Minus,
    /// .
    Dot,
    /// ..
    DotDot,
    /// (
    LeftParen,
    /// )
    RightParen,
    /// !
    Bang,
    /// =
    Equal,
    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// if
    If,
    /// else
    Else,

    EOF,
}

impl Kind {
    pub unsafe fn get_length(&self, src: TokenStarting<'_>) -> usize {
        unsafe {
            match self {
                Self::Ident => Ident::lex_length(src),
                Self::NumberLiteral => NumberLiteral::lex_length(src),
                Self::StringLiteral => StringLiteral::lex_length(src),
                Self::Plus => Plus::lex_length(src),
                Self::Minus => Minus::lex_length(src),
                Self::Dot => Dot::lex_length(src),
                Self::DotDot => DotDot::lex_length(src),
                Self::LeftParen => LeftParen::lex_length(src),
                Self::RightParen => RightParen::lex_length(src),
                Self::Bang => Bang::lex_length(src),
                Self::Equal => Equal::lex_length(src),
                Self::EqualEqual => EqualEqual::lex_length(src),
                Self::BangEqual => BangEqual::lex_length(src),
                Self::If => If::lex_length(src),
                Self::Else => Else::lex_length(src),
                Self::EOF => 0,
            }
        }
    }

    pub fn at<'src>(self, start: usize) -> Core<'src> {
        Core::new(self, start)
    }
    pub fn spanning<'src>(self, span: Span) -> Full<'src> {
        Full::new(self, span)
    }
}
impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Content<Variant: Lex> {
    pub value: <Variant as Lex>::Value,
    /// Byte length of the token
    pub length: usize,
}
impl<Variant: Lex> Content<Variant> {
    pub fn build(value: Variant::Value) -> impl FnOnce(usize) -> Self {
        move |length| Self { value, length }
    }
}

macro_rules! kind_conversion {
    ($kind:ident) => {
        impl From<$kind> for Kind {
            fn from(_: $kind) -> Self {
                Self::$kind
            }
        }
        impl TryFrom<Kind> for $kind {
            type Error = Kind;
            fn try_from(value: Kind) -> Result<Self, Self::Error> {
                match value {
                    Kind::$kind => Ok(Self),
                    _ => Err(value),
                }
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ident;
kind_conversion!(Ident);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct NumberLiteral;
kind_conversion!(NumberLiteral);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLiteral;
kind_conversion!(StringLiteral);

macro_rules! static_kind {
    ($kind:ident = $bytes:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $kind;
        impl StaticLex for $kind {
            const BYTES: &'static [u8] = $bytes;
        }
        kind_conversion!($kind);
    };
}

static_kind!(Plus = b"+");
static_kind!(Minus = b"-");
// static_kind!(Star = b"*");
// static_kind!(Slash = b"/");
static_kind!(Equal = b"=");
// static_kind!(Comma = b",");
static_kind!(Dot = b".");
static_kind!(DotDot = b"..");
static_kind!(LeftParen = b"(");
static_kind!(RightParen = b")");
// static_kind!(LeftBrace = b"{");
// static_kind!(RightBrace = b"}");
// static_kind!(LeftBracket = b"[");
// static_kind!(RightBracket = b"]");
static_kind!(Bang = b"!");
static_kind!(BangEqual = b"!=");
static_kind!(EqualEqual = b"==");
// static_kind!(Less = b"<");
// static_kind!(LessEqual = b"<=");
// static_kind!(Greater = b">");
// static_kind!(GreaterEqual = b">=");
static_kind!(If = b"if");
static_kind!(Else = b"else");

pub mod export {
    pub use super::Content as TokenContent;
    pub use super::Core as TokenCore;
    pub use super::Full as TokenFull;
    pub use super::Kind as TokenKind;
}
