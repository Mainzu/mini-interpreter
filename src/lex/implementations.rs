use std::result;

use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{
    source::{Span, TokenStarting},
    token,
};

use super::{error, CharGroup, Error, Lex, Result, StaticLex};

trait SomeToErr<E> {
    fn err_or<T>(self, value: T) -> result::Result<T, E>;
    fn err_or_else<T>(self, f: impl FnOnce() -> T) -> result::Result<T, E>;
}
impl<E> SomeToErr<E> for Option<E> {
    fn err_or<T>(self, value: T) -> result::Result<T, E> {
        match self {
            Some(error) => Err(error),
            None => Ok(value),
        }
    }
    fn err_or_else<T>(self, f: impl FnOnce() -> T) -> result::Result<T, E> {
        match self {
            Some(error) => Err(error),
            None => Ok(f()),
        }
    }
}

impl Lex for token::Kind {
    type Value = token::Kind;

    fn can_start_with(lead: char) -> bool {
        is_xid_start(lead)
            || matches!(
                lead,
                '+' | '-' | '(' | ')' | '!' | '=' | ',' | ';' | '0'..='9' | '"'
            )
    }

    fn try_lex(src: TokenStarting<'_>) -> Result<()> {
        Self::try_lex_content(src).map(|_| ())
    }

    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>> {
        use token::*;
        let TokenStarting { lead, text } = src;

        match lead {
            _ if is_xid_start(lead) => {
                let length = Ident::try_lex_length(src)?;
                let build = match text[..length].as_bytes() {
                    If::BYTES => Content::build(Kind::If),
                    Else::BYTES => Content::build(Kind::Else),
                    Let::BYTES => Content::build(Kind::Let),
                    _ => Content::build(Kind::Ident),
                };
                Ok(build(length))
            }
            '+' => Plus::try_lex_length(src).map(Content::build(Kind::Plus)),
            '-' => Minus::try_lex_length(src).map(Content::build(Kind::Minus)),
            '(' => LeftParen::try_lex_length(src).map(Content::build(Kind::LeftParen)),
            ')' => RightParen::try_lex_length(src).map(Content::build(Kind::RightParen)),
            '!' => {
                if text.as_bytes().get(1) == Some(&b'=') {
                    BangEqual::try_lex_length(src).map(Content::build(Kind::BangEqual))
                } else {
                    Bang::try_lex_length(src).map(Content::build(Kind::Bang))
                }
            }
            '=' => {
                if text.as_bytes().get(1) == Some(&b'=') {
                    EqualEqual::try_lex_length(src).map(Content::build(Kind::EqualEqual))
                } else {
                    Equal::try_lex_length(src).map(Content::build(Kind::Equal))
                }
            }
            '0'..='9' => {
                NumberLiteral::try_lex_length(src).map(Content::build(Kind::NumberLiteral))
            }
            '.' => {
                if text.as_bytes().get(1) == Some(&b'.') {
                    DotDot::try_lex_length(src).map(Content::build(Kind::DotDot))
                } else {
                    Dot::try_lex_length(src).map(Content::build(Kind::Dot))
                }
            }
            ';' => Semicolon::try_lex_length(src).map(Content::build(Kind::Semicolon)),
            '"' => StringLiteral::try_lex_length(src).map(Content::build(Kind::StringLiteral)),
            _ => Err(Error {
                kind: error::Kind::UnrecognizedCharacter,
                span: Span::end_at(1),
            }),
        }
    }
}

impl Lex for token::Ident {
    type Value = Box<str>;

    fn can_start_with(lead: char) -> bool {
        is_xid_start(lead)
    }
    fn try_lex(src: TokenStarting<'_>) -> Result<()> {
        (!Self::can_start_with(src.lead))
            .then(|| Error {
                kind: error::Kind::ExpectedCharGroup {
                    expected: CharGroup::IdentStart,
                    found: src.lead,
                },
                span: Span::end_at(1),
            })
            .err_or(())
    }

    unsafe fn lex_length(src: TokenStarting<'_>) -> usize {
        let cont_len = src
            .text
            .chars()
            .skip(1)
            .take_while(|&ch| is_xid_continue(ch))
            .map(|ch| ch.len_utf8())
            .sum::<usize>();
        src.lead.len_utf8() + cont_len
    }
    unsafe fn lex_value(src: TokenStarting<'_>) -> Self::Value {
        Box::from(&src.text[..Self::lex_length(src)])
    }

    fn try_lex_length(src: TokenStarting<'_>) -> Result<usize> {
        Self::try_lex(src)?;
        Ok(unsafe { Self::lex_length(src) })
    }
    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>> {
        let byte_len = Self::try_lex_length(src)?;
        Ok(token::Content {
            value: Box::from(&src.text[..byte_len]),
            length: byte_len,
        })
    }
}
impl Lex for token::NumberLiteral {
    type Value = f64;

    fn can_start_with(lead: char) -> bool {
        lead.is_ascii_digit()
    }
    fn try_lex(src: TokenStarting<'_>) -> Result<()> {
        (!Self::can_start_with(src.lead))
            .then(|| Error {
                kind: error::Kind::ExpectedCharGroup {
                    expected: CharGroup::Digit,
                    found: src.lead,
                },
                span: Span::end_at(1),
            })
            .err_or(())
    }

    unsafe fn lex_length(src: TokenStarting<'_>) -> usize {
        let mut has_period = false;
        src.text
            .chars()
            .take_while(|ch| match ch {
                '0'..='9' => true,
                '.' if !has_period => {
                    has_period = true;
                    true
                }
                _ => false,
            })
            .map(|ch| ch.len_utf8())
            .sum::<usize>()
    }
    unsafe fn lex_value(src: TokenStarting<'_>) -> Self::Value {
        src.text[..Self::lex_length(src)].parse().unwrap()
    }

    fn try_lex_length(src: TokenStarting<'_>) -> Result<usize> {
        Self::try_lex(src)?;
        Ok(unsafe { Self::lex_length(src) })
    }

    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>> {
        let byte_len = Self::try_lex_length(src)?;
        Ok(token::Content {
            value: src.text[..byte_len].parse().unwrap(),
            length: byte_len,
        })
    }
}

fn escape(ch: char) -> Option<char> {
    match ch {
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        '0' => Some('\0'),
        '"' => Some('"'),
        '\'' => Some('\''),
        '\\' => Some('\\'),
        _ => None,
    }
}
impl Lex for token::StringLiteral {
    type Value = Box<str>;

    fn can_start_with(lead: char) -> bool {
        lead == '"'
    }
    fn try_lex(src: TokenStarting<'_>) -> Result<()> {
        (!Self::can_start_with(src.lead))
            .then(|| Error {
                kind: error::Kind::ExpectedChar {
                    expected: '"',
                    found: src.lead,
                },
                span: Span::end_at(1),
            })
            .or_else(|| {
                let mut escaped = false;
                let mut terminated = false;
                let mut chars = src.text.chars().skip(1);

                while let Some(ch) = chars.next() {
                    if terminated {
                        break;
                    }

                    match ch {
                        '\\' => escaped = true,
                        '"' if !escaped => terminated = true,
                        _ if escaped => {
                            if escape(ch).is_none() {
                                return Some(Error {
                                    kind: error::Kind::InvalidEscapeSequence,
                                    span: Span::start_at(1).with_length(2),
                                });
                            }
                            escaped = false
                        }
                        _ => {}
                    }
                }

                if terminated {
                    None
                } else {
                    Some(Error {
                        kind: error::Kind::UnterminatedStringLiteral,
                        span: Span::end_at(src.text.len() - 1),
                    })
                }
            })
            .err_or(())
    }

    fn try_lex_length(src: TokenStarting<'_>) -> Result<usize> {
        if src.lead != '"' {
            return Err(Error {
                kind: error::Kind::ExpectedChar {
                    expected: '"',
                    found: src.lead,
                },
                span: Span::end_at(1),
            });
        }

        let mut escaped = false;
        let mut terminated = false;
        let mut byte_len = src.lead.len_utf8();
        let mut chars = src.text.chars().skip(1);

        while let Some(ch) = chars.next() {
            if terminated {
                break;
            }

            match ch {
                '\\' => escaped = true,
                '"' if !escaped => terminated = true,
                _ if escaped => {
                    escape(ch).ok_or_else(|| Error {
                        kind: error::Kind::InvalidEscapeSequence,
                        span: Span::start_at(byte_len).with_length(2),
                    })?;
                    escaped = false
                }
                _ => {}
            }

            byte_len += ch.len_utf8();
        }

        if terminated {
            Ok(byte_len)
        } else {
            Err(Error {
                kind: error::Kind::UnterminatedStringLiteral,
                span: Span::end_at(byte_len),
            })
        }
    }

    fn try_lex_content(src: TokenStarting<'_>) -> Result<token::Content<Self>> {
        if src.lead != '"' {
            return Err(Error {
                kind: error::Kind::ExpectedChar {
                    expected: '"',
                    found: src.lead,
                },
                span: Span::end_at(1),
            });
        }

        let mut escaped = false;
        let mut terminated = false;
        let mut chars = src.text.chars().skip(1);
        let mut value = String::new();
        let mut byte_len = src.lead.len_utf8();

        while let Some(ch) = chars.next() {
            if terminated {
                break;
            }
            match ch {
                '\\' => escaped = true,
                '"' if !escaped => terminated = true,
                ch if escaped => {
                    escaped = false;
                    value.push(escape(ch).ok_or_else(|| Error {
                        kind: error::Kind::InvalidEscapeSequence,
                        span: Span::start_at(byte_len).with_length(2),
                    })?);
                }
                ch => value.push(ch),
            }
            byte_len += ch.len_utf8();
        }

        if terminated {
            Ok(token::Content {
                value: Box::from(value),
                length: byte_len,
            })
        } else {
            Err(Error {
                kind: error::Kind::UnterminatedStringLiteral,
                span: Span::end_at(byte_len),
            })
        }
    }
}
