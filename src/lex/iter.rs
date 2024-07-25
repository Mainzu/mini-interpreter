use super::*;

use std::iter;

pub struct CoreNoneEOF<'a, 'src>(pub(super) &'a mut Lexer<'src>);
impl<'a, 'src> Iterator for CoreNoneEOF<'a, 'src> {
    type Item = Result<TokenCore<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.lex_token_core().eof_as_none()
    }
}
impl<'a, 'src> iter::FusedIterator for CoreNoneEOF<'a, 'src> {}
pub struct FullNoneEOF<'a, 'src>(pub(super) &'a mut Lexer<'src>);
impl<'a, 'src> Iterator for FullNoneEOF<'a, 'src> {
    type Item = Result<TokenFull<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.lex_token_full().eof_as_none()
    }
}
impl<'a, 'src> iter::FusedIterator for FullNoneEOF<'a, 'src> {}

pub struct CoreSomeEOF<'a, 'src>(pub(super) &'a mut Lexer<'src>);
impl<'a, 'src> Iterator for CoreSomeEOF<'a, 'src> {
    type Item = Result<TokenCore<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.lex_token_core())
    }
}

pub struct FullSomeEOF<'a, 'src>(pub(super) &'a mut Lexer<'src>);
impl<'a, 'src> Iterator for FullSomeEOF<'a, 'src> {
    type Item = Result<TokenFull<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.lex_token_full())
    }
}

// pub struct IterPairWindow<'a, 'src> {
//     lexer: &'a mut Lexer<'src>,
//     curr: Result<TokenCore<'src>>,
// }
// impl<'a, 'src> Iterator for IterPairWindow<'a, 'src> {
//     type Item = (Result<TokenCore<'src>>, Result<TokenCore<'src>>);

//     fn next(&mut self) -> Option<Self::Item> {
//         let next = self.lexer.lex_token_core();
//         if self.curr.is_eof() && next.is_eof() {
//             None
//         } else {
//             let curr = std::mem::replace(&mut self.curr, next.clone());
//             Some((curr, next))
//         }
//     }
// }
// impl<'a, 'src> iter::FusedIterator for IterPairWindow<'a, 'src> {}
// impl<'a, 'src> IterPairWindow<'a, 'src> {
//     pub fn peek(&self) -> Option<&Result<TokenCore<'src>>> {
//         if self.curr.is_eof() {
//             None
//         } else {
//             Some(&self.curr)
//         }
//     }
// }
