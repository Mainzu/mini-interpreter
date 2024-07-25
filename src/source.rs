use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::Path;

use thiserror::Error;

use crate::lex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source<'path> {
    root_path: &'path Path,
    root: Directory,
}

impl<'path> Source<'path> {
    pub fn open(root_path: &'path Path) -> Result<Self, Error> {
        Ok(Self {
            root_path,
            root: Directory::open(root_path)?,
        })
    }
    pub fn new(root_path: &'path Path, root: Directory) -> Self {
        Self { root_path, root }
    }

    pub fn root_path(&self) -> &Path {
        self.root_path
    }
    pub fn root(&self) -> &Directory {
        &self.root
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error; // TODO

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct File {
    path: Box<Path>,
    text: Box<str>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directory {
    path: Box<Path>,
    directories: Vec<Directory>,
    files: Vec<File>,
}

impl Directory {
    pub fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
        let mut directories = Vec::new();
        let mut files = Vec::new();

        for entry in fs::read_dir(&path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                directories.push(Directory::open(path)?);
            } else {
                files.push(File::open(path)?);
            }
        }

        Ok(Self {
            path: Box::from(path.as_ref()),
            directories,
            files,
        })
    }
    pub fn new(
        path: impl AsRef<Path>,
        files: impl IntoIterator<Item = File>,
        directories: impl IntoIterator<Item = Directory>,
    ) -> Self {
        Self {
            path: Box::from(path.as_ref()),
            files: files.into_iter().collect(),
            directories: directories.into_iter().collect(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
    pub fn files(&self) -> &[File] {
        &self.files
    }
    pub fn directories(&self) -> &[Directory] {
        &self.directories
    }

    pub fn get_file(&self, path: impl AsRef<Path>) -> Option<&File> {
        self.files.iter().find(|file| file.path() == path.as_ref())
    }
    pub fn get_directory(&self, path: impl AsRef<Path>) -> Option<&Directory> {
        self.directories
            .iter()
            .find(|directory| directory.path() == path.as_ref())
    }
}
impl File {
    pub fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = Box::from(path.as_ref());
        let text = std::fs::read_to_string(&path)?;
        if text.len() > usize::MAX / 2 {
            panic!(
                "File {} must be smaller than {} bytes",
                path.display(),
                usize::MAX / 2
            );
        }
        let text = Box::from(text);
        Ok(Self { path, text })
    }
    pub fn new(path: impl AsRef<Path>, text: impl AsRef<str>) -> Self {
        let path = Box::from(path.as_ref());
        let text = Box::from(text.as_ref());
        Self { path, text }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn locate(&self, index: usize) -> Location {
        let p_text = &self.text[..index];

        let line = p_text.matches('\n').count() + 1;

        let last_line = &p_text[p_text.rfind('\n').unwrap_or(0)..];
        let column = last_line.chars().count() + 1;

        Location {
            file: self.path(),
            line,
            column,
        }
    }

    pub fn cursor(&self) -> Cursor {
        Cursor {
            file: self,
            index: 0,
        }
    }
    pub fn cursor_at(&self, index: usize) -> Option<Cursor> {
        (index <= self.text.len()).then(|| Cursor { file: self, index })
    }

    pub fn token_ended_at(&self, index: usize) -> TokenEnded {
        TokenEnded(&self.text[..index])
    }
    pub fn token_starting_at(&self, index: usize) -> TokenStarting {
        let text = &self.text[index..];
        let lead = text
            .chars()
            .next()
            .expect("Token cannot start at end of file");
        assert!(!lead.is_whitespace(), "Token cannot start with whitespace");
        TokenStarting { lead, text }
    }
}

/// Source text beginning at the end + 1 of the previous token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenEnded<'src>(pub &'src str);

/// Source text beginning at the start of the next token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenStarting<'src> {
    pub lead: char,
    pub text: &'src str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location<'src> {
    pub file: &'src Path,
    /// 1-based line number
    pub line: usize,
    /// 1-based character index in the line
    pub column: usize,
}

impl<'src> PartialOrd for Location<'src> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.file == other.file).then(|| {
            let line = self.line.cmp(&other.line);
            if line.is_eq() {
                self.column.cmp(&other.column)
            } else {
                line
            }
        })
    }
}
impl<'src> fmt::Display for Location<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file.display(), self.line, self.column)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    #[inline]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    #[inline]
    pub const fn start_at(start: usize) -> Self {
        Self::new(start, start)
    }
    #[inline]
    pub const fn end_at(end: usize) -> Self {
        Self::new(0, end)
    }

    #[inline]
    pub const fn start(&self) -> usize {
        self.start
    }
    #[inline]
    pub const fn end(&self) -> usize {
        self.end
    }
    #[inline]
    pub const fn length(&self) -> usize {
        self.end - self.start
    }
    #[inline]
    pub const fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    #[inline]
    pub const fn with_start(self, start: usize) -> Self {
        Self::new(start, self.end)
    }
    #[inline]
    pub const fn with_end(self, end: usize) -> Self {
        Self::new(self.start, end)
    }
    #[inline]
    pub const fn with_length(self, length: usize) -> Self {
        Self::new(self.start, self.start + length)
    }
    #[inline]
    pub const fn lengthen(self, length: usize) -> Self {
        self.with_length(self.length() + length)
    }
    #[inline]
    pub fn translate_foward(self, offset: usize) -> Self {
        Self::new(self.start + offset, self.end + offset)
    }
    #[inline]
    pub fn translate_backward(self, offset: usize) -> Self {
        Self::new(self.start - offset, self.end - offset)
    }
}
impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}
impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cursor<'src> {
    file: &'src File,
    index: usize,
}

impl<'src> Cursor<'src> {
    pub fn text(&self) -> &str {
        &self.file.text[self.index..]
    }
    pub fn index(&self) -> usize {
        self.index
    }
    pub fn is_eof(&self) -> bool {
        self.index == self.file.text.len()
    }
    pub fn get_location(&self) -> Location {
        self.file.locate(self.index)
    }

    fn with_index(self, index: usize) -> Self {
        Self {
            file: self.file,
            index,
        }
    }

    pub fn try_seek_pos(self, index: usize) -> Option<Self> {
        (index <= self.file.text.len()).then(|| self.with_index(index))
    }
    pub fn try_seek_off(self, offset: isize) -> Option<Self> {
        let index = self.index as isize + offset;
        if index < 0 {
            None
        } else {
            self.try_seek_pos(index as usize)
        }
    }
    pub fn try_seek_forward(self, offset: usize) -> Option<Self> {
        self.try_seek_pos(self.index + offset)
    }
    pub fn try_seek_backward(self, offset: usize) -> Option<Self> {
        let index = self.index.checked_sub(offset)?;
        Some(self.with_index(index))
    }

    pub fn seek_pos(self, index: usize) -> Self {
        let index = index.min(self.file.text.len());
        self.with_index(index)
    }
    pub fn seek_off(self, offset: isize) -> Self {
        let index = self.index as isize + offset;
        let index = index.max(0) as usize;
        self.seek_pos(index)
    }
    pub fn seek_forward(self, offset: usize) -> Self {
        self.seek_pos(self.index + offset)
    }
    pub fn seek_backward(self, offset: usize) -> Self {
        let index = self.index.saturating_sub(offset);
        self.with_index(index)
    }

    pub fn peek_char(&self) -> Option<char> {
        self.file.text[self.index..].chars().next()
    }

    pub fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }

    pub fn start_token(&self) -> lex::Result<TokenStarting> {
        let text = self.text();
        let lead = text.chars().next().ok_or_else(|| lex::Error {
            kind: lex::error::Kind::UnexpectedEOF,
            span: Span::start_at(self.index),
        })?;
        Ok(TokenStarting { lead, text })
    }
    pub fn on_token_start<T>(
        &self,
        f: impl FnOnce(TokenStarting) -> lex::Result<T>,
    ) -> lex::Result<T> {
        let src = self.start_token()?;
        f(src).map_err(|err| lex::Error {
            span: err.span.translate_foward(self.index),
            ..err
        })
    }
}

pub mod export {
    pub use super::Cursor as SourceCursor;
    pub use super::Directory as SourceDirectory;
    pub use super::File as SourceFile;
    pub use super::Location as SourceLocation;
}
