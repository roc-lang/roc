//! The roc code formatter.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod annotation;
pub mod collection;
pub mod def;
pub mod expr;
pub mod module;
pub mod pattern;
pub mod spaces;

use bumpalo::{collections::String, Bump};
use roc_parse::ast::Module;

#[cfg(windows)]
const NEWLINE: &str = "\r\n";
#[cfg(not(windows))]
const NEWLINE: &str = "\n";

#[derive(Debug)]
pub struct Ast<'a> {
    pub module: Module<'a>,
    pub defs: roc_parse::ast::Defs<'a>,
}

#[derive(Debug)]
pub struct Buf<'a> {
    text: String<'a>,
    spaces_to_flush: usize,
    beginning_of_line: bool,
}

impl<'a> Buf<'a> {
    pub fn new_in(arena: &'a Bump) -> Buf<'a> {
        Buf {
            text: String::new_in(arena),
            spaces_to_flush: 0,
            beginning_of_line: true,
        }
    }

    pub fn as_str(&'a self) -> &'a str {
        self.text.as_str()
    }

    pub fn into_bump_str(self) -> &'a str {
        self.text.into_bump_str()
    }

    pub fn indent(&mut self, indent: u16) {
        if self.beginning_of_line {
            for _ in 0..indent {
                self.text.push(' ');
            }
        }
        self.beginning_of_line = false;
    }

    pub fn push(&mut self, ch: char) {
        debug_assert!(!self.beginning_of_line);
        debug_assert!(
            ch != '\n',
            "Don't call buf.push('\\n') - rather, call buf.newline()"
        );
        debug_assert!(
            ch != ' ',
            "Don't call buf.push(' ') - rather, call buf.spaces(1)"
        );

        self.flush_spaces();

        self.text.push(ch);
    }

    pub fn push_str_allow_spaces(&mut self, s: &str) {
        debug_assert!(!self.beginning_of_line);

        self.flush_spaces();

        self.text.push_str(s);
    }

    pub fn push_str(&mut self, s: &str) {
        debug_assert!(!self.beginning_of_line);
        debug_assert!(!s.contains('\n') && !s.ends_with(' '));

        if !s.is_empty() {
            self.flush_spaces();
        }

        self.text.push_str(s);
    }

    pub fn push_char_literal(&mut self, c: char) {
        self.flush_spaces();

        self.text.push(c);
    }

    pub fn spaces(&mut self, count: usize) {
        self.spaces_to_flush += count;
    }

    pub fn newline(&mut self) {
        self.spaces_to_flush = 0;

        self.text.push_str(NEWLINE);

        self.beginning_of_line = true;
    }

    /// Ensures the current buffer ends in a newline, if it didn't already.
    /// Doesn't add a newline if the buffer already ends in one.
    pub fn ensure_ends_with_newline(&mut self) {
        if self.spaces_to_flush > 0 {
            self.flush_spaces();
            self.newline();
        } else if !self.text.ends_with('\n') && !self.text.is_empty() {
            self.newline()
        }
    }

    pub fn ensure_ends_with_blank_line(&mut self) {
        if self.spaces_to_flush > 0 {
            self.flush_spaces();
            self.newline();
            self.newline();
        } else if !self.text.ends_with('\n') {
            self.newline();
            self.newline();
        } else if !self.text.ends_with("\n\n") {
            self.newline();
        }
    }

    fn flush_spaces(&mut self) {
        if self.spaces_to_flush > 0 {
            for _ in 0..self.spaces_to_flush {
                self.text.push(' ');
            }
            self.spaces_to_flush = 0;
        }
    }

    /// Ensures the text ends in a newline with no whitespace preceding it.
    pub fn fmt_end_of_file(&mut self) {
        fmt_text_eof(&mut self.text)
    }

    pub fn ends_with_space(&self) -> bool {
        self.spaces_to_flush > 0 || self.text.ends_with(' ')
    }

    pub fn ends_with_newline(&self) -> bool {
        self.spaces_to_flush == 0 && self.text.ends_with('\n')
    }

    fn is_empty(&self) -> bool {
        self.spaces_to_flush == 0 && self.text.is_empty()
    }
}

/// Ensures the text ends in a newline with no whitespace preceding it.
fn fmt_text_eof(text: &mut bumpalo::collections::String<'_>) {
    let mut chars_rev = text.chars().rev();
    let mut last_whitespace = None;
    let mut last_whitespace_index = text.len();

    // Keep going until we either run out of characters or encounter one
    // that isn't whitespace.
    loop {
        match chars_rev.next() {
            Some(ch) if ch.is_whitespace() => {
                last_whitespace = Some(ch);
                last_whitespace_index -= 1;
            }
            _ => {
                break;
            }
        }
    }

    match last_whitespace {
        Some('\n') => {
            // There may have been more whitespace after this newline; remove it!
            text.truncate(last_whitespace_index + '\n'.len_utf8());
        }
        Some(_) => {
            // There's some whitespace at the end of this file, but the first
            // whitespace char after the last non-whitespace char isn't a newline.
            // So replace that whitespace char (and everything after it) with a newline.
            text.replace_range(last_whitespace_index.., NEWLINE);
        }
        None => {
            debug_assert!(last_whitespace_index == text.len());
            debug_assert!(!text.ends_with(char::is_whitespace));

            // This doesn't end in whitespace at all, so add a newline.
            text.push_str(NEWLINE);
        }
    }
}

#[test]
fn eof_text_ends_with_newline() {
    use bumpalo::{collections::String, Bump};

    let arena = Bump::new();
    let input = "This should be a newline:\n";
    let mut text = String::from_str_in(input, &arena);

    fmt_text_eof(&mut text);

    // This should be unchanged!
    assert_eq!(text.as_str(), input);
}

#[test]
fn eof_text_ends_with_whitespace() {
    use bumpalo::{collections::String, Bump};

    let arena = Bump::new();
    let input = "This should be a newline: \t";
    let mut text = String::from_str_in(input, &arena);

    fmt_text_eof(&mut text);

    assert_eq!(text.as_str(), "This should be a newline:\n");
}

#[test]
fn eof_text_ends_with_whitespace_then_newline() {
    use bumpalo::{collections::String, Bump};

    let arena = Bump::new();
    let input = "This should be a newline:  \n";
    let mut text = String::from_str_in(input, &arena);

    fmt_text_eof(&mut text);

    assert_eq!(text.as_str(), "This should be a newline:\n");
}

#[test]
fn eof_text_ends_with_no_whitespace() {
    use bumpalo::{collections::String, Bump};

    let arena = Bump::new();
    let input = "This should be a newline:";
    let mut text = String::from_str_in(input, &arena);

    fmt_text_eof(&mut text);

    assert_eq!(text.as_str(), "This should be a newline:\n");
}

#[test]
fn eof_text_is_empty() {
    use bumpalo::{collections::String, Bump};

    let arena = Bump::new();
    let input = "";
    let mut text = String::from_str_in(input, &arena);

    fmt_text_eof(&mut text);

    assert_eq!(text.as_str(), "\n");
}
