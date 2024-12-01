//! The roc code formatter.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod annotation;
pub mod collection;
pub mod def;
pub mod expr;
pub mod header;
pub mod pattern;
pub mod spaces;

use bumpalo::{collections::String, Bump};

#[derive(Debug)]
pub struct Buf<'a> {
    text: String<'a>,
    spaces_to_flush: usize,
    newlines_to_flush: usize,
    beginning_of_line: bool,
}

impl<'a> Buf<'a> {
    pub fn new_in(arena: &'a Bump) -> Buf<'a> {
        Buf {
            text: String::new_in(arena),
            spaces_to_flush: 0,
            newlines_to_flush: 0,
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
            self.spaces_to_flush = indent as usize;
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
        debug_assert!(
            !self.beginning_of_line,
            "push_str: `{s}` with text:\n{}",
            self.text
        );

        self.flush_spaces();

        self.text.push_str(s);
    }

    pub fn push_str(&mut self, s: &str) {
        debug_assert!(
            !self.beginning_of_line,
            "push_str: `{s}` with text:\n{}",
            self.text
        );
        debug_assert!(!s.contains('\n'));
        debug_assert!(!s.ends_with(' '));

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

    /// Only for use in emitting newlines in block strings, which don't follow the rule of
    /// having at most two newlines in a row.
    pub fn push_newline_literal(&mut self) {
        self.spaces_to_flush = 0;
        self.newlines_to_flush += 1;
        self.beginning_of_line = true;
    }

    pub fn newline(&mut self) {
        self.spaces_to_flush = 0;
        self.newlines_to_flush = std::cmp::min(self.newlines_to_flush + 1, 2);
        self.beginning_of_line = true;
    }

    /// Ensures the current buffer ends in a newline, if it didn't already.
    /// Doesn't add a newline if the buffer already ends in one.
    pub fn ensure_ends_with_newline(&mut self) {
        if !self.text.is_empty() && self.newlines_to_flush == 0 {
            self.newline()
        }
    }

    pub fn ensure_ends_with_blank_line(&mut self) {
        if !self.text.is_empty() && self.newlines_to_flush < 2 {
            self.spaces_to_flush = 0;
            self.newlines_to_flush = 2;
            self.beginning_of_line = true;
        }
    }

    fn flush_spaces(&mut self) {
        for _ in 0..self.newlines_to_flush {
            self.text.push('\n');
        }
        self.newlines_to_flush = 0;

        for _ in 0..self.spaces_to_flush {
            self.text.push(' ');
        }
        self.spaces_to_flush = 0;
    }

    /// Ensures the text ends in a newline with no whitespace preceding it.
    pub fn fmt_end_of_file(&mut self) {
        self.ensure_ends_with_newline();
        self.flush_spaces();
    }

    pub fn ends_with_space(&self) -> bool {
        self.spaces_to_flush > 0 || self.text.ends_with(' ')
    }

    pub fn ends_with_newline(&self) -> bool {
        self.newlines_to_flush > 0 || self.text.ends_with('\n')
    }

    fn is_empty(&self) -> bool {
        self.spaces_to_flush == 0 && self.text.is_empty()
    }
}
