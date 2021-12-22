#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod annotation;
pub mod collection;
pub mod def;
pub mod expr;
pub mod module;
pub mod pattern;
pub mod spaces;

use bumpalo::{collections::String, Bump};

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
        debug_assert!(ch != '\n' && ch != ' ');

        self.flush_spaces();

        self.text.push(ch);
    }

    pub fn push_str_allow_spaces(&mut self, s: &str) {
        debug_assert!(!self.beginning_of_line);
        debug_assert!(!s.contains('\n'));

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

    pub fn spaces(&mut self, count: usize) {
        self.spaces_to_flush += count;
    }

    pub fn newline(&mut self) {
        self.spaces_to_flush = 0;
        self.text.push('\n');
        self.beginning_of_line = true;
    }

    fn flush_spaces(&mut self) {
        if self.spaces_to_flush > 0 {
            for _ in 0..self.spaces_to_flush {
                self.text.push(' ');
            }
            self.spaces_to_flush = 0;
        }
    }
}
