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
    beginning_of_line: bool,

    // #[cfg(debug)]
    debugger: Option<Box<dyn FmtDebugger>>,
}

impl<'a> Buf<'a> {
    pub fn new_in(arena: &'a Bump) -> Buf<'a> {
        Buf {
            text: String::new_in(arena),
            beginning_of_line: true,

            // #[cfg(debug)]
            debugger: Some(Box::new(BacktraceFmtDebugger::new())),
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

                // #[cfg(debug)]
                if let Some(debugger) = &mut self.debugger {
                    debugger.pushed(' ');
                }
            }
        }
        self.beginning_of_line = false;
    }

    pub fn push(&mut self, ch: char) {
        debug_assert!(!self.beginning_of_line);
        debug_assert!(ch != '\n');
        self.text.push(ch);

        // #[cfg(debug)]
        if let Some(debugger) = &mut self.debugger {
            debugger.pushed(ch);
        }
    }

    pub fn push_str(&mut self, s: &str) {
        debug_assert!(!self.beginning_of_line);
        debug_assert!(!s.contains('\n'));
        self.text.push_str(s);

        // #[cfg(debug)]
        if let Some(debugger) = &mut self.debugger {
            debugger.pushed_str(s);
        }
    }

    pub fn newline(&mut self) {
        self.text.push('\n');
        self.beginning_of_line = true;

        // #[cfg(debug)]
        if let Some(debugger) = &mut self.debugger {
            debugger.pushed('\n');
        }
    }
}

pub trait FmtDebugger {
    fn pushed(&mut self, _ch: char) {}
    fn pushed_str(&mut self, _s: &str) {}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Address(u64);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct FrameIndex(u32);

struct BacktraceFmtDebugger {
    recorder: roc_debug_buf::FlightRecorder,
}

impl BacktraceFmtDebugger {
    fn new() -> Self {
        BacktraceFmtDebugger {
            recorder: roc_debug_buf::FlightRecorder::new(),
        }
    }
}

impl FmtDebugger for BacktraceFmtDebugger {
    fn pushed(&mut self, ch: char) {
        let len = self.recorder.len();
        self.recorder.push(ch);
        self.recorder.take_sample(len, ch.len_utf8());
    }

    fn pushed_str(&mut self, s: &str) {
        let len = self.recorder.len();
        self.recorder.push_str(s);
        self.recorder.take_sample(len, s.len());
    }
}
