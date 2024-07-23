//! Provides testing utility functions for use throughout the Rust code base.

#[doc(hidden)]
pub use pretty_assertions::assert_eq as _pretty_assert_eq;

#[derive(PartialEq, Eq)]
pub struct DebugAsDisplay<T>(pub T);

impl<T: std::fmt::Display> std::fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[macro_export]
macro_rules! assert_multiline_str_eq {
    ($a:expr, $b:expr) => {
        $crate::_pretty_assert_eq!($crate::DebugAsDisplay($a), $crate::DebugAsDisplay($b))
    };
}

/// a very simple implementation of En/DecoderFormatting to be embedded in roc source under test
///
/// - numbers and bools are encoded as 'n' <num> ' '
/// - strings are encoded as 's' <count utf-8 bytes> ' ' <str> ' '
/// - records are encoded as 'r' <number of fields> ' ' [<key><value>]*
/// - lists and tuples are encoded as 'l' <len> ' ' [<elem>]*
pub const TAG_LEN_ENCODER_FMT: &str = include_str!("TagLenEncoderFmt.roc");
