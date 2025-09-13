//! Working with Roc source code - whether in files or individual strings.
const std = @import("std");

pub const Span = @import("Span.zig");

/// A slice of bytes representing source code, aligned for 128-bit SIMD and
/// guaranteed to end in a newline, so that syntax-elements which end in newlines
/// (e.g. comments, multiline string literals) don't need to check for EOF,
/// they can just end on newline and that's it. Also means we can always lookahead
/// 1 byte from any non-whitespace byte without exceeding the slice's bounds.
pub const Bytes = [:'\n']align(16) const u8;
