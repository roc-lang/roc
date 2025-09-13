//! Working with Roc source code - whether in files or individual strings.
const std = @import("std");

pub const Span = @import("Span.zig");
pub const Bytes = [:'\n']align(16) const u8;
