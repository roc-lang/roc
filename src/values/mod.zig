//! Shared value formatting module for Roc runtime values.
//!
//! Provides a common `RocValue` type that wraps raw bytes + layout and a
//! canonical `format()` function used by the interpreter, dev backend, test
//! helpers, and the snapshot tool.

const std = @import("std");

pub const RocValue = @import("RocValue.zig");

test "values tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("RocValue.zig"));
}
