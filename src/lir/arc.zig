//! Mechanical ARC insertion for LIR.
//!
//! This pass is the only non-builtin stage that may synthesize explicit
//! `incref`, `decref`, and `free` statements. Backends consume those statements
//! without doing reference-counting analysis.

const std = @import("std");

const LirStore = @import("LirStore.zig");

pub const ResourceError = std.mem.Allocator.Error;

pub fn insert(store: *LirStore) ResourceError!void {
    _ = store;
}

test "arc insertion boundary exists" {
    std.testing.refAllDecls(@This());
}
