//! Tracks where MonoSymbols live in wasm (local variables vs linear memory).
//!
//! Phase 1: stub — no local variables needed for literal-only expressions.
//! Phase 4 will add local variable allocation for let bindings.

const std = @import("std");

const Self = @This();

pub fn init() Self {
    return .{};
}

pub fn deinit(self: *Self) void {
    _ = self;
}
