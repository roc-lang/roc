//! Per-target C-ABI parameter/return classification for Roc layouts.
//!
//! These classifiers are adapted from the Zig compiler (MIT License, "Copyright (c) Zig
//! contributors"), `src/codegen/<arch>/abi.zig` @ 24fdd5b7a4 (Release 0.16.0), rewritten to
//! read Roc's layout store instead of Zig's `Type`/`Zcu`. They decide, per the published
//! platform C ABI, whether a value is passed/returned in registers or in memory, which the
//! backends and glue then lower into actual calls.

const std = @import("std");

pub const aarch64 = @import("aarch64.zig");
pub const x86_64 = @import("x86_64.zig");

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(aarch64);
    std.testing.refAllDecls(x86_64);
}
