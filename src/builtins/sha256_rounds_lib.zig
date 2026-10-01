//! The SHA-256 compression function the LLVM builtins bitcode links in for its
//! target. The bitcode is compiled for wasm64 and retargeted, so it cannot
//! pick its rounds when it is compiled; build.zig compiles this file once per
//! `sha256.Rounds` (for wasm64 with the portable rounds, and for x86_64 and
//! aarch64 with their SHA-256 instructions), and the LLVM backend links the
//! one the target's CPU features select.

const std = @import("std");
const sha256 = @import("sha256.zig");

/// Builtin payloads must not pull in Zig's panic formatting machinery.
pub const panic = std.debug.no_panic;

fn compress(state: *sha256.State, blocks: [*]const sha256.Block, count: usize) callconv(.c) void {
    sha256.compressForTarget(state, blocks[0..count]);
}

comptime {
    const abi: *const sha256.CompressAbi = &compress;
    @export(abi, .{ .name = sha256.compress_symbol });
}
