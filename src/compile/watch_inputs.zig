//! Watch-mode filesystem inputs recorded by compiler runs for refresh checks.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// File state that was consumed by a compiler run for watch-mode refresh.
pub const State = union(enum) {
    hash: [32]u8,
    missing,
    unreadable,
};

/// Filesystem input path plus the state read by the compiler for that path.
pub const Input = struct {
    path: []const u8,
    state: State,
};

/// Return the SHA-256 digest used by watch-mode snapshots.
pub fn hashBytes(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

/// Free a slice of watch inputs whose paths are owned by the allocator.
pub fn deinit(allocator: Allocator, inputs: []const Input) void {
    for (inputs) |input| allocator.free(input.path);
    allocator.free(inputs);
}
