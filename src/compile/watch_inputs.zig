//! Watch-mode filesystem inputs recorded by compiler runs for refresh checks.

const std = @import("std");
const Sha256 = @import("base").Sha256;
const SourceDigest = @import("base").SourceDigest;

const Allocator = std.mem.Allocator;

/// File state that was consumed by a compiler run for watch-mode refresh.
pub const State = union(enum) {
    /// The `base.SourceDigest` of the file's bytes exactly as they are on
    /// disk. A compiler run records the digest it took when it read the file,
    /// and a refresh check digests the file again to compare.
    hash: [32]u8,
    missing,
    unreadable,
};

/// Filesystem input path plus the state read by the compiler for that path.
pub const Input = struct {
    path: []const u8,
    state: State,
};

/// The state of a readable file whose bytes have the digest `file_digest`.
pub fn fileState(file_digest: SourceDigest) State {
    return .{ .hash = file_digest.bytes };
}

/// SHA-256 of a short name, such as a module's qualified name or a path. The
/// CLI keys its per-module tables by it. File contents are never hashed here;
/// their states carry a `base.SourceDigest`.
pub fn hashBytes(bytes: []const u8) [32]u8 {
    var hasher = Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

/// Free a slice of watch inputs whose paths are owned by the allocator.
pub fn deinit(allocator: Allocator, inputs: []const Input) void {
    for (inputs) |input| allocator.free(input.path);
    allocator.free(inputs);
}
