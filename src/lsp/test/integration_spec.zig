//! Shared spec shape for compiler-backed LSP integration cases.

/// Function pointer type for one integration spec body.
pub const RunFn = *const fn () anyerror!void;

/// Named LSP integration spec that can be scheduled by the harness.
pub const Spec = struct {
    name: []const u8,
    run: RunFn,
};
