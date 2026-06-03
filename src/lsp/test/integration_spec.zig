//! Shared spec shape for compiler-backed LSP integration cases.

pub const RunFn = *const fn () anyerror!void;

pub const Spec = struct {
    name: []const u8,
    run: RunFn,
};
