//! Shared compile-time exhaustiveness context policy.

const std = @import("std");

const ProblemStore = @import("problem/store.zig").Store;

/// Syntax sites whose pending exhaustiveness diagnostics can be tracked.
pub const SiteKind = enum {
    match,
    destructure,
    if_,
};

/// Returns whether checking records a static pending diagnostic for this site.
pub fn siteHasPendingStaticDiagnostic(kind: SiteKind) bool {
    return switch (kind) {
        .match,
        .destructure,
        => true,
        .if_ => false,
    };
}

/// Returns whether compile-time evaluation can observe this site.
pub fn siteCanBeObservedAtCompileTime(kind: SiteKind) bool {
    return switch (kind) {
        .match,
        .destructure,
        .if_,
        => true,
    };
}

/// Tracks compile-time roots that affect exhaustiveness reporting mode.
pub const Context = struct {
    depth: u32 = 0,

    pub fn enterCompileTimeRoot(self: *Context) Scope {
        self.depth += 1;
        return .{ .context = self, .saved_depth = null };
    }

    pub fn resetForRuntimeFunction(self: *Context) Scope {
        const saved_depth = self.depth;
        self.depth = 0;
        return .{ .context = self, .saved_depth = saved_depth };
    }

    pub fn active(self: *const Context) bool {
        return self.depth != 0;
    }

    pub fn shouldRecordSite(self: *const Context, kind: SiteKind) bool {
        return self.active() and siteCanBeObservedAtCompileTime(kind);
    }

    pub fn pendingStaticMode(self: *const Context) ProblemStore.PendingStaticExhaustivenessMode {
        return if (self.active()) .empirical else .static;
    }
};

/// Restores a `Context` after entering or temporarily resetting a root scope.
pub const Scope = struct {
    context: *Context,
    saved_depth: ?u32,

    pub fn leave(self: Scope) void {
        if (self.saved_depth) |saved_depth| {
            self.context.depth = saved_depth;
        } else {
            std.debug.assert(self.context.depth > 0);
            self.context.depth -= 1;
        }
    }
};

test "compile-time exhaustiveness context records active roots" {
    var context = Context{};
    try std.testing.expect(!context.active());
    try std.testing.expectEqual(ProblemStore.PendingStaticExhaustivenessMode.static, context.pendingStaticMode());

    const outer = context.enterCompileTimeRoot();
    defer outer.leave();
    try std.testing.expect(context.active());
    try std.testing.expectEqual(ProblemStore.PendingStaticExhaustivenessMode.empirical, context.pendingStaticMode());

    const reset = context.resetForRuntimeFunction();
    defer reset.leave();
    try std.testing.expect(!context.active());
    try std.testing.expectEqual(ProblemStore.PendingStaticExhaustivenessMode.static, context.pendingStaticMode());
}

test "site kinds declare whether they have checked pending diagnostics" {
    try std.testing.expect(siteHasPendingStaticDiagnostic(.match));
    try std.testing.expect(siteHasPendingStaticDiagnostic(.destructure));
    try std.testing.expect(!siteHasPendingStaticDiagnostic(.if_));

    try std.testing.expect(siteCanBeObservedAtCompileTime(.match));
    try std.testing.expect(siteCanBeObservedAtCompileTime(.destructure));
    try std.testing.expect(siteCanBeObservedAtCompileTime(.if_));
}
