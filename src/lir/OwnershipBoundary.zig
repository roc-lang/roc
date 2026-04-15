//! Ownership-boundary markers and debug-only invariant traps.
//!
//! This file exists to make the ownership split explicit:
//! - builtin/runtime internals may perform RC as part of primitive semantics
//! - explicit LIR RC execution may perform RC because LIR told it to
//! - all ordinary interpreter/backend paths are forbidden from deciding RC
//!
//! During the enforcement phase these helpers primarily document and mark the
//! intended choke points. As the migration proceeds, ordinary-path violations
//! should be rewritten to call `forbidden*` so debug builds panic immediately
//! and release builds hit `unreachable`.

const builtin = @import("builtin");
const std = @import("std");

/// Classification for code paths that touch ownership-sensitive behavior.
pub const UseClass = enum {
    builtin_runtime_internal,
    explicit_lir_rc_execution,
    forbidden_ordinary_path,
};

/// Marker for builtin/runtime-internal RC sites.
pub inline fn builtinRuntimeInternal(comptime site: []const u8) void {
    _ = site;
}

/// Marker for the only legal non-builtin RC execution/lowering sites.
pub inline fn explicitLirRcExecution(comptime site: []const u8) void {
    _ = site;
}

/// Debug-only trap for ordinary interpreter RC outside explicit RC statements.
pub inline fn forbiddenInterpreterOrdinaryRc(comptime site: []const u8) void {
    violation("interpreter ordinary path executed RC outside explicit LIR RC", site);
}

/// Debug-only trap for ordinary backend RC emission outside explicit RC statements.
pub inline fn forbiddenBackendOrdinaryRc(comptime site: []const u8) void {
    violation("backend ordinary path emitted RC outside explicit LIR RC", site);
}

/// Debug-only trap for ordinary layout/refcount branching outside ownership planning.
pub inline fn forbiddenOrdinaryOwnershipDecision(comptime site: []const u8) void {
    violation("ordinary path performed ownership reasoning outside LIR", site);
}

fn violation(comptime message: []const u8, comptime site: []const u8) void {
    if (builtin.mode == .Debug) {
        @panic(std.fmt.comptimePrint("LIR ownership boundary violated: {s} at {s}", .{ message, site }));
    } else {
        unreachable;
    }
}

