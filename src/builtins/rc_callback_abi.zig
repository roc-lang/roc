//! The single declaration of the ABIs that compiled refcount helpers are
//! called through.
//!
//! Every backend that generates an RC helper, and every builtin that calls one
//! through a function pointer, derives its signature from this module. A
//! backend that spells the parameter list itself can drift from what the
//! builtins call, which native calling conventions hide and wasm's
//! `call_indirect` rejects at runtime.
//!
//! Compiled Roc code reaches its host through fixed runtime symbols, so a
//! generated helper carries no host pointer: `incref` takes the value pointer
//! and the amount, and `decref`/`free` take the value pointer alone.
//!
//! `Payload.on_drop` is the exception. It is a published host ABI that glue
//! emits for Zig, Rust, and C, so a helper installed there must arrive in the
//! host shape. `host_drop` names that shape: a generated adapter that ignores
//! the ops slot and performs the layout's `decref`.

const std = @import("std");

const utils = @import("utils.zig");

/// Runtime ops table a host-shaped final-drop callback receives.
pub const RocOps = utils.RocOps;

/// One parameter of a compiled RC helper, named by the role it plays.
///
/// Backends map these to their own type representations rather than repeating
/// the parameter lists below.
pub const Param = enum {
    /// Pointer to the value the helper operates on.
    value_ptr,
    /// Pointer-sized signed amount to add to the refcount.
    count,
    /// The published host ABI's ops slot, which generated helpers ignore.
    ops_ptr,
};

/// Parameters of a generated `incref` helper.
pub const incref_params: []const Param = &.{ .value_ptr, .count };

/// Parameters of a generated `decref` or `free` helper.
pub const drop_params: []const Param = &.{.value_ptr};

/// Parameters of a generated `host_drop` adapter.
pub const host_drop_params: []const Param = &.{ .value_ptr, .ops_ptr };

/// ABI of a compiled `incref` helper.
pub const RcIncrefFn = *const fn (?[*]u8, isize) callconv(.c) void;

/// ABI of a compiled `decref` helper.
pub const RcDecrefFn = *const fn (?[*]u8) callconv(.c) void;

/// ABI of a compiled `free` helper.
pub const RcFreeFn = *const fn (?[*]u8) callconv(.c) void;

/// ABI of a compiled `host_drop` adapter, which is the published
/// `Payload.on_drop` ABI. `erased_callable.OnDropFn` is this type.
pub const HostDropFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;

/// The Zig type a parameter role must have in a callback function type.
fn paramType(comptime param: Param) type {
    return switch (param) {
        .value_ptr => ?[*]u8,
        .count => isize,
        .ops_ptr => *RocOps,
    };
}

/// Fail to compile unless `Fn` is a C-callconv function whose parameters are
/// exactly `params`. This is what keeps the lists above and the function types
/// above from drifting apart, and therefore what lets a backend build a helper
/// signature from a list and still match the pointer the builtins call it
/// through.
fn assertMatches(comptime Fn: type, comptime params: []const Param) void {
    const info = @typeInfo(@typeInfo(Fn).pointer.child).@"fn";
    // `CallingConvention.c` is a per-target alias rather than a tag, so the
    // comparison is against the tag that alias resolves to on this target.
    const c_tag = std.meta.activeTag(std.builtin.CallingConvention.c);
    if (std.meta.activeTag(info.calling_convention) != c_tag) {
        @compileError("RC callback ABI must be callconv(.c)");
    }
    if (info.return_type != void) @compileError("RC callback ABI must return void");
    if (info.params.len != params.len) @compileError("RC callback ABI parameter count disagrees with its Param list");
    inline for (info.params, params) |actual, expected| {
        if (actual.type != paramType(expected)) {
            @compileError("RC callback ABI parameter type disagrees with its Param list");
        }
    }
}

comptime {
    assertMatches(RcIncrefFn, incref_params);
    assertMatches(RcDecrefFn, drop_params);
    assertMatches(RcFreeFn, drop_params);
    assertMatches(HostDropFn, host_drop_params);
}

test "only the published host on-drop ABI carries an ops slot" {
    try std.testing.expectEqualSlices(Param, &.{ .value_ptr, .count }, incref_params);
    try std.testing.expectEqualSlices(Param, &.{.value_ptr}, drop_params);
    try std.testing.expectEqualSlices(Param, &.{ .value_ptr, .ops_ptr }, host_drop_params);
}
