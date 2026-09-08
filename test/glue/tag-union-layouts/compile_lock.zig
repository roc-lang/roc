//! Compile generated tag-union layouts and ownership methods across host targets.

/// This compile-only object has no host runtime; safety failures trap on every target.
pub const panic = @import("std").debug.no_panic;

const abi = @import("glue_abi");

fn Return(comptime function: anytype) type {
    return @typeInfo(@TypeOf(function)).@"fn".return_type.?;
}

comptime {
    for (.{ abi.roc_first_scope, abi.roc_second_scope, abi.roc_nested, abi.roc_tuple }) |function| {
        const T = Return(function);
        if (@typeInfo(T) != .@"enum" or @sizeOf(T) != 1 or @alignOf(T) != 1)
            @compileError("zero-sized payloads must use a one-byte enum");
    }
    for (.{ abi.roc_mixed, abi.roc_scalar }) |function| {
        const T = Return(function);
        if (!@hasField(T, "tag") or !@hasField(T, "payload"))
            @compileError("nonzero payloads must retain tag and payload storage");
    }
    if (abi.ShapesNestedPayload != Return(abi.roc_nested_payload) or
        abi.ShapesMixedEmptyPayload != Return(abi.roc_mixed_empty_payload))
        @compileError("omitted payload types must not reserve legitimate alias names");
    if (abi.ShapesSecond_scopeResult != abi.ShapesFirst_scopeResult)
        @compileError("shared payload-free unions must alias the same type");
}

// Exported callers force analysis of generated method bodies, including RC
// branches for zero-sized payloads nested inside an owning union.
export fn retain_and_release(roc_host: *abi.RocHost, input: u8) void {
    inline for (.{ abi.roc_first_scope, abi.roc_second_scope, abi.roc_nested, abi.roc_tuple, abi.roc_mixed, abi.roc_scalar }) |function| {
        const value = function(input);
        value.incref(1);
        value.decref(roc_host);
        value.decref(roc_host);
    }
}
