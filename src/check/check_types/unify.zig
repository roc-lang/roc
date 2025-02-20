const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("../../types/type.zig").Type;
const ModuleEnv = @import("../../base/ModuleEnv.zig");

const UnificationResult = struct {
    mismatches: std.ArrayList(TypeMismatch),
    has_changed: bool,
};

const TypeMismatch = enum { type_mismatch };

pub fn unify(
    allocator: Allocator,
    env: *ModuleEnv,
    first: Type.Idx,
    second: Type.Idx,
) !UnificationResult {
    const first_type = env.type_store.get(first) orelse return error.TypeNotFound;
    const second_type = env.type_store.get(second) orelse return error.TypeNotFound;

    var result = UnificationResult{
        .mismatches = std.ArrayList(TypeMismatch).init(allocator),
        .has_changed = false,
    };

    if (first == second) {
        return result;
    }

    try unifyType(allocator, env, &result, first, first_type, second, second_type);
}

fn unifyType(
    allocator: Allocator,
    env: *ModuleEnv,
    result: *UnificationResult,
    first: Type.Idx,
    first_type: Type,
    second: Type.Idx,
    second_type: Type,
) !void {
    _ = allocator;

    switch (first_type) {
        .flex_var => |opt_name| try unifyFlex(env, result, first, opt_name, second, second_type),
        _ => @panic("not implemented"),
    }

    return result;
}

fn unifyFlex(
    env: *ModuleEnv,
    result: *UnificationResult,
    first: Type.Idx,
    opt_name: ?[]const u8,
    second: Type.Idx,
    second_type: Type,
) !void {
    switch (second_type) {
        .flex_var => |other_name| {
            // Prefer right's name
            const name = other_name orelse opt_name;
            try merge(env, result, first, second, .{ .flex_var = name });
        },
        _ => @panic("not implemented"),
    }
}

fn merge(
    env: *ModuleEnv,
    result: *UnificationResult,
    left: Type.Idx,
    right: Type.Idx,
    type_value: Type,
) !void {
    try env.type_store.put(left, type_value);
    try env.type_store.put(right, type_value);
    result.has_changed = true;
}
