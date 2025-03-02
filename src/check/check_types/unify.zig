const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../../base.zig");

const Type = @import("../../types/type.zig").Type;
const Ident = base.Ident;
const ModuleEnv = base.ModuleEnv;

const UnificationResult = struct {
    mismatches: std.ArrayList(TypeMismatch),
    has_changed: bool,
};

const TypeMismatch = enum { type_mismatch };

pub fn unify(
    allocator: Allocator,
    type_store: *Type.Store,
    first: Type.Idx,
    second: Type.Idx,
) !UnificationResult {
    const first_type = type_store.get(first);
    const second_type = type_store.get(second);

    var result = UnificationResult{
        .mismatches = std.ArrayList(TypeMismatch).init(allocator),
        .has_changed = false,
    };

    if (first == second) {
        return result;
    }

    try unifyType(allocator, type_store, &result, first, first_type, second, second_type);

    return result;
}

fn unifyType(
    allocator: Allocator,
    type_store: *Type.Store,
    result: *UnificationResult,
    first: Type.Idx,
    first_type: *Type,
    second: Type.Idx,
    second_type: *Type,
) !void {
    _ = allocator;

    switch (first_type.*) {
        .bool => {
            @panic("todo");
        },
        .apply => {
            @panic("todo");
        },
        .str => {
            @panic("todo");
        },
        .int => |i| {
            switch (i) {
                .u8 => @panic("todo"),
                .i8 => @panic("todo"),
                .u16 => @panic("todo"),
                .i16 => @panic("todo"),
                .u32 => @panic("todo"),
                .i32 => @panic("todo"),
                .u64 => @panic("todo"),
                .i64 => @panic("todo"),
                .u128 => @panic("todo"),
                .i128 => @panic("todo"),
            }
        },
        .frac => {
            @panic("todo");
        },
        .flex_var => |opt_name| unifyFlex(type_store, result, first, opt_name, second, second_type),
        .rigid_var => {
            @panic("todo");
        },
        .func => {
            @panic("todo");
        },
        .type_error => {
            @panic("todo");
        },
    }
}

fn unifyFlex(
    type_store: *Type.Store,
    result: *UnificationResult,
    first: Type.Idx,
    opt_name: ?Ident.Idx,
    second: Type.Idx,
    second_type: *Type,
) void {
    switch (second_type.*) {
        .flex_var => |other_name| {
            // Prefer right's name
            const name = other_name orelse opt_name;
            merge(type_store, result, first, second, .{ .flex_var = name });
        },
        .bool => {
            @panic("todo");
        },
        .apply => {
            @panic("todo");
        },
        .str => {
            @panic("todo");
        },
        .int => |i| {
            switch (i) {
                .u8 => @panic("todo"),
                .i8 => @panic("todo"),
                .u16 => @panic("todo"),
                .i16 => @panic("todo"),
                .u32 => @panic("todo"),
                .i32 => @panic("todo"),
                .u64 => @panic("todo"),
                .i64 => @panic("todo"),
                .u128 => @panic("todo"),
                .i128 => @panic("todo"),
            }
        },
        .frac => {
            @panic("todo");
        },
        .rigid_var => {
            @panic("todo");
        },
        .func => {
            @panic("todo");
        },
        .type_error => {
            @panic("todo");
        },
    }
}

fn merge(
    type_store: *Type.Store,
    result: *UnificationResult,
    left: Type.Idx,
    right: Type.Idx,
    type_value: Type,
) void {
    type_store.set(left, type_value);
    type_store.set(right, type_value);
    result.has_changed = true;
}
