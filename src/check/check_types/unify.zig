const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../../base.zig");

const roc_type = @import("../../types/type.zig");
const Type = roc_type.Type;
const Mark = roc_type.Mark;
const Rank = roc_type.Rank;
const Descriptor = roc_type.Descriptor;
const Ident = base.Ident;
const ModuleEnv = base.ModuleEnv;

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
    const first_desc = env.type_store.get(first);
    const second_desc = env.type_store.get(second);

    var result = UnificationResult{
        .mismatches = std.ArrayList(TypeMismatch).init(allocator),
        .has_changed = false,
    };

    if (first == second) {
        return result;
    }

    var ctx = Context{
        .allocator = allocator,
        .env = env,
        .first = first,
        .first_desc = first_desc,
        .second = second,
        .second_desc = second_desc,
        .result = &result,
    };

    try ctx.unify();

    return result;
}

const Context = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    first: Type.Idx,
    first_desc: *const Descriptor,
    second: Type.Idx,
    second_desc: *const Descriptor,
    result: *UnificationResult,

    fn unify(self: *Context) !void {
        switch (self.first_desc.type) {
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
            .flex_var => |opt_name| self.unifyFlex(opt_name),
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

    fn unifyFlex(self: *Context, opt_name: ?Ident.Idx) void {
        switch (self.second_desc.type) {
            .flex_var => |other_name| {
                // Prefer right's name
                const name = other_name orelse opt_name;
                self.merge(.{ .flex_var = name });
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

    fn merge(self: *Context, ty: Type) void {
        const rank = self.first_desc.rank.min(self.second_desc.rank);
        const desc = Descriptor{
            .rank = rank,
            .mark = Mark.NONE,
            .type = ty,
        };

        // TODO union
        self.env.type_store.set(self.first, desc);
        self.env.type_store.set(self.second, desc);
        self.result.has_changed = true;
    }
};
