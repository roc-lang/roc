const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../../base.zig");

const roc_type = @import("../../types/type.zig");
const RType = roc_type.RType;
const Prim = roc_type.Prim;
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

/// Unifies two types in a type store.
pub fn unify(
    allocator: Allocator,
    type_store: *RType.Store,
    first: RType.Idx,
    second: RType.Idx,
) !UnificationResult {
    const first_desc = type_store.get(first);
    const second_desc = type_store.get(second);

    var result = UnificationResult{
        .mismatches = std.ArrayList(TypeMismatch).init(allocator),
        .has_changed = false,
    };

    if (first == second) {
        return result;
    }

    var env = ModuleEnv.init(allocator);
    defer env.deinit();
    var ctx = Context{
        .allocator = allocator,
        .typeStore = type_store,
        .env = &env,
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
    allocator: Allocator, //TODO this probably should be merged with `env`
    typeStore: *RType.Store,
    env: *ModuleEnv,
    first: RType.Idx,
    first_desc: *const Descriptor,
    second: RType.Idx,
    second_desc: *const Descriptor,
    result: *UnificationResult,

    fn unify(self: *Context) !void {
        switch (self.first_desc.type) {
            .prim => |prim| {
                switch (prim) {
                    Prim.bool => {
                        try self.unifyBase();
                    },
                    Prim.str => {
                        try self.unifyBase();
                    },
                    Prim.int => {
                        try self.unifyBase();
                    },
                    Prim.frac => {
                        try self.unifyBase();
                    },
                }
            },
            .apply => {
                @panic("TODO");
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
    fn unifyBase(self: *Context) !void {
        if (@intFromEnum(self.first_desc.type) == @intFromEnum(self.second_desc.type)) {
            self.merge(self.first_desc.type);
        } else {
            @panic("TODO");
        }
    }

    fn unifyFlex(self: *Context, opt_name: ?Ident.Idx) void {
        switch (self.second_desc.type) {
            .flex_var => |other_name| {
                // Prefer right's name
                const name = other_name orelse opt_name;
                self.merge(.{ .flex_var = name });
            },
            .prim => |_| {
                self.merge(self.second_desc.type);
            },
            .apply => {
                @panic("todo");
            },
            .rigid_var => {
                self.merge(self.second_desc.type);
            },
            .func => {
                @panic("todo");
            },
            .type_error => {
                @panic("todo");
            },
        }
    }

    fn merge(self: *Context, ty: RType) void {
        const rank = self.first_desc.rank.min(self.second_desc.rank);
        const desc = Descriptor{
            .rank = rank,
            .mark = Mark.NONE,
            .type = ty,
        };

        self.typeStore.merge(self.first, self.second, desc);
        self.result.has_changed = true;
    }
};
