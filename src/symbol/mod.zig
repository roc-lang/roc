//! Stable symbol identities shared across the cor-style lowering pipeline.
//!
//! These are not parser idents. A symbol names one specific binding occurrence.
//! The pipeline threads symbols through specialization, lambda lifting, lambda-set
//! solving, monomorphization, and final lowering.

const std = @import("std");
const base = @import("base");

pub const Symbol = enum(u32) {
    _,

    pub const none: Symbol = @enumFromInt(std.math.maxInt(u32));

    pub fn fromRaw(value: u32) Symbol {
        return @enumFromInt(value);
    }

    pub fn raw(self: Symbol) u32 {
        return @intFromEnum(self);
    }

    pub fn isNone(self: Symbol) bool {
        return self == Symbol.none;
    }
};

pub const BindingOrigin = union(enum) {
    top_level_def: struct {
        module_idx: u32,
        def_idx: u32,
    },
    specialized_top_level_def: struct {
        source_symbol: u32,
    },
    specialized_local_fn: struct {
        source_symbol: u32,
    },
    local_pattern: struct {
        module_idx: u32,
        pattern_idx: u32,
    },
    lifted_local_fn: struct {
        source_symbol: u32,
    },
    lifted_lambda: struct {
        module_idx: u32,
        expr_idx: u32,
    },
    synthetic: void,
};

pub const Entry = struct {
    name: base.Ident.Idx,
    origin: BindingOrigin,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayList(Entry),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .entries = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.entries.deinit(self.allocator);
    }

    pub fn add(self: *Store, name: base.Ident.Idx, origin: BindingOrigin) std.mem.Allocator.Error!Symbol {
        const idx: u32 = @intCast(self.entries.items.len);
        try self.entries.append(self.allocator, .{
            .name = name,
            .origin = origin,
        });
        return @enumFromInt(idx);
    }

    pub fn get(self: *const Store, symbol: Symbol) Entry {
        return self.entries.items[@intFromEnum(symbol)];
    }

    pub fn len(self: *const Store) usize {
        return self.entries.items.len;
    }
};

test "symbol tests" {
    std.testing.refAllDecls(@This());
}
