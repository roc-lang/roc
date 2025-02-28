const std = @import("std");
const testing = std.testing;
const collections = @import("../collections.zig");
const base = @import("../base.zig");
const exitOnOom = @import("../collections/utils.zig").exitOnOom;

const Ident = base.Ident;

pub const Type = union(enum) {
    /// Builtin `Bool` type
    bool,

    /// Builtin `Str` type
    str,

    /// Builtin `Int a` type
    int: Int,

    /// Builtin `Float a` type
    frac: Frac,

    /// A type variable which the user did not name in an annotation
    flex_var: ?Ident.Idx,

    /// Name given in a user-written annotation
    rigid_var: Ident.Idx,

    /// Function
    func: Func,

    /// Type application (e.g List(Str))
    apply: Apply,

    /// Type error
    type_error,

    pub const Func = struct {
        arguments: []Idx,
        lambda_set: Idx,
        result: Idx,
        fx: ?enum {
            pure,
            effectful,
        },
    };

    pub const Apply = struct {
        name: Idx,
        arguments: []Idx,
    };

    pub const Frac = enum {
        f32,
        f64,
        dec,
    };

    pub const Int = enum {
        u8,
        i8,
        u16,
        i16,
        u32,
        i32,
        u64,
        i64,
        u128,
        i128,
    };

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .bool => {
                try writer.writeAll("Bool");
            },
            .apply => {
                @panic("todo");
            },
            .str => {
                @panic("todo");
            },
            .int => |i| {
                switch (i) {
                    .u8 => try writer.writeAll("U8"),
                    .i8 => try writer.writeAll("I8"),
                    .u16 => try writer.writeAll("U16"),
                    .i16 => try writer.writeAll("I16"),
                    .u32 => try writer.writeAll("U32"),
                    .i32 => try writer.writeAll("I32"),
                    .u64 => try writer.writeAll("U64"),
                    .i64 => try writer.writeAll("I64"),
                    .u128 => try writer.writeAll("U128"),
                    .i128 => try writer.writeAll("I128"),
                }
            },
            .frac => {
                @panic("todo");
            },
            .flex_var => {
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

    pub const Idx = enum(u31) { _ };

    pub const Store = struct {
        /// Slots that haven't been pushed yet
        pending: usize,
        slots: Slot.List,
        descriptors: Descriptor.List,

        const Slot = packed struct {
            tag: enum(u1) { root, redirect },
            value: u31,

            const UNINITIALIZED: Slot = .{ .tag = .root, .value = 0 };
            const List = std.ArrayList(Slot);

            fn root(idx: u31) Slot {
                return .{ .tag = .root, .value = idx };
            }

            fn redirect(idx: Idx) Slot {
                return .{ .tag = .redirect, .value = @intFromEnum(idx) };
            }
        };

        pub fn init(allocator: std.mem.Allocator) Store {
            var store = Store{
                .pending = 0,
                .slots = Slot.List.init(allocator),
                .descriptors = Descriptor.List.init(allocator),
            };

            store.descriptors.append(Descriptor.NONE) catch exitOnOom();

            return store;
        }

        pub fn deinit(self: *Store) void {
            self.slots.deinit();
            self.descriptors.deinit();
        }

        pub fn get(self: *Store, idx: Idx) *const Descriptor {
            return self.getInitialized(idx) orelse &Descriptor.NONE;
        }

        pub fn set(self: *Store, idx: Idx, descriptor: Descriptor) void {
            if (self.getInitialized(idx)) |current| {
                current.* = descriptor;
            } else {
                self.descriptors.append(descriptor) catch exitOnOom();
                const descriptor_idx: u31 = @intCast(self.descriptors.items.len - 1);

                self.setSlot(idx, Slot.root(descriptor_idx));
            }
        }

        fn setSlot(self: *Store, idx: Idx, slot: Slot) void {
            if (self.pending > 0) {
                const slice = self.slots.addManyAsSlice(self.pending) catch exitOnOom();
                @memset(slice, Slot.UNINITIALIZED);

                // Assuming more than half of slots are redirects
                self.descriptors.ensureTotalCapacity(self.slots.items.len / 2) catch exitOnOom();
                self.pending = 0;
            }

            const slot_idx = @intFromEnum(idx);
            self.slots.items[slot_idx] = slot;
        }

        pub fn merge(self: *Store, to: Idx, from: Idx, desc: Descriptor) void {
            const to_desc = self.getInitialized(to);
            const from_desc = self.getInitialized(from);

            if (to_desc == null or from_desc == null or to_desc != from_desc) {
                self.setSlot(from, Slot.redirect(to));
            }

            self.set(to, desc);
        }

        /// Returns a pointer to a descriptor or null if uninitialized
        fn getInitialized(self: *Store, idx: Idx) ?*Descriptor {
            const slot_idx = @intFromEnum(idx);
            if (slot_idx >= self.slots.items.len) {
                return null;
            }

            var slot = self.slots.items[slot_idx];

            while (slot.tag == .redirect) {
                slot = self.slots.items[@as(usize, slot.value)];
            }
            // TODO compacting

            if (slot.value == Slot.UNINITIALIZED.value) {
                return null;
            }

            return &self.descriptors.items[slot.value];
        }

        /// Create a slot for a type without initializing it
        pub fn fresh(self: *Store) Idx {
            defer self.pending += 1;
            return @enumFromInt(self.slots.items.len + self.pending);
        }
    };
};

pub const Rank = struct {
    rank: u32,
    pub const GENERALIZED: Rank = .{ .rank = 0 };

    pub fn min(self: Rank, other: Rank) Rank {
        return .{ .rank = @min(self.rank, other.rank) };
    }
};

pub const Mark = struct {
    mark: i32,
    pub const NONE: Mark = .{ .mark = 0 };
};

pub const Descriptor = struct {
    rank: Rank,
    mark: Mark,
    type: Type,

    const NONE: Descriptor = .{ .rank = Rank.GENERALIZED, .mark = Mark.NONE, .type = .{ .flex_var = null } };

    const List = std.ArrayList(Descriptor);
};

test "store" {
    var store = Type.Store.init(testing.allocator);
    defer store.deinit();

    // Test fresh index creation
    const idx1 = store.fresh();
    const idx2 = store.fresh();
    try testing.expect(@intFromEnum(idx1) < @intFromEnum(idx2));

    // Test uninitialized slot returns NONE
    try testing.expectEqual(store.get(idx1).type, Descriptor.NONE.type);

    // Test setting and getting a descriptor
    const desc = .{ .rank = Rank.GENERALIZED, .mark = Mark.NONE, .type = Type.bool };
    store.set(idx1, desc);
    try testing.expectEqual(store.get(idx1).type, Type.bool);

    // Test updating an existing descriptor
    const updated_desc = .{ .rank = Rank.GENERALIZED, .mark = Mark.NONE, .type = Type.str };
    store.set(idx1, updated_desc);
    try testing.expectEqual(store.get(idx1).type, Type.str);

    // Test merge
    const idx3 = store.fresh();
    const idx4 = store.fresh();
    const union_desc = .{ .rank = Rank.GENERALIZED, .mark = Mark.NONE, .type = .bool };
    store.merge(idx3, idx4, union_desc);
    try testing.expectEqual(store.get(idx3).type, .bool);
    try testing.expectEqual(store.get(idx4).type, .bool);

    // TODO test double redirect
}

// test "formatting" {
//     var store = Type.Store.init(std.testing.allocator);
//     defer store.deinit();

//     const bool_str = try std.fmt.allocPrint(testing.allocator, "{}", .{store.get(Type.Store.BOOL)});
//     defer testing.allocator.free(bool_str);

//     try testing.expectEqualStrings(bool_str, "Bool");

//     const i128_str = try std.fmt.allocPrint(testing.allocator, "{}", .{store.get(Type.Store.I128)});
//     defer testing.allocator.free(i128_str);

//     try testing.expectEqualStrings(i128_str, "I128");
// }
