const std = @import("std");
const testing = std.testing;
const collections = @import("../collections.zig");
const TypeVarName = @import("../base/TypeVarName.zig");

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
    flex_var: ?TypeVarName.Idx,

    /// Name given in a user-written annotation
    rigid_var: TypeVarName.Idx,

    /// Function application
    func: Func,

    /// Function call
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
            .int => {
                @panic("todo");
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

    pub fn equal(self: *Type, other: *Type) bool {
        return self == other;
    }

    pub const Store = struct {
        types: List,

        pub const BOOL: Idx = @enumFromInt(0);
        pub const STR: Idx = @enumFromInt(1);
        pub const U8: Idx = @enumFromInt(2);
        pub const I8: Idx = @enumFromInt(3);
        pub const U16: Idx = @enumFromInt(4);
        pub const I16: Idx = @enumFromInt(5);
        pub const U32: Idx = @enumFromInt(6);
        pub const I32: Idx = @enumFromInt(7);
        pub const U64: Idx = @enumFromInt(8);
        pub const I64: Idx = @enumFromInt(9);
        pub const U128: Idx = @enumFromInt(10);
        pub const I128: Idx = @enumFromInt(11);

        pub fn init(allocator: std.mem.Allocator) Store {
            var store = Store{
                .types = List.init(allocator),
            };

            // APPEND THE BUILTINS ORDER MATTERS FOR THE CONSTANTS
            // DEFINED ABOVE

            _ = store.types.append(.bool);
            _ = store.types.append(.str);
            _ = store.types.append(.{ .int = .u8 });
            _ = store.types.append(.{ .int = .i8 });
            _ = store.types.append(.{ .int = .u16 });
            _ = store.types.append(.{ .int = .i16 });
            _ = store.types.append(.{ .int = .u32 });
            _ = store.types.append(.{ .int = .i32 });
            _ = store.types.append(.{ .int = .u64 });
            _ = store.types.append(.{ .int = .i64 });
            _ = store.types.append(.{ .int = .u128 });
            _ = store.types.append(.{ .int = .i128 });

            // TODO other builtins... can we find a nicer solution for managing this?

            return store;
        }

        pub fn deinit(self: *Store) void {
            self.types.deinit();
        }

        pub fn get(self: *Store, id: Idx) Type {
            return self.types.get(id);
        }

        /// Create a fresh type variable
        /// Used in canonicalization when creating type slots
        pub fn fresh(self: *Store) Idx {
            return self.types.append(.{ .flex_var = null });
        }
    };

    pub const List = collections.SafeList(Type);
    pub const Idx = List.Idx;
};

test "formatting" {
    var store = Type.Store.init(std.testing.allocator);
    defer store.deinit();

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try store.get(Type.Store.BOOL).format("", .{}, &buffer.writer());

    const formattedString = try buffer.toOwnedSlice();
    defer std.testing.allocator.free(formattedString);
    try testing.expectEqualStrings(formattedString, "Bool");
}
