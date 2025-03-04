const std = @import("std");
const testing = std.testing;
const collections = @import("../collections.zig");
const base = @import("../base.zig");

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

    pub const Store = struct {
        env: *base.ModuleEnv,
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

        pub fn init(env: *base.ModuleEnv) Store {
            var store = Store{ .env = env, .types = .{} };

            // APPEND THE BUILTINS ORDER MATTERS FOR THE CONSTANTS
            // DEFINED ABOVE

            _ = store.types.append(env.gpa, .bool);
            _ = store.types.append(env.gpa, .str);
            _ = store.types.append(env.gpa, .{ .int = .u8 });
            _ = store.types.append(env.gpa, .{ .int = .i8 });
            _ = store.types.append(env.gpa, .{ .int = .u16 });
            _ = store.types.append(env.gpa, .{ .int = .i16 });
            _ = store.types.append(env.gpa, .{ .int = .u32 });
            _ = store.types.append(env.gpa, .{ .int = .i32 });
            _ = store.types.append(env.gpa, .{ .int = .u64 });
            _ = store.types.append(env.gpa, .{ .int = .i64 });
            _ = store.types.append(env.gpa, .{ .int = .u128 });
            _ = store.types.append(env.gpa, .{ .int = .i128 });

            // TODO other builtins... can we find a nicer solution for managing this?

            return store;
        }

        pub fn deinit(self: *Store) void {
            self.types.deinit(self.env.gpa);
        }

        pub fn get(self: *const Store, id: Idx) *Type {
            return self.types.get(id);
        }

        pub fn set(self: *const Store, id: Idx, value: Type) void {
            self.types.set(id, value);
        }

        /// Create a fresh type variable
        /// Used in canonicalization when creating type slots
        pub fn fresh(self: *Store) Idx {
            return self.types.append(self.env.gpa, .{ .flex_var = null });
        }
    };

    pub const List = collections.SafeList(Type);
    pub const Idx = List.Idx;
};

test "formatting" {
    const gpa = testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var store = Type.Store.init(&env);
    defer store.deinit();

    const bool_str = try std.fmt.allocPrint(gpa, "{}", .{store.get(Type.Store.BOOL)});
    defer gpa.free(bool_str);

    try testing.expectEqualStrings(bool_str, "Bool");

    const i128_str = try std.fmt.allocPrint(gpa, "{}", .{store.get(Type.Store.I128)});
    defer gpa.free(i128_str);

    try testing.expectEqualStrings(i128_str, "I128");
}
