//! Four-byte optional typed IDs shared by persisted rows and transient columns.
//! Shared zero-based domains bias the stored value; a private domain may
//! instead reserve slot zero and store its IDs unchanged.

const std = @import("std");

/// Zero means absent; other words store the zero-based ID plus one.
pub fn OptionalId(comptime Id: type) type {
    return IndexValue(Id, false);
}

/// Zero means absent in a domain whose producer issues IDs starting at one.
pub fn ZeroSentinelId(comptime Id: type) type {
    return IndexValue(Id, true);
}

fn IndexValue(comptime Id: type, comptime reserved_zero: bool) type {
    comptime {
        const info = @typeInfo(Id);
        if (info != .@"enum" or info.@"enum".tag_type != u32)
            @compileError("optional IDs require a typed u32 index");
    }
    return enum(u32) {
        none = 0,
        _,

        const Self = @This();

        /// The type this form stores optionally.
        pub const CompactPayload = Id;

        /// Wrap an ID within the owning store's bounded index domain.
        pub inline fn some(id: Id) Self {
            const raw = @backingInt(id);
            std.debug.assert(raw < std.math.maxInt(u32));
            if (reserved_zero) std.debug.assert(raw != 0);
            return @fromBackingInt(if (reserved_zero) raw else raw + 1);
        }

        /// Wrap an optional ID: null is absence.
        pub inline fn of(id: ?Id) Self {
            return if (id) |present| some(present) else .none;
        }

        /// Read the original ID, or null for absence.
        pub inline fn get(self: Self) ?Id {
            if (self == .none) return null;
            const raw = @backingInt(self);
            return @fromBackingInt(if (reserved_zero) raw else raw - 1);
        }
    };
}

/// The payload of a compact optional form from this file, or null for any
/// other type. A reflective walker treats a compact optional exactly as the
/// `?Payload` its `get` returns.
pub fn CompactOptionalPayload(comptime T: type) ?type {
    const info = @typeInfo(T);
    if (info != .@"struct" and info != .@"enum" and info != .@"union") return null;
    if (!@hasDecl(T, "CompactPayload")) return null;
    return T.CompactPayload;
}

/// An optional raw `u32` (an offset or an index without a typed ID) in four
/// bytes: zero means absent, other words store the value plus one.
pub const OptionalU32 = enum(u32) {
    none = 0,
    _,

    /// The type this form stores optionally.
    pub const CompactPayload = u32;

    /// Wrap a value below `maxInt(u32)`.
    pub inline fn some(value: u32) OptionalU32 {
        std.debug.assert(value < std.math.maxInt(u32));
        return @fromBackingInt(value + 1);
    }

    /// Wrap an optional value: null is absence.
    pub inline fn of(value: ?u32) OptionalU32 {
        return if (value) |present| some(present) else .none;
    }

    /// Read the original value, or null for absence.
    pub inline fn get(self: OptionalU32) ?u32 {
        if (self == .none) return null;
        return @backingInt(self) - 1;
    }
};

/// An optional enum value in one byte: zero means absent, other bytes store
/// the tag's integer value plus one. `E` must have fewer than 256 tags, each
/// with a value below 255.
pub fn OptionalEnum(comptime E: type) type {
    comptime {
        const info = @typeInfo(E);
        if (info != .@"enum" or info.@"enum".mode != .exhaustive or @bitSizeOf(info.@"enum".tag_type) > 8)
            @compileError("optional enums require an exhaustive enum of at most eight bits");
        for (info.@"enum".field_values) |value| {
            if (value >= std.math.maxInt(u8)) @compileError("optional enum tag values must leave room for absence");
        }
    }
    return enum(u8) {
        none = 0,
        _,

        const Self = @This();

        /// The type this form stores optionally.
        pub const CompactPayload = E;

        pub inline fn some(value: E) Self {
            return @fromBackingInt(@as(u8, @backingInt(value)) + 1);
        }

        /// Wrap an optional value: null is absence.
        pub inline fn of(value: ?E) Self {
            return if (value) |present| some(present) else .none;
        }

        /// Read the original value, or null for absence.
        pub inline fn get(self: Self) ?E {
            if (self == .none) return null;
            return @fromBackingInt(@as(@typeInfo(E).@"enum".tag_type, @intCast(@backingInt(self) - 1)));
        }
    };
}

/// An optional struct value at the struct's own size. A present value is
/// stored with its `key` field (a `u32` or a `u32`-backed enum) plus one; an
/// absent value is all zeros. Every other field of `T` must accept zero.
pub fn OptionalStruct(comptime T: type, comptime key: []const u8) type {
    comptime {
        const Key = @FieldType(T, key);
        const key_info = @typeInfo(Key);
        const key_ok = Key == u32 or (key_info == .@"enum" and key_info.@"enum".tag_type == u32);
        if (!key_ok) @compileError("optional structs require a u32 or u32-backed enum key field");
    }
    return struct {
        biased: T = std.mem.zeroes(T),

        const Self = @This();
        const Key = @FieldType(T, key);

        /// The type this form stores optionally.
        pub const CompactPayload = T;

        pub const none: Self = .{};

        /// Wrap a value whose key is below `maxInt(u32)`.
        pub fn some(value: T) Self {
            var biased = value;
            const raw = keyRaw(@field(value, key));
            std.debug.assert(raw < std.math.maxInt(u32));
            @field(biased, key) = keyFromRaw(raw + 1);
            return .{ .biased = biased };
        }

        /// Wrap an optional value: null is absence.
        pub fn of(value: ?T) Self {
            return if (value) |present| some(present) else .none;
        }

        /// Read the original value, or null for absence.
        pub fn get(self: Self) ?T {
            const raw = keyRaw(@field(self.biased, key));
            if (raw == 0) {
                std.debug.assert(std.meta.eql(self.biased, none.biased));
                return null;
            }
            var value = self.biased;
            @field(value, key) = keyFromRaw(raw - 1);
            return value;
        }

        fn keyRaw(value: Key) u32 {
            return if (Key == u32) value else @backingInt(value);
        }

        fn keyFromRaw(raw: u32) Key {
            return if (Key == u32) raw else @fromBackingInt(raw);
        }
    };
}

test "optional IDs use one word and preserve both numbering policies" {
    const Id = enum(u32) { zero = 0, _ };
    const Biased = OptionalId(Id);
    const Direct = ZeroSentinelId(Id);
    try std.testing.expectEqual(@as(usize, 4), @sizeOf(Biased));
    try std.testing.expectEqual(@as(usize, 4), @sizeOf(Direct));
    try std.testing.expect(Biased.none.get() == null);
    try std.testing.expect(Direct.none.get() == null);
    const first: Id = .zero;
    const one: Id = @fromBackingInt(1);
    const last: Id = @fromBackingInt(std.math.maxInt(u32) - 1);
    try std.testing.expectEqual(first, Biased.some(first).get().?);
    try std.testing.expectEqual(last, Biased.some(last).get().?);
    try std.testing.expectEqual(one, Direct.some(one).get().?);
    try std.testing.expectEqual(last, Direct.some(last).get().?);
    try std.testing.expectEqual(@as(u32, 1), @backingInt(Biased.some(first)));
    try std.testing.expectEqual(@as(u32, 1), @backingInt(Direct.some(one)));
    try std.testing.expect(Biased.of(null) == .none);
    try std.testing.expectEqual(last, Biased.of(last).get().?);
}

test "optional raw values, enums and structs fit their payload width" {
    try std.testing.expectEqual(@as(usize, 4), @sizeOf(OptionalU32));
    try std.testing.expect(OptionalU32.none.get() == null);
    try std.testing.expectEqual(@as(u32, 0), OptionalU32.some(0).get().?);
    try std.testing.expectEqual(std.math.maxInt(u32) - 1, OptionalU32.of(std.math.maxInt(u32) - 1).get().?);

    const Small = enum { a, b };
    const OptSmall = OptionalEnum(Small);
    try std.testing.expectEqual(@as(usize, 1), @sizeOf(OptSmall));
    try std.testing.expect(OptSmall.none.get() == null);
    try std.testing.expectEqual(Small.a, OptSmall.some(.a).get().?);
    try std.testing.expectEqual(Small.b, OptSmall.of(.b).get().?);

    const Id = enum(u32) { _ };
    const Pair = struct { id: Id, offset: u32 };
    const OptPair = OptionalStruct(Pair, "offset");
    try std.testing.expectEqual(@sizeOf(Pair), @sizeOf(OptPair));
    try std.testing.expect(OptPair.none.get() == null);
    const zero_offset_pair: Pair = .{ .id = @fromBackingInt(3), .offset = 0 };
    try std.testing.expectEqual(zero_offset_pair, OptPair.some(zero_offset_pair).get().?);
    const OptById = OptionalStruct(Pair, "id");
    const pair: Pair = .{ .id = @fromBackingInt(7), .offset = 9 };
    try std.testing.expectEqual(pair, OptById.of(pair).get().?);
    try std.testing.expect(std.meta.eql(OptById.of(null), OptById.none));

    try std.testing.expect(CompactOptionalPayload(OptionalU32).? == u32);
    try std.testing.expect(CompactOptionalPayload(OptSmall).? == Small);
    try std.testing.expect(CompactOptionalPayload(OptById).? == Pair);
    try std.testing.expect(CompactOptionalPayload(OptionalId(Id)).? == Id);
    try std.testing.expect(CompactOptionalPayload(Pair) == null);
    try std.testing.expect(CompactOptionalPayload(u32) == null);
}
