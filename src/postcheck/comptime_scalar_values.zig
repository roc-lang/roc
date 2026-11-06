//! Completed scalar values can be emitted as immediate literals. All
//! aggregate values remain in immutable static storage; this module cannot
//! generate construction instructions.
const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const core = @import("lir_core");
const layout = @import("layout");
const checked = check.CheckedModule;
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;

pub const ScalarLiteral = union(enum) {
    i64_literal: struct {
        value: i64,
        layout_idx: layout.Idx,
    },
    i128_literal: LIR.I128Literal,
    f64_literal: f64,
    f32_literal: f32,
    dec_literal: i128,

    /// The literal as `store` holds it.
    pub fn toLiteralValue(self: ScalarLiteral, store: *core.LirStore) Allocator.Error!LIR.LiteralValue {
        return switch (self) {
            .i64_literal => |int| .{ .i64_literal = .{ .value = int.value, .layout_idx = int.layout_idx } },
            .i128_literal => |int| .{ .i128_literal = try store.addStmtRow(LIR.I128Literal, int) },
            .f64_literal => |value| .{ .f64_literal = value },
            .f32_literal => |value| .{ .f32_literal = value },
            .dec_literal => |value| .{ .dec_literal = value },
        };
    }
};

pub const CompletedScalarValues = struct {
    entries: Map,
    /// The completed successful roots whose value is a string too short to
    /// need target storage (`Common.StaticDataStorage.needsTargetStorage`),
    /// with its bytes: a read lowers as the string's literal, exactly as a
    /// read restoring the stored string does.
    short_strings: StrMap,
    /// Owns the short-string bytes.
    arena: collections.SingleThreadArena,

    const Key = struct {
        module: checked.ModuleId,
        root: LIR.ComptimeProducer,
    };

    const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(&key.module.bytes);
            key.root.hash(&hasher);
            return hasher.final();
        }

        pub fn eql(_: Context, a: Key, b: Key) bool {
            return std.meta.eql(a.module, b.module) and a.root.eql(b.root);
        }
    };

    const Map = std.HashMapUnmanaged(Key, ScalarLiteral, Context, std.hash_map.default_max_load_percentage);
    const StrMap = std.HashMapUnmanaged(Key, []const u8, Context, std.hash_map.default_max_load_percentage);

    /// No completed values yet.
    pub fn initEmpty(allocator: Allocator) CompletedScalarValues {
        return .{ .entries = .empty, .short_strings = .empty, .arena = collections.SingleThreadArena.init(allocator) };
    }

    /// Collects every completed successful root of `program` whose frozen
    /// image decodes to a scalar immediate.
    pub fn init(allocator: Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!CompletedScalarValues {
        var values = initEmpty(allocator);
        errdefer values.deinit(allocator);
        try values.add(allocator, program, frozen, null);
        return values;
    }

    /// Which roots of a program `add` collects.
    pub const Filter = struct {
        context: *const anyopaque,
        admits: *const fn (context: *const anyopaque, module: checked.ModuleId, root: LIR.ComptimeProducer) bool,
    };

    /// Collects every completed successful root of `program` that `filter`
    /// admits, or every one without a filter, whose frozen image decodes to
    /// a scalar immediate.
    pub fn add(self: *CompletedScalarValues, allocator: Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData, filter: ?Filter) Allocator.Error!void {
        var decoder = Decoder{ .program = program, .frozen = frozen, .arena = self.arena.allocator() };
        for (program.static_data_values.items, 0..) |entry, index| {
            if (entry.representation_of != null) continue;
            const root = entry.compile_time_root orelse continue;
            if (root.role != .value) continue;
            if (filter) |admitted| if (!admitted.admits(admitted.context, root.module, root.root)) continue;
            const slot: LIR.StaticDataId = @fromBackingInt(@intCast(index));
            if (!slotSucceeded(program, frozen, slot)) continue;
            const data_export = exportOf(frozen, slot) orelse continue;
            const key = Key{ .module = root.module, .root = root.root };
            if (decoder.decodeShortString(data_export, data_export.bytes[data_export.symbol_offset..], data_export.symbol_offset, root.role.value.plan, entry.layout_idx)) |bytes| {
                try self.short_strings.put(allocator, key, try self.arena.allocator().dupe(u8, bytes));
                continue;
            }
            if (decodeScalar(entry.layout_idx, data_export.bytes[data_export.symbol_offset..])) |literal| {
                try self.entries.put(allocator, key, literal);
            }
        }
    }

    pub fn deinit(self: *CompletedScalarValues, allocator: Allocator) void {
        self.entries.deinit(allocator);
        self.short_strings.deinit(allocator);
        self.arena.deinit();
    }

    /// The bytes of the short string a root completed to, when it did.
    pub fn shortStringFor(self: *const CompletedScalarValues, module: checked.ModuleId, root: LIR.ComptimeProducer) ?[]const u8 {
        return self.short_strings.get(.{ .module = module, .root = root });
    }

    /// Scalar immediates preserve the completed value without construction.
    pub fn literalFor(self: *const CompletedScalarValues, module: checked.ModuleId, root: LIR.ComptimeProducer, layout_idx: layout.Idx) ?ScalarLiteral {
        const literal = self.entries.get(.{ .module = module, .root = root }) orelse return null;
        return if (literalFitsLayout(literal, layout_idx)) literal else null;
    }
};

fn literalFitsLayout(literal: ScalarLiteral, layout_idx: layout.Idx) bool {
    return switch (literal) {
        inline .i64_literal, .i128_literal => |int| int.layout_idx == layout_idx,
        .f32_literal => layout_idx == .f32,
        .f64_literal => layout_idx == .f64,
        .dec_literal => layout_idx == .dec,
    };
}

const Decoder = struct {
    program: *const Program.Result,
    frozen: ?*const Program.FrozenStaticData,
    arena: Allocator,
    pub fn decodeShortString(self: *Decoder, data_export: *const Program.StaticDataExport, bytes: []const u8, offset: usize, plan_id: Program.ConstPlanId, layout_idx: layout.Idx) ?[]const u8 {
        var plan = self.program.const_plans.items[@backingInt(plan_id)];
        while (plan == .named) plan = self.program.const_plans.items[@backingInt(plan.named.backing)];
        switch (plan) {
            .str => {},
            .zst, .scalar, .list, .named, .tuple, .record, .tag_union, .pending, .layout_only, .box, .boxy_fn, .fn_value, .erased_fn => return null,
        }
        if (layout_idx != .str) return null;
        const size = self.word();
        if (bytes.len < 3 * size) return null;
        const length_word = self.readWord(bytes, 2) orelse return null;
        const small = switch (size) {
            4 => @as(i32, @bitCast(@as(u32, @intCast(length_word)))) < 0,
            8 => @as(i64, @bitCast(length_word)) < 0,
            else => return null,
        };
        if (small) {
            const len = bytes[3 * size - 1] & 0x7f;
            if (len == 0) return null;
            return bytes[0..len];
        }
        if (length_word == 0 or length_word >= 3 * size) return null;
        const len: usize = @intCast(length_word);
        const frozen = self.frozen orelse return null;
        const relocation = relocationAt(data_export, offset) orelse return null;
        const backing = exportNamed(frozen, relocation.target_symbol_name) orelse return null;
        if (relocation.addend < 0) return null;
        const start = backing.symbol_offset + @as(usize, @intCast(relocation.addend));
        if (start > backing.bytes.len or backing.bytes.len - start < len) return null;
        return backing.bytes[start..][0..len];
    }

    fn word(self: *const Decoder) usize {
        return self.program.layouts.targetUsize().size();
    }

    fn readWord(self: *const Decoder, bytes: []const u8, index: usize) ?u64 {
        const size = self.word();
        if (bytes.len < (index + 1) * size) return null;
        const start = index * size;
        return switch (size) {
            4 => std.mem.readInt(u32, bytes[start..][0..4], .little),
            8 => std.mem.readInt(u64, bytes[start..][0..8], .little),
            else => null,
        };
    }
};

fn slotSucceeded(program: *const Program.Result, frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) bool {
    const root = program.static_data_values.items[@backingInt(slot)].compile_time_root orelse return false;
    if (root.role != .value) return false;
    const failure_slot = root.role.value.failure_slot;
    const failure_root = program.static_data_values.items[@backingInt(failure_slot)].compile_time_root orelse return false;
    if (failure_root.role != .failure_message) return false;
    const failure_export = exportOf(frozen, failure_slot) orelse return false;
    const offset = failure_export.symbol_offset + failure_root.role.failure_message.failed_offset;
    if (offset >= failure_export.bytes.len) return false;
    return failure_export.bytes[offset] == 0;
}

fn relocationAt(data_export: *const Program.StaticDataExport, offset: usize) ?Program.StaticDataRelocation {
    for (data_export.relocations) |relocation| {
        if (relocation.offset == offset) return relocation;
    }
    return null;
}

fn exportNamed(frozen: *const Program.FrozenStaticData, name: []const u8) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (std.mem.eql(u8, item.symbol_name, name)) return item;
    }
    return null;
}

fn exportOf(frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (item.value_id == slot) return item;
    }
    return null;
}

/// The literal form of a scalar's target bytes, or null for a layout the
/// LIR has no literal for.
fn decodeScalar(layout_idx: layout.Idx, bytes: []const u8) ?ScalarLiteral {
    return switch (layout_idx) {
        .u8 => intLiteral(u8, layout_idx, bytes),
        .i8 => intLiteral(i8, layout_idx, bytes),
        .u16 => intLiteral(u16, layout_idx, bytes),
        .i16 => intLiteral(i16, layout_idx, bytes),
        .u32 => intLiteral(u32, layout_idx, bytes),
        .i32 => intLiteral(i32, layout_idx, bytes),
        .u64 => intLiteral(u64, layout_idx, bytes),
        .i64 => intLiteral(i64, layout_idx, bytes),
        .u128 => intLiteral(u128, layout_idx, bytes),
        .i128 => intLiteral(i128, layout_idx, bytes),
        .f32 => if (bytes.len >= 4) .{ .f32_literal = @bitCast(std.mem.readInt(u32, bytes[0..4], .little)) } else null,
        .f64 => if (bytes.len >= 8) .{ .f64_literal = @bitCast(std.mem.readInt(u64, bytes[0..8], .little)) } else null,
        .dec => if (bytes.len >= 16) .{ .dec_literal = std.mem.readInt(i128, bytes[0..16], .little) } else null,
        .bool => intLiteral(u8, layout_idx, bytes),
        .str, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => null,
        _ => null,
    };
}

fn intLiteral(comptime Int: type, layout_idx: layout.Idx, bytes: []const u8) ?ScalarLiteral {
    const size = @sizeOf(Int);
    if (bytes.len < size) return null;
    const value = std.mem.readInt(Int, bytes[0..size], .little);
    // An integer literal carries its value's two's-complement bits in i128,
    // as source literals do: a u128 above the i128 range keeps its bit
    // pattern, and every narrower integer extends losslessly.
    return .{ .i128_literal = .{
        .value = if (Int == u128) @bitCast(value) else value,
        .layout_idx = layout_idx,
    } };
}

test "a u128 scalar above the i128 range decodes to its bit pattern" {
    const max_bytes = @as([16]u8, @splat(0xff));
    const max = decodeScalar(.u128, &max_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u128, std.math.maxInt(u128)), @as(u128, @bitCast(max.i128_literal.value)));
    try std.testing.expectEqual(layout.Idx.u128, max.i128_literal.layout_idx);

    const high_bit_bytes = @as([15]u8, @splat(0)) ++ [_]u8{0x80};
    const high_bit = decodeScalar(.u128, &high_bit_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u128, 1) << 127, @as(u128, @bitCast(high_bit.i128_literal.value)));

    const min_i128 = decodeScalar(.i128, &high_bit_bytes) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, std.math.minInt(i128)), min_i128.i128_literal.value);
}

test "completed successful scalar roots decode to literals; failed and aggregate roots do not" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .scalar);

    // Slots: 0 = failure record of 1, 1 = successful u32 root, 2 = failure
    // record of 3, 3 = failed u32 root, 4 = failure record of 5 and 6, 5 =
    // successful i16 root, 6 = successful string root.
    const roles = [_]enum { failure, value, string }{ .failure, .value, .failure, .value, .failure, .value, .string };
    for (roles, 0..) |role, index| {
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = switch (role) {
                .failure => record_layout,
                .value => if (index == 5) .i16 else .u32,
                .string => .str,
            },
            .compile_time_root = .{
                .module = .{},
                .root = .{ .checked = @fromBackingInt(@intCast(index)) },
                .const_locator = null,
                .role = switch (role) {
                    .failure => .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } },
                    .value => .{ .value = .{ .failure_slot = @fromBackingInt(@intCast(index - 1)), .plan = plan } },
                    .string => .{ .value = .{ .failure_slot = @fromBackingInt(@intCast(4)), .plan = plan } },
                },
            },
        });
    }
    var ok_record = @as([32]u8, @splat(0));
    var failed_record = @as([32]u8, @splat(0));
    failed_record[failed_offset] = 1;
    const relocation = [_]Program.StaticDataRelocation{.{ .offset = 0, .target_symbol_name = "backing" }};
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "s0", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .bytes = &.{ 0x39, 0x30, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s2", .bytes = &failed_record, .alignment = 8 },
        .{ .symbol_name = "s3", .bytes = &.{ 7, 0, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s4", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s5", .bytes = &.{ 0xfe, 0xff }, .alignment = 2 },
        .{ .symbol_name = "s6", .bytes = &(@as([24]u8, @splat(0))), .alignment = 8, .relocations = &relocation },
    };
    // This program's static roots are exported densely in root order, so each
    // export carries the id of its own position.
    for (&exports, 0..) |*item, index| item.value_id = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };

    var values = try CompletedScalarValues.init(allocator, &program, &frozen);
    defer values.deinit(allocator);
    const first = values.literalFor(.{}, .{ .checked = @fromBackingInt(@intCast(1)) }, .u32) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, 12345), first.i128_literal.value);
    try std.testing.expectEqual(layout.Idx.u32, first.i128_literal.layout_idx);
    try std.testing.expect(values.literalFor(.{}, .{ .checked = @fromBackingInt(@intCast(1)) }, .u64) == null);
    try std.testing.expect(values.literalFor(.{}, .{ .checked = @fromBackingInt(@intCast(3)) }, .u32) == null);
    const third = values.literalFor(.{}, .{ .checked = @fromBackingInt(@intCast(5)) }, .i16) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, -2), third.i128_literal.value);
    try std.testing.expect(values.literalFor(.{}, .{ .checked = @fromBackingInt(@intCast(6)) }, .str) == null);
}
