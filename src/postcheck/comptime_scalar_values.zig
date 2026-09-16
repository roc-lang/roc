//! Completed compile-time scalar values, ready to lower as literals.
//!
//! A runtime continuation forked from a completed host program is lowered
//! after every compile-time root has been evaluated, so a scalar root's
//! value is known when its read is lowered. Reading it through a
//! static-data slot instead would hide the constant from the LIR passes that
//! reason about constants—range proving, loop versioning, overflow
//! elision—so a table built by `List.repeat` with a compile-time length
//! would keep every index check the prover otherwise discharges. This table
//! holds each completed successful scalar root's literal, decoded from the
//! host's frozen image and keyed by checked root identity exactly as the
//! later transcoding matches slots, so the lowerer emits the literal
//! directly and creates no slot, failure record, or guard for it. Aggregate
//! roots keep their slots and fold in the backend; failed roots keep the
//! guard that crashes with the original failure.
const std = @import("std");
const check = @import("check");
const core = @import("lir_core");
const layout = @import("layout");
const checked = check.CheckedModule;
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;

/// Literals of the completed successful scalar roots of one host program,
/// keyed by checked root identity.
pub const CompletedScalarValues = struct {
    entries: Map,

    const Key = struct {
        module: checked.ModuleId,
        root: checked.ComptimeRootId,
    };

    const Entry = struct {
        layout_idx: layout.Idx,
        literal: LIR.LiteralValue,
    };

    const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(&key.module.bytes);
            hasher.update(std.mem.asBytes(&key.root));
            return hasher.final();
        }

        pub fn eql(_: Context, a: Key, b: Key) bool {
            return std.meta.eql(a.module, b.module) and a.root == b.root;
        }
    };

    const Map = std.HashMapUnmanaged(Key, Entry, Context, std.hash_map.default_max_load_percentage);

    pub const empty: CompletedScalarValues = .{ .entries = .empty };

    /// Collects every completed successful scalar root of `program` from its
    /// frozen image.
    pub fn init(allocator: Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!CompletedScalarValues {
        var values = CompletedScalarValues.empty;
        errdefer values.deinit(allocator);
        for (program.static_data_values.items, 0..) |entry, index| {
            const root = entry.compile_time_root orelse continue;
            if (root.role != .value) continue;
            const slot: LIR.StaticDataId = @enumFromInt(index);
            if (!slotSucceeded(program, frozen, slot)) continue;
            const data_export = exportOf(frozen, slot) orelse continue;
            if (data_export.relocations.len != 0) continue;
            const literal = decodeScalar(entry.layout_idx, data_export.bytes[data_export.symbol_offset..]) orelse continue;
            try values.entries.put(allocator, .{ .module = root.module, .root = root.root }, .{ .layout_idx = entry.layout_idx, .literal = literal });
        }
        return values;
    }

    pub fn deinit(self: *CompletedScalarValues, allocator: Allocator) void {
        self.entries.deinit(allocator);
    }

    /// The literal for a root read at `layout_idx`, when the root completed
    /// successfully with a scalar of that layout.
    pub fn literalFor(self: *const CompletedScalarValues, module: checked.ModuleId, root: checked.ComptimeRootId, layout_idx: layout.Idx) ?LIR.LiteralValue {
        const entry = self.entries.get(.{ .module = module, .root = root }) orelse return null;
        if (entry.layout_idx != layout_idx) return null;
        return entry.literal;
    }
};

/// Whether the completed value in `slot` is a successful root: its failure
/// record's `failed` byte is zero in the frozen image.
fn slotSucceeded(program: *const Program.Result, frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) bool {
    const root = program.static_data_values.items[@intFromEnum(slot)].compile_time_root orelse return false;
    if (root.role != .value) return false;
    const failure_slot = root.role.value.failure_slot;
    const failure_root = program.static_data_values.items[@intFromEnum(failure_slot)].compile_time_root orelse return false;
    if (failure_root.role != .failure_message) return false;
    const failure_export = exportOf(frozen, failure_slot) orelse return false;
    const offset = failure_export.symbol_offset + failure_root.role.failure_message.failed_offset;
    if (offset >= failure_export.bytes.len) return false;
    return failure_export.bytes[offset] == 0;
}

fn exportOf(frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (item.value_id == slot) return item;
    }
    return null;
}

/// The literal form of a scalar's target bytes, or null for a layout the
/// LIR has no literal for.
fn decodeScalar(layout_idx: layout.Idx, bytes: []const u8) ?LIR.LiteralValue {
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
        .bool, .str, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => null,
        _ => null,
    };
}

fn intLiteral(comptime Int: type, layout_idx: layout.Idx, bytes: []const u8) ?LIR.LiteralValue {
    const size = @sizeOf(Int);
    if (bytes.len < size) return null;
    const value = std.mem.readInt(Int, bytes[0..size], .little);
    return .{ .i128_literal = .{ .value = @intCast(value), .layout_idx = layout_idx } };
}

test "completed successful scalar roots decode to literals; failed and aggregate roots do not" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
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
                .root = @enumFromInt(index),
                .const_locator = null,
                .role = switch (role) {
                    .failure => .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } },
                    .value => .{ .value = .{ .failure_slot = @enumFromInt(index - 1), .plan = plan } },
                    .string => .{ .value = .{ .failure_slot = @enumFromInt(4), .plan = plan } },
                },
            },
        });
    }
    var ok_record = [_]u8{0} ** 32;
    var failed_record = [_]u8{0} ** 32;
    failed_record[failed_offset] = 1;
    const relocation = [_]Program.StaticDataRelocation{.{ .offset = 0, .target_symbol_name = "backing" }};
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "s0", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .bytes = &.{ 0x39, 0x30, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s2", .bytes = &failed_record, .alignment = 8 },
        .{ .symbol_name = "s3", .bytes = &.{ 7, 0, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s4", .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s5", .bytes = &.{ 0xfe, 0xff }, .alignment = 2 },
        .{ .symbol_name = "s6", .bytes = &([_]u8{0} ** 24), .alignment = 8, .relocations = &relocation },
    };
    // This program's static roots are exported densely in root order, so each
    // export carries the id of its own position.
    for (&exports, 0..) |*item, index| item.value_id = @enumFromInt(@as(u32, @intCast(index)));
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };

    var values = try CompletedScalarValues.init(allocator, &program, &frozen);
    defer values.deinit(allocator);
    const first = values.literalFor(.{}, exports[1].value_id.?, .u32) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, 12345), first.i128_literal.value);
    try std.testing.expectEqual(layout.Idx.u32, first.i128_literal.layout_idx);
    try std.testing.expect(values.literalFor(.{}, exports[1].value_id.?, .u64) == null);
    try std.testing.expect(values.literalFor(.{}, exports[3].value_id.?, .u32) == null);
    const third = values.literalFor(.{}, exports[5].value_id.?, .i16) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i128, -2), third.i128_literal.value);
    try std.testing.expect(values.literalFor(.{}, exports[6].value_id.?, .str) == null);
}
