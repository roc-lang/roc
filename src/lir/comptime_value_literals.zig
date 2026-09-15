//! Completed compile-time scalar values as literals.
//!
//! A runtime program forked from a completed host program reads each
//! compile-time value through a static-data slot. For a scalar value that
//! read is a load of bytes the compiler already holds, and the LIR passes
//! that reason about constants—range proving, loop versioning, overflow
//! elision—cannot see through it: a table built by `List.repeat` with a
//! compile-time length keeps every index check the prover would otherwise
//! discharge. This pass substitutes the decoded literal for every read of a
//! completed, successful scalar slot before those passes run, taking the
//! bytes from the host program's frozen image, whose slots are matched by
//! checked root identity exactly as the later transcoding matches them.
//! Reads of aggregate slots keep their static-data form and fold in the
//! backend, and reads of failed roots keep the guard that crashes with the
//! original failure.
const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;

/// Whether the completed value in `slot` is a successful root: its failure
/// record's `failed` byte is zero in the frozen image.
pub fn slotSucceeded(program: *const Program.Result, frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) bool {
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

/// The source program's value slot with the same checked root identity.
fn sourceSlotOf(source: *const Program.Result, target_root: anytype) ?LIR.StaticDataId {
    for (source.static_data_values.items, 0..) |entry, index| {
        const root = entry.compile_time_root orelse continue;
        if (root.role != .value) continue;
        if (std.meta.eql(root.module, target_root.module) and root.root == target_root.root) return @enumFromInt(index);
    }
    return null;
}

fn exportOf(frozen: *const Program.FrozenStaticData, slot: LIR.StaticDataId) ?*const Program.StaticDataExport {
    for (frozen.exports) |*item| {
        if (item.value_id == slot) return item;
    }
    return null;
}

/// Replace every read of a completed successful scalar slot in `program`
/// with its literal, decoded from `source`'s frozen image.
pub fn run(allocator: Allocator, program: *Program.Result, source: *const Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!void {
    const slot_count = program.static_data_values.items.len;
    if (slot_count == 0) return;
    const literals = try allocator.alloc(?LIR.LiteralValue, slot_count);
    defer allocator.free(literals);
    @memset(literals, null);
    var any = false;
    for (program.static_data_values.items, 0..) |entry, index| {
        const target_root = entry.compile_time_root orelse continue;
        if (target_root.role != .value) continue;
        const source_slot = sourceSlotOf(source, target_root) orelse continue;
        if (!slotSucceeded(source, frozen, source_slot)) continue;
        const data_export = exportOf(frozen, source_slot) orelse continue;
        if (data_export.relocations.len != 0) continue;
        const bytes = data_export.bytes[data_export.symbol_offset..];
        literals[index] = decodeScalar(entry.layout_idx, bytes) orelse continue;
        any = true;
    }
    if (!any) return;

    const store = &program.store;
    for (0..store.cfStmtCount()) |stmt_index| {
        const stmt = store.getCFStmtPtr(@enumFromInt(@as(u32, @intCast(stmt_index))));
        if (stmt.* != .assign_literal) continue;
        const assign = &stmt.assign_literal;
        if (assign.value != .static_data) continue;
        const raw = @intFromEnum(assign.value.static_data);
        if (raw >= slot_count) continue;
        if (literals[raw]) |literal| assign.value = literal;
    }
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

test "completed scalar slot reads become literals and failed or aggregate slots keep their reads" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);

    // Slots: 0 = failure record of 1, 1 = successful u32 value, 2 = failure
    // record of 3, 3 = failed u32 value, 4 = failure record of 5 and 6, 5 =
    // successful i16 value, 6 = a successful string value.
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
        .{ .symbol_name = "s0", .value_id = @enumFromInt(0), .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s1", .value_id = @enumFromInt(1), .bytes = &.{ 0x39, 0x30, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s2", .value_id = @enumFromInt(2), .bytes = &failed_record, .alignment = 8 },
        .{ .symbol_name = "s3", .value_id = @enumFromInt(3), .bytes = &.{ 7, 0, 0, 0 }, .alignment = 4 },
        .{ .symbol_name = "s4", .value_id = @enumFromInt(4), .bytes = &ok_record, .alignment = 8 },
        .{ .symbol_name = "s5", .value_id = @enumFromInt(5), .bytes = &.{ 0xfe, 0xff }, .alignment = 2 },
        .{ .symbol_name = "s6", .value_id = @enumFromInt(6), .bytes = &([_]u8{0} ** 24), .alignment = 8, .relocations = &relocation },
    };
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };

    const reads = [_]u32{ 1, 3, 5, 6 };
    var stmts: [reads.len]LIR.CFStmtId = undefined;
    for (reads, 0..) |slot, index| {
        const target = try program.store.addLocal(.{ .layout_idx = program.static_data_values.items[slot].layout_idx });
        const ret = try program.store.addCFStmt(.{ .ret = .{ .value = target } });
        stmts[index] = try program.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .static_data = @enumFromInt(slot) }, .next = ret } });
    }
    try run(allocator, &program, &program, &frozen);

    const first = program.store.getCFStmt(stmts[0]).assign_literal.value;
    try std.testing.expectEqual(@as(i128, 12345), first.i128_literal.value);
    try std.testing.expectEqual(layout.Idx.u32, first.i128_literal.layout_idx);
    try std.testing.expect(program.store.getCFStmt(stmts[1]).assign_literal.value == .static_data);
    try std.testing.expectEqual(@as(i128, -2), program.store.getCFStmt(stmts[2]).assign_literal.value.i128_literal.value);
    try std.testing.expect(program.store.getCFStmt(stmts[3]).assign_literal.value == .static_data);
}
