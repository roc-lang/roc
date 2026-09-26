//! Rebuilds completed compile-time roots inside their accessors when the
//! program that evaluated them is reused as the runtime program.
//!
//! A program lowered before its roots are evaluated reads each root through
//! an accessor procedure whose body is the slot read. Once the roots have
//! completed, a root that decodes to a construction (see
//! `ComptimeScalarValues`) is rebuilt in that body instead: an empty list
//! becomes the `with_capacity` it was evaluated with, a record or tag of
//! those their constructor. The call sites are untouched, so the reference
//! counting already inserted for them stays correct: a call result is an
//! owned value either way, and the emitted bodies are complete without a
//! further pass. The slots those accessors no longer read become
//! unreachable and leave the image with the next reachability pass.
const std = @import("std");
const core = @import("lir_core");
const postcheck = @import("postcheck");
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;
const scalar_values = postcheck.ComptimeScalarValues;

const EmitContext = struct {
    store: *core.LirStore,
    new_locals: *std.ArrayList(LIR.LocalId),

    pub fn addLocal(self: EmitContext, layout_idx: @import("layout").Idx) Allocator.Error!LIR.LocalId {
        const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.new_locals.append(self.store.allocator, local);
        return local;
    }
};

/// Replaces the body of every accessor whose root completed as a
/// construction with that construction.
pub fn rebuild(allocator: Allocator, result: *Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!void {
    var values = try scalar_values.CompletedScalarValues.init(allocator, result, frozen);
    defer values.deinit(allocator);
    if (values.entries.count() == 0) return;
    const store = &result.store;
    var new_locals: std.ArrayList(LIR.LocalId) = .empty;
    defer new_locals.deinit(allocator);
    for (result.static_data_values.items) |*entry| {
        if (entry.accessor_rebuilt) continue;
        const accessor = entry.accessor orelse continue;
        const root = entry.compile_time_root orelse continue;
        if (root.role != .value) continue;
        const construction = values.constructionFor(root.module, root.root, entry.layout_idx) orelse continue;
        new_locals.clearRetainingCapacity();
        const context = EmitContext{ .store = store, .new_locals = &new_locals };
        // The rebuilt body is compile-time constant reconstruction: it keeps
        // the location of the accessor body it replaces, which names the read
        // that requested the accessor.
        const replaced = store.stmtOrigin(store.getProcSpec(accessor).body.?);
        const origin = LIR.StmtOrigin{
            .loc = replaced.loc,
            .region = replaced.region,
            .inline_scope = replaced.inline_scope,
            .kind = .scaffold,
        };
        const value = try context.addLocal(entry.layout_idx);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = value } }, origin);
        const body = try scalar_values.emit(context, store, &result.layouts, origin, value, construction, ret) orelse continue;
        const locals = try store.addLocalSpan(new_locals.items);
        const proc = store.getProcSpecPtr(accessor);
        proc.body = body;
        proc.shapes = proc.shapes.merged(store.shapes);
        proc.frame_locals = locals;
        if (store.procNeedsStackProbe(&result.layouts, proc.*)) proc.stack_probe = .required;
        proc.native_code_revision += 1;
        entry.accessor_rebuilt = true;
    }
}

test "comptime root accessors declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "completed accessor reconstruction invalidates native code exactly once and failed roots stay guarded" {
    try testRebuild(std.testing.allocator);
}

test "completed accessor reconstruction allocation failure cleanup" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testRebuild, .{});
}

fn testRebuild(allocator: Allocator) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const failure_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(failure_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
    const list_layout = try program.layouts.insertList(.u32);
    const scalar_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);
    const list_plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .{ .list = scalar_plan });
    const failure_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = failure_layout,
        .compile_time_root = .{
            .module = .{},
            .root = .{ .checked = @enumFromInt(1) },
            .const_locator = null,
            .role = .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = failed_offset, .message_offset = message_offset } },
        },
    });
    const value_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = list_layout,
        .compile_time_root = .{
            .module = .{},
            .root = .{ .checked = @enumFromInt(1) },
            .const_locator = null,
            .role = .{ .value = .{ .failure_slot = failure_slot, .plan = list_plan } },
        },
    });
    const value = try program.store.addLocal(.{ .layout_idx = list_layout });
    const origin = LIR.StmtOrigin{ .loc = .none, .region = .zero(), .inline_scope = .none, .kind = .scaffold };
    const ret = try program.store.addCFStmt(.{ .ret = .{ .value = value } }, origin);
    const body = try program.store.addCFStmt(.{ .assign_literal = .{ .target = value, .value = .{ .static_data = value_slot }, .next = ret } }, origin);
    const accessor = try program.store.addProcSpec(.{
        .name = .fromRaw(1),
        .identity = LIR.ProcIdentity.forTest(1),
        .args = .empty(),
        .frame_locals = try program.store.addLocalSpan(&.{value}),
        .body = body,
        .ret_layout = list_layout,
    }, .none);
    program.static_data_values.items[@intFromEnum(value_slot)].accessor = accessor;
    try @import("comptime_value_guards.zig").insert(allocator, &program);
    var failure_record = [_]u8{0} ** 32;
    failure_record[failed_offset] = 1;
    const descriptor = [_]u8{0} ** 24;
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "failure", .value_id = failure_slot, .bytes = &failure_record, .alignment = 8 },
        .{ .symbol_name = "value", .value_id = value_slot, .bytes = &descriptor, .alignment = 8, .empty_list_capacities = &.{.{ .offset = 0, .capacity = 16 }} },
    };
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };
    try rebuild(allocator, &program, &frozen);
    try std.testing.expectEqual(body, program.store.getProcSpec(accessor).body.?);
    try std.testing.expectEqual(@as(u64, 0), program.store.getProcSpec(accessor).native_code_revision);
    try std.testing.expect(!program.comptime_value_guards.items[0].completed);
    try std.testing.expectEqual(failure_slot, program.store.getCFStmt(body).assign_literal.value.static_data);
    failure_record[failed_offset] = 0;
    try rebuild(allocator, &program, &frozen);
    const rebuilt = program.store.getProcSpec(accessor);
    try std.testing.expectEqual(@as(u64, 1), rebuilt.native_code_revision);
    try std.testing.expect(rebuilt.body.? != body);
    const capacity = program.store.getCFStmt(rebuilt.body.?).assign_literal;
    try std.testing.expectEqual(@as(i64, 16), capacity.value.i64_literal.value);
    try std.testing.expectEqual(LIR.LowLevel.list_with_capacity, program.store.getCFStmt(capacity.next).assign_low_level.op);
    try rebuild(allocator, &program, &frozen);
    try std.testing.expectEqual(rebuilt.body, program.store.getProcSpec(accessor).body);
    try std.testing.expectEqual(@as(u64, 1), program.store.getProcSpec(accessor).native_code_revision);
}
