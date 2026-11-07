//! Deterministic allocation-work guard for checked procedure publication.
const std = @import("std");
const Checked = @import("../checked_module.zig");
const type_ids = @import("../checked_type_ids.zig");
const TopLevelProcedureBinding = Checked.TopLevelProcedureBinding;
const TopLevelProcedureBindingTable = Checked.TopLevelProcedureBindingTable;

fn schemeAllocationFailures(gpa: std.mem.Allocator) (std.mem.Allocator.Error || error{TestExpectedEqual})!void {
    var store = Checked.CheckedTypeStore{};
    defer store.deinit(gpa);
    var inserted: usize = 0;
    defer for (0..inserted) |i| {
        const scheme = store.scheme(@fromBackingInt(@backingInt(type_ids.fromSynthesized(i))));
        std.debug.assert(@backingInt(scheme.id) == @backingInt(type_ids.fromSynthesized(i)));
        std.debug.assert(@backingInt(scheme.root) == i);
    };
    while (inserted < 128) : (inserted += 1) {
        const root: Checked.CheckedTypeId = @fromBackingInt(@intCast(inserted));
        const id = try store.internScheme(gpa, .synthetic, root);
        try std.testing.expectEqual(@backingInt(type_ids.fromSynthesized(inserted)), @backingInt(id));
        // Asking for a root's scheme again names the scheme it already has.
        try std.testing.expectEqual(id, try store.internScheme(gpa, .synthetic, root));
    }
    try std.testing.expectEqual(inserted, store.schemes.items().len);
}

test "issue 11128 scheme growth and allocation failure preserve existing schemes" {
    // Growth that extends a block in place makes no allocation, and whether
    // the backing allocator can extend one depends on its state, so the
    // sweep runs over a backing allocator that never extends: every growth
    // allocates, the same way on every run.
    try std.testing.checkAllAllocationFailures(never_extending, schemeAllocationFailures, .{});
}

/// `std.testing.allocator` with every in-place resize refused.
const never_extending: std.mem.Allocator = .{
    .ptr = undefined,
    .vtable = &.{
        .alloc = struct {
            fn alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
                return std.testing.allocator.rawAlloc(len, alignment, ret_addr);
            }
        }.alloc,
        .resize = std.mem.Allocator.noResize,
        .remap = std.mem.Allocator.noRemap,
        .free = struct {
            fn free(_: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
                std.testing.allocator.rawFree(memory, alignment, ret_addr);
            }
        }.free,
    },
};

test "issue 11128 top-level procedure binding appends stay amortized" {
    const binding_count: usize = 4096;
    const budget = 8 * binding_count * @sizeOf(TopLevelProcedureBinding);
    var direct_bytes = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    var callable_eval_bytes = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    {
        var table = TopLevelProcedureBindingTable.initEmpty();
        defer table.deinit(direct_bytes.allocator());
        for (0..binding_count) |i| {
            const ordinal: u32 = @intCast(i);
            const ref = try table.appendDirect(direct_bytes.allocator(), @fromBackingInt(@intCast(1)), .{ .proc_base = @fromBackingInt(@intCast(ordinal)) }, .{
                .proc_base = @fromBackingInt(@intCast(ordinal)),
                .template = @fromBackingInt(@intCast(ordinal)),
            });
            try std.testing.expectEqual(ordinal, @backingInt(ref));
        }
        for (0..binding_count) |i| {
            const binding = table.get(@fromBackingInt(@intCast(i)));
            try std.testing.expectEqual(i, @backingInt(binding.body.direct_template.proc_value.proc_base));
        }
    }
    {
        var table = TopLevelProcedureBindingTable.initEmpty();
        defer table.deinit(callable_eval_bytes.allocator());
        for (0..binding_count) |i| {
            _ = try table.appendCallableEval(callable_eval_bytes.allocator(), @fromBackingInt(@intCast(1)), @fromBackingInt(@intCast(i)));
        }
        for (0..binding_count) |i| {
            try std.testing.expectEqual(i, @backingInt(table.get(@fromBackingInt(@intCast(i))).body.callable_eval_template));
        }
    }
    if (direct_bytes.allocated_bytes > budget or callable_eval_bytes.allocated_bytes > budget) {
        std.debug.print("4096 bindings: direct={d}, callable_eval={d}, budget={d} bytes\n", .{
            direct_bytes.allocated_bytes, callable_eval_bytes.allocated_bytes, budget,
        });
    }
    try std.testing.expect(direct_bytes.allocated_bytes <= budget);
    try std.testing.expect(callable_eval_bytes.allocated_bytes <= budget);
}

test "issue 11128 wrapper and callable template appends stay amortized" {
    const count: usize = 4096;
    inline for (.{ Checked.EntryWrapperTable, Checked.CallableEvalTemplateTable }) |Table| {
        var bytes = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        const gpa = bytes.allocator();
        var table: Table = .{};
        defer table.deinit(gpa);
        for (0..count) |i| {
            const ref = if (Table == Checked.EntryWrapperTable)
                try table.append(gpa, @fromBackingInt(@intCast(i)), .{ .proc_base = @fromBackingInt(@intCast(i)), .template = @fromBackingInt(@intCast(i)) }, @fromBackingInt(@intCast(i)))
            else
                try table.append(gpa, @intCast(i), @fromBackingInt(@intCast(i)), @fromBackingInt(@intCast(i)), @fromBackingInt(@intCast(1)), @fromBackingInt(@intCast(i)));
            try std.testing.expectEqual(i, @backingInt(ref));
        }
        for (0..count) |i| {
            const row = table.get(@fromBackingInt(@intCast(i)));
            try std.testing.expectEqual(i, @backingInt(row.id));
            try std.testing.expectEqual(i, @backingInt(row.checked_fn_root));
        }
        const Row = @TypeOf(table.get(@fromBackingInt(@intCast(@as(u32, 1)))));
        try std.testing.expect(bytes.allocated_bytes <= 8 * count * @sizeOf(Row));
    }
}

fn bindingAllocationFailures(gpa: std.mem.Allocator) (std.mem.Allocator.Error || error{TestExpectedEqual})!void {
    var table = TopLevelProcedureBindingTable.initEmpty();
    defer table.deinit(gpa);
    var count: usize = 0;
    // This also runs when a growth allocation fails, before table destruction.
    defer for (0..count) |i| {
        const row = table.get(@fromBackingInt(@intCast(i)));
        std.debug.assert(@backingInt(row.body.callable_eval_template) == i);
    };
    while (count < 128) : (count += 1) {
        const ref = try table.appendCallableEval(gpa, @fromBackingInt(@intCast(1)), @fromBackingInt(@intCast(count)));
        try std.testing.expectEqual(count, @backingInt(ref));
    }
}

test "issue 11128 binding allocation failures preserve existing rows" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, bindingAllocationFailures, .{});
}

test "issue 11128 annotation reset work ignores unrelated CIR nodes" {
    const TestEnv = @import("TestEnv.zig");
    const annotation_count = 128;
    for ([_]usize{ 0, 2048 }) |unrelated_defs| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        for (0..annotation_count) |i| {
            try source.writer.print("f{d} : U64 -> U64\nf{d} = |x| x\n", .{ i, i });
        }
        for (0..unrelated_defs) |i| {
            try source.writer.print("filler{d} = {d}\n", .{ i, i });
        }
        var env = try TestEnv.init("Test", source.written());
        defer env.deinit();
        try env.assertNoErrors();
        // Read work performed by the actual checker, rather than timing it.
        // Each small signature touches only a few words regardless of how
        // many unrelated nodes occupy the same module's CIR store.
        const restored = env.checker.seen_annos.test_restored_words;
        try std.testing.expect(restored > 0);
        try std.testing.expect(restored <= 8 * annotation_count);
    }
}
