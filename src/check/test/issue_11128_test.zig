//! Deterministic allocation-work guard for checked procedure publication.
const std = @import("std");
const CheckedArtifact = @import("../checked_artifact.zig");
const TopLevelProcedureBinding = CheckedArtifact.TopLevelProcedureBinding;
const TopLevelProcedureBindingTable = CheckedArtifact.TopLevelProcedureBindingTable;

fn schemeAllocationFailures(gpa: std.mem.Allocator) (std.mem.Allocator.Error || error{TestExpectedEqual})!void {
    var store = CheckedArtifact.CheckedTypeStore{};
    defer store.deinit(gpa);
    var inserted: usize = 0;
    defer for (0..inserted) |i| {
        var key = @import("../canonical_names.zig").CanonicalTypeSchemeKey{};
        key.bytes[0] = @intCast(i);
        const scheme = store.schemeForKey(key).?;
        std.debug.assert(@intFromEnum(scheme.id) == i);
        std.debug.assert(@intFromEnum(scheme.root) == i);
    };
    while (inserted < 128) : (inserted += 1) {
        var key = @import("../canonical_names.zig").CanonicalTypeSchemeKey{};
        key.bytes[0] = @intCast(inserted);
        const id = try store.internScheme(gpa, key, @enumFromInt(inserted));
        try std.testing.expectEqual(inserted, @intFromEnum(id));
        // Re-interning keeps the first representative, including when another
        // source root has an equal scheme key.
        try std.testing.expectEqual(id, try store.internScheme(gpa, key, @enumFromInt(inserted + 128)));
    }
    try std.testing.expectEqual(inserted, store.schemes.items.len);
}

test "issue 11128 scheme index growth and allocation failure preserve representatives" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, schemeAllocationFailures, .{});
}

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
            const ref = try table.appendDirect(direct_bytes.allocator(), .{}, .{ .proc_base = @enumFromInt(ordinal) }, .{
                .proc_base = @enumFromInt(ordinal),
                .template = @enumFromInt(ordinal),
            });
            try std.testing.expectEqual(ordinal, @intFromEnum(ref));
        }
        for (0..binding_count) |i| {
            const binding = table.get(@enumFromInt(i));
            try std.testing.expectEqual(i, @intFromEnum(binding.body.direct_template.proc_value.proc_base));
        }
    }
    {
        var table = TopLevelProcedureBindingTable.initEmpty();
        defer table.deinit(callable_eval_bytes.allocator());
        for (0..binding_count) |i| {
            _ = try table.appendCallableEval(callable_eval_bytes.allocator(), .{}, @enumFromInt(i));
        }
        for (0..binding_count) |i| {
            try std.testing.expectEqual(i, @intFromEnum(table.get(@enumFromInt(i)).body.callable_eval_template));
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
    inline for (.{ CheckedArtifact.IntrinsicWrapperTable, CheckedArtifact.EntryWrapperTable, CheckedArtifact.CallableEvalTemplateTable }) |Table| {
        var bytes = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        const gpa = bytes.allocator();
        var table: Table = .{};
        defer table.deinit(gpa);
        for (0..count) |i| {
            const ref = if (Table == CheckedArtifact.IntrinsicWrapperTable)
                try table.append(gpa, .{ .proc_base = @enumFromInt(i), .template = @enumFromInt(i) }, @enumFromInt(i), .structural_eq)
            else if (Table == CheckedArtifact.EntryWrapperTable)
                try table.append(gpa, @enumFromInt(i), .{ .proc_base = @enumFromInt(i), .template = @enumFromInt(i) }, @enumFromInt(i), @enumFromInt(i))
            else
                try table.append(gpa, @intCast(i), @enumFromInt(i), @enumFromInt(i), .{}, @enumFromInt(i));
            try std.testing.expectEqual(i, @intFromEnum(ref));
        }
        for (0..count) |i| {
            const row = table.get(@enumFromInt(i));
            try std.testing.expectEqual(i, @intFromEnum(row.id));
            try std.testing.expectEqual(i, @intFromEnum(row.checked_fn_root));
        }
        const Row = @TypeOf(table.get(@enumFromInt(@as(u32, 1))));
        try std.testing.expect(bytes.allocated_bytes <= 8 * count * @sizeOf(Row));
    }
}

fn bindingAllocationFailures(gpa: std.mem.Allocator) (std.mem.Allocator.Error || error{TestExpectedEqual})!void {
    var table = TopLevelProcedureBindingTable.initEmpty();
    defer table.deinit(gpa);
    var count: usize = 0;
    // This also runs when a growth allocation fails, before table destruction.
    defer for (0..count) |i| {
        const row = table.get(@enumFromInt(i));
        std.debug.assert(@intFromEnum(row.body.callable_eval_template) == i);
    };
    while (count < 128) : (count += 1) {
        const ref = try table.appendCallableEval(gpa, .{}, @enumFromInt(count));
        try std.testing.expectEqual(count, @intFromEnum(ref));
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
