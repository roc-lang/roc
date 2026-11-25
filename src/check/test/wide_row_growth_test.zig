//! A projection or constructor pattern should not store the rest of a wide
//! type again. Measure solver entries and match-analysis storage, not time.

const std = @import("std");
const base = @import("base");
const ModuleEnv = @import("can").ModuleEnv;
const exhaustive = @import("../exhaustive.zig");
const TestEnv = @import("TestEnv.zig");

const Measure = enum { record_entries, tag_entries, coverage_bytes };

const MeasureError = TestEnv.TestEnvError || exhaustive.PatternResolveError;

fn measure(which: Measure, width: usize) MeasureError!usize {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    const w = &source.writer;
    switch (which) {
        .record_entries => {
            try w.writeAll("Wide : { ");
            for (0..width) |i| {
                if (i != 0) try w.writeAll(", ");
                try w.print("f{d} : I64", .{i});
            }
            try w.writeAll(" }\nfields : Wide -> List(I64)\nfields = |r| [");
            for (0..width) |i| {
                if (i != 0) try w.writeAll(", ");
                try w.print("r.f{d}", .{i});
            }
            try w.writeAll("]\n");
        },
        .tag_entries, .coverage_bytes => {
            try w.writeAll("Wide : [");
            for (0..width) |i| {
                if (i != 0) try w.writeAll(", ");
                try w.print("T{d}(I64)", .{i});
            }
            try w.writeAll("]\nvalue : Wide -> I64\nvalue = |t| match t {\n");
            for (0..width) |i| try w.print("    T{d}(v) => v\n", .{i});
            try w.writeAll("}\n");
        },
    }

    var env = try TestEnv.init("Test", source.written());
    defer env.deinit();
    try env.assertNoErrors();
    switch (which) {
        .record_entries => return env.checker.types.record_fields.len(),
        .tag_entries => return env.checker.types.tags.len(),
        .coverage_bytes => {
            // Replay just the read-only match check in a fresh arena, so a
            // solver improvement cannot hide a regression in coverage.
            const module = env.module_env;
            const lambda = module.store.getExpr(try env.defExpr("value")).e_lambda;
            const match = module.store.getExpr(lambda.body).e_match;
            var open_cache = exhaustive.NominalOpenCache.init(env.gpa);
            defer open_cache.deinit();
            var counter = std.testing.FailingAllocator.init(env.gpa, .{});
            var arena = base.SingleThreadArena.init(counter.allocator());
            defer arena.deinit();
            const result = try exhaustive.checkMatch(&arena, &module.types, module, &module.store, .{
                .idents = module.getIdentStoreConst(),
                .open_cache = &open_cache,
                .builtin_module = module.idents.builtin_module,
                .list = module.idents.list,
                .builtin_list = module.idents.builtin_list,
            }, match.branches, ModuleEnv.varFrom(match.cond), module.store.getExprRegion(lambda.body), null);
            try std.testing.expect(result.is_exhaustive);
            try std.testing.expectEqual(0, result.redundant_rows.len);
            try std.testing.expectEqual(0, result.unmatchable_rows.len);
            return counter.allocated_bytes;
        },
    }
}

fn expectLinearStorage(which: Measure) MeasureError!void {
    const small = try measure(which, 32);
    const large = try measure(which, 128);
    // Four times the input; allow for arena chunk sizing, but not 16x growth.
    if (large > 8 * small) {
        std.debug.print("{t}: 32 labels: {d}; 128 labels: {d}\n", .{ which, small, large });
    }
    try std.testing.expect(large <= 8 * small);
}

test "wide row growth: reading every field of a closed record (#13312)" {
    try expectLinearStorage(.record_entries);
}

test "wide row growth: matching every tag of a closed union (#13311)" {
    try expectLinearStorage(.tag_entries);
}

test "wide row growth: coverage of one arm per constructor (#13311)" {
    try expectLinearStorage(.coverage_bytes);
}
