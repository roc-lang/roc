//! Tests for publishing canonicalization diagnostics from scratch storage.

const std = @import("std");
const base = @import("base");
const ModuleEnv = @import("../ModuleEnv.zig");

const testing = std.testing;

fn pushInvalidNumLiteral(env: *ModuleEnv) !void {
    try env.pushDiagnostic(.{ .invalid_num_literal = .{ .region = base.Region.zero() } });
}

fn pushEmptyTuple(env: *ModuleEnv) !void {
    try env.pushDiagnostic(.{ .empty_tuple = .{ .region = base.Region.zero() } });
}

fn pushChainedRange(env: *ModuleEnv) !void {
    try env.pushDiagnostic(.{ .range_op_chained = .{ .region = base.Region.zero() } });
}

test "diagnostic publication extends a tail span without copying" {
    const allocator = testing.allocator;
    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.initCIRFields("Test");

    try pushInvalidNumLiteral(&env);
    try env.publishScratchDiagnostics();
    const first_span = env.diagnostics.span;
    const first_index_len = env.store.index_data.len();

    try pushEmptyTuple(&env);
    try env.publishScratchDiagnostics();

    try testing.expectEqual(first_span.start, env.diagnostics.span.start);
    try testing.expectEqual(first_index_len + 1, env.store.index_data.len());

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 2), diagnostics.len);
    try testing.expectEqual(.invalid_num_literal, std.meta.activeTag(diagnostics[0]));
    try testing.expectEqual(.empty_tuple, std.meta.activeTag(diagnostics[1]));
}

test "diagnostic publication reserves before copying a non-tail span" {
    const allocator = testing.allocator;
    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.initCIRFields("Test");

    try pushInvalidNumLiteral(&env);
    try pushEmptyTuple(&env);
    try env.publishScratchDiagnostics();

    // Move the published span away from the tail, then fill index_data to
    // capacity so publishing another diagnostic must grow the allocation.
    while (env.store.index_data.len() < env.store.index_data.items.capacity) {
        _ = try env.store.index_data.append(allocator, 0);
    }
    try pushChainedRange(&env);
    try env.publishScratchDiagnostics();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 3), diagnostics.len);
    try testing.expectEqual(.invalid_num_literal, std.meta.activeTag(diagnostics[0]));
    try testing.expectEqual(.empty_tuple, std.meta.activeTag(diagnostics[1]));
    try testing.expectEqual(.range_op_chained, std.meta.activeTag(diagnostics[2]));
}
