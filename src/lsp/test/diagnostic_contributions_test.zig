//! Compiler-free active-root diagnostic publication regressions.
const std = @import("std");
const lsp = @import("lsp");
const Contributions = lsp.diagnostic_contributions.Contributions;
const D = lsp.diagnostics;

fn report(message: []const u8, line: u32) std.mem.Allocator.Error![]D.PublishDiagnostics {
    const a = std.testing.allocator;
    const sets = try a.alloc(D.PublishDiagnostics, 1);
    errdefer a.free(sets);
    const uri = try a.dupe(u8, "dependency");
    errdefer a.free(uri);
    const diagnostics = try a.alloc(D.Diagnostic, 1);
    errdefer a.free(diagnostics);
    diagnostics[0] = .{
        .range = .{ .start = .{ .line = line, .character = 0 }, .end = .{ .line = line, .character = 1 } },
        .message = try a.dupe(u8, message),
    };
    sets[0] = .{ .uri = uri, .diagnostics = diagnostics };
    return sets;
}

test "ROC-12252 closed dependency deduplicates complete reports and retains other root ownership" {
    const a = std.testing.allocator;
    var contributions: Contributions = .{};
    defer contributions.deinit(a);
    try contributions.replace(a, "first", try report("same", 1));
    try contributions.replace(a, "second", try report("same", 1));
    try contributions.replace(a, "third", try report("different", 0));
    var union_result = try contributions.collect(a, "dependency");
    defer union_result.deinit(a);
    try std.testing.expectEqual(@as(usize, 2), union_result.diagnostics.len);
    try std.testing.expectEqualStrings("different", union_result.diagnostics[0].message);
    try std.testing.expectEqualStrings("same", union_result.diagnostics[1].message);
    contributions.remove(a, "first");
    contributions.remove(a, "third");
    var remaining = try contributions.collect(a, "dependency");
    defer remaining.deinit(a);
    try std.testing.expectEqual(@as(usize, 1), remaining.diagnostics.len);
    try std.testing.expectEqualStrings("same", remaining.diagnostics[0].message);
}

test "URI references include empty roots and empty contributed sets until removal" {
    const a = std.testing.allocator;
    var contributions: Contributions = .{};
    defer contributions.deinit(a);
    try std.testing.expect(!contributions.referencesUri("dependency"));
    try contributions.replace(a, "dependency", try a.alloc(D.PublishDiagnostics, 0));
    try std.testing.expect(contributions.referencesUri("dependency"));
    contributions.remove(a, "dependency");
    try std.testing.expect(!contributions.referencesUri("dependency"));
    const sets = try a.alloc(D.PublishDiagnostics, 1);
    sets[0] = .{ .uri = try a.dupe(u8, "dependency"), .diagnostics = try a.alloc(D.Diagnostic, 0) };
    try contributions.replace(a, "importer", sets);
    try std.testing.expect(contributions.referencesUri("dependency"));
    try contributions.replace(a, "importer", try a.alloc(D.PublishDiagnostics, 0));
    try std.testing.expect(!contributions.referencesUri("dependency"));
    try std.testing.expect(contributions.referencesUri("importer"));
    contributions.remove(a, "importer");
    try std.testing.expect(!contributions.referencesUri("importer"));
}

test "ROC-12252 diagnostic union is independent of root completion order" {
    const a = std.testing.allocator;
    var forward: Contributions = .{};
    defer forward.deinit(a);
    var reverse: Contributions = .{};
    defer reverse.deinit(a);
    try forward.replace(a, "first", try report("z", 0));
    try forward.replace(a, "second", try report("a", 0));
    try reverse.replace(a, "second", try report("a", 0));
    try reverse.replace(a, "first", try report("z", 0));
    var first = try forward.collect(a, "dependency");
    defer first.deinit(a);
    var second = try reverse.collect(a, "dependency");
    defer second.deinit(a);
    try std.testing.expectEqual(@as(usize, 2), first.diagnostics.len);
    for (first.diagnostics, second.diagnostics) |left, right| {
        try std.testing.expectEqualStrings(left.message, right.message);
    }
}
