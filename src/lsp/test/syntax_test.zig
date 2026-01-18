//! Tests for the LSP syntax checker integration.

const std = @import("std");
const SyntaxChecker = @import("../syntax.zig").SyntaxChecker;
const DependencyGraph = @import("../dependency_graph.zig").DependencyGraph;
const uri_util = @import("../uri.zig");

fn platformPath(allocator: std.mem.Allocator) ![]u8 {
    const this_dir = std.fs.path.dirname(@src().file) orelse ".";
    const abs_dir = try std.fs.path.resolve(allocator, &.{this_dir});
    defer allocator.free(abs_dir);
    return std.fs.path.resolve(allocator, &.{ abs_dir, "..", "..", "..", "test", "str", "platform", "main.roc" });
}

test "syntax checker skips rebuild when content unchanged" {
    // Test the incremental invalidation: same content should skip rebuild
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents);

    try tmp.dir.writeFile(.{ .sub_path = "test.roc", .data = contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // First check with override text - should do full build
    const result1 = try checker.check(uri, contents, null);
    defer {
        for (result1) |*set| set.deinit(allocator);
        allocator.free(result1);
    }

    // Verify hash is stored after first build
    const hash_after_first = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_first != null);

    // Second check with same content - should skip rebuild (returns empty)
    const result2 = try checker.check(uri, contents, null);
    defer {
        for (result2) |*set| set.deinit(allocator);
        allocator.free(result2);
    }

    // When content is unchanged, check returns empty (skip rebuild)
    try std.testing.expectEqual(@as(usize, 0), result2.len);

    // Hash should still be the same
    const hash_after_second = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_second != null);
    try std.testing.expectEqualSlices(u8, &hash_after_first.?, &hash_after_second.?);
}

test "syntax checker rebuilds when content changes" {
    // Test that changed content triggers rebuild
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents1 = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents1);

    const contents2 = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "world"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents2);

    try tmp.dir.writeFile(.{ .sub_path = "test2.roc", .data = contents1 });
    const file_path = try tmp.dir.realpathAlloc(allocator, "test2.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // First check
    const result1 = try checker.check(uri, contents1, null);
    defer {
        for (result1) |*set| set.deinit(allocator);
        allocator.free(result1);
    }

    const hash_after_first = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_first != null);

    // Second check with different content - should trigger rebuild
    const result2 = try checker.check(uri, contents2, null);
    defer {
        for (result2) |*set| set.deinit(allocator);
        allocator.free(result2);
    }

    // Hash should be different
    const hash_after_second = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_second != null);
    try std.testing.expect(!std.mem.eql(u8, &hash_after_first.?, &hash_after_second.?));
}

test "syntax checker reports diagnostics for invalid source" {
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main =
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents);

    try tmp.dir.writeFile(.{ .sub_path = "bad.roc", .data = contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "bad.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    const publish_sets = try checker.check(uri, null, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    try std.testing.expect(publish_sets.len > 0);
    var total_diags: usize = 0;
    for (publish_sets) |set| {
        total_diags += set.diagnostics.len;
    }
    try std.testing.expect(total_diags > 0);
}
