const std = @import("std");
const SyntaxChecker = @import("../syntax.zig").SyntaxChecker;
const uri_util = @import("../uri.zig");

fn platformPath(allocator: std.mem.Allocator) ![]u8 {
    const this_dir = std.fs.path.dirname(@src().file) orelse ".";
    const abs_dir = try std.fs.path.resolve(allocator, &.{this_dir});
    defer allocator.free(abs_dir);
    return std.fs.path.resolve(allocator, &.{ abs_dir, "..", "..", "..", "test", "str", "platform", "main.roc" });
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
