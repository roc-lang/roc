//! Regression test for issue #12722: a top-level constant holding the
//! `Str.split_on` pieces of a heap Str built at compile time is stored in the
//! checked module's ConstStore as a list of those pieces.
//! repro for https://github.com/roc-lang/roc/issues/12722

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

test "issue 12722: top-level constant stores split_on pieces of a compile-time heap Str" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\lines = Str.join_with(["a", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"], ",").split_on(",")
        \\
        \\main! = |_args| {
        \\    echo!(Str.inspect(lines))
        \\    Ok({})
        \\}
        \\
        ,
    });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(path);

    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const coord = build.coordinator.?;
    try std.testing.expect(!coord.hasUserErrors());

    const checked = checked: {
        var packages = coord.packages.iterator();
        while (packages.next()) |package| {
            for (package.value_ptr.*.modules.items) |*module| {
                if (std.mem.eql(u8, module.name, "main")) break :checked module.checkedModule().?;
            }
        }
        return error.TestUnexpectedResult;
    };

    const expected = [_][]const u8{ "a", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" };
    var list_constants: usize = 0;
    for ((try checked.checkedTables()).compile_time_roots.roots) |root| {
        if (root.kind != .constant) continue;
        const payload = checked.evaluation.rootPayload(root.id);
        if (payload != .const_node) continue;
        const value = checked.evaluation.const_store.get(payload.const_node);
        if (value != .list) continue;
        list_constants += 1;
        try std.testing.expect(value.list == .nodes);
        const nodes = value.list.nodes;
        try std.testing.expectEqual(expected.len, nodes.len);
        for (expected, nodes) |expected_piece, node| {
            const piece = checked.evaluation.const_store.get(node);
            try std.testing.expect(piece == .str);
            try std.testing.expectEqualStrings(expected_piece, checked.evaluation.const_store.blobData(piece.str));
        }
    }
    try std.testing.expectEqual(@as(usize, 1), list_constants);
}
