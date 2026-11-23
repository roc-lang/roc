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

    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss);
    defer build.deinit();
    try build.build(path);
    const coord = build.coordinator.?;
    try std.testing.expect(!coord.hasUserErrors());

    const artifact = artifact: {
        var packages = coord.packages.iterator();
        while (packages.next()) |package| {
            for (package.value_ptr.*.modules.items) |*module| {
                if (std.mem.eql(u8, module.name, "main")) break :artifact module.checkedArtifact().?;
            }
        }
        return error.TestUnexpectedResult;
    };

    const expected = [_][]const u8{ "a", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" };
    var list_constants: usize = 0;
    for (artifact.compile_time_roots.roots.items()) |root| {
        if (root.kind != .constant or root.payload != .const_node) continue;
        // The list of pieces is data alone, stored as its image: its
        // descriptor, then its items' string descriptors.
        const image = artifact.const_store.image(artifact.const_store.imageOf(root.payload.const_node) orelse continue);
        const word = 8;
        const len = std.mem.readInt(u64, image.bytes[word..][0..word], .little);
        try std.testing.expectEqual(@as(u64, expected.len), len);
        list_constants += 1;
        const items: u32 = @intCast(std.mem.readInt(u64, image.bytes[0..word], .little));
        for (expected, 0..) |expected_piece, index| {
            try std.testing.expectEqualStrings(expected_piece, image.strAt(items + @as(u32, @intCast(index * 3 * word))));
        }
    }
    try std.testing.expectEqual(@as(usize, 1), list_constants);
}
