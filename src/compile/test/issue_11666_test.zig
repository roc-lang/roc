//! Regression coverage for compile-time lists of custom string literals.

const std = @import("std");
const lir = @import("lir");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const TestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

const Measurement = struct {
    statements: usize = 0,
    releases: usize = 0,
};

fn measureLiteralList(literal_count: usize) TestError!Measurement {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{
        .sub_path = "T.roc",
        .data =
        \\T :: List(U8).{
        \\    from_quote : Str -> Try(T, [BadQuotedBytes(Str)])
        \\    from_quote = |s| Ok(T.(Str.to_utf8(s)))
        \\}
        ,
    });
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "module [words]\n\nimport T\n\nwords : List(T)\nwords = [\n");
    for (0..literal_count) |index| try source.print(gpa, "    \"w{d}\",\n", .{index});
    try source.appendSlice(gpa, "]\n");
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source.items });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);

    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const coord = build.coordinator.?;
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    const artifact = artifact: {
        var packages = coord.packages.iterator();
        while (packages.next()) |package| {
            for (package.value_ptr.*.modules.items) |*module| {
                if (std.mem.eql(u8, module.name, "Repro")) break :artifact module.checkedArtifact().?;
            }
        }
        return error.TestUnexpectedResult;
    };
    var constants: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .constant) continue;
        constants += 1;
        try std.testing.expect(root.payload == .const_node);
        const value = artifact.const_store.get(root.payload.const_node);
        try std.testing.expect(value == .list and value.list == .nodes);
        try std.testing.expectEqual(literal_count, value.list.nodes.len);
        for (value.list.nodes, 0..) |node, index| {
            const word = artifact.const_store.get(node);
            try std.testing.expect(word == .nominal);
            const bytes = artifact.const_store.get(word.nominal.backing);
            try std.testing.expect(bytes == .list and bytes.list == .packed_bytes);
            const expected = try std.fmt.allocPrint(gpa, "w{d}", .{index});
            defer gpa.free(expected);
            try std.testing.expectEqualStrings(expected, artifact.const_store.blobBytes(bytes.list.packed_bytes.bytes));
        }
    }
    try std.testing.expectEqual(@as(usize, 1), constants);

    // Count reachable emitted LIR, excluding abandoned rewrite nodes. This
    // measures the actual CTFE program without depending on CPU scheduling,
    // target instruction selection, or the compiler's optimization level.
    const store = &session.host.?.lir_result.store;
    var result: Measurement = .{};
    for (store.getProcSpecs()) |proc| {
        var walk = try lir.BodyClone.ReachableStmts.init(store, proc.body orelse continue);
        defer walk.deinit();
        while (try walk.next()) |stmt| {
            result.statements += 1;
            switch (store.getCFStmt(stmt)) {
                .decref, .decref_if_initialized, .free => result.releases += 1,
                else => {},
            }
        }
    }
    return result;
}

test "issue 11666: compile-time from_quote list LIR grows linearly" {
    // https://github.com/roc-lang/roc/issues/11666
    // Doubling a list of independent conversions should double the work,
    // with room for fixed scaffolding. Every stored byte is checked above.
    const small = try measureLiteralList(64);
    const large = try measureLiteralList(128);
    const linear = large.statements <= small.statements * 5 / 2;
    if (!linear) std.debug.print(
        "from_quote list CTFE LIR grew nonlinearly: 64/128 literals produced " ++
            "{d}/{d} reachable statements and {d}/{d} releases\n",
        .{ small.statements, large.statements, small.releases, large.releases },
    );
    try std.testing.expect(linear);
}
