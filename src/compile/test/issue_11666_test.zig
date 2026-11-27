//! Regression coverage for compile-time lists of custom string literals.

const std = @import("std");
const base = @import("base");
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

/// Counts the reachable statements of each compile-time program as it is
/// lowered, excluding abandoned rewrite nodes: each `.boxy` program and each
/// stage of a `.lss` evaluation's Solved program. This measures the
/// actual compile-time program without depending on CPU scheduling, target
/// instruction selection, or the compiler's optimization level.
const ProgramMeasure = struct {
    measurement: Measurement = .{},

    fn observe(raw: *anyopaque, result: *const lir.Program.Result) std.mem.Allocator.Error!void {
        const self: *ProgramMeasure = @ptrCast(@alignCast(raw));
        const store = &result.store;
        for (store.getProcSpecs()) |proc| {
            var walk = try lir.BodyClone.ReachableStmts.init(store, proc.body.get() orelse continue);
            defer walk.deinit();
            while (try walk.next()) |stmt| {
                self.measurement.statements += 1;
                switch (store.getCFStmtImmediate(stmt).*) {
                    .decref, .decref_if_initialized, .free => self.measurement.releases += 1,
                    .init_uninitialized,
                    .assign_ref,
                    .assign_literal,
                    .assign_call,
                    .assign_call_erased,
                    .assign_packed_erased_fn,
                    .assign_low_level,
                    .assign_list,
                    .assign_struct,
                    .assign_tag,
                    .store_struct,
                    .store_tag,
                    .set_local,
                    .debug,
                    .expect,
                    .expect_err,
                    .runtime_error,
                    .comptime_exhaustiveness_failed,
                    .incref,
                    .switch_stmt,
                    .switch_initialized_payload,
                    .str_match,
                    .str_match_set,
                    .join,
                    .jump,
                    .ret,
                    .crash,
                    .dyn,
                    => {},
                }
            }
        }
    }
};

fn measureLiteralList(literal_count: usize, strategy: base.SpecializationStrategy) TestError!Measurement {
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

    var build = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, strategy);
    defer build.deinit();
    var measure: ProgramMeasure = .{};
    build.compile_time_program_observer = .{ .context = &measure, .observe = ProgramMeasure.observe };
    try build.build(path);
    const coord = build.coordinator.?;
    try std.testing.expect(!coord.hasUserErrors());
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
    for (artifact.compile_time_roots.roots.items()) |root| {
        if (root.kind != .constant) continue;
        constants += 1;
        try std.testing.expect(root.payload == .const_node);
        // The list is stored as its image: its descriptor, then its items,
        // each a nominal over a list of bytes, whose bytes follow.
        const image = artifact.const_store.image(artifact.const_store.imageOf(root.payload.const_node) orelse return error.TestUnexpectedResult);
        const word = 8;
        try std.testing.expectEqual(@as(u64, literal_count), std.mem.readInt(u64, image.bytes[word..][0..word], .little));
        const items: usize = @intCast(std.mem.readInt(u64, image.bytes[0..word], .little));
        for (0..literal_count) |index| {
            const item = image.bytes[items + index * 3 * word ..][0 .. 3 * word];
            const len: usize = @intCast(std.mem.readInt(u64, item[word..][0..word], .little));
            const start: usize = @intCast(std.mem.readInt(u64, item[0..word], .little));
            const expected = try std.fmt.allocPrint(gpa, "w{d}", .{index});
            defer gpa.free(expected);
            try std.testing.expectEqualStrings(expected, image.bytes[start..][0..len]);
        }
    }
    try std.testing.expectEqual(@as(usize, 1), constants);

    // The constant is evaluated by the strategy's own evaluator (design.md
    // "Compile-Time Evaluation Strategy").
    try std.testing.expect(measure.measurement.statements != 0);
    return measure.measurement;
}

test "issue 11666: compile-time from_quote list LIR grows linearly" {
    // https://github.com/roc-lang/roc/issues/11666
    // Doubling a list of independent conversions should double the work,
    // with room for fixed scaffolding. Every stored byte is checked above.
    for ([_]base.SpecializationStrategy{ .boxy, .lss }) |strategy| {
        const small = try measureLiteralList(64, strategy);
        const large = try measureLiteralList(128, strategy);
        const linear = large.statements <= small.statements * 5 / 2;
        if (!linear) std.debug.print(
            "{t} from_quote list CTFE LIR grew nonlinearly: 64/128 literals produced " ++
                "{d}/{d} reachable statements and {d}/{d} releases\n",
            .{ strategy, small.statements, large.statements, small.releases, large.releases },
        );
        try std.testing.expect(linear);
    }
}
