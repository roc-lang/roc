//! A compile-time value must reach the runtime program the same way on every
//! path that produces one: the program a build continues from compile-time
//! evaluation (its runtime consumer reads the completed frozen image) and the
//! program lowered from checked modules whose values the constant store
//! restores. Each test drives the first path through `ProgramSession`, as
//! `roc build` does, and the second through `lowerCheckedModulesToLir`, so a
//! divergence between them fails here instead of in a binary's size.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const platform_files = [_]struct { path: []const u8, source: []const u8 }{
    .{ .path = ".roc_echo_platform/main.roc", .source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(other) => {
    \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
    \\            1
    \\        }
    \\    }
    },
    .{ .path = ".roc_echo_platform/Echo.roc", .source =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
    },
};

/// The runtime target of an optimized build: expects omitted, so the runtime
/// program is a separate consumer of the evaluation's specialization.
const optimized_target: lir.CheckedPipeline.TargetConfig = .{
    .inline_expects = .omit,
};

/// Both runtime programs of one app.
const Lowered = struct {
    coord: Coordinator,
    continued: lir.CheckedPipeline.LoweredProgram,
    restored: lir.CheckedPipeline.LoweredProgram,

    fn deinit(self: *Lowered) void {
        self.restored.deinit();
        self.continued.deinit();
        self.coord.deinit();
    }
};

fn lowerBothPaths(gpa: std.mem.Allocator, arena: std.mem.Allocator, tmp_dir: std.testing.TmpDir, source: []const u8) !Lowered {
    const io = std.testing.io;
    try tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
    for (platform_files) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = source });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", arena);

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(gpa, gpa, io),
    );
    errdefer coord.deinit();
    coord.enable_hosted_transform = true;
    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (coord.hasUserErrors()) return error.TestUnexpectedResult;

    coord.runtime_lowering = .{ .target = optimized_target };
    try coord.finishCheckedProgram(.executable_artifacts);
    if (coord.hasUserErrors()) return error.TestUnexpectedResult;
    const session = &coord.program_session.?;
    // The build continues the evaluation's specialization in a separate
    // runtime consumer, which is the path that reads the frozen image.
    try std.testing.expect(session.host != null);
    try std.testing.expect(session.runtime_prepared != null);
    var continued = try session.takeRuntime(gpa, session.runtime_roots, optimized_target);
    errdefer continued.deinit();

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);
    const restored = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
            .imports = imports,
        },
        .{
            .requests = lir_roots,
            .include_provided_data_exports = true,
            .include_internal_static_data = true,
        },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    return .{ .coord = coord, .continued = continued, .restored = restored };
}

fn countListSlots(result: *const lir.Program.Result) usize {
    var count: usize = 0;
    for (result.static_data_values.items) |slot| {
        if (result.layouts.getLayout(slot.layout_idx).tag == .list) count += 1;
    }
    return count;
}

fn countLowLevel(result: *const lir.Program.Result, op: lir.LIR.LowLevel) usize {
    var count: usize = 0;
    for (result.store.getCFStmts()) |stmt| {
        if (stmt == .assign_low_level and stmt.assign_low_level.op == op) count += 1;
    }
    return count;
}

fn hasIntLiteral(result: *const lir.Program.Result, value: i128) bool {
    for (result.store.getCFStmts()) |stmt| {
        if (stmt != .assign_literal) continue;
        switch (stmt.assign_literal.value) {
            .i64_literal => |literal| if (literal.value == value) return true,
            .i128_literal => |literal| if (literal.value == value) return true,
            .f64_literal, .f32_literal, .dec_literal, .str_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .static_data, .bytes_literal, .null_ptr, .proc_ref => {},
        }
    }
    return false;
}

test "two equal compile-time tables freeze to one backing in the continued runtime program" {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    var arena = base.SingleThreadArena.init(gpa);
    defer arena.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    var lowered = try lowerBothPaths(gpa, arena.allocator(), tmp_dir,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\first : List(U32)
        \\first = List.repeat(0.U32, 1000)
        \\second : List(U32)
        \\second = List.repeat(0.U32, 1000)
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(List.get(first, List.len(args))))
        \\    Echo.line!(Str.inspect(List.get(second, List.len(args))))
        \\    Ok({})
        \\}
    );
    defer lowered.deinit();

    const continued = &lowered.continued;
    const frozen = continued.frozen_static_data orelse return error.TestUnexpectedResult;
    // Each table is its own root with its own descriptor, and the two
    // descriptors name one 4000-byte backing.
    var backings: usize = 0;
    var descriptor_targets: [2]?u32 = .{ null, null };
    var descriptors: usize = 0;
    for (frozen.exports) |item| {
        if (item.value_id) |slot| {
            const layout_idx = continued.lir_result.static_data_values.items[@intFromEnum(slot)].layout_idx;
            if (continued.lir_result.layouts.getLayout(layout_idx).tag != .list) continue;
            try std.testing.expectEqual(@as(usize, 1), item.relocations.len);
            if (descriptors < descriptor_targets.len) descriptor_targets[descriptors] = @intFromEnum(item.relocations[0].target.data_symbol);
            descriptors += 1;
        } else if (item.bytes.len >= 4000) {
            backings += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), descriptors);
    try std.testing.expectEqual(@as(usize, 1), backings);
    try std.testing.expectEqual(descriptor_targets[0], descriptor_targets[1]);
    // Neither path rebuilds a table of copies at its reads.
    try std.testing.expectEqual(@as(usize, 0), countLowLevel(&continued.lir_result, .list_append_unsafe));
    try std.testing.expectEqual(@as(usize, 0), countLowLevel(&lowered.restored.lir_result, .list_append_unsafe));
    try std.testing.expect(countListSlots(&lowered.restored.lir_result) > 0);
}

test "an empty compile-time list read at another layout index still lowers as its capacity request" {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    var arena = base.SingleThreadArena.init(gpa);
    defer arena.deinit();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    // The roots only the expects read make the evaluation's program intern
    // three list layouts before the buffer's, while the runtime program, which
    // lowers only what `main!` reaches, interns the buffer's list layout first:
    // the two consumers name the same list layout by different indices.
    var lowered = try lowerBothPaths(gpa, arena.allocator(), tmp_dir,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\names : List(Str)
        \\names = ["a", "b"]
        \\shorts : List(U16)
        \\shorts = [1, 2, 3]
        \\bytes : List(U8)
        \\bytes = [4, 5, 6, 7]
        \\buffer : List(U64)
        \\buffer = List.with_capacity(64)
        \\expect List.len(names) == 2
        \\expect List.len(shorts) == 3
        \\expect List.len(bytes) == 4
        \\main! = |args| {
        \\    # The appended value comes from the arguments, so the read of
        \\    # `buffer` stays in the runtime program rather than folding.
        \\    Echo.line!(Str.inspect(List.append(buffer, List.len(args))))
        \\    Ok({})
        \\}
    );
    defer lowered.deinit();

    // The evaluation's program names the buffer's list layout by a different
    // index than the runtime program does, so a construction matched by
    // layout index would miss here.
    const host = &lowered.coord.program_session.?.host.?.lir_result;
    var host_buffer_layout: ?layout.Idx = null;
    for (host.static_data_values.items) |slot| {
        if (slot.compile_time_root == null) continue;
        const slot_layout = host.layouts.getLayout(slot.layout_idx);
        if (slot_layout.tag == .list and slot_layout.getIdx() == .u64) host_buffer_layout = slot.layout_idx;
    }
    var runtime_buffer_layout: ?layout.Idx = null;
    for (lowered.continued.lir_result.store.getCFStmts()) |stmt| {
        if (stmt == .assign_low_level and stmt.assign_low_level.op == .list_with_capacity) {
            runtime_buffer_layout = lowered.continued.lir_result.store.getLocal(stmt.assign_low_level.target).layout_idx;
        }
    }
    try std.testing.expect(host_buffer_layout != null and runtime_buffer_layout != null);
    try std.testing.expect(host_buffer_layout.? != runtime_buffer_layout.?);

    for ([_]*const lir.Program.Result{ &lowered.continued.lir_result, &lowered.restored.lir_result }) |result| {
        // The buffer is the `with_capacity` it was evaluated with, and no
        // list reaches static data: the buffer is a construction and the
        // other lists are read only by the expects the runtime omits.
        try std.testing.expect(countLowLevel(result, .list_with_capacity) >= 1);
        try std.testing.expect(hasIntLiteral(result, 64));
        try std.testing.expectEqual(@as(usize, 0), countListSlots(result));
    }
}
