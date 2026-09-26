//! Consumer manifests: what each consumer of one producer program lowers,
//! which evaluated values it materializes, and what it keeps of the
//! producer's root metadata.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CheckedArtifact = @import("check").CheckedArtifact;
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const echo_platform = [_]struct { path: []const u8, source: []const u8 }{
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

fn writeEchoPlatform(dir: std.Io.Dir, io: std.Io) (std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError)!void {
    try dir.createDirPath(io, ".roc_echo_platform");
    for (echo_platform) |file| try dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
}

fn materializedRootCount(program: *const lir.CheckedPipeline.LoweredProgram) usize {
    var count: usize = 0;
    for (program.lir_result.const_roots.items) |root| {
        if (root.value_slot != null) count += 1;
    }
    return count;
}

/// How many of one module's evaluated roots the program materializes a
/// completed value for.
fn materializedAmong(
    program: *const lir.CheckedPipeline.LoweredProgram,
    module: CheckedArtifact.ModuleId,
    requests: []const CheckedArtifact.RootRequest,
) usize {
    var count: usize = 0;
    for (requests) |request| {
        if (materializesRoot(program, module, rootIdentity(request))) count += 1;
    }
    return count;
}

fn moduleArtifactNamed(coord: *Coordinator, name: []const u8) ?*CheckedArtifact.CheckedModuleArtifact {
    var packages = coord.packages.iterator();
    while (packages.next()) |package| {
        for (package.value_ptr.*.modules.items) |*mod| {
            if (std.mem.eql(u8, mod.name, name)) return mod.checkedArtifact();
        }
    }
    return null;
}

/// The slots holding one evaluated root's completed value, by the root's own
/// checked identity rather than by any store-local id.
fn valueSlotsForRoot(
    program: *const lir.CheckedPipeline.LoweredProgram,
    module: CheckedArtifact.ModuleId,
    root: CheckedArtifact.ComptimeRootId,
) usize {
    var count: usize = 0;
    for (program.lir_result.static_data_values.items) |value| {
        const owner = value.compile_time_root orelse continue;
        if (owner.role != .value) continue;
        if (!std.meta.eql(owner.module, module) or !owner.root.eql(.{ .checked = root })) continue;
        count += 1;
    }
    return count;
}

/// The one slot holding an evaluated root's completed value, if it has one.
fn valueSlotForRoot(
    program: *const lir.CheckedPipeline.LoweredProgram,
    module: CheckedArtifact.ModuleId,
    root: CheckedArtifact.ComptimeRootId,
) ?lir.LIR.StaticDataId {
    for (program.lir_result.static_data_values.items, 0..) |value, index| {
        const owner = value.compile_time_root orelse continue;
        if (owner.role != .value) continue;
        if (!std.meta.eql(owner.module, module) or !owner.root.eql(.{ .checked = root })) continue;
        return @enumFromInt(index);
    }
    return null;
}

/// Whether the root's own const-root entry declares the slot its completed
/// value is published into.
fn materializesRoot(
    program: *const lir.CheckedPipeline.LoweredProgram,
    module: CheckedArtifact.ModuleId,
    root: CheckedArtifact.ComptimeRootId,
) bool {
    const slot = valueSlotForRoot(program, module, root) orelse return false;
    for (program.lir_result.const_roots.items) |const_root| {
        if (const_root.value_slot == slot) return true;
    }
    return false;
}

fn countNamedProcs(store: *const lir.LirStore, expected: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.eql(u8, name, expected)) count += 1;
    }
    return count;
}

/// Procedures whose debug name contains `fragment`, so a qualified name
/// cannot hide one.
fn countProcsNaming(store: *const lir.LirStore, fragment: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.find(u8, name, fragment) != null) count += 1;
    }
    return count;
}

fn rootIdentity(request: CheckedArtifact.RootRequest) CheckedArtifact.ComptimeRootId {
    return request.compile_time_root orelse unreachable;
}

/// The request for the root declared at `order` in its module.
fn rootWithOrder(requests: []const CheckedArtifact.RootRequest, order: u32) error{TestUnexpectedResult}!CheckedArtifact.RootRequest {
    for (requests) |request| {
        if (request.order == order) return request;
    }
    return error.TestUnexpectedResult;
}

test "compile-time consumer materializes only the evaluated values the program reads" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeEchoPlatform(tmp_dir.dir, io);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Helper.roc", .data =
        \\Helper := [].{
        \\    used_table : List(U64)
        \\    used_table = {
        \\        dbg "evaluated used table"
        \\        List.repeat(7, 4)
        \\    }
        \\    unused_table : List(U64)
        \\    unused_table = {
        \\        dbg "evaluated unused table"
        \\        List.repeat(9, 4)
        \\    }
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Helper
        \\main! = |_args| {
        \\    Echo.line!(Str.inspect(List.len(Helper.used_table)))
        \\    Ok({})
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    const Capture = struct {
        bytes: [4096]u8 = undefined,
        len: usize = 0,
        fn write(raw: ?*anyopaque, _: std.Io, bytes: []const u8) CoreCtx.StdioError!void {
            const self: *@This() = @ptrCast(@alignCast(raw.?));
            if (self.len + bytes.len > self.bytes.len) return error.IoError;
            @memcpy(self.bytes[self.len..][0..bytes.len], bytes);
            self.len += bytes.len;
        }
    };
    var capture = Capture{};
    var ctx = CoreCtx.os(allocator, allocator, io);
    ctx.ctx = &capture;
    ctx.vtable.writeStderr = Capture.write;

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        ctx,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    // A separate runtime consumer continuing the compile-time Solved program
    // (dev's Solved policy), so completed values are materialized for it
    // rather than read out of the compile-time consumer's own slots.
    const target: lir.CheckedPipeline.TargetConfig = .{ .inline_expects = .omit, .proc_debug_names = true, .inline_mode = .wrappers, .spec_constr_clone_inlining = .iterator_fusion };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;

    // Both roots ran: an unreachable value still reports its `dbg`.
    const observed = capture.bytes[0..capture.len];
    try std.testing.expect(std.mem.find(u8, observed, "evaluated used table") != null);
    try std.testing.expect(std.mem.find(u8, observed, "evaluated unused table") != null);

    const host = &session.host.?;
    const helper = moduleArtifactNamed(&coord, "Helper") orelse return error.TestUnexpectedResult;
    const helper_roots = helper.root_requests.compile_time_requests;
    try std.testing.expectEqual(@as(usize, 2), helper_roots.len);
    // Only the value the program reads is materialized and frozen. The other
    // root's value is checked module data, not target data. Transcoding
    // rejects a read whose value the host did not materialize, so the runtime
    // continuation below also fails if the wrong root were the retained one.
    try std.testing.expectEqual(@as(usize, 1), materializedAmong(host, helper.key, helper_roots));
    try std.testing.expect(materializedRootCount(host) < host.lir_result.const_roots.items.len);

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    try std.testing.expect(runtime.frozen_static_data != null);
}

test "one evaluated root has one completed-value slot however many places read it" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeEchoPlatform(tmp_dir.dir, io);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Helper.roc", .data =
        \\Helper := [].{
        \\    triple : U64 -> U64
        \\    triple = |n| n * 3
        \\    base_value : U64
        \\    base_value = triple(7)
        \\    doubled : U64
        \\    doubled = base_value + base_value
        \\    offset : I64
        \\    offset = -3
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Helper
        \\main! = |_args| {
        \\    Echo.line!(Str.inspect(Helper.base_value))
        \\    Echo.line!(Str.inspect(Helper.doubled))
        \\    Echo.line!(Str.inspect(Helper.offset))
        \\    Ok({})
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    // Dev's Solved policy: the runtime consumer continues the compile-time
    // Solved program and reads the values materialized for it.
    const target: lir.CheckedPipeline.TargetConfig = .{ .inline_expects = .omit, .proc_debug_names = true, .inline_mode = .wrappers, .spec_constr_clone_inlining = .iterator_fusion };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    const host = &session.host.?;
    const helper = moduleArtifactNamed(&coord, "Helper") orelse return error.TestUnexpectedResult;
    const helper_roots = helper.root_requests.compile_time_requests;
    try std.testing.expectEqual(@as(usize, 3), helper_roots.len);

    // Every one of these values is computed, read at runtime, and
    // materialized for the runtime consumer; `base_value` is additionally
    // read by `doubled`, at compile time, from this very program. Those two
    // demands name one concrete value, so they are one slot.
    for (helper_roots) |request| {
        const root = rootIdentity(request);
        try std.testing.expect(materializesRoot(host, helper.key, root));
        try std.testing.expectEqual(@as(usize, 1), valueSlotsForRoot(host, helper.key, root));
    }
    try std.testing.expectEqual(@as(usize, 3), materializedAmong(host, helper.key, helper_roots));

    // `base_value` really is read inside this program: a read before the
    // roots are evaluated goes through the slot's accessor, and only a read
    // creates one. A root this program never reads has none, so the shared
    // slot above is a reader's slot and the materialization request took it.
    const base_slot = valueSlotForRoot(host, helper.key, rootIdentity(try rootWithOrder(helper_roots, 0))).?;
    try std.testing.expect(host.lir_result.static_data_values.items[@intFromEnum(base_slot)].accessor != null);
    const offset_slot = valueSlotForRoot(host, helper.key, rootIdentity(try rootWithOrder(helper_roots, 2))).?;
    try std.testing.expect(host.lir_result.static_data_values.items[@intFromEnum(offset_slot)].accessor == null);

    // `base_value` and `offset` commit the same eight-byte layout and are
    // distinct values, so layout agreement alone never merges two demands.
    const first = valueSlotForRoot(host, helper.key, rootIdentity(helper_roots[0])).?;
    try std.testing.expect(helper_roots.len > 1);
    for (helper_roots[1..]) |request| {
        const other = valueSlotForRoot(host, helper.key, rootIdentity(request)).?;
        try std.testing.expect(first != other);
        const first_layout = host.lir_result.static_data_values.items[@intFromEnum(first)].layout_idx;
        const other_layout = host.lir_result.static_data_values.items[@intFromEnum(other)].layout_idx;
        try std.testing.expectEqual(
            host.lir_result.layouts.layoutSize(host.lir_result.layouts.getLayout(first_layout)),
            host.lir_result.layouts.layoutSize(host.lir_result.layouts.getLayout(other_layout)),
        );
    }

    // Transcoding rejects a root with ambiguous source slots, so this also
    // fails if the demands stopped sharing one slot.
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    var value_exports: usize = 0;
    for ((runtime.frozen_static_data orelse return error.TestUnexpectedResult).exports) |item| {
        if (item.value_id != null) value_exports += 1;
    }
    try std.testing.expect(value_exports > 0);
}

test "a separate compile-time consumer lowers no runtime-only procedure" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeEchoPlatform(tmp_dir.dir, io);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Helper.roc", .data =
        \\Helper := [].{
        \\    comptime_only : U64 -> U64
        \\    comptime_only = |n| n + n
        \\    answer : U64
        \\    answer = comptime_only(21)
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Helper
        \\runtime_only_a : U64 -> U64
        \\runtime_only_a = |n| n + 1
        \\runtime_only_b : U64 -> U64
        \\runtime_only_b = |n| n * 3
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(runtime_only_a(args.len())))
        \\    Echo.line!(Str.inspect(runtime_only_b(args.len())))
        \\    Echo.line!(Str.inspect(Helper.answer))
        \\    Ok({})
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    var metrics = lir.CheckedPipeline.WorkMetrics{};
    const target: lir.CheckedPipeline.TargetConfig = .{
        // --opt=speed omits runtime expects, which the evaluation must run,
        // so this compilation's consumers cannot share one program.
        .inline_expects = .omit,
        .inline_mode = .none,
        .proc_debug_names = true,
        .work_metrics = &metrics,
    };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    const host = &session.host.?.lir_result.store;

    // Asserted while the compile-time program is still whole, before any
    // consumer has taken the runtime continuation or released this code.
    try std.testing.expect(session.host != null);
    try std.testing.expect(session.runtime_prepared == null);
    try std.testing.expectEqual(@as(u32, 1), metrics.monotype_runs);
    try std.testing.expectEqual(@as(u32, 1), metrics.solved_runs);
    try std.testing.expectEqual(@as(u32, 1), metrics.lir_continuations);
    try std.testing.expectEqual(@as(usize, 0), countNamedProcs(host, "runtime_only_a"));
    try std.testing.expectEqual(@as(usize, 0), countNamedProcs(host, "runtime_only_b"));
    // However the app module qualifies its names, neither is here.
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(host, "runtime_only"));

    // The runtime consumer names both procedures, so missing debug names
    // cannot be what makes them absent from the evaluation's program.
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 1), countNamedProcs(&runtime.lir_result.store, "runtime_only_a"));
    try std.testing.expectEqual(@as(usize, 1), countNamedProcs(&runtime.lir_result.store, "runtime_only_b"));
}

test "a separate runtime consumer keeps the producer's root order and test-plan metadata" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeEchoPlatform(tmp_dir.dir, io);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Helper.roc", .data =
        \\Helper := [].{
        \\    twice : I64 -> I64
        \\    twice = |n| n + n
        \\    answer : I64
        \\    answer = twice(21)
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import Helper
        \\main! = |_args| Ok({})
        \\expect Helper.twice(1) == 2
        \\expect Helper.twice(21) == Helper.answer
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    const app = coord.appRootCheckedArtifact();
    var expects = std.ArrayList(CheckedArtifact.RootRequest).empty;
    defer expects.deinit(allocator);
    for (app.root_requests.requests) |request| {
        if (request.kind == .test_expect) try expects.append(allocator, request);
    }
    try std.testing.expectEqual(@as(usize, 2), expects.items.len);
    var plan_metadata = std.ArrayList(lir.CheckedPipeline.RootTestPlanMetadata).empty;
    defer plan_metadata.deinit(allocator);
    for (expects.items, 0..) |request, ordinal| {
        try plan_metadata.append(allocator, .{
            .request_index = @intCast(ordinal),
            .root_order = request.order,
            // Distinct from the request order, so a consumer that dropped the
            // pairing could not pass by coincidence.
            .result_index = @intCast(expects.items.len - ordinal),
            .module_index = 3,
            .root_index = @intCast(ordinal + 7),
        });
    }
    const requests: lir.CheckedPipeline.RootRequestSet = .{
        .requests = expects.items,
        .test_plan_metadata = plan_metadata.items,
    };
    var metrics = lir.CheckedPipeline.WorkMetrics{};
    // A width the compile-time consumer cannot serve, so the runtime consumer
    // lowers its own share of the producer program.
    const other_width: base.target.TargetUsize = if (base.target.TargetUsize.native == .u64) .u32 else .u64;
    const target: lir.CheckedPipeline.TargetConfig = .{
        .target_usize = other_width,
        .inline_expects = .run,
        .work_metrics = &metrics,
    };
    coord.runtime_lowering = .{ .target = target, .explicit_roots = requests, .root_module = app };
    try coord.finishCheckedProgram(.none);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    try std.testing.expect(session.host != null);
    try std.testing.expect(session.runtime_prepared == null);

    var runtime = try session.takeRuntime(allocator, requests, target);
    defer runtime.deinit();
    // The runtime consumer specializes the checked modules itself, reading
    // compile-time values from their constant stores.
    try std.testing.expectEqual(@as(u32, 2), metrics.monotype_runs);
    try std.testing.expectEqual(@as(u32, 2), metrics.solved_runs);
    try std.testing.expectEqual(@as(usize, 2), runtime.lir_result.root_metadata.items.len);
    for (expects.items, plan_metadata.items, runtime.lir_result.root_metadata.items) |request, declared, metadata| {
        try std.testing.expectEqual(request.order, metadata.order);
        try std.testing.expectEqual(@as(?u32, declared.result_index), if (metadata.test_plan) |plan| plan.result_index else null);
        try std.testing.expectEqual(@as(?u32, declared.root_index), if (metadata.test_plan) |plan| plan.root_index else null);
    }
}

test "a nominal declaration template over an imported alias carries no unbound variable" {
    // `State`'s backing names `Res(U64)`, whose body names the imported
    // `E.Err`. The template is the checker's own backing, with every alias
    // instance's markers closed as a nominal body writes them, so a nominal
    // with no formals has no variable anywhere in its template.
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writeEchoPlatform(tmp_dir.dir, io);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "E.roc", .data =
        \\module [Err]
        \\
        \\Err : [Bad]
        \\
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import E
        \\
        \\Res(a) : Try(a, E.Err)
        \\
        \\State := { r : Res(U64) }
        \\
        \\main! = |_args| {
        \\    state : State
        \\    state = { r: Ok(1) }
        \\    Echo.line!(match state.r { Ok(n) => Str.inspect(n), Err(Bad) => "bad" })
        \\    Ok({})
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    var found = false;
    var packages = coord.packages.iterator();
    while (packages.next()) |package| {
        for (package.value_ptr.*.modules.items) |*mod| {
            const artifact = mod.checkedArtifact() orelse continue;
            for (artifact.checked_types.nominal_declarations.items) |declaration| {
                if (!std.mem.eql(u8, artifact.canonical_names.typeNameText(declaration.nominal.type_name), "State")) continue;
                found = true;
                try std.testing.expect(!artifact.checked_types.roots.items[@intFromEnum(declaration.backing)].contains_identity_variables);
            }
        }
    }
    try std.testing.expect(found);
}
