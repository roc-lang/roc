//! Compile-time evaluation runs a dev build's code: every procedure a dev
//! build of a program shares with the program's compile-time evaluation has
//! the same machine code and metadata in both, so the build can take the
//! evaluation's code instead of compiling it again.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const backend = @import("backend");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const ProcArtifact = backend.dev.ProcArtifact;

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

/// How two artifacts of one procedure differ, or null when they are the same
/// code: the same bytes, frame, references, relocations, lines, and failure
/// sites. A relocation to a datum the program numbers names that datum by the
/// program's own numbering, which two programs need not share, so only its
/// scope and kind are compared.
fn artifactDifference(expected: ProcArtifact.Artifact, actual: ProcArtifact.Artifact) ?[]const u8 {
    if (!std.mem.eql(u8, expected.code, actual.code)) return "code";
    if (expected.entry != actual.entry) return "entry";
    if (!std.meta.eql(expected.frame, actual.frame)) return "frame";
    if (expected.refs.len != actual.refs.len) return "reference count";
    for (expected.refs, actual.refs) |left, right| {
        if (left.site != right.site or left.form != right.form or left.delta != right.delta) return "reference";
    }
    if (expected.symbolic_refs.len != actual.symbolic_refs.len) return "symbolic reference count";
    if (expected.relocations.len != actual.relocations.len) return "relocation count";
    for (expected.relocations, actual.relocations) |left, right| {
        if (left.offset != right.offset or left.scope != right.scope or !std.meta.eql(left.kind, right.kind)) return "relocation";
        if (left.scope == .shared and !std.mem.eql(u8, left.name, right.name)) return "relocation target";
    }
    if (expected.lines.len != actual.lines.len) return "line count";
    for (expected.lines, actual.lines) |left, right| {
        if (left.offset != right.offset or !std.meta.eql(left.loc, right.loc)) return "line";
    }
    if (expected.failure_sites.len != actual.failure_sites.len) return "failure site count";
    for (expected.failure_sites, actual.failure_sites) |left, right| {
        if (left.offset != right.offset or !std.meta.eql(left.meaning, right.meaning)) return "failure site";
    }
    return null;
}

test "every procedure compile-time evaluation shares with a dev build is the build's code" {
    if (is_freestanding or comptime !backend.host_lir_codegen_available) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    for (echo_platform) |file| {
        if (std.fs.path.dirname(file.path)) |dir| try tmp_dir.dir.createDirPath(io, dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    }
    // Compile-time evaluation runs `square` through `List.map`, `describe`
    // reading the folded `total`, string and dictionary operations, and the
    // program runs each again at runtime.
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\square : U64 -> U64
        \\square = |n| n * n
        \\total : U64
        \\total = List.map([1, 2, 3, 4], square).sum()
        \\describe : U64 -> Str
        \\describe = |n| if n > total { "big" } else { "small" }
        \\bigger : Str
        \\bigger = describe(40)
        \\names : List(Str)
        \\names = ["alpha", "beta"].map(|s| Str.concat(s, "!"))
        \\lookup : Dict(Str, U64)
        \\lookup = Dict.empty().insert("a", 1).insert("b", total)
        \\main! = |args| {
        \\    lengths = List.map(args, |arg| arg.to_utf8().len())
        \\    Echo.line!(describe(args.len()))
        \\    Echo.line!(bigger)
        \\    Echo.line!(Str.inspect(names.map(|s| Str.concat(s, "!"))))
        \\    Echo.line!(Str.inspect(Dict.empty().insert("a", 1).insert("b", args.len()).get("b")))
        \\    Echo.line!(Str.inspect(lookup.get("b")))
        \\    Echo.line!(Str.inspect(List.map(lengths, square).sum()))
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
        build_options.compiler_compatibility_id,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    coord.keep_compile_time_artifacts = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    // A dev build's runtime lowering (`checkedRuntimeLoweringConfig` for
    // `--opt=dev`).
    coord.runtime_lowering = .{
        .include_provided_data_exports = true,
        .include_internal_static_data = true,
        .target = .{
            .inline_mode = lir.CheckedPipeline.dev_inline_mode,
            .spec_constr_clone_inlining = lir.CheckedPipeline.dev_spec_constr_clone_inlining,
            .consume_dead_boxes = true,
            .inline_expects = .run,
            .list_in_place_map = false,
            .zero_dict_seed = true,
            .tag_reachability = false,
            .prove_ranges = false,
            .scalarize_joins = false,
            .reuse_boxes = false,
            .thread_confined_rc = false,
            .proc_debug_names = true,
        },
    };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    var evaluated = try session.completed.?.combinedArtifacts(allocator);
    defer evaluated.deinit();

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, coord.runtime_lowering.?.target);
    defer runtime.deinit();
    var compiler = backend.ObjectFileCompiler.initForPack(allocator);
    compiler.emit_object = false;
    var compiled = try compiler.compileToObjectFile(
        &runtime.lir_result.store,
        &runtime.lir_result.layouts,
        &.{},
        if (runtime.frozen_static_data) |frozen| frozen.exports else &.{},
        runtime.lir_result.store.getProcSpecs(),
        runtime.lir_result.boxy_erased_arg_desc_offsets.items,
        runtime.lir_result.boxy_erased_arg_desc_params.items,
        runtime.lir_result.boxy_worker_procs.items,
        roc_target.RocTarget.detectNative(),
    );
    defer compiled.deinit();
    const built = &(compiled.artifacts orelse return error.TestUnexpectedResult);

    var built_by_identity = std.AutoHashMap(lir.ProcIdentity, usize).init(allocator);
    defer built_by_identity.deinit();
    for (built.artifacts, 0..) |artifact, index| switch (artifact.kind) {
        .proc => |identity| try built_by_identity.put(identity, index),
        .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
    };
    var shared: usize = 0;
    var differing: usize = 0;
    for (evaluated.artifacts) |artifact| {
        const identity = switch (artifact.kind) {
            .proc => |identity| identity,
            .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => continue,
        };
        const index = built_by_identity.get(identity) orelse continue;
        shared += 1;
        if (artifactDifference(built.artifacts[index], artifact)) |difference| {
            differing += 1;
            var name: []const u8 = "";
            for (runtime.lir_result.store.getProcSpecs(), 0..) |proc, proc_index| {
                if (std.meta.eql(proc.identity, identity)) name = runtime.lir_result.store.procDebugName(@fromBackingInt(@intCast(proc_index))) orelse "";
            }
            std.debug.print("procedure {s} ({x}) differs in its {s}\n", .{ name, identity.bytes[0..8], difference });
        }
    }
    try std.testing.expectEqual(@as(usize, 0), differing);
    // `square`'s map, `describe`, and the string and dictionary operations.
    try std.testing.expect(shared >= 4);
}
