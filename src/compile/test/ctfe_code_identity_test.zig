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
const check = @import("check");
const static_data_exports = @import("static_data");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const CodeExcerpt = backend.dev.CodeExcerpt;

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

test "linked Boxy CTFE roots omit unrelated workers while both producer modes export them" {
    if (is_freestanding) return error.SkipZigTest;
    const boxy = @import("postcheck").Boxy;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    for (echo_platform) |file| {
        if (std.fs.path.dirname(file.path)) |dir| try tmp_dir.dir.createDirPath(io, dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    }
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import Library
        \\main! = |_args| Ok(Library.answer)
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Library.roc", .data =
        \\module [answer, used, unrelated]
        \\import Other
        \\used : {} -> {}
        \\used = |value| Other.identity(value)
        \\answer = used({})
        \\unrelated : a -> a
        \\unrelated = |value| value
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Other.roc", .data =
        \\module [identity]
        \\identity : {} -> {}
        \\identity = |value| value
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);
    var builtin_modules = try eval.BuiltinModules.init(allocator, io);
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
    coord.specialization_strategy = .boxy;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    // Leave the constant pending: exercise lowering its requested root,
    // without evaluating it or generating native code during setup.
    const app_imports = try coord.collectImportedArtifactViews(arena.allocator(), coord.appRootCheckedArtifact());
    const library = for (app_imports) |view| {
        if (std.mem.eql(u8, view.module_env.module_name, "Library")) {
            break coord.checkedArtifactByKey(view.key.digest()) orelse return error.TestUnexpectedResult;
        }
    } else return error.TestUnexpectedResult;
    const library_view = check.CheckedArtifact.importedView(library);
    const imports = try coord.collectImportedArtifactViews(arena.allocator(), library);
    const views = try arena.allocator().alloc(check.CheckedArtifact.ImportedModuleView, imports.len + 1);
    views[0] = library_view;
    @memcpy(views[1..], imports);
    var names = try lir.CheckedPipeline.BoxyModuleNames.fromViews(allocator, views);
    defer names.deinit(allocator);
    var interfaces = lir.CheckedPipeline.BoxyExportInterfaces.init(allocator, io);
    defer interfaces.deinit();

    // Produce the interface of code Library calls through the link. Its
    // checked bodies must remain unindexed in the subsequent consumer.
    const other = for (imports) |view| {
        if (std.mem.eql(u8, view.module_env.module_name, "Other")) break coord.checkedArtifactByKey(view.key.digest()).?;
    } else return error.TestUnexpectedResult;
    var producer = try boxy.run(allocator, .{
        .modules = .{ .root = check.CheckedArtifact.loweringView(other), .imports = try coord.collectImportedArtifactViews(arena.allocator(), other) },
        .roots = .{ .requests = &.{} },
        .target_usize = base.target.TargetUsize.native,
        .unit = .{ .module = .{ .imports = &interfaces, .root = false, .names = &names } },
    });
    defer producer.lir_result.deinit();
    defer static_data_exports.deinitStaticData(allocator, producer.static_data);
    for (producer.lir_result.exported_procs.items) |proc| {
        const identity = producer.lir_result.store.getProcSpec(proc).identity;
        _ = try interfaces.putProc(&producer.lir_result.store, proc, producer.lir_result.boxy_table_needs.get(identity) orelse &.{});
    }

    // Identify the worker from the checked declaration, independently of the
    // lowering's export inventory. Its generic argument also requires a table
    // contract in producer mode, even though no root calls it.
    const unrelated_template = for (library.checked_procedure_templates.templates.items()) |template| {
        const name = library.canonical_names.procBase(template.proc_base).export_name.get() orelse continue;
        if (std.mem.eql(u8, library.canonical_names.exportNameText(name), "unrelated")) break template.template_id;
    } else return error.TestUnexpectedResult;
    const unrelated_identity = lir.CheckedPipeline.boxyTemplateWorkerIdentity(names.of(library_view), unrelated_template);
    const used_template = for (library.checked_procedure_templates.templates.items()) |template| {
        const name = library.canonical_names.procBase(template.proc_base).export_name.get() orelse continue;
        if (std.mem.eql(u8, library.canonical_names.exportNameText(name), "used")) break template.template_id;
    } else return error.TestUnexpectedResult;
    const used_identity = lir.CheckedPipeline.boxyTemplateWorkerIdentity(names.of(library_view), used_template);
    const roots = library.root_requests.compile_time_requests.items();
    try std.testing.expect(roots.len != 0);

    const Case = struct {
        compile_time: bool,
        stored_functions: bool = false,
        function_unit: ?[32]u8 = null,
        exports_unrelated: bool,
    };
    var completed = false;
    for ([_]Case{
        .{ .compile_time = true, .exports_unrelated = false },
        .{ .compile_time = false, .exports_unrelated = true },
        .{ .compile_time = false, .stored_functions = true, .function_unit = @splat(17), .exports_unrelated = true },
    }) |case| {
        // Runtime producers consume completed roots, unlike the evaluator
        // consumer above whose pending roots are the subject of the test.
        if (!case.compile_time and !completed) {
            coord.specialization_strategy = .boxy;
            try coord.finishCheckedProgram(.executable_artifacts);
            try std.testing.expect(!coord.hasUserErrors());
            completed = true;
        }
        // Restored checked modules have their CIR and published tables, but
        // fresh analysis slots. Preparation must not demand every visible
        // module's bodies; this unit emits only Library's workers.
        var lazy_library = library.*;
        lazy_library.body_analyses = try check.BodyAnalyses.BodyAnalyses.create(allocator, io, library.moduleEnvConst(), library.checked_procedure_templates.promoted.items());
        defer lazy_library.body_analyses.?.destroy();
        const lazy_imports = try allocator.dupe(check.CheckedArtifact.ImportedModuleView, imports);
        defer allocator.free(lazy_imports);
        var initialized: usize = 0;
        defer for (lazy_imports[0..initialized]) |view| view.checked_bodies.analyses.destroy();
        for (lazy_imports) |*view| {
            view.checked_bodies.analyses = try check.BodyAnalyses.BodyAnalyses.create(allocator, io, view.module_env, view.checked_procedure_templates.promoted.items());
            initialized += 1;
        }
        try std.testing.expect(lazy_imports.len != 0);
        const modules: lir.CheckedPipeline.CheckedModuleSet = .{ .root = check.CheckedArtifact.loweringView(&lazy_library), .imports = lazy_imports };
        var prepared = try lir.CheckedPipeline.prepareBoxyCheckedModules(allocator, modules, .{ .requests = if (case.compile_time) roots else &.{} }, .{
            .specialization_strategy = .boxy,
            .checked_module_state = .checking_finalization,
        });
        defer prepared.deinit();
        try std.testing.expect(lazy_library.body_analyses.?.indexed.peek() == null);
        for (lazy_imports) |view| try std.testing.expect(view.checked_bodies.analyses.indexed.peek() == null);

        var output = try boxy.run(allocator, .{
            .modules = .{ .root = modules.root, .imports = modules.imports },
            .roots = .{ .requests = if (case.compile_time) roots else &.{} },
            .target_usize = base.target.TargetUsize.native,
            .compile_time = case.compile_time,
            .stored_functions = case.stored_functions,
            .function_unit = case.function_unit,
            .unit = .{ .module = .{ .imports = &interfaces, .root = false, .names = &names } },
        });
        defer output.lir_result.deinit();
        defer output.runtime_schemas.deinit();
        defer static_data_exports.deinitStaticData(allocator, output.static_data);
        defer if (output.retained) |retained| retained.deinit();
        try std.testing.expect(lazy_library.body_analyses.?.indexed.peek() != null);
        for (lazy_imports) |view| try std.testing.expect(view.checked_bodies.analyses.indexed.peek() == null);

        const has_linked_import = for (output.lir_result.store.getProcSpecs()) |proc| {
            if (proc.linked) break true;
        } else false;
        try std.testing.expect(has_linked_import);
        const has_used_body = for (output.lir_result.store.getProcSpecs()) |proc| {
            if (std.meta.eql(proc.identity, used_identity) and !proc.linked) break true;
        } else false;
        try std.testing.expect(has_used_body);
        const has_unrelated_body = for (output.lir_result.store.getProcSpecs()) |proc| {
            if (std.meta.eql(proc.identity, unrelated_identity) and !proc.linked) break true;
        } else false;
        const exports_unrelated = for (output.lir_result.exported_procs.items) |proc| {
            if (std.meta.eql(output.lir_result.store.getProcSpec(proc).identity, unrelated_identity)) break true;
        } else false;
        try std.testing.expectEqual(case.exports_unrelated, has_unrelated_body);
        try std.testing.expectEqual(case.exports_unrelated, exports_unrelated);
        try std.testing.expectEqual(case.exports_unrelated, output.lir_result.boxy_table_needs.contains(unrelated_identity));
        try std.testing.expectEqual(case.stored_functions, output.retained != null);
        if (case.compile_time) {
            try std.testing.expectEqual(roots.len, output.lir_result.const_roots.items.len);
            try std.testing.expectEqual(@as(usize, 0), output.lir_result.exported_procs.items.len);
            try std.testing.expectEqual(@as(usize, 0), output.lir_result.boxy_table_needs.count());
        }
    }
}

/// How two artifacts of one procedure differ, or null when they are the same
/// code: the same bytes, frame, references, relocations, lines, and failure
/// sites. Compile-time evaluation's combined artifacts name each constant by
/// its content, so a relocation of `expected` to a constant the build
/// numbers is compared by the content name `expected_names` gives it.
fn excerptDifference(expected: CodeExcerpt.Excerpt, expected_names: *const CodeExcerpt.ContentNames, actual: CodeExcerpt.Excerpt) ?[]const u8 {
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
        const left_scope: @TypeOf(left.scope) = if (expected_names.renames(left.name)) .shared else left.scope;
        if (left.offset != right.offset or left_scope != right.scope or !std.meta.eql(left.kind, right.kind)) return "relocation";
        if (left_scope == .shared and !std.mem.eql(u8, expected_names.of(left.name), right.name)) return "relocation target";
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
    // program runs each again at runtime. Each value reads `offset_by`, which
    // holds a function, so the Solved program evaluates them all (design.md
    // "Compile-Time Evaluation Under `.lss`").
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\square : U64 -> U64
        \\square = |n| n * n
        \\make_offset : U64 -> (U64 -> U64)
        \\make_offset = |k| |n| n + k
        \\offset_by : U64 -> U64
        \\offset_by = make_offset(0)
        \\total : U64
        \\total = offset_by(List.map([1, 2, 3, 4], square).sum())
        \\describe : U64 -> Str
        \\describe = |n| if n > total { "big" } else { "small" }
        \\bigger : Str
        \\bigger = describe(40)
        \\names : List(Str)
        \\names = ["alpha", "beta"].map(|s| Str.concat(s, "!")).drop_first(offset_by(0))
        \\lookup : Dict(Str, U64)
        \\lookup = Dict.empty().insert("a", 1).insert("b", total)
        \\measure : U64 -> { large : U128, ratio : F64, text : Str }
        \\measure = |n| {
        \\    var $i = n
        \\    while $i > 0 { $i = $i - 1 }
        \\    { large: n.to_u128() * 18446744073709551617, ratio: n.to_f64() / 2.0, text: "a long string carried by both native procedure consumers" }
        \\}
        \\measured = measure(offset_by(3))
        \\main! = |args| {
        \\    lengths = List.map(args, |arg| arg.to_utf8().len())
        \\    increment = args.len().to_u64()
        \\    Echo.line!(Str.inspect(lengths.map(|n| n + increment)))
        \\    Echo.line!(Str.inspect(measured))
        \\    Echo.line!(Str.inspect(measure(args.len())))
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

    var builtin_modules = try eval.BuiltinModules.init(allocator, std.testing.io);
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
            .owned_proc_signatures = true,
            .erased_capture_prefix = @import("builtins").erased_callable.hot_reload_capture_prefix_size,
            .consume_dead_boxes = true,
            .inline_expects = .run,
            .list_in_place_map = false,
            .zero_dict_seed = true,
            .site_code_keys = true,
            .tag_reachability = false,
            .prove_ranges = false,
            .reuse_boxes = false,
            .thread_confined_rc = false,
            .proc_debug_names = true,
        },
    };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    var evaluated = try session.completed.?.combinedExcerpts(allocator);
    defer evaluated.deinit();

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, coord.runtime_lowering.?.target);
    defer runtime.deinit();
    // The build links the runtime program's materialized static data.
    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena.allocator(), root);
    const relations = try coord.collectRelationArtifactViews(arena.allocator(), root);
    const image = try static_data_exports.buildStaticData(
        allocator,
        .{ .root = check.CheckedArtifact.loweringViewWithRelations(root, relations), .imports = imports },
        &runtime,
        roc_target.RocTarget.detectNative(),
        .{ .include_provided_exports = true },
    );
    defer static_data_exports.deinitStaticData(allocator, image);
    var compiler = backend.ObjectFileCompiler.initForExcerpts(allocator);
    compiler.emit_object = false;
    var compiled = try compiler.compileToObjectFile(
        &runtime.lir_result.store,
        &runtime.lir_result.layouts,
        &.{},
        image,
        runtime.lir_result.store.getProcSpecs(),
        roc_target.RocTarget.detectNative(),
    );
    defer compiled.deinit();
    const built = &(compiled.excerpts orelse return error.TestUnexpectedResult);
    var built_names = try CodeExcerpt.ContentNames.init(allocator, built);
    defer built_names.deinit();
    var built_by_identity = std.AutoHashMap(lir.ProcIdentity, usize).init(allocator);
    defer built_by_identity.deinit();
    for (built.excerpts, 0..) |artifact, index| switch (artifact.kind) {
        .proc => |identity| try built_by_identity.put(identity, index),
        .rc_helper, .entrypoint, .message_pool_run, .branch_island => {},
    };
    var shared: usize = 0;
    var differing: usize = 0;
    for (evaluated.excerpts) |artifact| {
        const identity = switch (artifact.kind) {
            .proc => |identity| identity,
            .rc_helper, .entrypoint, .message_pool_run, .branch_island => continue,
        };
        const index = built_by_identity.get(identity) orelse continue;
        shared += 1;
        if (excerptDifference(built.excerpts[index], &built_names, artifact)) |difference| {
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
