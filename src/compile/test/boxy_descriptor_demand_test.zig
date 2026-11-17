//! A boxy descriptor's `Methods` slot is present exactly where a
//! descriptor-guided equality, hashing, or inspection reaches it (design.md
//! "Descriptor Demand"): through list items, record fields, tag payloads of a
//! recursive nominal type, and a nominal type's backing, but not through an
//! opaque type's backing when printing.
const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const layout = @import("layout");
const roc_target = @import("roc_target");
const static_data_exports = @import("static_data");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;
const harness = @import("lower_to_lir_harness.zig");

fn keepsProc(store: *const lir.LirStore, comptime suffix: []const u8) bool {
    for (0..store.getProcSpecs().len) |index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (std.mem.endsWith(u8, name, suffix)) return true;
    }
    return false;
}

fn expectKeepsTokenToInspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    if (!keepsProc(store, "Token.to_inspect")) return error.TestUnexpectedResult;
}

fn expectKeepsTokenIsEq(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    if (!keepsProc(store, "Token.is_eq")) return error.TestUnexpectedResult;
}

fn expectKeepsTokenToHash(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    if (!keepsProc(store, "Token.to_hash")) return error.TestUnexpectedResult;
}

fn expectNoTokenToInspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    if (keepsProc(store, "Token.to_inspect")) return error.TestUnexpectedResult;
}

fn expectNoTryEqOrHash(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    if (keepsProc(store, "Try.is_eq") or keepsProc(store, "Try.to_hash")) return error.TestUnexpectedResult;
}

const token_decl =
    \\Token := [Token(U64)].{
    \\    is_eq : Token, Token -> Bool
    \\    is_eq = |_, _| Bool.True
    \\    to_hash : Token, Hasher -> Hasher
    \\    to_hash = |_, hasher| "TOKEN_HASH".to_hash(hasher)
    \\    to_inspect : Token -> Str
    \\    to_inspect = |_| "TOKEN_INSPECT"
    \\}
    \\identity : a -> a
    \\identity = |x| x
    \\
;

fn expectFor(comptime body: []const u8, inspect: harness.LirInspectFn) harness.LowerToLirHarnessError!void {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(token_decl ++ body, .{ .specialization_strategy = .boxy, .target_usize = width }, inspect);
    }
}

test "inspecting a list of records reaches the to_inspect override of a field's nominal type" {
    try expectFor(
        \\main! = |args| {
        \\    token = Token.Token(args.len())
        \\    echo!(Str.inspect(identity([{ token }])))
        \\    Ok({})
        \\}
    , expectKeepsTokenToInspect);
}

test "comparing a recursive nominal type reaches the is_eq override in its tag payloads" {
    try expectFor(
        \\Tree := [Leaf, Node(Tree, Token)].{
        \\    is_eq : _
        \\}
        \\main! = |args| {
        \\    left = Tree.Node(Tree.Leaf, Token.Token(args.len()))
        \\    right = Tree.Node(Tree.Leaf, Token.Token(0))
        \\    echo!(if identity(left) == right { "same" } else { "different" })
        \\    Ok({})
        \\}
    , expectKeepsTokenIsEq);
}

test "hashing a nominal type with derived hashing reaches the to_hash override in its backing" {
    try expectFor(
        \\Wrapper := [Wrapper(Token)].{
        \\    is_eq : _
        \\    to_hash : _
        \\}
        \\main! = |args| {
        \\    key = Wrapper.Wrapper(Token.Token(args.len()))
        \\    echo!(if Dict.single(identity(key), 1.U64).contains(key) { "found" } else { "missing" })
        \\    Ok({})
        \\}
    , expectKeepsTokenToHash);
}

test "inspecting an opaque type without to_inspect does not reach the to_inspect override in its backing" {
    try expectFor(
        \\Hidden :: [Hidden(Token)]
        \\main! = |args| {
        \\    echo!(Str.inspect(identity([Hidden.Hidden(Token.Token(args.len()))])))
        \\    Ok({})
        \\}
    , expectNoTokenToInspect);
}

test "a program that compares no Try keeps neither Try.is_eq nor Try.to_hash" {
    try expectFor(
        \\main! = |args| {
        \\    n = match identity(Token.Token(args.len())) { Token(k) => k + 1 }
        \\    echo!(n.to_str())
        \\    Ok({})
        \\}
    , expectNoTryEqOrHash);
}

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
    \\        Err(_) => 1
    \\    }
    },
    .{ .path = ".roc_echo_platform/Echo.roc", .source =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
    },
};

/// How many `Methods` nodes a lowering's static data holds: the nodes
/// `StaticDescs` names with index 3 of their owning descriptor's slot.
fn methodsNodeCount(static_data: []const lir.Program.StaticDataExport) usize {
    var count: usize = 0;
    for (static_data) |item| {
        if (std.mem.startsWith(u8, item.symbol_name, "roc__d") and std.mem.endsWith(u8, item.symbol_name, "_3")) count += 1;
    }
    return count;
}

fn findArtifact(coord: *Coordinator, views: []const check.CheckedArtifact.ImportedModuleView, name: []const u8) error{TestUnexpectedResult}!*const check.CheckedArtifact.CheckedModuleArtifact {
    for (views) |view| {
        if (std.mem.eql(u8, view.module_env.module_name, name)) return coord.checkedArtifactByKey(view.key.digest()) orelse error.TestUnexpectedResult;
    }
    return error.TestUnexpectedResult;
}

test "a module unit fills an imported generic worker's tables with its own descriptors and the demand the worker's needs export" {
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
        \\import pf.Echo
        \\import Printing
        \\import Passing
        \\main! = |_args| {
        \\    Echo.line!(Printing.shown(5))
        \\    Echo.line!(Passing.passed(5).to_str())
        \\    Ok({})
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Library.roc", .data =
        \\module [render, keep]
        \\render : a -> Str
        \\render = |value| Str.inspect(value)
        \\keep : a -> a
        \\keep = |value| value
    });
    // Each consumer declares a type with an override and passes a value of it
    // to a generic worker of `Library`: one that inspects it and one that only
    // moves it.
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Printing.roc", .data =
        \\module [shown]
        \\import Library
        \\Token := [Token(U64)].{
        \\    to_inspect : Token -> Str
        \\    to_inspect = |_| "custom"
        \\}
        \\shown : U64 -> Str
        \\shown = |n| Library.render(Token.Token(n))
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Passing.roc", .data =
        \\module [passed]
        \\import Library
        \\Token := [Token(U64)].{
        \\    to_inspect : Token -> Str
        \\    to_inspect = |_| "custom"
        \\}
        \\passed : U64 -> U64
        \\passed = |n| match Library.keep(Token.Token(n)) {
        \\    Token(k) => k
        \\}
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
        .boxy,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());

    const app_imports = try coord.collectImportedArtifactViews(arena.allocator(), coord.appRootCheckedArtifact());
    const printing = try findArtifact(&coord, app_imports, "Printing");
    const passing = try findArtifact(&coord, app_imports, "Passing");
    const printing_imports = try coord.collectImportedArtifactViews(arena.allocator(), printing);
    const library = try findArtifact(&coord, printing_imports, "Library");

    var all_views: std.ArrayList(check.CheckedArtifact.ImportedModuleView) = .empty;
    defer all_views.deinit(allocator);
    for ([_]*const check.CheckedArtifact.CheckedModuleArtifact{ library, printing, passing }) |artifact| {
        try all_views.append(allocator, check.CheckedArtifact.importedView(artifact));
    }
    for (printing_imports) |view| {
        if (!std.mem.eql(u8, view.module_env.module_name, "Library")) try all_views.append(allocator, view);
    }
    var names = try lir.CheckedPipeline.BoxyModuleNames.fromViews(allocator, all_views.items);
    defer names.deinit(allocator);
    var interfaces = lir.CheckedPipeline.BoxyExportInterfaces.init(allocator, io);
    defer interfaces.deinit();

    // The Builtin unit, then `Library`'s, export their workers with their
    // needs lists, as the units a build compiles before the consumers.
    for ([_]*const check.CheckedArtifact.CheckedModuleArtifact{ &coord.builtin_modules.checked_artifact, library }) |artifact| {
        var producer = try boxy.run(allocator, .{
            .modules = .{ .root = check.CheckedArtifact.loweringView(artifact), .imports = try coord.collectImportedArtifactViews(arena.allocator(), artifact) },
            .roots = .{ .requests = &.{} },
            .target_usize = base.target.TargetUsize.native,
            .unit = .{ .module = .{ .imports = &interfaces, .root = false, .names = &names } },
        });
        defer producer.lir_result.deinit();
        defer producer.runtime_schemas.deinit();
        defer static_data_exports.deinitStaticData(allocator, producer.static_data);
        for (producer.lir_result.exported_procs.items) |proc| {
            const identity = producer.lir_result.store.getProcSpec(proc).identity;
            _ = try interfaces.putProc(&producer.lir_result.store, proc, producer.lir_result.boxy_table_needs.get(identity) orelse &.{}, producer.lir_result.boxy_exports.get(identity).?);
        }
    }

    // A consumer's `Token` descriptor holds its `to_inspect` override only
    // where the imported worker inspects it.
    const Case = struct { artifact: *const check.CheckedArtifact.CheckedModuleArtifact, methods_nodes: usize };
    for ([_]Case{
        .{ .artifact = printing, .methods_nodes = 1 },
        .{ .artifact = passing, .methods_nodes = 0 },
    }) |case| {
        var consumer = try boxy.run(allocator, .{
            .modules = .{ .root = check.CheckedArtifact.loweringView(case.artifact), .imports = try coord.collectImportedArtifactViews(arena.allocator(), case.artifact) },
            .roots = .{ .requests = &.{} },
            .target_usize = base.target.TargetUsize.native,
            .unit = .{ .module = .{ .imports = &interfaces, .root = false, .names = &names } },
        });
        defer consumer.lir_result.deinit();
        defer consumer.runtime_schemas.deinit();
        defer static_data_exports.deinitStaticData(allocator, consumer.static_data);
        defer if (consumer.retained) |retained| retained.deinit();
        try std.testing.expectEqual(case.methods_nodes, methodsNodeCount(consumer.static_data));
    }
}
