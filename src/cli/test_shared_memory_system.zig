//! Tests for CLI platform resolution that do not cross the post-check lowering boundary

const std = @import("std");
const testing = std.testing;
const main = @import("main.zig");
const base = @import("base");
const eval = @import("eval");
const lir = @import("lir");
const test_helpers = eval.test_helpers;
const cli_context = @import("CliCtx.zig");
const CliCtx = cli_context.CliCtx;
const Io = cli_context.Io;

var shared_test_builtins: ?eval.BuiltinModules = null;

fn sharedPrePublishedBuiltin() anyerror!test_helpers.PrePublishedBuiltin {
    if (shared_test_builtins == null) {
        shared_test_builtins = try eval.BuiltinModules.init(std.heap.page_allocator);
    }
    return .{
        .env = shared_test_builtins.?.builtin_module.env,
        .indices = shared_test_builtins.?.builtin_indices,
        .artifact = &shared_test_builtins.?.checked_artifact,
    };
}

test "platform resolution - basic cli platform" {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // Create a CLI context for error reporting
    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(gpa, arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file with cli platform
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\app "test"
        \\    packages { pf: platform "cli" }
        \\    imports [pf.Task]
        \\    provides [main] to pf
        \\
        \\main = "Hello, World!"
    ;

    var roc_file = temp_dir.dir.createFile(std.testing.io, "test.roc", .{}) catch unreachable;
    defer roc_file.close(std.testing.io);
    roc_file.writeStreamingAll(std.testing.io, roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realPathFileAlloc(std.testing.io, "test.roc", gpa);
    defer gpa.free(roc_path);

    // This should return CliError since we don't have the actual CLI platform installed
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - no platform in file" {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // Create a CLI context for error reporting
    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(gpa, arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file without platform specification
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\# Just a simple expression
        \\42 + 58
    ;

    var roc_file = temp_dir.dir.createFile(std.testing.io, "test.roc", .{}) catch unreachable;
    defer roc_file.close(std.testing.io);
    roc_file.writeStreamingAll(std.testing.io, roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realPathFileAlloc(std.testing.io, "test.roc", gpa);
    defer gpa.free(roc_path);

    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - file not found" {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // Create a CLI context for error reporting
    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(gpa, arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    const result = main.resolvePlatformPaths(&ctx, "nonexistent.roc");
    try testing.expectError(error.CliError, result);
}

test "platform resolution - insecure HTTP URL rejected" {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // Create a CLI context for error reporting
    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(gpa, arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file with insecure HTTP URL (not localhost)
    // This should be rejected for security - only HTTPS or localhost HTTP allowed
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\app [main] { pf: platform "http://example.com/abc123.tar.zst" }
        \\
        \\main = "Hello, World!"
    ;

    var roc_file = temp_dir.dir.createFile(std.testing.io, "test.roc", .{}) catch unreachable;
    defer roc_file.close(std.testing.io);
    roc_file.writeStreamingAll(std.testing.io, roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realPathFileAlloc(std.testing.io, "test.roc", gpa);
    defer gpa.free(roc_path);

    // Insecure HTTP URLs (not localhost) should fail validation
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

fn compileLirImageForSharedTest(
    allocator: std.mem.Allocator,
    source: []const u8,
    imports: []const test_helpers.ModuleSource,
) anyerror!test_helpers.CompiledTargetProgram {
    return compileLirImageForSharedTestTarget(allocator, source, imports, .native);
}

fn compileLirImageForSharedTestTarget(
    allocator: std.mem.Allocator,
    source: []const u8,
    imports: []const test_helpers.ModuleSource,
    target_usize: base.target.TargetUsize,
) anyerror!test_helpers.CompiledTargetProgram {
    return test_helpers.compileProgramForTargetWithBuiltin(
        allocator,
        std.testing.io,
        .module,
        source,
        imports,
        target_usize,
        try sharedPrePublishedBuiltin(),
    );
}

fn expectLirImageCanBeViewedFromMappedHeader(compiled: *const test_helpers.CompiledTargetProgram) anyerror!void {
    const used = compiled.lowered.shm.getUsedSize();
    try testing.expect(used > @sizeOf(lir.LirImage.Header));
    try testing.expect(compiled.lowered.view.root_procs.len > 0);
    try testing.expect(compiled.lowered.view.store.proc_specs.items.len > 0);
    try testing.expect(compiled.lowered.view.layouts.layouts.items.items.len > 0);

    const header = compiled.lowered.image_header;
    const child_view = try lir.LirImage.viewMappedImage(header, compiled.lowered.shm.base_ptr, used);
    try testing.expectEqual(lir.LirImage.MAGIC, header.magic);
    try testing.expectEqual(lir.LirImage.FORMAT_VERSION, header.format_version);
    try testing.expectEqual(compiled.lowered.view.root_procs.len, child_view.root_procs.len);
    try testing.expectEqual(compiled.lowered.view.store.proc_specs.items.len, child_view.store.proc_specs.items.len);
    try testing.expectEqual(compiled.lowered.view.layouts.layouts.items.items.len, child_view.layouts.layouts.items.items.len);
}

fn expectPublishedImportArtifactCount(compiled: *const test_helpers.CompiledTargetProgram, expected: usize) anyerror!void {
    const borrowed_builtin_count: usize = if (compiled.resources.borrowed_builtin_artifact != null) 1 else 0;
    try testing.expectEqual(expected, compiled.resources.import_artifacts.len + borrowed_builtin_count);
}

test "integration - shared memory setup and parsing" {
    var compiled = try compileLirImageForSharedTest(
        testing.allocator,
        "main : () -> I64\nmain = || 40 + 2",
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&compiled);
}

test "integration - compilation pipeline for native platform" {
    var native = try compileLirImageForSharedTestTarget(
        testing.allocator,
        "main : () -> List(I64)\nmain = || [1, 2, 3]",
        &.{},
        .native,
    );
    defer native.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&native);
    try testing.expectEqual(base.target.TargetUsize.native, native.lowered.view.target_usize);
}

test "integration - compilation pipeline for u32 platform" {
    var wasm32 = try compileLirImageForSharedTestTarget(
        testing.allocator,
        "main : () -> List(I64)\nmain = || [1, 2, 3]",
        &.{},
        .u32,
    );
    defer wasm32.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&wasm32);
    try testing.expectEqual(base.target.TargetUsize.u32, wasm32.lowered.view.target_usize);
}

test "integration - error handling for non-existent file" {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(gpa, arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    try testing.expectError(error.CliError, main.resolvePlatformPaths(&ctx, "does/not/exist.roc"));
}

test "integration - automatic module dependency ordering" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "Leaf", .source = "module [value]\nvalue : I64\nvalue = 40\n" },
        .{ .name = "Branch", .source = "module [value]\nimport Leaf\nvalue : I64\nvalue = Leaf.value + 2\n" },
    };
    var compiled = try compileLirImageForSharedTest(
        testing.allocator,
        "import Branch\nmain : () -> I64\nmain = || Branch.value",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&compiled);
    try expectPublishedImportArtifactCount(&compiled, 3);
}

test "integration - transitive module imports (module A imports module B)" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "B", .source = "module [value]\nvalue : I64\nvalue = 40\n" },
        .{ .name = "A", .source = "module [value]\nimport B\nvalue : I64\nvalue = B.value + 2\n" },
    };
    var compiled = try compileLirImageForSharedTest(
        testing.allocator,
        "import A\nmain : () -> I64\nmain = || A.value",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&compiled);
    try expectPublishedImportArtifactCount(&compiled, 3);
}

test "integration - diamond dependency pattern (A imports B and C, both import D)" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "D", .source = "module [value]\nvalue : {} -> I64\nvalue = |_| 10\n" },
        .{ .name = "B", .source = "module [value]\nimport D\nvalue : {} -> I64\nvalue = |_| D.value({}) + 10\n" },
        .{ .name = "C", .source = "module [value]\nimport D\nvalue : {} -> I64\nvalue = |_| D.value({}) + 12\n" },
        .{ .name = "A", .source = "module [value]\nimport B\nimport C\nvalue : {} -> I64\nvalue = |_| B.value({}) + C.value({})\n" },
    };
    var compiled = try compileLirImageForSharedTest(
        testing.allocator,
        "import A\nmain : () -> I64\nmain = || A.value({})",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&compiled);
    try expectPublishedImportArtifactCount(&compiled, 5);
}

test "integration - direct Core and Utils calls from app" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "Core", .source = "module [inc]\ninc : I64 -> I64\ninc = |x| x + 1\n" },
        .{ .name = "Utils", .source = "module [double]\ndouble : I64 -> I64\ndouble = |x| x * 2\n" },
    };
    var compiled = try compileLirImageForSharedTest(
        testing.allocator,
        "import Core\nimport Utils\nmain : () -> I64\nmain = || Utils.double(Core.inc(20))",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectLirImageCanBeViewedFromMappedHeader(&compiled);
    try expectPublishedImportArtifactCount(&compiled, 3);
}
