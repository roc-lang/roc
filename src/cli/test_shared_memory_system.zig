//! Tests for CLI platform resolution that do not cross the post-check lowering boundary

const std = @import("std");
const testing = std.testing;
const main = @import("main.zig");
const base = @import("base");
const eval = @import("eval");
const lir = @import("lir");
const Allocators = base.Allocators;
const cli_context = @import("CliContext.zig");
const CliContext = cli_context.CliContext;
const Io = cli_context.Io;
const test_helpers = eval.test_helpers;

test "platform resolution - basic cli platform" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
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

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    // This should return CliError since we don't have the actual CLI platform installed
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - no platform in file" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    // Create a temporary Roc file without platform specification
    var temp_dir = testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const roc_content =
        \\# Just a simple expression
        \\42 + 58
    ;

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

test "platform resolution - file not found" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    const result = main.resolvePlatformPaths(&ctx, "nonexistent.roc");
    try testing.expectError(error.CliError, result);
}

test "platform resolution - insecure HTTP URL rejected" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    // Create a CLI context for error reporting
    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
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

    var roc_file = temp_dir.dir.createFile("test.roc", .{}) catch unreachable;
    defer roc_file.close();
    roc_file.writeAll(roc_content) catch unreachable;

    const roc_path = try temp_dir.dir.realpathAlloc(allocs.gpa, "test.roc");
    defer allocs.gpa.free(roc_path);

    // Insecure HTTP URLs (not localhost) should fail validation
    const result = main.resolvePlatformPaths(&ctx, roc_path);
    try testing.expectError(error.CliError, result);
}

fn compileRuntimeImageForSharedTest(
    allocator: std.mem.Allocator,
    source: []const u8,
    imports: []const test_helpers.ModuleSource,
) !test_helpers.CompiledTargetProgram {
    return test_helpers.compileProgramForTarget(allocator, .module, source, imports, .native);
}

fn expectRuntimeImageCanBeViewedFromMappedHeader(compiled: *const test_helpers.CompiledTargetProgram) !void {
    const used = compiled.lowered.shm.getUsedSize();
    try testing.expect(used > @sizeOf(lir.RuntimeImage.Header));
    try testing.expect(compiled.lowered.view.root_procs.len > 0);
    try testing.expect(compiled.lowered.view.store.proc_specs.items.len > 0);
    try testing.expect(compiled.lowered.view.layouts.layouts.items.items.len > 0);

    const header = compiled.lowered.runtime_header;
    const child_view = try lir.RuntimeImage.viewMappedImage(header, compiled.lowered.shm.base_ptr, used);
    try testing.expectEqual(lir.RuntimeImage.MAGIC, header.magic);
    try testing.expectEqual(lir.RuntimeImage.FORMAT_VERSION, header.format_version);
    try testing.expectEqual(compiled.lowered.view.root_procs.len, child_view.root_procs.len);
    try testing.expectEqual(compiled.lowered.view.store.proc_specs.items.len, child_view.store.proc_specs.items.len);
    try testing.expectEqual(compiled.lowered.view.layouts.layouts.items.items.len, child_view.layouts.layouts.items.items.len);
}

test "integration - shared memory setup and parsing" {
    var compiled = try compileRuntimeImageForSharedTest(
        testing.allocator,
        "main = || 40 + 2",
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&compiled);
}

test "integration - compilation pipeline for different platforms" {
    var native = try test_helpers.compileProgramForTarget(
        testing.allocator,
        .module,
        "main = || [1, 2, 3]",
        &.{},
        .native,
    );
    defer native.deinit(testing.allocator);

    var wasm32 = try test_helpers.compileProgramForTarget(
        testing.allocator,
        .module,
        "main = || [1, 2, 3]",
        &.{},
        .u32,
    );
    defer wasm32.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&native);
    try expectRuntimeImageCanBeViewedFromMappedHeader(&wasm32);
    try testing.expectEqual(base.target.TargetUsize.native, native.lowered.view.target_usize);
    try testing.expectEqual(base.target.TargetUsize.u32, wasm32.lowered.view.target_usize);
}

test "integration - error handling for non-existent file" {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa_impl.allocator());
    defer allocs.deinit();

    var io = Io.init();
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, .run);
    ctx.initIo();
    defer ctx.deinit();

    try testing.expectError(error.CliError, main.resolvePlatformPaths(&ctx, "does/not/exist.roc"));
}

test "integration - automatic module dependency ordering" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "Leaf", .source = "module [value]\nvalue = 40\n" },
        .{ .name = "Branch", .source = "module [value]\nimport Leaf\nvalue = Leaf.value + 2\n" },
    };
    var compiled = try compileRuntimeImageForSharedTest(
        testing.allocator,
        "import Branch\nmain = || Branch.value",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&compiled);
    try testing.expectEqual(@as(usize, 3), compiled.resources.import_artifacts.len);
}

test "integration - transitive module imports (module A imports module B)" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "B", .source = "module [value]\nvalue = 40\n" },
        .{ .name = "A", .source = "module [value]\nimport B\nvalue = B.value + 2\n" },
    };
    var compiled = try compileRuntimeImageForSharedTest(
        testing.allocator,
        "import A\nmain = || A.value",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&compiled);
    try testing.expectEqual(@as(usize, 3), compiled.resources.import_artifacts.len);
}

test "integration - diamond dependency pattern (A imports B and C, both import D)" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "D", .source = "module [value]\nvalue : {} -> I64\nvalue = |_| 10\n" },
        .{ .name = "B", .source = "module [value]\nimport D\nvalue : {} -> I64\nvalue = |_| D.value({}) + 10\n" },
        .{ .name = "C", .source = "module [value]\nimport D\nvalue : {} -> I64\nvalue = |_| D.value({}) + 12\n" },
        .{ .name = "A", .source = "module [value]\nimport B\nimport C\nvalue : {} -> I64\nvalue = |_| B.value({}) + C.value({})\n" },
    };
    var compiled = try compileRuntimeImageForSharedTest(
        testing.allocator,
        "import A\nmain = || A.value({})",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&compiled);
    try testing.expectEqual(@as(usize, 5), compiled.resources.import_artifacts.len);
}

test "integration - direct Core and Utils calls from app" {
    const imports = [_]test_helpers.ModuleSource{
        .{ .name = "Core", .source = "module [inc]\ninc = |x| x + 1\n" },
        .{ .name = "Utils", .source = "module [double]\ndouble = |x| x * 2\n" },
    };
    var compiled = try compileRuntimeImageForSharedTest(
        testing.allocator,
        "import Core\nimport Utils\nmain = || Utils.double(Core.inc(20))",
        &imports,
    );
    defer compiled.deinit(testing.allocator);

    try expectRuntimeImageCanBeViewedFromMappedHeader(&compiled);
    try testing.expectEqual(@as(usize, 3), compiled.resources.import_artifacts.len);
}
