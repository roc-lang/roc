//! Build system utilities for configuring Zig modules with test filtering and dependency management.

const std = @import("std");
const builtin = @import("builtin");
const Build = std.Build;
const Module = Build.Module;
const Step = Build.Step;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;
const Dependency = std.Build.Dependency;

const FilterInjection = struct {
    filters: []const []const u8,
    forced_count: usize,
};

const wrapper_scan_max_bytes = 16 * 1024 * 1024;

fn filtersContain(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

fn aggregatorFilters(module_type: ModuleType) []const []const u8 {
    return switch (module_type) {
        .base => &.{"base tests"},
        .collections => &.{"collections tests"},
        .builtins => &.{"builtins tests"},
        .compile => &.{"compile tests"},
        .can => &.{"compile tests"},
        .check => &.{"check tests"},
        .parse => &.{"parser tests"},
        .layout => &.{"layout tests"},
        .eval => &.{"eval tests"},
        .ipc => &.{"ipc tests"},
        .repl => &.{"repl tests"},
        .fmt => &.{"fmt tests"},
        .mir => &.{"mir tests"},
        else => &.{},
    };
}

const FileToScan = struct {
    path: []const u8,
    include_imports: bool,
};

// Count `test { ... }` blocks (no names) so filtered runs can subtract the
// wrappers they inevitably execute even when Zig test filters are set.
fn wrapperTestCount(b: *Build, module_type: ModuleType, module: *Module) usize {
    const lazy_path = module.root_source_file orelse return 0;
    const root_file_path = lazy_path.getPath(b);
    const aggregator_names = aggregatorFilters(module_type);
    const has_aggregators = aggregator_names.len != 0;

    var arena = std.heap.ArenaAllocator.init(b.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pending = std.ArrayList(FileToScan).empty;
    defer pending.deinit(allocator);
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    const root_copy = allocator.dupe(u8, root_file_path) catch @panic("OOM");
    pending.append(allocator, .{
        .path = root_copy,
        .include_imports = has_aggregators,
    }) catch @panic("OOM");
    seen.put(root_copy, {}) catch @panic("OOM");

    var total: usize = 0;
    while (pending.items.len != 0) {
        const entry = pending.items[pending.items.len - 1];
        pending.items.len -= 1;
        total += scanFileForWrappers(
            allocator,
            entry,
            &pending,
            &seen,
            has_aggregators,
        );
    }

    return total;
}

fn scanFileForWrappers(
    allocator: std.mem.Allocator,
    entry: FileToScan,
    pending: *std.ArrayList(FileToScan),
    seen: *std.StringHashMap(void),
    has_aggregators: bool,
) usize {
    const path = entry.path;
    const source = std.fs.cwd().readFileAllocOptions(
        allocator,
        path,
        wrapper_scan_max_bytes,
        null,
        .@"1",
        0,
    ) catch |err| {
        std.log.warn(
            "Failed to read {s} while counting unnamed tests: {s}",
            .{ path, @errorName(err) },
        );
        return 0;
    };

    var tree = std.zig.Ast.parse(allocator, source, .zig) catch |err| {
        std.log.warn(
            "Failed to parse {s} while counting unnamed tests: {s}",
            .{ path, @errorName(err) },
        );
        return 0;
    };
    defer tree.deinit(allocator);

    const tags = tree.nodes.items(.tag);
    const all_data = tree.nodes.items(.data);

    var unnamed: usize = 0;
    for (tags, all_data) |tag, data| {
        if (tag == .test_decl and data.opt_token_and_node[0] == .none) {
            unnamed += 1;
        }
    }

    if (entry.include_imports and has_aggregators) {
        collectAggregatorImports(allocator, source, path, pending, seen);
    }

    return unnamed;
}

fn collectAggregatorImports(
    allocator: std.mem.Allocator,
    source: []const u8,
    current_path: []const u8,
    pending: *std.ArrayList(FileToScan),
    seen: *std.StringHashMap(void),
) void {
    const pattern = "std.testing.refAllDecls(@import(\"";
    var search_index: usize = 0;
    const current_dir = std.fs.path.dirname(current_path) orelse ".";

    while (std.mem.indexOfPos(u8, source, search_index, pattern)) |match_pos| {
        const literal_start = match_pos + pattern.len;
        var cursor = literal_start;
        while (cursor < source.len) : (cursor += 1) {
            if (source[cursor] == '\\') {
                cursor += 1;
                continue;
            }
            if (source[cursor] == '"') break;
        }
        if (cursor >= source.len) break;

        const literal_bytes = source[literal_start..cursor];
        const quoted = std.fmt.allocPrint(allocator, "\"{s}\"", .{literal_bytes}) catch break;
        const import_rel = std.zig.string_literal.parseAlloc(allocator, quoted) catch |err| {
            std.log.warn(
                "Failed to parse aggregator import in {s}: {s}",
                .{ current_path, @errorName(err) },
            );
            search_index = cursor + 1;
            continue;
        };

        const resolved = resolveImportPath(allocator, current_dir, import_rel) catch |err| {
            std.log.warn(
                "Failed to resolve aggregator import {s} from {s}: {s}",
                .{ import_rel, current_path, @errorName(err) },
            );
            search_index = cursor + 1;
            continue;
        };

        if (seen.contains(resolved)) {
            search_index = cursor + 1;
            continue;
        }

        seen.put(resolved, {}) catch @panic("OOM");
        pending.append(allocator, .{
            .path = resolved,
            .include_imports = false,
        }) catch @panic("OOM");

        search_index = cursor + 1;
    }
}

fn resolveImportPath(
    allocator: std.mem.Allocator,
    current_dir: []const u8,
    import_rel: []const u8,
) ![]const u8 {
    if (std.fs.path.isAbsolute(import_rel)) {
        return std.fs.path.resolve(allocator, &.{import_rel});
    }
    return std.fs.path.resolve(allocator, &.{ current_dir, import_rel });
}

// Keep module-level aggregator tests (e.g. "check tests") when user passes
// a filter for an inner test: we must still run the aggregator so that
// std.testing.refAllDecls brings the inner test into the build.
fn ensureAggregatorFilters(
    b: *Build,
    module_type: ModuleType,
    base_filters: []const []const u8,
) FilterInjection {
    if (base_filters.len == 0) {
        return .{ .filters = base_filters, .forced_count = 0 };
    }

    const aggregators = aggregatorFilters(module_type);
    if (aggregators.len == 0) {
        return .{ .filters = base_filters, .forced_count = 0 };
    }

    var missing: usize = 0;
    for (aggregators) |agg| {
        if (!filtersContain(base_filters, agg)) {
            missing += 1;
        }
    }
    if (missing == 0) {
        return .{ .filters = base_filters, .forced_count = 0 };
    }

    const combined = b.allocator.alloc([]const u8, base_filters.len + missing) catch
        @panic("OOM while applying aggregator filters");
    for (combined[0..base_filters.len], base_filters) |*dest, src| {
        dest.* = src;
    }

    var next = base_filters.len;
    var added: usize = 0;
    for (aggregators) |agg| {
        if (!filtersContain(base_filters, agg)) {
            combined[next] = agg;
            next += 1;
            added += 1;
        }
    }

    return .{
        .filters = combined,
        .forced_count = added,
    };
}

fn targetMatchesHost(target: ResolvedTarget) bool {
    return target.result.os.tag == builtin.target.os.tag and
        target.result.cpu.arch == builtin.target.cpu.arch and
        target.result.abi == builtin.target.abi;
}

/// Represents a test module with its compilation and execution steps.
pub const ModuleTest = struct {
    test_step: *Step.Compile,
    run_step: *Step.Run,
};

/// Bundles the per-module test steps with accounting for forced passes (aggregators +
/// unnamed wrappers) so callers can correct the reported totals.
pub const ModuleTestsResult = struct {
    /// Compile/run steps for each module's tests, in creation order.
    tests: [25]ModuleTest,
    /// Number of synthetic passes the summary must subtract when filters were injected.
    /// Includes aggregator ensures and unconditional wrapper tests.
    forced_passes: usize,
};

/// Enumerates the different modules in the Roc compiler codebase.
pub const ModuleType = enum {
    collections,
    base,
    roc_src,
    types,
    builtins,
    compile,
    reporting,
    parse,
    can,
    check,
    tracy,
    fs,
    build_options,
    layout,
    eval,
    ipc,
    repl,
    fmt,
    watch,
    bundle,
    unbundle,
    base58,
    lsp,
    backend,
    mir,
    lir,
    mono,
    roc_target,
    sljmp,

    /// Returns the dependencies for this module type
    pub fn getDependencies(self: ModuleType) []const ModuleType {
        return switch (self) {
            .build_options => &.{},
            .builtins => &.{.tracy},
            .fs => &.{},
            .tracy => &.{.build_options},
            .collections => &.{},
            .base => &.{ .collections, .builtins },
            .roc_src => &.{},
            .types => &.{ .tracy, .base, .collections },
            .reporting => &.{ .collections, .base },
            .parse => &.{ .tracy, .collections, .base, .reporting },
            .can => &.{ .tracy, .builtins, .collections, .types, .base, .parse, .reporting, .build_options },
            .check => &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .reporting },
            .layout => &.{ .tracy, .collections, .base, .types, .builtins, .can },
            .eval => &.{ .tracy, .collections, .base, .types, .builtins, .parse, .can, .check, .layout, .build_options, .reporting, .backend, .mono, .roc_target, .sljmp },
            .compile => &.{ .tracy, .build_options, .fs, .builtins, .collections, .base, .types, .parse, .can, .check, .reporting, .layout, .eval, .unbundle, .roc_target },
            .ipc => &.{},
            .repl => &.{ .base, .collections, .compile, .parse, .types, .can, .check, .builtins, .layout, .eval, .backend, .roc_target },
            .fmt => &.{ .base, .parse, .collections, .can, .fs, .tracy },
            .watch => &.{.build_options},
            .bundle => &.{ .base, .collections, .base58, .unbundle },
            .unbundle => &.{ .base, .collections, .base58 },
            .base58 => &.{},
            .lsp => &.{ .compile, .reporting, .build_options, .fs, .base, .parse, .can, .types, .fmt, .roc_target },
            .backend => &.{ .base, .layout, .builtins, .can, .mono, .lir, .roc_target },
            .mir => &.{ .base, .can, .types, .builtins, .parse, .check, .collections, .reporting, .build_options, .tracy },
            .lir => &.{ .base, .layout, .types, .mir, .can },
            .mono => &.{ .base, .layout, .can, .types, .mir },
            .roc_target => &.{.base},
            .sljmp => &.{},
        };
    }
};

/// Manages all Roc compiler modules and their dependencies
pub const RocModules = struct {
    collections: *Module,
    base: *Module,
    roc_src: *Module,
    types: *Module,
    builtins: *Module,
    compile: *Module,
    reporting: *Module,
    parse: *Module,
    can: *Module,
    check: *Module,
    tracy: *Module,
    fs: *Module,
    build_options: *Module,
    layout: *Module,
    eval: *Module,
    ipc: *Module,
    repl: *Module,
    fmt: *Module,
    watch: *Module,
    bundle: *Module,
    unbundle: *Module,
    base58: *Module,
    lsp: *Module,
    backend: *Module,
    mir: *Module,
    lir: *Module,
    mono: *Module,
    roc_target: *Module,
    sljmp: *Module,

    pub fn create(b: *Build, build_options_step: *Step.Options, zstd: ?*Dependency) RocModules {
        const self = RocModules{
            .collections = b.addModule(
                "collections",
                .{ .root_source_file = b.path("src/collections/mod.zig") },
            ),
            .base = b.addModule("base", .{ .root_source_file = b.path("src/base/mod.zig") }),
            .roc_src = b.addModule("roc_src", .{ .root_source_file = b.path("src/roc_src/mod.zig") }),
            .types = b.addModule("types", .{ .root_source_file = b.path("src/types/mod.zig") }),
            .builtins = b.addModule("builtins", .{ .root_source_file = b.path("src/builtins/mod.zig") }),
            .compile = b.addModule("compile", .{ .root_source_file = b.path("src/compile/mod.zig") }),
            .reporting = b.addModule("reporting", .{ .root_source_file = b.path("src/reporting/mod.zig") }),
            .parse = b.addModule("parse", .{ .root_source_file = b.path("src/parse/mod.zig") }),
            .can = b.addModule("can", .{ .root_source_file = b.path("src/canonicalize/mod.zig") }),
            .check = b.addModule("check", .{ .root_source_file = b.path("src/check/mod.zig") }),
            .tracy = b.addModule("tracy", .{ .root_source_file = b.path("src/build/tracy.zig") }),
            .fs = b.addModule("fs", .{ .root_source_file = b.path("src/fs/mod.zig") }),
            .build_options = b.addModule(
                "build_options",
                .{ .root_source_file = build_options_step.getOutput() },
            ),
            .layout = b.addModule("layout", .{ .root_source_file = b.path("src/layout/mod.zig") }),
            .eval = b.addModule("eval", .{ .root_source_file = b.path("src/eval/mod.zig") }),
            .ipc = b.addModule("ipc", .{ .root_source_file = b.path("src/ipc/mod.zig") }),
            .repl = b.addModule("repl", .{ .root_source_file = b.path("src/repl/mod.zig") }),
            .fmt = b.addModule("fmt", .{ .root_source_file = b.path("src/fmt/mod.zig") }),
            .watch = b.addModule("watch", .{ .root_source_file = b.path("src/watch/watch.zig") }),
            .bundle = b.addModule("bundle", .{ .root_source_file = b.path("src/bundle/mod.zig") }),
            .unbundle = b.addModule("unbundle", .{ .root_source_file = b.path("src/unbundle/mod.zig") }),
            .base58 = b.addModule("base58", .{ .root_source_file = b.path("src/base58/mod.zig") }),
            .lsp = b.addModule("lsp", .{ .root_source_file = b.path("src/lsp/mod.zig") }),
            .backend = b.addModule("backend", .{ .root_source_file = b.path("src/backend/mod.zig") }),
            .mir = b.addModule("mir", .{ .root_source_file = b.path("src/mir/mod.zig") }),
            .lir = b.addModule("lir", .{ .root_source_file = b.path("src/lir/mod.zig") }),
            .mono = b.addModule("mono", .{ .root_source_file = b.path("src/mono/mod.zig") }),
            .roc_target = b.addModule("roc_target", .{ .root_source_file = b.path("src/target/mod.zig") }),
            .sljmp = b.addModule("sljmp", .{ .root_source_file = b.path("src/sljmp/mod.zig") }),
        };

        // Link zstd to bundle module if available (it's unsupported on wasm32, so don't link it)
        // Note: unbundle uses Zig's stdlib zstd for WASM compatibility
        if (zstd) |z| {
            self.bundle.linkLibrary(z.artifact("zstd"));
        }

        // Setup module dependencies using our generic helper
        self.setupModuleDependencies();

        return self;
    }

    fn setupModuleDependencies(self: RocModules) void {
        const all_modules = [_]ModuleType{
            .collections,
            .base,
            .types,
            .builtins,
            .compile,
            .reporting,
            .parse,
            .can,
            .check,
            .tracy,
            .fs,
            .build_options,
            .layout,
            .eval,
            .ipc,
            .repl,
            .fmt,
            .watch,
            .bundle,
            .unbundle,
            .base58,
            .lsp,
            .backend,
            .mir,
            .lir,
            .mono,
            .roc_target,
            .sljmp,
        };

        // Setup dependencies for each module
        for (all_modules) |module_type| {
            const module = self.getModule(module_type);
            const dependencies = module_type.getDependencies();

            for (dependencies) |dep_type| {
                const dep_module = self.getModule(dep_type);
                module.addImport(@tagName(dep_type), dep_module);
            }
        }
    }

    pub fn addAll(self: RocModules, step: *Step.Compile) void {
        const is_wasm = step.rootModuleTarget().cpu.arch == .wasm32;

        step.root_module.addImport("base", self.base);
        step.root_module.addImport("collections", self.collections);
        step.root_module.addImport("types", self.types);
        step.root_module.addImport("reporting", self.reporting);
        step.root_module.addImport("parse", self.parse);
        step.root_module.addImport("can", self.can);
        step.root_module.addImport("check", self.check);
        step.root_module.addImport("tracy", self.tracy);
        step.root_module.addImport("builtins", self.builtins);
        step.root_module.addImport("fs", self.fs);
        step.root_module.addImport("build_options", self.build_options);
        step.root_module.addImport("layout", self.layout);
        step.root_module.addImport("eval", self.eval);
        step.root_module.addImport("repl", self.repl);
        step.root_module.addImport("fmt", self.fmt);
        step.root_module.addImport("unbundle", self.unbundle);
        step.root_module.addImport("base58", self.base58);
        step.root_module.addImport("roc_target", self.roc_target);
        step.root_module.addImport("backend", self.backend);
        step.root_module.addImport("mir", self.mir);
        step.root_module.addImport("lir", self.lir);
        step.root_module.addImport("mono", self.mono);

        // Don't add thread-dependent modules for WASM targets (threads not supported)
        if (!is_wasm) {
            step.root_module.addImport("compile", self.compile);
            step.root_module.addImport("ipc", self.ipc);
            step.root_module.addImport("watch", self.watch);
            step.root_module.addImport("lsp", self.lsp);
            // Don't add bundle module for WASM targets (zstd C library not available)
            step.root_module.addImport("bundle", self.bundle);
        }
    }

    pub fn addAllToTest(self: RocModules, step: *Step.Compile) void {
        self.addAll(step);
    }

    /// Get a module by its type
    pub fn getModule(self: RocModules, module_type: ModuleType) *Module {
        return switch (module_type) {
            .collections => self.collections,
            .base => self.base,
            .roc_src => self.roc_src,
            .types => self.types,
            .builtins => self.builtins,
            .compile => self.compile,
            .reporting => self.reporting,
            .parse => self.parse,
            .can => self.can,
            .check => self.check,
            .tracy => self.tracy,
            .fs => self.fs,
            .build_options => self.build_options,
            .layout => self.layout,
            .eval => self.eval,
            .ipc => self.ipc,
            .repl => self.repl,
            .fmt => self.fmt,
            .watch => self.watch,
            .bundle => self.bundle,
            .unbundle => self.unbundle,
            .base58 => self.base58,
            .lsp => self.lsp,
            .backend => self.backend,
            .mir => self.mir,
            .lir => self.lir,
            .mono => self.mono,
            .roc_target => self.roc_target,
            .sljmp => self.sljmp,
        };
    }

    /// Add dependencies for a specific module type to a compile step
    pub fn addModuleDependencies(self: RocModules, step: *Step.Compile, module_type: ModuleType) void {
        const dependencies = module_type.getDependencies();
        for (dependencies) |dep_type| {
            const dep_module = self.getModule(dep_type);
            step.root_module.addImport(@tagName(dep_type), dep_module);
        }
    }

    pub fn createModuleTests(
        self: RocModules,
        b: *Build,
        target: ResolvedTarget,
        optimize: OptimizeMode,
        zstd: ?*Dependency,
        test_filters: []const []const u8,
    ) ModuleTestsResult {
        const test_configs = [_]ModuleType{
            .collections,
            .base,
            .types,
            .builtins,
            .compile,
            .reporting,
            .parse,
            .can,
            .check,
            .fs,
            .layout,
            .eval,
            .ipc,
            .repl,
            .fmt,
            .watch,
            .bundle,
            .unbundle,
            .base58,
            .lsp,
            .backend,
            .mir,
            .lir,
            .mono,
            .sljmp,
        };

        var tests: [test_configs.len]ModuleTest = undefined;
        var forced_passes: usize = 0;

        inline for (test_configs, 0..) |module_type, i| {
            const module = self.getModule(module_type);
            const filter_injection = ensureAggregatorFilters(b, module_type, test_filters);
            forced_passes += filter_injection.forced_count;
            if (test_filters.len != 0) {
                const wrappers = wrapperTestCount(b, module_type, module);
                forced_passes += wrappers;
            }
            const test_step = b.addTest(.{
                .name = b.fmt("{s}", .{@tagName(module_type)}),
                .root_module = b.createModule(.{
                    .root_source_file = module.root_source_file.?,
                    .target = target,
                    .optimize = optimize,
                    // IPC module needs libc for mmap, munmap, close on POSIX systems
                    // Bundle module needs libc for C zstd (unbundle uses stdlib zstd)
                    // Eval/repl modules need libc for setjmp/longjmp crash protection
                    // sljmp module needs libc for setjmp/longjmp functions
                    .link_libc = (module_type == .ipc or module_type == .bundle or module_type == .eval or module_type == .repl or module_type == .sljmp),
                }),
                .filters = filter_injection.filters,
            });

            // Watch module needs Core Foundation and FSEvents on macOS (only when not cross-compiling)
            // These frameworks provide the FSEvents API for proper event-driven file system monitoring on macOS.
            if (module_type == .watch and target.result.os.tag == .macos and targetMatchesHost(target)) {
                test_step.linkFramework("CoreFoundation");
                test_step.linkFramework("CoreServices");
            }

            // Add only the necessary dependencies for each module test
            self.addModuleDependencies(test_step, module_type);

            // Link zstd for bundle module (unbundle uses stdlib zstd)
            if (module_type == .bundle) {
                if (zstd) |z| {
                    test_step.linkLibrary(z.artifact("zstd"));
                }
            }

            const run_step = b.addRunArtifact(test_step);

            tests[i] = .{
                .test_step = test_step,
                .run_step = run_step,
            };
        }

        return .{
            .tests = tests,
            .forced_passes = forced_passes,
        };
    }
};
