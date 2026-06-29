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
        .lir_core => &.{"lir core declarations are referenced"},
        .postcheck => &.{"postcheck declarations are referenced"},
        .values => &.{"values tests"},
        .eval => &.{"eval tests"},
        .ipc => &.{"ipc tests"},
        .fmt => &.{"fmt tests"},
        .lsp_unit => &.{"lsp unit tests"},
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
            b.graph.io,
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
    io: std.Io,
    entry: FileToScan,
    pending: *std.ArrayList(FileToScan),
    seen: *std.StringHashMap(void),
    has_aggregators: bool,
) usize {
    const path = entry.path;
    const source = std.Io.Dir.cwd().readFileAllocOptions(
        io,
        path,
        allocator,
        .limited(wrapper_scan_max_bytes),
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

    while (std.mem.findPos(u8, source, search_index, pattern)) |match_pos| {
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
        if (!std.mem.endsWith(u8, import_rel, ".zig")) {
            search_index = cursor + 1;
            continue;
        }

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
    tests: []const ModuleTest,
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
    ctx,
    build_options,
    layout,
    interpreter_layout,
    values,
    interpreter_values,
    eval,
    ipc,
    fmt,
    watch,
    bundle,
    unbundle,
    base58,
    lsp,
    lsp_unit,
    lsp_integration,
    backend,
    lir_core,
    postcheck,
    lir,
    symbol,
    roc_target,
    sljmp,
    echo_platform,
    docs,
    glue,

    /// Returns the dependencies for this module type
    pub fn getDependencies(self: ModuleType) []const ModuleType {
        return switch (self) {
            .build_options => &.{},
            .builtins => &.{.tracy},
            .ctx => &.{},
            .tracy => &.{.build_options},
            .collections => &.{},
            .base => &.{.collections},
            .roc_src => &.{},
            .types => &.{ .tracy, .base, .collections },
            .reporting => &.{ .collections, .base },
            .parse => &.{ .tracy, .collections, .base, .reporting },
            .can => &.{ .tracy, .builtins, .collections, .types, .base, .parse, .reporting, .build_options, .ctx },
            .check => &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .reporting, .build_options },
            .layout => &.{ .tracy, .collections, .base, .types, .builtins, .can },
            .interpreter_layout => &.{ .tracy, .collections, .base, .types, .builtins, .can, .layout },
            .values => &.{ .collections, .base, .builtins, .layout },
            .interpreter_values => &.{ .collections, .base, .builtins, .interpreter_layout },
            .eval => &.{ .tracy, .ctx, .collections, .base, .types, .builtins, .parse, .can, .check, .layout, .interpreter_layout, .values, .interpreter_values, .build_options, .reporting, .backend, .lir, .symbol, .roc_target, .sljmp, .ipc },
            .compile => &.{ .tracy, .build_options, .ctx, .builtins, .collections, .base, .types, .parse, .can, .check, .reporting, .layout, .eval, .unbundle, .roc_target, .backend, .lir, .symbol, .sljmp },
            .ipc => &.{},

            .fmt => &.{ .base, .parse, .collections, .can, .ctx, .tracy },
            .watch => &.{.build_options},
            .bundle => &.{ .base, .collections, .base58, .unbundle },
            .unbundle => &.{ .base, .collections, .base58 },
            .base58 => &.{},
            .lsp => &.{ .compile, .reporting, .build_options, .ctx, .base, .parse, .can, .types, .fmt, .eval, .roc_target },
            .lsp_unit, .lsp_integration => &.{ .lsp, .compile, .reporting, .build_options, .ctx, .base, .parse, .can, .types, .fmt, .eval, .roc_target },
            .backend => &.{ .base, .layout, .builtins, .can, .lir, .roc_target, .ctx },
            .lir_core => &.{ .base, .collections, .layout, .types, .can, .check },
            .postcheck => &.{ .base, .builtins, .can, .check, .layout, .lir_core },
            .lir => &.{ .base, .collections, .layout, .types, .can, .check, .build_options, .lir_core, .postcheck },
            .symbol => &.{.base},
            .roc_target => &.{.base},
            .sljmp => &.{},
            .echo_platform => &.{.builtins},
            .docs => &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .check, .reporting },
            .glue => &.{ .base, .parse, .compile, .can, .check, .reporting, .echo_platform, .builtins, .roc_target, .types, .layout, .backend, .eval, .lir },
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
    ctx: *Module,
    build_options: *Module,
    layout: *Module,
    interpreter_layout: *Module,
    values: *Module,
    interpreter_values: *Module,
    eval: *Module,
    ipc: *Module,
    fmt: *Module,
    watch: *Module,
    bundle: *Module,
    unbundle: *Module,
    base58: *Module,
    lsp: *Module,
    lsp_unit: *Module,
    lsp_integration: *Module,
    backend: *Module,
    lir_core: *Module,
    postcheck: *Module,
    lir: *Module,
    symbol: *Module,
    roc_target: *Module,
    sljmp: *Module,
    echo_platform: *Module,
    docs: *Module,
    glue: *Module,
    embedded_lld: *Module,

    // Vendored-from-Zig modules. Kept out of the `ModuleType` dependency graph
    // (like `embedded_lld`) and wired into their specific consumers via
    // `applyVendorImports`, so it stays clear which code comes from elsewhere.
    // The sources live under `vendor/`.
    vendor_parse_float: *Module,
    vendor_ryu: *Module,
    vendor_eval_loader: *Module,
    vendor_macho: *Module,
    vendor_llvm_ir: *Module,
    vendor_llvm_compile_bindings: *Module,

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
            .ctx = b.addModule("ctx", .{ .root_source_file = b.path("src/ctx/mod.zig") }),
            .build_options = b.addModule(
                "build_options",
                .{ .root_source_file = build_options_step.getOutput() },
            ),
            .layout = b.addModule("layout", .{ .root_source_file = b.path("src/layout/mod.zig") }),
            .interpreter_layout = b.addModule("interpreter_layout", .{ .root_source_file = b.path("src/interpreter_layout/mod.zig") }),
            .values = b.addModule("values", .{ .root_source_file = b.path("src/values/mod.zig") }),
            .interpreter_values = b.addModule("interpreter_values", .{ .root_source_file = b.path("src/eval/interpreter_values.zig") }),
            .eval = b.addModule("eval", .{ .root_source_file = b.path("src/eval/mod.zig") }),
            .ipc = b.addModule("ipc", .{ .root_source_file = b.path("src/ipc/mod.zig") }),
            .fmt = b.addModule("fmt", .{ .root_source_file = b.path("src/fmt/mod.zig") }),
            .watch = b.addModule("watch", .{ .root_source_file = b.path("src/watch/watch.zig") }),
            .bundle = b.addModule("bundle", .{ .root_source_file = b.path("src/bundle/mod.zig") }),
            .unbundle = b.addModule("unbundle", .{ .root_source_file = b.path("src/unbundle/mod.zig") }),
            .base58 = b.addModule("base58", .{ .root_source_file = b.path("src/base58/mod.zig") }),
            .lsp = b.addModule("lsp", .{ .root_source_file = b.path("src/lsp/mod.zig") }),
            .lsp_unit = b.addModule("lsp_unit", .{ .root_source_file = b.path("src/lsp/test/unit.zig") }),
            .lsp_integration = b.addModule("lsp_integration", .{ .root_source_file = b.path("src/lsp/test/integration.zig") }),
            .backend = b.addModule("backend", .{ .root_source_file = b.path("src/backend/mod.zig") }),
            .lir_core = b.addModule("lir_core", .{ .root_source_file = b.path("src/lir/core.zig") }),
            .postcheck = b.addModule("postcheck", .{ .root_source_file = b.path("src/postcheck/mod.zig") }),
            .lir = b.addModule("lir", .{ .root_source_file = b.path("src/lir/mod.zig") }),
            .symbol = b.addModule("symbol", .{ .root_source_file = b.path("src/symbol/mod.zig") }),
            .roc_target = b.addModule("roc_target", .{ .root_source_file = b.path("src/target/mod.zig") }),
            .sljmp = b.addModule("sljmp", .{ .root_source_file = b.path("src/sljmp/mod.zig") }),
            .echo_platform = b.addModule("echo_platform", .{ .root_source_file = b.path("src/echo_platform/mod.zig") }),
            .docs = b.addModule("docs", .{ .root_source_file = b.path("src/docs/mod.zig") }),
            .glue = b.addModule("glue", .{ .root_source_file = b.path("src/glue/mod.zig") }),
            .embedded_lld = b.addModule("embedded_lld", .{ .root_source_file = b.path("src/build/embedded_lld.zig") }),

            .vendor_parse_float = b.addModule("vendor_parse_float", .{ .root_source_file = b.path("vendor/parse_float/parse_float.zig") }),
            .vendor_ryu = b.addModule("vendor_ryu", .{ .root_source_file = b.path("vendor/ryu.zig") }),
            .vendor_eval_loader = b.addModule("vendor_eval_loader", .{ .root_source_file = b.path("vendor/eval_loader.zig") }),
            .vendor_macho = b.addModule("vendor_macho", .{ .root_source_file = b.path("vendor/macho/mod.zig") }),
            .vendor_llvm_ir = b.addModule("vendor_llvm_ir", .{ .root_source_file = b.path("vendor/llvm_ir/mod.zig") }),
            .vendor_llvm_compile_bindings = b.addModule("vendor_llvm_compile_bindings", .{ .root_source_file = b.path("vendor/llvm_compile_bindings.zig") }),
        };

        // Link zstd to bundle module if available (it's unsupported on wasm32, so don't link it)
        // Note: unbundle uses Zig's stdlib zstd for WASM compatibility
        if (zstd) |z| {
            self.bundle.linkLibrary(z.artifact("zstd"));
        }

        // The interpreter's hosted-call trampoline is hand-written assembly (see
        // host_trampoline.S); attach it to the eval module so it is assembled and linked into
        // every artifact that uses the interpreter. The file is arch-guarded, so it compiles
        // to an empty object on targets without a trampoline (e.g. wasm).
        self.eval.addAssemblyFile(b.path("src/eval/host_trampoline.S"));

        // Setup module dependencies using our generic helper
        self.setupModuleDependencies();

        // `embedded_lld` is created outside the dependency table above; it only
        // needs `collections` for the single-threaded arena.
        self.embedded_lld.addImport("collections", self.collections);

        // The vendored ELF loader reaches one roc helper (`elf_self_relocate`)
        // through the `base` module.
        self.vendor_eval_loader.addImport("base", self.base);

        // The vendored Mach-O code-signing helpers use the build-time `tracy`
        // tracing shim.
        self.vendor_macho.addImport("tracy", self.tracy);

        // The vendored LLVM IR library's BitcodeReader reaches roc's `base`.
        self.vendor_llvm_ir.addImport("base", self.base);

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
            .ctx,
            .build_options,
            .layout,
            .interpreter_layout,
            .values,
            .interpreter_values,
            .eval,
            .ipc,
            .fmt,
            .watch,
            .bundle,
            .unbundle,
            .base58,
            .lsp,
            .lsp_unit,
            .lsp_integration,
            .backend,
            .lir_core,
            .postcheck,
            .lir,
            .symbol,
            .roc_target,
            .sljmp,
            .echo_platform,
            .docs,
            .glue,
        };

        // Setup dependencies for each module
        for (all_modules) |module_type| {
            const module = self.getModule(module_type);
            const dependencies = module_type.getDependencies();

            for (dependencies) |dep_type| {
                const dep_module = self.getModule(dep_type);
                module.addImport(@tagName(dep_type), dep_module);
            }

            self.applyVendorImports(module, module_type);
        }
    }

    /// Wire vendored-from-Zig modules into the specific roc modules that use
    /// them. Called for both the persistent modules and the per-module test
    /// builds, so a `@import("vendor_x")` resolves in both. Vendored modules
    /// are deliberately not part of the `ModuleType` dependency graph.
    fn applyVendorImports(self: RocModules, module: *Module, module_type: ModuleType) void {
        switch (module_type) {
            .builtins => {
                module.addImport("vendor_parse_float", self.vendor_parse_float);
                module.addImport("vendor_ryu", self.vendor_ryu);
            },
            .eval => {
                module.addImport("vendor_eval_loader", self.vendor_eval_loader);
            },
            else => {},
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
        step.root_module.addImport("ctx", self.ctx);
        step.root_module.addImport("build_options", self.build_options);
        step.root_module.addImport("layout", self.layout);
        step.root_module.addImport("interpreter_layout", self.interpreter_layout);
        step.root_module.addImport("eval", self.eval);
        step.root_module.addImport("fmt", self.fmt);
        step.root_module.addImport("unbundle", self.unbundle);
        step.root_module.addImport("base58", self.base58);
        step.root_module.addImport("roc_target", self.roc_target);
        step.root_module.addImport("backend", self.backend);
        step.root_module.addImport("lir_core", self.lir_core);
        step.root_module.addImport("postcheck", self.postcheck);
        step.root_module.addImport("lir", self.lir);
        step.root_module.addImport("symbol", self.symbol);
        step.root_module.addImport("sljmp", self.sljmp);
        step.root_module.addImport("echo_platform", self.echo_platform);
        step.root_module.addImport("docs", self.docs);
        step.root_module.addImport("glue", self.glue);
        step.root_module.addImport("compile", self.compile);
        step.root_module.addImport("embedded_lld", self.embedded_lld);

        // Vendored, used by the CLI linker (Mach-O code signing). Harmless where
        // unused (it is only @import-ed from CLI code, never from wasm).
        step.root_module.addImport("vendor_macho", self.vendor_macho);

        // Don't add thread-dependent or native-only modules for WASM targets
        if (!is_wasm) {
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
            .ctx => self.ctx,
            .build_options => self.build_options,
            .layout => self.layout,
            .interpreter_layout => self.interpreter_layout,
            .values => self.values,
            .interpreter_values => self.interpreter_values,
            .eval => self.eval,
            .ipc => self.ipc,
            .fmt => self.fmt,
            .watch => self.watch,
            .bundle => self.bundle,
            .unbundle => self.unbundle,
            .base58 => self.base58,
            .lsp => self.lsp,
            .lsp_unit => self.lsp_unit,
            .lsp_integration => self.lsp_integration,
            .backend => self.backend,
            .lir_core => self.lir_core,
            .postcheck => self.postcheck,
            .lir => self.lir,
            .symbol => self.symbol,
            .roc_target => self.roc_target,
            .sljmp => self.sljmp,
            .echo_platform => self.echo_platform,
            .docs => self.docs,
            .glue => self.glue,
        };
    }

    /// Add dependencies for a specific module type to a compile step
    pub fn addModuleDependencies(self: RocModules, step: *Step.Compile, module_type: ModuleType) void {
        const dependencies = module_type.getDependencies();
        for (dependencies) |dep_type| {
            const dep_module = self.getModule(dep_type);
            step.root_module.addImport(@tagName(dep_type), dep_module);
        }
        self.applyVendorImports(step.root_module, module_type);
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
            .ctx,
            .layout,
            .interpreter_layout,
            .values,
            .ipc,
            .fmt,
            .watch,
            .bundle,
            .unbundle,
            .base58,
            .lsp_unit,
            .backend,
            .lir_core,
            .postcheck,
            .lir,
            .symbol,
            .sljmp,
            .echo_platform,
            .docs,
        };

        const tests = b.allocator.alloc(ModuleTest, test_configs.len) catch
            @panic("OOM while creating module tests");
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
                    // Zig 0.16 requires explicit link_libc on any compile unit that references
                    // std.c.* (directly or transitively). Our modules use std.c in multiple
                    // places — stack_overflow, CoreCtx, ExecutableMemory, channel.nanosleep,
                    // download.getaddrinfo, server.zig, etc. — and most of the remaining
                    // modules import ctx/unbundle transitively. It's simpler (and has no
                    // practical cost for native-only tests) to enable link_libc uniformly.
                    .link_libc = true,
                }),
                .filters = filter_injection.filters,
            });

            // Watch module needs Core Foundation and FSEvents on macOS (only when not cross-compiling)
            // These frameworks provide the FSEvents API for proper event-driven file system monitoring on macOS.
            if (module_type == .watch and target.result.os.tag == .macos and targetMatchesHost(target)) {
                test_step.root_module.linkFramework("CoreFoundation", .{});
                test_step.root_module.linkFramework("CoreServices", .{});
            }

            // Add only the necessary dependencies for each module test
            self.addModuleDependencies(test_step, module_type);

            // Link zstd for bundle module (unbundle uses stdlib zstd)
            if (module_type == .bundle) {
                if (zstd) |z| {
                    test_step.root_module.linkLibrary(z.artifact("zstd"));
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
