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

const FileToScan = struct {
    path: []const u8,
    include_imports: bool,
};

const glue_platform_files = [_][]const u8{
    "AbiFieldLayout.roc",
    "AbiLayout.roc",
    "AbiLayoutDetails.roc",
    "AbiRecordLayout.roc",
    "AbiTagLayout.roc",
    "AbiTagUnionLayout.roc",
    "AbiWidth.roc",
    "ArgShape.roc",
    "CallableSignature.roc",
    "File.roc",
    "FunctionInfo.roc",
    "FunctionSignature.roc",
    "GlueInput.roc",
    "HostRcPlan.roc",
    "HostedFunctionInfo.roc",
    "ModuleTypeInfo.roc",
    "ProvidedExport.roc",
    "ProvidesEntry.roc",
    "RecordField.roc",
    "RecordFieldInfo.roc",
    "RecordRepr.roc",
    "RocName.roc",
    "TagUnionRepr.roc",
    "TagVariant.roc",
    "TypeId.roc",
    "TypeInfo.roc",
    "TypeNamePlan.roc",
    "TypeRepr.roc",
    "TypeTable.roc",
    "Types.roc",
    "main.roc",
};

fn createCompilerPlatformSourcesModule(b: *Build) *Module {
    const write_files = b.addWriteFiles();
    var source = std.ArrayList(u8).empty;
    source.appendSlice(b.allocator,
        \\pub const File = struct {
        \\    path: []const u8,
        \\    bytes: []const u8,
        \\};
        \\
        \\pub const glue_files = [_]File{
        \\
    ) catch @panic("OOM");

    for (glue_platform_files) |file_name| {
        const source_path = b.fmt("src/glue/platform/{s}", .{file_name});
        const embed_path = b.fmt("glue-platform/{s}", .{file_name});
        _ = write_files.addCopyFile(b.path(source_path), embed_path);
        source.appendSlice(
            b.allocator,
            b.fmt("    .{{ .path = \"{s}\", .bytes = @embedFile(\"{s}\") }},\n", .{ file_name, embed_path }),
        ) catch @panic("OOM");
    }

    source.appendSlice(b.allocator,
        \\};
        \\
    ) catch @panic("OOM");

    const generated_source = write_files.add("compiler_platform_sources.zig", source.items);
    return b.createModule(.{ .root_source_file = generated_source });
}

// Count `test { ... }` blocks (no names) so filtered runs can subtract the
// wrappers they inevitably execute even when Zig test filters are set.
fn wrapperTestCount(b: *Build, module_type: ModuleType, module: *Module) usize {
    const lazy_path = module.root_source_file orelse return 0;
    const root_file_path = lazy_path.getPath(b);
    const aggregator_names = module_type.aggregators();
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

    const aggregators = module_type.aggregators();
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

/// Represents a test module's compilation step.
///
/// Deliberately does not carry a pre-made `Step.Run`. Every run of these
/// binaries is created by `TestSuiteRegistry` in build.zig, which builds one
/// run for the tests summary and a separate one for the public
/// `run-test-zig-module-*` step. A ready-made run stored here is exactly the
/// convenient thing to hand to `TestsSummaryStep.addRun` *and* to a public
/// step -- which is how eleven other suites ended up sharing one run and, on
/// Windows, dragging the whole serialization chain behind them.
pub const ModuleTest = struct {
    test_step: *Step.Compile,
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

/// Whether a module's tests run, and where.
pub const Tests = union(enum) {
    /// The module's root file is compiled as a Zig test binary behind a
    /// `run-test-zig-module-<name>` step.
    unit: struct {
        /// The test blocks that bring the rest of the module's tests into the
        /// build through `std.testing.refAllDecls`. A filtered run keeps them,
        /// so the inner tests the filter names still get compiled.
        aggregators: []const []const u8 = &.{},
        /// Whether MiniCI runs the step. A step it does not run is covered
        /// only by the nightly `run-test-zig` aggregate.
        minici: bool = true,
    },
    /// The module is the spec list of a test runner that build.zig wires to
    /// `run-test-zig-module-<name>` itself.
    harness,
    /// The module has no test step, for the stated reason.
    no_tests: []const u8,
};

/// Which compiler binaries import a module through `RocModules.addAll`.
pub const Availability = enum {
    /// Every binary.
    all,
    /// Every binary except wasm32 ones, which have no threads and cannot link
    /// the zstd C library.
    native,
    /// None: only the modules that name it as a dependency import it.
    dependents_only,
};

/// Everything the build knows about one compiler module. The one table of
/// these, `ModuleType.info`, is what module creation, dependency wiring,
/// `RocModules.addAll`, the unit-test steps, and MiniCI's module jobs all
/// read, each in `ModuleType` declaration order.
pub const ModuleInfo = struct {
    /// The module's root source file. Null only for `build_options`, whose
    /// source the build generates.
    root: ?[]const u8,
    /// The compiler modules it imports, each under its own name.
    deps: []const ModuleType = &.{},
    /// The modules outside this table it imports: fields of `RocModules`
    /// vendored from Zig or shared with code built without the compiler.
    extra_imports: []const []const u8 = &.{},
    tests: Tests,
    available: Availability = .all,
};

/// Enumerates the different modules in the Roc compiler codebase.
///
/// The declaration order is the order `RocModules.addAll` imports them in,
/// which is the order their `--dep` arguments reach the compiler.
pub const ModuleType = enum {
    base,
    collections,
    types,
    reporting,
    parse,
    can,
    check,
    tracy,
    builtins,
    ctx,
    build_options,
    layout,
    static_data,
    eval,
    fmt,
    unbundle,
    base58,
    roc_target,
    backend,
    lir_core,
    postcheck,
    lir,
    symbol,
    sljmp,
    roc_args,
    echo_platform,
    docs,
    bump,
    glue,
    compile,
    ipc,
    watch,
    lsp,
    bundle,
    roc_src,
    lsp_unit,
    lsp_integration,
    host_alloc,

    /// The build's facts about this module. Adding a module means adding its
    /// arm here and its field to `RocModules`; both are checked at comptime.
    pub fn info(self: ModuleType) ModuleInfo {
        return switch (self) {
            .base => .{
                .root = "src/base/mod.zig",
                .deps = &.{ .collections, .builtins },
                .tests = .{ .unit = .{ .aggregators = &.{"base tests"} } },
            },
            .collections => .{
                .root = "src/collections/mod.zig",
                .tests = .{ .unit = .{ .aggregators = &.{"collections tests"} } },
            },
            .types => .{
                .root = "src/types/mod.zig",
                .deps = &.{ .tracy, .base, .collections },
                .tests = .{ .unit = .{} },
            },
            .reporting => .{
                .root = "src/reporting/mod.zig",
                .deps = &.{ .collections, .base },
                .tests = .{ .unit = .{} },
            },
            .parse => .{
                .root = "src/parse/mod.zig",
                .deps = &.{ .tracy, .collections, .base, .reporting },
                .tests = .{ .unit = .{ .aggregators = &.{"parser tests"} } },
            },
            .can => .{
                .root = "src/canonicalize/mod.zig",
                .deps = &.{ .tracy, .builtins, .collections, .types, .base, .parse, .reporting, .build_options, .ctx },
                .tests = .{ .unit = .{ .aggregators = &.{"compile tests"} } },
            },
            .check => .{
                .root = "src/check/mod.zig",
                .deps = &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .reporting, .build_options, .fmt },
                .tests = .{ .unit = .{ .aggregators = &.{"check tests"} } },
            },
            .tracy => .{
                .root = "src/build/tracy.zig",
                .deps = &.{.build_options},
                .tests = .{ .no_tests = "a tracing shim with no test blocks" },
            },
            .builtins => .{
                .root = "src/builtins/mod.zig",
                .deps = &.{.tracy},
                // `roc_str_view` lets a `builtins` test assert that the
                // default-platform `RocStr` view matches the canonical `RocStr`.
                .extra_imports = &.{ "vendor_parse_float", "vendor_ryu", "roc_str_view" },
                .tests = .{ .unit = .{ .aggregators = &.{"builtins tests"} } },
            },
            .ctx => .{
                .root = "src/ctx/mod.zig",
                .tests = .{ .unit = .{} },
            },
            .build_options => .{
                .root = null,
                .tests = .{ .no_tests = "generated constants" },
            },
            .layout => .{
                .root = "src/layout/mod.zig",
                .deps = &.{ .tracy, .collections, .base, .types, .builtins, .can },
                .tests = .{ .unit = .{ .aggregators = &.{"layout tests"} } },
            },
            .static_data => .{
                .root = "src/static_data.zig",
                .deps = &.{ .base, .builtins, .check, .collections, .layout, .lir, .roc_target },
                .tests = .{ .unit = .{} },
            },
            .eval => .{
                .root = "src/eval/mod.zig",
                .deps = &.{ .tracy, .ctx, .collections, .base, .types, .builtins, .parse, .can, .check, .layout, .static_data, .build_options, .reporting, .backend, .lir, .symbol, .roc_target, .sljmp, .ipc },
                .extra_imports = &.{"vendor_relocatable_loader"},
                .tests = .{ .unit = .{ .aggregators = &.{"eval tests"} } },
            },
            .fmt => .{
                .root = "src/fmt/mod.zig",
                .deps = &.{ .base, .parse, .collections, .can, .ctx, .tracy, .reporting },
                .tests = .{ .unit = .{ .aggregators = &.{"fmt tests"} } },
            },
            .unbundle => .{
                .root = "src/unbundle/mod.zig",
                .deps = &.{ .base, .collections, .base58 },
                .tests = .{ .unit = .{} },
            },
            .base58 => .{
                .root = "src/base58/mod.zig",
                .tests = .{ .unit = .{} },
            },
            .roc_target => .{
                .root = "src/target/mod.zig",
                .deps = &.{.base},
                .tests = .{ .unit = .{} },
            },
            .backend => .{
                .root = "src/backend/mod.zig",
                .deps = &.{ .collections, .base, .layout, .builtins, .can, .lir, .static_data, .roc_target, .ctx },
                .tests = .{ .unit = .{} },
            },
            .lir_core => .{
                .root = "src/lir/core.zig",
                .deps = &.{ .base, .collections, .layout, .types, .can, .check },
                .tests = .{ .unit = .{ .aggregators = &.{"lir core declarations are referenced"} } },
            },
            .postcheck => .{
                .root = "src/postcheck/mod.zig",
                .deps = &.{ .base, .builtins, .can, .check, .collections, .layout, .lir_core, .types },
                .tests = .{ .unit = .{ .aggregators = &.{"postcheck declarations are referenced"} } },
            },
            .lir => .{
                .root = "src/lir/mod.zig",
                .deps = &.{ .base, .collections, .layout, .types, .can, .check, .build_options, .lir_core, .postcheck, .builtins },
                .tests = .{ .unit = .{} },
            },
            .symbol => .{
                .root = "src/symbol/mod.zig",
                .deps = &.{.base},
                .tests = .{ .unit = .{} },
            },
            .sljmp => .{
                .root = "src/sljmp/mod.zig",
                .tests = .{ .unit = .{} },
            },
            .roc_args => .{
                .root = "src/default_platform/roc_args.zig",
                .extra_imports = &.{"roc_str_view"},
                .tests = .{ .unit = .{} },
            },
            .echo_platform => .{
                .root = "src/echo_platform/mod.zig",
                .deps = &.{ .builtins, .roc_args },
                .tests = .{ .unit = .{} },
            },
            .docs => .{
                .root = "src/docs/mod.zig",
                .deps = &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .check, .reporting },
                .tests = .{ .unit = .{} },
            },
            .bump => .{
                .root = "src/bump/mod.zig",
                .deps = &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .check, .reporting },
                .tests = .{ .unit = .{} },
            },
            .glue => .{
                .root = "src/glue/mod.zig",
                .deps = &.{ .base, .collections, .parse, .compile, .can, .check, .reporting, .echo_platform, .builtins, .roc_target, .types, .layout, .backend, .eval, .lir, .build_options },
                .extra_imports = &.{"compiler_platform_sources"},
                .tests = .{ .unit = .{ .aggregators = &.{"glue tests"}, .minici = false } },
            },
            .compile => .{
                .root = "src/compile/mod.zig",
                .deps = &.{ .tracy, .build_options, .ctx, .builtins, .collections, .base, .types, .parse, .can, .check, .reporting, .layout, .static_data, .eval, .unbundle, .roc_target, .backend, .lir, .symbol, .sljmp },
                .extra_imports = &.{"compiler_platform_sources"},
                .tests = .{ .unit = .{ .aggregators = &.{"compile tests"} } },
            },
            .ipc => .{
                .root = "src/ipc/mod.zig",
                .tests = .{ .unit = .{ .aggregators = &.{"ipc tests"} } },
                .available = .native,
            },
            .watch => .{
                .root = "src/watch/watch.zig",
                .deps = &.{.build_options},
                .tests = .{ .unit = .{} },
                .available = .native,
            },
            .lsp => .{
                .root = "src/lsp/mod.zig",
                .deps = &.{ .compile, .reporting, .build_options, .ctx, .base, .parse, .can, .types, .fmt, .eval, .roc_target },
                .tests = .{ .unit = .{} },
                .available = .native,
            },
            .bundle => .{
                .root = "src/bundle/mod.zig",
                .deps = &.{ .base, .collections, .base58, .unbundle },
                .tests = .{ .unit = .{} },
                .available = .native,
            },
            .roc_src => .{
                .root = "src/roc_src/mod.zig",
                .tests = .{ .no_tests = "no test blocks" },
                .available = .dependents_only,
            },
            .lsp_unit => .{
                .root = "src/lsp/test/unit.zig",
                .deps = lsp_test_deps,
                .tests = .{ .unit = .{ .aggregators = &.{"lsp unit tests"} } },
                .available = .dependents_only,
            },
            .lsp_integration => .{
                .root = "src/lsp/test/integration.zig",
                .deps = lsp_test_deps,
                .tests = .harness,
                .available = .dependents_only,
            },
            // The size-tracking host allocator shared by the test platform
            // hosts: consumed only by hosts, never by the compiler.
            .host_alloc => .{
                .root = "src/host_alloc/mod.zig",
                .deps = &.{ .builtins, .build_options },
                .tests = .{ .unit = .{} },
                .available = .dependents_only,
            },
        };
    }

    const lsp_test_deps: []const ModuleType = &.{ .lsp, .compile, .reporting, .build_options, .ctx, .base, .parse, .can, .types, .fmt, .eval, .roc_target };

    /// Returns the dependencies for this module type
    pub fn getDependencies(self: ModuleType) []const ModuleType {
        return self.info().deps;
    }

    /// The module's aggregator test blocks; see `Tests.unit`.
    fn aggregators(self: ModuleType) []const []const u8 {
        return switch (self.info().tests) {
            .unit => |unit| unit.aggregators,
            .harness, .no_tests => &.{},
        };
    }
};

/// A module outside the `ModuleType` dependency graph: wired into its specific
/// consumers by `ModuleInfo.extra_imports` or by hand, so it stays clear which
/// code is vendored or shared with builds that leave the compiler out.
const ExtraModule = struct {
    name: []const u8,
    root: []const u8,
};

const extra_modules = [_]ExtraModule{
    .{ .name = "embedded_lld", .root = "src/build/embedded_lld.zig" },
    .{ .name = "roc_str_view", .root = "src/default_platform/roc_str_view.zig" },
    .{ .name = "shim_symbols", .root = "src/builtins/shim_symbols.zig" },
    .{ .name = "raw_pages", .root = "src/raw_pages.zig" },
    .{ .name = "memory_fault", .root = "src/base/memory_fault.zig" },
    .{ .name = "vendor_parse_float", .root = "vendor/parse_float/parse_float.zig" },
    .{ .name = "vendor_ryu", .root = "vendor/ryu.zig" },
    .{ .name = "vendor_relocatable_loader", .root = "vendor/relocatable_loader/mod.zig" },
    .{ .name = "vendor_macho", .root = "vendor/macho/mod.zig" },
    .{ .name = "vendor_llvm_ir", .root = "vendor/llvm_ir/mod.zig" },
    .{ .name = "vendor_llvm_compile_bindings", .root = "vendor/llvm_compile_bindings.zig" },
};

/// The one `RocModules` field `create` builds from generated source.
const generated_module = "compiler_platform_sources";

comptime {
    // `RocModules.create` fills the struct from `ModuleType`, `extra_modules`,
    // and `generated_module`, so its fields must be exactly those names.
    const fields = @typeInfo(RocModules).@"struct".fields;
    for (std.enums.values(ModuleType)) |module_type| {
        if (!@hasField(RocModules, @tagName(module_type))) {
            @compileError("RocModules has no field for module `" ++ @tagName(module_type) ++ "`");
        }
    }
    for (extra_modules) |extra| {
        if (!@hasField(RocModules, extra.name)) @compileError("RocModules has no field for `" ++ extra.name ++ "`");
    }
    if (fields.len != std.enums.values(ModuleType).len + extra_modules.len + 1) {
        @compileError("RocModules has a field that is neither a ModuleType, an extra module, nor the generated module");
    }
}

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
    static_data: *Module,
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
    roc_args: *Module,
    echo_platform: *Module,
    docs: *Module,
    bump: *Module,
    glue: *Module,
    host_alloc: *Module,
    embedded_lld: *Module,
    compiler_platform_sources: *Module,

    // The default-platform runtimes (`src/default_platform/*_runtime.zig`) are
    // compiled as standalone freestanding objects without the `builtins` module,
    // so they read the host-boundary `RocStr` encoding through this single-file
    // module instead of restating it.
    roc_str_view: *Module,

    // Boundary symbol names (`src/builtins/shim_symbols.zig`) as a standalone
    // module for consumers compiled without the `builtins` module (the
    // default-platform runtimes); everything else reaches the same file through
    // `builtins.shim_symbols`.
    shim_symbols: *Module,

    // Anonymous page mapping (`src/raw_pages.zig`) for the objects linked into
    // standalone Roc programs: the default-platform runtimes and the boxy
    // runtime. Shared rather than restated per consumer because a NetBSD
    // program links two of them and the assembly thunk its `mmap` ABI needs may
    // only be defined once.
    raw_pages: *Module,

    // Memory-fault classification (`src/base/memory_fault.zig`) as a standalone
    // module for the default-platform Linux runtime, which is compiled without
    // the `base` module; the compiler's own crash handler reaches the same file
    // through `base.memory_fault`, so both report a stack overflow by one rule.
    memory_fault: *Module,

    // Vendored-from-Zig modules. The sources live under `vendor/`.
    vendor_parse_float: *Module,
    vendor_ryu: *Module,
    vendor_relocatable_loader: *Module,
    vendor_macho: *Module,
    vendor_llvm_ir: *Module,
    vendor_llvm_compile_bindings: *Module,

    pub fn create(b: *Build, build_options_step: *Step.Options, zstd: ?*Dependency) RocModules {
        var self: RocModules = undefined;
        inline for (comptime std.enums.values(ModuleType)) |module_type| {
            const root = if (comptime module_type.info().root) |path| b.path(path) else build_options_step.getOutput();
            @field(self, @tagName(module_type)) = b.addModule(@tagName(module_type), .{ .root_source_file = root });
        }
        inline for (extra_modules) |extra| {
            @field(self, extra.name) = b.addModule(extra.name, .{ .root_source_file = b.path(extra.root) });
        }
        @field(self, generated_module) = createCompilerPlatformSourcesModule(b);

        self.fmt.addAnonymousImport("builtin_source", .{ .root_source_file = b.path("src/build/builtin_source.zig") });

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

        inline for (comptime std.enums.values(ModuleType)) |module_type| {
            self.addImports(self.getModule(module_type), module_type);
        }

        // `embedded_lld` needs `collections` for the single-threaded arena and
        // `build_options` for the Darwin sysroot path baked in at build time.
        self.embedded_lld.addImport("collections", self.collections);
        self.embedded_lld.addImport("build_options", self.build_options);

        // The vendored Mach-O code-signing helpers use the build-time `tracy`
        // tracing shim.
        self.vendor_macho.addImport("tracy", self.tracy);

        // The vendored LLVM IR library's BitcodeReader reaches roc's `base`.
        self.vendor_llvm_ir.addImport("base", self.base);

        return self;
    }

    /// Gives `module` the imports `module_type` declares: its compiler-module
    /// dependencies, then its extra imports. Called for the persistent modules
    /// and for the per-module test builds, so an `@import` resolves in both.
    fn addImports(self: RocModules, module: *Module, comptime module_type: ModuleType) void {
        const module_info = comptime module_type.info();
        inline for (module_info.deps) |dep_type| {
            module.addImport(@tagName(dep_type), self.getModule(dep_type));
        }
        inline for (module_info.extra_imports) |name| {
            module.addImport(name, @field(self, name));
        }
    }

    pub fn addAll(self: RocModules, step: *Step.Compile) void {
        const is_wasm = step.rootModuleTarget().cpu.arch == .wasm32;

        inline for (comptime std.enums.values(ModuleType)) |module_type| {
            if (comptime module_type.info().available == .all) {
                step.root_module.addImport(@tagName(module_type), self.getModule(module_type));
            }
        }
        step.root_module.addImport("embedded_lld", self.embedded_lld);

        // Vendored, used by the CLI linker (Mach-O code signing). Harmless where
        // unused (it is only @import-ed from CLI code, never from wasm).
        step.root_module.addImport("vendor_macho", self.vendor_macho);

        if (!is_wasm) {
            inline for (comptime std.enums.values(ModuleType)) |module_type| {
                if (comptime module_type.info().available == .native) {
                    step.root_module.addImport(@tagName(module_type), self.getModule(module_type));
                }
            }
        }
    }

    /// Get a module by its type
    pub fn getModule(self: RocModules, module_type: ModuleType) *Module {
        inline for (comptime std.enums.values(ModuleType)) |candidate| {
            if (module_type == candidate) return @field(self, @tagName(candidate));
        }
        unreachable;
    }

    /// Add dependencies for a specific module type to a compile step
    pub fn addModuleDependencies(self: RocModules, step: *Step.Compile, comptime module_type: ModuleType) void {
        const module_info = comptime module_type.info();
        inline for (module_info.deps) |dep_type| {
            step.root_module.addImport(@tagName(dep_type), self.getModule(dep_type));
        }
        if (module_type == .fmt) step.root_module.addImport("builtin_source", self.fmt.import_table.get("builtin_source").?);
        inline for (module_info.extra_imports) |name| {
            step.root_module.addImport(name, @field(self, name));
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
        var tests: std.ArrayList(ModuleTest) = .empty;
        var forced_passes: usize = 0;

        inline for (comptime std.enums.values(ModuleType)) |module_type| {
            if (comptime module_type.info().tests != .unit) continue;
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
                    // places—stack_overflow, CoreCtx, ExecutableMemory, channel.nanosleep,
                    // download.getaddrinfo, server.zig, etc.—and most of the remaining
                    // modules import ctx/unbundle transitively. It's simpler (and has no
                    // practical cost for native-only tests) to enable link_libc uniformly.
                    .link_libc = true,
                }),
                .filters = filter_injection.filters,
            });

            // The eval module's host-call trampoline is implemented in assembly;
            // the test compile has its own root module, so it needs the file too.
            if (module_type == .eval) {
                test_step.root_module.addAssemblyFile(b.path("src/eval/host_trampoline.S"));
            }

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

            tests.append(b.allocator, .{ .test_step = test_step }) catch @panic("OOM while creating module tests");
        }

        return .{
            .tests = tests.items,
            .forced_passes = forced_passes,
        };
    }
};
