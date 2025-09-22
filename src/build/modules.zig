const std = @import("std");
const Build = std.Build;
const Module = Build.Module;
const Step = Build.Step;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;
const Dependency = std.Build.Dependency;

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
        else => &.{},
    };
}

fn extendWithAggregatorFilters(
    b: *Build,
    base: []const []const u8,
    module_type: ModuleType,
) []const []const u8 {
    if (base.len == 0) return base;

    const extras = aggregatorFilters(module_type);
    if (extras.len == 0) return base;

    var list = std.ArrayList([]const u8).init(b.allocator);
    list.ensureTotalCapacity(base.len + extras.len) catch @panic("OOM while extending module test filters");
    list.appendSlice(base) catch @panic("OOM while extending module test filters");

    for (extras) |extra| {
        if (filtersContain(base, extra)) continue;
        list.append(b.dupe(extra)) catch @panic("OOM while extending module test filters");
    }

    return list.toOwnedSlice() catch @panic("OOM while finalizing module test filters");
}

/// Represents a test module with its compilation and execution steps.
pub const ModuleTest = struct {
    test_step: *Step.Compile,
    run_step: *Step.Run,
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

    /// Returns the dependencies for this module type
    pub fn getDependencies(self: ModuleType) []const ModuleType {
        return switch (self) {
            .build_options => &.{},
            .builtins => &.{},
            .fs => &.{},
            .tracy => &.{ .build_options, .builtins },
            .collections => &.{},
            .base => &.{.collections},
            .roc_src => &.{},
            .types => &.{ .base, .collections },
            .reporting => &.{ .collections, .base },
            .parse => &.{ .tracy, .collections, .base, .reporting },
            .can => &.{ .tracy, .builtins, .collections, .types, .base, .parse, .reporting },
            .check => &.{ .tracy, .builtins, .collections, .base, .parse, .types, .can, .reporting },
            .layout => &.{ .collections, .base, .types, .builtins, .can },
            .eval => &.{ .collections, .base, .types, .builtins, .parse, .can, .check, .layout, .build_options },
            .compile => &.{ .tracy, .build_options, .fs, .builtins, .collections, .base, .types, .parse, .can, .check, .reporting, .layout, .eval },
            .ipc => &.{},
            .repl => &.{ .base, .compile, .parse, .types, .can, .check, .builtins, .layout, .eval },
            .fmt => &.{ .base, .parse, .collections, .can, .fs, .tracy },
            .watch => &.{.build_options},
            .bundle => &.{ .base, .collections, .base58 },
            .unbundle => &.{ .base, .collections, .base58 },
            .base58 => &.{},
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
        };

        // Link zstd to bundle module if available (it's unsupported on wasm32, so don't link it)
        if (zstd) |z| {
            self.bundle.linkLibrary(z.artifact("zstd"));
        }
        // Note: unbundle module uses Zig's std zstandard, so doesn't need C library

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
        step.root_module.addImport("base", self.base);
        step.root_module.addImport("collections", self.collections);
        step.root_module.addImport("types", self.types);
        step.root_module.addImport("compile", self.compile);
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
        step.root_module.addImport("ipc", self.ipc);
        step.root_module.addImport("repl", self.repl);
        step.root_module.addImport("fmt", self.fmt);
        step.root_module.addImport("watch", self.watch);

        // Don't add bundle module for WASM targets (zstd C library not available)
        if (step.rootModuleTarget().cpu.arch != .wasm32) {
            step.root_module.addImport("bundle", self.bundle);
        }

        step.root_module.addImport("unbundle", self.unbundle);
        step.root_module.addImport("base58", self.base58);
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
    ) [19]ModuleTest {
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
        };

        var tests: [test_configs.len]ModuleTest = undefined;

        inline for (test_configs, 0..) |module_type, i| {
            const module = self.getModule(module_type);
            const module_filters = extendWithAggregatorFilters(b, test_filters, module_type);
            const test_step = b.addTest(.{
                .name = b.fmt("{s}", .{@tagName(module_type)}),
                .root_source_file = module.root_source_file.?,
                .target = target,
                .optimize = optimize,
                .filters = module_filters,
                // IPC module needs libc for mmap, munmap, close on POSIX systems
                // Bundle module needs libc for zstd
                // Unbundle module doesn't need libc (uses Zig's std zstandard)
                .link_libc = (module_type == .ipc or module_type == .bundle),
            });

            // Watch module needs Core Foundation and FSEvents on macOS (only when not cross-compiling)
            // These frameworks provide the FSEvents API for proper event-driven file system monitoring on macOS.
            if (module_type == .watch and target.result.os.tag == .macos and target.query.isNative()) {
                test_step.linkFramework("CoreFoundation");
                test_step.linkFramework("CoreServices");
            }

            // Add only the necessary dependencies for each module test
            self.addModuleDependencies(test_step, module_type);

            // Link zstd for bundle module
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

        return tests;
    }
};
