const std = @import("std");
const Build = std.Build;
const Module = Build.Module;
const Step = Build.Step;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;

pub const ModuleTest = struct {
    test_step: *Step.Compile,
    run_step: *Step.Run,
};

pub const ModuleType = enum {
    serialization,
    collections,
    base,
    types,
    builtins,
    compile,
    reporting,
    parse,
    can,
    check,
    tracy,
    cache,
    fs,
    build_options,

    /// Returns the dependencies for this module type
    pub fn getDependencies(self: ModuleType) []const ModuleType {
        return switch (self) {
            .serialization => &.{},
            .collections => &.{.serialization},
            .base => &.{ .serialization, .collections },
            .types => &.{ .serialization, .base, .collections },
            .builtins => &.{},
            .reporting => &.{.base},
            .compile => &.{ .base, .collections, .types, .builtins, .reporting, .serialization },
            .parse => &.{ .base, .compile, .collections, .tracy, .reporting },
            .can => &.{ .base, .parse, .collections, .compile, .types, .builtins, .tracy },
            .check => &.{ .base, .tracy, .collections, .types, .can, .compile, .builtins, .reporting },
            .tracy => &.{ .build_options, .builtins },
            .cache => &.{ .compile, .base, .collections, .serialization, .reporting, .fs, .build_options },
            .fs => &.{},
            .build_options => &.{},
        };
    }
};

/// Manages all Roc compiler modules and their dependencies
pub const RocModules = struct {
    serialization: *Module,
    collections: *Module,
    base: *Module,
    types: *Module,
    builtins: *Module,
    compile: *Module,
    reporting: *Module,
    parse: *Module,
    can: *Module,
    check: *Module,
    tracy: *Module,
    cache: *Module,
    fs: *Module,
    build_options: *Module,

    pub fn create(b: *Build, build_options_step: *Step.Options) RocModules {
        const self = RocModules{
            .serialization = b.addModule("serialization", .{ .root_source_file = b.path("src/serialization/mod.zig") }),
            .collections = b.addModule("collections", .{ .root_source_file = b.path("src/collections/mod.zig") }),
            .base = b.addModule("base", .{ .root_source_file = b.path("src/base/mod.zig") }),
            .types = b.addModule("types", .{ .root_source_file = b.path("src/types/mod.zig") }),
            .builtins = b.addModule("builtins", .{ .root_source_file = b.path("src/builtins/mod.zig") }),
            .compile = b.addModule("compile", .{ .root_source_file = b.path("src/compile/mod.zig") }),
            .reporting = b.addModule("reporting", .{ .root_source_file = b.path("src/reporting/mod.zig") }),
            .parse = b.addModule("parse", .{ .root_source_file = b.path("src/parse/mod.zig") }),
            .can = b.addModule("can", .{ .root_source_file = b.path("src/canonicalize/Mod.zig") }),
            .check = b.addModule("check", .{ .root_source_file = b.path("src/check/Mod.zig") }),
            .tracy = b.addModule("tracy", .{ .root_source_file = b.path("src/tracy.zig") }),
            .cache = b.addModule("cache", .{ .root_source_file = b.path("src/cache/mod.zig") }),
            .fs = b.addModule("fs", .{ .root_source_file = b.path("src/fs/mod.zig") }),
            .build_options = b.addModule("build_options", .{ .root_source_file = build_options_step.getOutput() }),
        };

        // Setup module dependencies using our generic helper
        self.setupModuleDependencies();

        return self;
    }

    fn setupModuleDependencies(self: RocModules) void {
        const all_modules = [_]ModuleType{
            .serialization,
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
            .cache,
            .fs,
            .build_options,
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
        step.root_module.addImport("serialization", self.serialization);
        step.root_module.addImport("compile", self.compile);
        step.root_module.addImport("reporting", self.reporting);
        step.root_module.addImport("parse", self.parse);
        step.root_module.addImport("can", self.can);
        step.root_module.addImport("check", self.check);
        step.root_module.addImport("tracy", self.tracy);
        step.root_module.addImport("builtins", self.builtins);
        step.root_module.addImport("cache", self.cache);
        step.root_module.addImport("fs", self.fs);
        step.root_module.addImport("build_options", self.build_options);
    }

    pub fn addAllToTest(self: RocModules, step: *Step.Compile) void {
        self.addAll(step);
    }

    /// Get a module by its type
    pub fn getModule(self: RocModules, module_type: ModuleType) *Module {
        return switch (module_type) {
            .serialization => self.serialization,
            .collections => self.collections,
            .base => self.base,
            .types => self.types,
            .builtins => self.builtins,
            .compile => self.compile,
            .reporting => self.reporting,
            .parse => self.parse,
            .can => self.can,
            .check => self.check,
            .tracy => self.tracy,
            .cache => self.cache,
            .fs => self.fs,
            .build_options => self.build_options,
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

    pub fn createModuleTests(self: RocModules, b: *Build, target: ResolvedTarget, optimize: OptimizeMode) [12]ModuleTest {
        const test_configs = [_]ModuleType{
            .serialization,
            .collections,
            .base,
            .types,
            .builtins,
            .compile,
            .reporting,
            .parse,
            .can,
            .check,
            .cache,
            .fs,
        };

        var tests: [test_configs.len]ModuleTest = undefined;

        inline for (test_configs, 0..) |module_type, i| {
            const module = self.getModule(module_type);
            const test_step = b.addTest(.{
                .name = b.fmt("{s}_test", .{@tagName(module_type)}),
                .root_source_file = module.root_source_file.?,
                .target = target,
                .optimize = optimize,
            });

            // Add only the necessary dependencies for each module test
            self.addModuleDependencies(test_step, module_type);

            const run_step = b.addRunArtifact(test_step);

            tests[i] = .{
                .test_step = test_step,
                .run_step = run_step,
            };
        }

        return tests;
    }

};
