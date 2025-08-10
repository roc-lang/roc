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

        // Setup module dependencies
        self.tracy.addImport("build_options", self.build_options);
        self.tracy.addImport("builtins", self.builtins);

        self.collections.addImport("serialization", self.serialization);

        self.base.addImport("serialization", self.serialization);
        self.base.addImport("collections", self.collections);

        self.reporting.addImport("base", self.base);

        self.types.addImport("serialization", self.serialization);
        self.types.addImport("base", self.base);
        self.types.addImport("collections", self.collections);

        self.compile.addImport("base", self.base);
        self.compile.addImport("collections", self.collections);
        self.compile.addImport("types", self.types);
        self.compile.addImport("builtins", self.builtins);
        self.compile.addImport("reporting", self.reporting);
        self.compile.addImport("serialization", self.serialization);
        self.compile.addImport("parse", self.parse);
        self.compile.addImport("can", self.can);
        self.compile.addImport("check", self.check);
        self.compile.addImport("cache", self.cache);

        self.parse.addImport("base", self.base);
        self.parse.addImport("compile", self.compile);
        self.parse.addImport("collections", self.collections);
        self.parse.addImport("tracy", self.tracy);
        self.parse.addImport("reporting", self.reporting);

        self.can.addImport("base", self.base);
        self.can.addImport("parse", self.parse);
        self.can.addImport("collections", self.collections);
        self.can.addImport("compile", self.compile);
        self.can.addImport("types", self.types);
        self.can.addImport("builtins", self.builtins);
        self.can.addImport("tracy", self.tracy);

        self.check.addImport("base", self.base);
        self.check.addImport("tracy", self.tracy);
        self.check.addImport("collections", self.collections);
        self.check.addImport("types", self.types);
        self.check.addImport("can", self.can);
        self.check.addImport("compile", self.compile);
        self.check.addImport("builtins", self.builtins);
        self.check.addImport("reporting", self.reporting);

        self.cache.addImport("compile", self.compile);
        self.cache.addImport("base", self.base);
        self.cache.addImport("collections", self.collections);
        self.cache.addImport("serialization", self.serialization);
        self.cache.addImport("reporting", self.reporting);
        self.cache.addImport("fs", self.fs);
        self.cache.addImport("build_options", self.build_options);

        return self;
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

    pub fn createModuleTests(self: RocModules, b: *Build, target: ResolvedTarget, optimize: OptimizeMode) []ModuleTest {
        const test_configs = [_]struct { name: []const u8, path: []const u8 }{
            .{ .name = "serialization", .path = "src/serialization/mod.zig" },
            .{ .name = "collections", .path = "src/collections/mod.zig" },
            .{ .name = "base", .path = "src/base/mod.zig" },
            .{ .name = "types", .path = "src/types/mod.zig" },
            .{ .name = "builtins", .path = "src/builtins/mod.zig" },
            .{ .name = "compile", .path = "src/compile/mod.zig" },
            .{ .name = "reporting", .path = "src/reporting/mod.zig" },
            .{ .name = "parse", .path = "src/parse/mod.zig" },
            .{ .name = "can", .path = "src/canonicalize/Mod.zig" },
            .{ .name = "check", .path = "src/check/Mod.zig" },
            .{ .name = "cache", .path = "src/cache/mod.zig" },
            .{ .name = "fs", .path = "src/fs/mod.zig" },
        };
        
        var tests = b.allocator.alloc(ModuleTest, test_configs.len) catch @panic("OOM");
        
        for (test_configs, 0..) |config, i| {
            const test_step = b.addTest(.{
                .root_source_file = b.path(config.path),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            });
            
            // Add all modules as dependencies so tests can import what they need
            self.addAllToTest(test_step);
            
            const run_step = b.addRunArtifact(test_step);
            
            tests[i] = ModuleTest{
                .test_step = test_step,
                .run_step = run_step,
            };
        }
        
        return tests;
    }
};
