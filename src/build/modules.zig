const std = @import("std");
const Build = std.Build;
const Module = Build.Module;
const Step = Build.Step;

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

        self.cache.addImport("cache", self.cache);
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

    /// Returns the list of modules that have test files
    /// Each entry contains the module name and the path to its test file
    pub fn getTestableModules() []const struct { name: []const u8, test_path: []const u8 } {
        return &.{
            .{ .name = "base", .test_path = "src/base/mod.zig" },
            .{ .name = "builtins", .test_path = "src/builtins/mod.zig" },
            .{ .name = "cache", .test_path = "src/cache/mod.zig" },
            .{ .name = "canonicalize", .test_path = "src/canonicalize/Mod.zig" },
            .{ .name = "check", .test_path = "src/check/Mod.zig" },
            .{ .name = "collections", .test_path = "src/collections/mod.zig" },
            .{ .name = "compile", .test_path = "src/compile/mod.zig" },
            .{ .name = "eval", .test_path = "src/eval/mod.zig" },
            .{ .name = "types", .test_path = "src/types/mod.zig" },
            .{ .name = "parse", .test_path = "src/parse/test.zig" },
        };
    }
};
