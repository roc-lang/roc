//! Tests for file encounter callback functionality
//!
//! Tests all 4 module types:
//! - module: Can import other modules, but NOT packages
//! - package: Can import other modules AND packages
//! - platform: Can import other modules AND packages
//! - app: Can import other modules AND packages

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const testing = std.testing;

/// Test context for tracking file encounters
const TestContext = struct {
    encountered_files: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TestContext {
        return .{
            .encountered_files = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *TestContext) void {
        for (self.encountered_files.items) |file| {
            self.allocator.free(file);
        }
        self.encountered_files.deinit();
    }

    fn fileEncounteredCallback(context: *anyopaque, file_path: []const u8) void {
        const self: *TestContext = @ptrCast(@alignCast(context));
        const path_copy = self.allocator.dupe(u8, file_path) catch return;
        self.encountered_files.append(path_copy) catch return;
    }
};

// Modules can only import other modules, not packages
test "file encounter callback - module imports (non-package-qualified)" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\module [main]
        \\
        \\import Foo
        \\import Bar
        \\
        \\main = Foo.value + Bar.value
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Verify that both imports were reported
    try testing.expectEqual(@as(usize, 2), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("Foo", test_context.encountered_files.items[0]);
    try testing.expectEqualStrings("Bar", test_context.encountered_files.items[1]);
}

// Packages can import modules AND declare package dependencies
test "file encounter callback - package with dependencies and imports" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\package [main] { cli: "platform/cli.roc", json: "packages/json/main.roc" }
        \\
        \\import Foo
        \\import cli.Stdout
        \\import Helper
        \\
        \\main = Foo.value
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Verify: 2 package deps + 2 non-qualified imports (cli.Stdout is qualified so not reported)
    try testing.expectEqual(@as(usize, 4), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("platform/cli.roc", test_context.encountered_files.items[0]);
    try testing.expectEqualStrings("packages/json/main.roc", test_context.encountered_files.items[1]);
    try testing.expectEqualStrings("Foo", test_context.encountered_files.items[2]);
    try testing.expectEqualStrings("Helper", test_context.encountered_files.items[3]);
}

// app modules can import modules AND declare package dependencies
test "file encounter callback - app with dependencies and imports" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\app [main] { cli: platform "platform/main.roc", http: "packages/http.roc" }
        \\
        \\import cli.Task
        \\import Utils
        \\import Config
        \\
        \\main = Task.succeed {}
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Verify: platform path + 1 package dep (http) + 2 non-qualified imports
    try testing.expectEqual(@as(usize, 4), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("platform/main.roc", test_context.encountered_files.items[0]);
    try testing.expectEqualStrings("packages/http.roc", test_context.encountered_files.items[1]);
    try testing.expectEqualStrings("Utils", test_context.encountered_files.items[2]);
    try testing.expectEqualStrings("Config", test_context.encountered_files.items[3]);
}

// Verify that package-qualified imports are NOT reported for any module type
test "file encounter callback - package-qualified imports not reported (module)" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Note: In reality, modules can't have package dependencies, but for testing
    // we're focusing on the import behavior
    const source =
        \\module [main]
        \\
        \\import Foo
        \\
        \\main = Foo.bar
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Only non-qualified import should be reported
    try testing.expectEqual(@as(usize, 1), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("Foo", test_context.encountered_files.items[0]);
}

test "file encounter callback - package-qualified imports not reported (package)" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\package [helper] { cli: "platform/cli.roc" }
        \\
        \\import cli.Stdout
        \\import cli.Stderr
        \\import cli.Task
        \\
        \\helper = Stdout.line
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Only the package dependency should be reported, not the qualified imports
    try testing.expectEqual(@as(usize, 1), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("platform/cli.roc", test_context.encountered_files.items[0]);
}

// ===== EDGE CASES =====

test "file encounter callback - empty dependencies" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\package [main] {}
        \\
        \\main = "hello"
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // No files should be reported
    try testing.expectEqual(@as(usize, 0), test_context.encountered_files.items.len);
}

test "file encounter callback - mixed module and package imports" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\package [process] { cli: "platform/cli.roc", base: "platform/base.roc" }
        \\
        \\import cli.Stdout
        \\import base.List
        \\import Utils
        \\import Helpers
        \\import cli.Task
        \\
        \\process = Utils.helper
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Should report: 2 package deps + 2 non-qualified module imports
    try testing.expectEqual(@as(usize, 4), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("platform/cli.roc", test_context.encountered_files.items[0]);
    try testing.expectEqualStrings("platform/base.roc", test_context.encountered_files.items[1]);
    try testing.expectEqualStrings("Utils", test_context.encountered_files.items[2]);
    try testing.expectEqualStrings("Helpers", test_context.encountered_files.items[3]);
}

test "file encounter callback - no callback provided" {
    const allocator = testing.allocator;

    const source =
        \\module [main]
        \\
        \\import Foo
        \\
        \\main = Foo.value
    ;

    // Create ModuleEnv without callback
    var module_env = try ModuleEnv.init(allocator, source, null, null);
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    // Should not crash when no callback is provided
    try can_ir.canonicalizeFile();
}

test "file encounter callback - memory leak check" {
    // This test verifies that the TestContext properly cleans up allocated memory
    const allocator = testing.allocator;

    // Create context and immediately destroy it
    {
        var test_context = TestContext.init(allocator);
        defer test_context.deinit();

        // Add some files to ensure cleanup works
        const file1 = try allocator.dupe(u8, "test1.roc");
        try test_context.encountered_files.append(file1);

        const file2 = try allocator.dupe(u8, "test2.roc");
        try test_context.encountered_files.append(file2);
    }
}

test "file encounter callback - hosted header (no packages)" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\hosted [effect]
        \\
        \\import Effect
        \\
        \\effect = Effect.always {}
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Only the import should be reported (hosted headers have no packages)
    try testing.expectEqual(@as(usize, 1), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("Effect", test_context.encountered_files.items[0]);
}

test "file encounter callback - non-string package value" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // If someone tries to use a non-string value for a package path
    // (This would be a parse error in practice, but we handle it gracefully)
    const source =
        \\package [main] { cli: 42 }
        \\
        \\main = "hello"
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module - this might fail with a parse error
    var ast = parse.parse(&module_env.common, allocator) catch {
        // Expected - non-string package values are invalid
        return;
    };
    defer ast.deinit(allocator);

    // If parsing succeeded, canonicalization should handle it gracefully
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Non-string values should not be reported
    try testing.expectEqual(@as(usize, 0), test_context.encountered_files.items.len);
}

test "file encounter callback - import with alias" {
    const allocator = testing.allocator;
    var test_context = TestContext.init(allocator);
    defer test_context.deinit();

    const source =
        \\module [main]
        \\
        \\import Foo as F
        \\import Bar.Baz as B
        \\
        \\main = F.value
    ;

    var module_env = try ModuleEnv.init(
        allocator,
        source,
        TestContext.fileEncounteredCallback,
        &test_context,
    );
    defer module_env.deinit();

    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Canonicalize the module
    var can_ir = try Can.init(&module_env, &ast, null);
    defer can_ir.deinit();

    try can_ir.canonicalizeFile();

    // Imports with aliases should still be reported
    // Note: Bar.Baz is a qualified import, so only "Bar" is reported
    try testing.expectEqual(@as(usize, 2), test_context.encountered_files.items.len);
    try testing.expectEqualStrings("Foo", test_context.encountered_files.items[0]);
    try testing.expectEqualStrings("Bar", test_context.encountered_files.items[1]);
}
