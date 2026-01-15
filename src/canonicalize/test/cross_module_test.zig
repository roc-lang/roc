//! Tests for cross-module canonicalization code paths.
//!
//! These tests exercise error handling and edge cases in import processing.
//! The main cross-module functionality is tested in import_validation_test.zig.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const parse = @import("parse");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const CIR = @import("../CIR.zig");

const Ident = base.Ident;

const ParseResult = struct {
    parse_env: *ModuleEnv,
    ast: *parse.AST,
    can: *Can,
};

/// Helper to parse and canonicalize source code
fn parseAndCanonicalizeSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    module_envs: ?*std.AutoHashMap(Ident.Idx, Can.AutoImportedType),
) !ParseResult {
    const parse_env = try allocator.create(ModuleEnv);
    parse_env.* = try ModuleEnv.init(allocator, source);

    const ast = try allocator.create(parse.AST);
    ast.* = try parse.parse(&parse_env.common, allocator);

    try parse_env.initCIRFields("Test");

    const can = try allocator.create(Can);
    can.* = try Can.init(parse_env, ast, module_envs);

    return .{
        .parse_env = parse_env,
        .ast = ast,
        .can = can,
    };
}

fn cleanupResult(allocator: std.mem.Allocator, result: ParseResult) void {
    result.can.deinit();
    allocator.destroy(result.can);
    result.ast.deinit(allocator);
    allocator.destroy(result.ast);
    result.parse_env.deinit();
    allocator.destroy(result.parse_env);
}

/// Count how many module_not_found diagnostics are in the list
fn countModuleNotFoundDiagnostics(diagnostics: []const CIR.Diagnostic) u32 {
    var count: u32 = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .module_not_found => count += 1,
            else => {},
        }
    }
    return count;
}

// ============================================================================
// Cross-module import error tests
// ============================================================================

test "cross-module: module not found with empty module_envs" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    // Empty module_envs (no modules available) - this triggers the module_not_found check
    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import SomeModule
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Should have a module_not_found diagnostic
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expect(module_not_found_count >= 1);
}

test "cross-module: multiple missing modules with module_envs" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import ModuleA
        \\import ModuleB
        \\import ModuleC
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Should have multiple module_not_found diagnostics
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expect(module_not_found_count >= 3);
}

test "cross-module: import with exposing list - module not found" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import MissingModule exposing [someValue, SomeType]
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Should have module_not_found since the module doesn't exist
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expect(module_not_found_count >= 1);
}

test "cross-module: import with alias - module not found" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import VeryLongModuleName as Short
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Should have module_not_found since the module doesn't exist
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expect(module_not_found_count >= 1);
}

test "cross-module: import star from missing module" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import MissingMod exposing [..]
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Should have module_not_found
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expect(module_not_found_count >= 1);
}

// Tests that verify package-qualified imports DON'T generate module_not_found errors
// (since they're resolved by the workspace resolver, not at canonicalization time)

test "cross-module: package-qualified import does not trigger module_not_found" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import pf.Utils
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Package-qualified imports should NOT generate module_not_found
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expectEqual(@as(u32, 0), module_not_found_count);
}

test "cross-module: nested package-qualified import does not trigger module_not_found" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();

    var module_envs = std.AutoHashMap(Ident.Idx, Can.AutoImportedType).init(allocator);
    defer module_envs.deinit();

    const main_source =
        \\module [result]
        \\
        \\import pkg.Sub.Module
        \\
        \\result = 42
    ;

    var result = try parseAndCanonicalizeSource(allocator, main_source, &module_envs);
    defer cleanupResult(allocator, result);

    _ = try result.can.canonicalizeFile();

    // Package-qualified imports should NOT generate module_not_found
    const diagnostics = try result.parse_env.getDiagnostics();
    defer allocator.free(diagnostics);

    const module_not_found_count = countModuleNotFoundDiagnostics(diagnostics);
    try testing.expectEqual(@as(u32, 0), module_not_found_count);
}
