//! Regression tests for #10161: child modules resolve package aliases from
//! owning main.roc, and previously published false diagnostics are cleared.

const std = @import("std");
const SyntaxChecker = @import("lsp").syntax.SyntaxChecker;
const uri_util = @import("lsp").uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 10161 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 10161: platform child resolves package aliases via workspace main", .run = childResolvesPackageAliases },
    .{ .name = "issue 10161: opening platform main clears stale child diagnostics", .run = openingMainClearsStaleChildDiagnostics },
    .{ .name = "issue 10161: sibling package root under workspace uses its own header, not workspace main", .run = siblingPackageRootUsesOwnHeader },
};

fn fixturePlatformDir(allocator: std.mem.Allocator) integration_spec.SpecError![]u8 {
    const repo_root = try std.Io.Dir.cwd().realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(repo_root);
    return try std.fs.path.join(allocator, &.{
        repo_root,
        "test",
        "cli",
        "issue_9864_static_dispatch_package_nominal",
        "platform",
    });
}

fn pathToFileUri(allocator: std.mem.Allocator, path: []const u8) integration_spec.SpecError![]u8 {
    return try uri_util.pathToUri(allocator, path);
}

fn publishContainsModuleNotFound(publish_sets: []const @import("lsp").diagnostics.PublishDiagnostics) bool {
    for (publish_sets) |set| {
        for (set.diagnostics) |diag| {
            if (std.mem.find(u8, diag.message, "MODULE NOT FOUND") != null) return true;
            if (std.mem.find(u8, diag.message, "does not exist") != null) return true;
        }
    }
    return false;
}

fn freePublishSets(allocator: std.mem.Allocator, publish_sets: []@import("lsp").diagnostics.PublishDiagnostics) void {
    for (publish_sets) |*set| set.deinit(allocator);
    allocator.free(publish_sets);
}

fn findPublishForUri(publish_sets: []const @import("lsp").diagnostics.PublishDiagnostics, uri: []const u8) ?@import("lsp").diagnostics.PublishDiagnostics {
    for (publish_sets) |set| {
        if (std.mem.eql(u8, set.uri, uri)) return set;
    }
    return null;
}

/// Checking ThingFx.roc with the platform directory as workspace root must
/// resolve `thing.Thing` via platform/main.roc package aliases.
fn childResolvesPackageAliases() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const cache_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(cache_path);

    const platform_dir = try fixturePlatformDir(allocator);
    defer allocator.free(platform_dir);

    const thing_fx_path = try std.fs.path.join(allocator, &.{ platform_dir, "ThingFx.roc" });
    defer allocator.free(thing_fx_path);
    const thing_fx_uri = try pathToFileUri(allocator, thing_fx_path);
    defer allocator.free(thing_fx_uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, cache_path);
    defer checker.deinit();

    const publish_sets = try checker.check(thing_fx_uri, null, platform_dir);
    defer freePublishSets(allocator, publish_sets);

    try std.testing.expect(!publishContainsModuleNotFound(publish_sets));

    const for_child = findPublishForUri(publish_sets, thing_fx_uri) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 0), for_child.diagnostics.len);
}

/// After a false diagnostic was published for a child URI, checking the owning
/// main.roc must publish an empty diagnostic set for that child URI.
fn openingMainClearsStaleChildDiagnostics() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const cache_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(cache_path);

    const platform_dir = try fixturePlatformDir(allocator);
    defer allocator.free(platform_dir);

    const thing_fx_path = try std.fs.path.join(allocator, &.{ platform_dir, "ThingFx.roc" });
    defer allocator.free(thing_fx_path);
    const thing_fx_uri = try pathToFileUri(allocator, thing_fx_path);
    defer allocator.free(thing_fx_uri);

    const main_path = try std.fs.path.join(allocator, &.{ platform_dir, "main.roc" });
    defer allocator.free(main_path);
    const main_uri = try pathToFileUri(allocator, main_path);
    defer allocator.free(main_uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, cache_path);
    defer checker.deinit();

    // Seed a previously published non-empty diagnostic for the child URI.
    try checker.seedPublishedDiagnosticUriForTesting(thing_fx_uri);

    const publish_sets = try checker.check(main_uri, null, platform_dir);
    defer freePublishSets(allocator, publish_sets);

    const cleared = findPublishForUri(publish_sets, thing_fx_uri) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 0), cleared.diagnostics.len);
    try std.testing.expect(!checker.hasPublishedDiagnosticUriForTesting(thing_fx_uri));
}

/// A sibling package root (`pkg/main.roc`) opened while the workspace root's
/// `main.roc` belongs to an unrelated platform must build using its own header
/// (`package [Thing] {}`), not get routed through `buildWithMain` against the
/// platform's main.roc. Guards against `preferred_main` overriding a root file
/// that already carries its own package graph.
fn siblingPackageRootUsesOwnHeader() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const cache_path = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(cache_path);

    const platform_dir = try fixturePlatformDir(allocator);
    defer allocator.free(platform_dir);

    const repo_root = try std.Io.Dir.cwd().realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(repo_root);
    const pkg_main_path = try std.fs.path.join(allocator, &.{
        repo_root,
        "test",
        "cli",
        "issue_9864_static_dispatch_package_nominal",
        "pkg",
        "main.roc",
    });
    defer allocator.free(pkg_main_path);
    const pkg_main_uri = try pathToFileUri(allocator, pkg_main_path);
    defer allocator.free(pkg_main_uri);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, cache_path);
    defer checker.deinit();

    // workspace_root points at the platform dir, whose main.roc is unrelated
    // to pkg/main.roc; the package root must still resolve using its own header.
    const publish_sets = try checker.check(pkg_main_uri, null, platform_dir);
    defer freePublishSets(allocator, publish_sets);

    try std.testing.expect(!publishContainsModuleNotFound(publish_sets));

    const for_pkg_main = findPublishForUri(publish_sets, pkg_main_uri) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 0), for_pkg_main.diagnostics.len);
}
