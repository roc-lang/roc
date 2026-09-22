//! Regression tests for #11302 in the LSP: a module below the workspace root
//! is checked as part of the workspace `main.roc`'s package, identified by
//! its package-relative path.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const PublishDiagnostics = lsp.diagnostics.PublishDiagnostics;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 11302 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 11302: nested module in a symlinked workspace directory resolves", .run = symlinkedWorkspaceDirectoryResolves },
    .{ .name = "issue 11302: a symlinked workspace main.roc keeps the workspace as the package source root", .run = symlinkedMainKeepsWorkspaceSourceRoot },
    .{ .name = "issue 11302: a workspace main.roc linked within the workspace resolves", .run = symlinkedMainInsideWorkspaceResolves },
};

const nested_module_source =
    \\import ./Helper
    \\
    \\Widget :: [].{
    \\    default : Str
    \\    default = Helper.suffix()
    \\}
;

const sibling_module_source =
    \\Helper :: [].{
    \\    suffix : () -> Str
    \\    suffix = || "directory-qualified local module"
    \\}
;

const outside_package_message = "not inside the source directory";
const source_outside_package_message = "resolves outside this package's source root";

fn writeFile(dir: std.Io.Dir, sub_path: []const u8, data: []const u8) integration_spec.SpecError!void {
    if (std.fs.path.dirname(sub_path)) |parent| {
        dir.createDirPath(test_env.io, parent) catch return error.TestUnexpectedResult;
    }
    try dir.writeFile(test_env.io, .{ .sub_path = sub_path, .data = data });
}

fn symLink(dir: std.Io.Dir, target: []const u8, link: []const u8, is_directory: bool) integration_spec.SpecError!void {
    dir.symLink(test_env.io, target, link, .{ .is_directory = is_directory }) catch return error.TestUnexpectedResult;
}

fn freePublishSets(allocator: std.mem.Allocator, publish_sets: []PublishDiagnostics) void {
    for (publish_sets) |*set| set.deinit(allocator);
    allocator.free(publish_sets);
}

/// Check `module` (a path below `base`) with `workspace` (a path below `base`)
/// as the LSP workspace root, and return every published diagnostic message
/// joined by newlines.
fn checkModule(base: []const u8, module: []const u8, workspace: []const u8) integration_spec.SpecError![]u8 {
    const allocator = test_env.allocator;

    const module_path = try std.fs.path.join(allocator, &.{ base, module });
    defer allocator.free(module_path);
    const module_uri = try uri_util.pathToUri(allocator, module_path);
    defer allocator.free(module_uri);
    const workspace_root = try std.fs.path.join(allocator, &.{ base, workspace });
    defer allocator.free(workspace_root);
    const cache_path = try std.fs.path.join(allocator, &.{ base, "cache" });
    defer allocator.free(cache_path);

    var checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
    test_env.configureChecker(&checker, cache_path);
    defer checker.deinit();

    const publish_sets = try checker.check(module_uri, null, workspace_root);
    defer freePublishSets(allocator, publish_sets);

    var messages = std.ArrayList(u8).empty;
    errdefer messages.deinit(allocator);
    for (publish_sets) |set| {
        for (set.diagnostics) |diag| {
            try messages.appendSlice(allocator, diag.message);
            try messages.append(allocator, '\n');
        }
    }
    return messages.toOwnedSlice(allocator);
}

/// The LSP resolves the checked file through symlinks. Opening a nested
/// module through a symlinked workspace directory must still place it inside
/// the workspace `main.roc`'s package, so its relative imports resolve.
fn symlinkedWorkspaceDirectoryResolves() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    try writeFile(tmp.dir, "real/main.roc", "package [] {}");
    try writeFile(tmp.dir, "real/Src/Widget.roc", nested_module_source);
    try writeFile(tmp.dir, "real/Src/Helper.roc", sibling_module_source);
    try symLink(tmp.dir, "real", "link", true);

    const base = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(base);

    const messages = try checkModule(base, "link/Src/Widget.roc", "link");
    defer allocator.free(messages);
    try std.testing.expectEqualStrings("", messages);
}

/// A workspace `main.roc` that links to a file in another directory does not
/// move the package's source root to the link target's directory. The
/// package stays rooted at the workspace, so the existing rule that a
/// package's source files lie physically inside its source root rejects the
/// linked `main.roc`, and a module beside the link target is outside the
/// package rather than silently adopted by it.
fn symlinkedMainKeepsWorkspaceSourceRoot() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    try writeFile(tmp.dir, "elsewhere/main.roc", "package [] {}");
    try writeFile(tmp.dir, "elsewhere/Src/Widget.roc", nested_module_source);
    try writeFile(tmp.dir, "elsewhere/Src/Helper.roc", sibling_module_source);
    try writeFile(tmp.dir, "workspace/Src/Widget.roc", nested_module_source);
    try writeFile(tmp.dir, "workspace/Src/Helper.roc", sibling_module_source);
    try symLink(tmp.dir, "../elsewhere/main.roc", "workspace/main.roc", false);

    const base = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(base);

    const inside = try checkModule(base, "workspace/Src/Widget.roc", "workspace");
    defer allocator.free(inside);
    try std.testing.expect(std.mem.find(u8, inside, source_outside_package_message) != null);
    try std.testing.expect(std.mem.find(u8, inside, outside_package_message) == null);

    const outside = try checkModule(base, "elsewhere/Src/Widget.roc", "workspace");
    defer allocator.free(outside);
    try std.testing.expect(std.mem.find(u8, outside, outside_package_message) != null);
}

/// A workspace `main.roc` that links to another file inside the workspace is
/// an ordinary package root.
fn symlinkedMainInsideWorkspaceResolves() integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();

    try writeFile(tmp.dir, "workspace/package.roc", "package [] {}");
    try writeFile(tmp.dir, "workspace/Src/Widget.roc", nested_module_source);
    try writeFile(tmp.dir, "workspace/Src/Helper.roc", sibling_module_source);
    try symLink(tmp.dir, "package.roc", "workspace/main.roc", false);

    const base = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(base);

    const messages = try checkModule(base, "workspace/Src/Widget.roc", "workspace");
    defer allocator.free(messages);
    try std.testing.expectEqualStrings("", messages);
}
