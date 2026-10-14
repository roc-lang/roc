//! Every `run-test-zig-<suffix>` suite `build.zig` registers, other than the
//! per-module suites from the module inventory in `modules.zig`, and where CI
//! runs it.
//!
//! `build.zig` refuses to register a suite missing from this list, and MiniCI
//! takes its `run-test-zig-*` jobs from it, so a new suite is a MiniCI job
//! unless its entry says where else CI runs it and why.

/// Where CI runs a suite.
pub const Coverage = union(enum) {
    /// A MiniCI job on every host.
    minici,
    /// Not a MiniCI job; names the CI job that runs it instead, and why.
    elsewhere: []const u8,
};

/// One registered suite.
pub const Suite = struct {
    /// The suffix after `run-test-zig-`.
    suffix: []const u8,
    coverage: Coverage,
};

/// In MiniCI job order.
pub const suites = [_]Suite{
    .{ .suffix = "snapshot-tool", .coverage = .minici },
    .{ .suffix = "builtin-doc", .coverage = .minici },
    .{ .suffix = "cli-main", .coverage = .minici },
    .{ .suffix = "machine-code-shim", .coverage = .minici },
    .{ .suffix = "watch-cli", .coverage = .minici },
    .{ .suffix = "minici", .coverage = .minici },
    .{ .suffix = "fx-platform", .coverage = .minici },
    .{ .suffix = "lir-inline", .coverage = .minici },
    .{ .suffix = "trmc-lir", .coverage = .minici },
    .{ .suffix = "build-helpers", .coverage = .minici },
    .{ .suffix = "backend-llvm", .coverage = .minici },
    .{ .suffix = "rc-conformance", .coverage = .minici },
    .{ .suffix = "boxy-abi", .coverage = .minici },
    .{ .suffix = "cli-runner-unit", .coverage = .minici },
    .{ .suffix = "http-header-decoder-platform", .coverage = .minici },
    .{ .suffix = "json-decoder-platform", .coverage = .minici },
    .{ .suffix = "machine-code-boundary-link", .coverage = .minici },
    .{ .suffix = "interpreter-boundary-link", .coverage = .minici },
    .{ .suffix = "machine-code-shim-archive", .coverage = .minici },
    .{ .suffix = "archive-member-names", .coverage = .minici },
    .{ .suffix = "tidy-unit", .coverage = .{ .elsewhere = "ci_manager.yml's source-check job runs it: it tests a source check, needs no build-ci output, and its result is the same on every host" } },
    .{ .suffix = "restamp-zig-cache", .coverage = .{ .elsewhere = "ci_manager.yml's source-check job runs it: it tests a CI tool, needs no build-ci output, and its result is the same on every host" } },
};

/// The suite registered as `run-test-zig-<suffix>`, if listed.
pub fn find(suffix: []const u8) ?Suite {
    for (suites) |suite| {
        if (std.mem.eql(u8, suite.suffix, suffix)) return suite;
    }
    return null;
}

const std = @import("std");

test "suite suffixes are distinct" {
    for (suites, 0..) |suite, index| {
        for (suites[index + 1 ..]) |later| try std.testing.expect(!std.mem.eql(u8, suite.suffix, later.suffix));
    }
}
