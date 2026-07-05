//! Package identity construction for shared caches and cross-artifact nominal identity.
//!
//! Package identity is derived from the package's own provenance: a resolved URL,
//! a canonical local root path, or one of the compiler-synthesized roots whose
//! filesystem path is intentionally ephemeral.

const std = @import("std");
const CoreCtx = @import("ctx").CoreCtx;
const package_resolution = @import("package_resolution.zig");

const Allocator = std.mem.Allocator;

/// Identity for compiler-synthesized default-app roots whose temp path is ephemeral.
pub const synthetic_app_identity = "app";
/// Identity for compiler-synthesized default platforms whose temp path is ephemeral.
pub const synthetic_platform_identity = "pf";

/// Errors that can occur while deriving an owned package identity string.
pub const PackageIdentityError = Allocator.Error || CoreCtx.CanonicalizeError;

/// Provenance that determines package identity.
pub const PackageProvenance = union(enum) {
    /// A resolved package URL.
    url: []const u8,
    /// A local package root path, canonicalized before use.
    local_path: []const u8,
    /// Compiler-synthesized default-app root.
    synthetic_app,
    /// Compiler-synthesized default platform.
    synthetic_platform,
};

/// Return an owned package identity string derived from package provenance.
pub fn packageIdentityFor(
    allocator: Allocator,
    filesystem: CoreCtx,
    provenance: PackageProvenance,
) PackageIdentityError![]const u8 {
    return switch (provenance) {
        .url => |url| try allocator.dupe(u8, url),
        .local_path => |path| try filesystem.canonicalize(path, allocator),
        .synthetic_app => try allocator.dupe(u8, synthetic_app_identity),
        .synthetic_platform => try allocator.dupe(u8, synthetic_platform_identity),
    };
}

/// Options for mapping a resolved package graph to package identity keys.
pub const PackageKeyOptions = struct {
    /// Use the synthetic app identity for the resolved root package.
    synthetic_root: bool = false,
    /// Use the synthetic platform identity for root platform dependencies.
    synthetic_platform: bool = false,
};

/// Owned package identity strings indexed like package_resolution.Resolved.packages.
pub const PackageKeys = struct {
    allocator: Allocator,
    identities: []const []const u8,

    pub fn identity(self: PackageKeys, index: usize) []const u8 {
        return self.identities[index];
    }

    pub fn deinit(self: *PackageKeys) void {
        for (self.identities) |identity_value| {
            self.allocator.free(@constCast(identity_value));
        }
        self.allocator.free(self.identities);
        self.* = undefined;
    }
};

/// Build all package identity keys for a resolved graph once, indexed by package index.
pub fn buildPackageKeys(
    allocator: Allocator,
    filesystem: CoreCtx,
    resolved: *const package_resolution.Resolved,
    options: PackageKeyOptions,
) PackageIdentityError!PackageKeys {
    const identities = try allocator.alloc([]const u8, resolved.packages.len);
    errdefer allocator.free(identities);

    var initialized: usize = 0;
    errdefer {
        for (identities[0..initialized]) |identity_value| {
            allocator.free(@constCast(identity_value));
        }
    }

    var synthetic_platform_targets = std.DynamicBitSetUnmanaged{};
    try synthetic_platform_targets.resize(allocator, resolved.packages.len, false);
    defer synthetic_platform_targets.deinit(allocator);

    if (options.synthetic_platform and resolved.packages.len > package_resolution.Resolved.root_index) {
        const root = resolved.packages[package_resolution.Resolved.root_index];
        for (root.deps) |dep| {
            if (dep.is_platform) synthetic_platform_targets.set(dep.target);
        }
    }

    for (resolved.packages, 0..) |package, i| {
        const provenance: PackageProvenance = if (i == package_resolution.Resolved.root_index and options.synthetic_root)
            .synthetic_app
        else if (synthetic_platform_targets.isSet(i))
            .synthetic_platform
        else if (package.url) |url|
            .{ .url = url.url }
        else
            .{ .local_path = package.root_file };

        identities[i] = try packageIdentityFor(allocator, filesystem, provenance);
        initialized += 1;
    }

    return .{
        .allocator = allocator,
        .identities = identities,
    };
}

/// A version-bump note keyed by the package identity used for compilation.
pub const VersionBumpNote = struct {
    package_identity: []const u8,
    message: []const u8,
};

/// Collect version-bump notes using helper-built package identity keys.
pub fn versionBumpNotesForPackageKeys(
    resolved: *const package_resolution.Resolved,
    keys: PackageKeys,
    allocator: Allocator,
) Allocator.Error![]VersionBumpNote {
    var notes = std.ArrayListUnmanaged(VersionBumpNote).empty;
    for (resolved.packages, 0..) |package, package_index| {
        for (package.deps) |dep| {
            const declared = dep.declared_version orelse continue;
            const target = resolved.packages[dep.target];
            const resolved_url = target.url orelse continue;
            if (declared.eql(resolved_url.version)) continue;
            try notes.append(allocator, .{
                .package_identity = try allocator.dupe(u8, keys.identity(package_index)),
                .message = try std.fmt.allocPrint(
                    allocator,
                    "the package this error is in declares its dependency {s} as version {d}.{d}.{d}, " ++
                        "but version solving resolved {s} to {s} because something else in the " ++
                        "dependency graph mentions that higher version. " ++
                        "Minor version bumps are supposed to be backwards-compatible, but that is " ++
                        "a guideline and not an enforced invariant, and this particular minor " ++
                        "version bump may not have been backwards-compatible in practice.",
                    .{
                        dep.alias,
                        declared.major,
                        declared.minor,
                        declared.patch,
                        dep.alias,
                        resolved_url.url,
                    },
                ),
            });
        }
    }
    return notes.toOwnedSlice(allocator);
}

test "packageIdentityFor uses URL identity verbatim" {
    const allocator = std.testing.allocator;
    const filesystem = CoreCtx.testing(allocator, allocator);

    const identity = try packageIdentityFor(allocator, filesystem, .{ .url = "https://example.com/pkg/1.2.3/abcd" });
    defer allocator.free(@constCast(identity));

    try std.testing.expectEqualStrings("https://example.com/pkg/1.2.3/abcd", identity);
}

test "packageIdentityFor canonicalizes local paths" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(std.testing.io, "pkg", .default_dir);
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "pkg/main.roc", .data = "module []\n" });

    const tmp_root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_root);
    const direct = try std.fs.path.join(allocator, &.{ tmp_root, "pkg", "main.roc" });
    defer allocator.free(direct);
    const differently_spelled = try std.fs.path.join(allocator, &.{ tmp_root, "pkg", "..", "pkg", ".", "main.roc" });
    defer allocator.free(differently_spelled);

    const filesystem = CoreCtx.os(allocator, allocator, std.testing.io);
    const direct_identity = try packageIdentityFor(allocator, filesystem, .{ .local_path = direct });
    defer allocator.free(@constCast(direct_identity));
    const spelled_identity = try packageIdentityFor(allocator, filesystem, .{ .local_path = differently_spelled });
    defer allocator.free(@constCast(spelled_identity));

    try std.testing.expectEqualStrings(direct_identity, spelled_identity);
}

test "packageIdentityFor canonicalizes symlinked local paths" {
    if (comptime @import("builtin").os.tag == .windows) return error.SkipZigTest;

    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(std.testing.io, "real", .default_dir);
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "real/main.roc", .data = "module []\n" });
    tmp.dir.symLink(std.testing.io, "real", "link", .{ .is_directory = true }) catch return error.SkipZigTest;

    const tmp_root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_root);
    const real_path = try std.fs.path.join(allocator, &.{ tmp_root, "real", "main.roc" });
    defer allocator.free(real_path);
    const link_path = try std.fs.path.join(allocator, &.{ tmp_root, "link", "main.roc" });
    defer allocator.free(link_path);

    const filesystem = CoreCtx.os(allocator, allocator, std.testing.io);
    const real_identity = try packageIdentityFor(allocator, filesystem, .{ .local_path = real_path });
    defer allocator.free(@constCast(real_identity));
    const link_identity = try packageIdentityFor(allocator, filesystem, .{ .local_path = link_path });
    defer allocator.free(@constCast(link_identity));

    try std.testing.expectEqualStrings(real_identity, link_identity);
}

test "packageIdentityFor uses synthetic identities for synthesized roots" {
    const allocator = std.testing.allocator;
    const filesystem = CoreCtx.testing(allocator, allocator);

    const app_identity = try packageIdentityFor(allocator, filesystem, .synthetic_app);
    defer allocator.free(@constCast(app_identity));
    const platform_identity = try packageIdentityFor(allocator, filesystem, .synthetic_platform);
    defer allocator.free(@constCast(platform_identity));

    try std.testing.expectEqualStrings(synthetic_app_identity, app_identity);
    try std.testing.expectEqualStrings(synthetic_platform_identity, platform_identity);
}

test "buildPackageKeys applies synthetic root and platform only to compiler-synthesized packages" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(std.testing.io, "app", .default_dir);
    try tmp.dir.createDir(std.testing.io, "platform", .default_dir);
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "app/main.roc", .data = "app [] {}\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "platform/main.roc", .data = "platform [] {}\n" });

    const tmp_root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_root);
    const app_path = try std.fs.path.join(allocator, &.{ tmp_root, "app", "main.roc" });
    defer allocator.free(app_path);
    const platform_path = try std.fs.path.join(allocator, &.{ tmp_root, "platform", "main.roc" });
    defer allocator.free(platform_path);
    const app_dir = try std.fs.path.join(allocator, &.{ tmp_root, "app" });
    defer allocator.free(app_dir);
    const platform_dir = try std.fs.path.join(allocator, &.{ tmp_root, "platform" });
    defer allocator.free(platform_dir);

    var deps = [_]package_resolution.Resolved.Dep{.{
        .alias = "pf",
        .target = 1,
        .is_platform = true,
    }};
    var empty_deps = [_]package_resolution.Resolved.Dep{};
    var packages = [_]package_resolution.Resolved.Package{
        .{
            .kind = .app,
            .identity = app_path,
            .root_file = app_path,
            .root_dir = app_dir,
            .root_source_hash = [_]u8{0} ** 32,
            .url = null,
            .deps = &deps,
        },
        .{
            .kind = .platform,
            .identity = platform_path,
            .root_file = platform_path,
            .root_dir = platform_dir,
            .root_source_hash = [_]u8{1} ** 32,
            .url = null,
            .deps = &empty_deps,
        },
    };
    var resolved = package_resolution.Resolved{
        .arena = undefined,
        .packages = &packages,
    };

    const filesystem = CoreCtx.os(allocator, allocator, std.testing.io);
    var real_keys = try buildPackageKeys(allocator, filesystem, &resolved, .{});
    defer real_keys.deinit();
    try std.testing.expect(!std.mem.eql(u8, real_keys.identity(0), synthetic_app_identity));
    try std.testing.expect(!std.mem.eql(u8, real_keys.identity(1), synthetic_platform_identity));

    var synthetic_keys = try buildPackageKeys(allocator, filesystem, &resolved, .{
        .synthetic_root = true,
        .synthetic_platform = true,
    });
    defer synthetic_keys.deinit();
    try std.testing.expectEqualStrings(synthetic_app_identity, synthetic_keys.identity(0));
    try std.testing.expectEqualStrings(synthetic_platform_identity, synthetic_keys.identity(1));
}

test "buildPackageKeys produces byte-equal mappings for check and run path spellings" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDir(std.testing.io, "app", .default_dir);
    try tmp.dir.createDir(std.testing.io, "platform", .default_dir);
    try tmp.dir.createDir(std.testing.io, "pkg", .default_dir);
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "app/main.roc", .data = "app [] {}\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "platform/main.roc", .data = "platform [] {}\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "pkg/main.roc", .data = "package [] {}\n" });

    const tmp_root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const app_check_path = try std.fs.path.join(allocator, &.{ tmp_root, "app", "main.roc" });
    defer allocator.free(app_check_path);
    const app_run_path = try std.fs.path.join(allocator, &.{ tmp_root, "app", "..", "app", "main.roc" });
    defer allocator.free(app_run_path);
    const platform_check_path = try std.fs.path.join(allocator, &.{ tmp_root, "platform", "main.roc" });
    defer allocator.free(platform_check_path);
    const platform_run_path = try std.fs.path.join(allocator, &.{ tmp_root, "platform", ".", "main.roc" });
    defer allocator.free(platform_run_path);
    const pkg_check_path = try std.fs.path.join(allocator, &.{ tmp_root, "pkg", "main.roc" });
    defer allocator.free(pkg_check_path);
    const pkg_run_path = try std.fs.path.join(allocator, &.{ tmp_root, "pkg", "..", "pkg", "main.roc" });
    defer allocator.free(pkg_run_path);

    const app_dir = try std.fs.path.join(allocator, &.{ tmp_root, "app" });
    defer allocator.free(app_dir);
    const platform_dir = try std.fs.path.join(allocator, &.{ tmp_root, "platform" });
    defer allocator.free(platform_dir);
    const pkg_dir = try std.fs.path.join(allocator, &.{ tmp_root, "pkg" });
    defer allocator.free(pkg_dir);

    var root_deps = [_]package_resolution.Resolved.Dep{
        .{ .alias = "pf", .target = 1, .is_platform = true },
        .{ .alias = "util", .target = 2, .is_platform = false },
    };
    var empty_deps = [_]package_resolution.Resolved.Dep{};
    var check_packages = [_]package_resolution.Resolved.Package{
        .{
            .kind = .app,
            .identity = app_check_path,
            .root_file = app_check_path,
            .root_dir = app_dir,
            .root_source_hash = [_]u8{0} ** 32,
            .url = null,
            .deps = &root_deps,
        },
        .{
            .kind = .platform,
            .identity = platform_check_path,
            .root_file = platform_check_path,
            .root_dir = platform_dir,
            .root_source_hash = [_]u8{1} ** 32,
            .url = null,
            .deps = &empty_deps,
        },
        .{
            .kind = .package,
            .identity = pkg_check_path,
            .root_file = pkg_check_path,
            .root_dir = pkg_dir,
            .root_source_hash = [_]u8{2} ** 32,
            .url = null,
            .deps = &empty_deps,
        },
    };
    var run_packages = check_packages;
    run_packages[0].identity = app_run_path;
    run_packages[0].root_file = app_run_path;
    run_packages[1].identity = platform_run_path;
    run_packages[1].root_file = platform_run_path;
    run_packages[2].identity = pkg_run_path;
    run_packages[2].root_file = pkg_run_path;

    var check_resolved = package_resolution.Resolved{ .arena = undefined, .packages = &check_packages };
    var run_resolved = package_resolution.Resolved{ .arena = undefined, .packages = &run_packages };

    const filesystem = CoreCtx.os(allocator, allocator, std.testing.io);
    var check_keys = try buildPackageKeys(allocator, filesystem, &check_resolved, .{});
    defer check_keys.deinit();
    var run_keys = try buildPackageKeys(allocator, filesystem, &run_resolved, .{});
    defer run_keys.deinit();

    try std.testing.expectEqual(check_keys.identities.len, run_keys.identities.len);
    for (check_keys.identities, run_keys.identities) |check_identity, run_identity| {
        try std.testing.expectEqualStrings(check_identity, run_identity);
    }

    var synthetic_check_keys = try buildPackageKeys(allocator, filesystem, &check_resolved, .{
        .synthetic_root = true,
        .synthetic_platform = true,
    });
    defer synthetic_check_keys.deinit();
    var synthetic_run_keys = try buildPackageKeys(allocator, filesystem, &run_resolved, .{
        .synthetic_root = true,
        .synthetic_platform = true,
    });
    defer synthetic_run_keys.deinit();

    try std.testing.expectEqual(synthetic_check_keys.identities.len, synthetic_run_keys.identities.len);
    for (synthetic_check_keys.identities, synthetic_run_keys.identities) |check_identity, run_identity| {
        try std.testing.expectEqualStrings(check_identity, run_identity);
    }
}
