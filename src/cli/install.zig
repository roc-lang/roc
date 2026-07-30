//! Persistent install store for `roc install` and installed-shorthand lookups.
//!
//! Installed tools live outside the disposable compiler and package caches:
//! clearing a cache must never break an installed tool. Entries are namespaced
//! by compiler version so one compiler can never consume another's artifacts,
//! and each entry is published with staging + atomic rename so a partially
//! completed installation is never visible under its shorthand.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const CoreCtx = @import("ctx").CoreCtx;
const unbundle = @import("unbundle");
const Allocator = std.mem.Allocator;

/// Manifest file recorded at the root of every install entry.
pub const manifest_filename = "install.json";
/// Directory holding the extracted bundle source within an entry.
pub const source_dir_name = "source";
/// Directory holding built artifacts within an entry.
pub const bin_dir_name = "bin";
/// Bump when the entry layout or manifest schema changes incompatibly.
pub const manifest_format_version: u32 = 1;

/// How a CLI source argument is interpreted. Classification is purely
/// syntactic — no filesystem probing, and never a fallback from one
/// category to another.
pub const SourceRefKind = enum {
    /// A filesystem path (anything that is neither a URL nor a valid shorthand).
    local_path,
    /// An `https://` URL (or loopback `http://`, validated later by download).
    url,
    /// A bare token matching the installed-shorthand grammar.
    shorthand,
};

/// Classify a CLI source argument. A local extensionless file whose name
/// happens to match the shorthand grammar must be written `./name`.
pub fn classifySourceRef(input: []const u8) SourceRefKind {
    if (std.mem.startsWith(u8, input, "https://") or std.mem.startsWith(u8, input, "http://")) {
        return .url;
    }
    if (isValidShorthand(input)) {
        return .shorthand;
    }
    return .local_path;
}

/// The shorthand grammar: `[a-z][a-z0-9_]*`, minus Windows reserved device
/// names and the name `roc` itself. The grammar excludes `.`, dashes, path
/// separators, and uppercase so a shorthand can never be confused with a
/// path or a URL; reserved device names are rejected on every OS because a
/// shorthand becomes a directory and executable name, and the grammar must
/// stay portable; and `roc` is reserved so commands like `roc run roc`
/// cannot exist.
pub fn isValidShorthand(name: []const u8) bool {
    if (name.len == 0) return false;
    switch (name[0]) {
        'a'...'z' => {},
        else => return false,
    }
    for (name[1..]) |c| {
        switch (c) {
            'a'...'z', '0'...'9', '_' => {},
            else => return false,
        }
    }
    if (std.mem.eql(u8, name, "roc")) return false;
    for (unbundle.unbundle.WINDOWS_RESERVED_NAMES) |reserved| {
        if (std.ascii.eqlIgnoreCase(name, reserved)) return false;
    }
    return true;
}

/// What kind of artifact an install entry carries: an app built to an
/// executable that `roc run` executes, or a glue spec built to a plugin
/// dylib that `roc glue` loads.
pub const InstallKind = enum {
    executable,
    glue,
};

/// Metadata recorded for every install entry. The URL is the source's
/// compiler identity; the shorthand and paths are storage details.
pub const Manifest = struct {
    format_version: u32,
    kind: []const u8,
    url: []const u8,
    hash: []const u8,
    compiler_version: []const u8,
};

/// Parse a manifest's kind string. Null means the manifest is corrupt.
pub fn manifestKind(manifest: Manifest) ?InstallKind {
    if (std.mem.eql(u8, manifest.kind, "executable")) return .executable;
    if (std.mem.eql(u8, manifest.kind, "glue")) return .glue;
    return null;
}

/// Resolve the install root directory: `ROC_INSTALL_DIR` if set, otherwise a
/// platform-appropriate persistent user data directory (deliberately not a
/// cache location, so cache cleanup can never touch installed tools).
pub fn installRootDir(roc_ctx: CoreCtx, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})![]u8 {
    // Empty env values are treated as unset (per the XDG spec, and because a
    // cwd-relative install root would make installs appear and disappear
    // with the working directory).
    if (roc_ctx.getEnvVar("ROC_INSTALL_DIR", allocator)) |dir| {
        if (dir.len != 0) return dir;
        allocator.free(dir);
    } else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.EnvironmentVariableMissing => {},
    }

    if (builtin.target.os.tag != .windows) {
        if (roc_ctx.getEnvVar("XDG_DATA_HOME", allocator)) |xdg_data| {
            defer allocator.free(xdg_data);
            if (xdg_data.len != 0) {
                return std.fs.path.join(allocator, &.{ xdg_data, "roc" });
            }
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.EnvironmentVariableMissing => {},
        }
    }

    const home_env = switch (builtin.target.os.tag) {
        .windows => "APPDATA",
        else => "HOME",
    };
    const home_dir = roc_ctx.getEnvVar(home_env, allocator) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.EnvironmentVariableMissing => return error.NoHomeDirectory,
    };
    defer allocator.free(home_dir);
    if (home_dir.len == 0) return error.NoHomeDirectory;

    return switch (builtin.target.os.tag) {
        .windows => std.fs.path.join(allocator, &.{ home_dir, "Roc" }),
        .macos => std.fs.path.join(allocator, &.{ home_dir, "Library", "Application Support", "roc" }),
        else => std.fs.path.join(allocator, &.{ home_dir, ".local", "share", "roc" }),
    };
}

/// The compiler-version namespace directory under the install root.
pub fn versionDir(allocator: Allocator, install_root: []const u8) Allocator.Error![]u8 {
    return std.fs.path.join(allocator, &.{ install_root, build_options.compiler_version });
}

/// All the paths that make up one install entry.
pub const EntryPaths = struct {
    /// `<root>/<compiler_version>/<shorthand>`
    entry_dir: []const u8,
    /// `<entry>/install.json`
    manifest_path: []const u8,
    /// `<entry>/source`
    source_dir: []const u8,
    /// `<entry>/source/main.roc`
    main_roc_path: []const u8,
    /// `<entry>/bin`
    bin_dir: []const u8,
    /// `<entry>/bin/<shorthand>[.exe]`
    exe_path: []const u8,
    /// `<entry>/bin/<shorthand>.<dylib|so|dll>`
    glue_dylib_path: []const u8,

    /// The artifact path an entry of the given kind must contain.
    pub fn artifactPath(self: *const EntryPaths, kind: InstallKind) []const u8 {
        return switch (kind) {
            .executable => self.exe_path,
            .glue => self.glue_dylib_path,
        };
    }
};

/// Compute the paths for a shorthand's entry under a version directory.
pub fn entryPaths(allocator: Allocator, version_dir_path: []const u8, shorthand: []const u8) Allocator.Error!EntryPaths {
    const entry_dir = try std.fs.path.join(allocator, &.{ version_dir_path, shorthand });
    return entryPathsIn(allocator, entry_dir, shorthand);
}

/// Compute the intra-entry paths for an entry rooted at `entry_dir` (also
/// used for staging directories before they are renamed into place).
pub fn entryPathsIn(allocator: Allocator, entry_dir: []const u8, shorthand: []const u8) Allocator.Error!EntryPaths {
    const exe_filename = if (builtin.target.os.tag == .windows)
        try std.fmt.allocPrint(allocator, "{s}.exe", .{shorthand})
    else
        shorthand;
    const dylib_ext = switch (builtin.target.os.tag) {
        .windows => ".dll",
        .macos => ".dylib",
        else => ".so",
    };
    const dylib_filename = try std.fmt.allocPrint(allocator, "{s}{s}", .{ shorthand, dylib_ext });
    return .{
        .entry_dir = entry_dir,
        .manifest_path = try std.fs.path.join(allocator, &.{ entry_dir, manifest_filename }),
        .source_dir = try std.fs.path.join(allocator, &.{ entry_dir, source_dir_name }),
        .main_roc_path = try std.fs.path.join(allocator, &.{ entry_dir, source_dir_name, "main.roc" }),
        .bin_dir = try std.fs.path.join(allocator, &.{ entry_dir, bin_dir_name }),
        .exe_path = try std.fs.path.join(allocator, &.{ entry_dir, bin_dir_name, exe_filename }),
        .glue_dylib_path = try std.fs.path.join(allocator, &.{ entry_dir, bin_dir_name, dylib_filename }),
    };
}

/// Serialize a manifest to JSON. Caller owns the returned slice.
pub fn manifestToJson(allocator: Allocator, manifest: Manifest) Allocator.Error![]u8 {
    var buffer: std.Io.Writer.Allocating = .init(allocator);
    defer buffer.deinit();
    std.json.Stringify.value(manifest, .{}, &buffer.writer) catch return error.OutOfMemory;
    return buffer.toOwnedSlice();
}

/// A parsed manifest together with its backing JSON allocation.
pub const ParsedManifest = struct {
    parsed: std.json.Parsed(Manifest),

    pub fn manifest(self: *const ParsedManifest) Manifest {
        return self.parsed.value;
    }

    pub fn deinit(self: *ParsedManifest) void {
        self.parsed.deinit();
    }
};

/// Parse and validate manifest JSON. Returns null when the bytes are not a
/// valid manifest of the current format version — callers must treat that as
/// a corrupt entry, never fall back to guessing.
pub fn parseManifest(allocator: Allocator, bytes: []const u8) Allocator.Error!?ParsedManifest {
    const parsed = std.json.parseFromSlice(Manifest, allocator, bytes, .{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return null,
    };
    if (parsed.value.format_version != manifest_format_version or manifestKind(parsed.value) == null) {
        var owned = parsed;
        owned.deinit();
        return null;
    }
    return .{ .parsed = parsed };
}

test "shorthand grammar" {
    try std.testing.expect(isValidShorthand("tokei"));
    try std.testing.expect(isValidShorthand("rust_glue"));
    try std.testing.expect(isValidShorthand("a"));
    try std.testing.expect(isValidShorthand("a2_b_c"));

    try std.testing.expect(!isValidShorthand(""));
    try std.testing.expect(!isValidShorthand("Tokei"));
    try std.testing.expect(!isValidShorthand("2fast"));
    try std.testing.expect(!isValidShorthand("rust-glue"));
    try std.testing.expect(!isValidShorthand("-dash"));
    try std.testing.expect(!isValidShorthand("_under"));
    try std.testing.expect(!isValidShorthand("has.dot"));
    try std.testing.expect(!isValidShorthand("has/slash"));
    try std.testing.expect(!isValidShorthand("has\\backslash"));
    try std.testing.expect(!isValidShorthand("has space"));
    try std.testing.expect(!isValidShorthand("."));
    try std.testing.expect(!isValidShorthand(".."));

    // `roc` is reserved so `roc run roc` cannot exist.
    try std.testing.expect(!isValidShorthand("roc"));
    try std.testing.expect(isValidShorthand("rocky"));

    // Windows reserved device names are rejected on every OS so the grammar
    // stays portable.
    try std.testing.expect(!isValidShorthand("con"));
    try std.testing.expect(!isValidShorthand("nul"));
    try std.testing.expect(!isValidShorthand("aux"));
    try std.testing.expect(!isValidShorthand("prn"));
    try std.testing.expect(!isValidShorthand("com1"));
    try std.testing.expect(!isValidShorthand("lpt9"));
    try std.testing.expect(isValidShorthand("console"));
    try std.testing.expect(isValidShorthand("com"));
    try std.testing.expect(isValidShorthand("com10"));
}

test "source ref classification" {
    try std.testing.expectEqual(SourceRefKind.url, classifySourceRef("https://example.com/tokei/1.2.3/abc.tar.zst"));
    try std.testing.expectEqual(SourceRefKind.url, classifySourceRef("http://127.0.0.1:8000/abc.tar.zst"));

    try std.testing.expectEqual(SourceRefKind.shorthand, classifySourceRef("tokei"));
    try std.testing.expectEqual(SourceRefKind.shorthand, classifySourceRef("rust_glue"));

    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("main.roc"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("./tokei"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("../tokei"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("dir/tokei"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("/abs/tokei"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("Tokei"));
    try std.testing.expectEqual(SourceRefKind.local_path, classifySourceRef("foo.roc"));
}

test "manifest json round trip" {
    const gpa = std.testing.allocator;
    const json = try manifestToJson(gpa, .{
        .format_version = manifest_format_version,
        .kind = "executable",
        .url = "https://example.com/tokei/1.2.3/abc.tar.zst",
        .hash = "abc",
        .compiler_version = "test-version",
    });
    defer gpa.free(json);

    var parsed = (try parseManifest(gpa, json)) orelse return error.TestUnexpectedResult;
    defer parsed.deinit();
    try std.testing.expectEqualStrings("https://example.com/tokei/1.2.3/abc.tar.zst", parsed.manifest().url);
    try std.testing.expectEqualStrings("abc", parsed.manifest().hash);
    try std.testing.expectEqual(InstallKind.executable, manifestKind(parsed.manifest()).?);
}

test "manifest rejects wrong format version" {
    const gpa = std.testing.allocator;
    const json = try manifestToJson(gpa, .{
        .format_version = manifest_format_version + 1,
        .kind = "executable",
        .url = "https://example.com/x/1.0.0/abc.tar.zst",
        .hash = "abc",
        .compiler_version = "test-version",
    });
    defer gpa.free(json);

    try std.testing.expect((try parseManifest(gpa, json)) == null);
    try std.testing.expect((try parseManifest(gpa, "not json at all")) == null);
}
