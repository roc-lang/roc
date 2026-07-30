//! Compiler-owned Roc platform sources.
//!
//! These platforms are not package URLs and are not resolved from user input.
//! The compiler embeds their source bytes, materializes them to an internal
//! source directory when the normal package pipeline needs file paths, and
//! gives them explicit package identities independent of that directory.

const std = @import("std");
const CoreCtx = @import("ctx").CoreCtx;
const cache_config = @import("cache_config.zig");
const compiler_platform_sources = @import("compiler_platform_sources");

const Allocator = std.mem.Allocator;

/// The platforms whose sources the compiler embeds and owns.
pub const CompilerOwnedPlatform = enum {
    glue,
};

/// Errors from materializing embedded platform sources to disk.
pub const MaterializeError = Allocator.Error || CoreCtx.MakePathError || CoreCtx.WriteError || error{NoHomeDirectory};

/// An embedded platform written out to the internal source directory.
pub const MaterializedPlatform = struct {
    root_file: []const u8,
    root_dir: []const u8,
    root_source_hash: [32]u8,
    content_bytes: u64,
};

const EmbeddedFile = compiler_platform_sources.File;

/// Look up a compiler-owned platform from its app-header ident (`glue`).
pub fn fromHeaderIdent(text: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, text, "glue")) return .glue;
    return null;
}

/// The platform's stable package identity, independent of any file path.
pub fn identity(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "roc:compiler/platform/glue",
    };
}

/// Look up a compiler-owned platform from its package identity string.
pub fn fromIdentity(text: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, text, identity(.glue))) return .glue;
    return null;
}

/// The resolver group key for the platform's dependency-graph node.
pub fn groupKey(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "c@roc:compiler/platform/glue",
    };
}

/// Look up a compiler-owned platform from its resolver group key.
pub fn fromGroupKey(group: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, group, groupKey(.glue))) return .glue;
    return null;
}

/// The header spec text an app writes to reference this platform.
pub fn headerSpecText(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "platform glue",
    };
}

/// The platform's embedded source files.
pub fn files(platform: CompilerOwnedPlatform) []const EmbeddedFile {
    return switch (platform) {
        .glue => &compiler_platform_sources.glue_files,
    };
}

/// A BLAKE3 hash over the platform's embedded sources, for cache keys
/// and plugin stamps.
pub fn sourceHash(platform: CompilerOwnedPlatform) [32]u8 {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(identity(platform));
    hasher.update(&[_]u8{0});
    for (files(platform)) |file| {
        hasher.update(file.path);
        hasher.update(&[_]u8{0});
        hasher.update(file.bytes);
        hasher.update(&[_]u8{0});
    }
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    return digest;
}

/// Write the platform's embedded sources into the internal source
/// directory so the package pipeline can consume them as files.
pub fn materialize(
    allocator: Allocator,
    fs: CoreCtx,
    source_root_override: ?[]const u8,
    platform: CompilerOwnedPlatform,
) MaterializeError!MaterializedPlatform {
    const root = if (source_root_override) |dir|
        try allocator.dupe(u8, dir)
    else blk: {
        const config = cache_config.CacheConfig{ .roc_ctx = fs };
        break :blk try config.getModuleCacheDir(allocator);
    };
    defer allocator.free(root);

    const hash = sourceHash(platform);
    const hash_hex = std.fmt.bytesToHex(hash, .lower);
    const platform_dir_name = switch (platform) {
        .glue => "glue",
    };
    const root_dir = try std.fs.path.join(allocator, &.{ root, "compiler-platforms", platform_dir_name, hash_hex[0..] });
    errdefer allocator.free(root_dir);

    try fs.makePath(root_dir);

    var content_bytes: u64 = 0;
    var root_source_hash: [32]u8 = undefined;
    for (files(platform)) |file| {
        const dest = try std.fs.path.join(allocator, &.{ root_dir, file.path });
        defer allocator.free(dest);
        try fs.writeFile(dest, file.bytes);
        content_bytes += file.bytes.len;
        if (std.mem.eql(u8, file.path, "main.roc")) {
            var sha = std.crypto.hash.sha2.Sha256.init(.{});
            sha.update(file.bytes);
            root_source_hash = sha.finalResult();
        }
    }

    const root_file = try std.fs.path.join(allocator, &.{ root_dir, "main.roc" });
    errdefer allocator.free(root_file);

    return .{
        .root_file = root_file,
        .root_dir = root_dir,
        .root_source_hash = root_source_hash,
        .content_bytes = content_bytes,
    };
}
