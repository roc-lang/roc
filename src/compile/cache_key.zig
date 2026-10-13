//! Canonicalized-module cache keys.
//!
//! A key names one module's canonicalization output and depends only on the
//! inputs canonicalization reads. Checked-artifact keys live in
//! `check.CheckedArtifact`.

const std = @import("std");
const Sha256 = @import("base").Sha256;
const can = @import("can");

/// Names one module's canonicalization output in the canonicalized-module cache.
pub const CanonicalizedModuleCacheKey = struct {
    bytes: [32]u8 = @as([32]u8, @splat(0)),
};

/// Every input canonicalization reads, and nothing else.
///
/// Canonicalizing a module is a pure function of these, so an entry keyed by
/// them stays valid when any other module changes, and identical modules in
/// different packages, directories, or workspaces share one entry. Adding a
/// field that names a package, a path, an import, or the root module would
/// break that property, so the key deliberately has no way to express one.
pub const CanonicalizedCacheKeyInput = struct {
    /// The module's source bytes, after line-ending normalization.
    source: []const u8,
    /// The file name a type module's main type must match. Not the logical
    /// module path, which carries the module's location inside its package.
    module_basename: []const u8,
    /// Whether the compiler was pointed at this file, which gates the
    /// `default_app` classification.
    is_entry_module: bool,
    /// Whether this module is the compiler's own builtin source, which
    /// canonicalization reads directly (builtin low-level declarations,
    /// module-alias publication, builtin type wiring).
    module_role: can.ModuleEnv.ModuleRole,
    /// The post-canonicalization validation this module receives.
    validation: can.Can.Validation,
    /// The compiler that produced the entry.
    compiler_version: []const u8,
    /// The cache entry format's version hash: the serialized `ModuleEnv`
    /// layout, the parse-stage record layout, and the envelope version.
    entry_version_hash: [32]u8,
};

/// Compute the canonicalized-module cache key for one module.
pub fn canonicalizedModuleCacheKey(input: CanonicalizedCacheKeyInput) CanonicalizedModuleCacheKey {
    var hasher = Sha256.init(.{});
    hasher.update("roc-canonicalized-module-key-v2");
    hasher.update(&@import("build_options").compiler_compatibility_hash);
    hasher.update(&input.entry_version_hash);
    hashLengthPrefixed(&hasher, input.compiler_version);
    hashLengthPrefixed(&hasher, input.module_basename);
    hashLengthPrefixed(&hasher, input.source);
    hasher.update(&[_]u8{@intFromBool(input.is_entry_module)});
    hasher.update(&[_]u8{@backingInt(input.validation)});
    hasher.update(&[_]u8{@backingInt(input.module_role)});

    var key = CanonicalizedModuleCacheKey{};
    hasher.final(&key.bytes);
    return key;
}

/// Whether two canonicalized-module cache keys name the same entry.
pub fn canonicalizedKeyEql(a: CanonicalizedModuleCacheKey, b: CanonicalizedModuleCacheKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

/// Feed `bytes` with an explicit length, so no two different field splits
/// produce the same hash input.
fn hashLengthPrefixed(hasher: *Sha256, bytes: []const u8) void {
    var len_bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_bytes, bytes.len, .little);
    hasher.update(&len_bytes);
    hasher.update(bytes);
}

fn hashWithByte(byte: u8) [32]u8 {
    return @as([32]u8, @splat(byte));
}

fn canonicalizedTestInput() CanonicalizedCacheKeyInput {
    return .{
        .source = "main = 42",
        .module_basename = "Main.roc",
        .is_entry_module = true,
        .module_role = .user,
        .validation = .checking,
        .compiler_version = "roc-test-version",
        .entry_version_hash = hashWithByte(7),
    };
}

test "canonicalized module cache key is a function of exactly its declared inputs" {
    const baseline = canonicalizedModuleCacheKey(canonicalizedTestInput());

    // Identical inputs produce an identical key.
    try std.testing.expect(canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(canonicalizedTestInput())));

    var changed_source = canonicalizedTestInput();
    changed_source.source = "main = 43";
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_source)));

    var changed_basename = canonicalizedTestInput();
    changed_basename.module_basename = "Other.roc";
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_basename)));

    var changed_entry = canonicalizedTestInput();
    changed_entry.is_entry_module = false;
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_entry)));

    var changed_role = canonicalizedTestInput();
    changed_role.module_role = .builtin;
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_role)));

    var changed_validation = canonicalizedTestInput();
    changed_validation.validation = .explicit_roots;
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_validation)));

    var changed_compiler = canonicalizedTestInput();
    changed_compiler.compiler_version = "roc-other-version";
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_compiler)));

    var changed_format = canonicalizedTestInput();
    changed_format.entry_version_hash = hashWithByte(8);
    try std.testing.expect(!canonicalizedKeyEql(baseline, canonicalizedModuleCacheKey(changed_format)));
}

test "canonicalized module cache key cannot be split differently by its variable-length inputs" {
    // A length prefix on every variable-length input keeps one field's tail
    // from reading as the next field's head.
    var moved_boundary = canonicalizedTestInput();
    moved_boundary.module_basename = "Main.ro";
    moved_boundary.source = "cmain = 42";

    try std.testing.expect(!canonicalizedKeyEql(
        canonicalizedModuleCacheKey(canonicalizedTestInput()),
        canonicalizedModuleCacheKey(moved_boundary),
    ));
}

test "canonicalized module cache key input names nothing outside the module" {
    const field_names = @typeInfo(CanonicalizedCacheKeyInput).@"struct".field_names;
    inline for (field_names) |field_name| {
        try std.testing.expect(!std.mem.eql(u8, field_name, "package_name"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "path"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "module_name"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "qualified_module_name"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "imports"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "shorthands"));
        try std.testing.expect(!std.mem.eql(u8, field_name, "root_module"));
    }

    try std.testing.expectEqual(@as(usize, 7), field_names.len);
}
