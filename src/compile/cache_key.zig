//! Checked artifact cache keys.
//!
//! This cache key is target-independent. It names one complete checked module,
//! including compile-time values and every checked-stage table consumed by
//! post-check lowering. Object code, layout, pointer width, and backend inputs
//! belong to later target-specific caches only.

const std = @import("std");
const Allocator = std.mem.Allocator;
const can = @import("can");
const check = @import("check");

const CheckedArtifact = check.CheckedArtifact;

/// Names one module's canonicalization output in the canonicalized-module cache.
pub const CanonicalizedModuleCacheKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
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
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-canonicalized-module-key-v1");
    hasher.update(&input.entry_version_hash);
    hashLengthPrefixed(&hasher, input.compiler_version);
    hashLengthPrefixed(&hasher, input.module_basename);
    hashLengthPrefixed(&hasher, input.source);
    hasher.update(&[_]u8{@intFromBool(input.is_entry_module)});
    hasher.update(&[_]u8{@intFromEnum(input.validation)});
    hasher.update(&[_]u8{@intFromEnum(input.module_role)});

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
fn hashLengthPrefixed(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    var len_bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_bytes, bytes.len, .little);
    hasher.update(&len_bytes);
    hasher.update(bytes);
}

/// Public `CheckedModuleArtifactKey` declaration.
pub const CheckedModuleArtifactKey = CheckedArtifact.CheckedModuleArtifactKey;

/// Public `DirectImportArtifactKey` declaration.
pub const DirectImportArtifactKey = struct {
    import_order: u32,
    key: CheckedModuleArtifactKey,
};

/// Public `CacheKeyInput` declaration.
pub const CacheKeyInput = struct {
    source: []const u8,
    module_identity: CheckedArtifact.ModuleIdentity,
    checking_context_identity: CheckedArtifact.CheckingContextIdentity,
    direct_import_artifact_keys: []const CheckedModuleArtifactKey,
};

/// Public `checkedModuleArtifactKey` function.
pub fn checkedModuleArtifactKey(input: CacheKeyInput) CheckedModuleArtifactKey {
    return CheckedModuleArtifactKey.compute(
        input.source,
        input.module_identity,
        input.checking_context_identity,
        input.direct_import_artifact_keys,
    );
}

/// Public `cacheFileName` function.
pub fn cacheFileName(allocator: std.mem.Allocator, key: CheckedModuleArtifactKey) std.mem.Allocator.Error![]u8 {
    const filename = try allocator.alloc(u8, key.bytes.len * 2);
    _ = std.fmt.bufPrint(filename, "{x}", .{&key.bytes}) catch unreachable;
    return filename;
}

/// Public `eql` function.
pub fn eql(a: CheckedModuleArtifactKey, b: CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn hashWithByte(byte: u8) [32]u8 {
    return [_]u8{byte} ** 32;
}

fn moduleIdentity(byte: u8) CheckedArtifact.ModuleIdentity {
    return .{
        .stable_hash = hashWithByte(byte),
        .module_idx = byte,
        .module_name = @enumFromInt(@as(u32, byte)),
        .display_module_name = @enumFromInt(@as(u32, byte + 1)),
        .qualified_module_name = @enumFromInt(@as(u32, byte + 2)),
        .kind = .app,
    };
}

fn checkingContext(byte: u8) CheckedArtifact.CheckingContextIdentity {
    return .{
        .imports = &.{},
        .platform_requirement_context = .{ .bytes = hashWithByte(byte) },
        .platform_app_relation = null,
    };
}

fn importKey(byte: u8) CheckedModuleArtifactKey {
    return .{ .bytes = hashWithByte(byte) };
}

fn testInput() CacheKeyInput {
    return .{
        .source = "main = 42",
        .module_identity = moduleIdentity(1),
        .checking_context_identity = checkingContext(2),
        .direct_import_artifact_keys = &.{importKey(3)},
    };
}

fn expectDifferent(a: CheckedModuleArtifactKey, b: CheckedModuleArtifactKey) error{TestUnexpectedResult}!void {
    try std.testing.expect(!eql(a, b));
}

test "checked artifact cache key changes for semantic inputs" {
    const baseline = checkedModuleArtifactKey(testInput());

    var changed_source = testInput();
    changed_source.source = "main = 43";
    const source_key = checkedModuleArtifactKey(changed_source);
    try expectDifferent(baseline, source_key);
    try std.testing.expect(!std.mem.eql(u8, &baseline.source_hash, &source_key.source_hash));

    var changed_module = testInput();
    changed_module.module_identity = moduleIdentity(4);
    const module_key = checkedModuleArtifactKey(changed_module);
    try expectDifferent(baseline, module_key);
    try std.testing.expect(!std.mem.eql(u8, &baseline.module_identity_hash, &module_key.module_identity_hash));

    var changed_context = testInput();
    changed_context.checking_context_identity = checkingContext(5);
    const context_key = checkedModuleArtifactKey(changed_context);
    try expectDifferent(baseline, context_key);
    try std.testing.expect(!std.mem.eql(u8, &baseline.checking_context_identity_hash, &context_key.checking_context_identity_hash));

    var changed_import = testInput();
    changed_import.direct_import_artifact_keys = &.{importKey(6)};
    const import_key = checkedModuleArtifactKey(changed_import);
    try expectDifferent(baseline, import_key);
    try std.testing.expect(!std.mem.eql(u8, &baseline.direct_import_artifact_keys_hash, &import_key.direct_import_artifact_keys_hash));
}

test "checked artifact cache key input has no target or layout ABI fields" {
    const fields = @typeInfo(CacheKeyInput).@"struct".fields;
    inline for (fields) |field| {
        try std.testing.expect(!std.mem.eql(u8, field.name, "target"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "target_config"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "layout"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "layout_abi"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "backend"));
    }

    try std.testing.expectEqual(@as(usize, 4), fields.len);
    _ = checkedModuleArtifactKey(testInput());
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
    const fields = @typeInfo(CanonicalizedCacheKeyInput).@"struct".fields;
    inline for (fields) |field| {
        try std.testing.expect(!std.mem.eql(u8, field.name, "package_name"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "path"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "module_name"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "qualified_module_name"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "imports"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "shorthands"));
        try std.testing.expect(!std.mem.eql(u8, field.name, "root_module"));
    }

    try std.testing.expectEqual(@as(usize, 7), fields.len);
}
