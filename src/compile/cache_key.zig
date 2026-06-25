//! Checked artifact cache keys.
//!
//! This cache key is target-independent. It names one complete checked module,
//! including compile-time values and every checked-stage table consumed by
//! post-check lowering. Object code, layout, pointer width, and backend inputs
//! belong to later target-specific caches only.

const std = @import("std");
const Allocator = std.mem.Allocator;
const check = @import("check");

const CheckedArtifact = check.CheckedArtifact;

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
