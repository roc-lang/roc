//! Checked artifact cache keys.
//!
//! This cache key is semantic and target-independent. It names one complete
//! checked artifact, including compile-time values and every checked-stage table
//! consumed by later MIR-family stages. Object-code, layout, pointer-width, and
//! backend inputs belong to later target-specific caches only.

const std = @import("std");
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

test "checked artifact cache keys are target independent" {
    std.testing.refAllDecls(@This());
}
