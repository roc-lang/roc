//! Combined typing and finalized checked-module cache envelope.
const std = @import("std");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const cache_versioning = @import("cache_versioning.zig");
const ModuleEnv = can.ModuleEnv;
const CheckedArtifact = check.CheckedArtifact;
const CompactWriter = collections.CompactWriter;
const Allocator = std.mem.Allocator;
pub const CacheData = @import("cache_module.zig").CacheModule.CacheData;

/// Serialized metadata owns an allocation or borrows the checked mapping.
pub const Metadata = struct {
    bytes: []align(16) const u8,
    owned: bool,
    pub fn deinit(self: Metadata, allocator: Allocator) void {
        if (self.owned) allocator.free(self.bytes);
    }
};

/// Exact typing inputs owned until the scheduled checking worker finishes.
pub const Context = struct {
    allocator: Allocator,
    key: CheckedArtifact.CheckedModuleArtifactKey,
    scope: check.CheckingDependencies.Scope,
    owner_envs: []const *const ModuleEnv,
    cached: ?CacheData = null,
    pub fn deinit(self: *Context) void {
        if (self.cached) |data| data.deinit(self.allocator);
        self.scope.deinit();
        self.allocator.free(self.owner_envs);
        self.allocator.destroy(self);
    }
};

/// Byte prefix identifying the checked-module cache envelope format.
pub const checked_module_cache_magic = "roc-mod-cache-v14";

// The envelope identifies the typing-addressed entry and its full finalized
// snapshot independently. Aligned bodies hold the environment once, publication
// metadata, the finalized artifact, then its reports. Admission checks version, identities,
// and expected lengths against loaded bytes; never hash or recursively scan the
// payload for integrity. Atomic replacement leaves readers' mappings intact.
const checked_module_cache_header_fields_len: usize = checked_module_cache_magic.len + 32 + 32 + 8 + 8 + 32 + 32 + 8 + 8;

/// Header size including padding that aligns the first serialized body.
pub const checked_module_cache_header_len: usize = std.mem.alignForward(usize, checked_module_cache_header_fields_len, CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits());

/// Zero bytes after a body so the following serialized body is aligned.
pub fn checkedModuleCacheEnvPadding(env_len: usize) usize {
    return std.mem.alignForward(usize, env_len, CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) - env_len;
}

/// Zero-filled buffer for writing alignment padding between serialized bodies.
pub const checked_module_cache_zero_padding: [CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()]u8 = @splat(0);

/// The cache entry kinds written in the checked-module envelope.
pub const CheckedEnvelopeEntry = enum {
    checked_module,
    platform_pairing,

    fn versioned(comptime self: CheckedEnvelopeEntry) cache_versioning.Entry {
        return switch (self) {
            .checked_module => .checked_module,
            .platform_pairing => .platform_pairing,
        };
    }
};

/// A decoded checked-module cache entry's relocatable env and artifact bodies.
pub const CheckedModuleCacheBodies = struct {
    complete_key: CheckedArtifact.CheckedModuleArtifactKey,
    metadata_body: []const u8,
    env_body: []const u8,
    artifact_body: []const u8,
    reports_body: []const u8,
    pin_validation_hash: [32]u8,
};

/// Write the fixed-size checked-module cache header into the first
/// `checked_module_cache_header_len` bytes of `dest`. The caller gathers the
/// relocatable bodies (env blob then artifact blob) into `dest` after the header.
pub fn writeCheckedModuleCacheHeader(
    comptime entry: CheckedEnvelopeEntry,
    dest: []u8,
    key: check.CheckedArtifact.CheckedModuleArtifactKey,
    env_len: usize,
    artifact_len: usize,
    reports_len: usize,
    pin_validation_hash: [32]u8,
) void {
    writeTypedHeader(entry, dest, key, key, env_len, 0, artifact_len, reports_len, pin_validation_hash);
}

/// Write the envelope header with separate storage and completed-module keys.
/// `dest` must hold the full header; the caller writes the aligned env, metadata
/// and artifact bodies after it.
pub fn writeTypedHeader(
    comptime entry: CheckedEnvelopeEntry,
    dest: []u8,
    storage_key: CheckedArtifact.CheckedModuleArtifactKey,
    complete_key: CheckedArtifact.CheckedModuleArtifactKey,
    env_len: usize,
    metadata_len: usize,
    artifact_len: usize,
    reports_len: usize,
    pin_validation_hash: [32]u8,
) void {
    var offset: usize = 0;
    @memcpy(dest[offset..][0..checked_module_cache_magic.len], checked_module_cache_magic);
    offset += checked_module_cache_magic.len;
    @memcpy(dest[offset..][0..32], &cache_versioning.entryVersionHash(entry.versioned()));
    offset += 32;
    @memcpy(dest[offset..][0..32], &storage_key.bytes);
    offset += 32;
    std.mem.writeInt(u64, dest[offset..][0..8], env_len, .little);
    offset += 8;
    std.mem.writeInt(u64, dest[offset..][0..8], artifact_len, .little);
    offset += 8;
    @memcpy(dest[offset..][0..32], &pin_validation_hash);
    offset += 32;
    @memcpy(dest[offset..][0..32], &complete_key.bytes);
    offset += 32;
    std.mem.writeInt(u64, dest[offset..][0..8], metadata_len, .little);
    offset += 8;
    std.mem.writeInt(u64, dest[offset..][0..8], reports_len, .little);
    offset += 8;
    @memset(dest[offset..checked_module_cache_header_len], 0);
}

/// Validate the envelope version, storage key and body lengths, returning slices
/// borrowed from `bytes`. Returns null for an incompatible or malformed envelope
/// without inspecting body contents.
pub fn decodeCheckedCacheEnvelope(
    comptime entry: CheckedEnvelopeEntry,
    key: check.CheckedArtifact.CheckedModuleArtifactKey,
    bytes: []const u8,
) ?CheckedModuleCacheBodies {
    if (bytes.len < checked_module_cache_header_len) return null;
    var offset: usize = 0;
    if (!std.mem.eql(u8, bytes[offset..][0..checked_module_cache_magic.len], checked_module_cache_magic)) return null;
    offset += checked_module_cache_magic.len;
    // Reject (as a cache miss) an entry whose envelope or body layouts differ
    // from this compiler's.
    if (!std.mem.eql(u8, bytes[offset..][0..32], &cache_versioning.entryVersionHash(entry.versioned()))) return null;
    offset += 32;
    if (!std.mem.eql(u8, bytes[offset..][0..32], &key.bytes)) return null;
    offset += 32;
    // Cast the on-disk u64 lengths to usize up front: this both keeps every
    // downstream index/slice in usize (so the cache compiles for 32-bit targets
    // like wasm32) and rejects a corrupt entry whose length cannot fit the host
    // address space as a cache miss rather than crashing.
    const env_len = std.math.cast(usize, std.mem.readInt(u64, bytes[offset..][0..8], .little)) orelse return null;
    offset += 8;
    const artifact_len = std.math.cast(usize, std.mem.readInt(u64, bytes[offset..][0..8], .little)) orelse return null;
    offset += 8;
    const pin_validation_hash = bytes[offset..][0..32].*;
    offset += 32;
    const complete_key: CheckedArtifact.CheckedModuleArtifactKey = .{ .bytes = bytes[offset..][0..32].* };
    offset += 32;
    const metadata_len = std.math.cast(usize, std.mem.readInt(u64, bytes[offset..][0..8], .little)) orelse return null;
    offset += 8;
    const reports_len = std.math.cast(usize, std.mem.readInt(u64, bytes[offset..][0..8], .little)) orelse return null;
    offset = checked_module_cache_header_len;

    const remaining = bytes.len - offset;
    if (env_len > remaining) return null;
    const env_padding = checkedModuleCacheEnvPadding(env_len);
    if (env_padding > remaining - env_len) return null;
    const after_env = remaining - env_len - env_padding;
    if (metadata_len > after_env) return null;
    const metadata_padding = checkedModuleCacheEnvPadding(metadata_len);
    if (metadata_padding > after_env - metadata_len) return null;
    const after_metadata = after_env - metadata_len - metadata_padding;
    if (artifact_len > after_metadata) return null;
    const artifact_padding = if (reports_len == 0) 0 else checkedModuleCacheEnvPadding(artifact_len);
    if (artifact_padding > after_metadata - artifact_len) return null;
    if (reports_len != after_metadata - artifact_len - artifact_padding) return null;

    const env_body = bytes[offset..][0..env_len];
    offset += env_len + env_padding;
    const metadata_body = bytes[offset..][0..metadata_len];
    offset += metadata_len + metadata_padding;
    const artifact_body = bytes[offset..][0..artifact_len];
    offset += artifact_len + artifact_padding;
    const reports_body = bytes[offset..][0..reports_len];

    return .{
        .complete_key = complete_key,
        .metadata_body = metadata_body,
        .env_body = env_body,
        .artifact_body = artifact_body,
        .reports_body = reports_body,
        .pin_validation_hash = pin_validation_hash,
    };
}

/// Decode a checked-module envelope and require its env and artifact bodies to
/// hold their serialized headers. Returned body slices borrow `bytes`.
pub fn decodeCheckedModuleCacheEntry(key: CheckedArtifact.CheckedModuleArtifactKey, bytes: []const u8) ?CheckedModuleCacheBodies {
    const bodies = decodeCheckedCacheEnvelope(.checked_module, key, bytes) orelse return null;
    if (bodies.env_body.len < @sizeOf(ModuleEnv.Serialized)) return null;
    if (bodies.artifact_body.len < @sizeOf(CheckedArtifact.CheckedModuleArtifact.Serialized)) return null;
    if (bodies.reports_body.len != 0 and bodies.reports_body.len < @sizeOf(check.CheckedReports.Serialized)) return null;
    return bodies;
}

/// Restore only typing. The finalized artifact body is deliberately untouched.
pub fn restoreTyping(context: *const Context, env: *ModuleEnv) Allocator.Error!?check.CheckingReuse {
    const data = context.cached orelse return null;
    const bodies = decodeCheckedModuleCacheEntry(context.key, data.data()) orelse return null;
    if (bodies.metadata_body.len < @sizeOf(check.CheckingReuse.Serialized)) return null;
    const encoded: *const check.CheckingReuse.Serialized = @ptrCast(@alignCast(bodies.metadata_body.ptr));
    const metadata = encoded.deserialize(@intFromPtr(encoded));
    if (!metadata.outcome.sourcesMatch(&context.scope)) return null;
    const serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(bodies.env_body.ptr));
    const cached = try serialized.deserializeOwned(@intFromPtr(serialized), env.gpa, env.common.source, env.module_name);
    defer env.gpa.destroy(cached);
    errdefer cached.deinit();
    metadata.bindReferences(cached, &context.scope) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IncompatibleReferences => {
            cached.deinit();
            return null;
        },
    };
    try check.CheckingDependencies.bindOwners(cached, &context.scope);
    cached.qualified_module_name = env.qualified_module_name;
    env.deinit();
    env.* = cached.*;
    return metadata;
}

/// Serialize publication inputs only; the entry's env body owns solved types.
pub fn captureMetadata(allocator: Allocator, checker: *const check.Check, scope: *const check.CheckingDependencies.Scope) Allocator.Error!Metadata {
    var arena = @import("base").SingleThreadArena.init(allocator);
    defer arena.deinit();
    const output = try check.CheckingReuse.fromChecker(arena.allocator(), checker, scope);
    var writer = CompactWriter.init();
    const serialized = try writer.appendAlloc(arena.allocator(), check.CheckingReuse.Serialized);
    try serialized.serialize(&output, arena.allocator(), &writer);
    const bytes = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = writer.writeToBuffer(bytes) catch unreachable; // bytes has the writer's exact total length
    return .{ .bytes = bytes, .owned = true };
}
