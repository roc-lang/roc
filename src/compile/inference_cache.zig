//! Content-addressed inference capsules. Each holds solved source plus the
//! checker's publication inputs, never evaluated values or a finalized artifact.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const CacheManager = @import("cache_manager.zig").CacheManager;
const CacheData = @import("cache_module.zig").CacheModule.CacheData;
const versioning = @import("cache_versioning.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const header_len = 128;
const magic = "ROCINFR1";

/// An inference namespace in the coordinator's existing content-addressed cache.
pub const Cache = struct {
    manager: *CacheManager,
    directory: []const u8,

    pub const Loaded = struct {
        data: CacheData,
        metadata: check.InferenceOutput,
        allocator: Allocator,

        pub fn deinit(self: Loaded) void {
            self.data.deinit(self.allocator);
        }
    };

    /// Cache decoding is optional and fully precedes replacement of current
    /// source. Successful admission relocates explicit foreign locators and
    /// binds the dense owner table without changing solved relations.
    pub fn load(self: Cache, key: [32]u8, env: *ModuleEnv, scope: *const check.CheckingDependencies.Scope) Allocator.Error!?Loaded {
        const data = self.manager.loadRawBytesMappedIn(env.gpa, .inference, key, self.directory) orelse return null;
        var owned = true;
        defer if (owned) data.deinit(env.gpa);
        const bodies = decode(key, data.data()) catch {
            self.manager.recordInvalidationFor(.inference);
            return null;
        };
        if (!bodies.metadata.outcome.sourcesMatch(scope)) {
            self.manager.recordInvalidationFor(.inference);
            return null;
        }
        const serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(bodies.env.ptr));
        const cached = try serialized.deserializeOwned(@intFromPtr(bodies.env.ptr), env.gpa, env.common.source, env.module_name);
        defer env.gpa.destroy(cached);
        errdefer cached.deinit();
        bodies.metadata.bindReferences(cached, scope) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.IncompatibleReferences => {
                cached.deinit();
                self.manager.recordInvalidationFor(.inference);
                return null;
            },
        };
        try check.CheckingDependencies.bindOwners(cached, scope);
        cached.qualified_module_name = env.qualified_module_name;
        env.deinit();
        env.* = cached.*;
        owned = false;
        return .{ .data = data, .metadata = bodies.metadata, .allocator = env.gpa };
    }

    /// Store the complete pre-evaluation outcome. Evaluation reports are
    /// generated anew after publication, including on inference hits.
    pub fn store(self: Cache, key: [32]u8, env: *const ModuleEnv, checker: *const check.Check, scope: *const check.CheckingDependencies.Scope) void {
        self.storeFallible(key, env, checker, scope) catch self.manager.recordStoreFailureFor(.inference);
    }

    fn storeFallible(self: Cache, key: [32]u8, env: *const ModuleEnv, checker: *const check.Check, scope: *const check.CheckingDependencies.Scope) Allocator.Error!void {
        var arena = base.SingleThreadArena.init(env.gpa);
        defer arena.deinit();
        const allocator = arena.allocator();
        const output = try check.InferenceOutput.fromChecker(allocator, checker, scope);
        const buffer = try encode(allocator, key, env, &output);
        self.manager.storeRawBytesIn(env.gpa, .inference, key, buffer, self.directory, env.module_name);
    }
};

/// Allocate the envelope and both independently relocatable bodies in an arena.
fn encode(allocator: Allocator, key: [32]u8, env: *const ModuleEnv, output: *const check.InferenceOutput) Allocator.Error![]u8 {
    var env_writer = CompactWriter.init();
    const serialized_env = try env_writer.appendAlloc(allocator, ModuleEnv.Serialized);
    try serialized_env.serialize(env, allocator, &env_writer);
    var output_writer = CompactWriter.init();
    const serialized_output = try output_writer.appendAlloc(allocator, check.InferenceOutput.Serialized);
    try serialized_output.serialize(output, allocator, &output_writer);
    const output_start = header_len + std.mem.alignForward(usize, env_writer.total_bytes, 16);
    const buffer = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, output_start + output_writer.total_bytes);
    @memset(buffer, 0);
    @memcpy(buffer[0..8], magic);
    @memcpy(buffer[8..40], &versioning.entryVersionHash(.inference));
    @memcpy(buffer[40..72], &key);
    std.mem.writeInt(u64, buffer[104..112], env_writer.total_bytes, .little);
    std.mem.writeInt(u64, buffer[112..120], output_writer.total_bytes, .little);
    _ = env_writer.writeToBuffer(buffer[header_len..][0..env_writer.total_bytes]) catch unreachable; // exact writer-sized slice
    _ = output_writer.writeToBuffer(buffer[output_start..]) catch unreachable; // exact writer-sized slice
    var hash = base.Sha256.init(.{});
    hash.update(buffer[104..]);
    @memcpy(buffer[72..104], &hash.finalResult());
    return buffer;
}

const Bodies = struct { env: []const u8, metadata: check.InferenceOutput };

fn decode(key: [32]u8, bytes: []const u8) error{CorruptArtifact}!Bodies {
    if (@intFromPtr(bytes.ptr) % CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits() != 0) return error.CorruptArtifact;
    if (bytes.len < header_len or !std.mem.eql(u8, bytes[0..8], magic) or
        !std.mem.eql(u8, bytes[8..40], &versioning.entryVersionHash(.inference)) or
        !std.mem.eql(u8, bytes[40..72], &key)) return error.CorruptArtifact;
    var hash = base.Sha256.init(.{});
    hash.update(bytes[104..]);
    if (!std.mem.eql(u8, bytes[72..104], &hash.finalResult())) return error.CorruptArtifact;
    const env_len = std.math.cast(usize, std.mem.readInt(u64, bytes[104..112], .little)) orelse return error.CorruptArtifact;
    const output_len = std.math.cast(usize, std.mem.readInt(u64, bytes[112..120], .little)) orelse return error.CorruptArtifact;
    if (env_len > bytes.len - header_len or env_len < @sizeOf(ModuleEnv.Serialized)) return error.CorruptArtifact;
    const output_start = header_len + std.mem.alignForward(usize, env_len, 16);
    if (output_start > bytes.len or output_len != bytes.len - output_start or output_len < @sizeOf(check.InferenceOutput.Serialized))
        return error.CorruptArtifact;
    const env = bytes[header_len..][0..env_len];
    const serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(env.ptr));
    try serialized.validate(env.len);
    const output: *const check.InferenceOutput.Serialized = @ptrCast(@alignCast(bytes[output_start..].ptr));
    try check.validateSerialized(check.InferenceOutput.Serialized, output, output_len);
    const metadata = output.deserialize(@intFromPtr(output));
    try metadata.validate();
    return .{ .env = env, .metadata = metadata };
}

test "inference capsule rejects truncation corruption incompatible versions and wrong keys" {
    const allocator = std.testing.allocator;
    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.initCIRFields("Capsule");
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const key: [32]u8 = @splat(7);
    const bytes = try encode(arena.allocator(), key, &env, &.{});
    const bodies = try decode(key, bytes);
    try std.testing.expectEqual(@as(usize, 0), bodies.metadata.hoisted_roots.len);
    try std.testing.expectError(error.CorruptArtifact, decode(@splat(8), bytes));
    for ([_]usize{ 0, header_len - 1, header_len, bytes.len - 1 }) |len|
        try std.testing.expectError(error.CorruptArtifact, decode(key, bytes[0..len]));
    for ([_]usize{ 0, 8, 40, 72, 104, 112, header_len, bytes.len - 1 }) |offset| {
        bytes[offset] ^= 1;
        try std.testing.expectError(error.CorruptArtifact, decode(key, bytes));
        bytes[offset] ^= 1;
    }
    _ = try decode(key, bytes);
}
