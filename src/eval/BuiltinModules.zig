//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const builtin_loading = @import("builtin_loading.zig");
const builtins = @import("builtins.zig");

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const LoadedModule = builtin_loading.LoadedModule;
const BuiltinIndices = CIR.BuiltinIndices;
const BuiltinTypes = builtins.BuiltinTypes;
const CompactWriter = collections.CompactWriter;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

/// Information about a single builtin module
pub const ModuleInfo = struct {
    name: []const u8,
    module: *const LoadedModule,
};

/// Centralized container for all builtin modules
pub const BuiltinModules = struct {
    allocator: Allocator,
    builtin_module: LoadedModule,
    builtin_indices: BuiltinIndices,
    checked_artifact: CheckedModuleArtifact,
    /// 16-byte-aligned buffer holding the deserialized artifact's frozen
    /// sub-stores. The artifact's sub-stores alias this buffer, so it must live
    /// as long as the artifact.
    artifact_buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,

    /// Get an array of all builtin modules for iteration
    /// For compatibility, we expose the Builtin module for each auto-imported type
    pub fn modules(self: *const BuiltinModules) [3]ModuleInfo {
        return .{
            .{ .name = "Bool", .module = &self.builtin_module },
            .{ .name = "Try", .module = &self.builtin_module },
            .{ .name = "Str", .module = &self.builtin_module },
        };
    }

    /// Create a BuiltinTypes instance from these builtin modules
    pub fn asBuiltinTypes(self: *const BuiltinModules) BuiltinTypes {
        return BuiltinTypes.init(
            self.builtin_indices,
            self.builtin_module.env,
            self.builtin_module.env,
            self.builtin_module.env,
        );
    }

    /// Initialize all builtin modules by relocating the baked CheckedModuleArtifact.
    ///
    /// The artifact is produced at build time and embedded; loading it is a single
    /// copy-into-aligned-buffer plus an O(1) relocate, replacing the per-startup
    /// re-publish that previously cost several seconds in Debug builds.
    pub fn init(allocator: Allocator) anyerror!BuiltinModules {
        // Load the builtin indices
        const indices = try builtin_loading.deserializeBuiltinIndices(allocator, compiled_builtins.builtin_indices_bin);

        // Load the single Builtin module (env + buffer); the artifact's module_env
        // storage takes ownership of these.
        var builtin_module = try builtin_loading.loadCompiledModule(allocator, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        // Prepare the env exactly as the build-time compiler did before it published
        // the baked artifact: enable runtime ident inserts, ensure module-name
        // idents, and finalize method tables. This keeps the runtime env paired
        // identically with the baked artifact and leaves its interner heap-owned so
        // the `compiled_buffer` storage can free it on deinit.
        var typed_modules = try check.TypedCIR.Modules.init(allocator, &.{
            .{ .precompiled = builtin_module.env },
        });
        defer typed_modules.deinit();

        // The baked blob is the serialized artifact followed by a 32-byte
        // layout-version trailer. Validate the trailer against the running
        // compiler's layout hash before relocating: a mismatch means the embedded
        // blob is stale relative to this compiler (a build-system inconsistency),
        // which we reject rather than relocate into a differently-shaped struct.
        const artifact_bytes = compiled_builtins.builtin_artifact_bin;
        const hash_len = CheckedModuleArtifact.SERIALIZED_VERSION_HASH.len;
        if (artifact_bytes.len < hash_len) return error.CorruptBuiltinArtifact;
        const serialized_len = artifact_bytes.len - hash_len;
        if (!CheckedModuleArtifact.expectSerializedVersion(artifact_bytes[serialized_len..][0..hash_len])) {
            return error.BuiltinArtifactVersionMismatch;
        }

        // Copy the serialized region into a freshly-allocated 16-byte-aligned buffer
        // so the relocated sub-stores can alias it for the life of the artifact.
        const artifact_buffer = try allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, serialized_len);
        errdefer allocator.free(artifact_buffer);
        @memcpy(artifact_buffer, artifact_bytes[0..serialized_len]);

        const serialized: *const CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(artifact_buffer.ptr));
        const checked_artifact = serialized.deserialize(
            @intFromPtr(artifact_buffer.ptr),
            allocator,
            .{ .compiled_buffer = .{
                .env = builtin_module.env,
                .buffer = builtin_module.buffer,
            } },
        );

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
            .checked_artifact = checked_artifact,
            .artifact_buffer = artifact_buffer,
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *BuiltinModules) void {
        // The artifact is buffer-backed: every sub-store slice points into
        // `artifact_buffer`, so the artifact's own `deinit` (which unconditionally
        // frees several of those slices) must NOT run. Free only the two things the
        // baked artifact actually owns: the module_env storage (`compiled_buffer`,
        // which owns the builtin env + Builtin.bin buffer) and the artifact buffer.
        self.checked_artifact.module_env.deinit();
        self.allocator.free(self.artifact_buffer);
    }
};
