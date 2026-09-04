//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const builtin_static = can.BuiltinStatic;

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const BuiltinModuleView = builtin_static.BuiltinModuleView;
const BuiltinIndices = CIR.BuiltinIndices;
const CompactWriter = collections.CompactWriter;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

/// Centralized container for all builtin modules
pub const BuiltinModules = struct {
    allocator: Allocator,
    /// NON-OWNING handle to the builtin module env. The env wrapper is owned by
    /// `checked_artifact` through `.static_builtin`; the bytes it views are the
    /// compiler executable's static data.
    builtin_module: BuiltinModuleView,
    builtin_indices: BuiltinIndices,
    /// Owner-minted admission capability for the immutable Builtin env.
    validated_module: check.Check.OwnedValidatedModuleEnv,
    /// Self-describing frozen artifact whose sub-stores alias static embedded
    /// bytes. `deinit` tears down only the env wrapper, never the static backing.
    checked_artifact: CheckedModuleArtifact,

    pub const InitError = Allocator.Error || error{ CorruptBuiltinArtifact, BuiltinArtifactVersionMismatch, CorruptArtifact, StaleEmbeddedBuiltins, CorruptEmbeddedBuiltins };

    /// Initialize builtin handles over the baked static ModuleEnv and artifact.
    pub fn init(allocator: Allocator) InitError!BuiltinModules {
        const indices = compiled_builtins.builtinIndices(CIR);
        if (builtin.mode == .Debug) {
            try builtin_static.validateBuiltinManifest(
                compiled_builtins.builtin_type_registry_hash,
                compiled_builtins.builtin_indices_layout_hash,
            );
        }

        var builtin_module = try builtin_static.moduleView(allocator, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        if (builtin.mode == .Debug) {
            try builtin_static.validateBuiltinIndices(builtin_module.env, indices);
        }
        var validated_module = try check.Check.admitBuiltinOwned(allocator, builtin_module.env, indices);
        errdefer validated_module.deinit();

        // The baked blob is the serialized artifact followed by a layout-version
        // trailer. Validate and strip it before relocating: a mismatch means the
        // embedded blob is stale relative to this compiler (a build-system
        // inconsistency), which we reject rather than relocate into a differently-
        // shaped struct.
        const artifact_blob = compiled_builtins.builtin_artifact_bin[0..];
        const serialized_bytes = try CheckedModuleArtifact.splitVersionTrailer(artifact_blob);
        const artifact_buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 =
            @alignCast(artifact_blob[0..serialized_bytes.len]);

        const serialized: *const CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(artifact_buffer.ptr));
        // L-10: bounds-check every relocatable marker against the buffer before any
        // sub-store aliases it, so a corrupt/truncated baked blob fails cleanly.
        try serialized.validate(artifact_buffer.len);
        const checked_artifact = serialized.deserializeStatic(
            artifact_buffer,
            allocator,
            .{ .static_builtin = builtin_module.env },
        );

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
            .validated_module = validated_module,
            .checked_artifact = checked_artifact,
        };
    }

    /// Clean up the small view objects. Static builtin bytes remain owned by the
    /// compiler executable for the process lifetime.
    pub fn deinit(self: *BuiltinModules) void {
        self.validated_module.deinit();
        self.checked_artifact.deinit(self.allocator);
    }
};

test "BuiltinModules W6b semantic validation rejects invalid self identities" {
    const allocator = std.testing.allocator;
    const indices = compiled_builtins.builtinIndices(CIR);
    var builtin_module = try builtin_static.moduleView(
        allocator,
        compiled_builtins.builtin_bin[0..],
        "Builtin",
        compiled_builtins.builtin_source,
    );
    defer builtin_module.deinit();

    const saved_identity = builtin_module.env.self_module_identity;
    builtin_module.env.self_module_identity = base.ModuleIdentity.Idx.NONE;
    try std.testing.expectError(
        error.CorruptArtifact,
        check.Check.admitBuiltinOwned(allocator, builtin_module.env, indices),
    );

    builtin_module.env.self_module_identity = @enumFromInt(builtin_module.env.module_identities.count());
    try std.testing.expectError(
        error.CorruptArtifact,
        check.Check.admitBuiltinOwned(allocator, builtin_module.env, indices),
    );

    builtin_module.env.self_module_identity = saved_identity;
    var failing_allocator = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    try std.testing.expectError(
        error.OutOfMemory,
        check.Check.admitBuiltinOwned(failing_allocator.allocator(), builtin_module.env, indices),
    );
    try std.testing.expect(!builtin_module.env.w6b_semantically_validated);

    var admitted = try check.Check.admitBuiltinOwned(allocator, builtin_module.env, indices);
    defer admitted.deinit();
    try std.testing.expect(builtin_module.env.w6b_semantically_validated);
    try std.testing.expectError(
        error.CorruptArtifact,
        check.Check.admitBuiltinOwned(allocator, builtin_module.env, indices),
    );
    try std.testing.expect(builtin_module.env.w6b_semantically_validated);
    try std.testing.expectEqual(builtin_module.env, try admitted.capability().validate());
    const borrowed_capability = admitted.capability();
    admitted.releaseImportedBindingsForShutdown();
    try std.testing.expectError(error.CorruptArtifact, borrowed_capability.validate());
}
