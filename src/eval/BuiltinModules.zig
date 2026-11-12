//! Centralized loading and management of builtin modules (Bool, Try, Str, etc.)
//!
//! This struct consolidates all builtin module loading into a single place,
//! using the actual .roc source files (embedded at compile time) rather than
//! duplicated hardcoded strings.

const std = @import("std");
const builtin = @import("builtin");
const can = @import("can");
const check = @import("check");
const builtin_static = can.BuiltinStatic;

const CIR = can.CIR;
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");
const BuiltinModuleView = builtin_static.BuiltinModuleView;
const BuiltinIndices = CIR.BuiltinIndices;
const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;

/// Centralized container for all builtin modules
pub const BuiltinModules = struct {
    allocator: Allocator,
    /// NON-OWNING handle to the builtin module env. The env wrapper is owned by
    /// `checked_artifact` through `.static_builtin`; the bytes it views are the
    /// compiler executable's static data.
    builtin_module: BuiltinModuleView,
    builtin_indices: BuiltinIndices,
    /// Self-describing frozen artifact whose sub-stores alias static embedded
    /// bytes. `deinit` tears down only the env wrapper, never the static backing.
    checked_artifact: CheckedModuleArtifact,

    pub const InitError = Allocator.Error || error{ CorruptBuiltinArtifact, BuiltinArtifactVersionMismatch, CorruptArtifact, StaleEmbeddedBuiltins, CorruptEmbeddedBuiltins };

    /// Initialize builtin handles over the baked static ModuleEnv and artifact.
    /// Every thread that reads the builtin artifact's key slots waits on them
    /// through `io`.
    pub fn init(allocator: Allocator, io: std.Io) InitError!BuiltinModules {
        const indices = compiled_builtins.builtinIndices(CIR);
        if (builtin.mode == .debug) {
            try builtin_static.validateBuiltinManifest(
                compiled_builtins.builtin_type_registry_hash,
                compiled_builtins.builtin_indices_layout_hash,
            );
        }

        var builtin_module = try builtin_static.moduleView(allocator, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        if (builtin.mode == .debug) {
            try builtin_static.validateBuiltinIndices(builtin_module.env, indices);
        }

        // The baked blob is the serialized artifact followed by a layout-version
        // trailer. `viewStaticBuiltin` validates and strips it before relocating:
        // a mismatch means the embedded blob is stale relative to this compiler
        // (a build-system inconsistency), which we reject rather than relocate
        // into a differently-shaped struct. It also bounds-checks every
        // relocatable marker against the blob before any sub-store aliases it,
        // so a corrupt or truncated baked blob fails cleanly.
        var checked_artifact = try CheckedModuleArtifact.viewStaticBuiltin(
            compiled_builtins.builtin_artifact_bin,
            allocator,
            .{ .static_builtin = builtin_module.env },
        );
        try checked_artifact.attachSealedTypes(allocator, io);

        return BuiltinModules{
            .allocator = allocator,
            .builtin_module = builtin_module,
            .builtin_indices = indices,
            .checked_artifact = checked_artifact,
        };
    }

    /// Clean up the small view objects. Static builtin bytes remain owned by the
    /// compiler executable for the process lifetime.
    pub fn deinit(self: *BuiltinModules) void {
        self.checked_artifact.deinit(self.allocator);
    }
};
