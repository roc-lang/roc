//! Admission-authorized checked-artifact publication.
//!
//! This boundary pairs the root and sealed direct-import prefix with their
//! exact owner-minted admission capabilities. Available-artifact and relation
//! registries remain distinct publication inputs. The ordinary entry builds one
//! borrowed typed-CIR graph only after the allocation-free authority check; the
//! prebuilt entry validates and consumes an existing graph unchanged.

const std = @import("std");
const Allocator = std.mem.Allocator;

const ModuleEnv = @import("can").ModuleEnv;
const Check = @import("Check.zig");
const CheckedArtifact = @import("checked_artifact.zig");
const TypedCIR = @import("typed_cir.zig");

pub const Error = CheckedArtifact.CompileTimeFinalizer.Error || Check.W6bSemanticValidationError;

/// Publish from an admitted root and its exact direct-import prefix. Authority
/// is checked before `initForRootModule` may prepare the root for runtime use.
pub fn publishFromCheckedModule(
    allocator: Allocator,
    root_env: *ModuleEnv,
    root_capability: Check.ValidatedModuleEnv,
    imported_modules: Check.ValidatedModuleSet,
    inputs: CheckedArtifact.PublishInputs,
) Error!CheckedArtifact.CheckedModuleArtifact {
    try validateAuthority(root_env, root_capability, imported_modules, inputs);

    var root_modules = try TypedCIR.Modules.initForRootModule(
        allocator,
        root_env,
        imported_modules.envs,
    );
    defer root_modules.modules.deinit();

    return publishFromPrebuiltModules(
        allocator,
        &root_modules,
        root_capability,
        imported_modules,
        inputs,
    );
}

/// Publish through an existing borrowed root graph. This entry performs no
/// runtime preparation and does not rebuild or take ownership of the graph.
pub fn publishFromPrebuiltModules(
    allocator: Allocator,
    root_modules: *const TypedCIR.Modules.RootModules,
    root_capability: Check.ValidatedModuleEnv,
    imported_modules: Check.ValidatedModuleSet,
    inputs: CheckedArtifact.PublishInputs,
) Error!CheckedArtifact.CheckedModuleArtifact {
    const root_env = inputs.module_env_storage.envConst();
    try validateAuthority(root_env, root_capability, imported_modules, inputs);
    if (!root_modules.validateBorrowedGraph(root_env, imported_modules.envs)) {
        return error.CorruptArtifact;
    }

    return CheckedArtifact.publishFromTypedModule(
        allocator,
        &root_modules.modules,
        root_modules.module_idx,
        inputs,
    );
}

/// Allocation-free validation shared by both publication entries.
fn validateAuthority(
    root_env: *const ModuleEnv,
    root_capability: Check.ValidatedModuleEnv,
    imported_modules: Check.ValidatedModuleSet,
    inputs: CheckedArtifact.PublishInputs,
) Check.W6bSemanticValidationError!void {
    if (inputs.module_env_storage.envConst() != root_env or
        try root_capability.validate() != root_env)
    {
        return error.CorruptArtifact;
    }
    try root_capability.validateImportedModules(imported_modules);

    if (inputs.imports.len != imported_modules.envs.len or
        imported_modules.envs.len != imported_modules.modules.len)
    {
        return error.CorruptArtifact;
    }

    // Publication consumes this slice in the checker's sealed direct-prefix
    // order. Require that canonical order here; accepting an equivalent
    // permutation would let order-sensitive scopes observe different input.
    for (inputs.imports, 0..) |imported_artifact, row_index| {
        const module_idx: usize = imported_artifact.module_idx;
        if (module_idx != row_index) return error.CorruptArtifact;

        if (!std.meta.eql(imported_artifact.key.bytes, imported_artifact.view.key.bytes)) {
            return error.CorruptArtifact;
        }
        const raw_env = imported_modules.envs[module_idx];
        if (imported_artifact.view.module_env != raw_env or
            try imported_modules.modules[module_idx].validate() != raw_env)
        {
            return error.CorruptArtifact;
        }
    }
}
