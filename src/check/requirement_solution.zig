//! The check-time platform/app requirement solution: for each platform
//! requirement an app was checked against, the exported app def that satisfies
//! it and the solver vars holding the solved requirement type and its identity
//! bindings. `Check` produces these when requirement unification succeeds;
//! checked-artifact publication materializes the vars as checked type roots
//! (`PlatformRequirementSolutionTable`), so the app↔platform correspondence
//! travels as checked data instead of being re-derived at finalization.

const types_mod = @import("types");
const can = @import("can");

/// One solved platform requirement, produced while checking an app root.
pub const SolutionInput = struct {
    /// Index of the requirement in the platform's requires clause. Equals the
    /// platform-side `PlatformRequiredDeclarationId` by construction: both are
    /// assigned positionally over the same `requires_types` list.
    requires_idx: u32,
    /// The exported app def the requirement resolved to at check time.
    def: can.CIR.Def.Idx,
    /// The requirement type instantiated into the app's store and unified
    /// against the app def's expression; solved once checking completes.
    solved_var: types_mod.Var,
    /// Whether the platform declares this requirement at a function type
    /// (procedure value) rather than a non-function type (const value).
    is_function: bool,
    /// The requirement type's identity variables (flex/rigid), instantiated
    /// into the app's store, in canonical identity-slot order — the
    /// first-encounter order the canonical type key digest assigns
    /// (`canonical_type_keys.identityVarsFromVar`). The index in this slice IS
    /// the identity slot shared with the platform's own published requirement
    /// payload.
    identity_vars: []const types_mod.Var,
};
