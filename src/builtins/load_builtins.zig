//! Placeholder for loading builtin .roc modules
//!
//! TODO: In the future, this will:
//! 1. At build time: Compile Set.roc, Dict.roc, etc. using a bootstrap compiler
//! 2. At build time: Serialize their ModuleEnvs to bytes
//! 3. At build time: Embed those bytes using @embedFile
//! 4. At runtime: Deserialize the embedded bytes to get full ModuleEnvs
//!
//! For now, this is a placeholder to set up the infrastructure.

const std = @import("std");

/// List of builtin .roc files that should be loaded
/// Note: List.roc and Result.roc currently have compilation issues and are excluded
pub const builtin_roc_files = [_][]const u8{
    "Bool.roc",
    "Dict.roc",
    "Set.roc",
};
