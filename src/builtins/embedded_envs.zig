//! Placeholder for embedded builtin ModuleEnvs
//!
//! NOTE: The actual embedded_envs.zig file is auto-generated at build time
//! by compile_builtins.zig and lives in the build cache alongside the .env files.
//! This file exists only for documentation purposes.
//!
//! When adding a new builtin:
//! 1. Add the .roc file to src/builtins/roc/
//! 2. Add it to the list in load_builtins.zig
//! 3. Run the build - embedded_envs.zig will be auto-generated
//! 4. The build will validate the list matches what's on disk
//!
//! The generated file will contain:
//! - @embedFile declarations for each .env file (e.g., Bool_env_bytes)
//! - A loadAll() function that deserializes all embedded ModuleEnvs

const std = @import("std");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;

/// Placeholder - this function will be generated at build time
pub fn loadAll(_: std.mem.Allocator) ![]const *const ModuleEnv {
    @compileError("This is a placeholder file. The real embedded_envs.zig is generated at build time in the cache.");
}
