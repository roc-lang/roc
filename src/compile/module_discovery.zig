//! Shared utilities for module discovery during compilation.
//!
//! These functions are used by both the IPC path (roc run) and the BuildEnv path (roc check/build)
//! to ensure consistent behavior when discovering and loading sibling modules.

const std = @import("std");
const base = @import("base");
const can = @import("can");

const ModuleEnv = can.ModuleEnv;
const Can = can.Can;

/// Discover sibling .roc files in a directory and add them to the module_envs_map.
/// This prevents MODULE NOT FOUND errors for modules that exist but haven't been loaded yet.
///
/// The sibling modules are added with a placeholder env (just to pass the "contains" check).
/// The actual env will be loaded later when the module is compiled.
///
/// Parameters:
/// - dir_path: The directory to scan for .roc files
/// - current_module_name: The name of the current module (will be skipped)
/// - env: The ModuleEnv to insert identifiers into
/// - module_envs_map: The map to add discovered modules to
/// - placeholder_env: The placeholder env to use for discovered modules
pub fn discoverSiblingModules(
    dir_path: []const u8,
    current_module_name: []const u8,
    env: *ModuleEnv,
    module_envs_map: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    placeholder_env: *const ModuleEnv,
) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch {
        // If we can't open the directory, just proceed without sibling discovery
        return;
    };
    defer dir.close();

    var dir_iter = dir.iterate();
    while (dir_iter.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".roc")) continue;

        // Skip "main.roc" - it's typically the platform entry point
        if (std.mem.eql(u8, entry.name, "main.roc")) continue;

        // Extract module name without .roc extension
        const module_name = entry.name[0 .. entry.name.len - 4];

        // Skip the current module to avoid TYPE REDECLARED errors when a type module
        // defines a type with the same name as the module (e.g., Simple.roc with type Simple)
        if (std.mem.eql(u8, module_name, current_module_name)) continue;

        // Add to module_envs with a placeholder env (just to pass the "contains" check)
        const module_ident = try env.insertIdent(base.Ident.for_text(module_name));
        // For user modules, the qualified name is just the module name itself
        const qualified_ident = try env.insertIdent(base.Ident.for_text(module_name));
        // Only add if not already present (platform modules may already be there)
        if (!module_envs_map.contains(module_ident)) {
            try module_envs_map.put(module_ident, .{
                .env = placeholder_env,
                .qualified_type_ident = qualified_ident,
            });
        }
    }
}

test "discoverSiblingModules finds .roc files" {
    // This test would require creating a temp directory with test files
    // For now, just ensure the function compiles
}
