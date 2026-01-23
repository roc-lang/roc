//! Shared utilities for module discovery during compilation.
//!
//! These functions are used by both the IPC path (roc run) and the BuildEnv path (roc check/build)
//! to ensure consistent behavior when discovering and loading sibling modules.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Can = can.Can;
const AST = parse.AST;

/// Extract unqualified sibling module imports from a parsed AST.
/// Returns module names that:
/// 1. Have no qualifier (not like "pf.Stdout")
/// 2. Are uppercase identifiers (module names start with uppercase)
/// 3. Are not "Builtin" (always available)
///
/// This is used to identify which sibling modules need to be compiled
/// before canonicalizing the current module.
///
/// Parameters:
///   parse_ast: The parsed AST to extract imports from
///   gpa: Allocator for the returned strings
///
/// Returns: Slice of imported module names (caller owns memory)
pub fn extractImportsFromAST(
    parse_ast: *const AST,
    gpa: Allocator,
) ![][]const u8 {
    var result = std.ArrayList([]const u8).empty;
    errdefer {
        for (result.items) |item| gpa.free(item);
        result.deinit(gpa);
    }

    // Get the file and its statements
    const file = parse_ast.store.getFile();
    const stmt_slice = parse_ast.store.statementSlice(file.statements);

    for (stmt_slice) |stmt_idx| {
        const stmt = parse_ast.store.getStatement(stmt_idx);
        switch (stmt) {
            .import => |import_stmt| {
                // Skip qualified imports (e.g., "pf.Stdout")
                // These have a qualifier_tok set
                if (import_stmt.qualifier_tok != null) continue;

                // Get the module name from the token
                const module_name_raw = parse_ast.resolve(import_stmt.module_name_tok);

                // Strip leading dot if present (from tokens like .NoSpaceDotUpperIdent)
                const module_name = if (module_name_raw.len > 0 and module_name_raw[0] == '.')
                    module_name_raw[1..]
                else
                    module_name_raw;

                // Skip "Builtin" - always available
                if (std.mem.eql(u8, module_name, "Builtin")) continue;

                // Check if it looks like a module name (starts with uppercase)
                if (module_name.len == 0) continue;
                if (module_name[0] < 'A' or module_name[0] > 'Z') continue;

                // Check for duplicates using linear scan (typically few imports)
                var found = false;
                for (result.items) |existing| {
                    if (std.mem.eql(u8, existing, module_name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try result.append(gpa, try gpa.dupe(u8, module_name));
                }
            },
            else => {},
        }
    }

    return result.toOwnedSlice(gpa);
}

/// Add imported sibling modules to the module_envs_map.
///
/// 1. Extracts imports from the parsed AST
/// 2. Checks if each imported module has a corresponding .roc file
/// 3. Only adds those that exist to the module_envs_map
///
/// The sibling modules are added with a placeholder env (just to pass the "contains" check).
/// The actual env will be loaded later when the module is compiled.
///
/// Parameters:
/// - parse_ast: The parsed AST to extract imports from
/// - dir_path: The directory where sibling .roc files are located
/// - current_module_name: The name of the current module (will be skipped)
/// - env: The ModuleEnv to insert identifiers into
/// - module_envs_map: The map to add discovered modules to
/// - placeholder_env: The placeholder env to use for discovered modules
/// - gpa: Allocator for temporary allocations
pub fn addImportedModulesToEnvMap(
    parse_ast: *const AST,
    dir_path: []const u8,
    current_module_name: []const u8,
    env: *ModuleEnv,
    module_envs_map: *std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    placeholder_env: *const ModuleEnv,
    gpa: Allocator,
) !void {
    // Extract imports from the parsed AST
    const imports = try extractImportsFromAST(parse_ast, gpa);
    defer {
        for (imports) |imp| gpa.free(imp);
        gpa.free(imports);
    }

    for (imports) |module_name| {
        // Skip the current module
        if (std.mem.eql(u8, module_name, current_module_name)) continue;

        // Check if the corresponding .roc file exists
        const file_name = try std.fmt.allocPrint(gpa, "{s}.roc", .{module_name});
        defer gpa.free(file_name);

        const file_path = try std.fs.path.join(gpa, &.{ dir_path, file_name });
        defer gpa.free(file_path);

        // Only add if the file exists
        std.fs.cwd().access(file_path, .{}) catch continue;

        // Add to module_envs with a placeholder env (just to pass the "contains" check)
        const module_ident = try env.insertIdent(base.Ident.for_text(module_name));
        // For user modules, the qualified name is just the module name itself
        const qualified_ident = try env.insertIdent(base.Ident.for_text(module_name));
        // Only add if not already present (platform modules may already be there)
        if (!module_envs_map.contains(module_ident)) {
            try module_envs_map.put(module_ident, .{
                .env = placeholder_env,
                .qualified_type_ident = qualified_ident,
                // Mark as placeholder so canonicalizer skips member validation
                .is_placeholder = true,
            });
        }
    }
}
