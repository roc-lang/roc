//! Shared utilities for module discovery during compilation.
//!
//! These functions are used by both the IPC path (roc run) and the BuildEnv path (roc check/build)
//! to ensure consistent behavior when discovering and loading sibling modules.

const std = @import("std");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const AST = parse.AST;

/// Extract unqualified sibling module imports from a parsed AST.
/// Returns module names that:
/// 1. Have no qualifier (not like "pf.Stdout")
/// 2. Are uppercase identifiers (module names start with uppercase)
/// 3. Are not "Builtin" (always available)
///
/// In addition to explicit `import` statements, the upper-cased entries in a
/// `package [Mod1, Mod2, ...] {}` header are treated as auto-imports.
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

    const file = parse_ast.store.getFile();

    // Modules listed in a `package [...]` header are auto-imported.
    const header = parse_ast.store.getHeader(file.header);
    if (header == .package) {
        const collection = parse_ast.store.getCollection(header.package.exposes);
        const exposed_items = parse_ast.store.exposedItemSlice(.{ .span = collection.span });
        for (exposed_items) |exposed_idx| {
            const exposed = parse_ast.store.getExposedItem(exposed_idx);
            const name_token = switch (exposed) {
                .upper_ident => |ui| ui.ident,
                .upper_ident_star => |ui| ui.ident,
                .lower_ident, .malformed => continue,
            };
            const module_name = parse_ast.resolve(name_token);
            try appendModuleName(gpa, &result, module_name);
        }
    }

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

                try appendModuleName(gpa, &result, module_name);
            },
            else => {},
        }
    }

    return result.toOwnedSlice(gpa);
}

fn appendModuleName(
    gpa: Allocator,
    result: *std.ArrayList([]const u8),
    module_name: []const u8,
) !void {
    // Skip "Builtin" - always available
    if (std.mem.eql(u8, module_name, "Builtin")) return;

    // Check if it looks like a module name (starts with uppercase)
    if (module_name.len == 0) return;
    if (module_name[0] < 'A' or module_name[0] > 'Z') return;

    // Check for duplicates using linear scan (typically few imports)
    for (result.items) |existing| {
        if (std.mem.eql(u8, existing, module_name)) return;
    }
    try result.append(gpa, try gpa.dupe(u8, module_name));
}

/// Extract qualified/external imports from the AST.
/// These are imports like "import pf.Stdout" where qualifier_tok is set.
///
/// Returns: Slice of qualified import names (e.g., "pf.Stdout") (caller owns memory)
pub fn extractQualifiedImportsFromAST(
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
                // Only process qualified imports (e.g., "pf.Stdout")
                // These have a qualifier_tok set
                if (import_stmt.qualifier_tok == null) continue;

                // Get the qualifier (e.g., "pf") and module name (e.g., "Stdout")
                const qualifier = parse_ast.resolve(import_stmt.qualifier_tok.?);
                const module_name_raw = parse_ast.resolve(import_stmt.module_name_tok);

                // Strip leading dot if present
                const module_name = if (module_name_raw.len > 0 and module_name_raw[0] == '.')
                    module_name_raw[1..]
                else
                    module_name_raw;

                // Build qualified name like "pf.Stdout"
                const qualified_name = try std.fmt.allocPrint(gpa, "{s}.{s}", .{ qualifier, module_name });
                errdefer gpa.free(qualified_name);

                // Check for duplicates
                var found = false;
                for (result.items) |existing| {
                    if (std.mem.eql(u8, existing, qualified_name)) {
                        found = true;
                        gpa.free(qualified_name);
                        break;
                    }
                }
                if (!found) {
                    try result.append(gpa, qualified_name);
                }
            },
            else => {},
        }
    }

    return result.toOwnedSlice(gpa);
}
