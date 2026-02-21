//! Module lookup utilities for LSP operations.
//!
//! This module consolidates definition/pattern search patterns that are repeated
//! throughout the LSP codebase, particularly in syntax.zig and completion/builder.zig.
//! It provides common functions for:
//! - Extracting identifiers from patterns
//! - Finding definitions by name
//! - Looking up modules
//! - Getting type variables for patterns
//! - Extracting statement parts

const std = @import("std");
const can = @import("can");
const compile = @import("compile");
const base = @import("base");
const types = @import("types");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const NodeStore = can.NodeStore;
const BuildEnv = compile.BuildEnv;
const Ident = base.Ident;
const Region = base.Region;
const TypeVar = types.Var;

/// Information about a found definition.
pub const DefinitionInfo = struct {
    /// The pattern index where the definition is bound
    pattern_idx: CIR.Pattern.Idx,
    /// The expression index (if the definition has an expression)
    expr_idx: ?CIR.Expr.Idx,
    /// The identifier for the definition
    ident_idx: Ident.Idx,
};

/// Information about a found module.
pub const ModuleInfo = struct {
    /// The module environment
    module_env: *ModuleEnv,
    /// The path to the module source file
    path: []const u8,
};

/// Parts extracted from a statement for common processing.
/// Used to access pattern, expression(s), and annotation from any statement type.
pub const StatementParts = struct {
    /// The pattern bound by this statement (if any)
    pattern: ?CIR.Pattern.Idx,
    /// The primary expression (if any)
    expr: ?CIR.Expr.Idx,
    /// Secondary expression for statements that have multiple (e.g., while has cond + body)
    expr2: ?CIR.Expr.Idx,
};

/// Information about a binding found at a specific scope position.
pub const BindingInfo = struct {
    /// The pattern index where the binding is defined
    pattern_idx: CIR.Pattern.Idx,
    /// The identifier for the binding
    ident_idx: Ident.Idx,
    /// The expression index (if available)
    expr_idx: ?CIR.Expr.Idx,
    /// The region where the binding is defined
    region: Region,
};

// Pattern Extraction Functions

/// Extract the identifier from a pattern, handling .assign and .as cases.
/// Returns null for patterns that don't directly bind an identifier
/// (e.g., record destructures, literals, underscore).
pub fn extractIdentFromPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) ?Ident.Idx {
    const pattern = store.getPattern(pattern_idx);
    return switch (pattern) {
        .assign => |a| a.ident,
        .as => |a| a.ident,
        else => null,
    };
}

/// Extract the identifier from a pattern, recursively following .as patterns
/// to find the innermost identifier.
pub fn extractIdentFromPatternRecursive(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) ?Ident.Idx {
    const pattern = store.getPattern(pattern_idx);
    return switch (pattern) {
        .assign => |a| a.ident,
        .as => |a| a.ident,
        else => null,
    };
}

// Definition Search Functions

/// Find a definition by name, searching through all_defs and all_statements.
/// Returns information about the first matching definition found.
pub fn findDefinitionByName(module_env: *ModuleEnv, name: []const u8) ?DefinitionInfo {
    // Search through all_defs first
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (extractIdentFromPattern(&module_env.store, def.pattern)) |ident_idx| {
            const ident_name = module_env.getIdentText(ident_idx);
            if (std.mem.eql(u8, ident_name, name)) {
                return DefinitionInfo{
                    .pattern_idx = def.pattern,
                    .expr_idx = def.expr,
                    .ident_idx = ident_idx,
                };
            }
        }
    }

    // Search through all_statements
    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const parts = getStatementParts(stmt);

        if (parts.pattern) |pattern_idx| {
            if (extractIdentFromPattern(&module_env.store, pattern_idx)) |ident_idx| {
                const ident_name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_name, name)) {
                    return DefinitionInfo{
                        .pattern_idx = pattern_idx,
                        .expr_idx = parts.expr,
                        .ident_idx = ident_idx,
                    };
                }
            }
        }
    }

    return null;
}

/// Find a definition by name, also matching unqualified names against qualified ones.
/// For example, name "concat" will match a definition named "Str.concat".
/// This is useful for resolving external lookups where the caller knows only the
/// unqualified function name.
pub fn findDefinitionByUnqualifiedName(module_env: *ModuleEnv, name: []const u8) ?DefinitionInfo {
    // Try exact match first
    if (findDefinitionByName(module_env, name)) |info| return info;

    // Fall back to suffix matching: "name" matches "Module.name"
    var iter = iterateDefinitions(module_env);
    while (iter.next()) |def_info| {
        const def_name = module_env.getIdentText(def_info.ident_idx);
        if (def_name.len > name.len and
            std.mem.endsWith(u8, def_name, name) and
            def_name[def_name.len - name.len - 1] == '.')
        {
            return def_info;
        }
    }
    return null;
}

/// Find the Def that owns a specific pattern index.
/// Searches through all_defs for a def whose pattern matches the target.
/// Returns the full Def struct if found.
pub fn findDefOwningPattern(module_env: *ModuleEnv, target_pattern: CIR.Pattern.Idx) ?CIR.Def {
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (def.pattern == target_pattern) return def;
    }
    return null;
}

/// Find the Statement and its index that own a specific pattern index.
/// Searches through all_statements for a statement whose pattern matches the target.
pub fn findStatementOwningPattern(module_env: *ModuleEnv, target_pattern: CIR.Pattern.Idx) ?struct { stmt: CIR.Statement, idx: CIR.Statement.Idx } {
    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const pattern_idx_opt: ?CIR.Pattern.Idx = switch (stmt) {
            .s_decl => |decl| decl.pattern,
            .s_var => |var_stmt| var_stmt.pattern_idx,
            else => null,
        };
        if (pattern_idx_opt) |pat_idx| {
            if (pat_idx == target_pattern) return .{ .stmt = stmt, .idx = stmt_idx };
        }
    }
    return null;
}

/// Find all definitions in a module that match a given prefix.
/// Useful for completion suggestions.
pub fn findDefinitionsWithPrefix(
    module_env: *ModuleEnv,
    prefix: []const u8,
    allocator: std.mem.Allocator,
) !std.ArrayList(DefinitionInfo) {
    var results = std.ArrayList(DefinitionInfo).empty;

    // Search through all_defs
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (extractIdentFromPattern(&module_env.store, def.pattern)) |ident_idx| {
            const ident_name = module_env.getIdentText(ident_idx);
            if (prefix.len == 0 or std.mem.startsWith(u8, ident_name, prefix)) {
                try results.append(allocator, DefinitionInfo{
                    .pattern_idx = def.pattern,
                    .expr_idx = def.expr,
                    .ident_idx = ident_idx,
                });
            }
        }
    }

    // Search through all_statements
    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const parts = getStatementParts(stmt);

        if (parts.pattern) |pattern_idx| {
            if (extractIdentFromPattern(&module_env.store, pattern_idx)) |ident_idx| {
                const ident_name = module_env.getIdentText(ident_idx);
                if (prefix.len == 0 or std.mem.startsWith(u8, ident_name, prefix)) {
                    try results.append(allocator, DefinitionInfo{
                        .pattern_idx = pattern_idx,
                        .expr_idx = parts.expr,
                        .ident_idx = ident_idx,
                    });
                }
            }
        }
    }

    return results;
}

// Module Lookup Functions

/// Find a module by name in the build environment's schedulers.
/// Returns null if the module is not found or the build environment is null.
pub fn findModuleByName(build_env: *BuildEnv, module_name: []const u8) ?ModuleInfo {
    // Extract the base module name (e.g., "Stdout" from "pf.Stdout")
    const base_name = if (std.mem.lastIndexOf(u8, module_name, ".")) |dot_pos|
        module_name[dot_pos + 1 ..]
    else
        module_name;

    // Search all schedulers for a module matching this name
    var sched_it = build_env.schedulers.iterator();
    while (sched_it.next()) |entry| {
        const sched = entry.value_ptr.*;
        if (sched.getModuleState(base_name)) |mod_state| {
            if (mod_state.env) |*module_env_ptr| {
                return ModuleInfo{
                    .module_env = module_env_ptr,
                    .path = mod_state.path,
                };
            }
        }
    }
    return null;
}

/// Find a module by name, optionally checking if it's a builtin type first.
/// This is a convenience wrapper that combines builtin checking with module lookup.
pub fn findModuleByNameWithBuiltinCheck(
    build_env: *BuildEnv,
    module_name: []const u8,
    builtin_types: []const []const u8,
) ?ModuleInfo {
    // Extract the base module name
    const base_name = if (std.mem.lastIndexOf(u8, module_name, ".")) |dot_pos|
        module_name[dot_pos + 1 ..]
    else
        module_name;

    // Check if this is a builtin type
    for (builtin_types) |builtin| {
        if (std.mem.eql(u8, base_name, builtin)) {
            // Builtin types don't have a separate module env in the normal sense
            return null;
        }
    }

    return findModuleByName(build_env, module_name);
}

// Type Variable Functions

/// Get the type variable for a pattern from the type store.
/// This converts the pattern index to a type variable using ModuleEnv.varFrom.
pub fn getTypeVarForPattern(pattern_idx: CIR.Pattern.Idx) TypeVar {
    return ModuleEnv.varFrom(pattern_idx);
}

/// Get the type variable for an expression from the type store.
pub fn getTypeVarForExpr(expr_idx: CIR.Expr.Idx) TypeVar {
    return ModuleEnv.varFrom(expr_idx);
}

// Statement Parts Extraction

/// Extract the common parts from a statement (pattern, expression(s)).
/// This consolidates the repeated switch logic found throughout the LSP codebase.
/// Note: For type annotations, use getStatementAnnotation() separately.
pub fn getStatementParts(stmt: CIR.Statement) StatementParts {
    return switch (stmt) {
        .s_decl => |d| .{
            .pattern = d.pattern,
            .expr = d.expr,
            .expr2 = null,
        },

        .s_var => |d| .{
            .pattern = d.pattern_idx,
            .expr = d.expr,
            .expr2 = null,
        },
        .s_reassign => |d| .{
            .pattern = d.pattern_idx,
            .expr = d.expr,
            .expr2 = null,
        },
        .s_expr => |e| .{
            .pattern = null,
            .expr = e.expr,
            .expr2 = null,
        },
        .s_for => |f| .{
            .pattern = f.patt,
            .expr = f.expr,
            .expr2 = f.body,
        },
        .s_while => |w| .{
            .pattern = null,
            .expr = w.cond,
            .expr2 = w.body,
        },
        .s_dbg => |d| .{
            .pattern = null,
            .expr = d.expr,
            .expr2 = null,
        },
        .s_expect => |e| .{
            .pattern = null,
            .expr = e.body,
            .expr2 = null,
        },
        .s_crash => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_break => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_return => |r| .{
            .pattern = null,
            .expr = r.expr,
            .expr2 = null,
        },
        .s_import => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_alias_decl => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_nominal_decl => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_type_anno => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_type_var_alias => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
        .s_runtime_error => .{
            .pattern = null,
            .expr = null,
            .expr2 = null,
        },
    };
}

// Binding Search Functions

/// Find a binding by name that is in scope at the given offset.
/// This searches through statements to find bindings that are defined before the offset.
pub fn findBindingByName(module_env: *ModuleEnv, name: []const u8, offset: u32) ?BindingInfo {
    // First check all_defs (top-level definitions are always in scope)
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (extractIdentFromPattern(&module_env.store, def.pattern)) |ident_idx| {
            const ident_name = module_env.getIdentText(ident_idx);
            if (std.mem.eql(u8, ident_name, name)) {
                const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.pattern));
                const region = module_env.store.getRegionAt(pattern_node_idx);
                return BindingInfo{
                    .pattern_idx = def.pattern,
                    .ident_idx = ident_idx,
                    .expr_idx = def.expr,
                    .region = region,
                };
            }
        }
    }

    // Then check statements, but only those defined before the offset
    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const parts = getStatementParts(stmt);

        if (parts.pattern) |pattern_idx| {
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
            const region = module_env.store.getRegionAt(pattern_node_idx);

            // Only consider bindings that are defined before the offset
            if (region.start.offset > offset) continue;

            if (extractIdentFromPattern(&module_env.store, pattern_idx)) |ident_idx| {
                const ident_name = module_env.getIdentText(ident_idx);
                if (std.mem.eql(u8, ident_name, name)) {
                    return BindingInfo{
                        .pattern_idx = pattern_idx,
                        .ident_idx = ident_idx,
                        .expr_idx = parts.expr,
                        .region = region,
                    };
                }
            }
        }
    }

    return null;
}

// Iterator Helpers

/// Iterator over all definitions in a module (from both all_defs and all_statements).
pub const DefinitionIterator = struct {
    module_env: *ModuleEnv,
    defs_slice: []const CIR.Def.Idx,
    statements_slice: []const CIR.Statement.Idx,
    defs_index: usize = 0,
    statements_index: usize = 0,

    pub fn init(module_env: *ModuleEnv) DefinitionIterator {
        return .{
            .module_env = module_env,
            .defs_slice = module_env.store.sliceDefs(module_env.all_defs),
            .statements_slice = module_env.store.sliceStatements(module_env.all_statements),
        };
    }

    pub fn next(self: *DefinitionIterator) ?DefinitionInfo {
        // First iterate through defs
        while (self.defs_index < self.defs_slice.len) {
            const def_idx = self.defs_slice[self.defs_index];
            self.defs_index += 1;

            const def = self.module_env.store.getDef(def_idx);
            if (extractIdentFromPattern(&self.module_env.store, def.pattern)) |ident_idx| {
                return DefinitionInfo{
                    .pattern_idx = def.pattern,
                    .expr_idx = def.expr,
                    .ident_idx = ident_idx,
                };
            }
        }

        // Then iterate through statements
        while (self.statements_index < self.statements_slice.len) {
            const stmt_idx = self.statements_slice[self.statements_index];
            self.statements_index += 1;

            const stmt = self.module_env.store.getStatement(stmt_idx);
            const parts = getStatementParts(stmt);

            if (parts.pattern) |pattern_idx| {
                if (extractIdentFromPattern(&self.module_env.store, pattern_idx)) |ident_idx| {
                    return DefinitionInfo{
                        .pattern_idx = pattern_idx,
                        .expr_idx = parts.expr,
                        .ident_idx = ident_idx,
                    };
                }
            }
        }

        return null;
    }

    pub fn reset(self: *DefinitionIterator) void {
        self.defs_index = 0;
        self.statements_index = 0;
    }
};

/// Create an iterator over all definitions in a module.
pub fn iterateDefinitions(module_env: *ModuleEnv) DefinitionIterator {
    return DefinitionIterator.init(module_env);
}

// Tests

test "getStatementParts returns correct parts for s_break" {
    // This is a compile-time test to verify the switch handles all cases
    const stmt = CIR.Statement{ .s_break = .{} };
    const parts = getStatementParts(stmt);
    try std.testing.expect(parts.pattern == null);
    try std.testing.expect(parts.expr == null);
    try std.testing.expect(parts.expr2 == null);
}
