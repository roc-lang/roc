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
const Allocator = std.mem.Allocator;
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
    if (std.meta.activeTag(pattern) == .assign) return pattern.assign.ident;
    if (std.meta.activeTag(pattern) == .as) return pattern.as.ident;
    return null;
}

/// Extract the identifier from a pattern, recursively following .as patterns
/// to find the innermost identifier.
pub fn extractIdentFromPatternRecursive(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) ?Ident.Idx {
    const pattern = store.getPattern(pattern_idx);
    if (std.meta.activeTag(pattern) == .assign) return pattern.assign.ident;
    if (std.meta.activeTag(pattern) == .as) return pattern.as.ident;
    return null;
}

/// Return the binding pattern for declaration-like statements.
pub fn getDeclarationPattern(statement: CIR.Statement) ?CIR.Pattern.Idx {
    return switch (statement) {
        .s_decl => |decl| decl.pattern,
        .s_var => |var_stmt| var_stmt.pattern_idx,
        .s_var_uninitialized => |var_stmt| var_stmt.pattern_idx,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => null,
    };
}

// Definition Search Functions

/// Find a definition by name, searching through all_defs and all_statements.
/// Returns information about the first matching definition found.
pub fn findDefinitionByName(module_env: *ModuleEnv, name: []const u8) ?DefinitionInfo {
    return findDefinitionByModuleMember(module_env, module_env.module_name, name);
}

/// Find a definition by an explicit module/type name and member name.
/// This is useful for qualified definitions such as `List.append` in Builtin.roc,
/// whose containing module is `Builtin` rather than `List`.
pub fn findDefinitionByModuleMember(module_env: *ModuleEnv, module_name: []const u8, name: []const u8) ?DefinitionInfo {
    const matches_name = struct {
        fn check(ident_name: []const u8, qualifier: []const u8, member: []const u8) bool {
            if (std.mem.eql(u8, ident_name, member)) return true;
            if (qualifier.len == 0) return false;

            const qualified_len = qualifier.len + 1 + member.len;
            return ident_name.len == qualified_len and
                std.mem.startsWith(u8, ident_name, qualifier) and
                ident_name[qualifier.len] == '.' and
                std.mem.eql(u8, ident_name[qualifier.len + 1 ..], member);
        }
    }.check;

    // Search through all_defs first
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (extractIdentFromPattern(&module_env.store, def.pattern)) |ident_idx| {
            const ident_name = module_env.getIdentText(ident_idx);
            if (matches_name(ident_name, module_name, name)) {
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
                if (matches_name(ident_name, module_name, name)) {
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

/// Find a type declaration whose qualified name is `module_name.name`.
pub fn findTypeDeclarationByModuleMember(module_env: *ModuleEnv, module_name: []const u8, name: []const u8) ?CIR.Statement.Idx {
    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        const header_idx: ?CIR.TypeHeader.Idx = switch (module_env.store.getStatement(stmt_idx)) {
            .s_alias_decl => |alias| alias.header,
            .s_nominal_decl => |nominal| nominal.header,
            .s_decl,
            .s_var,
            .s_var_uninitialized,
            .s_reassign,
            .s_crash,
            .s_dbg,
            .s_expr,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_break,
            .s_return,
            .s_import,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => null,
        };
        if (header_idx) |header_idx_value| {
            const header = module_env.store.getTypeHeader(header_idx_value);
            const header_name = module_env.getIdentText(header.name);
            const qualified_len = module_name.len + 1 + name.len;
            const exact_match = module_name.len > 0 and header_name.len == qualified_len and
                std.mem.startsWith(u8, header_name, module_name) and
                header_name[module_name.len] == '.' and
                std.mem.eql(u8, header_name[module_name.len + 1 ..], name);
            const nested_match = module_name.len > 0 and header_name.len > qualified_len and
                header_name[header_name.len - qualified_len - 1] == '.' and
                std.mem.eql(u8, header_name[header_name.len - qualified_len ..][0..module_name.len], module_name) and
                header_name[header_name.len - name.len - 1] == '.' and
                std.mem.eql(u8, header_name[header_name.len - name.len ..], name);
            if (exact_match or nested_match) {
                return stmt_idx;
            }
        }
    }
    return null;
}

/// Find a definition by name, accepting a qualified definition's final segment.
pub fn findDefinitionByUnqualifiedName(module_env: *ModuleEnv, name: []const u8) ?DefinitionInfo {
    if (findDefinitionByName(module_env, name)) |info| return info;

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
        const pattern_idx_opt = getDeclarationPattern(stmt);
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
) Allocator.Error!std.ArrayList(DefinitionInfo) {
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

/// Find a module by name in the build environment's Coordinator state within an importing package context.
/// Returns null if the module is not found or the build environment is null.
pub fn findModuleByNameInPackage(
    build_env: *BuildEnv,
    importing_pkg: ?*compile.coordinator.PackageState,
    module_name: []const u8,
) ?ModuleInfo {
    if (build_env.findModuleByQualifiedNameInPackage(importing_pkg, module_name)) |mod_state| {
        if (mod_state.moduleEnv()) |module_env_ptr| {
            return ModuleInfo{
                .module_env = module_env_ptr,
                .path = mod_state.path,
            };
        }
    }
    return null;
}

/// Find a module by name in the build environment's Coordinator state.
/// Returns null if the module is not found or the build environment is null.
pub fn findModuleByName(build_env: *BuildEnv, module_name: []const u8) ?ModuleInfo {
    return findModuleByNameInPackage(build_env, null, module_name);
}

/// Find a module by name, optionally checking if it's a builtin type first.
/// This is a convenience wrapper that combines builtin checking with module lookup.
pub fn findModuleByNameWithBuiltinCheck(
    build_env: *BuildEnv,
    module_name: []const u8,
    builtin_types: []const []const u8,
) ?ModuleInfo {
    // Only check builtin types if module_name is unqualified
    if (std.mem.find(u8, module_name, ".") == null) {
        for (builtin_types) |builtin| {
            if (std.mem.eql(u8, module_name, builtin)) {
                // Builtin types don't have a separate module env in the normal sense
                return null;
            }
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
        .s_var_uninitialized => |d| .{
            .pattern = d.pattern_idx,
            .expr = null,
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
        .s_infinite_loop => |w| .{
            .pattern = null,
            .expr = w.cond,
            .expr2 = w.body,
        },
        .s_breakable_loop => |w| .{
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
        .s_where_alias_decl => .{
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
