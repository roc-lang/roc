//! CIR to Mono IR Lowering Pass
//!
//! This module lowers the Canonical Intermediate Representation (CIR) to Mono IR.
//! The key transformation is converting module-local indices to globally unique symbols.
//!
//! Entry points:
//! - `lowerExpression`: Lower a single expression (for REPL/constant folding)
//! - `lowerFromEntryPoints`: Lower all reachable expressions from entry points
//!
//! The lowering process:
//! 1. Walks CIR expressions recursively
//! 2. Converts CIR.Pattern.Idx → MonoSymbol (using pattern's module + ident)
//! 3. Resolves type variables → layout.Idx via the layout store
//! 4. Handles cross-module references via Import resolution
//! 5. Produces a flat MonoExprStore consumable by any backend

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout_mod = @import("layout");
const types = @import("types");

const ir = @import("MonoIR.zig");
const store_mod = @import("MonoExprStore.zig");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const LambdaSetInference = can.LambdaSetInference;
const Region = base.Region;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const MonoExpr = ir.MonoExpr;
const MonoPattern = ir.MonoPattern;
const MonoExprId = ir.MonoExprId;
const MonoPatternId = ir.MonoPatternId;
const MonoExprSpan = ir.MonoExprSpan;
const MonoPatternSpan = ir.MonoPatternSpan;
const MonoFieldNameSpan = ir.MonoFieldNameSpan;
const MonoSymbol = ir.MonoSymbol;
const MonoCapture = ir.MonoCapture;
const MonoCaptureSpan = ir.MonoCaptureSpan;
const MonoWhenBranch = ir.MonoWhenBranch;
const ClosureRepresentation = ir.ClosureRepresentation;
const Recursive = ir.Recursive;
const SelfRecursive = ir.SelfRecursive;
const JoinPointId = ir.JoinPointId;
const MonoIfBranch = ir.MonoIfBranch;
const MonoStmt = ir.MonoStmt;

// Control flow statement types (for tail recursion)
const CFStmtId = ir.CFStmtId;
const CFSwitchBranch = ir.CFSwitchBranch;
const LayoutIdxSpan = ir.LayoutIdxSpan;
const MonoProc = ir.MonoProc;

const TailRecursion = @import("TailRecursion.zig");

const MonoExprStore = store_mod;
const LayoutIdx = layout_mod.Idx;
const LayoutStore = layout_mod.Store;
const TypeScope = types.TypeScope;

/// Map of (global_module_idx << 32 | node_idx) → hosted_function_index.
/// Used for fast lookup of hosted function indices during lowering.
pub const HostedFunctionMap = std.AutoHashMap(u64, u32);

/// Helper to create a key for the hosted function map.
pub fn hostedFunctionKey(global_module_idx: u16, node_idx: u32) u64 {
    return @as(u64, global_module_idx) << 32 | node_idx;
}

const Self = @This();

/// Lowering context
allocator: Allocator,

/// The target store we're lowering into
store: *MonoExprStore,

/// All module environments (for resolving cross-module references)
all_module_envs: []const *ModuleEnv,

/// Index of the app module in all_module_envs (for resolving e_lookup_required)
app_module_idx: ?u16,

/// Lambda set inference results (for closure dispatch)
lambda_inference: ?*LambdaSetInference,

/// Global layout store shared across all modules
layout_store: ?*LayoutStore,

/// Type scope for layout computation (for polymorphic type variable resolution)
type_scope: TypeScope,

/// Track which (module_idx, pattern_idx) pairs have been lowered to avoid duplicates
/// Maps to the MonoSymbol that was created
lowered_patterns: std.AutoHashMap(u64, MonoSymbol),

/// Track which top-level symbols have been lowered
lowered_symbols: std.AutoHashMap(u48, MonoExprId),

/// Type environment: maps pattern_idx to layout (inferred from expressions)
type_env: std.AutoHashMap(u32, LayoutIdx),

/// Current module index during lowering
current_module_idx: u16 = 0,

/// The module whose type variables are in the type_scope mappings.
/// When we call an external function, we map its rigid type vars to the caller's
/// concrete types. This tracks the caller's module so fromTypeVar can resolve
/// the mapped vars using the correct module's types store.
type_scope_caller_module: ?u16 = null,

/// Current binding pattern (for detecting recursive closures)
/// When lowering a statement like `f = |x| ...`, this holds the pattern for `f`
/// so we can detect if the closure body references itself.
current_binding_pattern: ?CIR.Pattern.Idx = null,

/// Current binding symbol (MonoSymbol version of current_binding_pattern)
/// Used to create MonoProcs for recursive closures.
current_binding_symbol: ?MonoSymbol = null,

/// Counter for generating unique join point IDs
next_join_point_id: u32 = 0,

/// Expression-based layout hints for external rigid type vars.
/// When an external function's rigid type var maps to a caller's flex var,
/// but we know from the expression that it should be a list, we pre-compute
/// the layout and store it here. This handles cases like `[[10], [20], [30]]`
/// where the inner list's type var is flex but the expression is clearly a list.
expr_layout_hints: std.AutoHashMap(types.Var, LayoutIdx),

/// Deferred lambda/closure definitions.
/// When a block-local s_decl binds a lambda/closure, we defer lowering the body
/// until call time (when type_scope is populated). Maps pattern_idx → CIR expr_idx.
deferred_defs: std.AutoHashMap(u32, CIR.Expr.Idx),

/// Map of hosted function indices for fast lookup during lowering.
/// Key: (global_module_idx << 32 | node_idx), Value: hosted_function_index
hosted_functions: ?*const HostedFunctionMap = null,

/// Initialize a new Lowerer
pub fn init(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    lambda_inference: ?*LambdaSetInference,
    layout_store: ?*LayoutStore,
    app_module_idx: ?u16,
    hosted_functions: ?*const HostedFunctionMap,
) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .app_module_idx = app_module_idx,
        .lambda_inference = lambda_inference,
        .layout_store = layout_store,
        .type_scope = TypeScope.init(allocator),
        .lowered_patterns = std.AutoHashMap(u64, MonoSymbol).init(allocator),
        .lowered_symbols = std.AutoHashMap(u48, MonoExprId).init(allocator),
        .type_env = std.AutoHashMap(u32, LayoutIdx).init(allocator),
        .expr_layout_hints = std.AutoHashMap(types.Var, LayoutIdx).init(allocator),
        .deferred_defs = std.AutoHashMap(u32, CIR.Expr.Idx).init(allocator),
        .hosted_functions = hosted_functions,
    };
}

/// Cleanup
pub fn deinit(self: *Self) void {
    self.lowered_patterns.deinit();
    self.lowered_symbols.deinit();
    self.type_env.deinit();
    self.type_scope.deinit();
    self.expr_layout_hints.deinit();
    self.deferred_defs.deinit();
}

/// Get the module environment at the given index
fn getModuleEnv(self: *const Self, module_idx: u16) ?*ModuleEnv {
    if (module_idx >= self.all_module_envs.len) return null;
    return self.all_module_envs[module_idx];
}

/// Find the module index for a given origin_module ident (from a NominalType).
/// Uses the import system of source_env to resolve the origin module name to a module index.
fn findModuleForOrigin(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) ?u16 {
    // Check if origin is source_env itself
    if (origin_module == source_env.module_name_idx) {
        for (self.all_module_envs, 0..) |env, idx| {
            if (env == source_env) return @intCast(idx);
        }
    }

    // Use the import system: iterate source_env's imports
    const import_count: usize = @intCast(source_env.imports.imports.len());
    for (0..import_count) |i| {
        const import_idx: CIR.Import.Idx = @enumFromInt(i);
        if (source_env.imports.getIdentIdx(import_idx)) |import_ident| {
            if (import_ident == origin_module) {
                if (source_env.imports.getResolvedModule(import_idx)) |mod_idx| {
                    return @intCast(mod_idx);
                }
            }
        }
    }

    // Fallback: compare origin name against resolved module names
    const origin_name = source_env.getIdent(origin_module);
    for (0..import_count) |i| {
        const import_idx: CIR.Import.Idx = @enumFromInt(i);
        if (source_env.imports.getResolvedModule(import_idx)) |mod_idx| {
            if (mod_idx < self.all_module_envs.len) {
                const import_env = self.all_module_envs[mod_idx];
                if (std.mem.eql(u8, import_env.module_name, origin_name)) {
                    return @intCast(mod_idx);
                }
            }
        }
    }

    return null;
}

/// Create a MonoSymbol from a pattern in the current module
fn patternToSymbol(self: *Self, pattern_idx: CIR.Pattern.Idx) MonoSymbol {
    const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);

    if (self.lowered_patterns.get(key)) |existing| {
        return existing;
    }

    // Get the pattern's identifier from the current module
    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| a.ident,
        .as => |as| as.ident,
        else => Ident.Idx.NONE,
    };

    const symbol = MonoSymbol{
        .module_idx = self.current_module_idx,
        .ident_idx = ident_idx,
    };

    self.lowered_patterns.put(key, symbol) catch {};
    return symbol;
}

/// Create a MonoSymbol from an external reference
fn externalToSymbol(self: *Self, import_idx: CIR.Import.Idx, ident_idx: Ident.Idx) MonoSymbol {
    // Resolve the import to a module index
    const module_env = self.all_module_envs[self.current_module_idx];
    const resolved_module = module_env.imports.getResolvedModule(import_idx);

    if (resolved_module) |mod_idx| {
        return MonoSymbol{
            .module_idx = @intCast(mod_idx),
            .ident_idx = ident_idx,
        };
    }

    // Unresolved import - use sentinel
    return MonoSymbol.none;
}

/// Lower an external definition using its direct Def.Idx
/// This is the primary path - uses the target_node_idx from e_lookup_external
fn lowerExternalDefByIdx(self: *Self, symbol: MonoSymbol, target_def_idx: u16) Allocator.Error!void {
    // Skip if symbol is invalid
    if (symbol.module_idx >= self.all_module_envs.len) {
        return;
    }

    // Get the external module environment
    const ext_module_env = self.all_module_envs[symbol.module_idx];

    // Check if this is actually a def node (type modules with hosted functions may have different node types)
    if (!ext_module_env.store.isDefNode(target_def_idx)) {
        // Not a def node - this could be a hosted lambda or other special node type
        // Skip lowering; the node will be handled elsewhere (e.g., in e_call for hosted lambdas)
        return;
    }

    // Get the definition directly using the index
    const def_idx: CIR.Def.Idx = @enumFromInt(target_def_idx);
    const def = ext_module_env.store.getDef(def_idx);

    // Create symbol key for deduplication
    const symbol_key: u48 = @bitCast(symbol);

    // Avoid infinite recursion - check if already lowered
    if (self.lowered_symbols.contains(symbol_key)) return;

    // Lower the definition's expression in the context of the external module
    const old_module = self.current_module_idx;
    self.current_module_idx = symbol.module_idx;
    defer self.current_module_idx = old_module;

    // Set up binding context for recursive closure detection
    // This is necessary for external functions like List.repeat that have recursive helpers
    const old_binding = self.current_binding_pattern;
    const old_symbol = self.current_binding_symbol;
    self.current_binding_pattern = def.pattern;
    self.current_binding_symbol = symbol;
    defer {
        self.current_binding_pattern = old_binding;
        self.current_binding_symbol = old_symbol;
    }

    const expr_id = self.lowerExprFromIdx(ext_module_env, def.expr) catch {
        // If lowering fails, don't register - the lookup will fail at codegen time
        return;
    };

    // Register the symbol definition
    try self.store.registerSymbolDef(symbol, expr_id);
    try self.lowered_symbols.put(symbol_key, expr_id);
}

/// Find the definition index for a given pattern (if it's a top-level def).
/// Returns null if the pattern doesn't correspond to a top-level definition.
fn findDefForPattern(_: *Self, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (def.pattern == pattern_idx) {
            return def_idx;
        }
    }
    return null;
}

/// Lower a local definition using its pattern index.
/// This handles same-module function references (like walk calling walk_help).
fn lowerLocalDefByPattern(self: *Self, module_env: *ModuleEnv, symbol: MonoSymbol, pattern_idx: CIR.Pattern.Idx) Allocator.Error!void {
    // Create symbol key for deduplication
    const symbol_key: u48 = @bitCast(symbol);

    // Avoid infinite recursion - check if already lowered
    if (self.lowered_symbols.contains(symbol_key)) return;

    // Find the CIR expression: check module defs first, then deferred defs
    const pattern_key = @intFromEnum(pattern_idx);
    const cir_expr_idx: CIR.Expr.Idx = blk: {
        if (self.findDefForPattern(module_env, pattern_idx)) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (std.debug.runtime_safety) std.debug.assert(def.pattern == pattern_idx);
            break :blk def.expr;
        }
        if (self.deferred_defs.get(pattern_key)) |deferred| {
            break :blk deferred;
        }
        return; // Not a top-level or deferred definition
    };

    // Remove from deferred_defs before lowering to prevent infinite recursion
    // when the body contains a self-reference (e.g., recursive closures like factorial).
    _ = self.deferred_defs.remove(pattern_key);

    // Set up binding context for recursive closure detection
    const old_binding = self.current_binding_pattern;
    const old_symbol = self.current_binding_symbol;
    self.current_binding_pattern = pattern_idx;
    self.current_binding_symbol = symbol;
    defer {
        self.current_binding_pattern = old_binding;
        self.current_binding_symbol = old_symbol;
    }

    // Lower the definition's expression
    const expr_id = self.lowerExprFromIdx(module_env, cir_expr_idx) catch {
        // If lowering fails, don't register - the lookup will fail at codegen time
        // This matches the behavior of lowerExternalDefByIdx
        return;
    };

    // Register the symbol definition
    try self.store.registerSymbolDef(symbol, expr_id);
    try self.lowered_symbols.put(symbol_key, expr_id);
}

/// Get layout for a block expression by inferring from the final expression
fn getBlockLayout(self: *Self, module_env: *ModuleEnv, block: anytype) LayoutIdx {
    // Process declarations to populate type_env for lookups
    const stmts = module_env.store.sliceStatements(block.stmts);
    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                // Infer layout from the expression using type variable
                const pattern_key = @intFromEnum(decl.pattern);
                const inferred = self.getExprLayoutFromIdx(module_env, decl.expr);
                self.type_env.put(pattern_key, inferred) catch {};
            },
            .s_var => |var_stmt| {
                // Mutable variable - also populate type_env
                const pattern_key = @intFromEnum(var_stmt.pattern_idx);
                const inferred = self.getExprLayoutFromIdx(module_env, var_stmt.expr);
                self.type_env.put(pattern_key, inferred) catch {};
            },
            else => {},
        }
    }

    // Return the layout of the final expression using type variable
    return self.getExprLayoutFromIdx(module_env, block.final_expr);
}

/// Get layout for an expression from its index using the global layout store.
fn getExprLayoutFromIdx(self: *Self, _: *ModuleEnv, expr_idx: CIR.Expr.Idx) LayoutIdx {
    const type_var = ModuleEnv.varFrom(expr_idx);
    const ls = self.layout_store orelse unreachable;
    return ls.fromTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
}

/// Look up the discriminant for a tag name within a tag union type.
/// The layout store sorts tag names alphabetically to assign discriminants,
/// so we must replicate that sorting here.
fn resolveTagDiscriminant(self: *Self, module_env: *ModuleEnv, type_var: types.Var, tag_name: Ident.Idx) u16 {
    const types_store = &module_env.types;
    var resolved = types_store.resolveVar(type_var);

    // Follow type scope if unresolved (flex/rigid)
    if (resolved.desc.content == .flex or resolved.desc.content == .rigid) {
        if (self.type_scope.lookup(resolved.var_)) |mapped| {
            const scope_env = if (self.type_scope_caller_module) |caller_idx|
                self.all_module_envs[caller_idx]
            else
                module_env;
            resolved = scope_env.types.resolveVar(mapped);
        }
    }

    // Unwrap nominal types (e.g., Bool := [False, True]) to their backing type
    if (resolved.desc.content.unwrapNominalType()) |nominal| {
        const backing_var = types_store.getNominalBackingVar(nominal);
        resolved = types_store.resolveVar(backing_var);
    }

    const tag_union = resolved.desc.content.unwrapTagUnion() orelse return 0;

    // Count how many tag names across the entire tag union (following extension
    // variable chains) sort alphabetically before our target name. This matches
    // the layout store's alphabetical discriminant assignment.
    const ident_store = module_env.getIdentStoreConst();
    const target_text = ident_store.getText(tag_name);
    var discriminant: u16 = 0;

    // Process the initial tag union row
    const tags_slice = types_store.getTagsSlice(tag_union.tags);
    for (tags_slice.items(.name)) |other_name| {
        const other_text = ident_store.getText(other_name);
        if (std.mem.order(u8, other_text, target_text) == .lt) {
            discriminant += 1;
        }
    }

    // Follow the extension variable chain to collect tags from all rows
    var current_ext = tag_union.ext;
    var guard: u32 = 0;
    while (guard < 100) : (guard += 1) {
        const ext_resolved = types_store.resolveVar(current_ext);
        switch (ext_resolved.desc.content) {
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |ext_tu| {
                    const ext_tags = types_store.getTagsSlice(ext_tu.tags);
                    for (ext_tags.items(.name)) |other_name| {
                        const other_text = ident_store.getText(other_name);
                        if (std.mem.order(u8, other_text, target_text) == .lt) {
                            discriminant += 1;
                        }
                    }
                    current_ext = ext_tu.ext;
                },
                .empty_tag_union => break,
                .nominal_type => |nominal| {
                    const backing_var = types_store.getNominalBackingVar(nominal);
                    current_ext = backing_var;
                },
                else => break,
            },
            .alias => |alias| {
                current_ext = types_store.getAliasBackingVar(alias);
            },
            .flex, .rigid => break,
            else => break,
        }
    }

    return discriminant;
}

/// Push a type_scope mapping from the for-loop pattern's type variable to the element
/// type extracted from the list expression's type. Returns true if a scope was pushed.
/// The caller must call popForLoopElementTypeScope() after lowering the body.
fn pushForLoopElementTypeScope(self: *Self, module_env: *const ModuleEnv, list_expr_idx: CIR.Expr.Idx, patt_idx: CIR.Pattern.Idx) bool {
    const list_type_var = ModuleEnv.varFrom(list_expr_idx);
    var list_resolved = module_env.types.resolveVar(list_type_var);

    // If the list type is a flex/rigid var, check type_scope for a concrete mapping
    // (This handles polymorphic functions where the list parameter type is unresolved)
    var list_type_source_env: *const ModuleEnv = module_env;
    if (list_resolved.desc.content == .flex or list_resolved.desc.content == .rigid) {
        if (self.type_scope.lookup(list_resolved.var_)) |mapped| {
            if (self.type_scope_caller_module) |caller_idx| {
                list_type_source_env = self.all_module_envs[caller_idx];
            }
            list_resolved = list_type_source_env.types.resolveVar(mapped);
        }
    }

    if (list_resolved.desc.content != .structure) return false;
    const structure = list_resolved.desc.content.structure;
    if (structure != .nominal_type) return false;
    const nominal = structure.nominal_type;
    const args = list_type_source_env.types.sliceNominalArgs(nominal);
    if (args.len == 0) return false;

    const elem_type_var = args[0];

    // Get the pattern's type variable
    const patt_type_var = ModuleEnv.varFrom(patt_idx);
    const patt_resolved = module_env.types.resolveVar(patt_type_var);

    // Only add mapping if the pattern's type is unresolved (flex/rigid)
    if (patt_resolved.desc.content != .flex and patt_resolved.desc.content != .rigid) return false;

    // Push a new scope with the mapping
    var new_scope = types.VarMap.init(self.allocator);
    new_scope.put(patt_resolved.var_, elem_type_var) catch return false;
    self.type_scope.scopes.append(new_scope) catch return false;
    return true;
}

/// Pop a type scope that was pushed by pushForLoopElementTypeScope.
fn popForLoopElementTypeScope(self: *Self) void {
    if (self.type_scope.scopes.items.len > 0) {
        var scope = self.type_scope.scopes.items[self.type_scope.scopes.items.len - 1];
        self.type_scope.scopes.items.len -= 1;
        scope.deinit();
    }
}

/// Get the element layout from the for-loop's LIST expression, not the pattern.
/// The list expression has type List(T) where T is the concrete element type.
/// The pattern's type may be a flex variable with constraints that doesn't resolve correctly.
fn getForLoopElementLayout(self: *Self, list_expr_idx: CIR.Expr.Idx) LayoutIdx {
    const module_env = self.getModuleEnv(self.current_module_idx) orelse unreachable;
    const ls = self.layout_store orelse unreachable;

    // Get the list's type variable
    const list_type_var = ModuleEnv.varFrom(list_expr_idx);
    const resolved = module_env.types.resolveVar(list_type_var);

    // The list type must be a nominal structure (List)
    switch (resolved.desc.content) {
        .structure => |structure| {
            switch (structure) {
                .nominal_type => |nominal| {
                    // Get the element type (first type argument of List)
                    const args = module_env.types.sliceNominalArgs(nominal);
                    std.debug.assert(args.len > 0); // List must have element type arg
                    const elem_type_var = args[0];
                    // For local calls, setupLocalCallLayoutHints populates the type_scope
                    // with intra-module mappings (e.g., flex_var -> concrete_type), but
                    // type_scope_caller_module remains null. fromTypeVar only checks the
                    // type_scope when caller_module_idx is non-null. So when the element
                    // type is a flex/rigid var and the type_scope has a mapping for it,
                    // pass the current module as caller so the mapping is used.
                    const effective_caller = self.type_scope_caller_module orelse blk: {
                        const elem_resolved = module_env.types.resolveVar(elem_type_var);
                        if (elem_resolved.desc.content == .flex or elem_resolved.desc.content == .rigid) {
                            if (self.type_scope.lookup(elem_resolved.var_) != null) {
                                break :blk self.current_module_idx;
                            }
                        }
                        break :blk null;
                    };
                    // Compute layout for the element type from the list's type arg
                    const elem_layout = ls.fromTypeVar(self.current_module_idx, elem_type_var, &self.type_scope, effective_caller) catch unreachable;
                    return elem_layout;
                },
                else => unreachable, // For loop list must be List type
            }
        },
        else => unreachable, // For loop list must be structure type
    }
}

/// Get the layout for a pattern from its type variable using the global layout store.
fn getPatternLayout(self: *Self, pattern_idx: CIR.Pattern.Idx) LayoutIdx {
    const type_var = ModuleEnv.varFrom(pattern_idx);

    // Check if we have a pre-computed layout hint for this type variable.
    // This handles cases where the type var maps to a flex but we know from
    // expression analysis that it should be a list (e.g., [[10], [20], [30]] elements).
    const module_env = self.getModuleEnv(self.current_module_idx) orelse unreachable;
    const resolved = module_env.types.resolveVar(type_var);
    if (self.expr_layout_hints.get(resolved.var_)) |hint_layout| {
        return hint_layout;
    }

    const ls = self.layout_store orelse unreachable;
    return ls.fromTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
}

/// Convert a CIR low-level op to the corresponding Mono IR low-level op.
/// Returns null for ops that don't have a direct Mono IR equivalent (e.g. str ops).
fn convertToMonoLowLevel(op: CIR.Expr.LowLevel) ?ir.MonoExpr.LowLevel {
    return switch (op) {
        .list_len => .list_len,
        .list_is_empty => .list_is_empty,
        .list_get_unsafe => .list_get,
        .list_append => .list_append,
        .list_append_unsafe => .list_append,
        .list_concat => .list_concat,
        .list_with_capacity => .list_with_capacity,
        .list_drop_at => .list_drop_first,
        .list_sublist => .list_take_first,

        // String operations
        .str_is_empty => .str_is_empty,
        .str_is_eq => .str_is_eq,
        .str_concat => .str_concat,
        .str_contains => .str_contains,
        .str_trim => .str_trim,
        .str_trim_start => .str_trim_start,
        .str_trim_end => .str_trim_end,
        .str_caseless_ascii_equals => .str_caseless_ascii_equals,
        .str_with_ascii_lowercased => .str_with_ascii_lowercased,
        .str_with_ascii_uppercased => .str_with_ascii_uppercased,
        .str_starts_with => .str_starts_with,
        .str_ends_with => .str_ends_with,
        .str_repeat => .str_repeat,
        .str_with_prefix => .str_with_prefix,
        .str_drop_prefix => .str_drop_prefix,
        .str_drop_suffix => .str_drop_suffix,
        .str_count_utf8_bytes => .str_count_utf8_bytes,
        .str_with_capacity => .str_with_capacity,
        .str_reserve => .str_reserve,
        .str_release_excess_capacity => .str_release_excess_capacity,
        .str_to_utf8 => .str_to_utf8,
        .str_from_utf8_lossy => .str_from_utf8_lossy,
        .str_from_utf8 => .str_from_utf8,
        .str_split_on => .str_split,
        .str_join_with => .str_join_with,

        // Numeric conversion operations (U8)
        .u8_to_i8_wrap => .u8_to_i8_wrap,
        .u8_to_i8_try => .u8_to_i8_try,
        .u8_to_i16 => .u8_to_i16,
        .u8_to_i32 => .u8_to_i32,
        .u8_to_i64 => .u8_to_i64,
        .u8_to_i128 => .u8_to_i128,
        .u8_to_u16 => .u8_to_u16,
        .u8_to_u32 => .u8_to_u32,
        .u8_to_u64 => .u8_to_u64,
        .u8_to_u128 => .u8_to_u128,
        .u8_to_f32 => .u8_to_f32,
        .u8_to_f64 => .u8_to_f64,
        .u8_to_dec => .u8_to_dec,

        // Numeric conversion operations (I8)
        .i8_to_i16 => .i8_to_i16,
        .i8_to_i32 => .i8_to_i32,
        .i8_to_i64 => .i8_to_i64,
        .i8_to_i128 => .i8_to_i128,
        .i8_to_u8_wrap => .i8_to_u8_wrap,
        .i8_to_u8_try => .i8_to_u8_try,
        .i8_to_u16_wrap => .i8_to_u16_wrap,
        .i8_to_u16_try => .i8_to_u16_try,
        .i8_to_u32_wrap => .i8_to_u32_wrap,
        .i8_to_u32_try => .i8_to_u32_try,
        .i8_to_u64_wrap => .i8_to_u64_wrap,
        .i8_to_u64_try => .i8_to_u64_try,
        .i8_to_u128_wrap => .i8_to_u128_wrap,
        .i8_to_u128_try => .i8_to_u128_try,
        .i8_to_f32 => .i8_to_f32,
        .i8_to_f64 => .i8_to_f64,
        .i8_to_dec => .i8_to_dec,

        // Numeric conversion operations (U16)
        .u16_to_i8_wrap => .u16_to_i8_wrap,
        .u16_to_i8_try => .u16_to_i8_try,
        .u16_to_i16_wrap => .u16_to_i16_wrap,
        .u16_to_i16_try => .u16_to_i16_try,
        .u16_to_i32 => .u16_to_i32,
        .u16_to_i64 => .u16_to_i64,
        .u16_to_i128 => .u16_to_i128,
        .u16_to_u8_wrap => .u16_to_u8_wrap,
        .u16_to_u8_try => .u16_to_u8_try,
        .u16_to_u32 => .u16_to_u32,
        .u16_to_u64 => .u16_to_u64,
        .u16_to_u128 => .u16_to_u128,
        .u16_to_f32 => .u16_to_f32,
        .u16_to_f64 => .u16_to_f64,
        .u16_to_dec => .u16_to_dec,

        // Numeric conversion operations (I16)
        .i16_to_i8_wrap => .i16_to_i8_wrap,
        .i16_to_i8_try => .i16_to_i8_try,
        .i16_to_i32 => .i16_to_i32,
        .i16_to_i64 => .i16_to_i64,
        .i16_to_i128 => .i16_to_i128,
        .i16_to_u8_wrap => .i16_to_u8_wrap,
        .i16_to_u8_try => .i16_to_u8_try,
        .i16_to_u16_wrap => .i16_to_u16_wrap,
        .i16_to_u16_try => .i16_to_u16_try,
        .i16_to_u32_wrap => .i16_to_u32_wrap,
        .i16_to_u32_try => .i16_to_u32_try,
        .i16_to_u64_wrap => .i16_to_u64_wrap,
        .i16_to_u64_try => .i16_to_u64_try,
        .i16_to_u128_wrap => .i16_to_u128_wrap,
        .i16_to_u128_try => .i16_to_u128_try,
        .i16_to_f32 => .i16_to_f32,
        .i16_to_f64 => .i16_to_f64,
        .i16_to_dec => .i16_to_dec,

        // Numeric conversion operations (U32)
        .u32_to_i8_wrap => .u32_to_i8_wrap,
        .u32_to_i8_try => .u32_to_i8_try,
        .u32_to_i16_wrap => .u32_to_i16_wrap,
        .u32_to_i16_try => .u32_to_i16_try,
        .u32_to_i32_wrap => .u32_to_i32_wrap,
        .u32_to_i32_try => .u32_to_i32_try,
        .u32_to_i64 => .u32_to_i64,
        .u32_to_i128 => .u32_to_i128,
        .u32_to_u8_wrap => .u32_to_u8_wrap,
        .u32_to_u8_try => .u32_to_u8_try,
        .u32_to_u16_wrap => .u32_to_u16_wrap,
        .u32_to_u16_try => .u32_to_u16_try,
        .u32_to_u64 => .u32_to_u64,
        .u32_to_u128 => .u32_to_u128,
        .u32_to_f32 => .u32_to_f32,
        .u32_to_f64 => .u32_to_f64,
        .u32_to_dec => .u32_to_dec,

        // Numeric conversion operations (I32)
        .i32_to_i8_wrap => .i32_to_i8_wrap,
        .i32_to_i8_try => .i32_to_i8_try,
        .i32_to_i16_wrap => .i32_to_i16_wrap,
        .i32_to_i16_try => .i32_to_i16_try,
        .i32_to_i64 => .i32_to_i64,
        .i32_to_i128 => .i32_to_i128,
        .i32_to_u8_wrap => .i32_to_u8_wrap,
        .i32_to_u8_try => .i32_to_u8_try,
        .i32_to_u16_wrap => .i32_to_u16_wrap,
        .i32_to_u16_try => .i32_to_u16_try,
        .i32_to_u32_wrap => .i32_to_u32_wrap,
        .i32_to_u32_try => .i32_to_u32_try,
        .i32_to_u64_wrap => .i32_to_u64_wrap,
        .i32_to_u64_try => .i32_to_u64_try,
        .i32_to_u128_wrap => .i32_to_u128_wrap,
        .i32_to_u128_try => .i32_to_u128_try,
        .i32_to_f32 => .i32_to_f32,
        .i32_to_f64 => .i32_to_f64,
        .i32_to_dec => .i32_to_dec,

        // Numeric conversion operations (U64)
        .u64_to_i8_wrap => .u64_to_i8_wrap,
        .u64_to_i8_try => .u64_to_i8_try,
        .u64_to_i16_wrap => .u64_to_i16_wrap,
        .u64_to_i16_try => .u64_to_i16_try,
        .u64_to_i32_wrap => .u64_to_i32_wrap,
        .u64_to_i32_try => .u64_to_i32_try,
        .u64_to_i64_wrap => .u64_to_i64_wrap,
        .u64_to_i64_try => .u64_to_i64_try,
        .u64_to_i128 => .u64_to_i128,
        .u64_to_u8_wrap => .u64_to_u8_wrap,
        .u64_to_u8_try => .u64_to_u8_try,
        .u64_to_u16_wrap => .u64_to_u16_wrap,
        .u64_to_u16_try => .u64_to_u16_try,
        .u64_to_u32_wrap => .u64_to_u32_wrap,
        .u64_to_u32_try => .u64_to_u32_try,
        .u64_to_u128 => .u64_to_u128,
        .u64_to_f32 => .u64_to_f32,
        .u64_to_f64 => .u64_to_f64,
        .u64_to_dec => .u64_to_dec,

        // Numeric conversion operations (I64)
        .i64_to_i8_wrap => .i64_to_i8_wrap,
        .i64_to_i8_try => .i64_to_i8_try,
        .i64_to_i16_wrap => .i64_to_i16_wrap,
        .i64_to_i16_try => .i64_to_i16_try,
        .i64_to_i32_wrap => .i64_to_i32_wrap,
        .i64_to_i32_try => .i64_to_i32_try,
        .i64_to_i128 => .i64_to_i128,
        .i64_to_u8_wrap => .i64_to_u8_wrap,
        .i64_to_u8_try => .i64_to_u8_try,
        .i64_to_u16_wrap => .i64_to_u16_wrap,
        .i64_to_u16_try => .i64_to_u16_try,
        .i64_to_u32_wrap => .i64_to_u32_wrap,
        .i64_to_u32_try => .i64_to_u32_try,
        .i64_to_u64_wrap => .i64_to_u64_wrap,
        .i64_to_u64_try => .i64_to_u64_try,
        .i64_to_u128_wrap => .i64_to_u128_wrap,
        .i64_to_u128_try => .i64_to_u128_try,
        .i64_to_f32 => .i64_to_f32,
        .i64_to_f64 => .i64_to_f64,
        .i64_to_dec => .i64_to_dec,

        // Numeric conversion operations (U128)
        .u128_to_i8_wrap => .u128_to_i8_wrap,
        .u128_to_i8_try => .u128_to_i8_try,
        .u128_to_i16_wrap => .u128_to_i16_wrap,
        .u128_to_i16_try => .u128_to_i16_try,
        .u128_to_i32_wrap => .u128_to_i32_wrap,
        .u128_to_i32_try => .u128_to_i32_try,
        .u128_to_i64_wrap => .u128_to_i64_wrap,
        .u128_to_i64_try => .u128_to_i64_try,
        .u128_to_i128_wrap => .u128_to_i128_wrap,
        .u128_to_i128_try => .u128_to_i128_try,
        .u128_to_u8_wrap => .u128_to_u8_wrap,
        .u128_to_u8_try => .u128_to_u8_try,
        .u128_to_u16_wrap => .u128_to_u16_wrap,
        .u128_to_u16_try => .u128_to_u16_try,
        .u128_to_u32_wrap => .u128_to_u32_wrap,
        .u128_to_u32_try => .u128_to_u32_try,
        .u128_to_u64_wrap => .u128_to_u64_wrap,
        .u128_to_u64_try => .u128_to_u64_try,
        .u128_to_f32 => .u128_to_f32,
        .u128_to_f64 => .u128_to_f64,
        .u128_to_dec_try_unsafe => .u128_to_dec_try_unsafe,

        // Numeric conversion operations (I128)
        .i128_to_i8_wrap => .i128_to_i8_wrap,
        .i128_to_i8_try => .i128_to_i8_try,
        .i128_to_i16_wrap => .i128_to_i16_wrap,
        .i128_to_i16_try => .i128_to_i16_try,
        .i128_to_i32_wrap => .i128_to_i32_wrap,
        .i128_to_i32_try => .i128_to_i32_try,
        .i128_to_i64_wrap => .i128_to_i64_wrap,
        .i128_to_i64_try => .i128_to_i64_try,
        .i128_to_u8_wrap => .i128_to_u8_wrap,
        .i128_to_u8_try => .i128_to_u8_try,
        .i128_to_u16_wrap => .i128_to_u16_wrap,
        .i128_to_u16_try => .i128_to_u16_try,
        .i128_to_u32_wrap => .i128_to_u32_wrap,
        .i128_to_u32_try => .i128_to_u32_try,
        .i128_to_u64_wrap => .i128_to_u64_wrap,
        .i128_to_u64_try => .i128_to_u64_try,
        .i128_to_u128_wrap => .i128_to_u128_wrap,
        .i128_to_u128_try => .i128_to_u128_try,
        .i128_to_f32 => .i128_to_f32,
        .i128_to_f64 => .i128_to_f64,
        .i128_to_dec_try_unsafe => .i128_to_dec_try_unsafe,

        // Numeric conversion operations (F32)
        .f32_to_i8_trunc => .f32_to_i8_trunc,
        .f32_to_i8_try_unsafe => .f32_to_i8_try_unsafe,
        .f32_to_i16_trunc => .f32_to_i16_trunc,
        .f32_to_i16_try_unsafe => .f32_to_i16_try_unsafe,
        .f32_to_i32_trunc => .f32_to_i32_trunc,
        .f32_to_i32_try_unsafe => .f32_to_i32_try_unsafe,
        .f32_to_i64_trunc => .f32_to_i64_trunc,
        .f32_to_i64_try_unsafe => .f32_to_i64_try_unsafe,
        .f32_to_i128_trunc => .f32_to_i128_trunc,
        .f32_to_i128_try_unsafe => .f32_to_i128_try_unsafe,
        .f32_to_u8_trunc => .f32_to_u8_trunc,
        .f32_to_u8_try_unsafe => .f32_to_u8_try_unsafe,
        .f32_to_u16_trunc => .f32_to_u16_trunc,
        .f32_to_u16_try_unsafe => .f32_to_u16_try_unsafe,
        .f32_to_u32_trunc => .f32_to_u32_trunc,
        .f32_to_u32_try_unsafe => .f32_to_u32_try_unsafe,
        .f32_to_u64_trunc => .f32_to_u64_trunc,
        .f32_to_u64_try_unsafe => .f32_to_u64_try_unsafe,
        .f32_to_u128_trunc => .f32_to_u128_trunc,
        .f32_to_u128_try_unsafe => .f32_to_u128_try_unsafe,
        .f32_to_f64 => .f32_to_f64,

        // Numeric conversion operations (F64)
        .f64_to_i8_trunc => .f64_to_i8_trunc,
        .f64_to_i8_try_unsafe => .f64_to_i8_try_unsafe,
        .f64_to_i16_trunc => .f64_to_i16_trunc,
        .f64_to_i16_try_unsafe => .f64_to_i16_try_unsafe,
        .f64_to_i32_trunc => .f64_to_i32_trunc,
        .f64_to_i32_try_unsafe => .f64_to_i32_try_unsafe,
        .f64_to_i64_trunc => .f64_to_i64_trunc,
        .f64_to_i64_try_unsafe => .f64_to_i64_try_unsafe,
        .f64_to_i128_trunc => .f64_to_i128_trunc,
        .f64_to_i128_try_unsafe => .f64_to_i128_try_unsafe,
        .f64_to_u8_trunc => .f64_to_u8_trunc,
        .f64_to_u8_try_unsafe => .f64_to_u8_try_unsafe,
        .f64_to_u16_trunc => .f64_to_u16_trunc,
        .f64_to_u16_try_unsafe => .f64_to_u16_try_unsafe,
        .f64_to_u32_trunc => .f64_to_u32_trunc,
        .f64_to_u32_try_unsafe => .f64_to_u32_try_unsafe,
        .f64_to_u64_trunc => .f64_to_u64_trunc,
        .f64_to_u64_try_unsafe => .f64_to_u64_try_unsafe,
        .f64_to_u128_trunc => .f64_to_u128_trunc,
        .f64_to_u128_try_unsafe => .f64_to_u128_try_unsafe,
        .f64_to_f32_wrap => .f64_to_f32_wrap,
        .f64_to_f32_try_unsafe => .f64_to_f32_try_unsafe,

        // Numeric conversion operations (Dec)
        .dec_to_i8_trunc => .dec_to_i8_trunc,
        .dec_to_i8_try_unsafe => .dec_to_i8_try_unsafe,
        .dec_to_i16_trunc => .dec_to_i16_trunc,
        .dec_to_i16_try_unsafe => .dec_to_i16_try_unsafe,
        .dec_to_i32_trunc => .dec_to_i32_trunc,
        .dec_to_i32_try_unsafe => .dec_to_i32_try_unsafe,
        .dec_to_i64_trunc => .dec_to_i64_trunc,
        .dec_to_i64_try_unsafe => .dec_to_i64_try_unsafe,
        .dec_to_i128_trunc => .dec_to_i128_trunc,
        .dec_to_i128_try_unsafe => .dec_to_i128_try_unsafe,
        .dec_to_u8_trunc => .dec_to_u8_trunc,
        .dec_to_u8_try_unsafe => .dec_to_u8_try_unsafe,
        .dec_to_u16_trunc => .dec_to_u16_trunc,
        .dec_to_u16_try_unsafe => .dec_to_u16_try_unsafe,
        .dec_to_u32_trunc => .dec_to_u32_trunc,
        .dec_to_u32_try_unsafe => .dec_to_u32_try_unsafe,
        .dec_to_u64_trunc => .dec_to_u64_trunc,
        .dec_to_u64_try_unsafe => .dec_to_u64_try_unsafe,
        .dec_to_u128_trunc => .dec_to_u128_trunc,
        .dec_to_u128_try_unsafe => .dec_to_u128_try_unsafe,
        .dec_to_f32_wrap => .dec_to_f32_wrap,
        .dec_to_f32_try_unsafe => .dec_to_f32_try_unsafe,
        .dec_to_f64 => .dec_to_f64,

        else => null,
    };
}

/// Convert a CIR *_to_str low-level op to the corresponding MonoExpr.
/// These ops need type-specific MonoExprs (int_to_str, float_to_str, dec_to_str)
/// rather than a generic low_level wrapper, because the backend needs the precision info.
/// Returns null if the op is not a *_to_str operation.
fn toStrMonoExpr(op: CIR.Expr.LowLevel, module_env: *ModuleEnv, args: anytype, self: *Self) ?ir.MonoExpr {
    const arg_indices = module_env.store.sliceExpr(args);
    if (arg_indices.len != 1) return null;
    const arg_idx = arg_indices[0];
    const arg_id = self.lowerExprFromIdx(module_env, arg_idx) catch return null;
    return switch (op) {
        .u8_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .u8 } },
        .i8_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .i8 } },
        .u16_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .u16 } },
        .i16_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .i16 } },
        .u32_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .u32 } },
        .i32_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .i32 } },
        .u64_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .u64 } },
        .i64_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .i64 } },
        .u128_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .u128 } },
        .i128_to_str => .{ .int_to_str = .{ .value = arg_id, .int_precision = .i128 } },
        .dec_to_str => .{ .dec_to_str = arg_id },
        .f32_to_str => .{ .float_to_str = .{ .value = arg_id, .float_precision = .f32 } },
        .f64_to_str => .{ .float_to_str = .{ .value = arg_id, .float_precision = .f64 } },
        else => null,
    };
}

/// Look up whether an external definition is a low-level lambda.
/// Returns the low-level lambda data if so, null otherwise.
fn getExternalLowLevelLambda(self: *Self, caller_env: *ModuleEnv, lookup: anytype) ?@FieldType(CIR.Expr, "e_low_level_lambda") {
    const ext_module_idx = caller_env.imports.getResolvedModule(lookup.module_idx) orelse return null;
    if (ext_module_idx >= self.all_module_envs.len) return null;
    const ext_env = self.all_module_envs[ext_module_idx];
    // Check if this is actually a def node (type modules with hosted functions may have different node types)
    if (!ext_env.store.isDefNode(lookup.target_node_idx)) return null;
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const def = ext_env.store.getDef(def_idx);
    const def_expr = ext_env.store.getExpr(def.expr);
    return if (def_expr == .e_low_level_lambda) def_expr.e_low_level_lambda else null;
}

/// Look up whether an external definition is a hosted lambda.
/// Returns the hosted function index if so, null otherwise.
fn getExternalHostedLambdaIndex(self: *Self, caller_env: *ModuleEnv, lookup: anytype) ?u32 {
    const hosted_fns = self.hosted_functions orelse return null;
    const ext_module_idx = caller_env.imports.getResolvedModule(lookup.module_idx) orelse return null;
    if (ext_module_idx >= self.all_module_envs.len) return null;
    return hosted_fns.get(hostedFunctionKey(@intCast(ext_module_idx), lookup.target_node_idx));
}

/// Set up layout hints for a local function call.
/// When a generic function (like `walk`) is called from within the same module,
/// the function's parameter type variables (rigids from its annotation) may not
/// be in the type scope. This function pre-computes parameter layouts from the
/// call arguments so that `getPatternLayout` returns the correct concrete layouts.
fn setupLocalCallLayoutHints(
    self: *Self,
    module_env: *ModuleEnv,
    lookup_pattern_idx: CIR.Pattern.Idx,
    call_args: CIR.Expr.Span,
) Allocator.Error!void {
    // Find the CIR expression for the definition
    const def_expr_idx: CIR.Expr.Idx = blk: {
        if (self.findDefForPattern(module_env, lookup_pattern_idx)) |def_idx| {
            break :blk module_env.store.getDef(def_idx).expr;
        }
        if (self.deferred_defs.get(@intFromEnum(lookup_pattern_idx))) |deferred| {
            break :blk deferred;
        }
        return;
    };

    // Get the lambda's parameter patterns
    const def_expr = module_env.store.getExpr(def_expr_idx);
    const lambda_args: CIR.Pattern.Span = switch (def_expr) {
        .e_lambda => |l| l.args,
        .e_closure => |c| blk: {
            const inner = module_env.store.getExpr(c.lambda_idx);
            break :blk switch (inner) {
                .e_lambda => |l| l.args,
                else => return,
            };
        },
        else => return,
    };

    const param_pattern_indices = module_env.store.slicePatterns(lambda_args);
    const arg_indices = module_env.store.sliceExpr(call_args);

    // Also populate the type scope by walking the function's type signature.
    // This handles nested type variables (e.g., the element type `a` inside `List a`)
    // that are not directly represented by the parameter patterns.
    const func_type_var = ModuleEnv.varFrom(def_expr_idx);
    const func_resolved = module_env.types.resolveVar(func_type_var);
    if (func_resolved.desc.content.unwrapFunc()) |func| {
        const param_vars = module_env.types.sliceVars(func.args);
        const num_type_mappings = @min(param_vars.len, arg_indices.len);

        // Ensure we have a scope
        if (self.type_scope.scopes.items.len == 0) {
            try self.type_scope.scopes.append(types.VarMap.init(self.allocator));
        }
        const scope = &self.type_scope.scopes.items[0];

        for (0..num_type_mappings) |i| {
            const param_var = param_vars[i];
            const arg_idx = arg_indices[i];
            const caller_arg_var = ModuleEnv.varFrom(arg_idx);
            try self.collectTypeMappingsWithExpr(scope, module_env, param_var, module_env, caller_arg_var, arg_idx);
        }
    }

    const num_mappings = @min(param_pattern_indices.len, arg_indices.len);
    for (0..num_mappings) |i| {
        const param_pattern_idx = param_pattern_indices[i];
        const arg_idx = arg_indices[i];

        // Get the param's type var and resolve it
        const param_type_var = ModuleEnv.varFrom(param_pattern_idx);
        const resolved = module_env.types.resolveVar(param_type_var);

        // Only add hints for type vars that are flex or rigid (generic)
        if (resolved.desc.content != .flex and resolved.desc.content != .rigid) continue;

        // Check if already in the type scope (no need to add a hint)
        if (self.type_scope.lookup(resolved.var_) != null) continue;

        // Compute the layout from the arg's type
        const arg_layout = self.getExprLayoutFromIdx(module_env, arg_idx);

        // Store the hint: when this resolved type var is encountered,
        // use the layout computed from the argument
        try self.expr_layout_hints.put(resolved.var_, arg_layout);
    }
}

/// Set up type scope mappings for an external function call.
/// This maps the external function's rigid type variables to concrete types from the call site.
/// The call_expr_idx is the call expression itself, used to map return type params.
fn setupExternalCallTypeScope(
    self: *Self,
    caller_module_env: *ModuleEnv,
    lookup: anytype,
    args: CIR.Expr.Span,
    call_expr_idx: CIR.Expr.Idx,
) Allocator.Error!void {
    // Get the external module
    const ext_module_idx = caller_module_env.imports.getResolvedModule(lookup.module_idx) orelse return;
    if (ext_module_idx >= self.all_module_envs.len) return;
    const ext_module_env = self.all_module_envs[ext_module_idx];

    // Check if this is actually a def node (type modules with hosted functions may have different node types)
    if (!ext_module_env.store.isDefNode(lookup.target_node_idx)) {
        return;
    }

    // Get the external definition
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const def = ext_module_env.store.getDef(def_idx);

    // Get the external function's type
    const ext_type_var = ModuleEnv.varFrom(def.expr);
    const ext_resolved = ext_module_env.types.resolveVar(ext_type_var);

    // Check if it's a function type
    const func = ext_resolved.desc.content.unwrapFunc() orelse return;

    // Get argument expressions from call site
    const arg_indices = caller_module_env.store.sliceExpr(args);

    // Ensure we have a scope to put mappings into
    if (self.type_scope.scopes.items.len == 0) {
        try self.type_scope.scopes.append(types.VarMap.init(self.allocator));
    }
    const scope = &self.type_scope.scopes.items[0];

    // Track the caller module - mapped vars in type_scope belong to this module.
    // IMPORTANT: Only set this for the OUTERMOST external call!
    // For nested calls (e.g., List.map calling List.with_capacity), we should preserve
    // the original caller's module context, not overwrite it with the builtins module.
    if (self.type_scope_caller_module == null) {
        self.type_scope_caller_module = self.current_module_idx;
    }

    // Get the function's parameter type variables
    const param_vars = ext_module_env.types.sliceVars(func.args);

    // Map each external rigid param type to the call site's argument type
    const num_mappings = @min(param_vars.len, arg_indices.len);
    for (0..num_mappings) |i| {
        const ext_param_var = param_vars[i];
        const caller_arg_idx = arg_indices[i];
        const caller_arg_var = ModuleEnv.varFrom(caller_arg_idx);

        // Walk the external parameter type to find rigid vars
        // and map them to corresponding parts of the caller's arg type.
        // Pass the expression index so we can look at the actual expression
        // when type resolution fails (e.g., for nested lists with unresolved numerics).
        try self.collectTypeMappingsWithExpr(
            scope,
            ext_module_env,
            ext_param_var,
            caller_module_env,
            caller_arg_var,
            caller_arg_idx,
        );
    }

    // Also map the return type. This is important for functions like List.map
    // where the return type has type parameters (List b) that are used in the body.
    // The call expression's type represents the return type after unification with the caller's context.
    const caller_return_var = ModuleEnv.varFrom(call_expr_idx);
    try self.collectTypeMappings(scope, ext_module_env, func.ret, caller_module_env, caller_return_var);
}

/// Recursively collect type variable mappings by walking two types in parallel.
/// Maps rigid vars in ext_type to corresponding parts of caller_type.
/// Collect resolved rigid type variable representatives from a type, recursing into
/// structures like List, function types, etc.
fn collectRigidVars(self: *Self, env: *const ModuleEnv, type_var: types.Var, out: []types.Var, count: *usize) void {
    if (count.* >= out.len) return;
    const resolved = env.types.resolveVar(type_var);
    switch (resolved.desc.content) {
        .rigid => {
            // Add this rigid var if not already present
            for (out[0..count.*]) |existing| {
                if (existing == resolved.var_) return;
            }
            out[count.*] = resolved.var_;
            count.* += 1;
        },
        .structure => |st| switch (st) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const param_vars = env.types.sliceVars(func.args);
                for (param_vars) |pv| {
                    self.collectRigidVars(env, pv, out, count);
                }
                self.collectRigidVars(env, func.ret, out, count);
            },
            .nominal_type => |nt| {
                const type_args = env.types.sliceNominalArgs(nt);
                for (type_args) |ta| {
                    self.collectRigidVars(env, ta, out, count);
                }
            },
            .tuple => |tup| {
                const elems = env.types.sliceVars(tup.elems);
                for (elems) |e| {
                    self.collectRigidVars(env, e, out, count);
                }
            },
            else => {},
        },
        .alias => |alias| {
            const backing = env.types.getAliasBackingVar(alias);
            self.collectRigidVars(env, backing, out, count);
        },
        else => {},
    }
}

/// Walk two types from the same module in parallel. When the first type has a
/// flex var where the second has a concrete type, add a scope entry mapping the
/// flex var to the concrete var. This bridges gaps when a callback parameter's
/// type has unresolved flex payload vars but the actual argument's type (already
/// unified by the type checker) has concrete payloads.
fn addIntraModuleMappings(
    self: *Self,
    scope: *types.VarMap,
    env: *const ModuleEnv,
    unresolved_var: types.Var,
    concrete_var: types.Var,
) Allocator.Error!void {
    const ur = env.types.resolveVar(unresolved_var);
    const cr = env.types.resolveVar(concrete_var);

    // If they resolve to the same var, nothing to do
    if (ur.var_ == cr.var_) return;

    // If the unresolved side is flex, add a mapping — but only if the concrete side
    // is actually concrete (a structure or alias). Mapping flex→flex is pointless.
    if (ur.desc.content == .flex or ur.desc.content == .rigid) {
        if (cr.desc.content == .structure or cr.desc.content == .alias) {
            if (scope.get(ur.var_) == null) {
                try scope.put(ur.var_, cr.var_);
            }
        }
        return;
    }

    // Both must be structures to recurse
    if (ur.desc.content != .structure or cr.desc.content != .structure) return;

    const us = ur.desc.content.structure;
    const cs = cr.desc.content.structure;

    switch (us) {
        .tag_union => |u_tu| {
            const c_tu = if (cs == .tag_union) cs.tag_union else return;
            const u_tags = env.types.getTagsSlice(u_tu.tags);
            const c_tags = env.types.getTagsSlice(c_tu.tags);
            const ident = env.getIdentStoreConst();

            for (u_tags.items(.name), u_tags.items(.args)) |u_name, u_args| {
                const u_text = ident.getText(u_name);
                const u_pvars = env.types.sliceVars(u_args);

                for (c_tags.items(.name), c_tags.items(.args)) |c_name, c_args| {
                    const c_text = ident.getText(c_name);
                    if (std.mem.eql(u8, u_text, c_text)) {
                        const c_pvars = env.types.sliceVars(c_args);
                        const num = @min(u_pvars.len, c_pvars.len);
                        for (0..num) |j| {
                            try self.addIntraModuleMappings(scope, env, u_pvars[j], c_pvars[j]);
                        }
                        break;
                    }
                }
            }
            // Don't recurse into extension vars — they represent "no more tags"
            // for closed tag unions and mapping them can interfere with layout computation.
        },
        .nominal_type => |u_nt| {
            const c_nt = if (cs == .nominal_type) cs.nominal_type else return;
            const u_args = env.types.sliceNominalArgs(u_nt);
            const c_args = env.types.sliceNominalArgs(c_nt);
            const num = @min(u_args.len, c_args.len);
            for (0..num) |i| {
                try self.addIntraModuleMappings(scope, env, u_args[i], c_args[i]);
            }
        },
        .record => |u_rec| {
            const c_rec = if (cs == .record) cs.record else return;
            const u_fields = env.types.getRecordFieldsSlice(u_rec.fields);
            const c_fields = env.types.getRecordFieldsSlice(c_rec.fields);
            const num = @min(u_fields.len, c_fields.len);
            for (0..num) |i| {
                try self.addIntraModuleMappings(scope, env, u_fields.get(i).var_, c_fields.get(i).var_);
            }
        },
        .tuple => |u_tup| {
            if (cs == .tuple) {
                const c_tup = cs.tuple;
                const u_elems = env.types.sliceVars(u_tup.elems);
                const c_elems = env.types.sliceVars(c_tup.elems);
                const num = @min(u_elems.len, c_elems.len);
                for (0..num) |i| {
                    try self.addIntraModuleMappings(scope, env, u_elems[i], c_elems[i]);
                }
            }
        },
        else => {},
    }
}

fn collectTypeMappings(
    self: *Self,
    scope: *types.VarMap,
    ext_env: *const ModuleEnv,
    ext_var: types.Var,
    caller_env: *ModuleEnv,
    caller_var: types.Var,
) Allocator.Error!void {
    return self.collectTypeMappingsWithExpr(scope, ext_env, ext_var, caller_env, caller_var, null);
}

/// Recursively collect type variable mappings by walking two types in parallel.
/// Maps rigid vars in ext_type to corresponding parts of caller_type.
/// When caller_expr_idx is provided and type resolution fails (e.g., for lists with
/// unresolved numeric elements), falls back to examining the actual expression.
fn collectTypeMappingsWithExpr(
    self: *Self,
    scope: *types.VarMap,
    ext_env: *const ModuleEnv,
    ext_var: types.Var,
    caller_env: *ModuleEnv,
    caller_var: types.Var,
    caller_expr_idx: ?CIR.Expr.Idx,
) Allocator.Error!void {
    const ext_resolved = ext_env.types.resolveVar(ext_var);
    const caller_resolved = caller_env.types.resolveVar(caller_var);

    switch (ext_resolved.desc.content) {
        .rigid => {
            // Found a rigid - map it to the caller's type variable
            // Special handling: if we're mapping to a flex but the expression is a list,
            // we need to pre-compute the list layout. Otherwise the layout store will see
            // the flex and return a scalar layout (Dec) instead of List layout.
            if (caller_resolved.desc.content == .flex) {
                if (caller_expr_idx) |expr_idx| {
                    const caller_expr = caller_env.store.getExpr(expr_idx);
                    if (caller_expr == .e_list) {
                        // The expression is a list but the type is flex.
                        // Compute the list layout based on the expression's elements.
                        const list = caller_expr.e_list;
                        if (self.layout_store) |ls| {
                            const list_layout = ls.computeListLayout(
                                self.current_module_idx,
                                caller_env,
                                list.elems,
                                &self.type_scope,
                                self.type_scope_caller_module,
                            ) catch {
                                // Fall back to normal mapping if layout computation fails
                                try scope.put(ext_resolved.var_, caller_resolved.var_);
                                return;
                            };
                            self.expr_layout_hints.put(ext_resolved.var_, list_layout) catch {};
                            // Still map the var for other lookups
                            try scope.put(ext_resolved.var_, caller_resolved.var_);
                            return;
                        }
                    }
                }
            }
            // For nested calls: if the caller's type is also rigid/flex (because the
            // intermediate function is generic), resolve transitively through the existing
            // type scope. Otherwise fromTypeVar would try to resolve a module N var using
            // the outermost caller's module (module 0), giving wrong results.
            if (caller_resolved.desc.content == .rigid or caller_resolved.desc.content == .flex) {
                if (self.type_scope.lookup(caller_resolved.var_)) |transitive_var| {
                    try scope.put(ext_resolved.var_, transitive_var);
                    return;
                }
            }
            try scope.put(ext_resolved.var_, caller_resolved.var_);
        },
        .flex => {
            // Flex vars might also need mapping if they're unresolved
            // Same transitive resolution as rigid vars for nested calls.
            if (caller_resolved.desc.content == .rigid or caller_resolved.desc.content == .flex) {
                if (self.type_scope.lookup(caller_resolved.var_)) |transitive_var| {
                    try scope.put(ext_resolved.var_, transitive_var);
                    return;
                }
            }
            try scope.put(ext_resolved.var_, caller_resolved.var_);
        },
        .structure => |st| {
            // Recurse into structure to find nested rigids
            switch (st) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    const ext_params = ext_env.types.sliceVars(func.args);
                    const caller_func = caller_resolved.desc.content.unwrapFunc() orelse return;
                    const caller_params = caller_env.types.sliceVars(caller_func.args);
                    const num = @min(ext_params.len, caller_params.len);
                    for (0..num) |i| {
                        try self.collectTypeMappings(scope, ext_env, ext_params[i], caller_env, caller_params[i]);
                    }
                    try self.collectTypeMappings(scope, ext_env, func.ret, caller_env, caller_func.ret);
                },
                .record => |rec| {
                    const caller_rec = caller_resolved.desc.content.unwrapRecord() orelse return;
                    const ext_fields = ext_env.types.getRecordFieldsSlice(rec.fields);
                    const caller_fields = caller_env.types.getRecordFieldsSlice(caller_rec.fields);
                    const num = @min(ext_fields.len, caller_fields.len);
                    for (0..num) |i| {
                        try self.collectTypeMappings(scope, ext_env, ext_fields.get(i).var_, caller_env, caller_fields.get(i).var_);
                    }
                },
                .tuple => |tup| {
                    const ext_elems = ext_env.types.sliceVars(tup.elems);
                    if (caller_resolved.desc.content == .structure) {
                        if (caller_resolved.desc.content.structure == .tuple) {
                            const caller_tup = caller_resolved.desc.content.structure.tuple;
                            const caller_elems = caller_env.types.sliceVars(caller_tup.elems);
                            const num = @min(ext_elems.len, caller_elems.len);
                            for (0..num) |i| {
                                try self.collectTypeMappings(scope, ext_env, ext_elems[i], caller_env, caller_elems[i]);
                            }
                        }
                    }
                },
                .nominal_type => |nt| {
                    // For nominal types, map the type arguments
                    const ext_args = ext_env.types.sliceNominalArgs(nt);
                    if (caller_resolved.desc.content.unwrapNominalType()) |caller_nt| {
                        // Caller type is also nominal, recurse normally
                        const caller_args = caller_env.types.sliceNominalArgs(caller_nt);
                        const num = @min(ext_args.len, caller_args.len);
                        for (0..num) |i| {
                            // For list expressions, use the actual element's type var instead
                            // of the type arg from the nominal. This handles cases where the
                            // type arg is an unresolved flex but the element is a list.
                            if (caller_expr_idx) |expr_idx| {
                                const caller_expr = caller_env.store.getExpr(expr_idx);
                                if (caller_expr == .e_list) {
                                    const list = caller_expr.e_list;
                                    const elems = caller_env.store.exprSlice(list.elems);
                                    if (i < elems.len) {
                                        // Use the element's type var, not the type arg from the nominal
                                        const elem_type_var = ModuleEnv.varFrom(elems[i]);
                                        try self.collectTypeMappingsWithExpr(scope, ext_env, ext_args[i], caller_env, elem_type_var, elems[i]);
                                        continue;
                                    }
                                }
                            }
                            try self.collectTypeMappings(scope, ext_env, ext_args[i], caller_env, caller_args[i]);
                        }
                    } else if (caller_expr_idx) |expr_idx| {
                        // Caller type is not nominal (maybe flex due to unresolved numerics).
                        // Try to get element type from the actual expression.
                        // This handles cases like [[10], [20], [30]] where the outer list's
                        // element type var resolves to flex instead of List.
                        const caller_expr = caller_env.store.getExpr(expr_idx);
                        switch (caller_expr) {
                            .e_list => |list| {
                                const elems = caller_env.store.exprSlice(list.elems);
                                if (elems.len > 0 and ext_args.len > 0) {
                                    // Use the first element's type var and expression
                                    const first_elem_var = ModuleEnv.varFrom(elems[0]);
                                    try self.collectTypeMappingsWithExpr(scope, ext_env, ext_args[0], caller_env, first_elem_var, elems[0]);
                                }
                            },
                            else => {},
                        }
                    }
                },
                .tag_union => |tu| {
                    // Recurse into tag union variants to map payload type vars.
                    // Without this, flex vars inside tag union payloads (e.g.,
                    // the `a` in `[Ok({}), Err(a)]`) won't be mapped to their
                    // concrete types from the caller's tag union.
                    const caller_tu = caller_resolved.desc.content.unwrapTagUnion() orelse return;
                    const ext_tags = ext_env.types.getTagsSlice(tu.tags);
                    const caller_tags = caller_env.types.getTagsSlice(caller_tu.tags);
                    const ext_ident = ext_env.getIdentStoreConst();
                    const caller_ident = caller_env.getIdentStoreConst();

                    // Match tags by name and recurse into payload vars
                    for (ext_tags.items(.name), ext_tags.items(.args)) |ext_name, ext_args| {
                        const ext_text = ext_ident.getText(ext_name);
                        const ext_payload_vars = ext_env.types.sliceVars(ext_args);

                        for (caller_tags.items(.name), caller_tags.items(.args)) |caller_name, caller_args| {
                            const caller_text = caller_ident.getText(caller_name);
                            if (std.mem.eql(u8, ext_text, caller_text)) {
                                // Same tag name — recurse into payload vars
                                const caller_payload_vars = caller_env.types.sliceVars(caller_args);
                                const num = @min(ext_payload_vars.len, caller_payload_vars.len);
                                for (0..num) |j| {
                                    try self.collectTypeMappings(scope, ext_env, ext_payload_vars[j], caller_env, caller_payload_vars[j]);
                                }
                                break;
                            }
                        }
                    }

                    // Also recurse into extension vars
                    try self.collectTypeMappings(scope, ext_env, tu.ext, caller_env, caller_tu.ext);
                },
                else => {},
            }
        },
        .alias => |alias| {
            // Follow the alias
            const backing = ext_env.types.getAliasBackingVar(alias);
            try self.collectTypeMappings(scope, ext_env, backing, caller_env, caller_var);
        },
        .err => {},
    }
}

/// Lower a single expression
pub fn lowerExpr(self: *Self, module_idx: u16, expr_idx: CIR.Expr.Idx) Allocator.Error!MonoExprId {
    const old_module = self.current_module_idx;
    self.current_module_idx = module_idx;
    defer self.current_module_idx = old_module;

    const module_env = self.getModuleEnv(module_idx) orelse unreachable;
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);

    return self.lowerExprInner(module_env, expr, region, expr_idx);
}

/// Lower an expression with its CIR representation
fn lowerExprInner(self: *Self, module_env: *ModuleEnv, expr: CIR.Expr, region: Region, expr_idx: CIR.Expr.Idx) Allocator.Error!MonoExprId {
    const mono_expr: MonoExpr = switch (expr) {
        .e_num => |num| blk: {
            const val = num.value.toI128();
            // Use the computed layout from type inference to determine the literal type.
            // The layout reflects the actual resolved type (e.g., U64 from function signature),
            // not just the syntactic hint from num.kind.
            const layout_idx = self.getExprLayoutFromIdx(module_env, expr_idx);
            const is_dec = layout_idx == .dec;

            if (is_dec) {
                // Dec values are scaled by 10^18 (one_point_zero = 10^18)
                const one_point_zero: i128 = 1_000_000_000_000_000_000;
                const scaled_val = val * one_point_zero;
                break :blk .{ .dec_literal = scaled_val };
            }
            if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                break :blk .{ .i64_literal = @intCast(val) };
            }
            break :blk .{ .i128_literal = val };
        },

        .e_frac_f32 => |f| .{ .f32_literal = f.value },
        .e_frac_f64 => |f| .{ .f64_literal = f.value },
        .e_dec => |d| .{ .dec_literal = d.value.num },
        .e_dec_small => |d| .{ .dec_literal = d.value.toRocDec().num },

        .e_str_segment => |seg| blk: {
            // Copy the string from the module to the mono store
            const str_text = module_env.common.getString(seg.literal);
            const mono_idx = self.store.insertString(str_text) catch return error.OutOfMemory;
            break :blk .{ .str_literal = mono_idx };
        },
        .e_str => |str| blk: {
            // For now, handle simple strings - interpolation needs more work
            const segments = module_env.store.sliceExpr(str.span);
            if (segments.len == 1) {
                const seg_expr = module_env.store.getExpr(segments[0]);
                if (seg_expr == .e_str_segment) {
                    // Copy the string from the module to the mono store
                    const str_text = module_env.common.getString(seg_expr.e_str_segment.literal);
                    const mono_idx = self.store.insertString(str_text) catch return error.OutOfMemory;
                    break :blk .{ .str_literal = mono_idx };
                }
            }
            // Complex string - lower all segments
            const lowered = try self.lowerExprSpan(module_env, str.span);
            break :blk .{ .list = .{ .elem_layout = .str, .elems = lowered } };
        },

        .e_empty_record => .{ .empty_record = {} },
        .e_empty_list => .{ .empty_list = .{ .elem_layout = .i64 } }, // TODO: get actual elem layout

        .e_lookup_local => |lookup| blk: {
            const symbol = self.patternToSymbol(lookup.pattern_idx);

            // Ensure the local definition is lowered if it's a top-level def
            const symbol_key: u48 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key)) {
                // Bridge the lookup expression's resolved type with the definition's type.
                // When a lambda is passed as an argument to a polymorphic function (e.g., List.fold),
                // the lookup expression's type has concrete payloads (e.g., [A(I64), B(I64)])
                // resolved through unification, but the lambda definition's internal type still has
                // flex vars (e.g., [A(*12), B(*19)]). Adding intra-module mappings ensures layout
                // computation inside the lambda body resolves these flex vars to concrete types.
                //
                // Only do this when we're in the caller module (not inside an external def).
                // Inside external defs, current_module_idx differs from type_scope_caller_module,
                // and adding external module vars to a scope resolved via the caller's type store
                // would cause cross-module variable resolution errors.
                const in_caller_module = self.type_scope_caller_module == null or
                    self.type_scope_caller_module.? == self.current_module_idx;
                if (in_caller_module and self.type_scope.scopes.items.len > 0) {
                    const pattern_key = @intFromEnum(lookup.pattern_idx);
                    const def_expr_idx: ?CIR.Expr.Idx = def_blk: {
                        if (self.findDefForPattern(module_env, lookup.pattern_idx)) |def_idx| {
                            break :def_blk module_env.store.getDef(def_idx).expr;
                        }
                        if (self.deferred_defs.get(pattern_key)) |deferred| {
                            break :def_blk deferred;
                        }
                        break :def_blk null;
                    };
                    if (def_expr_idx) |def_idx| {
                        // Only bridge function PARAMETER types, not the entire function type.
                        // Bridging return types can add spurious scope entries that interfere
                        // with layout computation for other expressions.
                        const def_type_var = ModuleEnv.varFrom(def_idx);
                        const lookup_type_var = ModuleEnv.varFrom(expr_idx);
                        const def_resolved = module_env.types.resolveVar(def_type_var);
                        const lookup_resolved = module_env.types.resolveVar(lookup_type_var);
                        if (def_resolved.desc.content.unwrapFunc()) |def_func| {
                            if (lookup_resolved.desc.content.unwrapFunc()) |lookup_func| {
                                const scope = &self.type_scope.scopes.items[0];
                                const def_params = module_env.types.sliceVars(def_func.args);
                                const lookup_params = module_env.types.sliceVars(lookup_func.args);
                                const num = @min(def_params.len, lookup_params.len);
                                for (0..num) |i| {
                                    try self.addIntraModuleMappings(scope, module_env, def_params[i], lookup_params[i]);
                                }
                            }
                        }
                    }
                }
                try self.lowerLocalDefByPattern(module_env, symbol, lookup.pattern_idx);
            }

            break :blk .{ .lookup = .{
                .symbol = symbol,
                .layout_idx = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_lookup_external => |lookup| blk: {
            const symbol = self.externalToSymbol(lookup.module_idx, lookup.ident_idx);

            // Ensure the external definition is lowered using target_node_idx
            const symbol_key: u48 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key)) {
                // Lower the external definition using the direct Def.Idx
                try self.lowerExternalDefByIdx(symbol, lookup.target_node_idx);
            }

            break :blk .{ .lookup = .{
                .symbol = symbol,
                .layout_idx = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_lookup_required => |lookup| blk: {
            // Required lookups reference values from the app module that satisfy
            // the platform's `requires` clause. E.g., `main!` in `requires { main! : ... }`

            // Get the app module index (set during lowerer initialization)
            const app_idx = self.app_module_idx orelse break :blk .{ .runtime_error = {} };
            if (app_idx >= self.all_module_envs.len) break :blk .{ .runtime_error = {} };

            const app_env = self.all_module_envs[app_idx];

            // Get the required identifier name from the platform's requires_types
            const required_type = module_env.requires_types.get(lookup.requires_idx);
            const required_name = module_env.getIdent(required_type.ident);

            // Find the matching export in the app module's exports
            const exports = app_env.store.sliceDefs(app_env.exports);
            var found_def_idx: ?CIR.Def.Idx = null;
            var found_ident_idx: ?Ident.Idx = null;

            for (exports) |def_idx| {
                const def = app_env.store.getDef(def_idx);
                const pattern = app_env.store.getPattern(def.pattern);
                if (pattern == .assign) {
                    const name = app_env.getIdent(pattern.assign.ident);
                    if (std.mem.eql(u8, name, required_name)) {
                        found_def_idx = def_idx;
                        found_ident_idx = pattern.assign.ident;
                        break;
                    }
                }
            }

            if (found_def_idx == null or found_ident_idx == null) {
                break :blk .{ .runtime_error = {} };
            }

            // Create symbol for the app's export
            const symbol = MonoSymbol{
                .module_idx = app_idx,
                .ident_idx = found_ident_idx.?,
            };

            // Ensure the app definition is lowered
            const symbol_key: u48 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key)) {
                try self.lowerExternalDefByIdx(symbol, @intCast(@intFromEnum(found_def_idx.?)));
            }

            break :blk .{ .lookup = .{
                .symbol = symbol,
                .layout_idx = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_call => |call| blk: {
            // Check if this is a call to a low-level lambda (like str_inspekt)
            const fn_expr = module_env.store.getExpr(call.func);

            // If calling an external function, set up type scope mappings before lowering.
            // We need to clean up after the call is done to avoid polluting subsequent calls.
            const is_external_call = fn_expr == .e_lookup_external;

            // Check for hosted lambda EARLY - before setupExternalCallTypeScope which doesn't handle them
            if (is_external_call) {
                const lookup = fn_expr.e_lookup_external;
                if (self.getExternalHostedLambdaIndex(module_env, lookup)) |hosted_index| {
                    const args = try self.lowerExprSpan(module_env, call.args);
                    const hosted_expr = MonoExpr{
                        .hosted_call = .{
                            .index = hosted_index,
                            .args = args,
                            .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                        },
                    };
                    return try self.store.addExpr(hosted_expr, region);
                }
            }

            const old_caller_module = self.type_scope_caller_module;
            if (is_external_call) {
                const lookup = fn_expr.e_lookup_external;
                try self.setupExternalCallTypeScope(module_env, lookup, call.args, expr_idx);
            }
            // For local function calls, set up layout hints so generic parameters
            // get the correct concrete layouts from the call arguments.
            const is_local_call_with_hints = fn_expr == .e_lookup_local;
            if (is_local_call_with_hints) {
                const lookup = fn_expr.e_lookup_local;
                try self.setupLocalCallLayoutHints(module_env, lookup.pattern_idx, call.args);
                // Set type_scope_caller_module so that fromTypeVar checks the type
                // scope for flex/rigid vars inside the lambda body. Without this,
                // layout computations for parameters with unresolved types (like list
                // elements) would return ZST instead of the concrete type.
                if (self.type_scope_caller_module == null and self.type_scope.scopes.items.len > 0) {
                    self.type_scope_caller_module = self.current_module_idx;
                }
            }
            // Clean up type scope after this expression, whether we exit normally or break early.
            // Only clear scope[0] if this call was the outermost one that set up the type scope
            // (old_caller_module was null). Inner calls (where old_caller_module was already set)
            // must not clear scope[0] because the outer call still needs those mappings.
            defer if (is_external_call or is_local_call_with_hints) {
                self.type_scope_caller_module = old_caller_module;
                if (old_caller_module == null) {
                    if (self.type_scope.scopes.items.len > 0) {
                        self.type_scope.scopes.items[0].clearRetainingCapacity();
                    }
                }
            };

            // For external calls to low-level lambdas, inline the operation directly.
            // This avoids going through lowerExternalDefByIdx which would compute
            // ret_layout in the builtins module context with potentially unmapped rigid vars.
            // The type scope has been set up above, so we can resolve the return type
            // using the external function's return type variable with the scope mappings.
            if (is_external_call) {
                const lookup = fn_expr.e_lookup_external;
                if (self.getExternalLowLevelLambda(module_env, lookup)) |ll| {
                    if (convertToMonoLowLevel(ll.op)) |mono_op| {
                        const args = try self.lowerExprSpan(module_env, call.args);
                        // Compute ret_layout from the external function's return type
                        // using the type scope mappings (which map rigid vars to caller types).
                        const ret_layout = ret_layout_blk: {
                            const ext_module_idx = module_env.imports.getResolvedModule(lookup.module_idx) orelse
                                break :ret_layout_blk self.getExprLayoutFromIdx(module_env, expr_idx);
                            if (ext_module_idx >= self.all_module_envs.len)
                                break :ret_layout_blk self.getExprLayoutFromIdx(module_env, expr_idx);
                            const ext_env = self.all_module_envs[ext_module_idx];
                            const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                            const def = ext_env.store.getDef(def_idx);
                            const ext_type_var = ModuleEnv.varFrom(def.expr);
                            const ext_resolved = ext_env.types.resolveVar(ext_type_var);
                            const func = ext_resolved.desc.content.unwrapFunc() orelse
                                break :ret_layout_blk self.getExprLayoutFromIdx(module_env, expr_idx);
                            const ls = self.layout_store orelse
                                break :ret_layout_blk self.getExprLayoutFromIdx(module_env, expr_idx);
                            break :ret_layout_blk ls.fromTypeVar(
                                @intCast(ext_module_idx),
                                func.ret,
                                &self.type_scope,
                                self.type_scope_caller_module,
                            ) catch self.getExprLayoutFromIdx(module_env, expr_idx);
                        };
                        break :blk .{
                            .low_level = .{
                                .op = mono_op,
                                .args = args,
                                .ret_layout = ret_layout,
                            },
                        };
                    }
                    // Non-convertible ops: check for str_inspekt which needs
                    // type-directed expansion rather than a simple op conversion.
                    if (ll.op == .str_inspekt) {
                        const arg_indices = module_env.store.sliceExpr(call.args);
                        if (arg_indices.len == 1) {
                            const arg_idx = arg_indices[0];
                            const arg_id = try self.lowerExprFromIdx(module_env, arg_idx);
                            const arg_type_var = ModuleEnv.varFrom(arg_idx);
                            const arg_layout = self.getExprLayoutFromIdx(module_env, arg_idx);
                            return self.lowerStrInspekt(arg_id, arg_type_var, arg_layout, module_env, region);
                        }
                    }
                    // Non-convertible ops: *_to_str needs type-specific MonoExprs.
                    if (toStrMonoExpr(ll.op, module_env, call.args, self)) |mono_expr| {
                        return try self.store.addExpr(mono_expr, region);
                    }
                }
            }

            if (fn_expr == .e_low_level_lambda) {
                const ll = fn_expr.e_low_level_lambda;
                if (ll.op == .str_inspekt) {
                    // Expand str_inspekt at lowering time
                    const arg_indices = module_env.store.sliceExpr(call.args);
                    if (arg_indices.len == 1) {
                        const arg_idx = arg_indices[0];
                        const arg_id = try self.lowerExprFromIdx(module_env, arg_idx);
                        // Get the type variable for the argument
                        // In the Zig implementation, expr indices ARE type variables
                        const arg_type_var = ModuleEnv.varFrom(arg_idx);
                        const arg_layout = self.getExprLayoutFromIdx(module_env, arg_idx);
                        return self.lowerStrInspekt(arg_id, arg_type_var, arg_layout, module_env, region);
                    }
                }
                // *_to_str ops need type-specific MonoExprs (int_to_str, float_to_str, dec_to_str)
                if (toStrMonoExpr(ll.op, module_env, call.args, self)) |mono_expr| {
                    return try self.store.addExpr(mono_expr, region);
                }
                // Convert CIR LowLevel ops to MonoExpr LowLevel ops
                // Using the CALL expression's type for ret_layout (not the lambda's rigid type vars)
                // because the lambda's type vars are from the builtin definition and may not be
                // in the type scope. The call expression's type IS resolved through the caller's context.
                if (convertToMonoLowLevel(ll.op)) |op| {
                    const args = try self.lowerExprSpan(module_env, call.args);
                    break :blk .{
                        .low_level = .{
                            .op = op,
                            .args = args,
                            .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                        },
                    };
                }
                // Fall through to general call handling for unhandled low-level ops
            }

            const fn_id = try self.lowerExprFromIdx(module_env, call.func);
            const args = try self.lowerExprSpan(module_env, call.args);
            break :blk .{
                .call = .{
                    .fn_expr = fn_id,
                    .fn_layout = self.getExprLayoutFromIdx(module_env, call.func),
                    .args = args,
                    .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .called_via = call.called_via,
                },
            };
        },

        .e_lambda => |lambda| blk: {
            const params = try self.lowerPatternSpan(module_env, lambda.args);
            const body = try self.lowerExprFromIdx(module_env, lambda.body);
            const ret_layout = self.getExprLayoutFromIdx(module_env, lambda.body);
            const fn_layout = self.getExprLayoutFromIdx(module_env, expr_idx);

            break :blk .{
                .lambda = .{
                    .fn_layout = fn_layout,
                    .params = params,
                    .body = body,
                    .ret_layout = ret_layout,
                },
            };
        },

        .e_closure => |closure| blk: {
            const lambda_id = try self.lowerExprFromIdx(module_env, closure.lambda_idx);
            const captures = try self.lowerCaptures(module_env, closure.captures);

            // Determine if this closure is bound to a variable.
            // If current_binding_pattern is set, we're in a let statement like `f = |x| ...`
            // which means the closure may have multiple call sites.
            // If not set, the closure is used directly (e.g., `map(|x| x + 1, list)`)
            // and is safe to inline at its single call site.
            const is_bound = self.current_binding_pattern != null;

            // Select the closure representation based on captures
            const representation = self.selectClosureRepresentation(captures);

            // Detect if this closure is recursive (references itself)
            const recursion_info = self.detectClosureRecursion(module_env, closure.lambda_idx);

            // If this is a recursive closure bound to a variable, create a MonoProc
            // so it can be compiled as a separate procedure with proper stack frame.
            if (recursion_info.self_recursive != .not_self_recursive and is_bound) {
                if (self.current_binding_symbol) |binding_symbol| {
                    const join_point_id = switch (recursion_info.self_recursive) {
                        .self_recursive => |id| id,
                        .not_self_recursive => unreachable,
                    };
                    const proc = try self.lowerClosureToProc(module_env, closure, binding_symbol, join_point_id);
                    _ = try self.store.addProc(proc);
                }
            }

            break :blk .{
                .closure = .{
                    .closure_layout = .i64, // TODO: compute from representation
                    .lambda = lambda_id,
                    .captures = captures,
                    .representation = representation,
                    .recursion = recursion_info.recursion,
                    .self_recursive = recursion_info.self_recursive,
                    .is_bound_to_variable = is_bound,
                },
            };
        },

        .e_list => |list| blk: {
            const elems = try self.lowerExprSpan(module_env, list.elems);

            // Compute element layout from the list's type
            const elem_layout = elem_blk: {
                const ls = self.layout_store orelse break :elem_blk LayoutIdx.default_num;

                // Get the list expression's type var and resolve it
                const list_type_var = ModuleEnv.varFrom(expr_idx);
                const resolved = module_env.types.resolveVar(list_type_var);

                // Check if it's a List (nominal type)
                switch (resolved.desc.content) {
                    .structure => |structure| {
                        switch (structure) {
                            .nominal_type => |nominal| {
                                // Get the element type (first type argument of List)
                                const args = module_env.types.sliceNominalArgs(nominal);
                                if (args.len > 0) {
                                    const elem_type_var = args[0];
                                    // Compute layout for the element type
                                    break :elem_blk ls.fromTypeVar(self.current_module_idx, elem_type_var, &self.type_scope, self.type_scope_caller_module) catch LayoutIdx.default_num;
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
                break :elem_blk LayoutIdx.default_num;
            };

            break :blk .{
                .list = .{
                    .elem_layout = elem_layout,
                    .elems = elems,
                },
            };
        },

        .e_tuple => |tuple| blk: {
            const elems = try self.lowerExprSpan(module_env, tuple.elems);
            const tuple_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
            break :blk .{
                .tuple = .{
                    .tuple_layout = tuple_layout,
                    .elems = elems,
                },
            };
        },

        .e_record => |rec| blk: {
            const result = try self.lowerRecordFields(module_env, rec.fields);
            const record_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
            break :blk .{
                .record = .{
                    .record_layout = record_layout,
                    .fields = result.fields,
                    .field_names = result.field_names,
                },
            };
        },

        .e_dot_access => |dot| blk: {
            const receiver = try self.lowerExprFromIdx(module_env, dot.receiver);
            const field_name = module_env.getIdent(dot.field_name);

            // Check if this is a list.len or list.is_empty access
            // Check based on layout, not expression type, so it works for list variables too
            const receiver_layout = self.getExprLayoutFromIdx(module_env, dot.receiver);
            const is_list_receiver = if (self.layout_store) |ls| is_list: {
                const layout_val = ls.getLayout(receiver_layout);
                break :is_list layout_val.tag == .list or layout_val.tag == .list_of_zst;
            } else false;

            // Handle method calls (dot.args != null)
            if (dot.args) |arg_span| {
                const arg_slice = module_env.store.sliceExpr(arg_span);

                if (is_list_receiver) {
                    // List method calls
                    if (std.mem.eql(u8, field_name, "append")) {
                        // list.append(elem) -> list_append(list, elem)
                        if (arg_slice.len >= 1) {
                            const elem = try self.lowerExprFromIdx(module_env, arg_slice[0]);
                            const ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                            const args = try self.store.addExprSpan(&[_]ir.MonoExprId{ receiver, elem });
                            break :blk .{
                                .low_level = .{
                                    .op = .list_append,
                                    .args = args,
                                    .ret_layout = ret_layout,
                                },
                            };
                        }
                    } else if (std.mem.eql(u8, field_name, "prepend")) {
                        // list.prepend(elem) -> list_prepend(list, elem)
                        if (arg_slice.len >= 1) {
                            const elem = try self.lowerExprFromIdx(module_env, arg_slice[0]);
                            const ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                            const args = try self.store.addExprSpan(&[_]ir.MonoExprId{ receiver, elem });
                            break :blk .{
                                .low_level = .{
                                    .op = .list_prepend,
                                    .args = args,
                                    .ret_layout = ret_layout,
                                },
                            };
                        }
                    } else if (std.mem.eql(u8, field_name, "get")) {
                        // list.get(idx) -> list_get(list, idx)
                        // The codegen handles bounds checking and Try wrapping when
                        // ret_layout is a tag union (Try(elem, [OutOfBounds])).
                        if (arg_slice.len >= 1) {
                            const idx = try self.lowerExprFromIdx(module_env, arg_slice[0]);
                            const ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                            const args = try self.store.addExprSpan(&[_]ir.MonoExprId{ receiver, idx });
                            break :blk .{
                                .low_level = .{
                                    .op = .list_get,
                                    .args = args,
                                    .ret_layout = ret_layout,
                                },
                            };
                        }
                    } else if (std.mem.eql(u8, field_name, "concat")) {
                        // list.concat(other) -> list_concat(list, other)
                        if (arg_slice.len >= 1) {
                            const other = try self.lowerExprFromIdx(module_env, arg_slice[0]);
                            const ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                            const args = try self.store.addExprSpan(&[_]ir.MonoExprId{ receiver, other });
                            break :blk .{
                                .low_level = .{
                                    .op = .list_concat,
                                    .args = args,
                                    .ret_layout = ret_layout,
                                },
                            };
                        }
                    }
                }
                // Fall through to handle as external call lookup if not a known method
            }

            // Handle method calls via nominal type dispatch (e.g., n.to_str(), list.fold(...))
            // For list receivers, only use nominal dispatch for methods NOT already handled
            // above (append/prepend/get/concat) or below (len/is_empty).
            const skip_nominal_for_list = is_list_receiver and (std.mem.eql(u8, field_name, "len") or
                std.mem.eql(u8, field_name, "is_empty") or
                std.mem.eql(u8, field_name, "isEmpty"));
            if (dot.args != null and !skip_nominal_for_list) {
                const receiver_type_var = ModuleEnv.varFrom(dot.receiver);
                var recv_resolved = module_env.types.resolveVar(receiver_type_var);
                var recv_type_source_env: *const ModuleEnv = module_env;

                // Check type_scope for concrete mapping (polymorphic calls)
                if (recv_resolved.desc.content == .flex or recv_resolved.desc.content == .rigid) {
                    if (self.type_scope.lookup(recv_resolved.var_)) |mapped| {
                        if (self.type_scope_caller_module) |caller_idx| {
                            recv_type_source_env = self.all_module_envs[caller_idx];
                        }
                        recv_resolved = recv_type_source_env.types.resolveVar(mapped);
                    }
                }

                // Follow aliases
                while (recv_resolved.desc.content == .alias) {
                    const al = recv_resolved.desc.content.alias;
                    const backing = recv_type_source_env.types.getAliasBackingVar(al);
                    recv_resolved = recv_type_source_env.types.resolveVar(backing);
                }

                // Determine nominal type
                const recv_nominal: ?struct { origin: Ident.Idx, ident: Ident.Idx } = switch (recv_resolved.desc.content) {
                    .structure => |s| switch (s) {
                        .nominal_type => |nom| .{
                            .origin = nom.origin_module,
                            .ident = nom.ident.ident_idx,
                        },
                        else => null,
                    },
                    .flex => |flex| fi: {
                        if (!flex.constraints.isEmpty()) {
                            const constraints = recv_type_source_env.types.sliceStaticDispatchConstraints(flex.constraints);
                            for (constraints) |constraint| {
                                if (constraint.origin == .from_numeral) {
                                    break :fi .{
                                        .origin = recv_type_source_env.idents.builtin_module,
                                        .ident = recv_type_source_env.idents.dec_type,
                                    };
                                }
                            }
                        }
                        break :fi null;
                    },
                    .rigid => |rigid| ri: {
                        if (!rigid.constraints.isEmpty()) {
                            for (recv_type_source_env.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                                if (constraint.origin == .from_numeral) {
                                    break :ri .{
                                        .origin = recv_type_source_env.idents.builtin_module,
                                        .ident = recv_type_source_env.idents.dec_type,
                                    };
                                }
                            }
                        }
                        break :ri null;
                    },
                    else => null,
                };

                if (recv_nominal) |rn| {
                    const origin_module_idx = self.findModuleForOrigin(recv_type_source_env, rn.origin) orelse {
                        unreachable;
                    };
                    const origin_env = self.all_module_envs[origin_module_idx];

                    const qualified_method = origin_env.lookupMethodIdentFromTwoEnvsConst(
                        recv_type_source_env,
                        rn.ident,
                        module_env,
                        dot.field_name,
                    ) orelse {
                        unreachable;
                    };

                    const node_idx = origin_env.getExposedNodeIndexById(qualified_method) orelse {
                        unreachable;
                    };

                    // Check if the method definition is a low-level lambda
                    const method_def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                    const method_def = origin_env.store.getDef(method_def_idx);
                    const method_expr = origin_env.store.getExpr(method_def.expr);

                    if (method_expr == .e_low_level_lambda) {
                        const ll = method_expr.e_low_level_lambda;
                        // Emit the low-level op directly as a MonoIR expression
                        const CIRLowLevel = CIR.Expr.LowLevel;
                        break :blk switch (ll.op) {
                            CIRLowLevel.u8_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .u8 } },
                            CIRLowLevel.i8_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .i8 } },
                            CIRLowLevel.u16_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .u16 } },
                            CIRLowLevel.i16_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .i16 } },
                            CIRLowLevel.u32_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .u32 } },
                            CIRLowLevel.i32_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .i32 } },
                            CIRLowLevel.u64_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .u64 } },
                            CIRLowLevel.i64_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .i64 } },
                            CIRLowLevel.u128_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .u128 } },
                            CIRLowLevel.i128_to_str => .{ .int_to_str = .{ .value = receiver, .int_precision = .i128 } },
                            CIRLowLevel.dec_to_str => .{ .dec_to_str = receiver },
                            CIRLowLevel.f32_to_str => .{ .float_to_str = .{ .value = receiver, .float_precision = .f32 } },
                            CIRLowLevel.f64_to_str => .{ .float_to_str = .{ .value = receiver, .float_precision = .f64 } },
                            else => low_level_dispatch: {
                                // Inline convertible low-level ops directly at the call site.
                                // This uses the call expression's type (resolved in the caller's module)
                                // for ret_layout, avoiding cross-module type scope mapping issues.
                                if (convertToMonoLowLevel(ll.op)) |mono_op| {
                                    const ll_extra_args = module_env.store.sliceExpr(dot.args.?);
                                    var ll_all_args = std.ArrayList(ir.MonoExprId).empty;
                                    defer ll_all_args.deinit(self.allocator);
                                    try ll_all_args.append(self.allocator, receiver);
                                    for (ll_extra_args) |arg_idx| {
                                        try ll_all_args.append(self.allocator, try self.lowerExprFromIdx(module_env, arg_idx));
                                    }
                                    const ll_args_span = try self.store.addExprSpan(ll_all_args.items);
                                    break :low_level_dispatch .{
                                        .low_level = .{
                                            .op = mono_op,
                                            .args = ll_args_span,
                                            .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                                        },
                                    };
                                }
                                // Non-convertible ops: fall back to general call
                                const method_symbol = MonoSymbol{
                                    .module_idx = origin_module_idx,
                                    .ident_idx = qualified_method,
                                };
                                const method_symbol_key: u48 = @bitCast(method_symbol);
                                if (!self.lowered_symbols.contains(method_symbol_key)) {
                                    try self.lowerExternalDefByIdx(method_symbol, node_idx);
                                }
                                const ll_ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                                const ll_fn_expr_id = try self.store.addExpr(.{ .lookup = .{
                                    .symbol = method_symbol,
                                    .layout_idx = .i64,
                                } }, region);
                                const ll_extra_args = module_env.store.sliceExpr(dot.args.?);
                                var ll_all_args = std.ArrayList(ir.MonoExprId).empty;
                                defer ll_all_args.deinit(self.allocator);
                                try ll_all_args.append(self.allocator, receiver);
                                for (ll_extra_args) |arg_idx| {
                                    const lowered_arg = try self.lowerExprFromIdx(module_env, arg_idx);
                                    try ll_all_args.append(self.allocator, lowered_arg);
                                }
                                const ll_args_span = try self.store.addExprSpan(ll_all_args.items);
                                break :low_level_dispatch .{
                                    .call = .{
                                        .fn_expr = ll_fn_expr_id,
                                        .fn_layout = LayoutIdx.named_fn,
                                        .args = ll_args_span,
                                        .ret_layout = ll_ret_layout,
                                        .called_via = .apply,
                                    },
                                };
                            },
                        };
                    }

                    // Regular function method - lower as external definition + call
                    const method_symbol = MonoSymbol{
                        .module_idx = origin_module_idx,
                        .ident_idx = qualified_method,
                    };

                    // Set up type scope for the method call so that the method's
                    // generic type parameters are mapped to concrete types from the
                    // call site (e.g., ok_or's `ok` type param → Str).
                    const old_caller_module = self.type_scope_caller_module;
                    {
                        const ext_type_var = ModuleEnv.varFrom(method_def.expr);
                        const ext_resolved = origin_env.types.resolveVar(ext_type_var);

                        if (ext_resolved.desc.content.unwrapFunc()) |func| {
                            if (self.type_scope.scopes.items.len == 0) {
                                try self.type_scope.scopes.append(types.VarMap.init(self.allocator));
                            }
                            const scope = &self.type_scope.scopes.items[0];

                            if (self.type_scope_caller_module == null) {
                                self.type_scope_caller_module = self.current_module_idx;
                            }

                            const param_vars = origin_env.types.sliceVars(func.args);

                            // Map first parameter (receiver) type
                            if (param_vars.len > 0) {
                                const receiver_var = ModuleEnv.varFrom(dot.receiver);
                                try self.collectTypeMappings(scope, origin_env, param_vars[0], module_env, receiver_var);
                            }

                            // Map extra argument types
                            if (dot.args) |extra_args_span| {
                                const extra_arg_indices = module_env.store.sliceExpr(extra_args_span);
                                for (extra_arg_indices, 0..) |arg_idx, i| {
                                    if (i + 1 < param_vars.len) {
                                        const arg_var = ModuleEnv.varFrom(arg_idx);
                                        try self.collectTypeMappings(scope, origin_env, param_vars[i + 1], module_env, arg_var);
                                    }
                                }
                            }

                            // Map return type
                            const caller_return_var = ModuleEnv.varFrom(expr_idx);
                            try self.collectTypeMappings(scope, origin_env, func.ret, module_env, caller_return_var);
                        }
                    }

                    const method_symbol_key: u48 = @bitCast(method_symbol);
                    if (!self.lowered_symbols.contains(method_symbol_key)) {
                        try self.lowerExternalDefByIdx(method_symbol, node_idx);
                    }

                    // Compute ret_layout and lower arguments BEFORE clearing the type
                    // scope. Both need the type scope active so that generic type
                    // parameters are properly resolved. Arguments like `List.append`
                    // (passed as a function value) need the type scope to produce
                    // correct layouts for their wrapper lambdas.
                    const ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx);

                    const fn_expr_id = try self.store.addExpr(.{ .lookup = .{
                        .symbol = method_symbol,
                        .layout_idx = .i64,
                    } }, region);

                    // Build args: receiver is first arg, then any extra args
                    const extra_args = module_env.store.sliceExpr(dot.args.?);
                    var all_args = std.ArrayList(ir.MonoExprId).empty;
                    defer all_args.deinit(self.allocator);
                    try all_args.append(self.allocator, receiver);
                    for (extra_args) |arg_idx| {
                        const lowered_arg = try self.lowerExprFromIdx(module_env, arg_idx);
                        try all_args.append(self.allocator, lowered_arg);
                    }
                    const args_span = try self.store.addExprSpan(all_args.items);

                    // Clean up type scope after lowering arguments
                    self.type_scope_caller_module = old_caller_module;
                    if (self.type_scope.scopes.items.len > 0) {
                        self.type_scope.scopes.items[0].clearRetainingCapacity();
                    }

                    break :blk .{
                        .call = .{
                            .fn_expr = fn_expr_id,
                            .fn_layout = LayoutIdx.named_fn,
                            .args = args_span,
                            .ret_layout = ret_layout,
                            .called_via = .apply,
                        },
                    };
                }
            }

            // Handle field access (no args) or zero-arg method calls for list properties
            const has_no_extra_args = dot.args == null or (if (dot.args) |as| module_env.store.sliceExpr(as).len == 0 else false);
            if (is_list_receiver and has_no_extra_args) {
                if (std.mem.eql(u8, field_name, "len")) {
                    // Convert list.len to low_level list_len operation
                    const args = try self.store.addExprSpan(&[_]ir.MonoExprId{receiver});
                    break :blk .{
                        .low_level = .{
                            .op = .list_len,
                            .args = args,
                            .ret_layout = .u64,
                        },
                    };
                } else if (std.mem.eql(u8, field_name, "is_empty") or std.mem.eql(u8, field_name, "isEmpty")) {
                    // Convert list.is_empty to low_level list_is_empty operation
                    const args = try self.store.addExprSpan(&[_]ir.MonoExprId{receiver});
                    break :blk .{
                        .low_level = .{
                            .op = .list_is_empty,
                            .args = args,
                            .ret_layout = .bool,
                        },
                    };
                }
            }

            // Try to compute field index and layout from the record layout
            // (receiver_layout was already computed above for list check)
            var field_idx: u16 = 0;
            var field_layout: LayoutIdx = LayoutIdx.default_num;
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(receiver_layout);
                if (layout_val.tag == .record) {
                    const record_data = ls.getRecordData(layout_val.data.record.idx);
                    const fields = ls.record_fields.sliceRange(record_data.getFields());
                    const field_name_text = module_env.getIdent(dot.field_name);
                    // Find field by name
                    var i: u16 = 0;
                    while (i < fields.len) : (i += 1) {
                        const field = fields.get(i);
                        if (std.mem.eql(u8, module_env.getIdent(field.name), field_name_text)) {
                            field_idx = i;
                            field_layout = field.layout;
                            break;
                        }
                    }
                }
            }
            break :blk .{
                .field_access = .{
                    .record_expr = receiver,
                    .record_layout = receiver_layout,
                    .field_layout = field_layout,
                    .field_idx = field_idx,
                    .field_name = dot.field_name,
                },
            };
        },

        .e_zero_argument_tag => |tag| blk: {
            // Resolve discriminant by looking up the tag's position in the
            // alphabetically-sorted variant list of the tag union type.
            const type_var = ModuleEnv.varFrom(expr_idx);
            const discriminant = self.resolveTagDiscriminant(module_env, type_var, tag.name);
            const tag_layout = self.getExprLayoutFromIdx(module_env, expr_idx);

            break :blk .{ .zero_arg_tag = .{
                .discriminant = discriminant,
                .union_layout = tag_layout,
            } };
        },

        .e_tag => |tag| blk: {
            const args = try self.lowerExprSpan(module_env, tag.args);
            const args_slice = module_env.store.sliceExpr(tag.args);

            // Resolve discriminant by looking up the tag's position in the
            // alphabetically-sorted variant list of the tag union type.
            const type_var = ModuleEnv.varFrom(expr_idx);
            const discriminant = self.resolveTagDiscriminant(module_env, type_var, tag.name);

            // For zero-argument tags, use zero_arg_tag
            if (args_slice.len == 0) {
                const tag_layout = self.getExprLayoutFromIdx(module_env, expr_idx);
                break :blk .{ .zero_arg_tag = .{
                    .discriminant = discriminant,
                    .union_layout = tag_layout,
                } };
            }

            break :blk .{
                .tag = .{
                    .discriminant = discriminant,
                    .union_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .args = args,
                },
            };
        },

        .e_if => |if_expr| blk: {
            // Check if this if-then-else returns closures (lambda set dispatch case)
            const lambda_set_result = try self.collectIfClosureLambdaSet(module_env, if_expr);

            // Get result layout from the type system via the expression's type variable
            const result_layout = self.getExprLayoutFromIdx(module_env, expr_idx);

            if (lambda_set_result.has_closure_branches) {
                // Branches are closures - lower with lambda set info
                const branches = try self.lowerIfBranchesWithLambdaSet(
                    module_env,
                    if_expr.branches,
                    lambda_set_result.lambda_set_members,
                );
                const final_else = try self.lowerExprWithLambdaSet(
                    module_env,
                    if_expr.final_else,
                    lambda_set_result.lambda_set_members,
                    lambda_set_result.else_tag,
                );
                break :blk .{ .if_then_else = .{
                    .branches = branches,
                    .final_else = final_else,
                    .result_layout = result_layout,
                } };
            } else {
                // Normal if-then-else (no closures)
                const branches = try self.lowerIfBranches(module_env, if_expr.branches);
                const final_else = try self.lowerExprFromIdx(module_env, if_expr.final_else);
                break :blk .{ .if_then_else = .{
                    .branches = branches,
                    .final_else = final_else,
                    .result_layout = result_layout,
                } };
            }
        },

        .e_match => |match_expr| blk: {
            const cond = try self.lowerExprFromIdx(module_env, match_expr.cond);
            const branches = try self.lowerWhenBranches(module_env, match_expr.branches);
            break :blk .{
                .when = .{
                    .value = cond,
                    .value_layout = self.getExprLayoutFromIdx(module_env, match_expr.cond),
                    .branches = branches,
                    .result_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                },
            };
        },

        .e_block => |block| blk: {
            // Process type annotations FIRST to populate type_env before lowering
            const result_layout = self.getBlockLayout(module_env, block);
            const stmts = try self.lowerStmts(module_env, block.stmts);
            const final_expr = try self.lowerExprFromIdx(module_env, block.final_expr);
            break :blk .{ .block = .{
                .stmts = stmts,
                .final_expr = final_expr,
                .result_layout = result_layout,
            } };
        },

        .e_return => |ret| blk: {
            const inner = try self.lowerExprFromIdx(module_env, ret.expr);
            break :blk .{ .early_return = .{
                .expr = inner,
                .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_binop => |binop| blk: {
            const lhs = try self.lowerExprFromIdx(module_env, binop.lhs);
            const rhs = try self.lowerExprFromIdx(module_env, binop.rhs);
            const op = cirBinopToMonoBinop(binop.op);
            break :blk .{ .binop = .{
                .op = op,
                .lhs = lhs,
                .rhs = rhs,
                .result_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_unary_minus => |unary| blk: {
            const inner = try self.lowerExprFromIdx(module_env, unary.expr);
            break :blk .{ .unary_minus = .{
                .expr = inner,
                .result_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
            } };
        },

        .e_unary_not => |unary| blk: {
            const inner = try self.lowerExprFromIdx(module_env, unary.expr);
            break :blk .{ .unary_not = .{ .expr = inner } };
        },

        .e_dbg => |dbg| blk: {
            const inner = try self.lowerExprFromIdx(module_env, dbg.expr);
            break :blk .{
                .dbg = .{
                    .msg = @enumFromInt(std.math.maxInt(u32)), // dbg doesn't have a msg in CIR
                    .expr = inner,
                    .result_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                },
            };
        },

        .e_expect => |expect| blk: {
            // e_expect only has a body (the condition expression)
            const body = try self.lowerExprFromIdx(module_env, expect.body);
            break :blk .{
                .expect = .{
                    .cond = body, // The body is the condition
                    .body = body, // Same expression for both
                    .result_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                },
            };
        },

        .e_crash => |crash| .{ .crash = .{ .msg = crash.msg } },
        .e_runtime_error => .{ .runtime_error = {} },

        .e_nominal => |nom| blk: {
            const backing = try self.lowerExprFromIdx(module_env, nom.backing_expr);
            break :blk .{
                .nominal = .{
                    .backing_expr = backing,
                    .nominal_layout = .i64, // TODO
                },
            };
        },

        .e_nominal_external => |nom| blk: {
            const backing = try self.lowerExprFromIdx(module_env, nom.backing_expr);
            break :blk .{
                .nominal = .{
                    .backing_expr = backing,
                    .nominal_layout = .i64, // TODO
                },
            };
        },

        // Expressions that need more context or aren't supported yet
        .e_typed_int => |ti| blk: {
            const val = ti.value.toI128();
            if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                break :blk .{ .i64_literal = @intCast(val) };
            }
            break :blk .{ .i128_literal = val };
        },

        .e_typed_frac => |tf| .{ .dec_literal = tf.value.toI128() },

        // Low-level lambda - these are compiler-generated intrinsics
        .e_low_level_lambda => |ll| blk: {
            // When a low-level lambda is evaluated directly (not called),
            // we need to produce a lambda that wraps the low-level call.
            // This happens when a low-level function is passed as a value
            // or stored as a definition that's looked up later.

            // Convert CIR LowLevel ops to MonoExpr LowLevel ops
            const mono_op = convertToMonoLowLevel(ll.op) orelse
                break :blk .{ .runtime_error = {} };

            // Get the function type from the expression to extract parameter types.
            // This is needed because the pattern type variables in builtins are generic (flex),
            // but the function type has the proper mappings through the type scope.
            const expr_type_var = ModuleEnv.varFrom(expr_idx);
            const expr_resolved = module_env.types.resolveVar(expr_type_var);
            const func_type = expr_resolved.desc.content.unwrapFunc();
            const ls = self.layout_store orelse unreachable;

            // CRITICAL: Set up type scope mappings BEFORE lowering patterns.
            // Low-level lambdas from builtins have their own rigid type variables that
            // may not be in the type scope (which was set up for the enclosing function).
            // Without these mappings, lowerPatternSpan -> getPatternLayout -> fromTypeVar
            // cannot resolve the rigid type variables and produces ZST layouts instead of
            // the correct concrete types (e.g., List(I64) → list layout instead of zst).
            if (func_type != null and self.type_scope.scopes.items.len > 0) {
                const ft = func_type.?;
                const scope = &self.type_scope.scopes.items[0];
                // Collect rigid vars from the lambda's function type (params + return)
                var lambda_rigids: [8]types.Var = undefined;
                var n_rigids: usize = 0;
                const ft_params = module_env.types.sliceVars(ft.args);
                for (ft_params) |pv| {
                    self.collectRigidVars(module_env, pv, &lambda_rigids, &n_rigids);
                }
                self.collectRigidVars(module_env, ft.ret, &lambda_rigids, &n_rigids);
                // For each unmapped rigid, find a scope entry with the same name
                for (lambda_rigids[0..n_rigids]) |rigid_var| {
                    if (scope.get(rigid_var) != null) continue; // already mapped
                    const rigid_resolved = module_env.types.resolveVar(rigid_var);
                    const rigid_name = if (rigid_resolved.desc.content == .rigid) rigid_resolved.desc.content.rigid.name else continue;
                    // Search existing scope entries for a rigid with the same name
                    const rigid_name_text = module_env.getIdent(rigid_name);
                    var it = scope.iterator();
                    while (it.next()) |entry| {
                        const ext_var = entry.key_ptr.*;
                        const ext_resolved = module_env.types.resolveVar(ext_var);
                        if (ext_resolved.desc.content == .rigid) {
                            const ext_name_text = module_env.getIdent(ext_resolved.desc.content.rigid.name);
                            if (std.mem.eql(u8, rigid_name_text, ext_name_text)) {
                                // Same name - add mapping from lambda's rigid to the same caller var
                                scope.put(rigid_var, entry.value_ptr.*) catch {};
                                // Invalidate layout cache for this var since it may have been
                                // cached as opaquePtr from container processing
                                const cache_key = layout_mod.ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = rigid_var };
                                _ = ls.layouts_by_module_var.remove(cache_key);
                                break;
                            }
                        }
                    }
                }
            }

            // Lower the parameter patterns (AFTER type scope setup so layouts resolve correctly)
            const params = try self.lowerPatternSpan(module_env, ll.args);

            // Create argument expressions from the parameter patterns
            // Each parameter becomes a lookup to itself
            const param_patterns = module_env.store.slicePatterns(ll.args);
            var arg_list = std.ArrayList(MonoExprId).empty;
            defer arg_list.deinit(self.allocator);

            var param_idx: usize = 0;
            for (param_patterns) |patt_idx| {
                const symbol = self.patternToSymbol(patt_idx);

                // Get the layout from the function's parameter type, not the pattern's type variable.
                // The pattern type in builtins is generic (flex), but the function type has the
                // concrete types through the type scope mappings.
                const patt_layout = if (func_type) |ft| layout_blk: {
                    const param_vars = module_env.types.sliceVars(ft.args);
                    if (param_idx < param_vars.len) {
                        const param_type_var = param_vars[param_idx];
                        break :layout_blk ls.fromTypeVar(self.current_module_idx, param_type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
                    }
                    unreachable; // Pattern count should match function parameter count
                } else {
                    unreachable; // e_low_level_lambda should always have a function type
                };
                param_idx += 1;

                const arg_id = try self.store.addExpr(.{
                    .lookup = .{
                        .symbol = symbol,
                        .layout_idx = patt_layout,
                    },
                }, region);
                try arg_list.append(self.allocator, arg_id);
            }
            const args_span = try self.store.addExprSpan(arg_list.items);

            // Use the function's RETURN type for ret_layout, not the function type itself
            const ret_layout = if (func_type) |ft| ret_blk: {
                break :ret_blk ls.fromTypeVar(self.current_module_idx, ft.ret, &self.type_scope, self.type_scope_caller_module) catch unreachable;
            } else self.getExprLayoutFromIdx(module_env, expr_idx);

            // Create the low-level call as the body
            const body_id = try self.store.addExpr(.{
                .low_level = .{
                    .op = mono_op,
                    .args = args_span,
                    .ret_layout = ret_layout,
                },
            }, region);

            break :blk .{
                .lambda = .{
                    .fn_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .params = params,
                    .body = body_id,
                    .ret_layout = ret_layout,
                },
            };
        },

        // Hosted lambda - these are platform-provided effects (I/O, etc.)
        // When a hosted lambda is evaluated directly (not called), we create a lambda
        // wrapper that will call the hosted function. This happens when a hosted
        // function is passed as a value.
        .e_hosted_lambda => |hosted| blk: {
            // Get the function type from the expression to extract parameter types
            const expr_type_var = ModuleEnv.varFrom(expr_idx);
            const expr_resolved = module_env.types.resolveVar(expr_type_var);
            const func_type = expr_resolved.desc.content.unwrapFunc();
            const ls = self.layout_store orelse unreachable;

            // Lower the parameter patterns (AFTER type scope setup so layouts resolve correctly)
            const params = try self.lowerPatternSpan(module_env, hosted.args);

            // Create argument expressions from the parameter patterns
            // Each parameter becomes a lookup to itself
            const param_patterns = module_env.store.slicePatterns(hosted.args);
            var arg_list = std.ArrayList(ir.MonoExprId).empty;
            defer arg_list.deinit(self.allocator);

            var param_idx: usize = 0;
            for (param_patterns) |patt_idx| {
                const symbol = self.patternToSymbol(patt_idx);

                // Get the layout from the function's parameter type, not the pattern's type variable.
                // The pattern type in hosted lambdas may be generic, but the function type has the
                // concrete types through the type scope mappings.
                const patt_layout = if (func_type) |ft| layout_blk: {
                    const param_vars = module_env.types.sliceVars(ft.args);
                    if (param_idx < param_vars.len) {
                        const param_type_var = param_vars[param_idx];
                        break :layout_blk ls.fromTypeVar(self.current_module_idx, param_type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
                    }
                    unreachable; // Pattern count should match function parameter count
                } else {
                    unreachable; // e_hosted_lambda should always have a function type
                };
                param_idx += 1;

                const arg_id = try self.store.addExpr(.{
                    .lookup = .{
                        .symbol = symbol,
                        .layout_idx = patt_layout,
                    },
                }, region);
                try arg_list.append(self.allocator, arg_id);
            }
            const args_span = try self.store.addExprSpan(arg_list.items);

            // Use the function's RETURN type for ret_layout
            const ret_layout = if (func_type) |ft| ret_blk: {
                break :ret_blk ls.fromTypeVar(self.current_module_idx, ft.ret, &self.type_scope, self.type_scope_caller_module) catch unreachable;
            } else self.getExprLayoutFromIdx(module_env, expr_idx);

            // Create the hosted call as the body
            const body_id = try self.store.addExpr(.{
                .hosted_call = .{
                    .index = hosted.index,
                    .args = args_span,
                    .ret_layout = ret_layout,
                },
            }, region);

            break :blk .{
                .lambda = .{
                    .fn_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .params = params,
                    .body = body_id,
                    .ret_layout = ret_layout,
                },
            };
        },

        .e_for => |for_expr| blk: {
            // Lower the list expression
            const list_expr = try self.lowerExprFromIdx(module_env, for_expr.expr);

            // Lower the element pattern
            const elem_pattern = try self.lowerPattern(module_env, for_expr.patt);

            // Push a type scope mapping from the element pattern's type variable
            // to the list's element type, so method dispatch (e.g., elem.to_str())
            // can resolve the receiver's type.
            const pushed_scope = self.pushForLoopElementTypeScope(module_env, for_expr.expr, for_expr.patt);

            // Lower the body expression
            const body = try self.lowerExprFromIdx(module_env, for_expr.body);

            // Pop the type scope if we pushed one
            if (pushed_scope) self.popForLoopElementTypeScope();

            // Get element layout from the list expression's type (not the pattern)
            const elem_layout = self.getForLoopElementLayout(for_expr.expr);

            break :blk .{
                .for_loop = .{
                    .list_expr = list_expr,
                    .elem_layout = elem_layout,
                    .elem_pattern = elem_pattern,
                    .body = body,
                },
            };
        },

        .e_type_var_dispatch => |tvd| blk: {
            // Type variable dispatch: Thing.method(args) where Thing is a type var alias.
            // Resolve the type variable to find the concrete nominal type, then look up the method.

            // Step 1: Get the type variable from the alias statement
            const stmt = module_env.store.getStatement(tvd.type_var_alias_stmt);
            const type_var_binding = stmt.s_type_var_alias;
            const type_var = ModuleEnv.varFrom(type_var_binding.type_var_anno);
            var resolved = module_env.types.resolveVar(type_var);

            // Step 2: If flex/rigid, check type_scope for concrete mapping (polymorphic calls)
            var type_source_env: *const ModuleEnv = module_env;
            if (resolved.desc.content == .flex or resolved.desc.content == .rigid) {
                if (self.type_scope.lookup(resolved.var_)) |mapped| {
                    if (self.type_scope_caller_module) |caller_idx| {
                        type_source_env = self.all_module_envs[caller_idx];
                    }
                    resolved = type_source_env.types.resolveVar(mapped);
                }
            }

            // Follow aliases to get to the underlying type
            while (resolved.desc.content == .alias) {
                const alias = resolved.desc.content.alias;
                const backing = type_source_env.types.getAliasBackingVar(alias);
                resolved = type_source_env.types.resolveVar(backing);
            }

            // Step 3: Determine the concrete nominal type
            const nominal_info: ?struct { origin: Ident.Idx, ident: Ident.Idx } = switch (resolved.desc.content) {
                .structure => |s| switch (s) {
                    .nominal_type => |nom| .{
                        .origin = nom.origin_module,
                        .ident = nom.ident.ident_idx,
                    },
                    else => null,
                },
                .flex => |flex| fi: {
                    if (!flex.constraints.isEmpty()) {
                        for (type_source_env.types.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                            if (constraint.origin == .from_numeral) {
                                break :fi .{
                                    .origin = type_source_env.idents.builtin_module,
                                    .ident = type_source_env.idents.dec_type,
                                };
                            }
                        }
                    }
                    break :fi null;
                },
                .rigid => |rigid| ri: {
                    if (!rigid.constraints.isEmpty()) {
                        for (type_source_env.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                            if (constraint.origin == .from_numeral) {
                                break :ri .{
                                    .origin = type_source_env.idents.builtin_module,
                                    .ident = type_source_env.idents.dec_type,
                                };
                            }
                        }
                    }
                    break :ri null;
                },
                else => null,
            };

            const info = nominal_info orelse {
                unreachable;
            };

            // Step 4: Find origin module index via imports
            const origin_module_idx = self.findModuleForOrigin(type_source_env, info.origin) orelse {
                unreachable;
            };
            const origin_env = self.all_module_envs[origin_module_idx];

            // Step 5: Look up method in origin module (cross-ident-store lookup)
            const qualified_method = origin_env.lookupMethodIdentFromTwoEnvsConst(
                type_source_env,
                info.ident,
                module_env,
                tvd.method_name,
            ) orelse {
                unreachable;
            };

            // Get the node index for the method definition
            const node_idx = origin_env.getExposedNodeIndexById(qualified_method) orelse {
                unreachable;
            };

            // Step 6: Lower as external definition + lookup/call
            const symbol = MonoSymbol{
                .module_idx = origin_module_idx,
                .ident_idx = qualified_method,
            };

            const symbol_key: u48 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key)) {
                try self.lowerExternalDefByIdx(symbol, node_idx);
            }

            const method_layout = self.getExprLayoutFromIdx(module_env, expr_idx);

            const fn_expr_id = try self.store.addExpr(.{ .lookup = .{
                .symbol = symbol,
                .layout_idx = .i64,
            } }, region);

            const arg_indices = module_env.store.sliceExpr(tvd.args);
            if (arg_indices.len == 0) {
                break :blk .{
                    .call = .{
                        .fn_expr = fn_expr_id,
                        .fn_layout = LayoutIdx.named_fn,
                        .args = MonoExprSpan.empty(),
                        .ret_layout = method_layout,
                        .called_via = .apply,
                    },
                };
            } else {
                const args = try self.lowerExprSpan(module_env, tvd.args);
                break :blk .{
                    .call = .{
                        .fn_expr = fn_expr_id,
                        .fn_layout = LayoutIdx.named_fn,
                        .args = args,
                        .ret_layout = method_layout,
                        .called_via = .apply,
                    },
                };
            }
        },

        else => {
            unreachable;
        },
    };

    return self.store.addExpr(mono_expr, region);
}

/// Lower an expression by its index in the current module
fn lowerExprFromIdx(self: *Self, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) Allocator.Error!MonoExprId {
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);
    return self.lowerExprInner(module_env, expr, region, expr_idx);
}

/// Lower a span of expressions
fn lowerExprSpan(self: *Self, module_env: *ModuleEnv, span: CIR.Expr.Span) Allocator.Error!MonoExprSpan {
    const expr_indices = module_env.store.sliceExpr(span);
    if (expr_indices.len == 0) return MonoExprSpan.empty();

    var lowered = std.ArrayList(MonoExprId).empty;
    defer lowered.deinit(self.allocator);

    for (expr_indices) |idx| {
        const id = try self.lowerExprFromIdx(module_env, idx);
        try lowered.append(self.allocator, id);
    }

    return self.store.addExprSpan(lowered.items);
}

/// Result of lowering record fields
const LowerRecordFieldsResult = struct {
    fields: MonoExprSpan,
    field_names: MonoFieldNameSpan,
};

/// Lower record fields - sorted alphabetically by field name to match layout order
fn lowerRecordFields(self: *Self, module_env: *ModuleEnv, fields: CIR.RecordField.Span) Allocator.Error!LowerRecordFieldsResult {
    const field_indices = module_env.store.sliceRecordFields(fields);
    if (field_indices.len == 0) return .{
        .fields = MonoExprSpan.empty(),
        .field_names = MonoFieldNameSpan.empty(),
    };

    // Collect (name, value, alignment) for sorting
    const FieldPair = struct {
        name: base.Ident.Idx,
        value: CIR.Expr.Idx,
        alignment: u32,
    };

    var pairs = std.ArrayList(FieldPair).empty;
    defer pairs.deinit(self.allocator);

    for (field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        // Get alignment from the field value's layout
        const field_layout_idx = self.getExprLayoutFromIdx(module_env, field.value);
        const alignment: u32 = if (self.layout_store) |ls| blk: {
            const field_layout = ls.getLayout(field_layout_idx);
            break :blk @intCast(ls.layoutSizeAlign(field_layout).alignment.toByteUnits());
        } else 1;
        try pairs.append(self.allocator, .{ .name = field.name, .value = field.value, .alignment = alignment });
    }

    // Sort fields by alignment descending, then alphabetically by name
    // This matches the layout store's field ordering
    const SortContext = struct {
        module_env: *ModuleEnv,
        pub fn lessThan(ctx: @This(), a: FieldPair, b: FieldPair) bool {
            // First sort by alignment descending (larger alignment first)
            if (a.alignment != b.alignment) {
                return a.alignment > b.alignment;
            }
            // Then sort alphabetically by name
            const a_str = ctx.module_env.getIdent(a.name);
            const b_str = ctx.module_env.getIdent(b.name);
            return std.mem.order(u8, a_str, b_str) == .lt;
        }
    };
    std.mem.sort(FieldPair, pairs.items, SortContext{ .module_env = module_env }, SortContext.lessThan);

    // Lower fields and collect names in sorted order
    var lowered = std.ArrayList(MonoExprId).empty;
    defer lowered.deinit(self.allocator);

    var names = std.ArrayList(base.Ident.Idx).empty;
    defer names.deinit(self.allocator);

    for (pairs.items) |pair| {
        const id = try self.lowerExprFromIdx(module_env, pair.value);
        try lowered.append(self.allocator, id);
        try names.append(self.allocator, pair.name);
    }

    return .{
        .fields = self.store.addExprSpan(lowered.items) catch return error.OutOfMemory,
        .field_names = self.store.addFieldNameSpan(names.items) catch return error.OutOfMemory,
    };
}

/// Lower a pattern
fn lowerPattern(self: *Self, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MonoPatternId {
    const pattern = module_env.store.getPattern(pattern_idx);
    const region = module_env.store.getPatternRegion(pattern_idx);

    const mono_pattern: MonoPattern = switch (pattern) {
        .assign => |_| .{
            .bind = .{
                .symbol = self.patternToSymbol(pattern_idx),
                .layout_idx = self.getPatternLayout(pattern_idx),
            },
        },

        .underscore => .{ .wildcard = .{ .layout_idx = self.getPatternLayout(pattern_idx) } },

        .num_literal => |n| .{ .int_literal = .{
            .value = n.value.toI128(),
            .layout_idx = .i64,
        } },

        .str_literal => |s| .{ .str_literal = s.literal },

        .applied_tag => |t| blk: {
            const args = try self.lowerPatternSpan(module_env, t.args);
            // Resolve discriminant by looking up the tag's position in the
            // alphabetically-sorted variant list of the tag union type.
            const type_var = ModuleEnv.varFrom(pattern_idx);
            const discriminant = self.resolveTagDiscriminant(module_env, type_var, t.name);
            const union_layout = self.getPatternLayout(pattern_idx);

            break :blk .{
                .tag = .{
                    .discriminant = discriminant,
                    .union_layout = union_layout,
                    .args = args,
                },
            };
        },

        .record_destructure => |r| blk: {
            // Record destructures contain RecordDestruct entries, not direct patterns
            // For now, we create binding patterns for each destruct entry
            const destruct_indices = module_env.store.sliceRecordDestructs(r.destructs);
            var field_patterns = std.ArrayList(MonoPatternId).empty;
            defer field_patterns.deinit(self.allocator);

            for (destruct_indices) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                // Create a binding pattern for this destruct
                // Both Required and SubPattern contain a pattern index
                const sub_pattern_idx = destruct.kind.toPatternIdx();
                const lowered_id = try self.lowerPattern(module_env, sub_pattern_idx);
                try field_patterns.append(self.allocator, lowered_id);
            }

            const fields = try self.store.addPatternSpan(field_patterns.items);
            // Compute the record layout from the pattern's type variable
            const record_layout = self.getPatternLayout(pattern_idx);
            break :blk .{
                .record = .{
                    .record_layout = record_layout,
                    .fields = fields,
                },
            };
        },

        .tuple => |t| blk: {
            const elems = try self.lowerPatternSpan(module_env, t.patterns);
            // Compute the tuple layout from the pattern's type variable
            const tuple_layout = self.getPatternLayout(pattern_idx);
            break :blk .{
                .tuple = .{
                    .tuple_layout = tuple_layout,
                    .elems = elems,
                },
            };
        },

        .as => |a| blk: {
            const inner = try self.lowerPattern(module_env, a.pattern);
            // Compute the layout from the pattern's type variable
            const layout_idx = self.getPatternLayout(pattern_idx);
            break :blk .{
                .as_pattern = .{
                    .symbol = self.patternToSymbol(pattern_idx),
                    .layout_idx = layout_idx,
                    .inner = inner,
                },
            };
        },

        .list => |l| blk: {
            // Lower prefix patterns
            const prefix = try self.lowerPatternSpan(module_env, l.patterns);
            // Lower rest pattern if present.
            // When rest_info exists but has no binding pattern (bare `..`),
            // emit a wildcard so the codegen knows this is a rest match
            // (length >= prefix_count) rather than an exact match (length == prefix_count).
            const rest_id = if (l.rest_info) |rest_info|
                if (rest_info.pattern) |rest_pattern|
                    try self.lowerPattern(module_env, rest_pattern)
                else
                    try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region)
            else
                MonoPatternId.none;

            // Compute element layout from the list pattern's type
            const elem_layout = elem_blk: {
                const ls = self.layout_store orelse unreachable; // Layout store must exist

                // Get the type variable from the pattern
                const pattern_type_var = ModuleEnv.varFrom(pattern_idx);
                const resolved = module_env.types.resolveVar(pattern_type_var);

                // Must be a List (nominal type)
                switch (resolved.desc.content) {
                    .structure => |structure| {
                        switch (structure) {
                            .nominal_type => |nominal| {
                                // Get the element type (first type argument of List)
                                const args = module_env.types.sliceNominalArgs(nominal);
                                std.debug.assert(args.len > 0); // List must have element type
                                const elem_type_var = args[0];
                                // Check if we have a layout hint for this element type
                                const elem_resolved = module_env.types.resolveVar(elem_type_var);

                                if (self.expr_layout_hints.get(elem_resolved.var_)) |hint_layout| {
                                    break :elem_blk hint_layout;
                                }
                                // Compute layout for the element type
                                break :elem_blk ls.fromTypeVar(self.current_module_idx, elem_type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
                            },
                            else => unreachable, // List pattern must match List type
                        }
                    },
                    else => unreachable, // List pattern must match structure type
                }
            };

            break :blk .{
                .list = .{
                    .elem_layout = elem_layout,
                    .prefix = prefix,
                    .rest = rest_id,
                },
            };
        },

        .nominal => |n| {
            // Unwrap nominal pattern to its backing pattern
            return self.lowerPattern(module_env, n.backing_pattern);
        },

        .nominal_external => |n| {
            // Unwrap nominal_external pattern to its backing pattern
            return self.lowerPattern(module_env, n.backing_pattern);
        },

        else => .{ .wildcard = .{ .layout_idx = self.getPatternLayout(pattern_idx) } }, // Fallback for unsupported patterns
    };

    return self.store.addPattern(mono_pattern, region);
}

/// Lower a span of patterns
fn lowerPatternSpan(self: *Self, module_env: *ModuleEnv, span: CIR.Pattern.Span) Allocator.Error!MonoPatternSpan {
    const indices = module_env.store.slicePatterns(span);
    if (indices.len == 0) return MonoPatternSpan.empty();

    var lowered = std.ArrayList(MonoPatternId).empty;
    defer lowered.deinit(self.allocator);

    for (indices) |idx| {
        const id = try self.lowerPattern(module_env, idx);
        try lowered.append(self.allocator, id);
    }

    return self.store.addPatternSpan(lowered.items);
}

/// Lower closure captures
fn lowerCaptures(self: *Self, module_env: *ModuleEnv, captures: CIR.Expr.Capture.Span) Allocator.Error!MonoCaptureSpan {
    const capture_indices = module_env.store.sliceCaptures(captures);
    if (capture_indices.len == 0) return MonoCaptureSpan.empty();

    var lowered = std.ArrayList(MonoCapture).empty;
    defer lowered.deinit(self.allocator);

    for (capture_indices) |capture_idx| {
        const cap = module_env.store.getCapture(capture_idx);
        const symbol = self.patternToSymbol(cap.pattern_idx);

        // Ensure the captured symbol's definition is lowered if it's a top-level def
        // This handles cases like closures capturing local functions
        const symbol_key: u48 = @bitCast(symbol);
        if (!self.lowered_symbols.contains(symbol_key)) {
            try self.lowerLocalDefByPattern(module_env, symbol, cap.pattern_idx);
        }

        // Compute layout from the capture's type variable
        const capture_layout_idx = blk: {
            const ls = self.layout_store orelse break :blk LayoutIdx.default_num;
            const type_var = ModuleEnv.varFrom(cap.pattern_idx);
            break :blk ls.fromTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch LayoutIdx.default_num;
        };

        try lowered.append(self.allocator, .{
            .symbol = symbol,
            .layout_idx = capture_layout_idx,
        });
    }

    return self.store.addCaptures(lowered.items);
}

/// Select the optimal closure representation based on captures.
/// This implements Roc-style representation selection:
/// - 0 captures: direct_call (no runtime representation needed)
/// - 1 capture: unwrapped_capture (zero overhead)
/// - N captures: struct_captures (sorted by alignment)
///
/// For lambda sets with multiple functions, we'd use enum_dispatch or union_repr,
/// but that requires lambda set inference results which we'll add later.
fn selectClosureRepresentation(self: *Self, captures: MonoCaptureSpan) ClosureRepresentation {
    const capture_list = self.store.getCaptures(captures);

    if (capture_list.len == 0) {
        // No captures - function can be called directly
        return .{ .direct_call = {} };
    } else if (capture_list.len == 1) {
        // Single capture - unwrapped, zero overhead
        return .{ .unwrapped_capture = .{
            .capture_layout = capture_list[0].layout_idx,
        } };
    } else {
        // Multiple captures - store in a struct
        // TODO: Sort captures by alignment (largest first) for memory efficiency
        // For now, just use the captures as-is
        return .{
            .struct_captures = .{
                .captures = captures,
                .struct_layout = .i64, // TODO: compute actual struct layout
            },
        };
    }
}

/// Result of recursion detection for a closure
const RecursionInfo = struct {
    recursion: Recursive,
    self_recursive: SelfRecursive,
};

/// Detect if a closure is recursive (references itself).
///
/// Uses the ClosureTransformer's detectRecursion functionality to check
/// if the closure body contains a reference to the binding pattern.
/// If recursive, generates a join point ID for the recursive entry.
fn detectClosureRecursion(self: *Self, module_env: *ModuleEnv, lambda_idx: CIR.Expr.Idx) RecursionInfo {
    // If we have a current binding pattern, check if the closure body references it
    if (self.current_binding_pattern) |binding_pattern| {
        // Get the lambda body to check for self-references
        const lambda_expr = module_env.store.getExpr(lambda_idx);
        if (lambda_expr == .e_lambda) {
            const body_expr = lambda_expr.e_lambda.body;

            // Check if the body contains a reference to the binding pattern
            const contains_ref = self.exprContainsPatternRef(module_env, body_expr, binding_pattern);
            if (contains_ref) {
                // Generate a unique join point ID for this recursive closure
                const join_point_id: JoinPointId = @enumFromInt(self.next_join_point_id);
                self.next_join_point_id += 1;

                // Check if this is tail-recursive
                // TODO: Proper tail-recursion detection (check if all recursive calls are in tail position)
                // For now, just mark as recursive
                return .{
                    .recursion = .recursive,
                    .self_recursive = .{ .self_recursive = join_point_id },
                };
            }
        }
    }

    // Not recursive
    return .{
        .recursion = .not_recursive,
        .self_recursive = .not_self_recursive,
    };
}

/// Check if an expression contains a reference to the given pattern.
/// Used for recursive closure detection.
fn exprContainsPatternRef(
    self: *Self,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    target_pattern: CIR.Pattern.Idx,
) bool {
    const expr = module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            return lookup.pattern_idx == target_pattern;
        },
        .e_call => |call| {
            // Check function expression
            if (self.exprContainsPatternRef(module_env, call.func, target_pattern)) {
                return true;
            }
            // Check arguments
            const args = module_env.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(module_env, arg_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_lambda => |lambda| {
            return self.exprContainsPatternRef(module_env, lambda.body, target_pattern);
        },
        .e_closure => |closure| {
            const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
            if (lambda_expr == .e_lambda) {
                return self.exprContainsPatternRef(module_env, lambda_expr.e_lambda.body, target_pattern);
            }
            return false;
        },
        .e_block => |block| {
            // Check statements
            const stmts = module_env.store.sliceStatements(block.stmts);
            for (stmts) |stmt_idx| {
                const stmt = module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        if (self.exprContainsPatternRef(module_env, decl.expr, target_pattern)) {
                            return true;
                        }
                    },
                    else => {},
                }
            }
            // Check final expression
            return self.exprContainsPatternRef(module_env, block.final_expr, target_pattern);
        },
        .e_if => |if_expr| {
            // Check all branches
            const branches = module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                if (self.exprContainsPatternRef(module_env, branch.cond, target_pattern) or
                    self.exprContainsPatternRef(module_env, branch.body, target_pattern))
                {
                    return true;
                }
            }
            return self.exprContainsPatternRef(module_env, if_expr.final_else, target_pattern);
        },
        .e_binop => |binop| {
            return self.exprContainsPatternRef(module_env, binop.lhs, target_pattern) or
                self.exprContainsPatternRef(module_env, binop.rhs, target_pattern);
        },
        .e_unary_minus => |unary| {
            return self.exprContainsPatternRef(module_env, unary.expr, target_pattern);
        },
        .e_unary_not => |unary| {
            return self.exprContainsPatternRef(module_env, unary.expr, target_pattern);
        },
        .e_type_var_dispatch => |tvd| {
            const args = module_env.store.sliceExpr(tvd.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(module_env, arg_idx, target_pattern)) return true;
            }
            return false;
        },
        // Leaf expressions that can't contain references
        else => return false,
    }
}

/// Lower if branches
fn lowerIfBranches(self: *Self, module_env: *ModuleEnv, branches: CIR.Expr.IfBranch.Span) Allocator.Error!ir.MonoIfBranchSpan {
    const branch_indices = module_env.store.sliceIfBranches(branches);

    var lowered = std.ArrayList(MonoIfBranch).empty;
    defer lowered.deinit(self.allocator);

    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getIfBranch(branch_idx);
        const cond = try self.lowerExprFromIdx(module_env, branch.cond);
        const body = try self.lowerExprFromIdx(module_env, branch.body);
        try lowered.append(self.allocator, .{
            .cond = cond,
            .body = body,
        });
    }

    return self.store.addIfBranches(lowered.items);
}

/// Result of collecting closures from if-then-else branches
const IfClosureLambdaSetResult = struct {
    /// Whether the branches contain closures
    has_closure_branches: bool,
    /// Lambda set members (all closures in the if-then-else)
    lambda_set_members: ir.LambdaSetMemberSpan,
    /// Tag for the else branch (last tag in sequence)
    else_tag: u16,
};

/// Collect all closures from if-then-else branches to create a lambda set.
/// This implements Roc's approach: closures in the same if-then-else share a lambda set.
fn collectIfClosureLambdaSet(
    self: *Self,
    module_env: *ModuleEnv,
    if_expr: anytype,
) Allocator.Error!IfClosureLambdaSetResult {
    var members = std.ArrayList(ir.LambdaSetMember).empty;
    defer members.deinit(self.allocator);

    var has_closures = false;
    var tag: u16 = 0;

    // Check each branch body
    const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getIfBranch(branch_idx);
        const body_expr = module_env.store.getExpr(branch.body);

        switch (body_expr) {
            .e_closure => |closure| {
                has_closures = true;
                // Lower the lambda body to get MonoExprId
                const lambda_id = try self.lowerExprFromIdx(module_env, closure.lambda_idx);
                const captures = try self.lowerCaptures(module_env, closure.captures);

                try members.append(self.allocator, .{
                    .lambda_symbol = self.createClosureSymbol(tag),
                    .captures = captures,
                    .lambda_body = lambda_id,
                    .tag = tag,
                });
                tag += 1;
            },
            .e_lambda => {
                has_closures = true;
                const lambda_id = try self.lowerExprFromIdx(module_env, branch.body);

                try members.append(self.allocator, .{
                    .lambda_symbol = self.createClosureSymbol(tag),
                    .captures = ir.MonoCaptureSpan.empty(),
                    .lambda_body = lambda_id,
                    .tag = tag,
                });
                tag += 1;
            },
            else => {
                // Not a closure - add placeholder
                tag += 1;
            },
        }
    }

    // Check final else
    const else_tag = tag;
    const else_expr = module_env.store.getExpr(if_expr.final_else);
    switch (else_expr) {
        .e_closure => |closure| {
            has_closures = true;
            const lambda_id = try self.lowerExprFromIdx(module_env, closure.lambda_idx);
            const captures = try self.lowerCaptures(module_env, closure.captures);

            try members.append(self.allocator, .{
                .lambda_symbol = self.createClosureSymbol(tag),
                .captures = captures,
                .lambda_body = lambda_id,
                .tag = tag,
            });
        },
        .e_lambda => {
            has_closures = true;
            const lambda_id = try self.lowerExprFromIdx(module_env, if_expr.final_else);

            try members.append(self.allocator, .{
                .lambda_symbol = self.createClosureSymbol(tag),
                .captures = ir.MonoCaptureSpan.empty(),
                .lambda_body = lambda_id,
                .tag = tag,
            });
        },
        else => {},
    }

    if (!has_closures) {
        return .{
            .has_closure_branches = false,
            .lambda_set_members = ir.LambdaSetMemberSpan.empty(),
            .else_tag = 0,
        };
    }

    // Store the lambda set members
    const member_span = try self.store.addLambdaSetMembers(members.items);

    return .{
        .has_closure_branches = true,
        .lambda_set_members = member_span,
        .else_tag = else_tag,
    };
}

/// Create a unique symbol for a closure in a lambda set
fn createClosureSymbol(self: *Self, tag: u16) ir.MonoSymbol {
    // Create a synthetic symbol for this closure
    // Using module index and a unique identifier based on tag
    return .{
        .module_idx = self.current_module_idx,
        .ident_idx = @bitCast(@as(u32, tag) | 0x80000000), // High bit marks as synthetic
    };
}

/// Lower if branches with lambda set info for closures
fn lowerIfBranchesWithLambdaSet(
    self: *Self,
    module_env: *ModuleEnv,
    branches: CIR.Expr.IfBranch.Span,
    lambda_set_members: ir.LambdaSetMemberSpan,
) Allocator.Error!ir.MonoIfBranchSpan {
    const branch_indices = module_env.store.sliceIfBranches(branches);

    var lowered = std.ArrayList(MonoIfBranch).empty;
    defer lowered.deinit(self.allocator);

    var tag: u16 = 0;
    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getIfBranch(branch_idx);
        const cond = try self.lowerExprFromIdx(module_env, branch.cond);

        // Lower body with lambda set info if it's a closure
        const body = try self.lowerExprWithLambdaSet(module_env, branch.body, lambda_set_members, tag);

        try lowered.append(self.allocator, .{
            .cond = cond,
            .body = body,
        });
        tag += 1;
    }

    return self.store.addIfBranches(lowered.items);
}

/// Lower an expression with lambda set info (for closures in if-then-else)
fn lowerExprWithLambdaSet(
    self: *Self,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    lambda_set_members: ir.LambdaSetMemberSpan,
    tag: u16,
) Allocator.Error!MonoExprId {
    const expr = module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure => |closure| {
            // Lower with lambda set representation
            const lambda_id = try self.lowerExprFromIdx(module_env, closure.lambda_idx);
            const captures = try self.lowerCaptures(module_env, closure.captures);

            // Determine representation based on lambda set
            const representation = self.selectLambdaSetRepresentation(lambda_set_members, captures, tag);

            // Detect recursion
            const recursion_info = self.detectClosureRecursion(module_env, closure.lambda_idx);

            // Check if bound to variable (same logic as regular closure lowering)
            const is_bound = self.current_binding_pattern != null;

            const mono_expr: MonoExpr = .{
                .closure = .{
                    .closure_layout = .i64, // TODO
                    .lambda = lambda_id,
                    .captures = captures,
                    .representation = representation,
                    .recursion = recursion_info.recursion,
                    .self_recursive = recursion_info.self_recursive,
                    .is_bound_to_variable = is_bound,
                },
            };

            return self.store.addExpr(mono_expr, Region.zero());
        },
        .e_lambda => |lambda| {
            // Lambda without captures - use enum_dispatch
            const params = try self.lowerPatternSpan(module_env, lambda.args);
            const body = try self.lowerExprFromIdx(module_env, lambda.body);

            const lambda_expr: MonoExpr = .{
                .lambda = .{
                    .fn_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .params = params,
                    .body = body,
                    .ret_layout = self.getExprLayoutFromIdx(module_env, lambda.body),
                },
            };
            const lambda_id = try self.store.addExpr(lambda_expr, Region.zero());

            // Wrap in closure with enum_dispatch representation
            // Check if bound to variable (same logic as regular closure lowering)
            const is_bound = self.current_binding_pattern != null;
            const num_members = self.store.getLambdaSetMembers(lambda_set_members).len;
            const closure_expr: MonoExpr = .{ .closure = .{
                .closure_layout = .i64,
                .lambda = lambda_id,
                .captures = ir.MonoCaptureSpan.empty(),
                .representation = .{ .enum_dispatch = .{
                    .num_functions = @intCast(num_members),
                    .tag = @intCast(tag),
                    .lambda_set = lambda_set_members,
                } },
                .recursion = .not_recursive,
                .self_recursive = .not_self_recursive,
                .is_bound_to_variable = is_bound,
            } };

            return self.store.addExpr(closure_expr, Region.zero());
        },
        else => {
            // Not a closure - lower normally
            return self.lowerExprFromIdx(module_env, expr_idx);
        },
    }
}

/// Select representation for a closure based on its lambda set
fn selectLambdaSetRepresentation(
    self: *Self,
    lambda_set_members: ir.LambdaSetMemberSpan,
    captures: ir.MonoCaptureSpan,
    tag: u16,
) ClosureRepresentation {
    const members = self.store.getLambdaSetMembers(lambda_set_members);
    const capture_list = self.store.getCaptures(captures);

    if (members.len <= 1) {
        // Single function - use normal representation
        if (capture_list.len == 0) {
            return .{ .direct_call = {} };
        } else if (capture_list.len == 1) {
            return .{ .unwrapped_capture = .{
                .capture_layout = capture_list[0].layout_idx,
            } };
        } else {
            return .{ .struct_captures = .{
                .captures = captures,
                .struct_layout = .i64,
            } };
        }
    }

    // Multiple functions in lambda set
    // Check if any function has captures
    var any_captures = false;
    for (members) |member| {
        const member_captures = self.store.getCaptures(member.captures);
        if (member_captures.len > 0) {
            any_captures = true;
            break;
        }
    }

    if (!any_captures) {
        // All functions have no captures - use enum_dispatch
        return .{ .enum_dispatch = .{
            .num_functions = @intCast(members.len),
            .tag = @intCast(tag),
            .lambda_set = lambda_set_members,
        } };
    } else {
        // Some functions have captures - use union_repr
        return .{
            .union_repr = .{
                .tag = tag,
                .captures = captures,
                .union_layout = .i64, // TODO: compute actual union layout
                .lambda_set = lambda_set_members,
            },
        };
    }
}

/// Lower when/match branches
fn lowerWhenBranches(self: *Self, module_env: *ModuleEnv, branches: CIR.Expr.Match.Branch.Span) Allocator.Error!ir.MonoWhenBranchSpan {
    const branch_indices = module_env.store.sliceMatchBranches(branches);

    var lowered = std.ArrayList(MonoWhenBranch).empty;
    defer lowered.deinit(self.allocator);

    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getMatchBranch(branch_idx);

        // Match branches can have multiple patterns (for or-patterns like `A | B => ...`)
        // For now, we lower just the first pattern
        const pattern_indices = module_env.store.sliceMatchBranchPatterns(branch.patterns);
        if (pattern_indices.len == 0) continue;

        const first_bp = module_env.store.getMatchBranchPattern(pattern_indices[0]);
        const pattern = try self.lowerPattern(module_env, first_bp.pattern);

        const guard = if (branch.guard) |guard_idx|
            try self.lowerExprFromIdx(module_env, guard_idx)
        else
            MonoExprId.none;
        const body = try self.lowerExprFromIdx(module_env, branch.value);

        try lowered.append(self.allocator, .{
            .pattern = pattern,
            .guard = guard,
            .body = body,
        });
    }

    return self.store.addWhenBranches(lowered.items);
}

/// Lower statements in a block
fn lowerStmts(self: *Self, module_env: *ModuleEnv, stmts: CIR.Statement.Span) Allocator.Error!ir.MonoStmtSpan {
    const stmt_slice = module_env.store.sliceStatements(stmts);
    if (stmt_slice.len == 0) return ir.MonoStmtSpan.empty();

    var lowered = std.ArrayList(MonoStmt).empty;
    defer lowered.deinit(self.allocator);

    for (stmt_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                // Check if this declaration binds a lambda/closure
                const decl_expr = module_env.store.getExpr(decl.expr);
                const is_lambda = decl_expr == .e_lambda or decl_expr == .e_closure;

                if (is_lambda) {
                    // Defer lowering — the body will be lowered at call time when
                    // type_scope is populated with concrete types from the call args.
                    const pattern_key = @intFromEnum(decl.pattern);
                    self.deferred_defs.put(pattern_key, decl.expr) catch {};
                } else {
                    // Non-lambda: lower eagerly as before
                    const pattern = try self.lowerPattern(module_env, decl.pattern);
                    const old_binding = self.current_binding_pattern;
                    const old_symbol = self.current_binding_symbol;
                    self.current_binding_pattern = decl.pattern;
                    const binding_symbol = self.patternToSymbol(decl.pattern);
                    self.current_binding_symbol = binding_symbol;
                    const value = try self.lowerExprFromIdx(module_env, decl.expr);
                    self.current_binding_pattern = old_binding;
                    self.current_binding_symbol = old_symbol;

                    // Register the symbol definition for lookups
                    try self.store.registerSymbolDef(binding_symbol, value);

                    try lowered.append(self.allocator, .{
                        .pattern = pattern,
                        .expr = value,
                    });
                }
            },
            .s_var => |var_stmt| {
                // Mutable variable declaration - treated like s_decl for lowering
                // The mutation semantics are handled by s_reassign creating new bindings
                const pattern = try self.lowerPattern(module_env, var_stmt.pattern_idx);
                const binding_symbol = self.patternToSymbol(var_stmt.pattern_idx);

                // Store the expression's layout in type_env for later lookups.
                // This ensures lookups to this mutable variable get the correct layout.
                const pattern_key = @intFromEnum(var_stmt.pattern_idx);
                const expr_layout = self.getExprLayoutFromIdx(module_env, var_stmt.expr);
                self.type_env.put(pattern_key, expr_layout) catch {};

                const value = try self.lowerExprFromIdx(module_env, var_stmt.expr);

                // Register the symbol definition for lookups
                try self.store.registerSymbolDef(binding_symbol, value);

                try lowered.append(self.allocator, .{
                    .pattern = pattern,
                    .expr = value,
                });
            },
            .s_reassign => |reassign| {
                // Reassignment - create a new binding that shadows the old one
                // This is functional semantics: each reassignment creates a new binding
                const pattern = try self.lowerPattern(module_env, reassign.pattern_idx);
                const binding_symbol = self.patternToSymbol(reassign.pattern_idx);
                const value = try self.lowerExprFromIdx(module_env, reassign.expr);

                // Update the symbol definition to point to the new value
                try self.store.registerSymbolDef(binding_symbol, value);

                try lowered.append(self.allocator, .{
                    .pattern = pattern,
                    .expr = value,
                });
            },
            .s_expr => |expr_stmt| {
                // Expression statement - evaluate for side effects
                // Create a wildcard pattern to discard the result
                const value = try self.lowerExprFromIdx(module_env, expr_stmt.expr);
                const expr_layout = self.getExprLayoutFromIdx(module_env, expr_stmt.expr);
                const wildcard_pattern = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = expr_layout } }, Region.zero());

                try lowered.append(self.allocator, .{
                    .pattern = wildcard_pattern,
                    .expr = value,
                });
            },
            .s_for => |for_stmt| {
                // For loop statement - lower to a for_loop expression
                const list_expr = try self.lowerExprFromIdx(module_env, for_stmt.expr);
                const elem_pattern = try self.lowerPattern(module_env, for_stmt.patt);

                // Push a type scope mapping for the element type (see e_for comment)
                const pushed_scope = self.pushForLoopElementTypeScope(module_env, for_stmt.expr, for_stmt.patt);

                const body = try self.lowerExprFromIdx(module_env, for_stmt.body);

                if (pushed_scope) self.popForLoopElementTypeScope();

                // Get element layout from the list expression's type (not the pattern)
                const elem_layout = self.getForLoopElementLayout(for_stmt.expr);

                const for_loop_expr = try self.store.addExpr(.{
                    .for_loop = .{
                        .list_expr = list_expr,
                        .elem_layout = elem_layout,
                        .elem_pattern = elem_pattern,
                        .body = body,
                    },
                }, Region.zero());

                // Create a wildcard pattern to discard the result (for loops return unit)
                const wildcard_pattern = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());

                try lowered.append(self.allocator, .{
                    .pattern = wildcard_pattern,
                    .expr = for_loop_expr,
                });
            },
            .s_while => |while_stmt| {
                // While loop statement - lower to a while_loop expression
                const cond = try self.lowerExprFromIdx(module_env, while_stmt.cond);
                const body = try self.lowerExprFromIdx(module_env, while_stmt.body);

                const while_loop_expr = try self.store.addExpr(.{
                    .while_loop = .{
                        .cond = cond,
                        .body = body,
                    },
                }, Region.zero());

                // Create a wildcard pattern to discard the result (while loops return unit)
                const wildcard_pattern = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());

                try lowered.append(self.allocator, .{
                    .pattern = wildcard_pattern,
                    .expr = while_loop_expr,
                });
            },
            else => {
                // Skip other statement types (s_import, s_alias_decl, etc.)
            },
        }
    }

    return self.store.addStmts(lowered.items);
}

/// Convert CIR binary operator to Mono IR binary operator
fn cirBinopToMonoBinop(op: CIR.Expr.Binop.Op) MonoExpr.BinOp {
    return switch (op) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .rem => .mod,
        .eq => .eq,
        .ne => .neq,
        .lt => .lt,
        .le => .lte,
        .gt => .gt,
        .ge => .gte,
        .@"and" => .@"and",
        .@"or" => .@"or",
        .div_trunc => .div_trunc,
    };
}

/// Lower str_inspekt(value) by expanding it into a tree of MonoExprs
/// that directly build the result string with all names embedded as literals.
///
/// This traverses the TYPE (for field/tag names) alongside the LAYOUT (for offsets)
/// to generate specialized inspection code at compile time.
pub fn lowerStrInspekt(
    self: *Self,
    value_expr: MonoExprId,
    type_var: types.Var,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Allocator.Error!MonoExprId {
    // Need layout store to resolve layouts
    const layout_store = self.layout_store orelse unreachable;

    // Resolve the type to get its structure
    const resolved = module_env.types.resolveVar(type_var);

    return switch (resolved.desc.content) {
        .structure => |flat_type| switch (flat_type) {
            .record => |record| try self.lowerInspectRecord(
                value_expr,
                record,
                value_layout,
                module_env,
                region,
            ),
            .tuple => |tuple| try self.lowerInspectTuple(
                value_expr,
                tuple,
                value_layout,
                module_env,
                region,
            ),
            .tag_union => |tu| try self.lowerInspectTagUnion(
                value_expr,
                tu,
                value_layout,
                module_env,
                region,
            ),
            .empty_record => try self.addStrLiteral("{}", region),
            .empty_tag_union => try self.addStrLiteral("[]", region),
            .fn_pure, .fn_effectful, .fn_unbound => try self.addStrLiteral("<function>", region),
            .nominal_type => |nom| try self.lowerInspectNominal(
                value_expr,
                nom,
                value_layout,
                module_env,
                region,
            ),
            .record_unbound => try self.addStrLiteral("{}", region),
        },
        .flex, .rigid => {
            // Unresolved type variable - get layout info to determine how to render
            const layout_val = layout_store.getLayout(value_layout);
            return try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region);
        },
        .alias => {
            // Alias - inspect the underlying type
            // For now, just treat as the layout indicates
            const layout_val = layout_store.getLayout(value_layout);
            return try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region);
        },
        .err => try self.addStrLiteral("<error>", region),
    };
}

/// Inspect based on layout when we don't have full type info
fn lowerInspectByLayout(
    self: *Self,
    value_expr: MonoExprId,
    layout_val: layout_mod.Layout,
    _: LayoutIdx,
    region: Region,
) Allocator.Error!MonoExprId {
    return switch (layout_val.tag) {
        .scalar => switch (layout_val.data.scalar.tag) {
            .int => try self.store.addExpr(.{ .int_to_str = .{
                .value = value_expr,
                .int_precision = layout_val.data.scalar.data.int,
            } }, region),
            .frac => blk: {
                if (layout_val.data.scalar.data.frac == .dec) {
                    break :blk try self.store.addExpr(.{ .dec_to_str = value_expr }, region);
                }
                break :blk try self.store.addExpr(.{ .float_to_str = .{
                    .value = value_expr,
                    .float_precision = layout_val.data.scalar.data.frac,
                } }, region);
            },
            .str => try self.store.addExpr(.{ .str_escape_and_quote = value_expr }, region),
            .opaque_ptr => try self.addStrLiteral("<opaque>", region),
        },
        .list, .list_of_zst => try self.addStrLiteral("[...]", region),
        else => try self.addStrLiteral("<value>", region),
    };
}

/// Inspect a boolean value
fn lowerInspectBool(self: *Self, value_expr: MonoExprId, region: Region) Allocator.Error!MonoExprId {
    // Create branches for True (discriminant 1) and False (discriminant 0)
    // Bool renders without the nominal prefix, matching interpreter behavior
    const false_str = try self.addStrLiteral("False", region);
    const true_str = try self.addStrLiteral("True", region);

    const branches = try self.store.addExprSpan(&[_]MonoExprId{ false_str, true_str });

    return self.store.addExpr(.{ .discriminant_switch = .{
        .value = value_expr,
        .union_layout = .bool,
        .branches = branches,
    } }, region);
}

/// Inspect a record: { field1: value1, field2: value2, ... }
fn lowerInspectRecord(
    self: *Self,
    value_expr: MonoExprId,
    record: types.Record,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Allocator.Error!MonoExprId {
    const layout_store = self.layout_store orelse unreachable;

    // Get field info from TYPE (for names)
    const fields_slice = module_env.types.getRecordFieldsSlice(record.fields);
    const field_names = fields_slice.items(.name);
    const field_vars = fields_slice.items(.var_);

    if (field_names.len == 0) {
        return try self.addStrLiteral("{}", region);
    }

    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    // Opening brace
    try parts.append(self.allocator, try self.addStrLiteral("{ ", region));

    // Get layout data for field offsets
    const layout_val = layout_store.getLayout(value_layout);

    for (field_names, field_vars, 0..) |field_name_idx, field_var, i| {
        if (i > 0) {
            try parts.append(self.allocator, try self.addStrLiteral(", ", region));
        }

        // Get field name string from ident store
        const field_name = module_env.getIdent(field_name_idx);

        // Build "fieldName: "
        const name_with_colon = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(name_with_colon);
        try parts.append(self.allocator, try self.addStrLiteral(name_with_colon, region));

        // Get field layout from record layout
        // For now, we create field access based on index
        const field_layout = self.getFieldLayoutFromRecord(layout_val, @intCast(i), layout_store);

        // Create field access expression
        const field_access_expr = try self.store.addExpr(.{ .field_access = .{
            .record_expr = value_expr,
            .record_layout = value_layout,
            .field_layout = field_layout,
            .field_idx = @intCast(i),
            .field_name = field_name_idx,
        } }, region);

        // Recursively inspect the field value
        const field_inspect = try self.lowerStrInspekt(
            field_access_expr,
            field_var,
            field_layout,
            module_env,
            region,
        );
        try parts.append(self.allocator, field_inspect);
    }

    // Closing brace
    try parts.append(self.allocator, try self.addStrLiteral(" }", region));

    // Concatenate all parts
    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Inspect a tuple: (elem0, elem1, ...)
fn lowerInspectTuple(
    self: *Self,
    value_expr: MonoExprId,
    tuple: types.Tuple,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Allocator.Error!MonoExprId {
    const layout_store = self.layout_store orelse unreachable;

    const elem_vars = module_env.types.sliceVars(tuple.elems);

    if (elem_vars.len == 0) {
        return try self.addStrLiteral("()", region);
    }

    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    // Opening paren
    try parts.append(self.allocator, try self.addStrLiteral("(", region));

    const layout_val = layout_store.getLayout(value_layout);

    for (elem_vars, 0..) |elem_var, i| {
        if (i > 0) {
            try parts.append(self.allocator, try self.addStrLiteral(", ", region));
        }

        // Get element layout
        const elem_layout = self.getTupleElemLayout(layout_val, @intCast(i), layout_store);

        // Create tuple access expression
        const tuple_access_expr = try self.store.addExpr(.{ .tuple_access = .{
            .tuple_expr = value_expr,
            .tuple_layout = value_layout,
            .elem_layout = elem_layout,
            .elem_idx = @intCast(i),
        } }, region);

        // Recursively inspect the element
        const elem_inspect = try self.lowerStrInspekt(
            tuple_access_expr,
            elem_var,
            elem_layout,
            module_env,
            region,
        );
        try parts.append(self.allocator, elem_inspect);
    }

    // Closing paren
    try parts.append(self.allocator, try self.addStrLiteral(")", region));

    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Inspect a tag union: Ok(value) or Err(msg) or None
fn lowerInspectTagUnion(
    self: *Self,
    value_expr: MonoExprId,
    tag_union: types.TagUnion,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Allocator.Error!MonoExprId {
    // Get tag info from TYPE
    const tags_slice = module_env.types.getTagsSlice(tag_union.tags);
    const tag_names = tags_slice.items(.name);
    const tag_args = tags_slice.items(.args);

    if (tag_names.len == 0) {
        return try self.addStrLiteral("[]", region);
    }

    // Build a branch for each tag variant
    var branches = std.ArrayList(MonoExprId).empty;
    defer branches.deinit(self.allocator);

    for (tag_names, tag_args) |tag_name_idx, args_range| {
        const tag_name = module_env.getIdent(tag_name_idx);
        const args_slice = module_env.types.sliceVars(args_range);

        if (args_slice.len == 0) {
            // No payload: just emit tag name
            try branches.append(self.allocator, try self.addStrLiteral(tag_name, region));
        } else {
            // Has payload: emit "TagName(payload)" or "TagName(p1, p2, ...)"
            var branch_parts = std.ArrayList(MonoExprId).empty;
            defer branch_parts.deinit(self.allocator);

            try branch_parts.append(self.allocator, try self.addStrLiteral(tag_name, region));
            try branch_parts.append(self.allocator, try self.addStrLiteral("(", region));

            // For multi-arg payloads, we'd need to access each argument
            // For now, just show a placeholder for payload
            for (args_slice, 0..) |_, arg_i| {
                if (arg_i > 0) {
                    try branch_parts.append(self.allocator, try self.addStrLiteral(", ", region));
                }
                // TODO: Access actual payload values using layout info
                try branch_parts.append(self.allocator, try self.addStrLiteral("_", region));
            }

            try branch_parts.append(self.allocator, try self.addStrLiteral(")", region));
            try branches.append(self.allocator, try self.store.addExpr(.{
                .str_concat = try self.store.addExprSpan(branch_parts.items),
            }, region));
        }
    }

    // Create discriminant switch
    return self.store.addExpr(.{ .discriminant_switch = .{
        .value = value_expr,
        .union_layout = value_layout,
        .branches = try self.store.addExprSpan(branches.items),
    } }, region);
}

/// Inspect a nominal type: TypeName.value
fn lowerInspectNominal(
    self: *Self,
    value_expr: MonoExprId,
    nom: types.NominalType,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Allocator.Error!MonoExprId {
    const type_name = module_env.getIdent(nom.ident.ident_idx);

    // Check for Bool type (special case: render as Bool.true/Bool.false)
    if (std.mem.eql(u8, type_name, "Bool")) {
        return try self.lowerInspectBool(value_expr, region);
    }

    // Check for numeric nominal types that should be rendered as their value.
    // These are opaque but have well-known numeric representations.
    const layout_store = self.layout_store orelse unreachable;
    const lv = layout_store.getLayout(value_layout);
    if (lv.tag == .scalar) {
        switch (lv.data.scalar.tag) {
            .int => return try self.store.addExpr(.{ .int_to_str = .{
                .value = value_expr,
                .int_precision = lv.data.scalar.data.int,
            } }, region),
            .frac => {
                if (lv.data.scalar.data.frac == .dec) {
                    return try self.store.addExpr(.{ .dec_to_str = value_expr }, region);
                }
                return try self.store.addExpr(.{ .float_to_str = .{
                    .value = value_expr,
                    .float_precision = lv.data.scalar.data.frac,
                } }, region);
            },
            .str => return try self.store.addExpr(.{ .str_escape_and_quote = value_expr }, region),
            .opaque_ptr => {},
        }
    }

    if (nom.is_opaque) {
        // Opaque types render as <opaque>
        return try self.addStrLiteral("<opaque>", region);
    }

    // For transparent nominal types, inspect the backing value
    // Get the first type variable which should be the backing type
    const vars = module_env.types.sliceVars(nom.vars.nonempty);
    if (vars.len > 0) {
        const backing_var = vars[0];
        return self.lowerStrInspekt(value_expr, backing_var, value_layout, module_env, region);
    }

    // Fallback: just show the type name with the value
    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    try parts.append(self.allocator, try self.addStrLiteral(type_name, region));
    try parts.append(self.allocator, try self.addStrLiteral(".", region));
    // For backing value, inspect by layout
    const layout_val = layout_store.getLayout(value_layout);
    try parts.append(self.allocator, try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region));

    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Helper to add a string literal expression
fn addStrLiteral(self: *Self, text: []const u8, region: Region) Allocator.Error!MonoExprId {
    // Add the string literal to the MonoExprStore's string table
    const lit_idx = self.store.insertString(text) catch return error.OutOfMemory;
    return self.store.addExpr(.{ .str_literal = lit_idx }, region);
}

/// Helper to get field layout from record layout
fn getFieldLayoutFromRecord(
    _: *Self,
    layout_val: layout_mod.Layout,
    field_idx: u16,
    layout_store: *LayoutStore,
) LayoutIdx {
    if (layout_val.tag == .record) {
        const record_data = layout_store.getRecordData(layout_val.data.record.idx);
        const fields_range = record_data.getFields();
        const fields = layout_store.record_fields.sliceRange(fields_range);
        if (field_idx < fields.len) {
            return fields.get(field_idx).layout;
        }
    }
    // Fallback to a default layout
    return .i64;
}

/// Helper to get tuple element layout
fn getTupleElemLayout(
    _: *Self,
    layout_val: layout_mod.Layout,
    elem_idx: u16,
    layout_store: *LayoutStore,
) LayoutIdx {
    if (layout_val.tag == .tuple) {
        const tuple_data = layout_store.getTupleData(layout_val.data.tuple.idx);
        const fields_range = tuple_data.getFields();
        const fields = layout_store.tuple_fields.sliceRange(fields_range);
        if (elem_idx < fields.len) {
            return fields.get(elem_idx).layout;
        }
    }
    // Fallback to a default layout
    return .i64;
}

/// Lower an expression to a control flow statement.
/// This is used for function bodies where we need explicit control flow.
///
/// The key insight: every expression either:
/// - Returns a value (becomes a `ret` statement)
/// - Has subexpressions that might contain returns (recursively lowered)
fn lowerExprToStmt(self: *Self, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, ret_layout: LayoutIdx) Allocator.Error!CFStmtId {
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);

    return switch (expr) {
        .e_block => |block| try self.lowerBlockToStmt(module_env, block, ret_layout),
        .e_if => |ite| try self.lowerIfToSwitchStmt(module_env, ite, ret_layout),
        else => {
            // For other expressions, wrap in a return statement
            const expr_id = try self.lowerExprInner(module_env, expr, region, expr_idx);
            return try self.store.addCFStmt(.{
                .ret = .{ .value = expr_id },
            });
        },
    };
}

/// Lower a block to a chain of let statements.
/// The final expression is lowered to a statement (which might be ret, switch, etc.)
fn lowerBlockToStmt(self: *Self, module_env: *ModuleEnv, block: anytype, ret_layout: LayoutIdx) Allocator.Error!CFStmtId {
    const stmt_slice = module_env.store.sliceStatements(block.stmts);

    // First, lower the final expression to a statement
    var current_stmt = try self.lowerExprToStmt(module_env, block.final_expr, ret_layout);

    // Then prepend each statement (in reverse order to build the chain)
    var i = stmt_slice.len;
    while (i > 0) {
        i -= 1;
        const stmt_idx = stmt_slice[i];
        const stmt = module_env.store.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                const pattern_id = try self.lowerPattern(module_env, decl.pattern);
                // Set binding pattern and symbol for recursive closure detection
                const old_binding = self.current_binding_pattern;
                const old_symbol = self.current_binding_symbol;
                self.current_binding_pattern = decl.pattern;
                const binding_symbol = self.patternToSymbol(decl.pattern);
                self.current_binding_symbol = binding_symbol;
                const value_id = try self.lowerExprFromIdx(module_env, decl.expr);
                self.current_binding_pattern = old_binding;
                self.current_binding_symbol = old_symbol;

                // Register the symbol definition for lookups
                try self.store.registerSymbolDef(binding_symbol, value_id);

                current_stmt = try self.store.addCFStmt(.{
                    .let_stmt = .{
                        .pattern = pattern_id,
                        .value = value_id,
                        .next = current_stmt,
                    },
                });
            },
            else => {}, // Skip other statement types for now
        }
    }

    return current_stmt;
}

/// Lower if-then-else to a switch statement.
/// This makes branch structure explicit for tail call analysis.
fn lowerIfToSwitchStmt(self: *Self, module_env: *ModuleEnv, ite: anytype, ret_layout: LayoutIdx) Allocator.Error!CFStmtId {
    const branch_indices = module_env.store.sliceIfBranches(ite.branches);

    // For a simple if-then-else with one condition:
    // if cond then thenBranch else elseBranch
    // becomes:
    // switch cond { 1 => thenStmt, default => elseStmt }

    if (branch_indices.len == 1) {
        const branch = module_env.store.getIfBranch(branch_indices[0]);

        // Lower condition
        const cond_id = try self.lowerExprFromIdx(module_env, branch.cond);

        // Lower branches as statements (so tail calls become jumps)
        const then_stmt = try self.lowerExprToStmt(module_env, branch.body, ret_layout);
        const else_stmt = try self.lowerExprToStmt(module_env, ite.final_else, ret_layout);

        // Create switch with true branch (value 1)
        const branches = try self.store.addCFSwitchBranches(&[_]CFSwitchBranch{
            .{ .value = 1, .body = then_stmt }, // true = 1
        });

        return try self.store.addCFStmt(.{
            .switch_stmt = .{
                .cond = cond_id,
                .cond_layout = .bool,
                .branches = branches,
                .default_branch = else_stmt, // false = default
                .ret_layout = ret_layout,
            },
        });
    }

    // For multiple conditions (elif chains), build nested switches
    // We process from last to first, building up the else chain
    var else_stmt = try self.lowerExprToStmt(module_env, ite.final_else, ret_layout);

    var i = branch_indices.len;
    while (i > 0) {
        i -= 1;
        const branch = module_env.store.getIfBranch(branch_indices[i]);

        const cond_id = try self.lowerExprFromIdx(module_env, branch.cond);
        const then_stmt = try self.lowerExprToStmt(module_env, branch.body, ret_layout);

        const branches = try self.store.addCFSwitchBranches(&[_]CFSwitchBranch{
            .{ .value = 1, .body = then_stmt },
        });

        else_stmt = try self.store.addCFStmt(.{
            .switch_stmt = .{
                .cond = cond_id,
                .cond_layout = .bool,
                .branches = branches,
                .default_branch = else_stmt,
                .ret_layout = ret_layout,
            },
        });
    }

    return else_stmt;
}

/// Lower a closure to a complete procedure (MonoProc).
/// This creates a procedure that can be compiled as a complete unit.
///
/// The procedure includes:
/// - Parameter patterns and their layouts
/// - Body lowered to control flow statements
/// - Tail recursion transformation if applicable
fn lowerClosureToProc(
    self: *Self,
    module_env: *ModuleEnv,
    closure: CIR.Expr.Closure,
    binding_symbol: MonoSymbol,
    join_point_id: JoinPointId,
) Allocator.Error!MonoProc {
    // Get the lambda from the closure
    const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    const lambda = switch (lambda_expr) {
        .e_lambda => |l| l,
        else => unreachable,
    };

    // Lower parameters first, then extract their layouts
    const params = try self.lowerPatternSpan(module_env, lambda.args);
    const param_layouts = try self.extractParamLayouts(params);
    const ret_layout = self.getExprLayoutFromIdx(module_env, lambda.body);

    // Lower body to statements
    var body_stmt = try self.lowerExprToStmt(module_env, lambda.body, ret_layout);

    // Apply tail recursion transformation if recursive (join_point_id is valid)
    const is_recursive = !join_point_id.isNone();
    if (is_recursive) {
        if (try TailRecursion.makeTailRecursive(
            self.store,
            binding_symbol,
            join_point_id,
            body_stmt,
            params,
            param_layouts,
            self.allocator,
        )) |transformed| {
            body_stmt = transformed;
        }
    }

    return MonoProc{
        .name = binding_symbol,
        .args = params,
        .arg_layouts = param_layouts,
        .body = body_stmt,
        .ret_layout = ret_layout,
        .closure_data_layout = null, // TODO: compute from captures
        .is_self_recursive = if (is_recursive)
            .{ .self_recursive = join_point_id }
        else
            .not_self_recursive,
    };
}

/// Extract layouts from already-lowered parameter patterns.
/// Returns an error if a pattern type doesn't carry layout information.
fn extractParamLayouts(self: *Self, params: MonoPatternSpan) Allocator.Error!LayoutIdxSpan {
    const pattern_ids = self.store.getPatternSpan(params);
    if (pattern_ids.len == 0) return LayoutIdxSpan.empty();

    var layouts = std.ArrayList(LayoutIdx).empty;
    defer layouts.deinit(self.allocator);

    for (pattern_ids) |pattern_id| {
        const pattern = self.store.getPattern(pattern_id);
        const layout_idx: LayoutIdx = switch (pattern) {
            .bind => |b| b.layout_idx,
            .int_literal => |i| i.layout_idx,
            .float_literal => |f| f.layout_idx,
            .record => |r| r.record_layout,
            .tuple => |t| t.tuple_layout,
            .tag => |t| t.union_layout,
            .as_pattern => |a| a.layout_idx,
            .list => |l| l.elem_layout,
            .str_literal => .str, // String literals have string layout
            .wildcard => |wc| wc.layout_idx,
        };
        try layouts.append(self.allocator, layout_idx);
    }

    return self.store.addLayoutIdxSpan(layouts.items);
}

/// Lower a single expression (for REPL/constant folding)
/// Returns the MonoExprId of the lowered expression
pub fn lowerExpression(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    module_idx: u16,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!MonoExprId {
    var lowerer = init(allocator, store, all_module_envs, null, null, null, null);
    defer lowerer.deinit();
    return lowerer.lowerExpr(module_idx, expr_idx);
}

/// Entry point specification
pub const EntryPoint = struct {
    module_idx: u16,
    expr_idx: CIR.Expr.Idx,
};

/// A lowered constant: maps a definition to its Mono IR expression.
/// This is used to track constants after lowering so they can be
/// evaluated at compile time.
pub const LoweredConstant = struct {
    /// The module this constant belongs to
    module_idx: u16,
    /// The original CIR definition index
    def_idx: CIR.Def.Idx,
    /// The Mono IR expression ID for this constant's value
    mono_expr_id: MonoExprId,
};

/// Result of lowering all constants in a module.
pub const LoweredConstants = struct {
    /// Array of lowered constants in dependency order
    constants: []LoweredConstant,
    allocator: Allocator,

    pub fn deinit(self: *LoweredConstants) void {
        self.allocator.free(self.constants);
    }
};

/// Lower all constants from the given SCCs and return the mapping.
///
/// This lowers each constant definition to Mono IR and tracks the mapping
/// from def index to MonoExprId, enabling later compile-time evaluation.
///
/// Parameters:
/// - lowerer: The initialized lowerer to use
/// - module_idx: The module these constants belong to
/// - sccs: The SCCs containing constant definitions in dependency order
/// - allocator: Allocator for the result
///
/// Returns a LoweredConstants struct containing the mapping from each
/// constant's def_idx to its mono_expr_id.
pub fn lowerConstants(
    lowerer: *Self,
    module_idx: u16,
    sccs: []const can.DependencyGraph.SCC,
    allocator: Allocator,
) Allocator.Error!LoweredConstants {
    // Count total constants across all SCCs
    var total_constants: usize = 0;
    for (sccs) |scc| {
        total_constants += scc.defs.len;
    }

    if (total_constants == 0) {
        return LoweredConstants{
            .constants = &[_]LoweredConstant{},
            .allocator = allocator,
        };
    }

    var constants = allocator.alloc(LoweredConstant, total_constants) catch return error.OutOfMemory;
    errdefer allocator.free(constants);

    const module_env = lowerer.getModuleEnv(module_idx) orelse unreachable;

    var i: usize = 0;
    for (sccs) |scc| {
        for (scc.defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const mono_expr_id = try lowerer.lowerExpr(module_idx, def.expr);

            constants[i] = .{
                .module_idx = module_idx,
                .def_idx = def_idx,
                .mono_expr_id = mono_expr_id,
            };
            i += 1;
        }
    }

    return LoweredConstants{
        .constants = constants,
        .allocator = allocator,
    };
}

/// Lower all reachable expressions from entry points (for roc build)
pub fn lowerFromEntryPoints(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    lambda_inference: ?*LambdaSetInference,
    layout_store: ?*LayoutStore,
    entry_points: []const EntryPoint,
) Allocator.Error!void {
    var lowerer = init(allocator, store, all_module_envs, lambda_inference, layout_store, null, null);
    defer lowerer.deinit();

    for (entry_points) |entry| {
        _ = try lowerer.lowerExpr(entry.module_idx, entry.expr_idx);
    }
}

test "basic lowering" {
    // This is a smoke test - proper testing requires a full ModuleEnv
    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Test passes if no crash
    try std.testing.expect(store.exprCount() == 0);
}
