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

const Self = @This();

/// Lowering context
allocator: Allocator,

/// The target store we're lowering into
store: *MonoExprStore,

/// All module environments (for resolving cross-module references)
all_module_envs: []const *ModuleEnv,

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
/// concrete types. This tracks the caller's module so addTypeVar can resolve
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

/// Initialize a new Lowerer
pub fn init(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    lambda_inference: ?*LambdaSetInference,
    layout_store: ?*LayoutStore,
) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .lambda_inference = lambda_inference,
        .layout_store = layout_store,
        .type_scope = TypeScope.init(allocator),
        .lowered_patterns = std.AutoHashMap(u64, MonoSymbol).init(allocator),
        .lowered_symbols = std.AutoHashMap(u48, MonoExprId).init(allocator),
        .type_env = std.AutoHashMap(u32, LayoutIdx).init(allocator),
        .expr_layout_hints = std.AutoHashMap(types.Var, LayoutIdx).init(allocator),
    };
}

/// Cleanup
pub fn deinit(self: *Self) void {
    self.lowered_patterns.deinit();
    self.lowered_symbols.deinit();
    self.type_env.deinit();
    self.type_scope.deinit();
    self.expr_layout_hints.deinit();
}

/// Get the module environment at the given index
fn getModuleEnv(self: *const Self, module_idx: u16) ?*ModuleEnv {
    if (module_idx >= self.all_module_envs.len) return null;
    return self.all_module_envs[module_idx];
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

    const expr_id = self.lowerExprFromIdx(ext_module_env, def.expr) catch {
        // If lowering fails, don't register - the lookup will fail at codegen time
        return;
    };

    // Register the symbol definition
    try self.store.registerSymbolDef(symbol, expr_id);
    try self.lowered_symbols.put(symbol_key, expr_id);
}

/// Compute layout from an expression index using the global layout store
fn computeLayoutFromExprIdx(self: *Self, _: *ModuleEnv, expr_idx: CIR.Expr.Idx) ?LayoutIdx {
    const ls = self.layout_store orelse return null;
    const type_var = ModuleEnv.varFrom(expr_idx);
    return ls.addTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch null;
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
    return ls.addTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
}

/// Get the element layout from the for-loop pattern's type variable
fn getForLoopElementLayout(self: *Self, pattern_idx: CIR.Pattern.Idx) LayoutIdx {
    return self.getPatternLayout(pattern_idx);
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
    return ls.addTypeVar(self.current_module_idx, type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
}

/// Set up type scope mappings for an external function call.
/// This maps the external function's rigid type variables to concrete types from the call site.
fn setupExternalCallTypeScope(
    self: *Self,
    caller_module_env: *ModuleEnv,
    lookup: anytype,
    args: CIR.Expr.Span,
) Allocator.Error!void {
    // Get the external module
    const ext_module_idx = caller_module_env.imports.getResolvedModule(lookup.module_idx) orelse return;
    if (ext_module_idx >= self.all_module_envs.len) return;
    const ext_module_env = self.all_module_envs[ext_module_idx];

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

    // Track the caller module - mapped vars in type_scope belong to this module
    self.type_scope_caller_module = self.current_module_idx;

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
}

/// Recursively collect type variable mappings by walking two types in parallel.
/// Maps rigid vars in ext_type to corresponding parts of caller_type.
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
            try scope.put(ext_resolved.var_, caller_resolved.var_);
        },
        .flex => {
            // Flex vars might also need mapping if they're unresolved
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
            // Check if this is a Dec type (explicit or default via unbound)
            // Dec is the default numeric type, so num_unbound and int_unbound become Dec
            if (num.kind == .dec or num.kind == .num_unbound or num.kind == .int_unbound) {
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

        .e_call => |call| blk: {
            // Check if this is a call to a low-level lambda (like str_inspekt)
            const fn_expr = module_env.store.getExpr(call.func);

            // If calling an external function, set up type scope mappings before lowering.
            // We need to clean up after the call is done to avoid polluting subsequent calls.
            const is_external_call = fn_expr == .e_lookup_external;
            const old_caller_module = self.type_scope_caller_module;
            if (is_external_call) {
                const lookup = fn_expr.e_lookup_external;
                try self.setupExternalCallTypeScope(module_env, lookup, call.args);
            }
            // Clean up type scope after this expression, whether we exit normally or break early
            defer if (is_external_call) {
                self.type_scope_caller_module = old_caller_module;
                // Clear the type_scope mappings to avoid polluting subsequent calls
                if (self.type_scope.scopes.items.len > 0) {
                    self.type_scope.scopes.items[0].clearRetainingCapacity();
                }
            };

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
                // Convert CIR LowLevel ops to MonoExpr LowLevel ops
                const mono_op: ir.MonoExpr.LowLevel = switch (ll.op) {
                    .list_len => .list_len,
                    .list_is_empty => .list_is_empty,
                    else => break :blk .{ .runtime_error = {} }, // Not yet implemented
                };
                const args = try self.lowerExprSpan(module_env, call.args);
                break :blk .{
                    .low_level = .{
                        .op = mono_op,
                        .args = args,
                        .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    },
                };
            }

            const fn_id = try self.lowerExprFromIdx(module_env, call.func);
            const args = try self.lowerExprSpan(module_env, call.args);
            break :blk .{
                .call = .{
                    .fn_expr = fn_id,
                    .fn_layout = .i64, // TODO: proper function layout
                    .args = args,
                    .ret_layout = self.getExprLayoutFromIdx(module_env, expr_idx),
                    .called_via = call.called_via,
                },
            };
        },

        .e_lambda => |lambda| blk: {
            const params = try self.lowerPatternSpan(module_env, lambda.args);
            const body = try self.lowerExprFromIdx(module_env, lambda.body);
            break :blk .{
                .lambda = .{
                    .fn_layout = .i64, // TODO
                    .params = params,
                    .body = body,
                    .ret_layout = .i64, // TODO
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
                                    break :elem_blk ls.addTypeVar(self.current_module_idx, elem_type_var, &self.type_scope, self.type_scope_caller_module) catch LayoutIdx.default_num;
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
            const tuple_layout = self.computeLayoutFromExprIdx(module_env, expr_idx) orelse LayoutIdx.default_num;
            break :blk .{
                .tuple = .{
                    .tuple_layout = tuple_layout,
                    .elems = elems,
                },
            };
        },

        .e_record => |rec| blk: {
            const result = try self.lowerRecordFields(module_env, rec.fields);
            const record_layout = self.computeLayoutFromExprIdx(module_env, expr_idx) orelse LayoutIdx.default_num;
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
            const receiver_expr = module_env.store.getExpr(dot.receiver);
            if (receiver_expr == .e_list) {
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

            // Compute receiver's layout to get field info
            const receiver_layout = self.computeLayoutFromExprIdx(module_env, dot.receiver) orelse LayoutIdx.default_num;

            // Try to compute field index and layout from the record layout
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
            // Get discriminant from tag name by comparing text
            // For Bool, True = 1, False = 0
            // We compare by text rather than ident index to handle cross-module cases
            const tag_name_text = module_env.getIdent(tag.name);
            // Handle both "True"/"False" (canonical form) and potential variations
            const discriminant: u16 = if (std.mem.eql(u8, tag_name_text, "True") or std.mem.eql(u8, tag_name_text, "true"))
                1
            else if (std.mem.eql(u8, tag_name_text, "False") or std.mem.eql(u8, tag_name_text, "false"))
                0
            else if (std.ascii.eqlIgnoreCase(tag_name_text, "true"))
                1
            else if (std.ascii.eqlIgnoreCase(tag_name_text, "false"))
                0
            else
                0; // TODO: proper discriminant lookup

            break :blk .{ .zero_arg_tag = .{
                .discriminant = discriminant,
                .union_layout = .bool,
            } };
        },

        .e_tag => |tag| blk: {
            const args = try self.lowerExprSpan(module_env, tag.args);
            const args_slice = module_env.store.sliceExpr(tag.args);

            // Get discriminant from tag name
            // For Result type: tags are sorted alphabetically, so Err=0, Ok=1
            // For Bool type: False=0, True=1
            const tag_name_text = module_env.getIdent(tag.name);
            const discriminant: u16 = if (std.mem.eql(u8, tag_name_text, "Ok") or std.ascii.eqlIgnoreCase(tag_name_text, "ok"))
                1 // Ok comes after Err alphabetically
            else if (std.mem.eql(u8, tag_name_text, "Err") or std.ascii.eqlIgnoreCase(tag_name_text, "err"))
                0 // Err comes first alphabetically
            else if (args_slice.len == 0) disc_blk: {
                // Zero-argument tag - check for True/False
                if (std.mem.eql(u8, tag_name_text, "True") or std.ascii.eqlIgnoreCase(tag_name_text, "true")) {
                    break :disc_blk 1;
                } else if (std.mem.eql(u8, tag_name_text, "False") or std.ascii.eqlIgnoreCase(tag_name_text, "false")) {
                    break :disc_blk 0;
                } else {
                    break :disc_blk 0; // TODO: proper discriminant lookup
                }
            } else 0; // TODO: proper discriminant lookup for other tags with args

            // For zero-argument tags like True/False, use zero_arg_tag
            if (args_slice.len == 0 and (std.mem.eql(u8, tag_name_text, "True") or
                std.mem.eql(u8, tag_name_text, "False") or
                std.ascii.eqlIgnoreCase(tag_name_text, "true") or
                std.ascii.eqlIgnoreCase(tag_name_text, "false")))
            {
                break :blk .{ .zero_arg_tag = .{
                    .discriminant = discriminant,
                    .union_layout = .bool,
                } };
            }

            break :blk .{
                .tag = .{
                    .discriminant = discriminant,
                    .union_layout = self.computeLayoutFromExprIdx(module_env, expr_idx) orelse LayoutIdx.default_num,
                    .args = args,
                },
            };
        },

        .e_if => |if_expr| blk: {
            // Check if this if-then-else returns closures (lambda set dispatch case)
            const lambda_set_result = try self.collectIfClosureLambdaSet(module_env, if_expr);

            // Get result layout from the type system via the expression's type variable
            const result_layout = self.computeLayoutFromExprIdx(module_env, expr_idx) orelse LayoutIdx.default_num;

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
                    .value_layout = .i64, // TODO
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
        .e_low_level_lambda => |_| blk: {
            // Low-level lambdas are typically called, not evaluated directly
            // When called, we intercept them in e_call handling
            // If we encounter one directly, it's like a closure reference
            break :blk .{ .runtime_error = {} };
        },

        // These are inserted by the RC insertion pass after canonicalization

        .e_incref => |rc_op| blk: {
            // Compute the layout from the pattern's type variable
            const patt_type_var = ModuleEnv.varFrom(rc_op.pattern_idx);
            const ls = self.layout_store orelse break :blk .{ .runtime_error = {} };
            const patt_layout = ls.addTypeVar(self.current_module_idx, patt_type_var, &self.type_scope, self.type_scope_caller_module) catch break :blk .{ .runtime_error = {} };

            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{
                .lookup = .{
                    .symbol = symbol,
                    .layout_idx = patt_layout,
                },
            }, region);
            break :blk .{
                .incref = .{
                    .value = lookup_id,
                    .layout_idx = patt_layout,
                    .count = rc_op.count,
                },
            };
        },

        .e_decref => |rc_op| blk: {
            // Compute the layout from the pattern's type variable
            const patt_type_var = ModuleEnv.varFrom(rc_op.pattern_idx);
            const ls = self.layout_store orelse break :blk .{ .runtime_error = {} };
            const patt_layout = ls.addTypeVar(self.current_module_idx, patt_type_var, &self.type_scope, self.type_scope_caller_module) catch break :blk .{ .runtime_error = {} };

            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{
                .lookup = .{
                    .symbol = symbol,
                    .layout_idx = patt_layout,
                },
            }, region);
            break :blk .{
                .decref = .{
                    .value = lookup_id,
                    .layout_idx = patt_layout,
                },
            };
        },

        .e_free => |rc_op| blk: {
            // Compute the layout from the pattern's type variable
            const patt_type_var = ModuleEnv.varFrom(rc_op.pattern_idx);
            const ls = self.layout_store orelse break :blk .{ .runtime_error = {} };
            const patt_layout = ls.addTypeVar(self.current_module_idx, patt_type_var, &self.type_scope, self.type_scope_caller_module) catch break :blk .{ .runtime_error = {} };

            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{
                .lookup = .{
                    .symbol = symbol,
                    .layout_idx = patt_layout,
                },
            }, region);
            break :blk .{
                .free = .{
                    .value = lookup_id,
                    .layout_idx = patt_layout,
                },
            };
        },

        .e_for => |for_expr| blk: {
            // Lower the list expression
            const list_expr = try self.lowerExprFromIdx(module_env, for_expr.expr);

            // Lower the element pattern
            const elem_pattern = try self.lowerPattern(module_env, for_expr.patt);

            // Lower the body expression
            const body = try self.lowerExprFromIdx(module_env, for_expr.body);

            // Get element layout from the pattern's type variable
            const elem_layout = self.getForLoopElementLayout(for_expr.patt);

            break :blk .{
                .for_loop = .{
                    .list_expr = list_expr,
                    .elem_layout = elem_layout,
                    .elem_pattern = elem_pattern,
                    .body = body,
                },
            };
        },

        else => .{ .runtime_error = {} }, // Placeholder for unsupported expressions
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

    // Collect (name, value) pairs for sorting
    const FieldPair = struct {
        name: base.Ident.Idx,
        value: CIR.Expr.Idx,
    };

    var pairs = std.ArrayList(FieldPair).empty;
    defer pairs.deinit(self.allocator);

    for (field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        try pairs.append(self.allocator, .{ .name = field.name, .value = field.value });
    }

    // Sort fields alphabetically by name (matches layout order for same-alignment fields)
    const SortContext = struct {
        module_env: *ModuleEnv,
        pub fn lessThan(ctx: @This(), a: FieldPair, b: FieldPair) bool {
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
                .layout_idx = .i64, // TODO: get from type
            },
        },

        .underscore => .{ .wildcard = {} },

        .num_literal => |n| .{ .int_literal = .{
            .value = n.value.toI128(),
            .layout_idx = .i64,
        } },

        .str_literal => |s| .{ .str_literal = s.literal },

        .applied_tag => |t| blk: {
            const args = try self.lowerPatternSpan(module_env, t.args);
            // Compute discriminant based on tag name
            // For Result type: tags are sorted alphabetically, so Err=0, Ok=1
            // For Bool type: False=0, True=1
            const tag_name_text = module_env.getIdent(t.name);
            const discriminant: u16 = if (std.mem.eql(u8, tag_name_text, "Ok") or
                std.ascii.eqlIgnoreCase(tag_name_text, "ok"))
                1 // Ok comes after Err alphabetically
            else if (std.mem.eql(u8, tag_name_text, "Err") or
                std.ascii.eqlIgnoreCase(tag_name_text, "err"))
                0 // Err comes first alphabetically
            else if (std.mem.eql(u8, tag_name_text, "True") or
                std.ascii.eqlIgnoreCase(tag_name_text, "true"))
                1
            else if (std.mem.eql(u8, tag_name_text, "False") or
                std.ascii.eqlIgnoreCase(tag_name_text, "false"))
                0
            else
                0; // TODO: proper discriminant lookup for other tags

            break :blk .{
                .tag = .{
                    .discriminant = discriminant,
                    .union_layout = .i64, // TODO: proper layout
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
            // Lower rest pattern if present
            const rest_id = if (l.rest_info) |rest_info|
                if (rest_info.pattern) |rest_pattern|
                    try self.lowerPattern(module_env, rest_pattern)
                else
                    MonoPatternId.none
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
                                break :elem_blk ls.addTypeVar(self.current_module_idx, elem_type_var, &self.type_scope, self.type_scope_caller_module) catch unreachable;
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

        else => .{ .wildcard = {} }, // Fallback for unsupported patterns
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
        try lowered.append(self.allocator, .{
            .symbol = symbol,
            .layout_idx = .i64, // TODO: get from type
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
            if (self.exprContainsPatternRef(module_env, body_expr, binding_pattern)) {
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
                    .fn_layout = .i64, // TODO
                    .params = params,
                    .body = body,
                    .ret_layout = .i64, // TODO
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
                const pattern = try self.lowerPattern(module_env, decl.pattern);
                // Set current binding pattern and symbol for recursive closure detection
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
                const wildcard_pattern = try self.store.addPattern(.{ .wildcard = {} }, Region.zero());

                try lowered.append(self.allocator, .{
                    .pattern = wildcard_pattern,
                    .expr = value,
                });
            },
            .s_for => |for_stmt| {
                // For loop statement - lower to a for_loop expression
                const list_expr = try self.lowerExprFromIdx(module_env, for_stmt.expr);
                const elem_pattern = try self.lowerPattern(module_env, for_stmt.patt);
                const body = try self.lowerExprFromIdx(module_env, for_stmt.body);

                // Get element layout from the pattern's type variable
                const elem_layout = self.getForLoopElementLayout(for_stmt.patt);

                const for_loop_expr = try self.store.addExpr(.{
                    .for_loop = .{
                        .list_expr = list_expr,
                        .elem_layout = elem_layout,
                        .elem_pattern = elem_pattern,
                        .body = body,
                    },
                }, Region.zero());

                // Create a wildcard pattern to discard the result (for loops return unit)
                const wildcard_pattern = try self.store.addPattern(.{ .wildcard = {} }, Region.zero());

                try lowered.append(self.allocator, .{
                    .pattern = wildcard_pattern,
                    .expr = for_loop_expr,
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
    const false_str = try self.addStrLiteral("Bool.false", region);
    const true_str = try self.addStrLiteral("Bool.true", region);

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
    const layout_store = self.layout_store orelse unreachable;
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
            // Wildcard patterns don't carry layout information in MonoPattern.
            // This is a design gap - wildcard should have a layout field.
            // For now, surface this as an error rather than silently defaulting.
            .wildcard => unreachable,
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
    var lowerer = init(allocator, store, all_module_envs, null, null);
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
    var lowerer = init(allocator, store, all_module_envs, lambda_inference, layout_store);
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
