//! Monomorphizer - Specializes polymorphic functions to concrete types
//!
//! This module implements the monomorphization pass which:
//! 1. Finds all polymorphic function definitions
//! 2. Identifies call sites with concrete types
//! 3. Creates specialized versions of functions for each concrete type
//!
//! Following the Cor approach, monomorphization happens AFTER type checking
//! but BEFORE code generation.
//!
//! ## Two-Phase Specialization
//!
//! Monomorphization uses a two-phase approach to handle recursive types:
//!
//! 1. **Finding Phase**: Walk the code, discover all needed specializations.
//!    Add them to `pending_specializations` but don't create bodies yet.
//!
//! 2. **Making Phase**: Process pending specializations, duplicate function
//!    bodies with type substitutions. This may discover more specializations,
//!    which are added to pending and processed in the same phase.
//!
//! This separation prevents infinite loops with recursive types by allowing
//! forward references that are patched after the recursive type is fully resolved.

const std = @import("std");
const base = @import("base");
const types = @import("types");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Expr = CIR.Expr;
const Pattern = @import("Pattern.zig").Pattern;

const Instantiator = types.instantiate.Instantiator;

const Self = @This();

/// Key for looking up specializations
pub const SpecializationKey = struct {
    original_ident: base.Ident.Idx,
    type_hash: u64,

    pub fn eql(a: SpecializationKey, b: SpecializationKey) bool {
        return a.original_ident == b.original_ident and a.type_hash == b.type_hash;
    }
};

/// A function that hasn't been specialized yet (partial proc).
/// Stores the original function definition that can be specialized to
/// different concrete types.
pub const PartialProc = struct {
    /// The original function identifier
    original_ident: base.Ident.Idx,
    /// The function's body expression index
    body_expr: Expr.Idx,
    /// The function's argument patterns
    arg_patterns: CIR.Pattern.Span,
    /// The function's polymorphic type variable
    type_var: types.Var,
    /// Whether this is a top-level definition
    is_top_level: bool,
};

/// A pending specialization that needs to be made.
/// Created during the finding phase, processed during the making phase.
pub const PendingSpecialization = struct {
    /// The original function identifier
    original_ident: base.Ident.Idx,
    /// The concrete type to specialize for
    concrete_type: types.Var,
    /// The call site expression (for error reporting)
    call_site: ?Expr.Idx,
    /// Type substitutions: maps polymorphic vars to concrete vars
    type_substitutions: types.VarMap,
};

/// A completed specialization.
/// The result of processing a PendingSpecialization.
pub const SpecializedProc = struct {
    /// The specialized function name
    specialized_ident: base.Ident.Idx,
    /// The specialized function body (duplicated with concrete types)
    body_expr: Expr.Idx,
    /// The specialized argument patterns
    arg_patterns: CIR.Pattern.Span,
    /// The concrete type for this specialization
    concrete_type: types.Var,
    /// The original function this was specialized from
    original_ident: base.Ident.Idx,
};

/// The phase of monomorphization we're in
pub const Phase = enum {
    /// Discovering what specializations are needed
    finding,
    /// Actually creating specialized functions
    making,
};

/// The allocator for intermediate allocations
allocator: std.mem.Allocator,

/// The module environment containing the CIR
module_env: *ModuleEnv,

/// The type store for looking up concrete types
types_store: *types.Store,

/// Map from original ident to partial proc (functions that can be specialized)
partial_procs: std.AutoHashMap(base.Ident.Idx, PartialProc),

/// Specializations that need to be made (queue processed in making phase)
pending_specializations: std.ArrayList(PendingSpecialization),

/// Completed specializations: (original, type_hash) -> specialized proc
specialized: std.AutoHashMap(SpecializationKey, SpecializedProc),

/// Map from (original_name, concrete_type_hash) -> specialized_name (for lookup)
specialization_names: std.AutoHashMap(SpecializationKey, base.Ident.Idx),

/// Specializations currently being made (to detect recursion)
in_progress: std.AutoHashMap(SpecializationKey, void),

/// Counter for generating unique specialization names
specialization_counter: u32,

/// Current phase
phase: Phase,

/// Current recursion depth for detecting polymorphic recursion
recursion_depth: u32,

/// Maximum recursion depth before using fallback (fuel-based cutoff)
max_recursion_depth: u32,

/// Stack of functions currently being specialized (for recursion detection)
specialization_stack: std.ArrayList(SpecializationKey),

/// Default maximum recursion depth for polymorphic recursion
pub const DEFAULT_MAX_RECURSION_DEPTH: u32 = 64;

/// Initialize the monomorphizer
pub fn init(
    allocator: std.mem.Allocator,
    module_env: *ModuleEnv,
    types_store: *types.Store,
) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .types_store = types_store,
        .partial_procs = std.AutoHashMap(base.Ident.Idx, PartialProc).init(allocator),
        .pending_specializations = std.ArrayList(PendingSpecialization).empty,
        .specialized = std.AutoHashMap(SpecializationKey, SpecializedProc).init(allocator),
        .specialization_names = std.AutoHashMap(SpecializationKey, base.Ident.Idx).init(allocator),
        .in_progress = std.AutoHashMap(SpecializationKey, void).init(allocator),
        .specialization_counter = 0,
        .phase = .finding,
        .recursion_depth = 0,
        .max_recursion_depth = DEFAULT_MAX_RECURSION_DEPTH,
        .specialization_stack = std.ArrayList(SpecializationKey).empty,
    };
}

/// Free resources
pub fn deinit(self: *Self) void {
    self.partial_procs.deinit();

    // Free type substitution maps in pending specializations
    for (self.pending_specializations.items) |*pending| {
        pending.type_substitutions.deinit();
    }
    self.pending_specializations.deinit(self.allocator);

    self.specialized.deinit();
    self.specialization_names.deinit();
    self.in_progress.deinit();
    self.specialization_stack.deinit(self.allocator);
}

/// Register a polymorphic function definition as a partial proc.
/// This should be called for each function definition during the finding phase.
pub fn registerPartialProc(
    self: *Self,
    ident: base.Ident.Idx,
    body_expr: Expr.Idx,
    arg_patterns: CIR.Pattern.Span,
    type_var: types.Var,
    is_top_level: bool,
) !void {
    const partial = PartialProc{
        .original_ident = ident,
        .body_expr = body_expr,
        .arg_patterns = arg_patterns,
        .type_var = type_var,
        .is_top_level = is_top_level,
    };
    try self.partial_procs.put(ident, partial);
}

/// Check if a function is registered as a partial proc (can be specialized)
pub fn isPartialProc(self: *const Self, ident: base.Ident.Idx) bool {
    return self.partial_procs.contains(ident);
}

/// Request a specialization for a function at a specific concrete type.
/// This is called when we find a call site that uses a polymorphic function
/// with concrete types.
///
/// In the finding phase, this adds to pending_specializations.
/// In the making phase, this may recursively create the specialization.
pub fn requestSpecialization(
    self: *Self,
    original_ident: base.Ident.Idx,
    concrete_type: types.Var,
    call_site: ?Expr.Idx,
) !?base.Ident.Idx {
    const type_hash = self.structuralTypeHash(concrete_type);
    const key = SpecializationKey{
        .original_ident = original_ident,
        .type_hash = type_hash,
    };

    // Check if we already have this specialization
    if (self.specialization_names.get(key)) |specialized_name| {
        return specialized_name;
    }

    // Check if it's already pending
    for (self.pending_specializations.items) |pending| {
        if (pending.original_ident == original_ident) {
            const pending_hash = self.structuralTypeHash(pending.concrete_type);
            if (pending_hash == type_hash) {
                // Already pending, return null (will be resolved later)
                return null;
            }
        }
    }

    // Check if this function is registered as a partial proc
    if (!self.partial_procs.contains(original_ident)) {
        // Not a polymorphic function we know about - might be external
        return null;
    }

    // Create type substitutions by unifying the polymorphic type with concrete
    const type_subs = types.VarMap.init(self.allocator);
    // For now, we'll populate this during body duplication

    // Add to pending
    try self.pending_specializations.append(self.allocator, PendingSpecialization{
        .original_ident = original_ident,
        .concrete_type = concrete_type,
        .call_site = call_site,
        .type_substitutions = type_subs,
    });

    return null;
}

/// Process all pending specializations.
/// This is the main entry point for the making phase.
pub fn processPendingSpecializations(self: *Self) !void {
    self.phase = .making;

    // Process until no more pending (may add more during processing)
    while (self.pending_specializations.items.len > 0) {
        // Pop from the end (LIFO order for better cache locality)
        var pending = self.pending_specializations.pop() orelse break;

        try self.makeSpecialization(&pending);

        // Clean up the type substitutions map
        pending.type_substitutions.deinit();
    }

    self.phase = .finding;
}

/// Create a single specialization from a pending request.
fn makeSpecialization(self: *Self, pending: *PendingSpecialization) !void {
    const type_hash = self.structuralTypeHash(pending.concrete_type);
    const key = SpecializationKey{
        .original_ident = pending.original_ident,
        .type_hash = type_hash,
    };

    // Check if already made (could have been created by a recursive call)
    if (self.specialized.contains(key)) {
        return;
    }

    // Check for infinite recursion
    if (self.in_progress.contains(key)) {
        // We're in the middle of making this specialization - this is a recursive call.
        // The forward reference will be resolved when we complete the outer specialization.
        return;
    }

    // Check if we've exceeded recursion depth (fuel-based cutoff for polymorphic recursion)
    if (self.recursion_depth >= self.max_recursion_depth) {
        // Too deep - this might be polymorphic recursion that would never terminate.
        // In the future, we could use a boxed/dynamic fallback here.
        // For now, we just stop specializing.
        return;
    }

    // Check for polymorphic recursion: same function with growing type structure
    if (self.detectPolymorphicRecursion(key)) {
        // Detected polymorphic recursion pattern - stop to prevent infinite loop
        return;
    }

    // Get the partial proc
    const partial = self.partial_procs.get(pending.original_ident) orelse return;

    // Mark as in progress and track depth
    try self.in_progress.put(key, {});
    try self.specialization_stack.append(self.allocator, key);
    self.recursion_depth += 1;

    defer {
        _ = self.in_progress.remove(key);
        _ = self.specialization_stack.pop();
        self.recursion_depth -= 1;
    }

    // Create the specialized name
    const specialized_ident = try self.createSpecializedName(
        pending.original_ident,
        pending.concrete_type,
    );

    // Register the name mapping first (for recursive references)
    try self.specialization_names.put(key, specialized_ident);

    // Duplicate the body with type substitutions
    const specialized_body = try self.duplicateBody(
        partial.body_expr,
        &pending.type_substitutions,
    );

    // Create the specialized proc
    const specialized = SpecializedProc{
        .specialized_ident = specialized_ident,
        .body_expr = specialized_body,
        .arg_patterns = partial.arg_patterns, // TODO: Duplicate patterns too
        .concrete_type = pending.concrete_type,
        .original_ident = pending.original_ident,
    };

    try self.specialized.put(key, specialized);
}

/// Detect polymorphic recursion patterns.
/// Returns true if we detect the same function being specialized with increasingly
/// complex types, which would indicate non-terminating monomorphization.
fn detectPolymorphicRecursion(self: *const Self, new_key: SpecializationKey) bool {
    // Look through the specialization stack for the same function
    var same_function_count: u32 = 0;
    for (self.specialization_stack.items) |stack_key| {
        if (stack_key.original_ident == new_key.original_ident) {
            same_function_count += 1;
            // If we see the same function more than 3 times on the stack,
            // and with different type hashes, it's likely polymorphic recursion
            if (same_function_count >= 3 and stack_key.type_hash != new_key.type_hash) {
                return true;
            }
        }
    }
    return false;
}

/// Build type substitutions by comparing polymorphic type with concrete type.
/// This populates the VarMap with mappings from polymorphic vars to concrete vars.
fn buildTypeSubstitutions(
    self: *Self,
    polymorphic_var: types.Var,
    concrete_var: types.Var,
    var_map: *types.VarMap,
) std.mem.Allocator.Error!void {
    const poly_resolved = self.types_store.resolveVar(polymorphic_var);
    const concrete_resolved = self.types_store.resolveVar(concrete_var);

    // If the polymorphic type is a flex or rigid, map it to the concrete type
    switch (poly_resolved.desc.content) {
        .flex, .rigid => {
            try var_map.put(poly_resolved.var_, concrete_resolved.var_);
            return;
        },
        else => {},
    }

    // Otherwise, recursively compare structure
    switch (poly_resolved.desc.content) {
        .structure => |poly_flat| {
            const concrete_content = concrete_resolved.desc.content;
            switch (concrete_content) {
                .structure => |concrete_flat| {
                    try self.buildFlatTypeSubstitutions(poly_flat, concrete_flat, var_map);
                },
                else => {},
            }
        },
        .alias => |poly_alias| {
            const concrete_content = concrete_resolved.desc.content;
            switch (concrete_content) {
                .alias => |concrete_alias| {
                    // Compare alias type arguments
                    var poly_iter = self.types_store.iterAliasArgs(poly_alias);
                    var concrete_iter = self.types_store.iterAliasArgs(concrete_alias);

                    while (poly_iter.next()) |poly_arg| {
                        if (concrete_iter.next()) |concrete_arg| {
                            try self.buildTypeSubstitutions(poly_arg, concrete_arg, var_map);
                        }
                    }

                    // Also compare backing vars
                    const poly_backing = self.types_store.getAliasBackingVar(poly_alias);
                    const concrete_backing = self.types_store.getAliasBackingVar(concrete_alias);
                    try self.buildTypeSubstitutions(poly_backing, concrete_backing, var_map);
                },
                else => {},
            }
        },
        else => {},
    }
}

/// Build substitutions for flat types
fn buildFlatTypeSubstitutions(
    self: *Self,
    poly_flat: types.FlatType,
    concrete_flat: types.FlatType,
    var_map: *types.VarMap,
) std.mem.Allocator.Error!void {
    switch (poly_flat) {
        .fn_pure, .fn_effectful, .fn_unbound => |poly_func| {
            const concrete_func = switch (concrete_flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |f| f,
                else => return,
            };

            // Compare arguments
            const poly_args = self.types_store.sliceVars(poly_func.args);
            const concrete_args = self.types_store.sliceVars(concrete_func.args);

            const min_len = @min(poly_args.len, concrete_args.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_args[i], concrete_args[i], var_map);
            }

            // Compare return types
            try self.buildTypeSubstitutions(poly_func.ret, concrete_func.ret, var_map);
        },
        .record => |poly_record| {
            const concrete_record = switch (concrete_flat) {
                .record => |r| r,
                else => return,
            };

            // Compare field types
            const poly_fields = self.types_store.getRecordFieldsSlice(poly_record.fields);
            const concrete_fields = self.types_store.getRecordFieldsSlice(concrete_record.fields);

            const min_len = @min(poly_fields.len, concrete_fields.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(
                    poly_fields.items(.var_)[i],
                    concrete_fields.items(.var_)[i],
                    var_map,
                );
            }
        },
        .tuple => |poly_tuple| {
            const concrete_tuple = switch (concrete_flat) {
                .tuple => |t| t,
                else => return,
            };

            const poly_elems = self.types_store.sliceVars(poly_tuple.elems);
            const concrete_elems = self.types_store.sliceVars(concrete_tuple.elems);

            const min_len = @min(poly_elems.len, concrete_elems.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_elems[i], concrete_elems[i], var_map);
            }
        },
        .tag_union => |poly_union| {
            const concrete_union = switch (concrete_flat) {
                .tag_union => |u| u,
                else => return,
            };

            // Compare extension types
            try self.buildTypeSubstitutions(poly_union.ext, concrete_union.ext, var_map);
        },
        .nominal_type => |poly_nom| {
            const concrete_nom = switch (concrete_flat) {
                .nominal_type => |n| n,
                else => return,
            };

            // Compare type arguments
            const poly_vars = self.types_store.sliceVars(poly_nom.vars.nonempty);
            const concrete_vars = self.types_store.sliceVars(concrete_nom.vars.nonempty);

            const min_len = @min(poly_vars.len, concrete_vars.len);
            for (0..min_len) |i| {
                try self.buildTypeSubstitutions(poly_vars[i], concrete_vars[i], var_map);
            }
        },
        .record_unbound, .empty_record, .empty_tag_union => {},
    }
}

/// Instantiate a type variable using the type substitutions.
/// Returns the concrete type for a polymorphic type.
pub fn instantiateType(
    self: *Self,
    type_var: types.Var,
    var_map: *std.AutoHashMap(types.Var, types.Var),
) std.mem.Allocator.Error!types.Var {
    var instantiator = Instantiator{
        .store = self.types_store,
        .idents = self.module_env.getIdentStoreConst(),
        .var_map = var_map,
        .current_rank = types.Rank.top_level,
        .rigid_behavior = .fresh_flex,
    };

    return try instantiator.instantiateVar(type_var);
}

/// Duplicate a function body, substituting polymorphic types with concrete ones.
/// Also discovers and queues additional specializations needed.
fn duplicateBody(
    self: *Self,
    original_body: Expr.Idx,
    type_substitutions: *const types.VarMap,
) !Expr.Idx {
    return try self.duplicateExpr(original_body, type_substitutions);
}

/// Recursively duplicate an expression tree with type substitutions.
fn duplicateExpr(
    self: *Self,
    expr_idx: Expr.Idx,
    type_subs: *const types.VarMap,
) std.mem.Allocator.Error!Expr.Idx {
    const expr = self.module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_call => |call| {
            // Check if this is a call to a polymorphic function we should specialize
            const func_expr = self.module_env.store.getExpr(call.func);
            switch (func_expr) {
                .e_lookup_local => |lookup| {
                    // Get the identifier for this pattern
                    const pattern = self.module_env.store.getPattern(lookup.pattern_idx);
                    switch (pattern) {
                        .assign => |assign| {
                            // Check if this is a call to a partial proc
                            if (self.partial_procs.contains(assign.ident)) {
                                // TODO: Determine concrete type at call site and request specialization
                                // For now, just duplicate the call as-is
                            }
                        },
                        else => {},
                    }
                },
                else => {},
            }

            // Duplicate the function expression
            const new_func = try self.duplicateExpr(call.func, type_subs);

            // Duplicate arguments
            const args = self.module_env.store.sliceExpr(call.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            return try self.module_env.store.addExpr(Expr{
                .e_call = .{
                    .func = new_func,
                    .args = new_args_span,
                    .called_via = call.called_via,
                },
            }, base.Region.zero());
        },

        .e_lambda => |lambda| {
            // Duplicate the lambda body
            const new_body = try self.duplicateExpr(lambda.body, type_subs);

            if (new_body == lambda.body) {
                return expr_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_lambda = .{
                    .args = lambda.args, // TODO: Duplicate patterns
                    .body = new_body,
                },
            }, base.Region.zero());
        },

        .e_closure => |closure| {
            // Duplicate the inner lambda
            const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
            switch (lambda_expr) {
                .e_lambda => |lambda| {
                    const new_body = try self.duplicateExpr(lambda.body, type_subs);

                    if (new_body == lambda.body) {
                        return expr_idx;
                    }

                    const new_lambda = try self.module_env.store.addExpr(Expr{
                        .e_lambda = .{
                            .args = lambda.args,
                            .body = new_body,
                        },
                    }, base.Region.zero());

                    return try self.module_env.store.addExpr(Expr{
                        .e_closure = .{
                            .lambda_idx = new_lambda,
                            .captures = closure.captures,
                            .tag_name = closure.tag_name,
                        },
                    }, base.Region.zero());
                },
                else => return expr_idx,
            }
        },

        .e_block => |block| {
            // Duplicate block statements and final expression
            const stmts = self.module_env.store.sliceStatements(block.stmts);
            const stmt_start = self.module_env.store.scratch.?.statements.top();

            for (stmts) |stmt_idx| {
                const stmt = self.module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        const new_expr = try self.duplicateExpr(decl.expr, type_subs);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    .s_decl_gen => |decl| {
                        const new_expr = try self.duplicateExpr(decl.expr, type_subs);
                        const new_stmt_idx = try self.module_env.store.addStatement(
                            CIR.Statement{ .s_decl_gen = .{
                                .pattern = decl.pattern,
                                .expr = new_expr,
                                .anno = decl.anno,
                            } },
                            base.Region.zero(),
                        );
                        try self.module_env.store.scratch.?.statements.append(new_stmt_idx);
                    },
                    else => {
                        try self.module_env.store.scratch.?.statements.append(stmt_idx);
                    },
                }
            }

            const new_stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);
            const new_final = try self.duplicateExpr(block.final_expr, type_subs);

            return try self.module_env.store.addExpr(Expr{
                .e_block = .{
                    .stmts = new_stmts_span,
                    .final_expr = new_final,
                },
            }, base.Region.zero());
        },

        .e_if => |if_expr| {
            const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
            const branch_start = self.module_env.store.scratch.?.if_branches.top();

            for (branches) |branch_idx| {
                const branch = self.module_env.store.getIfBranch(branch_idx);
                const new_cond = try self.duplicateExpr(branch.cond, type_subs);
                const new_body = try self.duplicateExpr(branch.body, type_subs);

                const new_branch_idx = try self.module_env.store.addIfBranch(
                    Expr.IfBranch{ .cond = new_cond, .body = new_body },
                    base.Region.zero(),
                );
                try self.module_env.store.scratch.?.if_branches.append(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.ifBranchSpanFrom(branch_start);
            const new_else = try self.duplicateExpr(if_expr.final_else, type_subs);

            return try self.module_env.store.addExpr(Expr{
                .e_if = .{
                    .branches = new_branches_span,
                    .final_else = new_else,
                },
            }, base.Region.zero());
        },

        .e_binop => |binop| {
            const new_lhs = try self.duplicateExpr(binop.lhs, type_subs);
            const new_rhs = try self.duplicateExpr(binop.rhs, type_subs);

            if (new_lhs == binop.lhs and new_rhs == binop.rhs) {
                return expr_idx;
            }

            return try self.module_env.store.addExpr(Expr{
                .e_binop = .{
                    .op = binop.op,
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                },
            }, base.Region.zero());
        },

        .e_list => |list| {
            const elems = self.module_env.store.sliceExpr(list.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.duplicateExpr(elem_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_list = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },

        .e_tuple => |tuple| {
            const elems = self.module_env.store.sliceExpr(tuple.elems);
            const elems_start = self.module_env.store.scratch.?.exprs.top();

            for (elems) |elem_idx| {
                const new_elem = try self.duplicateExpr(elem_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_elem);
            }

            const new_elems_span = try self.module_env.store.exprSpanFrom(elems_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tuple = .{ .elems = new_elems_span },
            }, base.Region.zero());
        },

        .e_record => |record| {
            const field_indices = self.module_env.store.sliceRecordFields(record.fields);
            const fields_start = self.module_env.store.scratch.?.record_fields.top();

            for (field_indices) |field_idx| {
                const field = self.module_env.store.getRecordField(field_idx);
                const new_value = try self.duplicateExpr(field.value, type_subs);

                const new_field = CIR.RecordField{
                    .name = field.name,
                    .value = new_value,
                };
                const new_field_idx = try self.module_env.store.addRecordField(new_field, base.Region.zero());
                try self.module_env.store.scratch.?.record_fields.append(new_field_idx);
            }

            const new_fields_span = try self.module_env.store.recordFieldSpanFrom(fields_start);
            const new_ext = if (record.ext) |ext| try self.duplicateExpr(ext, type_subs) else null;

            return try self.module_env.store.addExpr(Expr{
                .e_record = .{
                    .fields = new_fields_span,
                    .ext = new_ext,
                },
            }, base.Region.zero());
        },

        .e_tag => |tag| {
            const args = self.module_env.store.sliceExpr(tag.args);
            const args_start = self.module_env.store.scratch.?.exprs.top();

            for (args) |arg_idx| {
                const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                try self.module_env.store.scratch.?.exprs.append(new_arg);
            }

            const new_args_span = try self.module_env.store.exprSpanFrom(args_start);

            return try self.module_env.store.addExpr(Expr{
                .e_tag = .{
                    .name = tag.name,
                    .args = new_args_span,
                },
            }, base.Region.zero());
        },

        .e_unary_minus => |unary| {
            const new_expr = try self.duplicateExpr(unary.expr, type_subs);
            if (new_expr == unary.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_minus = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_unary_not => |unary| {
            const new_expr = try self.duplicateExpr(unary.expr, type_subs);
            if (new_expr == unary.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_unary_not = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_dot_access => |dot| {
            const new_receiver = try self.duplicateExpr(dot.receiver, type_subs);
            const new_args = if (dot.args) |args_span| blk: {
                const args = self.module_env.store.sliceExpr(args_span);
                const args_start = self.module_env.store.scratch.?.exprs.top();

                for (args) |arg_idx| {
                    const new_arg = try self.duplicateExpr(arg_idx, type_subs);
                    try self.module_env.store.scratch.?.exprs.append(new_arg);
                }

                break :blk try self.module_env.store.exprSpanFrom(args_start);
            } else null;

            return try self.module_env.store.addExpr(Expr{
                .e_dot_access = .{
                    .receiver = new_receiver,
                    .field_name = dot.field_name,
                    .field_name_region = dot.field_name_region,
                    .args = new_args,
                },
            }, base.Region.zero());
        },

        .e_return => |ret| {
            const new_expr = try self.duplicateExpr(ret.expr, type_subs);
            if (new_expr == ret.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_return = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_dbg => |dbg| {
            const new_expr = try self.duplicateExpr(dbg.expr, type_subs);
            if (new_expr == dbg.expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_dbg = .{ .expr = new_expr },
            }, base.Region.zero());
        },

        .e_expect => |expect| {
            const new_body = try self.duplicateExpr(expect.body, type_subs);
            if (new_body == expect.body) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_expect = .{ .body = new_body },
            }, base.Region.zero());
        },

        .e_match => |match| {
            const new_cond = try self.duplicateExpr(match.cond, type_subs);

            // Duplicate match branches
            const branch_indices = self.module_env.store.sliceMatchBranches(match.branches);
            const branch_start = self.module_env.store.scratchMatchBranchTop();

            for (branch_indices) |branch_idx| {
                const branch = self.module_env.store.getMatchBranch(branch_idx);

                // Duplicate the branch value (body)
                const new_value = try self.duplicateExpr(branch.value, type_subs);

                // Duplicate the guard if present
                const new_guard = if (branch.guard) |guard|
                    try self.duplicateExpr(guard, type_subs)
                else
                    null;

                const new_branch = Expr.Match.Branch{
                    .patterns = branch.patterns, // TODO: Duplicate patterns
                    .value = new_value,
                    .guard = new_guard,
                    .redundant = branch.redundant,
                };
                const new_branch_idx = try self.module_env.store.addMatchBranch(new_branch, base.Region.zero());
                try self.module_env.store.addScratchMatchBranch(new_branch_idx);
            }

            const new_branches_span = try self.module_env.store.matchBranchSpanFrom(branch_start);

            return try self.module_env.store.addExpr(Expr{
                .e_match = .{
                    .cond = new_cond,
                    .branches = new_branches_span,
                    .exhaustive = match.exhaustive,
                },
            }, base.Region.zero());
        },

        .e_nominal => |nominal| {
            const new_backing = try self.duplicateExpr(nominal.backing_expr, type_subs);
            if (new_backing == nominal.backing_expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },

        .e_nominal_external => |nominal| {
            const new_backing = try self.duplicateExpr(nominal.backing_expr, type_subs);
            if (new_backing == nominal.backing_expr) return expr_idx;
            return try self.module_env.store.addExpr(Expr{
                .e_nominal_external = .{
                    .module_idx = nominal.module_idx,
                    .target_node_idx = nominal.target_node_idx,
                    .backing_expr = new_backing,
                    .backing_type = nominal.backing_type,
                },
            }, base.Region.zero());
        },

        .e_for => |for_expr| {
            const new_expr = try self.duplicateExpr(for_expr.expr, type_subs);
            const new_body = try self.duplicateExpr(for_expr.body, type_subs);
            return try self.module_env.store.addExpr(Expr{
                .e_for = .{
                    .patt = for_expr.patt,
                    .expr = new_expr,
                    .body = new_body,
                },
            }, base.Region.zero());
        },

        // Pass through simple expressions unchanged
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_str_segment,
        .e_str,
        .e_lookup_local,
        .e_lookup_external,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_ellipsis,
        .e_anno_only,
        .e_lookup_required,
        .e_type_var_dispatch,
        .e_hosted_lambda,
        .e_low_level_lambda,
        .e_crash,
        => return expr_idx,
    }
}

/// Check if a type variable represents a polymorphic type
pub fn isPolymorphic(self: *Self, type_var: types.Var) bool {
    const resolved = self.types_store.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .flex, .rigid => true,
        .structure, .alias, .recursion_var, .err => false,
    };
}

/// Compute a structural hash for a type.
/// Two structurally equivalent types will have the same hash.
pub fn structuralTypeHash(self: *Self, type_var: types.Var) u64 {
    var hasher = std.hash.Wyhash.init(0);
    var seen = std.AutoHashMap(types.Var, void).init(self.allocator);
    defer seen.deinit();
    self.hashTypeRecursive(&hasher, type_var, &seen);
    return hasher.final();
}

fn hashTypeRecursive(
    self: *Self,
    hasher: *std.hash.Wyhash,
    type_var: types.Var,
    seen: *std.AutoHashMap(types.Var, void),
) void {
    // Check for cycles
    if (seen.contains(type_var)) {
        hasher.update("CYCLE");
        return;
    }
    seen.put(type_var, {}) catch return;

    const resolved = self.types_store.resolveVar(type_var);

    // Hash based on content
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            hasher.update(std.mem.asBytes(&@as(u8, @intFromEnum(flat_type))));

            switch (flat_type) {
                .nominal_type => |nom| {
                    hasher.update(std.mem.asBytes(&nom.ident.ident_idx));
                    // Hash type parameters
                    const vars = self.types_store.sliceVars(nom.vars.nonempty);
                    for (vars) |v| {
                        self.hashTypeRecursive(hasher, v, seen);
                    }
                },
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    // Hash function arguments and return type
                    const args = self.types_store.sliceVars(func.args);
                    for (args) |arg| {
                        self.hashTypeRecursive(hasher, arg, seen);
                    }
                    self.hashTypeRecursive(hasher, func.ret, seen);
                },
                .record => |record| {
                    // Hash record fields
                    const fields_slice = self.types_store.getRecordFieldsSlice(record.fields);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        hasher.update(std.mem.asBytes(&name));
                        self.hashTypeRecursive(hasher, var_, seen);
                    }
                },
                .tag_union => |tag_union| {
                    // Hash tags
                    const tags_slice = self.types_store.getTagsSlice(tag_union.tags);
                    for (tags_slice.items(.name), tags_slice.items(.args)) |name, args| {
                        hasher.update(std.mem.asBytes(&name));
                        const tag_args = self.types_store.sliceVars(args);
                        for (tag_args) |arg| {
                            self.hashTypeRecursive(hasher, arg, seen);
                        }
                    }
                },
                .tuple => |tuple| {
                    const elems = self.types_store.sliceVars(tuple.elems);
                    for (elems) |elem| {
                        self.hashTypeRecursive(hasher, elem, seen);
                    }
                },
                .record_unbound => |fields_range| {
                    const fields_slice = self.types_store.getRecordFieldsSlice(fields_range);
                    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
                        hasher.update(std.mem.asBytes(&name));
                        self.hashTypeRecursive(hasher, var_, seen);
                    }
                },
                .empty_record => hasher.update("empty_record"),
                .empty_tag_union => hasher.update("empty_tag_union"),
            }
        },
        .flex => |flex| {
            hasher.update("flex");
            if (flex.name) |name| {
                hasher.update(std.mem.asBytes(&name));
            }
        },
        .rigid => |rigid| {
            hasher.update("rigid");
            hasher.update(std.mem.asBytes(&rigid.name));
        },
        .alias => |alias| {
            hasher.update("alias");
            hasher.update(std.mem.asBytes(&alias.ident.ident_idx));
        },
        .recursion_var => |rec| {
            hasher.update("recursion");
            self.hashTypeRecursive(hasher, rec.structure, seen);
        },
        .err => hasher.update("err"),
    }
}

/// Get a readable type name for specialization suffix
pub fn getTypeName(self: *Self, type_var: types.Var) []const u8 {
    const resolved = self.types_store.resolveVar(type_var);
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            switch (flat_type) {
                .nominal_type => |nom| {
                    return self.module_env.getIdent(nom.ident.ident_idx);
                },
                .fn_pure, .fn_effectful, .fn_unbound => return "Fn",
                .record, .record_unbound => return "Record",
                .tag_union => return "Tag",
                .tuple => return "Tuple",
                .empty_record => return "EmptyRecord",
                .empty_tag_union => return "EmptyTag",
            }
        },
        .flex, .rigid => return "a",
        .alias => |alias| return self.module_env.getIdent(alias.ident.ident_idx),
        .recursion_var => return "Rec",
        .err => return "Err",
    }
}

/// Create a specialized name for a function
pub fn createSpecializedName(
    self: *Self,
    original_name: base.Ident.Idx,
    type_var: types.Var,
) !base.Ident.Idx {
    const type_hash = self.structuralTypeHash(type_var);
    const key = SpecializationKey{
        .original_ident = original_name,
        .type_hash = type_hash,
    };

    // Check if we already have this specialization
    if (self.specialization_names.get(key)) |existing| {
        return existing;
    }

    // Create new specialized name: original_TypeName_N
    const original = self.module_env.getIdent(original_name);
    const type_name = self.getTypeName(type_var);
    self.specialization_counter += 1;

    const specialized = try std.fmt.allocPrint(
        self.allocator,
        "{s}_{s}_{d}",
        .{ original, type_name, self.specialization_counter },
    );
    defer self.allocator.free(specialized);

    const specialized_ident = try self.module_env.insertIdent(base.Ident.for_text(specialized));

    try self.specialization_names.put(key, specialized_ident);
    return specialized_ident;
}

/// Get the number of completed specializations
pub fn getSpecializationCount(self: *const Self) usize {
    return self.specialized.count();
}

/// Get the specialized name for a function at a concrete type, if it exists
pub fn getSpecializedName(
    self: *const Self,
    original_ident: base.Ident.Idx,
    type_var: types.Var,
) ?base.Ident.Idx {
    // Need mutable self for structuralTypeHash but we only read
    const mutable_self: *Self = @constCast(self);
    const type_hash = mutable_self.structuralTypeHash(type_var);
    const key = SpecializationKey{
        .original_ident = original_ident,
        .type_hash = type_hash,
    };
    return self.specialization_names.get(key);
}

/// Iterator for specialized procs
pub fn specializedIterator(self: *const Self) std.AutoHashMap(SpecializationKey, SpecializedProc).ValueIterator {
    return self.specialized.valueIterator();
}

const testing = std.testing;

test "monomorphizer: init and deinit" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    try testing.expectEqual(@as(u32, 0), mono.specialization_counter);
    try testing.expectEqual(Phase.finding, mono.phase);
}

test "monomorphizer: register partial proc" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Create a test identifier
    const test_ident = try module_env.insertIdent(base.Ident.for_text("identity"));

    // Create a dummy type variable
    const type_var = try module_env.types.fresh();

    // Register a partial proc
    // Note: body_expr is undefined since we're not testing body duplication here
    try mono.registerPartialProc(
        test_ident,
        undefined, // body_expr not needed for this test
        CIR.Pattern.Span{ .span = .{ .start = 0, .len = 0 } },
        type_var,
        true,
    );

    try testing.expect(mono.isPartialProc(test_ident));
}

test "monomorphizer: isPolymorphic" {
    const allocator = testing.allocator;

    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var mono = Self.init(allocator, module_env, &module_env.types);
    defer mono.deinit();

    // Fresh flex var should be polymorphic
    const flex_var = try module_env.types.fresh();
    try testing.expect(mono.isPolymorphic(flex_var));
}
