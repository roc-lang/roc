const std = @import("std");
const base = @import("../base.zig");
const tracy = @import("../tracy.zig");
const collections = @import("../collections.zig");
const types_mod = @import("../types.zig");
const can = @import("canonicalize.zig");
const unifier = @import("check_types/unify.zig");
const occurs = @import("check_types/occurs.zig");
const problem = @import("check_types/problem.zig");
const snapshot = @import("check_types/snapshot.zig");
const instantiate = @import("check_types/instantiate.zig");
const copy_import = @import("check_types/copy_import.zig");
const CIR = @import("./canonicalize/CIR.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const Func = types_mod.Func;
const Var = types_mod.Var;
const Content = types_mod.Content;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

/// Key for the import cache: module index + expression index in that module
const ImportCacheKey = struct {
    module_idx: CIR.Import.Idx,
    expr_idx: CIR.Expr.Idx,
};

/// Cache for imported types to avoid repeated copying
///
/// When we import a type from another module, we need to copy it into our module's
/// type store because type variables are module-specific. However, since we use
/// "preserve" mode unification with imported types (meaning the imported type is
/// read-only and never modified), we can safely cache these copies and reuse them;
/// they will never be mutated during unification.
///
/// Benefits:
/// - Reduces memory usage by avoiding duplicate copies of the same imported type
/// - Improves performance by avoiding redundant copying operations
/// - Particularly beneficial for commonly imported values/functions
///
/// Example: If a module imports `List.map` and uses it 10 times, without caching
/// we would create 10 separate copies of the `List.map` type. With caching, we
/// create just one copy and reuse it.
const ImportCache = std.HashMapUnmanaged(ImportCacheKey, Var, struct {
    pub fn hash(_: @This(), key: ImportCacheKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&key.module_idx));
        hasher.update(std.mem.asBytes(&key.expr_idx));
        return hasher.final();
    }

    pub fn eql(_: @This(), a: ImportCacheKey, b: ImportCacheKey) bool {
        return a.module_idx == b.module_idx and a.expr_idx == b.expr_idx;
    }
}, 80);

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
cir: *const CIR,
regions: *Region.List,
other_modules: []const *CIR,
// owned
snapshots: snapshot.Store,
problems: problem.Store,
unify_scratch: unifier.Scratch,
occurs_scratch: occurs.Scratch,
instantiate_subs: instantiate.VarSubstitution,
/// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,

/// Init type solver
/// Does *not* own types_store or cir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *const CIR,
    other_modules: []const *CIR,
    regions: *Region.List,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types,
        .cir = cir,
        .other_modules = other_modules,
        .regions = regions,
        .snapshots = snapshot.Store.initCapacity(gpa, 512),
        .problems = problem.Store.initCapacity(gpa, 64),
        .unify_scratch = unifier.Scratch.init(gpa),
        .occurs_scratch = occurs.Scratch.init(gpa),
        .instantiate_subs = instantiate.VarSubstitution.init(gpa),
        .import_cache = ImportCache{},
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.instantiate_subs.deinit();
    self.import_cache.deinit(self.gpa);
}

/// Assert that type vars and regions in sync
pub inline fn debugAssertArraysInSync(self: *const Self) void {
    if (std.debug.runtime_safety) {
        const region_nodes = self.regions.len();
        const type_nodes = self.types.len();
        if (!(region_nodes == type_nodes)) {
            std.debug.panic(
                "Arrays out of sync:\n type_nodes={}\n  region_nodes={}\n ",
                .{ type_nodes, region_nodes },
            );
        }
    }
}

// unify //

/// Unify two types
pub fn unify(self: *Self, a: Var, b: Var) unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    return unifier.unify(
        self.cir.env,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
    );
}

// instantiate //

const InstantiateRegionBehavior = union(enum) {
    explicit: Region,
    use_root_instantiated,
    use_last_var,
};

/// Instantiate a variable, writing su
fn instantiateVar(self: *Self, var_to_instantiate: Var, region_behavior: InstantiateRegionBehavior) std.mem.Allocator.Error!Var {
    self.instantiate_subs.clearRetainingCapacity();
    const instantiated_var = try instantiate.instantiateVar(
        self.types,
        var_to_instantiate,
        &self.instantiate_subs,
    );

    const root_instantiated_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    if (self.instantiate_subs.count() > 0) {
        var iterator = self.instantiate_subs.iterator();
        while (iterator.next()) |x| {
            // Get the newly created var
            const fresh_var = x.value_ptr.*;
            try self.fillInRegionsThrough(fresh_var);

            switch (region_behavior) {
                .explicit => |region| {
                    self.setRegionAt(fresh_var, region);
                },
                .use_root_instantiated => {
                    self.setRegionAt(fresh_var, root_instantiated_region);
                },
                .use_last_var => {
                    const old_var = x.key_ptr.*;
                    const old_region = self.regions.get(@enumFromInt(@intFromEnum(old_var))).*;
                    self.setRegionAt(fresh_var, old_region);
                },
            }
        }
    }

    // Assert that we have regions for every type variable
    self.debugAssertArraysInSync();

    return instantiated_var;
}

// regions //

/// Fill slots in the regions array up to and including the target var
fn fillInRegionsThrough(self: *Self, target_var: Var) Allocator.Error!void {
    const idx = @intFromEnum(target_var);

    if (idx >= self.regions.len()) {
        try self.regions.items.ensureTotalCapacity(self.gpa, idx + 1);

        const empty_region = Region.zero();
        while (self.regions.len() <= idx) {
            self.regions.items.appendAssumeCapacity(empty_region);
        }
    }
}

/// The the region for a variable
fn setRegionAt(self: *Self, target_var: Var, new_region: Region) void {
    self.regions.set(@enumFromInt(@intFromEnum(target_var)), new_region);
}

// fresh vars //

/// The the region for a variable
fn fresh(self: *Self, new_region: Region) Allocator.Error!Var {
    const var_ = self.types.fresh();
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

/// The the region for a variable
fn freshFromContent(self: *Self, content: Content, new_region: Region) Allocator.Error!Var {
    const var_ = self.types.freshFromContent(content);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

// check types //

/// Check the types for all defs
pub fn checkDefs(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
    for (defs_slice) |def_idx| {
        try self.checkDef(def_idx);
    }
}

/// Check the types for a single definition
fn checkDef(self: *Self, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const def = self.cir.store.getDef(def_idx);
    const expr_var: Var = CIR.varFrom(def.expr);
    const expr_region = self.cir.store.getNodeRegion(CIR.nodeIdxFrom(def.expr));

    try self.checkPattern(def.pattern);
    _ = try self.checkExpr(def.expr);

    // Ensure the def has a type variable slot
    const def_var = CIR.varFrom(def_idx);

    // Special handling for lambda expressions with annotations
    if (def.annotation) |anno_idx| {
        const annotation = self.cir.store.getAnnotation(anno_idx);
        const expr = self.cir.store.getExpr(def.expr);

        // If the expression is a lambda and we have an annotation, pass the expected type
        if (expr == .e_lambda) {
            _ = try self.checkLambdaWithExpected(
                def.expr,
                expr_region,
                expr.e_lambda,
                annotation.signature,
            );
        }

        // Unify the expression with its annotation
        _ = self.unify(expr_var, annotation.signature);
    }

    // Unify the def with its expression
    _ = self.unify(def_var, CIR.varFrom(def.expr));

    // Also unify the pattern with the def - needed so lookups work correctly
    // TODO could we unify directly with the pattern elsewhere, to save a type var and unify() here?
    _ = self.unify(CIR.varFrom(def.pattern), def_var);
}

/// Check the types for an exprexpression. Returns whether evaluating the expr might perform side effects.
pub fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(CIR.nodeIdxFrom(expr_idx));
    var does_fx = false; // Does this expression potentially perform any side effects?
    switch (expr) {
        .e_int => |_| {},
        .e_frac_f64 => |_| {},
        .e_frac_dec => |_| {},
        .e_dec_small => |_| {},
        .e_str_segment => |_| {},
        .e_str => |_| {},
        .e_lookup_local => |local| {
            // For lookups, we need to connect the lookup expression to the actual variable
            // The lookup expression should have the same type as the pattern it refers to
            const lookup_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
            const pattern_var = @as(Var, @enumFromInt(@intFromEnum(local.pattern_idx)));
            _ = self.unify(lookup_var, pattern_var);
        },
        .e_lookup_external => |e| {
            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

            const module_idx = @intFromEnum(e.module_idx);
            if (module_idx < self.other_modules.len) {
                const other_module_cir = self.other_modules[module_idx];
                const other_module_env = &other_module_cir.env;

                // The idx of the expression in the other module
                const target_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(e.target_node_idx));

                // Check if we've already copied this import
                const cache_key = ImportCacheKey{
                    .module_idx = e.module_idx,
                    .expr_idx = target_expr_idx,
                };

                const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
                    // Reuse the previously copied type.
                    // This is safe because imported types are never modified (preserve mode)
                    cached_var
                else blk: {
                    // First time importing this type - copy it and cache the result
                    const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_expr_idx)));
                    const new_copy = try copy_import.copyImportedType(
                        &other_module_env.*.types,
                        self.types,
                        imported_var,
                        &other_module_env.*.idents,
                        &self.cir.env.idents,
                        self.gpa,
                    );
                    try self.import_cache.put(self.gpa, cache_key, new_copy);
                    break :blk new_copy;
                };

                // Unify our expression with the copied type
                // Note: This unification uses "preserve" mode internally (via copy_import),
                // which means the imported type (copied_var) is read-only. This is why
                // we can safely cache and reuse copied_var for multiple imports.
                const result = self.unify(expr_var, copied_var);
                if (result.isProblem()) {
                    self.setProblemTypeMismatchDetail(result.problem, .{
                        .cross_module_import = .{
                            .import_region = expr_idx,
                            .module_idx = e.module_idx,
                        },
                    });

                    try self.types.setVarContent(expr_var, .err);
                }
            } else {
                // Import not found
                try self.types.setVarContent(expr_var, .err);
            }
        },
        .e_list => |list| {
            const elem_var = @as(Var, @enumFromInt(@intFromEnum(list.elem_var)));
            const elems = self.cir.store.exprSlice(list.elems);

            std.debug.assert(elems.len > 0); // Should never be 0 here, because this is not an .empty_list

            // We need to type-check the first element, but we don't need to unify it with
            // anything because we already pre-unified the list's elem var with it.
            const first_elem_idx = elems[0];
            var last_elem_idx: CIR.Expr.Idx = first_elem_idx;
            does_fx = try self.checkExpr(first_elem_idx) or does_fx;

            for (elems[1..], 1..) |elem_expr_id, i| {
                does_fx = try self.checkExpr(elem_expr_id) or does_fx;

                // Unify each element's var with the list's elem var
                const result = self.unify(elem_var, @enumFromInt(@intFromEnum(elem_expr_id)));
                self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                    .last_elem_expr = last_elem_idx,
                    .incompatible_elem_index = @intCast(i),
                    .list_length = @intCast(elems.len),
                } });

                if (!result.isOk()) {
                    // Check remaining elements to catch their individual errors
                    for (elems[i + 1 ..]) |remaining_elem_id| {
                        does_fx = try self.checkExpr(remaining_elem_id) or does_fx;
                    }

                    // Break to avoid cascading errors
                    break;
                }

                last_elem_idx = elem_expr_id;
            }
        },
        .e_empty_list => |_| {},
        .e_match => |match| {
            does_fx = try self.checkMatchExpr(expr_idx, match);
        },
        .e_if => |if_expr| {
            does_fx = try self.checkIfElseExpr(expr_idx, expr_region, if_expr);
        },
        .e_call => |call| {
            // Get all expressions - first is function, rest are arguments
            const all_exprs = self.cir.store.sliceExpr(call.args);

            if (all_exprs.len == 0) return false; // No function to call

            // First expression is the function being called; the rest are args.
            const func_expr_idx = all_exprs[0];
            does_fx = try self.checkExpr(func_expr_idx) or does_fx; // func_expr could be effectful, e.g. `(mk_fn!())(arg)`
            const call_args = all_exprs[1..];
            for (call_args) |arg_expr_idx| {
                // Each arg could also be effectful, e.g. `fn(mk_arg!(), mk_arg!())`
                does_fx = try self.checkExpr(arg_expr_idx) or does_fx;
            }

            // Don't try to unify with the function if the function is a runtime error.
            const func_expr = self.cir.store.getExpr(func_expr_idx);
            if (func_expr != .e_runtime_error) {
                const func_expr_region = self.cir.store.getRegionAt(CIR.nodeIdxFrom(func_expr_idx));

                const call_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                const func_var = @as(Var, @enumFromInt(@intFromEnum(func_expr_idx)));
                const resolved_func = self.types.resolveVar(func_var);

                // Check if this is an annotated function that needs instantiation
                // We only instantiate if the function actually contains type variables
                var current_func_var = func_var;
                var current_content = resolved_func.desc.content;

                content_switch: switch (current_content) {
                    .structure => |flat_type| switch (flat_type) {
                        .fn_effectful => |_| {
                            does_fx = true;
                            if (self.types.needsInstantiation(current_func_var)) {
                                const instantiated_var = try self.instantiateVar(current_func_var, .{ .explicit = expr_region });
                                const resolved_inst = self.types.resolveVar(instantiated_var);
                                std.debug.assert(resolved_inst.desc.content == .structure);
                                std.debug.assert(resolved_inst.desc.content.structure == .fn_effectful);
                                const inst_func = resolved_inst.desc.content.structure.fn_effectful;
                                try self.unifyCallWithFunc(call_var, inst_func, call_args, func_var, expr_region);
                                return does_fx;
                            }
                        },
                        .fn_pure => |_| {
                            if (self.types.needsInstantiation(current_func_var)) {
                                const instantiated_var = try self.instantiateVar(current_func_var, .{ .explicit = expr_region });
                                const resolved_inst = self.types.resolveVar(instantiated_var);
                                std.debug.assert(resolved_inst.desc.content == .structure);
                                std.debug.assert(resolved_inst.desc.content.structure == .fn_pure);
                                const inst_func = resolved_inst.desc.content.structure.fn_pure;
                                try self.unifyCallWithFunc(call_var, inst_func, call_args, func_var, func_expr_region);
                                return does_fx;
                            }
                        },
                        .fn_unbound => |_| {
                            // Create TypeWriter for converting types to strings
                            if (self.types.needsInstantiation(current_func_var)) {
                                const instantiated_var = try self.instantiateVar(current_func_var, .{ .explicit = expr_region });
                                const resolved_inst = self.types.resolveVar(instantiated_var);
                                std.debug.assert(resolved_inst.desc.content == .structure);
                                std.debug.assert(resolved_inst.desc.content.structure == .fn_unbound);
                                const inst_func = resolved_inst.desc.content.structure.fn_unbound;
                                try self.unifyCallWithFunc(call_var, inst_func, call_args, func_var, expr_region);
                                return does_fx;
                            }
                        },
                        else => {
                            // Non-function structure - fall through
                        },
                    },
                    .alias => |alias| {
                        // Resolve the alias, then continue on to the appropriate branch.
                        // (It might be another alias, or we might be done and ready to proceed.)
                        const backing_var = self.types.getAliasBackingVar(alias);
                        current_func_var = backing_var;
                        current_content = self.types.resolveVar(backing_var).desc.content;
                        continue :content_switch current_content;
                    },
                    else => {
                        // Non-structure content - fall through
                    },
                }

                // We didn't handle the function call above (either because it wasn't a function
                // or it didn't need instantiation), so fall back on this logic.
                const arg_vars: []Var = @constCast(@ptrCast(@alignCast(call_args)));

                // Create an unbound function type with the call result as return type
                // The unification will propagate the actual return type to the call
                //
                // TODO: Do we need to insert a CIR placeholder node here as well?
                // What happens if later this type variable has a problem, and we
                // try to look up its region in CIR?
                const func_content = self.types.mkFuncUnbound(arg_vars, call_var);
                const expected_func_var = try self.freshFromContent(func_content, expr_region);
                _ = self.unify(expected_func_var, current_func_var);
            }
        },
        .e_record => |e| {
            // Perform field-by-field unification between the record structure's
            // field type variables and the actual field value expression types.
            //
            // 1. Resolve the expression var to get the record structure
            // 2. Type check each field value expression (to get concrete types)
            // 3. For each field, unify the field type var with the field value type var
            // 4. Unification propagates concrete types through the type system

            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
            const record_var_resolved = self.types.resolveVar(expr_var);
            const record_var_content = record_var_resolved.desc.content;

            // Process each field
            for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
                const field = self.cir.store.getRecordField(field_idx);

                // STEP 1: Check the field value expression first
                // This ensures the field value has a concrete type to unify with
                does_fx = try self.checkExpr(field.value) or does_fx;

                // STEP 2: Find the corresponding field type in the record structure
                // This only works if record_var_content is .structure.record
                if (record_var_content == .structure and record_var_content.structure == .record) {
                    const record_fields = self.types.getRecordFieldsSlice(record_var_content.structure.record.fields);

                    // STEP 3: Find the field with matching name and unify types
                    const field_names = record_fields.items(.name);
                    const field_vars = record_fields.items(.var_);
                    for (field_names, field_vars) |type_field_name, type_field_var| {
                        if (self.cir.env.idents.identsHaveSameText(type_field_name, field.name)) {
                            // Extract the type variable from the field value expression
                            // Different expression types store their type variables in different places
                            const field_expr_type_var = @as(Var, @enumFromInt(@intFromEnum(field.value)));

                            // STEP 4: Unify field type variable with field value type variable
                            // This is where concrete types (like Str, Num) get propagated
                            // from field values to the record structure
                            _ = self.unify(type_field_var, field_expr_type_var);
                            break;
                        }
                    }
                }
                // If record_var_content is NOT .structure.record, unification is skipped
                // This typically happens when canonicalization didn't set the record structure properly
            }
        },
        .e_empty_record => |_| {},
        .e_tag => |_| {},
        .e_nominal => |nominal| {
            // We are unifying against a nominal type. The way this works is:
            // 1. First, get the `NominalType` out of the types store
            // 2. Then, instantiate it's backing var
            // 3. Next, unify that instantiated var against the CIR backing var
            // 4. If successful, instantiate the root nominal var, then unify it
            //    against the root expr var. (the root expr var should be flex)
            //
            // We have to do all this instantiating to avoid propagating `.err`
            // types across the module in the event of failure

            const expr_var = CIR.varFrom(expr_idx);
            const expr_backing_var = CIR.varFrom(nominal.backing_expr);

            // First, get the qualified variable and check if it's a nominal type
            const nominal_var = CIR.varFrom(nominal.nominal_type_decl);
            const nominal_content = self.types.resolveVar(nominal_var).desc.content;

            // Handle cases where the nominal type is malformed or in an error state
            const nominal_type = switch (nominal_content) {
                .structure => |structure| switch (structure) {
                    .nominal_type => |nt| nt,
                    else => {
                        // Nominal type is not actually a nominal type - set expr to error and continue
                        try self.types.setVarContent(expr_var, .err);
                        return false;
                    },
                },
                else => {
                    // Nominal type is in an error state - set expr to error and continue
                    try self.types.setVarContent(expr_var, .err);
                    return false;
                },
            };

            // Then, instantiate the nominal types backing var, for unification
            const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);
            const instantiated_backing_var = try self.instantiateVar(nominal_backing_var, .{ .explicit = expr_region });

            // Then, unify the nominal type's backing var against the CIR backing var
            const result = self.unify(instantiated_backing_var, expr_backing_var);

            // Handle the result
            switch (result) {
                .ok => {
                    // Then, instantiate the nominal type
                    const instantiated_qualified_var = try self.instantiateVar(nominal_var, .{ .explicit = expr_region });

                    // Unify - this should always succeed expr_var should be flex var
                    _ = self.unify(expr_var, instantiated_qualified_var);
                },
                .problem => |problem_idx| {
                    // Depending on the type of the backing variable, set the type
                    // mismatch detail so the user gets a better error message
                    switch (nominal.backing_type) {
                        .tag => {
                            self.setProblemTypeMismatchDetail(problem_idx, .invalid_nominal_tag);
                        },
                        else => {
                            // TODO: Add special-case problems for other nominal types
                        },
                    }

                    // Then, set the root expr to be an err
                    try self.types.setVarContent(expr_var, .err);
                },
            }
        },
        .e_zero_argument_tag => |_| {},
        .e_binop => |binop| {
            does_fx = try self.checkBinopExpr(expr_idx, expr_region, binop);
        },
        .e_block => |block| {
            // Check all statements in the block
            const statements = self.cir.store.sliceStatements(block.stmts);
            for (statements) |stmt_idx| {
                const stmt = self.cir.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl_stmt| {
                        // Check pattern and expression, then unify
                        try self.checkPattern(decl_stmt.pattern);
                        does_fx = try self.checkExpr(decl_stmt.expr) or does_fx;

                        // Unify the pattern with the expression
                        const pattern_var: Var = @enumFromInt(@intFromEnum(decl_stmt.pattern));
                        const expr_var: Var = @enumFromInt(@intFromEnum(decl_stmt.expr));
                        _ = self.unify(pattern_var, expr_var);
                    },
                    .s_reassign => |reassign| {
                        does_fx = try self.checkExpr(reassign.expr) or does_fx;
                    },
                    .s_expr => |expr_stmt| {
                        does_fx = try self.checkExpr(expr_stmt.expr) or does_fx;
                    },
                    else => {
                        // Other statement types don't need expression checking
                    },
                }
            }

            // Check the final expression
            does_fx = try self.checkExpr(block.final_expr) or does_fx;

            // Link the root expr with the final expr
            _ = self.unify(
                @enumFromInt(@intFromEnum(expr_idx)),
                @enumFromInt(@intFromEnum(block.final_expr)),
            );
        },
        .e_lambda => |lambda| {
            does_fx = try self.checkLambdaWithExpected(expr_idx, expr_region, lambda, null);
        },
        .e_tuple => |tuple| {
            // Check tuple elements
            const elems_slice = self.cir.store.exprSlice(tuple.elems);
            for (elems_slice) |single_elem_expr_idx| {
                does_fx = try self.checkExpr(single_elem_expr_idx) or does_fx;
            }

            // The tuple type is created in the type store in canonicalize, so
            // nothing more needs to be done here
        },
        .e_dot_access => |dot_access| {
            // Check the receiver expression
            does_fx = try self.checkExpr(dot_access.receiver) or does_fx;

            // Get the type of the receiver
            const receiver_var = @as(Var, @enumFromInt(@intFromEnum(dot_access.receiver)));
            const resolved_receiver = self.types.resolveVar(receiver_var);

            // Handle different receiver types
            switch (resolved_receiver.desc.content) {
                .structure => |structure| switch (structure) {
                    .nominal_type => |nominal| {
                        // This is a static dispatch on a nominal type
                        if (dot_access.args) |args_span| {
                            // Method call with arguments
                            // Get the origin module path
                            const origin_module_path = self.cir.env.idents.getText(nominal.origin_module);

                            // Find which imported module matches this path
                            var origin_module_idx: ?CIR.Import.Idx = null;
                            var origin_module: ?*const CIR = null;

                            // Check if it's the current module
                            const current_module_ident = self.cir.env.idents.insert(self.gpa, base.Ident.for_text(self.cir.module_name), base.Region.zero());
                            if (std.mem.eql(u8, origin_module_path, self.cir.env.idents.getText(current_module_ident))) {
                                origin_module = self.cir;
                            } else {
                                // Search through imported modules
                                for (self.other_modules, 0..) |other_module, idx| {
                                    const other_module_ident = other_module.env.idents.insert(self.gpa, base.Ident.for_text(other_module.module_name), base.Region.zero());
                                    const other_path = other_module.env.idents.getText(other_module_ident);
                                    if (std.mem.eql(u8, origin_module_path, other_path)) {
                                        origin_module_idx = @enumFromInt(idx);
                                        origin_module = other_module;
                                        break;
                                    }
                                }
                            }

                            if (origin_module) |module| {
                                // Look up the method in the origin module's exports
                                const method_name_str = self.cir.env.idents.getText(dot_access.field_name);

                                // Search through the module's exposed nodes
                                if (module.env.exposed_nodes.get(method_name_str)) |node_idx| {
                                    // Found the method!
                                    const target_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(node_idx));

                                    // Check if we've already copied this import
                                    const cache_key = ImportCacheKey{
                                        .module_idx = origin_module_idx orelse @enumFromInt(0), // Current module
                                        .expr_idx = target_expr_idx,
                                    };

                                    const method_var = if (self.import_cache.get(cache_key)) |cached_var|
                                        cached_var
                                    else blk: {
                                        // Copy the method's type from the origin module to our type store
                                        const source_var = @as(Var, @enumFromInt(@intFromEnum(target_expr_idx)));
                                        const new_copy = try copy_import.copyImportedType(
                                            &module.env.types,
                                            self.types,
                                            source_var,
                                            &module.env.idents,
                                            &self.cir.env.idents,
                                            self.gpa,
                                        );
                                        try self.import_cache.put(self.gpa, cache_key, new_copy);
                                        break :blk new_copy;
                                    };

                                    // Check all arguments
                                    var i: u32 = 0;
                                    while (i < args_span.span.len) : (i += 1) {
                                        const arg_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(args_span.span.start + i));
                                        does_fx = try self.checkExpr(arg_expr_idx) or does_fx;
                                    }

                                    // Create argument list for the function call
                                    var args = std.ArrayList(Var).init(self.gpa);
                                    defer args.deinit();

                                    // Add the receiver (the nominal type) as the first argument
                                    try args.append(receiver_var);

                                    // Add the remaining arguments
                                    i = 0;
                                    while (i < args_span.span.len) : (i += 1) {
                                        const arg_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(args_span.span.start + i));
                                        const arg_var = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));
                                        try args.append(arg_var);
                                    }

                                    // Create a function type for the method call
                                    const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                                    const func_content = self.types.mkFuncUnbound(args.items, dot_access_var);
                                    const expected_func_var = try self.freshFromContent(func_content, expr_region);

                                    // Unify with the imported method type
                                    _ = self.unify(expected_func_var, method_var);

                                    // Store the resolved method info for code generation
                                    // This will be used by the code generator to emit the correct function call
                                    // For now, the type information in the expression variable is sufficient
                                } else {
                                    // Method not found in origin module
                                    // TODO: Add a proper error type for method not found on nominal type
                                    try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
                                }
                            } else {
                                // Origin module not found
                                // TODO: Add a proper error type for origin module not found
                                try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
                            }
                        } else {
                            // No arguments - this might be a field access on a nominal type's backing type
                            // TODO: Handle field access on nominal types
                            try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
                        }
                    },
                    .record => |record| {
                        // Receiver is already a record, find the field
                        const fields = self.types.getRecordFieldsSlice(record.fields);

                        // Find the field with the matching name
                        for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
                            if (field_name == dot_access.field_name) {
                                // Unify the dot access expression with the field type
                                const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                                _ = self.unify(dot_access_var, field_var);
                                break;
                            }
                        }
                    },
                    .record_unbound => |record_unbound| {
                        // Receiver is an unbound record, find the field
                        const fields = self.types.getRecordFieldsSlice(record_unbound);

                        // Find the field with the matching name
                        for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
                            if (field_name == dot_access.field_name) {
                                // Unify the dot access expression with the field type
                                const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                                _ = self.unify(dot_access_var, field_var);
                                break;
                            }
                        }
                    },
                    else => {
                        // Receiver is not a record, this is a type error
                        // For now, we'll let unification handle the error
                    },
                },
                .flex_var => {
                    // Receiver is unbound, we need to constrain it to be a record with the field
                    // Create a fresh variable for the field type
                    const field_var = try self.fresh(expr_region);
                    const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                    _ = self.unify(dot_access_var, field_var);

                    // Create a record type with this field
                    const field_idx = self.types.appendRecordField(.{
                        .name = dot_access.field_name,
                        .var_ = field_var,
                    });
                    const fields_range = types_mod.RecordField.SafeMultiList.Range{
                        .start = field_idx,
                        .end = @enumFromInt(@intFromEnum(field_idx) + 1),
                    };

                    // Create an extension variable for other possible fields
                    // Create the record content
                    const record_content = types_mod.Content{
                        .structure = .{
                            .record_unbound = fields_range,
                        },
                    };

                    // Unify the receiver with this record type
                    //
                    // TODO: Do we need to insert a CIR placeholder node here as well?
                    // What happens if later this type variable has a problem, and we
                    // try to look up it's region in CIR?
                    const record_var = try self.freshFromContent(record_content, expr_region);
                    _ = self.unify(receiver_var, record_var);
                },
                else => {
                    // Other cases (rigid_var, alias, etc.) - let unification handle errors
                },
            }
        },
        .e_runtime_error => {},
        .e_crash => {},
        .e_dbg => {},
        .e_ellipsis => {},
        .e_expect => {},
    }

    return does_fx;
}

/// Helper function to unify a call expression with a function type
fn unifyCallWithFunc(
    self: *Self,
    call_var: Var,
    func: types_mod.Func,
    call_args: []const CIR.Expr.Idx,
    original_func_var: Var,
    region: Region,
) std.mem.Allocator.Error!void {
    // Unify instantiated argument types with actual arguments
    const inst_args = self.types.getFuncArgsSlice(func.args);
    const arg_vars: []Var = @constCast(@ptrCast(@alignCast(call_args)));

    // Only unify arguments if counts match - otherwise let the normal
    // unification process handle the arity mismatch error
    if (inst_args.len == arg_vars.len) {
        for (inst_args, arg_vars) |inst_arg, actual_arg| {
            _ = self.unify(actual_arg, inst_arg);
        }
        // The call's type is the instantiated return type
        _ = self.unify(call_var, func.ret);
    } else {
        // Fall back to normal unification to get proper error message
        // Use the original func_var to avoid issues with instantiated variables in error reporting
        const func_content = self.types.mkFuncUnbound(arg_vars, call_var);
        const expected_func_var = try self.freshFromContent(func_content, region);
        _ = self.unify(expected_func_var, original_func_var);
    }
}

/// Check a lambda expression with an optional expected type
fn checkLambdaWithExpected(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    _: Region,
    lambda: std.meta.FieldType(CIR.Expr, .e_lambda),
    expected_type: ?Var,
) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // The function is effectful iff the body of the lambda is effectful
    const is_effectful = try self.checkExpr(lambda.body);

    // Get the actual lambda arguments
    const arg_patterns = self.cir.store.slicePatterns(lambda.args);

    // Create type variables for each argument pattern
    // Since pattern idx map 1-to-1 to variables, we can get cast the slice to vars
    const arg_vars: []Var = @ptrCast(@alignCast(arg_patterns));

    // The root expr will be the entire functions var
    const fn_var = CIR.varFrom(expr_idx);

    // The return type var is just the body's var
    const return_var = @as(Var, @enumFromInt(@intFromEnum(lambda.body)));

    if (is_effectful) {
        // If the function body does effects, create an effectful function.
        _ = try self.types.setVarContent(fn_var, self.types.mkFuncEffectful(arg_vars, return_var));
    } else {
        // If the function body does *not* do effects, create an unbound function.
        // (Pure would mean it's *annotated* as pure, but we aren't claiming that here!)
        _ = try self.types.setVarContent(fn_var, self.types.mkFuncUnbound(arg_vars, return_var));
    }

    // If the expected type is a function, then check the args and return type
    // directly. This results in better error messages!
    //
    // If the expected type is *not* a function, unify like normal (which will be a mismatch)
    if (expected_type) |expected| {
        const instantiated_var = try self.instantiateVar(expected, .use_last_var);
        const instantiated_content = self.types.resolveVar(instantiated_var).desc.content;
        if (instantiated_content == .structure) {
            switch (instantiated_content.structure) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    const expected_args = self.types.getFuncArgsSlice(func.args);
                    // Unify each pattern with its expected type before checking body
                    if (expected_args.len == arg_patterns.len) {
                        for (arg_patterns, expected_args) |pattern_idx, expected_arg| {
                            const pattern_var = CIR.varFrom(pattern_idx);
                            _ = self.unify(pattern_var, expected_arg);
                        }
                        _ = self.unify(return_var, func.ret);
                    } else {
                        _ = self.unify(fn_var, instantiated_var);
                    }
                },
                else => {
                    _ = self.unify(fn_var, instantiated_var);
                },
            }
        } else {
            _ = self.unify(fn_var, instantiated_var);
        }
    }

    return is_effectful;
}

// binop //

/// Check the types for an if-else expr
fn checkBinopExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, binop: CIR.Expr.Binop) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    var does_fx = try self.checkExpr(binop.lhs);
    does_fx = try self.checkExpr(binop.rhs) or does_fx;

    switch (binop.op) {
        .add, .sub, .mul, .div, .rem, .lt, .gt, .le, .ge, .eq, .ne, .pow, .div_trunc => {
            // TODO: These will use static dispact of the lhs, passing in rhs
        },
        .@"and" => {
            const lhs_fresh_bool = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
            const lhs_result = self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            if (lhs_result.isOk()) {
                const rhs_fresh_bool = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
                const rhs_result = self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
                self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                    .binop_expr = expr_idx,
                    .problem_side = .rhs,
                    .binop = .@"and",
                } });
            }
        },
        .@"or" => {
            const lhs_fresh_bool = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
            const lhs_result = self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"or",
            } });

            if (lhs_result.isOk()) {
                const rhs_fresh_bool = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
                const rhs_result = self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
                self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                    .binop_expr = expr_idx,
                    .problem_side = .rhs,
                    .binop = .@"or",
                } });
            }
        },
        .pipe_forward => {},
        .null_coalesce => {},
    }

    return does_fx;
}

// if-else //

/// Check the types for an if-else expr
fn checkIfElseExpr(
    self: *Self,
    if_expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    if_: std.meta.FieldType(CIR.Expr, .e_if),
) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const branches = self.cir.store.sliceIfBranches(if_.branches);

    // Should never be 0
    std.debug.assert(branches.len > 0);

    // Get the first branch
    const first_branch_idx = branches[0];
    const first_branch = self.cir.store.getIfBranch(first_branch_idx);

    // Check the condition of the 1st branch
    var does_fx = try self.checkExpr(first_branch.cond);
    const first_cond_var: Var = @enumFromInt(@intFromEnum(first_branch.cond));
    const bool_var = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
    const first_cond_result = self.unify(bool_var, first_cond_var);
    self.setDetailIfTypeMismatch(first_cond_result, .incompatible_if_cond);

    // Then we check the 1st branch's body
    does_fx = try self.checkExpr(first_branch.body) or does_fx;

    // The 1st branch's body is the type all other branches must match
    const branch_var = @as(Var, @enumFromInt(@intFromEnum(first_branch.body)));

    // Total number of branches (including final else)
    const num_branches: u32 = @intCast(branches.len + 1);

    var last_if_branch = first_branch_idx;
    for (branches[1..], 1..) |branch_idx, cur_index| {
        const branch = self.cir.store.getIfBranch(branch_idx);

        // Check the branches condition
        does_fx = try self.checkExpr(branch.cond) or does_fx;
        const cond_var: Var = @enumFromInt(@intFromEnum(branch.cond));
        const branch_bool_var = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
        const cond_result = self.unify(branch_bool_var, cond_var);
        self.setDetailIfTypeMismatch(cond_result, .incompatible_if_cond);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body) or does_fx;
        const body_var: Var = @enumFromInt(@intFromEnum(branch.body));
        const body_result = self.unify(branch_var, body_var);
        self.setDetailIfTypeMismatch(body_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
            .parent_if_expr = if_expr_idx,
            .last_if_branch = last_if_branch,
            .num_branches = num_branches,
            .problem_branch_index = @intCast(cur_index),
        } });

        if (!body_result.isOk()) {
            // Check remaining branches to catch their individual errors
            for (branches[cur_index + 1 ..]) |remaining_branch_idx| {
                const remaining_branch = self.cir.store.getIfBranch(remaining_branch_idx);

                does_fx = try self.checkExpr(remaining_branch.cond) or does_fx;
                const remaining_cond_var: Var = @enumFromInt(@intFromEnum(remaining_branch.cond));

                const fresh_bool = try self.instantiateVar(CIR.varFrom(can.BUILTIN_BOOL), .{ .explicit = expr_region });
                const remaining_cond_result = self.unify(fresh_bool, remaining_cond_var);
                self.setDetailIfTypeMismatch(remaining_cond_result, .incompatible_if_cond);

                does_fx = try self.checkExpr(remaining_branch.body) or does_fx;
                try self.types.setVarContent(@enumFromInt(@intFromEnum(remaining_branch.body)), .err);
            }

            // Break to avoid cascading errors
            break;
        }

        last_if_branch = branch_idx;
    }

    // Check the final else
    does_fx = try self.checkExpr(if_.final_else) or does_fx;
    const final_else_var: Var = @enumFromInt(@intFromEnum(if_.final_else));
    const final_else_result = self.unify(branch_var, final_else_var);
    self.setDetailIfTypeMismatch(final_else_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
        .parent_if_expr = if_expr_idx,
        .last_if_branch = last_if_branch,
        .num_branches = num_branches,
        .problem_branch_index = num_branches - 1,
    } });

    return does_fx;
}

// match //

/// Check the types for an if-else expr
fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, match: CIR.Expr.Match) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond);
    const cond_var = CIR.varFrom(match.cond);

    // Bail if we somehow have 0 branches
    // TODO: Should this be an error? Here or in Can?
    if (match.branches.span.len == 0) return does_fx;

    // Get slice of branches
    const branch_idxs = self.cir.store.sliceMatchBranches(match.branches);

    // Manually check the 1st branch
    // The type of the branch's body becomes the var other branch bodies must unify
    // against.
    const first_branch_idx = branch_idxs[0];
    const first_branch = self.cir.store.getMatchBranch(first_branch_idx);

    const first_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(first_branch.patterns);

    for (first_branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
        const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
        try self.checkPattern(branch_ptrn.pattern);
        const branch_ptrn_var = CIR.varFrom(branch_ptrn.pattern);

        const ptrn_result = self.unify(cond_var, branch_ptrn_var);
        self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
            .match_expr = expr_idx,
            .num_branches = @intCast(match.branches.span.len),
            .problem_branch_index = 0,
            .num_patterns = @intCast(first_branch_ptrn_idxs.len),
            .problem_pattern_index = @intCast(cur_ptrn_index),
        } });
    }

    // Check the first branch's value, then use that at the branch_var
    does_fx = try self.checkExpr(first_branch.value) or does_fx;
    const branch_var = CIR.varFrom(first_branch.value);

    // Then iterate over the rest of the branches
    for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
        const branch = self.cir.store.getMatchBranch(branch_idx);

        // First, check the patterns of this branch
        const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
        for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
            // Check the pattern's sub types
            const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
            try self.checkPattern(branch_ptrn.pattern);

            // Check the pattern against the cond
            const branch_ptrn_var = CIR.varFrom(branch_ptrn.pattern);
            const ptrn_result = self.unify(cond_var, branch_ptrn_var);
            self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                .match_expr = expr_idx,
                .num_branches = @intCast(match.branches.span.len),
                .problem_branch_index = @intCast(branch_cur_index),
                .num_patterns = @intCast(branch_ptrn_idxs.len),
                .problem_pattern_index = @intCast(cur_ptrn_index),
            } });
        }

        // Then, check the body
        does_fx = try self.checkExpr(branch.value) or does_fx;
        const branch_result = self.unify(branch_var, CIR.varFrom(branch.value));
        self.setDetailIfTypeMismatch(branch_result, problem.TypeMismatchDetail{ .incompatible_match_branches = .{
            .match_expr = expr_idx,
            .num_branches = @intCast(match.branches.span.len),
            .problem_branch_index = @intCast(branch_cur_index),
        } });

        if (!branch_result.isOk()) {
            // If there was a body mismatch, do not check other branches to stop
            // cascading errors. But still check each other branch's sub types
            for (branch_idxs[branch_cur_index + 1 ..], branch_cur_index + 1..) |other_branch_idx, other_branch_cur_index| {
                const other_branch = self.cir.store.getMatchBranch(other_branch_idx);

                // Still check the other patterns
                const other_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(other_branch.patterns);
                for (other_branch_ptrn_idxs, 0..) |other_branch_ptrn_idx, other_cur_ptrn_index| {
                    // Check the pattern's sub types
                    const other_branch_ptrn = self.cir.store.getMatchBranchPattern(other_branch_ptrn_idx);
                    try self.checkPattern(other_branch_ptrn.pattern);

                    // Check the pattern against the cond
                    const other_branch_ptrn_var = CIR.varFrom(other_branch_ptrn.pattern);
                    const ptrn_result = self.unify(cond_var, other_branch_ptrn_var);
                    self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                        .match_expr = expr_idx,
                        .num_branches = @intCast(match.branches.span.len),
                        .problem_branch_index = @intCast(other_branch_cur_index),
                        .num_patterns = @intCast(other_branch_ptrn_idxs.len),
                        .problem_pattern_index = @intCast(other_cur_ptrn_index),
                    } });
                }

                // Then check the other branch's exprs
                does_fx = try self.checkExpr(other_branch.value) or does_fx;
                try self.types.setVarContent(CIR.varFrom(other_branch.value), .err);
            }

            // Then stop type checking for this branch
            break;
        }
    }

    return does_fx;
}

// problems //

/// If the provided result is a type mismatch problem, append the detail to the
/// problem in the store.
/// This allows us to show the user nice, more specific errors than a generic
/// type mismatch
fn setDetailIfTypeMismatch(self: *Self, result: unifier.Result, mismatch_detail: problem.TypeMismatchDetail) void {
    switch (result) {
        .ok => {},
        .problem => |problem_idx| {
            self.setProblemTypeMismatchDetail(problem_idx, mismatch_detail);
        },
    }
}

/// If the provided problem is a type mismatch, set the mismatch detail.
/// This allows us to show the user nice, more specific errors than a generic
/// type mismatch
fn setProblemTypeMismatchDetail(self: *Self, problem_idx: problem.Problem.Idx, mismatch_detail: problem.TypeMismatchDetail) void {
    switch (self.problems.problems.get(problem_idx)) {
        .type_mismatch => |mismatch| {
            self.problems.problems.set(problem_idx, .{
                .type_mismatch = .{
                    .types = mismatch.types,
                    .detail = mismatch_detail,
                },
            });
        },
        else => {
            // For other problem types (e.g., number_does_not_fit), the
            // original problem is already more specific than our custom
            // problem, so we should keep it as-is and not replace it.
        },
    }
}

// tests //

test "minimum signed values fit in their respective types" {
    const test_cases = .{
        .{ .value = -128, .type = types_mod.Num.Int.Precision.i8, .should_fit = true },
        .{ .value = -129, .type = types_mod.Num.Int.Precision.i8, .should_fit = false },
        .{ .value = -32768, .type = types_mod.Num.Int.Precision.i16, .should_fit = true },
        .{ .value = -32769, .type = types_mod.Num.Int.Precision.i16, .should_fit = false },
        .{ .value = -2147483648, .type = types_mod.Num.Int.Precision.i32, .should_fit = true },
        .{ .value = -2147483649, .type = types_mod.Num.Int.Precision.i32, .should_fit = false },
        .{ .value = -9223372036854775808, .type = types_mod.Num.Int.Precision.i64, .should_fit = true },
        .{ .value = -9223372036854775809, .type = types_mod.Num.Int.Precision.i64, .should_fit = false },
        .{ .value = -170141183460469231731687303715884105728, .type = types_mod.Num.Int.Precision.i128, .should_fit = true },
    };

    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var problems = problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var unify_scratch = unifier.Scratch.init(gpa);
    defer unify_scratch.deinit();

    var occurs_scratch = occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    inline for (test_cases) |tc| {
        // Calculate the magnitude
        const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

        // Apply the branchless adjustment for minimum signed values
        const is_negative = @as(u1, @intFromBool(tc.value < 0));
        const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
        const is_minimum_signed = is_negative & is_power_of_2;
        const adjusted_val = u128_val - is_minimum_signed;

        // Create requirements based on adjusted value
        const requirements = types_mod.Num.IntRequirements{
            .sign_needed = tc.value < 0,
            .bits_needed = @intFromEnum(types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val)),
        };

        const literal_var = module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_unbound = requirements } } });
        const type_var = module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_compact = .{ .int = tc.type } } } });

        const result = unifier.unify(
            &module_env,
            &module_env.types,
            &problems,
            &snapshots,
            &unify_scratch,
            &occurs_scratch,
            literal_var,
            type_var,
        );

        if (tc.should_fit) {
            try std.testing.expect(result == .ok);
        } else {
            try std.testing.expect(result == .problem);
        }
    }
}

test "minimum signed values have correct bits_needed" {
    const test_cases = .{
        .{ .value = -128, .expected_bits = types_mod.Num.Int.BitsNeeded.@"7" },
        .{ .value = -129, .expected_bits = types_mod.Num.Int.BitsNeeded.@"8" },
        .{ .value = -32768, .expected_bits = types_mod.Num.Int.BitsNeeded.@"9_to_15" },
        .{ .value = -32769, .expected_bits = types_mod.Num.Int.BitsNeeded.@"16" },
        .{ .value = -2147483648, .expected_bits = types_mod.Num.Int.BitsNeeded.@"17_to_31" },
        .{ .value = -2147483649, .expected_bits = types_mod.Num.Int.BitsNeeded.@"32" },
        .{ .value = -9223372036854775808, .expected_bits = types_mod.Num.Int.BitsNeeded.@"33_to_63" },
        .{ .value = -9223372036854775809, .expected_bits = types_mod.Num.Int.BitsNeeded.@"64" },
        .{ .value = -170141183460469231731687303715884105728, .expected_bits = types_mod.Num.Int.BitsNeeded.@"65_to_127" },
    };

    inline for (test_cases) |tc| {
        // Calculate the magnitude
        const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

        // Apply the branchless adjustment for minimum signed values
        const is_negative = @as(u1, if (tc.value < 0) 1 else 0);
        const is_power_of_2 = @as(u1, if (u128_val != 0 and (u128_val & (u128_val - 1)) == 0) 1 else 0);
        const is_minimum_signed = is_negative & is_power_of_2;
        const adjusted_val = u128_val - is_minimum_signed;

        const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
        try std.testing.expectEqual(tc.expected_bits, bits_needed);
    }
}

test "branchless minimum signed value detection" {
    const test_cases = .{
        // Minimum signed values (negative powers of 2)
        .{ .value = -1, .is_minimum = true }, // magnitude 1 = 2^0
        .{ .value = -2, .is_minimum = true }, // magnitude 2 = 2^1
        .{ .value = -4, .is_minimum = true }, // magnitude 4 = 2^2
        .{ .value = -8, .is_minimum = true }, // magnitude 8 = 2^3
        .{ .value = -16, .is_minimum = true }, // magnitude 16 = 2^4
        .{ .value = -32, .is_minimum = true }, // magnitude 32 = 2^5
        .{ .value = -64, .is_minimum = true }, // magnitude 64 = 2^6
        .{ .value = -128, .is_minimum = true }, // magnitude 128 = 2^7
        .{ .value = -256, .is_minimum = true }, // magnitude 256 = 2^8
        .{ .value = -32768, .is_minimum = true }, // magnitude 32768 = 2^15
        .{ .value = -2147483648, .is_minimum = true }, // magnitude 2^31

        // Not minimum signed values
        .{ .value = 128, .is_minimum = false }, // positive
        .{ .value = -3, .is_minimum = false }, // magnitude 3 (not power of 2)
        .{ .value = -5, .is_minimum = false }, // magnitude 5 (not power of 2)
        .{ .value = -127, .is_minimum = false }, // magnitude 127 (not power of 2)
        .{ .value = -129, .is_minimum = false }, // magnitude 129 (not power of 2)
        .{ .value = -130, .is_minimum = false }, // magnitude 130 (not power of 2)
        .{ .value = 0, .is_minimum = false }, // zero
    };

    inline for (test_cases) |tc| {
        const value: i128 = tc.value;
        const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

        const is_negative = @as(u1, @intFromBool(value < 0));
        const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
        const is_minimum_signed = is_negative & is_power_of_2;

        const expected: u1 = @intFromBool(tc.is_minimum);
        try std.testing.expectEqual(expected, is_minimum_signed);
    }
}

test "verify -128 produces 7 bits needed" {
    const value: i128 = -128;
    const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

    // Check intermediate values
    try std.testing.expectEqual(@as(u128, 128), u128_val);

    const is_negative = @as(u1, @intFromBool(value < 0));
    const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
    const is_minimum_signed = is_negative & is_power_of_2;

    try std.testing.expectEqual(@as(u1, 1), is_negative);
    try std.testing.expectEqual(@as(u1, 1), is_power_of_2);
    try std.testing.expectEqual(@as(u1, 1), is_minimum_signed);

    const adjusted_val = u128_val - is_minimum_signed;
    try std.testing.expectEqual(@as(u128, 127), adjusted_val);

    // Test that 127 maps to 7 bits
    const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
    try std.testing.expectEqual(types_mod.Num.Int.BitsNeeded.@"7", bits_needed);
    try std.testing.expectEqual(@as(u8, 7), bits_needed.toBits());
}

/// Check the types for the provided pattern
pub fn checkPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    _ = self;
    _ = pattern_idx;
}

test "lambda with record field access infers correct type" {
    // The lambda |x, y| { x: x, y: y }.x should have type a, b -> a
    // And when annotated as I32, I32 -> I32, it should unify correctly.
    // This is a regression test against a bug that previously existed in that scenario.
    const gpa = std.testing.allocator;

    // Create a minimal environment for testing
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env, "Test");
    defer cir.deinit();

    const empty_modules: []const *CIR = &.{};
    var solver = try Self.init(gpa, &module_env.types, &cir, empty_modules, &cir.store.regions);
    defer solver.deinit();

    // Create type variables for the lambda parameters
    const param_x_var = module_env.types.fresh();
    const param_y_var = module_env.types.fresh();

    // Create a record with fields x and y
    var record_fields = std.ArrayList(types_mod.RecordField).init(gpa);
    defer record_fields.deinit();

    const x_ident = module_env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = module_env.idents.insert(gpa, base.Ident.for_text("y"), base.Region.zero());

    try record_fields.append(.{ .name = x_ident, .var_ = param_x_var });
    try record_fields.append(.{ .name = y_ident, .var_ = param_y_var });

    const fields_range = module_env.types.appendRecordFields(record_fields.items);
    const ext_var = module_env.types.fresh();
    const record_content = types_mod.Content{
        .structure = .{
            .record = .{
                .fields = fields_range,
                .ext = ext_var,
            },
        },
    };
    _ = module_env.types.freshFromContent(record_content);

    // Simulate field access: record.x
    // The result type should unify with param_x_var
    const field_access_var = module_env.types.fresh();
    _ = solver.unify(field_access_var, param_x_var);

    // Create the lambda type: param_x, param_y -> field_access_result
    const lambda_content = module_env.types.mkFuncUnbound(&[_]types_mod.Var{ param_x_var, param_y_var }, field_access_var);
    const lambda_var = module_env.types.freshFromContent(lambda_content);

    // The lambda should have type a, b -> a (param_x and return type are unified)
    const resolved_lambda = module_env.types.resolveVar(lambda_var);
    try std.testing.expect(resolved_lambda.desc.content == .structure);
    try std.testing.expect(resolved_lambda.desc.content.structure == .fn_unbound);

    const func = resolved_lambda.desc.content.structure.fn_unbound;
    const args = module_env.types.getFuncArgsSlice(func.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);

    // Verify that first parameter and return type resolve to the same variable
    const first_param_resolved = module_env.types.resolveVar(args[0]);
    const return_resolved = module_env.types.resolveVar(func.ret);
    try std.testing.expectEqual(first_param_resolved.var_, return_resolved.var_);

    // Now test with annotation: I32, I32 -> I32
    const i32_content = types_mod.Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_var1 = module_env.types.freshFromContent(i32_content);
    const i32_var2 = module_env.types.freshFromContent(i32_content);
    const i32_var3 = module_env.types.freshFromContent(i32_content);

    const annotated_func = module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var1, i32_var2 }, i32_var3);
    const annotation_var = module_env.types.freshFromContent(annotated_func);

    // Unify the lambda with its annotation
    const unify_result = solver.unify(lambda_var, annotation_var);
    try std.testing.expect(unify_result == .ok);

    // Verify the lambda now has the concrete type
    const final_resolved = module_env.types.resolveVar(lambda_var);
    try std.testing.expect(final_resolved.desc.content == .structure);
    try std.testing.expect(final_resolved.desc.content.structure == .fn_pure);

    // Test call site: when calling with integer literals
    const num_unbound = types_mod.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
    const lit1_var = module_env.types.freshFromContent(num_unbound);
    const lit2_var = module_env.types.freshFromContent(num_unbound);
    const call_result_var = module_env.types.fresh();

    const expected_func_content = module_env.types.mkFuncUnbound(&[_]types_mod.Var{ lit1_var, lit2_var }, call_result_var);
    const expected_func_var = module_env.types.freshFromContent(expected_func_content);

    // The critical fix: unify expected with actual (not the other way around)
    const call_unify_result = solver.unify(expected_func_var, lambda_var);
    try std.testing.expect(call_unify_result == .ok);

    // Verify the literals got constrained to I32
    const lit1_resolved = module_env.types.resolveVar(lit1_var);
    try std.testing.expect(lit1_resolved.desc.content == .structure);
    try std.testing.expect(lit1_resolved.desc.content.structure == .num);
    try std.testing.expect(lit1_resolved.desc.content.structure.num == .int_precision);
    try std.testing.expect(lit1_resolved.desc.content.structure.num.int_precision == .i32);
}

test "dot access properly unifies field types with parameters" {
    // This test verifies that e_dot_access correctly handles field access
    // and unifies the field type with the expression result type.

    const gpa = std.testing.allocator;

    // Create a minimal environment for testing
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env, "Test");
    defer cir.deinit();

    const empty_modules: []const *CIR = &.{};
    var solver = try Self.init(gpa, &module_env.types, &cir, empty_modules, &cir.store.regions);
    defer solver.deinit();

    // Create a parameter type variable
    const param_var = module_env.types.fresh();

    // Create a record with field "x" of the same type as the parameter
    var record_fields = std.ArrayList(types_mod.RecordField).init(gpa);
    defer record_fields.deinit();

    const x_ident = module_env.idents.insert(gpa, base.Ident.for_text("x"), base.Region.zero());
    try record_fields.append(.{ .name = x_ident, .var_ = param_var });

    const fields_range = module_env.types.appendRecordFields(record_fields.items);
    const ext_var = module_env.types.fresh();
    const record_content = types_mod.Content{
        .structure = .{
            .record = .{
                .fields = fields_range,
                .ext = ext_var,
            },
        },
    };
    const record_var = module_env.types.freshFromContent(record_content);

    // Create a dot access result variable
    const dot_access_var = module_env.types.fresh();

    // Simulate the dot access logic from checkExpr
    const resolved_record = module_env.types.resolveVar(record_var);
    try std.testing.expect(resolved_record.desc.content == .structure);
    try std.testing.expect(resolved_record.desc.content.structure == .record);

    const record = resolved_record.desc.content.structure.record;
    const fields = module_env.types.getRecordFieldsSlice(record.fields);

    // Find field "x" and unify with dot access result
    var found_field = false;
    for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
        if (field_name == x_ident) {
            _ = solver.unify(dot_access_var, field_var);
            found_field = true;
            break;
        }
    }
    try std.testing.expect(found_field);

    // Verify that dot_access_var and param_var now resolve to the same variable
    const dot_resolved = module_env.types.resolveVar(dot_access_var);
    const param_resolved = module_env.types.resolveVar(param_var);
    try std.testing.expectEqual(dot_resolved.var_, param_resolved.var_);

    // Test with unbound record
    const unbound_record_content = types_mod.Content{
        .structure = .{
            .record_unbound = fields_range,
        },
    };
    const unbound_record_var = module_env.types.freshFromContent(unbound_record_content);
    const dot_access_var2 = module_env.types.fresh();

    // Same test with record_unbound
    const resolved_unbound = module_env.types.resolveVar(unbound_record_var);
    try std.testing.expect(resolved_unbound.desc.content == .structure);
    try std.testing.expect(resolved_unbound.desc.content.structure == .record_unbound);

    const unbound_record = resolved_unbound.desc.content.structure.record_unbound;
    const unbound_fields = module_env.types.getRecordFieldsSlice(unbound_record);

    found_field = false;
    for (unbound_fields.items(.name), unbound_fields.items(.var_)) |field_name, field_var| {
        if (field_name == x_ident) {
            _ = solver.unify(dot_access_var2, field_var);
            found_field = true;
            break;
        }
    }
    try std.testing.expect(found_field);

    // Verify unification worked
    const dot2_resolved = module_env.types.resolveVar(dot_access_var2);
    try std.testing.expectEqual(dot2_resolved.var_, param_resolved.var_);
}

test "call site unification order matters for concrete vs flexible types" {
    // This test verifies that unification order matters when dealing with
    // concrete types (like I32) and flexible types (like Num(*)).
    //
    // At call sites, we must unify in the correct order:
    // - unify(flexible, concrete) ✓ succeeds - flexible types can be constrained
    // - unify(concrete, flexible) ✗ fails - concrete types cannot become more general
    //
    // The test demonstrates the complete type checking scenario:
    // 1. Unification order matters (concrete→flexible fails, flexible→concrete succeeds)
    // 2. When unifying function types in the correct order, the flexible argument
    //    types are properly constrained to match the concrete parameter types
    // 3. Numeric arguments start as flexible num_unbound types
    // 4. After unification, they become concrete I32 types
    const gpa = std.testing.allocator;

    // Create a minimal environment for testing
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env, "Test");
    defer cir.deinit();

    const empty_modules: []const *CIR = &.{};
    var solver = try Self.init(gpa, &module_env.types, &cir, empty_modules, &cir.store.regions);
    defer solver.deinit();

    // First, verify basic number unification works as expected
    const i32_content = types_mod.Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
    const i32_test = module_env.types.freshFromContent(i32_content);
    const num_unbound = types_mod.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
    const flex_test = module_env.types.freshFromContent(num_unbound);

    // Flexible number should unify with concrete I32 and become I32
    const basic_result = solver.unify(flex_test, i32_test);
    try std.testing.expect(basic_result == .ok);

    // Verify the flexible variable was constrained to I32
    const flex_resolved = module_env.types.resolveVar(flex_test);
    switch (flex_resolved.desc.content) {
        .structure => |s| switch (s) {
            .num => |n| switch (n) {
                .int_precision => |prec| {
                    try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
                },
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }

    // Now test with function types
    // Create a concrete function type: I32, I32 -> I32
    const i32_var1 = module_env.types.freshFromContent(i32_content);
    const i32_var2 = module_env.types.freshFromContent(i32_content);
    const i32_var3 = module_env.types.freshFromContent(i32_content);

    const concrete_func = module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var1, i32_var2 }, i32_var3);
    const concrete_func_var = module_env.types.freshFromContent(concrete_func);

    // Create flexible argument types (like integer literals)
    const arg1_var = module_env.types.freshFromContent(num_unbound);
    const arg2_var = module_env.types.freshFromContent(num_unbound);
    const result_var = module_env.types.fresh();

    // Create expected function type from arguments
    const expected_func = module_env.types.mkFuncUnbound(&[_]types_mod.Var{ arg1_var, arg2_var }, result_var);
    const expected_func_var = module_env.types.freshFromContent(expected_func);

    // The wrong order: unify(concrete, expected) would fail
    // because I32 can't unify with Num(*)
    const wrong_order_result = solver.unify(concrete_func_var, expected_func_var);
    try std.testing.expect(wrong_order_result == .problem);

    // After failed unification, both variables become error types
    const concrete_after_fail = module_env.types.resolveVar(concrete_func_var);
    const expected_after_fail = module_env.types.resolveVar(expected_func_var);
    try std.testing.expectEqual(types_mod.Content.err, concrete_after_fail.desc.content);
    try std.testing.expectEqual(types_mod.Content.err, expected_after_fail.desc.content);

    // Now simulate a complete type checking scenario for a function call
    // This is what happens when type checking code like: myFunc(1, 2)
    // where myFunc : I32, I32 -> I32

    // Step 1: Create the known function type (I32, I32 -> I32)
    const i32_var4 = module_env.types.freshFromContent(i32_content);
    const i32_var5 = module_env.types.freshFromContent(i32_content);
    const i32_var6 = module_env.types.freshFromContent(i32_content);
    const known_func = module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var4, i32_var5 }, i32_var6);
    const known_func_var = module_env.types.freshFromContent(known_func);

    // Step 2: Create flexible argument types (representing literals like 1 and 2)
    const call_arg1 = module_env.types.freshFromContent(num_unbound);
    const call_arg2 = module_env.types.freshFromContent(num_unbound);

    // Verify the arguments start as flexible num_unbound types
    const arg1_before = module_env.types.resolveVar(call_arg1);
    const arg2_before = module_env.types.resolveVar(call_arg2);

    switch (arg1_before.desc.content) {
        .structure => |s| switch (s) {
            .num => |n| switch (n) {
                .num_unbound => {}, // Expected
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }

    switch (arg2_before.desc.content) {
        .structure => |s| switch (s) {
            .num => |n| switch (n) {
                .num_unbound => {}, // Expected
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }

    // Step 3: Create the expected function type from the call site
    // This represents the type we expect based on the arguments
    const call_result = module_env.types.fresh();
    const call_func = module_env.types.mkFuncUnbound(&[_]types_mod.Var{ call_arg1, call_arg2 }, call_result);
    const call_func_var = module_env.types.freshFromContent(call_func);

    // Step 4: Unify the expected type with the known type
    // This is the key step - unify(expected, known) in the correct order
    const unify_result = solver.unify(call_func_var, known_func_var);
    try std.testing.expect(unify_result == .ok);

    // Step 5: Verify that the call arguments were constrained to I32
    // This simulates what happens in real type checking - the argument
    // variables used at the call site get constrained by the function type

    // Step 6: Verify that both arguments are now constrained to I32
    for ([_]types_mod.Var{ call_arg1, call_arg2 }) |arg| {
        const arg_resolved = module_env.types.resolveVar(arg);
        switch (arg_resolved.desc.content) {
            .structure => |s| switch (s) {
                .num => |n| switch (n) {
                    .int_precision => |prec| {
                        try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
                    },
                    .num_compact => |compact| switch (compact) {
                        .int => |prec| {
                            try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
                        },
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        }
    }
}

test {
    _ = @import("check_types/cross_module_test.zig");
}
