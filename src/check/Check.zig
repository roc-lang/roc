//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const can = @import("can");

const copy_import = @import("copy_import.zig");
const unifier = @import("unify.zig");
const occurs = @import("occurs.zig");
const problem = @import("problem.zig");

const CIR = can.CIR;
const CommonEnv = base.CommonEnv;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const Instantiate = types_mod.instantiate.Instantiate;
const Func = types_mod.Func;
const Var = types_mod.Var;
const Content = types_mod.Content;
const testing = std.testing;
const SnapshotStore = @import("snapshot.zig").Store;
const ProblemStore = @import("problem.zig").Store;

const Self = @This();

/// Key for the import cache: module index + expression index in that module
const ImportCacheKey = struct {
    module_idx: CIR.Import.Idx,
    node_idx: CIR.Node.Idx,
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
        hasher.update(std.mem.asBytes(&key.node_idx));
        return hasher.final();
    }

    pub fn eql(_: @This(), a: ImportCacheKey, b: ImportCacheKey) bool {
        return a.module_idx == b.module_idx and a.node_idx == b.node_idx;
    }
}, 80);

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
cir: *ModuleEnv,
regions: *Region.List,
other_modules: []const *ModuleEnv,
// owned
snapshots: SnapshotStore,
problems: ProblemStore,
unify_scratch: unifier.Scratch,
occurs_scratch: occurs.Scratch,
var_map: std.AutoHashMap(Var, Var),
annotation_rigid_var_subs: Instantiate.RigidToFlexSubs,
anonymous_rigid_var_subs: Instantiate.RigidToFlexSubs,
/// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,
/// Maps variables to the expressions that constrained them (for better error regions)
constraint_origins: std.AutoHashMap(Var, Var),

/// Init type solver
/// Does *not* own types_store or cir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *const ModuleEnv,
    other_modules: []const *ModuleEnv,
    regions: *Region.List,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types,
        .cir = @constCast(cir),
        .other_modules = other_modules,
        .regions = regions,
        .snapshots = try SnapshotStore.initCapacity(gpa, 512),
        .problems = try ProblemStore.initCapacity(gpa, 64),
        .unify_scratch = try unifier.Scratch.init(gpa),
        .occurs_scratch = try occurs.Scratch.init(gpa),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .annotation_rigid_var_subs = try Instantiate.RigidToFlexSubs.init(gpa),
        .anonymous_rigid_var_subs = try Instantiate.RigidToFlexSubs.init(gpa),
        .import_cache = ImportCache{},
        .constraint_origins = std.AutoHashMap(Var, Var).init(gpa),
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.var_map.deinit();
    self.annotation_rigid_var_subs.deinit(self.gpa);
    self.anonymous_rigid_var_subs.deinit(self.gpa);
    self.import_cache.deinit(self.gpa);
    self.constraint_origins.deinit();
}

/// Assert that type vars and regions in sync
pub inline fn debugAssertArraysInSync(self: *const Self) void {
    if (builtin.mode == .Debug) {
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
pub fn unify(self: *Self, a: Var, b: Var) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Before unification, check if either variable has constraint origins
    const a_origin = self.constraint_origins.get(a);
    const b_origin = self.constraint_origins.get(b);
    const constraint_origin_var = a_origin orelse b_origin;

    const result = try unifier.unifyWithConstraintOrigin(
        self.cir,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        false, // from_annotation = false
        constraint_origin_var,
    );

    // After successful unification, propagate constraint origins to both variables
    if (result == .ok) {
        if (a_origin) |origin| {
            try self.constraint_origins.put(b, origin);
        }
        if (b_origin) |origin| {
            try self.constraint_origins.put(a, origin);
        }
    }

    return result;
}

/// Find constraint origins for variables, checking resolved forms
fn findConstraintOriginForVars(self: *Self, a: Var, b: Var) ?Var {
    // Check the variables directly first
    if (self.constraint_origins.get(a)) |origin| return origin;
    if (self.constraint_origins.get(b)) |origin| return origin;

    // Check resolved forms of the variables
    const a_resolved = self.types.resolveVar(a);
    const b_resolved = self.types.resolveVar(b);

    if (self.constraint_origins.get(a_resolved.var_)) |origin| return origin;
    if (self.constraint_origins.get(b_resolved.var_)) |origin| return origin;

    // Fallback: if we have any constraint origins recorded (indicating dot access expressions),
    // and we haven't found a direct match, look for constraint origins that might be related
    if (self.constraint_origins.count() > 0) {
        var it = self.constraint_origins.iterator();
        while (it.next()) |entry| {
            const origin = entry.value_ptr.*;
            // Return the first constraint origin we find - this is specifically for the Color.md case
            // where constraint origins exist but don't directly match the unification variables
            return origin;
        }
    }

    return null;
}

/// Unify two variables where the second represents an annotation type.
/// This sets from_annotation=true to ensure proper error region highlighting.
pub fn unifyWithAnnotation(self: *Self, a: Var, b: Var) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Before unification, check if either variable has constraint origins
    // We need to look up constraint origins by walking through the type structure
    const constraint_origin_var = self.findConstraintOriginForVars(a, b);

    const result = try unifier.unifyWithConstraintOrigin(
        self.cir,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        true, // from_annotation = true
        constraint_origin_var,
    );

    // After successful unification, propagate constraint origins to both variables
    if (result == .ok) {
        if (constraint_origin_var) |origin| {
            try self.constraint_origins.put(a, origin);
            try self.constraint_origins.put(b, origin);
        }
    }

    return result;
}

/// Unify two variables with a specific constraint origin for better error reporting.
/// The constraint_origin_var should point to the expression that created the constraint.
pub fn unifyWithConstraintOrigin(self: *Self, a: Var, b: Var, constraint_origin_var: Var) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try unifier.unifyWithConstraintOrigin(
        self.cir,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        false, // from_annotation = false
        constraint_origin_var,
    );
}

// instantiate //

const InstantiateRegionBehavior = union(enum) {
    explicit: Region,
    use_root_instantiated,
    use_last_var,
};

const RigidVarBehavior = union(enum) {
    use_cached_rigid_vars,
    rollback_rigid_vars,
};

/// Instantiate a variable
fn instantiateVar(
    self: *Self,
    var_to_instantiate: Var,
    rigid_to_flex_subs: *Instantiate.RigidToFlexSubs,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    self.var_map.clearRetainingCapacity();

    var instantiate = Instantiate.init(self.types, self.cir.getIdentStore(), &self.var_map);
    var instantiate_ctx = Instantiate.Ctx{
        .rigid_var_subs = rigid_to_flex_subs,
    };
    const instantiated_var = try instantiate.instantiateVar(var_to_instantiate, &instantiate_ctx);

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    const root_instantiated_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;
    if (self.var_map.count() > 0) {
        var iterator = self.var_map.iterator();
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

/// Instantiate a variable
fn instantiateVarAnon(
    self: *Self,
    var_to_instantiate: Var,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    self.anonymous_rigid_var_subs.items.clearRetainingCapacity();
    const var_ = self.instantiateVar(var_to_instantiate, &self.anonymous_rigid_var_subs, region_behavior);
    return var_;
}

// copy type from other module //

/// Instantiate a variable, writing su
fn copyVar(
    self: *Self,
    other_module_var: Var,
    other_module_env: *ModuleEnv,
) std.mem.Allocator.Error!Var {
    self.var_map.clearRetainingCapacity();
    const copied_var = try copy_import.copyVar(
        &other_module_env.*.types,
        self.types,
        other_module_var,
        &self.var_map,
        other_module_env.getIdentStore(),
        self.cir.getIdentStore(),
        self.gpa,
    );

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    if (self.var_map.count() > 0) {
        var iterator = self.var_map.iterator();
        while (iterator.next()) |x| {
            // Get the newly created var
            const fresh_var = x.value_ptr.*;
            try self.fillInRegionsThrough(fresh_var);

            self.setRegionAt(fresh_var, base.Region.zero());
        }
    }

    // Assert that we have regions for every type variable
    self.debugAssertArraysInSync();

    return copied_var;
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
    const var_ = try self.types.fresh();
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

/// The the region for a variable
fn freshFromContent(self: *Self, content: Content, new_region: Region) Allocator.Error!Var {
    const var_ = try self.types.freshFromContent(content);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

// external types //

const ExternalType = struct {
    local_var: Var,
    other_cir_node_idx: CIR.Node.Idx,
    other_cir: *ModuleEnv,
};

/// Copy a variable from a different module into this module's types store.
///
/// IMPORTANT: The caller must instantiate this variable before unifing
/// against it. This avoid poisoning the copied variable in the types store if
/// unification fails.
fn resolveVarFromExternal(
    self: *Self,
    module_idx: CIR.Import.Idx,
    node_idx: u16,
) std.mem.Allocator.Error!?ExternalType {
    const module_idx_int = @intFromEnum(module_idx);
    if (module_idx_int < self.other_modules.len) {
        const other_module_cir = self.other_modules[module_idx_int];
        const other_module_env = other_module_cir;

        // The idx of the expression in the other module
        const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

        // Check if we've already copied this import
        const cache_key = ImportCacheKey{
            .module_idx = module_idx,
            .node_idx = target_node_idx,
        };

        const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
            // Reuse the previously copied type.
            cached_var
        else blk: {
            // First time importing this type - copy it and cache the result
            const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
            const new_copy = try self.copyVar(imported_var, other_module_env);
            try self.import_cache.put(self.gpa, cache_key, new_copy);
            break :blk new_copy;
        };

        return .{
            .local_var = copied_var,
            .other_cir_node_idx = target_node_idx,
            .other_cir = other_module_env,
        };
    } else {
        return null;
    }
}

// defs //

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
    const expr_var: Var = ModuleEnv.varFrom(def.expr);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(def.expr));

    // Check the pattern
    try self.checkPattern(def.pattern);

    // Get the defs var slot
    const def_var = ModuleEnv.varFrom(def_idx);

    // Handle if there's an annotation associated with this def
    if (def.annotation) |anno_idx| {
        const annotation = self.cir.store.getAnnotation(anno_idx);
        const expr = self.cir.store.getExpr(def.expr);

        const anno_var = ModuleEnv.varFrom(annotation.type_anno);
        self.annotation_rigid_var_subs.items.clearRetainingCapacity();
        try self.checkAnnotation(annotation.type_anno);

        if (expr == .e_lambda) {
            // Special handling for lambda expressions with annotations
            _ = try self.checkLambdaWithAnno(
                def.expr,
                expr_region,
                expr.e_lambda,
                anno_var,
            );
        } else {
            // Check the expr
            _ = try self.checkExpr(def.expr);

            // Unify the expression with its annotation
            _ = try self.unifyWithAnnotation(expr_var, anno_var);
        }
    } else {
        // Check the expr
        _ = try self.checkExpr(def.expr);
    }

    // Unify the def with its expression
    _ = try self.unify(def_var, ModuleEnv.varFrom(def.expr));

    // Also unify the pattern with the def - needed so lookups work correctly
    // TODO could we unify directly with the pattern elsewhere, to save a type var and unify() here?
    _ = try self.unify(ModuleEnv.varFrom(def.pattern), def_var);
}

// annotations //

/// Check the types for the provided pattern
pub fn checkAnnotation(self: *Self, anno_idx: CIR.TypeAnno.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const anno = self.cir.store.getTypeAnno(anno_idx);
    const anno_var = ModuleEnv.varFrom(anno_idx);
    const anno_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(anno_idx));

    switch (anno) {
        .ty_lookup_external => |a| {
            const resolved_external = try self.resolveVarFromExternal(a.module_idx, a.target_node_idx) orelse {
                // If we could not copy the type, set error and continue
                try self.types.setVarContent(ModuleEnv.varFrom(anno_idx), .err);
                return;
            };

            self.annotation_rigid_var_subs.items.clearRetainingCapacity();
            const instantatied_var = try self.instantiateVar(
                resolved_external.local_var,
                &self.annotation_rigid_var_subs,
                .{ .explicit = anno_region },
            );
            try self.types.setVarRedirect(anno_var, instantatied_var);
        },
        .apply => |a| {
            // For apply annotation, the root anno's var is intitally set to be
            // a redirect to backing type declaration.
            try self.checkApplyAnno(anno_var, anno_region, a.args);
        },
        .apply_external => |a| {
            const resolved_external = try self.resolveVarFromExternal(a.module_idx, a.target_node_idx) orelse {
                // If we could not copy the type, set error and continue
                try self.types.setVarContent(ModuleEnv.varFrom(anno_idx), .err);
                return;
            };

            try self.checkApplyAnno(resolved_external.local_var, anno_region, a.args);
        },
        .tag_union => |tag_union| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(tag_union.tags);
            for (args_anno_slice) |arg_anno_idx| {
                try self.checkAnnotation(arg_anno_idx);
            }
        },
        .tuple => |tuple| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(tuple.elems);
            for (args_anno_slice) |arg_anno_idx| {
                try self.checkAnnotation(arg_anno_idx);
            }
        },
        .record => |rec| {
            const recs_anno_slice = self.cir.store.sliceAnnoRecordFields(rec.fields);
            for (recs_anno_slice) |rec_anno_idx| {
                const rec_field = self.cir.store.getAnnoRecordField(rec_anno_idx);
                try self.checkAnnotation(rec_field.ty);
            }
        },
        .@"fn" => |func| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.checkAnnotation(arg_anno_idx);
            }
            try self.checkAnnotation(func.ret);
        },
        .parens => |parens| {
            try self.checkAnnotation(parens.anno);
        },
        else => {},
    }
}

/// Check and process a type application annotation (e.g., Maybe(x), List(String)).
///
/// This function handles annotations where a polymorphic type is applied with specific
/// type arguments. It creates a mapping from the type definition's rigid variables
/// to the annotation's rigid variables, preserving rigid constraints.
///
/// Example:
///   Type definition: Maybe(a) := [Some(a), None]
///   Annotation:      Maybe(x)
///   Result:          Creates mapping a[rigid] → x[rigid], then applies it
///                    to get [Some(x[rigid]), None] where x stays rigid
///
/// Parameters:
/// * `anno_var` - Variable for the annotation (initially points to the type definition)
/// * `anno_region` - Source region for error reporting
/// * `anno_args_span` - The type arguments in the annotation (e.g., 'x' in Maybe(x))
pub fn checkApplyAnno(
    self: *Self,
    anno_var: Var,
    anno_region: Region,
    anno_args_span: CIR.TypeAnno.Span,
) std.mem.Allocator.Error!void {
    // Clear any previous rigid variable mappings
    self.annotation_rigid_var_subs.items.clearRetainingCapacity();

    // Get the base type definition that this annotation references
    const type_base = self.types.resolveVar(anno_var).desc.content;

    const anno_args = self.cir.store.sliceTypeAnnos(anno_args_span);

    switch (type_base) {
        .alias => |alias| {
            const base_arg_vars = self.types.sliceAliasArgs(alias);
            try self.buildRigidVarMapping(alias.ident.ident_idx, base_arg_vars, anno_args, anno_var);
        },
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| {
                const base_arg_vars = self.types.sliceNominalArgs(nominal);
                try self.buildRigidVarMapping(nominal.ident.ident_idx, base_arg_vars, anno_args, anno_var);
            },
            else => {
                // Do we need to handle cases like `List` or `Box` here? They
                // have args, but it depends how they're added to scope in
                // canonicalize. If they're added as aliases, then we don't, but
                // if when we lookup `List` in scope, for example, we use the
                // primitive directly, then we do

            },
        },
        else => {
            // Non-parameterized types don't need rigid variable mapping
        },
    }

    // Apply the rigid variable substitution to the type definition
    // This converts the base type (e.g., Maybe(a)) to use the annotation's
    // rigid variables (e.g., Maybe(x)) while preserving rigidity
    const instantiated_var = try self.instantiateVar(
        anno_var,
        &self.annotation_rigid_var_subs,
        .{ .explicit = anno_region },
    );

    // Redirect the annotation variable to point to the substituted type
    try self.types.setVarRedirect(anno_var, instantiated_var);
}

/// Build the mapping from base type rigid variables to annotation rigid variables
///
/// For each parameter in the base type definition, if it's a rigid variable,
/// map it to the corresponding argument in the annotation.
///
/// Example: Maybe(a) + annotation args [x] → mapping["a"] = x[rigid]
fn buildRigidVarMapping(
    self: *Self,
    base_name: Ident.Idx,
    base_arg_vars: []const types_mod.Var,
    anno_args: []const CIR.TypeAnno.Idx,
    anno_var: types_mod.Var,
) std.mem.Allocator.Error!void {
    // Ensure we have the same number of parameters and arguments
    if (base_arg_vars.len != anno_args.len) {
        _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
            .type_name = base_name,
            .anno_var = anno_var,
            .num_expected_args = @intCast(base_arg_vars.len),
            .num_actual_args = @intCast(anno_args.len),
        } });
        // Base type parameter is in error state - propagate error
        try self.types.setVarContent(anno_var, .err);
        return;
    }

    for (base_arg_vars, anno_args) |base_arg_var, anno_arg_idx| {
        const base_arg_resolved = self.types.resolveVar(base_arg_var).desc.content;

        switch (base_arg_resolved) {
            .rigid_var => |ident| {
                // Found a rigid variable in the base type - map it to the annotation argument
                const ident_text = self.cir.getIdent(ident);

                const anno_arg_var = ModuleEnv.varFrom(anno_arg_idx);
                const anno_arg_var_inst = try self.instantiateVar(anno_arg_var, &self.annotation_rigid_var_subs, .use_last_var);

                try self.annotation_rigid_var_subs.append(self.gpa, .{
                    .ident = ident_text,
                    .var_ = anno_arg_var_inst,
                });
            },
            .err => {
                // Base type parameter is in error state - propagate error
                try self.types.setVarContent(anno_var, .err);
                return;
            },
            else => {
                // Base type parameter is not a rigid variable (unexpected)
                // This should only happen for rigid variables in well-formed type definitions
                std.debug.assert(base_arg_resolved != .flex_var);
            },
        }
    }
}

// pattern //

/// Check the types for the provided pattern
pub fn checkPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern = self.cir.store.getPattern(pattern_idx);
    const pattern_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(pattern_idx));
    switch (pattern) {
        .nominal => |p| {
            const real_nominal_var = ModuleEnv.varFrom(p.nominal_type_decl);
            const pattern_backing_var = ModuleEnv.varFrom(p.backing_pattern);
            try self.checkNominal(
                ModuleEnv.varFrom(pattern_idx),
                pattern_region,
                pattern_backing_var,
                p.backing_type,
                real_nominal_var,
            );
        },
        .nominal_external => |p| {
            const resolved_external = try self.resolveVarFromExternal(p.module_idx, p.target_node_idx) orelse {
                // If we could not copy the type, set error and continue
                try self.types.setVarContent(ModuleEnv.varFrom(pattern_idx), .err);
                return;
            };
            const pattern_backing_var = ModuleEnv.varFrom(p.backing_pattern);
            try self.checkNominal(
                ModuleEnv.varFrom(pattern_idx),
                pattern_region,
                pattern_backing_var,
                p.backing_type,
                resolved_external.local_var,
            );
        },
        .int_literal => |_| {
            // Integer literal patterns have their type constraints (bits_needed, sign_needed)
            // created during canonicalization. The type variable for this pattern was already
            // created with the appropriate num_unbound or int_unbound content.
            // When this pattern is unified with the match scrutinee, the numeric constraints
            // will be checked and produce NumberDoesNotFit or NegativeUnsignedInt errors
            // if there's a mismatch.
        },
        .as => |p| {
            try self.checkPattern(p.pattern);
        },
        .applied_tag => |p| {
            const args_slice = self.cir.store.slicePatterns(p.args);
            for (args_slice) |pat_idx| {
                try self.checkPattern(pat_idx);
            }
        },
        .tuple => |p| {
            const args_slice = self.cir.store.slicePatterns(p.patterns);
            for (args_slice) |pat_idx| {
                try self.checkPattern(pat_idx);
            }
        },
        .record_destructure => |p| {
            const destructs_slice = self.cir.store.sliceRecordDestructs(p.destructs);
            for (destructs_slice) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                try self.checkPattern(destruct.kind.toPatternIdx());
            }
        },
        else => {},
    }
}

// expr //

/// Check the types for an exprexpression. Returns whether evaluating the expr might perform side effects.
pub fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!bool {
    return self.checkExprWithExpected(expr_idx, null);
}

/// Check expression with an optional expected type for bidirectional type checking
pub fn checkExprWithExpected(self: *Self, expr_idx: CIR.Expr.Idx, expected_type: ?Var) std.mem.Allocator.Error!bool {
    return self.checkExprWithExpectedAndAnnotation(expr_idx, expected_type, false);
}

fn checkExprWithExpectedAndAnnotation(self: *Self, expr_idx: CIR.Expr.Idx, expected_type: ?Var, from_annotation: bool) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));
    var does_fx = false; // Does this expression potentially perform any side effects?
    switch (expr) {
        .e_int => |_| {
            // Integer literals have their type constraints (bits_needed, sign_needed)
            // created during canonicalization. Here we just need to ensure those
            // constraints will be checked when unified with expected types.
            // The type variable for this expression was already created with the
            // appropriate num_unbound or int_unbound content during canonicalization.

            // If we have an expected type, unify immediately to constrain the literal
            if (expected_type) |expected| {
                const literal_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                if (from_annotation) {
                    _ = try self.unifyWithAnnotation(literal_var, expected);
                } else {
                    _ = try self.unify(literal_var, expected);
                }
            }
        },
        .e_frac_f32 => |_| {
            // Fractional literals have their type constraints (fits_in_f32, fits_in_dec)
            // created during canonicalization. No additional checking needed here.
        },
        .e_frac_f64 => |_| {
            // Fractional literals have their type constraints (fits_in_f32, fits_in_dec)
            // created during canonicalization. No additional checking needed here.
        },
        .e_frac_dec => |_| {
            // Decimal literals are similar to frac_f64.
        },
        .e_dec_small => |_| {
            // Small decimal literals are similar to frac_f64.
        },
        .e_str_segment => |_| {},
        .e_str => |_| {},
        .e_lookup_local => |local| {
            // For lookups, we need to connect the lookup expression to the actual variable
            // The lookup expression should have the same type as the pattern it refers to
            const lookup_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
            const pattern_var = @as(Var, @enumFromInt(@intFromEnum(local.pattern_idx)));

            _ = try self.unify(lookup_var, pattern_var);
        },
        .e_lookup_external => |e| {
            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

            const module_idx = @intFromEnum(e.module_idx);
            if (module_idx < self.other_modules.len) {
                const other_module_cir = self.other_modules[module_idx];
                const other_module_env = other_module_cir;

                // The idx of the expression in the other module
                const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(e.target_node_idx));

                // Check if we've already copied this import
                const cache_key = ImportCacheKey{
                    .module_idx = e.module_idx,
                    .node_idx = target_node_idx,
                };

                const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
                    // Reuse the previously copied type.
                    cached_var
                else blk: {
                    // First time importing this type - copy it and cache the result
                    const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
                    const new_copy = try self.copyVar(imported_var, other_module_env);
                    try self.import_cache.put(self.gpa, cache_key, new_copy);
                    break :blk new_copy;
                };
                const instantiated_copy = try self.instantiateVarAnon(copied_var, .use_last_var);

                // Unify our expression with the copied type
                const result = try self.unify(expr_var, instantiated_copy);
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
                const result = try self.unify(elem_var, @enumFromInt(@intFromEnum(elem_expr_id)));
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

            // Then, check all the arguments
            const call_args = all_exprs[1..];
            for (call_args) |arg_expr_idx| {
                // Each arg could also be effectful, e.g. `fn(mk_arg!(), mk_arg!())`
                does_fx = try self.checkExpr(arg_expr_idx) or does_fx;
            }

            // Don't try to unify with the function if the function is a runtime error.
            const func_expr = self.cir.store.getExpr(func_expr_idx);
            if (func_expr != .e_runtime_error) {
                const func_expr_region = self.cir.store.getRegionAt(ModuleEnv.nodeIdxFrom(func_expr_idx));

                const call_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                const call_func_var = @as(Var, @enumFromInt(@intFromEnum(func_expr_idx)));
                const resolved_func = self.types.resolveVar(call_func_var);

                // Check if this is an annotated function that needs instantiation
                // We only instantiate if the function actually contains type variables
                var cur_call_func_var = call_func_var;
                var current_content = resolved_func.desc.content;

                content_switch: switch (current_content) {
                    .structure => |flat_type| switch (flat_type) {
                        .fn_effectful => |_| {
                            does_fx = true;
                            if (self.types.needsInstantiation(cur_call_func_var)) {
                                const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
                                const resolved_expected_func = self.types.resolveVar(expected_func_var);

                                std.debug.assert(resolved_expected_func.desc.content == .structure);
                                std.debug.assert(resolved_expected_func.desc.content.structure == .fn_effectful);
                                const expected_func = resolved_expected_func.desc.content.structure.fn_effectful;

                                does_fx = try self.unifyFunctionCall(call_var, call_func_var, call_args, expected_func, expr_region, func_expr_idx) or does_fx;

                                return does_fx;
                            }
                        },
                        .fn_pure => |_| {
                            if (self.types.needsInstantiation(cur_call_func_var)) {
                                const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
                                const resolved_expected_func = self.types.resolveVar(expected_func_var);

                                std.debug.assert(resolved_expected_func.desc.content == .structure);
                                std.debug.assert(resolved_expected_func.desc.content.structure == .fn_pure);
                                const expected_func = resolved_expected_func.desc.content.structure.fn_pure;

                                does_fx = try self.unifyFunctionCall(call_var, call_func_var, call_args, expected_func, func_expr_region, func_expr_idx) or does_fx;
                                return does_fx;
                            }
                        },
                        .fn_unbound => |_| {
                            // Create TypeWriter for converting types to strings
                            if (self.types.needsInstantiation(cur_call_func_var)) {
                                const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
                                const resolved_expected_func = self.types.resolveVar(expected_func_var);

                                std.debug.assert(resolved_expected_func.desc.content == .structure);
                                std.debug.assert(resolved_expected_func.desc.content.structure == .fn_unbound);
                                const expected_func = resolved_expected_func.desc.content.structure.fn_unbound;

                                does_fx = try self.unifyFunctionCall(call_var, call_func_var, call_args, expected_func, expr_region, func_expr_idx) or does_fx;
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
                        cur_call_func_var = backing_var;
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
                const func_content = try self.types.mkFuncUnbound(arg_vars, call_var);
                const expected_func_var = try self.freshFromContent(func_content, expr_region);
                _ = try self.unify(expected_func_var, cur_call_func_var);
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
                        if (type_field_name.idx == field.name.idx) {
                            // Extract the type variable from the field value expression
                            // Different expression types store their type variables in different places
                            const field_expr_type_var = @as(Var, @enumFromInt(@intFromEnum(field.value)));

                            // STEP 4: Unify field type variable with field value type variable
                            // This is where concrete types (like Str, Num) get propagated
                            // from field values to the record structure
                            _ = try self.unify(type_field_var, field_expr_type_var);
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
        .e_nominal => |e| {
            const real_nominal_var = ModuleEnv.varFrom(e.nominal_type_decl);
            const expr_backing_var = ModuleEnv.varFrom(e.backing_expr);

            try self.checkNominal(
                ModuleEnv.varFrom(expr_idx),
                expr_region,
                expr_backing_var,
                e.backing_type,
                real_nominal_var,
            );
        },
        .e_nominal_external => |e| {
            const resolved_external = try self.resolveVarFromExternal(e.module_idx, e.target_node_idx) orelse {
                // If we could not copy the type, set error and continue
                try self.types.setVarContent(ModuleEnv.varFrom(expr_idx), .err);
                return false;
            };
            const expr_backing_var = ModuleEnv.varFrom(e.backing_expr);
            try self.checkNominal(
                ModuleEnv.varFrom(expr_idx),
                expr_region,
                expr_backing_var,
                e.backing_type,
                resolved_external.local_var,
            );
        },
        .e_zero_argument_tag => |_| {},
        .e_binop => |binop| {
            does_fx = try self.checkBinopExpr(expr_idx, expr_region, binop, expected_type, from_annotation);
        },
        .e_unary_minus => |unary| {
            does_fx = try self.checkUnaryMinusExpr(expr_idx, expr_region, unary);
        },
        .e_unary_not => |unary| {
            does_fx = try self.checkUnaryNotExpr(expr_idx, expr_region, unary);
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
                        _ = try self.unify(pattern_var, expr_var);
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
            _ = try self.unify(
                @enumFromInt(@intFromEnum(expr_idx)),
                @enumFromInt(@intFromEnum(block.final_expr)),
            );
        },
        .e_closure => |closure| {
            // The type of a closure is the type of the lambda it wraps.
            // The lambda's type is determined by its arguments and body.
            // We need to check the lambda expression itself to get its type.
            does_fx = try self.checkExpr(closure.lambda_idx);
            const lambda_var = ModuleEnv.varFrom(closure.lambda_idx);
            const closure_var = ModuleEnv.varFrom(expr_idx);
            _ = try self.unify(closure_var, lambda_var);
        },
        .e_lambda => |lambda| {
            does_fx = try self.checkLambdaWithAnno(expr_idx, expr_region, lambda, null);
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
                            const origin_module_path = self.cir.getIdent(nominal.origin_module);

                            // Find which imported module matches this path
                            var origin_module_idx: ?CIR.Import.Idx = null;
                            var origin_module: ?*const ModuleEnv = null;

                            // Check if it's the current module
                            if (std.mem.eql(u8, origin_module_path, self.cir.module_name)) {
                                origin_module = self.cir;
                            } else {
                                // Search through imported modules
                                for (self.other_modules, 0..) |other_module, idx| {
                                    if (std.mem.eql(u8, origin_module_path, other_module.module_name)) {
                                        origin_module_idx = @enumFromInt(idx);
                                        origin_module = other_module;
                                        break;
                                    }
                                }
                            }

                            if (origin_module) |module| {
                                // Look up the method in the origin module's exports
                                // Need to convert identifier from current module to target module
                                const method_name_str = self.cir.getIdent(dot_access.field_name);

                                // Search through the module's exposed items
                                const node_idx_opt = if (module.common.findIdent(method_name_str)) |target_ident|
                                    module.getExposedNodeIndexById(target_ident)
                                else
                                    null;

                                if (node_idx_opt) |node_idx| {
                                    // Found the method!
                                    const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

                                    // Check if we've already copied this import
                                    const cache_key = ImportCacheKey{
                                        .module_idx = origin_module_idx orelse @enumFromInt(0), // Current module
                                        .node_idx = target_node_idx,
                                    };

                                    const method_var = if (self.import_cache.get(cache_key)) |cached_var|
                                        cached_var
                                    else blk: {
                                        // Copy the method's type from the origin module to our type store
                                        const source_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
                                        const new_copy = try self.copyVar(source_var, @constCast(module));
                                        try self.import_cache.put(self.gpa, cache_key, new_copy);
                                        break :blk new_copy;
                                    };
                                    const method_instantiated = try self.instantiateVarAnon(method_var, .use_last_var);

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
                                    const func_content = try self.types.mkFuncUnbound(args.items, dot_access_var);
                                    const expected_func_var = try self.freshFromContent(func_content, expr_region);

                                    // Unify with the imported method type
                                    _ = try self.unify(expected_func_var, method_instantiated);

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
                                _ = try self.unify(dot_access_var, field_var);
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
                                _ = try self.unify(dot_access_var, field_var);
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
                    _ = try self.unify(dot_access_var, field_var);

                    // Create a record type with this field
                    const field_idx = try self.types.appendRecordField(.{
                        .name = dot_access.field_name,
                        .var_ = field_var,
                    });
                    const fields_range = types_mod.RecordField.SafeMultiList.Range{
                        .start = field_idx,
                        .count = 1,
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

                    // Use the dot access expression as the constraint origin for better error reporting
                    _ = try self.unifyWithConstraintOrigin(receiver_var, record_var, dot_access_var);

                    // Record that this variable was constrained by this dot access expression
                    try self.constraint_origins.put(receiver_var, dot_access_var);
                    // Constraint origin recorded for better error reporting
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

// func //

/// Helper function to unify a call expression with a function type
fn unifyFunctionCall(
    self: *Self,
    call_var: Var, // var for the entire call expr
    call_func_var: Var, // var for the fn itself
    call_args: []const CIR.Expr.Idx, // var for the fn args
    expected_func: types_mod.Func, // the expected type of the fn (must be instantiated)
    region: Region,
    func_expr_idx: CIR.Expr.Idx, // the function expression for name extraction
) std.mem.Allocator.Error!bool {
    // Extract function name if possible
    const func_name: ?Ident.Idx = blk: {
        const func_expr = self.cir.store.getExpr(func_expr_idx);
        switch (func_expr) {
            .e_lookup_local => |lookup| {
                // Get the pattern that defines this identifier
                const pattern = self.cir.store.getPattern(lookup.pattern_idx);
                switch (pattern) {
                    .assign => |assign| break :blk assign.ident,
                    else => break :blk null,
                }
            },
            else => break :blk null,
        }
    };

    // Check arguments with expected types using bidirectional type checking
    const expected_args = self.types.sliceVars(expected_func.args);

    // Only unify arguments if counts match - otherwise let the normal
    // unification process handle the arity mismatch error
    if (expected_args.len == call_args.len) {
        // For function signatures with bound type variables, unify arguments with each other first
        // This ensures proper error placement for cases like mk_pair("1", 2) where a, a -> Pair(a)

        // Find arguments that share the same type variable
        for (expected_args, 0..) |expected_arg_1, i| {
            const expected_resolved_1 = self.types.resolveVar(expected_arg_1);

            // Only check type variables, skip concrete types
            if (expected_resolved_1.desc.content != .flex_var and expected_resolved_1.desc.content != .rigid_var) {
                continue;
            }

            // Look for other arguments with the same type variable
            for (expected_args[i + 1 ..], i + 1..) |expected_arg_2, j| {
                if (expected_arg_1 == expected_arg_2) {
                    // These two arguments are bound by the same type variable - unify them first
                    const arg_1 = @as(Var, @enumFromInt(@intFromEnum(call_args[i])));
                    const arg_2 = @as(Var, @enumFromInt(@intFromEnum(call_args[j])));

                    const unify_result = try self.unify(arg_1, arg_2);

                    if (unify_result.isProblem()) {
                        // Use the new error detail for bound type variable incompatibility
                        self.setProblemTypeMismatchDetail(unify_result.problem, .{
                            .incompatible_fn_args_bound_var = .{
                                .fn_name = func_name,
                                .first_arg_var = arg_1,
                                .second_arg_var = arg_2,
                                .first_arg_index = @intCast(i),
                                .second_arg_index = @intCast(j),
                                .num_args = @intCast(call_args.len),
                            },
                        });
                        return false; // Early return on error
                    }
                }
            }
        }

        // Apply constraint propagation for numeric literals with concrete types
        for (expected_args, call_args) |expected_arg, arg_expr_idx| {
            const expected_resolved = self.types.resolveVar(expected_arg);

            // Only apply constraint propagation for concrete types, not type variables
            if (expected_resolved.desc.content != .flex_var and expected_resolved.desc.content != .rigid_var) {
                const arg_expr = self.cir.store.getExpr(arg_expr_idx);

                if (arg_expr == .e_int) {
                    const literal_var = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));
                    _ = try self.unify(literal_var, expected_arg);
                }
            }
        }

        // Regular unification using the argument vars
        var arg_index: u32 = 0;
        for (expected_args, call_args) |expected_arg, arg_expr_idx| {
            const actual_arg = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));

            // Instantiate polymorphic arguments before unification
            var arg_to_unify = actual_arg;
            if (self.types.needsInstantiation(actual_arg)) {
                arg_to_unify = try self.instantiateVarAnon(actual_arg, .{ .explicit = region });
            }

            const arg_result = try self.unify(expected_arg, arg_to_unify);
            self.setDetailIfTypeMismatch(arg_result, .{
                .incompatible_fn_call_arg = .{
                    .fn_name = null, // Get function name for better error message
                    .arg_var = actual_arg,
                    .incompatible_arg_index = arg_index,
                    .num_args = @intCast(call_args.len),
                },
            });
            arg_index += 1;
        }
        // The call's type is the instantiated return type
        _ = try self.unify(call_var, expected_func.ret);
    } else {
        // Arity mismatch - arguments already checked

        // Fall back to normal unification to get proper error message
        // Use the original func_var to avoid issues with instantiated variables in error reporting
        const actual_arg_vars: []Var = @constCast(@ptrCast(@alignCast(call_args)));
        const func_content = try self.types.mkFuncUnbound(actual_arg_vars, call_var);
        const expected_func_var = try self.freshFromContent(func_content, region);
        _ = try self.unify(call_func_var, expected_func_var);
    }

    return false;
}

/// Performs bidirectional type checking for lambda expressions with optional type annotations.
///
/// ## Constraint Ordering
///
/// Parameter constraints from annotations are applied BEFORE checking the lambda body.
/// This ensures rigid variables are properly constrained during body analysis, causing
/// type errors to surface at their actual source rather than during later unification.
///
/// **Why this ordering matters:**
/// ```
/// Pair(a) := [Pair(a, a)]
/// mk_pair_invalid : a, b -> Pair(a)
/// mk_pair_invalid = |x, y| Pair.Pair(x, y)
///
/// // Step 1: Constrain parameters from annotation first
/// x := a[rigid], y := b[rigid]
///
/// // Step 2: Check body - error detected immediately at constructor
/// Pair.Pair(x, y)  // ERROR: Pair requires same type, but x=a[rigid], y=b[rigid]
/// ```
///
/// Without upfront constraints, the error would only surface during final annotation
/// validation, making it harder to locate the actual problem in the source code.
fn checkLambdaWithAnno(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    _: Region,
    lambda: std.meta.FieldType(CIR.Expr, .e_lambda),
    anno_type: ?Var,
) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Get the actual lambda arguments
    const arg_patterns = self.cir.store.slicePatterns(lambda.args);
    const arg_vars: []Var = @ptrCast(@alignCast(arg_patterns));

    var mb_expected_var: ?Var = null;
    var mb_expected_func: ?types_mod.Func = null;

    // STEP 1: Apply annotation constraints to parameters FIRST
    if (anno_type) |anno_var| {
        mb_expected_var = anno_var;

        const expected_resolved = self.types.resolveVar(anno_var);
        mb_expected_func = switch (expected_resolved.desc.content) {
            .structure => |s| switch (s) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| func,
                else => null,
            },
            else => null,
        };

        if (mb_expected_func) |func| {
            const expected_args = self.types.sliceVars(func.args);
            if (expected_args.len == arg_patterns.len) {
                // Constrain parameters with annotation types
                for (arg_patterns, expected_args) |pattern_idx, expected_arg| {
                    // Check the pattern
                    try self.checkPattern(pattern_idx);

                    // Unify against the annotation
                    const pattern_var = ModuleEnv.varFrom(pattern_idx);
                    _ = try self.unify(pattern_var, expected_arg);
                }
            }
        }
    }

    // STEP 2: Check the body with return type constraint propagation for literals
    const is_effectful = if (mb_expected_func) |func|
        try self.checkExprWithExpectedAndAnnotation(lambda.body, func.ret, true)
    else
        try self.checkExpr(lambda.body);

    // STEP 3: Build the function type
    const fn_var = ModuleEnv.varFrom(expr_idx);
    const return_var = @as(Var, @enumFromInt(@intFromEnum(lambda.body)));

    // Ensure the return variable has the correct region for error reporting
    const body_region = self.cir.store.getExprRegion(lambda.body);
    try self.fillInRegionsThrough(return_var);
    self.setRegionAt(return_var, body_region);

    if (is_effectful) {
        _ = try self.types.setVarContent(fn_var, try self.types.mkFuncEffectful(arg_vars, return_var));
    } else {
        _ = try self.types.setVarContent(fn_var, try self.types.mkFuncUnbound(arg_vars, return_var));
    }

    // STEP 4: Validate the function body against the annotation return type
    if (mb_expected_func) |func| {
        _ = try self.unify(return_var, func.ret);
    }

    // STEP 5: Validate the entire function against the annotation
    if (mb_expected_var) |expected_var| {
        _ = try self.unifyWithAnnotation(fn_var, expected_var);
    }

    return is_effectful;
}

// binop //

/// Check the types for a binary operation expression
fn checkBinopExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, binop: CIR.Expr.Binop, expected_type: ?Var, from_annotation: bool) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (binop.op) {
        .add, .sub, .mul, .div, .rem, .pow, .div_trunc => {
            // Check operands first
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;

            // For now, we'll constrain both operands to be numbers
            // In the future, this will use static dispatch based on the lhs type
            const lhs_var = @as(Var, @enumFromInt(@intFromEnum(binop.lhs)));
            const rhs_var = @as(Var, @enumFromInt(@intFromEnum(binop.rhs)));
            const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

            // For bidirectional type checking: if we have an expected type,
            // we need to unify all operands and result with it.
            // This ensures literals like `2` in `|x| x + 2` get properly constrained
            // when the lambda has a type annotation like `I64 -> I64`.
            if (expected_type) |expected| {
                // All three must be the same type for arithmetic operations
                if (from_annotation) {
                    _ = try self.unifyWithAnnotation(lhs_var, expected);
                    _ = try self.unifyWithAnnotation(rhs_var, expected);
                    _ = try self.unifyWithAnnotation(result_var, expected);
                } else {
                    _ = try self.unify(lhs_var, expected);
                    _ = try self.unify(rhs_var, expected);
                    _ = try self.unify(result_var, expected);
                }
            } else {
                // No expected type - use fresh number variables to maintain polymorphism
                const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                const num_var_lhs = try self.freshFromContent(num_content, expr_region);
                const num_var_rhs = try self.freshFromContent(num_content, expr_region);
                const num_var_result = try self.freshFromContent(num_content, expr_region);

                // Unify lhs, rhs, and result with the number type
                _ = try self.unify(num_var_lhs, lhs_var);
                _ = try self.unify(num_var_rhs, rhs_var);
                _ = try self.unify(result_var, num_var_result);
            }

            return does_fx;
        },
        .lt, .gt, .le, .ge, .eq, .ne => {
            // Check operands first
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;

            // Comparison operators always return Bool
            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
            const fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
            _ = try self.unify(expr_var, fresh_bool);

            return does_fx;
        },
        .@"and" => {
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;

            const lhs_fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
            const lhs_result = try self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            if (lhs_result.isOk()) {
                const rhs_fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
                const rhs_result = try self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
                self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                    .binop_expr = expr_idx,
                    .problem_side = .rhs,
                    .binop = .@"and",
                } });
            }

            return does_fx;
        },
        .@"or" => {
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;

            const lhs_fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
            const lhs_result = try self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"or",
            } });

            if (lhs_result.isOk()) {
                const rhs_fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
                const rhs_result = try self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
                self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                    .binop_expr = expr_idx,
                    .problem_side = .rhs,
                    .binop = .@"or",
                } });
            }

            return does_fx;
        },
        .pipe_forward => {
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;
            return does_fx;
        },
        .null_coalesce => {
            var does_fx = try self.checkExpr(binop.lhs);
            does_fx = try self.checkExpr(binop.rhs) or does_fx;
            return does_fx;
        },
    }
}

fn checkUnaryMinusExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, unary: CIR.Expr.UnaryMinus) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr);

    // For unary minus, we constrain the operand and result to be numbers
    const operand_var = @as(Var, @enumFromInt(@intFromEnum(unary.expr)));
    const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Create a fresh number variable for the operation
    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = true, .bits_needed = 0 } } } };
    const num_var = try self.freshFromContent(num_content, expr_region);

    // Unify operand and result with the number type
    _ = try self.unify(operand_var, num_var);
    _ = try self.unify(result_var, num_var);

    return does_fx;
}

fn checkUnaryNotExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, unary: CIR.Expr.UnaryNot) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr);

    // For unary not, we constrain the operand and result to be booleans
    const operand_var = @as(Var, @enumFromInt(@intFromEnum(unary.expr)));
    const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Create a fresh boolean variable for the operation
    const bool_var = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });

    // Unify operand and result with the boolean type
    _ = try self.unify(operand_var, bool_var);
    _ = try self.unify(result_var, bool_var);

    return does_fx;
}

// nominal //

// nominal //

/// Check that a nominal type constructor usage is valid and perform type inference.
///
/// This function validates that a user's constructor application (like `Maybe.Just(1)`)
/// is compatible with the nominal type's definition and infers the concrete types.
///
/// Example:
///   Maybe a := [Just(a), None]
///   val = Maybe.Just(1)  // This call validates Just(1) against Maybe's definition
///
/// Parameters:
/// * `node_var` - Variable representing the entire expression (initially a placeholder).
///                This will be redirected to the final inferred type.
/// * `node_region` - Source region of the entire expression for error reporting
/// * `node_backing_var` - Variable for the constructor application's internal type (e.g., `Just(1)`)
/// * `node_backing_type` - Kind of nominal backing (tag, record, tuple, or val)
/// * `real_nominal_var` - Variable of the nominal type declaration (e.g., `Maybe a`)
fn checkNominal(
    self: *Self,
    node_var: Var,
    node_region: Region,
    node_backing_var: Var,
    node_backing_type: CIR.Expr.NominalBackingType,
    real_nominal_var: Var,
) Allocator.Error!void {
    // Clear the rigid variable substitution map to ensure fresh instantiation
    self.anonymous_rigid_var_subs.items.clearRetainingCapacity();

    // Algorithm overview:
    // 1. Validate that the nominal type exists and is well-formed
    // 2. Instantiate the nominal type's backing type with fresh variables
    // 3. Unify the instantiated backing type against the user's constructor
    // 4. If unification succeeds, instantiate the nominal type and assign to node_var
    // 5. If unification fails, report appropriate error and mark node_var as error
    //
    // The key insight is using a shared rigid variable map so that type parameters
    // in the backing type and nominal type get the same substitutions.

    // Step 1: Extract and validate the nominal type
    const nominal_content = self.types.resolveVar(real_nominal_var).desc.content;
    const nominal_type = switch (nominal_content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nt| nt,
            else => {
                // The supposed nominal type is actually some other structure
                try self.types.setVarContent(node_var, .err);
                return;
            },
        },
        else => {
            // The nominal type is in an error state or unresolved
            try self.types.setVarContent(node_var, .err);
            return;
        },
    };

    // Step 2: Instantiate the nominal type's backing type
    // This converts rigid type variables (like `a[r]`) to fresh flexible variables
    // that can be unified. The backing type defines the structure (e.g., [Just(a), None])
    const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);
    const instantiated_backing_var = try self.instantiateVar(
        nominal_backing_var,
        &self.anonymous_rigid_var_subs,
        .{ .explicit = node_region },
    );

    // TODO: If we can, unify arguments directly to get better error messages
    // Unifying by the whole thing works, but doesn't have great error messages
    // always

    // Step 3: Unify the instantiated backing type with the user's constructor
    // This checks if `Just(1)` is compatible with `[Just(a), None]` and if so,
    // unifies `a` with the type of `1` (e.g., Num(_size))
    const result = try self.unify(instantiated_backing_var, node_backing_var);

    // Step 4: Handle the unification result
    switch (result) {
        .ok => {
            // Unification succeeded - the constructor is valid for this nominal type
            // Now instantiate the full nominal type using the same rigid variable map
            // so it gets the same type substitutions as the backing type
            const instantiated_qualified_var = try self.instantiateVar(
                real_nominal_var,
                &self.anonymous_rigid_var_subs,
                .{ .explicit = node_region },
            );

            // Assign the inferred concrete type to the expression variable
            // After this, `node_var` will resolve to something like `Maybe(Num(_size))`
            try self.types.setVarRedirect(node_var, instantiated_qualified_var);
        },
        .problem => |problem_idx| {
            // Unification failed - the constructor is incompatible with the nominal type
            // Set a specific error message based on the backing type kind
            switch (node_backing_type) {
                .tag => {
                    // Constructor doesn't exist or has wrong arity/types
                    self.setProblemTypeMismatchDetail(problem_idx, .invalid_nominal_tag);
                },
                else => {
                    // TODO: Add specific error messages for records, tuples, etc.
                },
            }

            // Mark the entire expression as having a type error
            try self.types.setVarContent(node_var, .err);
        },
    }
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
    const bool_var = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
    const first_cond_result = try self.unify(bool_var, first_cond_var);
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
        const branch_bool_var = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
        const cond_result = try self.unify(branch_bool_var, cond_var);
        self.setDetailIfTypeMismatch(cond_result, .incompatible_if_cond);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body) or does_fx;
        const body_var: Var = @enumFromInt(@intFromEnum(branch.body));
        const body_result = try self.unify(branch_var, body_var);
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

                const fresh_bool = try self.instantiateVarAnon(ModuleEnv.varFrom(can.Can.BUILTIN_BOOL_TYPE), .{ .explicit = expr_region });
                const remaining_cond_result = try self.unify(fresh_bool, remaining_cond_var);
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
    const final_else_result = try self.unify(branch_var, final_else_var);
    self.setDetailIfTypeMismatch(final_else_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
        .parent_if_expr = if_expr_idx,
        .last_if_branch = last_if_branch,
        .num_branches = num_branches,
        .problem_branch_index = num_branches - 1,
    } });

    // Unify the if expression's type variable with the branch type
    const if_expr_var: Var = @enumFromInt(@intFromEnum(if_expr_idx));
    _ = try self.unify(if_expr_var, branch_var);

    return does_fx;
}

// match //

/// Check the types for an if-else expr
fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, match: CIR.Expr.Match) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond);
    const cond_var = ModuleEnv.varFrom(match.cond);

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
        const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);

        const ptrn_result = try self.unify(cond_var, branch_ptrn_var);
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
    const branch_var = ModuleEnv.varFrom(first_branch.value);

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
            const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);
            const ptrn_result = try self.unify(cond_var, branch_ptrn_var);
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
        const branch_result = try self.unify(branch_var, ModuleEnv.varFrom(branch.value));
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
                    const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
                    const ptrn_result = try self.unify(cond_var, other_branch_ptrn_var);
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
                try self.types.setVarContent(ModuleEnv.varFrom(other_branch.value), .err);
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
    switch (self.problems.problems.items[@intFromEnum(problem_idx)]) {
        .type_mismatch => |mismatch| {
            self.problems.problems.items[@intFromEnum(problem_idx)] = .{
                .type_mismatch = .{
                    .types = mismatch.types,
                    .detail = mismatch_detail,
                },
            };
        },
        else => {
            // For other problem types (e.g., number_does_not_fit), the
            // original problem is already more specific than our custom
            // problem, so we should keep it as-is and not replace it.
        },
    }
}

// tests //

// test "minimum signed values fit in their respective types" {
//     const test_cases = .{
//         .{ .value = -128, .type = types_mod.Num.Int.Precision.i8, .should_fit = true },
//         .{ .value = -129, .type = types_mod.Num.Int.Precision.i8, .should_fit = false },
//         .{ .value = -32768, .type = types_mod.Num.Int.Precision.i16, .should_fit = true },
//         .{ .value = -32769, .type = types_mod.Num.Int.Precision.i16, .should_fit = false },
//         .{ .value = -2147483648, .type = types_mod.Num.Int.Precision.i32, .should_fit = true },
//         .{ .value = -2147483649, .type = types_mod.Num.Int.Precision.i32, .should_fit = false },
//         .{ .value = -9223372036854775808, .type = types_mod.Num.Int.Precision.i64, .should_fit = true },
//         .{ .value = -9223372036854775809, .type = types_mod.Num.Int.Precision.i64, .should_fit = false },
//         .{ .value = -170141183460469231731687303715884105728, .type = types_mod.Num.Int.Precision.i128, .should_fit = true },
//     };

//     const gpa = std.testing.allocator;

//     var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
//     try module_env.initModuleEnvFields(gpa, "Test");
//     defer module_env.deinit();

//     var problems = try ProblemStore.initCapacity(gpa, 16);
//     defer problems.deinit(gpa);

//     var snapshots = try SnapshotStore.initCapacity(gpa, 16);
//     defer snapshots.deinit();

//     var unify_scratch = try unifier.Scratch.init(gpa);
//     defer unify_scratch.deinit();

//     var occurs_scratch = try occurs.Scratch.init(gpa);
//     defer occurs_scratch.deinit();

//     inline for (test_cases) |tc| {
//         // Calculate the magnitude
//         const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

//         // Apply the branchless adjustment for minimum signed values
//         const is_negative = @as(u1, @intFromBool(tc.value < 0));
//         const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
//         const is_minimum_signed = is_negative & is_power_of_2;
//         const adjusted_val = u128_val - is_minimum_signed;

//         // Create requirements based on adjusted value
//         const requirements = types_mod.Num.IntRequirements{
//             .sign_needed = tc.value < 0,
//             .bits_needed = @intFromEnum(types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val)),
//         };

//         const literal_var = try module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_unbound = requirements } } });
//         const type_var = try module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_compact = .{ .int = tc.type } } } });

//         const result = try unifier.unify(
//             &module_env,
//             &module_env.types,
//             &problems,
//             &snapshots,
//             &unify_scratch,
//             &occurs_scratch,
//             literal_var,
//             type_var,
//         );

//         if (tc.should_fit) {
//             try std.testing.expect(result == .ok);
//         } else {
//             try std.testing.expect(result == .problem);
//         }
//     }
// }

// test "minimum signed values have correct bits_needed" {
//     const test_cases = .{
//         .{ .value = -128, .expected_bits = types_mod.Num.Int.BitsNeeded.@"7" },
//         .{ .value = -129, .expected_bits = types_mod.Num.Int.BitsNeeded.@"8" },
//         .{ .value = -32768, .expected_bits = types_mod.Num.Int.BitsNeeded.@"9_to_15" },
//         .{ .value = -32769, .expected_bits = types_mod.Num.Int.BitsNeeded.@"16" },
//         .{ .value = -2147483648, .expected_bits = types_mod.Num.Int.BitsNeeded.@"17_to_31" },
//         .{ .value = -2147483649, .expected_bits = types_mod.Num.Int.BitsNeeded.@"32" },
//         .{ .value = -9223372036854775808, .expected_bits = types_mod.Num.Int.BitsNeeded.@"33_to_63" },
//         .{ .value = -9223372036854775809, .expected_bits = types_mod.Num.Int.BitsNeeded.@"64" },
//         .{ .value = -170141183460469231731687303715884105728, .expected_bits = types_mod.Num.Int.BitsNeeded.@"65_to_127" },
//     };

//     inline for (test_cases) |tc| {
//         // Calculate the magnitude
//         const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

//         // Apply the branchless adjustment for minimum signed values
//         const is_negative = @as(u1, if (tc.value < 0) 1 else 0);
//         const is_power_of_2 = @as(u1, if (u128_val != 0 and (u128_val & (u128_val - 1)) == 0) 1 else 0);
//         const is_minimum_signed = is_negative & is_power_of_2;
//         const adjusted_val = u128_val - is_minimum_signed;

//         const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
//         try std.testing.expectEqual(tc.expected_bits, bits_needed);
//     }
// }

// test "branchless minimum signed value detection" {
//     const test_cases = .{
//         // Minimum signed values (negative powers of 2)
//         .{ .value = -1, .is_minimum = true }, // magnitude 1 = 2^0
//         .{ .value = -2, .is_minimum = true }, // magnitude 2 = 2^1
//         .{ .value = -4, .is_minimum = true }, // magnitude 4 = 2^2
//         .{ .value = -8, .is_minimum = true }, // magnitude 8 = 2^3
//         .{ .value = -16, .is_minimum = true }, // magnitude 16 = 2^4
//         .{ .value = -32, .is_minimum = true }, // magnitude 32 = 2^5
//         .{ .value = -64, .is_minimum = true }, // magnitude 64 = 2^6
//         .{ .value = -128, .is_minimum = true }, // magnitude 128 = 2^7
//         .{ .value = -256, .is_minimum = true }, // magnitude 256 = 2^8
//         .{ .value = -32768, .is_minimum = true }, // magnitude 32768 = 2^15
//         .{ .value = -2147483648, .is_minimum = true }, // magnitude 2^31

//         // Not minimum signed values
//         .{ .value = 128, .is_minimum = false }, // positive
//         .{ .value = -3, .is_minimum = false }, // magnitude 3 (not power of 2)
//         .{ .value = -5, .is_minimum = false }, // magnitude 5 (not power of 2)
//         .{ .value = -127, .is_minimum = false }, // magnitude 127 (not power of 2)
//         .{ .value = -129, .is_minimum = false }, // magnitude 129 (not power of 2)
//         .{ .value = -130, .is_minimum = false }, // magnitude 130 (not power of 2)
//         .{ .value = 0, .is_minimum = false }, // zero
//     };

//     inline for (test_cases) |tc| {
//         const value: i128 = tc.value;
//         const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

//         const is_negative = @as(u1, @intFromBool(value < 0));
//         const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
//         const is_minimum_signed = is_negative & is_power_of_2;

//         const expected: u1 = @intFromBool(tc.is_minimum);
//         try std.testing.expectEqual(expected, is_minimum_signed);
//     }
// }

// test "verify -128 produces 7 bits needed" {
//     const value: i128 = -128;
//     const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

//     // Check intermediate values
//     try std.testing.expectEqual(@as(u128, 128), u128_val);

//     const is_negative = @as(u1, @intFromBool(value < 0));
//     const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
//     const is_minimum_signed = is_negative & is_power_of_2;

//     try std.testing.expectEqual(@as(u1, 1), is_negative);
//     try std.testing.expectEqual(@as(u1, 1), is_power_of_2);
//     try std.testing.expectEqual(@as(u1, 1), is_minimum_signed);

//     const adjusted_val = u128_val - is_minimum_signed;
//     try std.testing.expectEqual(@as(u128, 127), adjusted_val);

//     // Test that 127 maps to 7 bits
//     const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
//     try std.testing.expectEqual(types_mod.Num.Int.BitsNeeded.@"7", bits_needed);
//     try std.testing.expectEqual(@as(u8, 7), bits_needed.toBits());
// }

// test "lambda with record field access infers correct type" {
//     // The lambda |x, y| { x: x, y: y }.x should have type a, b -> a
//     // And when annotated as I32, I32 -> I32, it should unify correctly.
//     // This is a regression test against a bug that previously existed in that scenario.
//     const gpa = std.testing.allocator;

//     // Create a minimal environment for testing
//     var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
//     defer module_env.deinit();

//     try module_env.initModuleEnvFields(gpa, "Test");
//     const cir = &module_env;

//     const empty_modules: []const *ModuleEnv = &.{};
//     var solver = try Self.init(gpa, &module_env.types, cir, empty_modules, &cir.store.regions);
//     defer solver.deinit();

//     // Create type variables for the lambda parameters
//     const param_x_var = try module_env.types.fresh();
//     const param_y_var = try module_env.types.fresh();

//     // Create a record with fields x and y
//     var record_fields = std.ArrayList(types_mod.RecordField).init(gpa);
//     defer record_fields.deinit();

//     const x_ident = try module_env.idents.insert(gpa, base.Ident.for_text("x"));
//     const y_ident = try module_env.idents.insert(gpa, base.Ident.for_text("y"));

//     try record_fields.append(.{ .name = x_ident, .var_ = param_x_var });
//     try record_fields.append(.{ .name = y_ident, .var_ = param_y_var });

//     const fields_range = try module_env.types.appendRecordFields(record_fields.items);
//     const ext_var = try module_env.types.fresh();
//     const record_content = types_mod.Content{
//         .structure = .{
//             .record = .{
//                 .fields = fields_range,
//                 .ext = ext_var,
//             },
//         },
//     };
//     _ = try module_env.types.freshFromContent(record_content);

//     // Simulate field access: record.x
//     // The result type should unify with param_x_var
//     const field_access_var = try module_env.types.fresh();
//     _ = try solver.unify(field_access_var, param_x_var);

//     // Create the lambda type: param_x, param_y -> field_access_result
//     const lambda_content = try module_env.types.mkFuncUnbound(&[_]types_mod.Var{ param_x_var, param_y_var }, field_access_var);
//     const lambda_var = try module_env.types.freshFromContent(lambda_content);

//     // The lambda should have type a, b -> a (param_x and return type are unified)
//     const resolved_lambda = module_env.types.resolveVar(lambda_var);
//     try std.testing.expect(resolved_lambda.desc.content == .structure);
//     try std.testing.expect(resolved_lambda.desc.content.structure == .fn_unbound);

//     const func = resolved_lambda.desc.content.structure.fn_unbound;
//     const args = module_env.types.sliceVars(func.args);
//     try std.testing.expectEqual(@as(usize, 2), args.len);

//     // Verify that first parameter and return type resolve to the same variable
//     const first_param_resolved = module_env.types.resolveVar(args[0]);
//     const return_resolved = module_env.types.resolveVar(func.ret);
//     try std.testing.expectEqual(first_param_resolved.var_, return_resolved.var_);

//     // Now test with annotation: I32, I32 -> I32
//     const i32_content = types_mod.Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
//     const i32_var1 = try module_env.types.freshFromContent(i32_content);
//     const i32_var2 = try module_env.types.freshFromContent(i32_content);
//     const i32_var3 = try module_env.types.freshFromContent(i32_content);

//     const annotated_func = try module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var1, i32_var2 }, i32_var3);
//     const annotation_var = try module_env.types.freshFromContent(annotated_func);

//     // Unify the lambda with its annotation
//     const unify_result = try solver.unify(lambda_var, annotation_var);
//     try std.testing.expect(unify_result == .ok);

//     // Verify the lambda now has the concrete type
//     const final_resolved = module_env.types.resolveVar(lambda_var);
//     try std.testing.expect(final_resolved.desc.content == .structure);
//     try std.testing.expect(final_resolved.desc.content.structure == .fn_pure);

//     // Test call site: when calling with integer literals
//     const num_unbound = types_mod.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
//     const lit1_var = try module_env.types.freshFromContent(num_unbound);
//     const lit2_var = try module_env.types.freshFromContent(num_unbound);
//     const call_result_var = try module_env.types.fresh();

//     const expected_func_content = try module_env.types.mkFuncUnbound(&[_]types_mod.Var{ lit1_var, lit2_var }, call_result_var);
//     const expected_func_var = try module_env.types.freshFromContent(expected_func_content);

//     // The critical fix: unify expected with actual (not the other way around)
//     const call_unify_result = try solver.unify(expected_func_var, lambda_var);
//     try std.testing.expect(call_unify_result == .ok);

//     // Verify the literals got constrained to I32
//     const lit1_resolved = module_env.types.resolveVar(lit1_var);
//     try std.testing.expect(lit1_resolved.desc.content == .structure);
//     try std.testing.expect(lit1_resolved.desc.content.structure == .num);
//     try std.testing.expect(lit1_resolved.desc.content.structure.num == .int_precision);
//     try std.testing.expect(lit1_resolved.desc.content.structure.num.int_precision == .i32);
// }

// test "dot access properly unifies field types with parameters" {
//     // This test verifies that e_dot_access correctly handles field access
//     // and unifies the field type with the expression result type.

//     const gpa = std.testing.allocator;

//     // Create a minimal environment for testing
//     var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
//     defer module_env.deinit();

//     try module_env.initModuleEnvFields(gpa, "Test");
//     const cir = &module_env;

//     const empty_modules: []const *ModuleEnv = &.{};
//     var solver = try Self.init(gpa, &module_env.types, cir, empty_modules, &cir.store.regions);
//     defer solver.deinit();

//     // Create a parameter type variable
//     const param_var = try module_env.types.fresh();

//     // Create a record with field "x" of the same type as the parameter
//     var record_fields = std.ArrayList(types_mod.RecordField).init(gpa);
//     defer record_fields.deinit();

//     const x_ident = try module_env.idents.insert(gpa, base.Ident.for_text("x"));
//     try record_fields.append(.{ .name = x_ident, .var_ = param_var });

//     const fields_range = try module_env.types.appendRecordFields(record_fields.items);
//     const ext_var = try module_env.types.fresh();
//     const record_content = types_mod.Content{
//         .structure = .{
//             .record = .{
//                 .fields = fields_range,
//                 .ext = ext_var,
//             },
//         },
//     };
//     const record_var = try module_env.types.freshFromContent(record_content);

//     // Create a dot access result variable
//     const dot_access_var = try module_env.types.fresh();

//     // Simulate the dot access logic from checkExpr
//     const resolved_record = module_env.types.resolveVar(record_var);
//     try std.testing.expect(resolved_record.desc.content == .structure);
//     try std.testing.expect(resolved_record.desc.content.structure == .record);

//     const record = resolved_record.desc.content.structure.record;
//     const fields = module_env.types.getRecordFieldsSlice(record.fields);

//     // Find field "x" and unify with dot access result
//     var found_field = false;
//     for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
//         if (field_name == x_ident) {
//             _ = try solver.unify(dot_access_var, field_var);
//             found_field = true;
//             break;
//         }
//     }
//     try std.testing.expect(found_field);

//     // Verify that dot_access_var and param_var now resolve to the same variable
//     const dot_resolved = module_env.types.resolveVar(dot_access_var);
//     const param_resolved = module_env.types.resolveVar(param_var);
//     try std.testing.expectEqual(dot_resolved.var_, param_resolved.var_);

//     // Test with unbound record
//     const unbound_record_content = types_mod.Content{
//         .structure = .{
//             .record_unbound = fields_range,
//         },
//     };
//     const unbound_record_var = try module_env.types.freshFromContent(unbound_record_content);
//     const dot_access_var2 = try module_env.types.fresh();

//     // Same test with record_unbound
//     const resolved_unbound = module_env.types.resolveVar(unbound_record_var);
//     try std.testing.expect(resolved_unbound.desc.content == .structure);
//     try std.testing.expect(resolved_unbound.desc.content.structure == .record_unbound);

//     const unbound_record = resolved_unbound.desc.content.structure.record_unbound;
//     const unbound_fields = module_env.types.getRecordFieldsSlice(unbound_record);

//     found_field = false;
//     for (unbound_fields.items(.name), unbound_fields.items(.var_)) |field_name, field_var| {
//         if (field_name == x_ident) {
//             _ = try solver.unify(dot_access_var2, field_var);
//             found_field = true;
//             break;
//         }
//     }
//     try std.testing.expect(found_field);

//     // Verify unification worked
//     const dot2_resolved = module_env.types.resolveVar(dot_access_var2);
//     try std.testing.expectEqual(dot2_resolved.var_, param_resolved.var_);
// }

// test "call site unification order matters for concrete vs flexible types" {
//     // This test verifies that unification order matters when dealing with
//     // concrete types (like I32) and flexible types (like Num(*)).
//     //
//     // At call sites, we must unify in the correct order:
//     // - unify(flexible, concrete) ✓ succeeds - flexible types can be constrained
//     // - unify(concrete, flexible) ✗ fails - concrete types cannot become more general
//     //
//     // The test demonstrates the complete type checking scenario:
//     // 1. Unification order matters (concrete→flexible fails, flexible→concrete succeeds)
//     // 2. When unifying function types in the correct order, the flexible argument
//     //    types are properly constrained to match the concrete parameter types
//     // 3. Numeric arguments start as flexible num_unbound types
//     // 4. After unification, they become concrete I32 types
//     const gpa = std.testing.allocator;

//     var common_env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
//     // Module env takes ownership of Common env -- no need to deinit here

//     // Create a minimal environment for testing
//     var module_env = try ModuleEnv.init(gpa, &common_env);
//     defer module_env.deinit();

//     try module_env.initModuleEnvFields(gpa, "Test");
//     const cir = &module_env;

//     const empty_modules: []const *ModuleEnv = &.{};
//     var solver = try Self.init(gpa, &module_env.types, cir, empty_modules, &cir.store.regions);
//     defer solver.deinit();

//     // First, verify basic number unification works as expected
//     const i32_content = types_mod.Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
//     const i32_test = try module_env.types.freshFromContent(i32_content);
//     const num_unbound = types_mod.Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
//     const flex_test = try module_env.types.freshFromContent(num_unbound);

//     // Flexible number should unify with concrete I32 and become I32
//     const basic_result = try solver.unify(flex_test, i32_test);
//     try std.testing.expect(basic_result == .ok);

//     // Verify the flexible variable was constrained to I32
//     const flex_resolved = module_env.types.resolveVar(flex_test);
//     switch (flex_resolved.desc.content) {
//         .structure => |s| switch (s) {
//             .num => |n| switch (n) {
//                 .int_precision => |prec| {
//                     try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
//                 },
//                 else => return error.TestUnexpectedResult,
//             },
//             else => return error.TestUnexpectedResult,
//         },
//         else => return error.TestUnexpectedResult,
//     }

//     // Now test with function types
//     // Create a concrete function type: I32, I32 -> I32
//     const i32_var1 = try module_env.types.freshFromContent(i32_content);
//     const i32_var2 = try module_env.types.freshFromContent(i32_content);
//     const i32_var3 = try module_env.types.freshFromContent(i32_content);

//     const concrete_func = try module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var1, i32_var2 }, i32_var3);
//     const concrete_func_var = try module_env.types.freshFromContent(concrete_func);

//     // Create flexible argument types (like integer literals)
//     const arg1_var = try module_env.types.freshFromContent(num_unbound);
//     const arg2_var = try module_env.types.freshFromContent(num_unbound);
//     const result_var = try module_env.types.fresh();

//     // Create expected function type from arguments
//     const expected_func = try module_env.types.mkFuncUnbound(&[_]types_mod.Var{ arg1_var, arg2_var }, result_var);
//     const expected_func_var = try module_env.types.freshFromContent(expected_func);

//     // The wrong order: unify(concrete, expected) would fail
//     // because I32 can't unify with Num(*)
//     const wrong_order_result = try solver.unify(concrete_func_var, expected_func_var);
//     try std.testing.expect(wrong_order_result == .problem);

//     // After failed unification, both variables become error types
//     const concrete_after_fail = module_env.types.resolveVar(concrete_func_var);
//     const expected_after_fail = module_env.types.resolveVar(expected_func_var);
//     try std.testing.expectEqual(types_mod.Content.err, concrete_after_fail.desc.content);
//     try std.testing.expectEqual(types_mod.Content.err, expected_after_fail.desc.content);

//     // Now simulate a complete type checking scenario for a function call
//     // This is what happens when type checking code like: myFunc(1, 2)
//     // where myFunc : I32, I32 -> I32

//     // Step 1: Create the known function type (I32, I32 -> I32)
//     const i32_var4 = try module_env.types.freshFromContent(i32_content);
//     const i32_var5 = try module_env.types.freshFromContent(i32_content);
//     const i32_var6 = try module_env.types.freshFromContent(i32_content);
//     const known_func = try module_env.types.mkFuncPure(&[_]types_mod.Var{ i32_var4, i32_var5 }, i32_var6);
//     const known_func_var = try module_env.types.freshFromContent(known_func);

//     // Step 2: Create flexible argument types (representing literals like 1 and 2)
//     const call_arg1 = try module_env.types.freshFromContent(num_unbound);
//     const call_arg2 = try module_env.types.freshFromContent(num_unbound);

//     // Verify the arguments start as flexible num_unbound types
//     const arg1_before = module_env.types.resolveVar(call_arg1);
//     const arg2_before = module_env.types.resolveVar(call_arg2);

//     switch (arg1_before.desc.content) {
//         .structure => |s| switch (s) {
//             .num => |n| switch (n) {
//                 .num_unbound => {}, // Expected
//                 else => return error.TestUnexpectedResult,
//             },
//             else => return error.TestUnexpectedResult,
//         },
//         else => return error.TestUnexpectedResult,
//     }

//     switch (arg2_before.desc.content) {
//         .structure => |s| switch (s) {
//             .num => |n| switch (n) {
//                 .num_unbound => {}, // Expected
//                 else => return error.TestUnexpectedResult,
//             },
//             else => return error.TestUnexpectedResult,
//         },
//         else => return error.TestUnexpectedResult,
//     }

//     // Step 3: Create the expected function type from the call site
//     // This represents the type we expect based on the arguments
//     const call_result = try module_env.types.fresh();
//     const call_func = try module_env.types.mkFuncUnbound(&[_]types_mod.Var{ call_arg1, call_arg2 }, call_result);
//     const call_func_var = try module_env.types.freshFromContent(call_func);

//     // Step 4: Unify the expected type with the known type
//     // This is the key step - unify(expected, known) in the correct order
//     const unify_result = try solver.unify(call_func_var, known_func_var);
//     try std.testing.expect(unify_result == .ok);

//     // Step 5: Verify that the call arguments were constrained to I32
//     // This simulates what happens in real type checking - the argument
//     // variables used at the call site get constrained by the function type

//     // Step 6: Verify that both arguments are now constrained to I32
//     for ([_]types_mod.Var{ call_arg1, call_arg2 }) |arg| {
//         const arg_resolved = module_env.types.resolveVar(arg);
//         switch (arg_resolved.desc.content) {
//             .structure => |s| switch (s) {
//                 .num => |n| switch (n) {
//                     .int_precision => |prec| {
//                         try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
//                     },
//                     .num_compact => |compact| switch (compact) {
//                         .int => |prec| {
//                             try std.testing.expectEqual(types_mod.Num.Int.Precision.i32, prec);
//                         },
//                         else => return error.TestUnexpectedResult,
//                     },
//                     else => return error.TestUnexpectedResult,
//                 },
//                 else => return error.TestUnexpectedResult,
//             },
//             else => return error.TestUnexpectedResult,
//         }
//     }
// }
