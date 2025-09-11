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
const builtins = @import("builtins");

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
const Func = types_mod.Func;
const Var = types_mod.Var;
const Content = types_mod.Content;
const Rank = types_mod.Rank;
const Num = types_mod.Num;
const testing = std.testing;
const Instantiate = types_mod.instantiate.Instantiate;
const Generalizer = types_mod.generalize.Generalizer;
const VarPool = types_mod.generalize.VarPool;
const SnapshotStore = @import("snapshot.zig").Store;
const ProblemStore = @import("problem.zig").Store;

const Self = @This();

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
cir: *ModuleEnv,
regions: *Region.List,
other_modules: []const *ModuleEnv,
common_idents: CommonIdents,

/// type snapshots used in error messages
snapshots: SnapshotStore,
/// type problems
problems: ProblemStore,
/// reusable scratch arrays used in unification
unify_scratch: unifier.Scratch,
/// reusable scratch arrays used in occurs check
occurs_scratch: occurs.Scratch,
/// free vars collected when generation types from annotation
anno_free_vars: base.Scratch(FreeVar),
/// free vars collected when generation types from type decls
decl_free_vars: base.Scratch(FreeVar),
/// stmts we've already seen when generation a type from an annotation
seen_annos: std.AutoHashMap(CIR.TypeAnno.Idx, Var),
/// pool of variables that need to be generalized, built up during checking
var_pool: VarPool,
/// wrapper around generalization, contains some internal state used to do it's work
generalizer: Generalizer,
/// scratch vars used to build up intermediate lists, used for various things
scratch_vars: base.Scratch(Var),
/// scratch tags used to build up intermediate lists, used for various things
scratch_tags: base.Scratch(types_mod.Tag),
/// scratch record fields used to build up intermediate lists, used for various things
scratch_record_fields: base.Scratch(types_mod.RecordField),
/// used in instantiation. TODO: Move into something like Instantiator
var_map: std.AutoHashMap(Var, Var),
/// used in instantiation. TODO: Move into something like Instantiator
anonymous_rigid_var_subs: Instantiate.RigidToFlexSubs,
/// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,
/// Maps variables to the expressions that constrained them (for better error regions)
constraint_origins: std.AutoHashMap(Var, Var),

/// A map of rigid variables that we build up during a branch of type checking
const FreeVar = struct { ident: base.Ident.Idx, var_: Var };

/// A struct of common idents
pub const CommonIdents = struct {
    module_name: base.Ident.Idx,
    list: base.Ident.Idx,
    box: base.Ident.Idx,
};

/// Init type solver
/// Does *not* own types_store or cir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *const ModuleEnv,
    other_modules: []const *ModuleEnv,
    regions: *Region.List,
    common_idents: CommonIdents,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types,
        .cir = @constCast(cir),
        .other_modules = other_modules,
        .regions = regions,
        .common_idents = common_idents,
        .snapshots = try SnapshotStore.initCapacity(gpa, 512),
        .problems = try ProblemStore.initCapacity(gpa, 64),
        .unify_scratch = try unifier.Scratch.init(gpa),
        .occurs_scratch = try occurs.Scratch.init(gpa),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .anno_free_vars = try base.Scratch(FreeVar).init(gpa),
        .decl_free_vars = try base.Scratch(FreeVar).init(gpa),
        .seen_annos = std.AutoHashMap(CIR.TypeAnno.Idx, Var).init(gpa),
        .scratch_vars = try base.Scratch(types_mod.Var).init(gpa),
        .scratch_tags = try base.Scratch(types_mod.Tag).init(gpa),
        .scratch_record_fields = try base.Scratch(types_mod.RecordField).init(gpa),
        .anonymous_rigid_var_subs = try Instantiate.RigidToFlexSubs.init(gpa),
        .import_cache = ImportCache{},
        .constraint_origins = std.AutoHashMap(Var, Var).init(gpa),
        .var_pool = try VarPool.init(gpa),
        .generalizer = try Generalizer.init(gpa, types),
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.var_map.deinit();
    self.anno_free_vars.deinit(self.gpa);
    self.decl_free_vars.deinit(self.gpa);
    self.seen_annos.deinit();
    self.anonymous_rigid_var_subs.deinit(self.gpa);
    self.scratch_vars.deinit(self.gpa);
    self.scratch_tags.deinit(self.gpa);
    self.scratch_record_fields.deinit(self.gpa);
    self.import_cache.deinit(self.gpa);
    self.constraint_origins.deinit();
    self.var_pool.deinit();
    self.generalizer.deinit();
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

// import caches //

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

// unify //

/// Unify two types
pub fn unify(self: *Self, a: Var, b: Var, rank: Rank) std.mem.Allocator.Error!unifier.Result {
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

        for (self.unify_scratch.fresh_vars.items.items) |fresh_var| {
            try self.var_pool.addVarToRank(fresh_var, rank);
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
pub fn unifyWithAnnotation(self: *Self, a: Var, b: Var, rank: Rank) std.mem.Allocator.Error!unifier.Result {
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

        for (self.unify_scratch.fresh_vars.items.items) |fresh_var| {
            try self.var_pool.addVarToRank(fresh_var, rank);
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

// instantiate  //

const InstantiateRegionBehavior = union(enum) {
    explicit: Region,
    use_root_instantiated,
    use_last_var,
};

/// Instantiate a variable
fn instantiateVarWithRank(
    self: *Self,
    var_to_instantiate: Var,
    rank: types_mod.Rank,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    self.var_map.clearRetainingCapacity();
    self.anonymous_rigid_var_subs.items.clearRetainingCapacity();

    var instantiate = Instantiate.init(self.types, self.cir.getIdentStore(), &self.var_map);
    var instantiate_ctx = Instantiate.Ctx{
        .rigid_var_subs = &self.anonymous_rigid_var_subs,
        .current_rank = rank,
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

            // Add to pool
            try self.var_pool.addVarToRank(fresh_var, rank);

            // Set the region
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
fn fresh(self: *Self, rank: Rank, new_region: Region) Allocator.Error!Var {
    const var_ = try self.types.freshWithRank(rank);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

/// The the region for a variable
fn freshRedirect(self: *Self, redirect_to: Var, new_region: Region) Allocator.Error!Var {
    const var_ = try self.types.freshRedirect(redirect_to);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

/// The the region for a variable
fn freshFromContent(self: *Self, content: Content, rank: types_mod.Rank, new_region: Region) Allocator.Error!Var {
    const var_ = try self.types.freshFromContentWithRank(content, rank);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    return var_;
}

/// The the region for a variable
fn freshBool(self: *Self, new_region: Region) Allocator.Error!Var {
    return try self.generateTypeDeclInstance(&.{}, new_region, can.Can.BUILTIN_BOOL);
}

// fresh vars //

fn updateVar(self: *Self, target_var: Var, content: types_mod.Content, rank: types_mod.Rank) std.mem.Allocator.Error!void {
    try self.types.setVarDesc(target_var, .{ .content = content, .rank = rank, .mark = types_mod.Mark.none });
}

// file //

/// Check the types for all defs
pub fn checkFile(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stms_slice = self.cir.store.sliceStatements(self.cir.all_statements);
    for (stms_slice) |stm_idx| {
        const stmt = self.cir.store.getStatement(stm_idx);
        switch (stmt) {
            .s_alias_decl => {
                // TODO: Check decl and cache type?

            },
            .s_nominal_decl => {
                // TODO: Check decl and cache type?

            },
            else => {},
        }
    }

    try self.checkDefs();
}

// repl //

/// Check an expr for the repl
pub fn checkExprRepl(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    // Push the rank for this definitoin
    try self.var_pool.pushRank();
    defer self.var_pool.popRank();

    // Ensure that the current rank in the pool is top-level
    const rank = types_mod.Rank.top_level;
    std.debug.assert(rank == self.var_pool.current_rank);

    _ = try self.checkExprNew(expr_idx, rank, .no_expectation);

    // Now that we are existing the scope, we must generalize then pop this rank
    try self.generalizer.generalize(&self.var_pool, rank);
}

// defs //

/// Check the types for all defs
fn checkDefs(self: *Self) std.mem.Allocator.Error!void {
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

    // Push the rank for this definitoin
    try self.var_pool.pushRank();
    defer self.var_pool.popRank();

    // Ensure that the current rank in the pool is top-level
    const rank = types_mod.Rank.top_level;
    std.debug.assert(rank == self.var_pool.current_rank);

    const def = self.cir.store.getDef(def_idx);

    // Check the pattern
    try self.checkPatternNew(def.pattern, rank, .no_expectation);

    // Get the defs var slot
    const def_var = ModuleEnv.varFrom(def_idx);

    // Handle if there's an annotation associated with this def
    if (def.annotation) |anno_idx| {
        const annotation = self.cir.store.getAnnotation(anno_idx);

        self.anno_free_vars.items.clearRetainingCapacity();
        const anno_var = try self.generateAnnoType(
            FreeVarCtx{ .scratch = &self.anno_free_vars, .start = 0, .mode = .rigid },
            annotation.type_anno,
        );

        _ = try self.checkExprNew(def.expr, rank, .{
            .expected = .{ .var_ = anno_var, .from_annotation = true },
        });
    } else {
        _ = try self.checkExprNew(def.expr, rank, .no_expectation);
    }

    // Unify the def with its expression
    _ = try self.unify(def_var, ModuleEnv.varFrom(def.expr), rank);

    // Also unify the pattern with the def - needed so lookups work correctly
    // TODO could we unify directly with the pattern elsewhere, to save a type var and unify() here?
    _ = try self.unify(ModuleEnv.varFrom(def.pattern), def_var, rank);

    // Now that we are existing the scope, we must generalize then pop this rank
    try self.generalizer.generalize(&self.var_pool, rank);
}

// annotations //

/// The context use for free var generation
const FreeVarCtx = struct {
    scratch: *base.Scratch(FreeVar),
    start: usize,
    mode: enum { flex, rigid },

    // TODO: Do we need rank here?

    pub fn sliceFreeVars(self: *const @This()) []FreeVar {
        return self.scratch.items.items[self.start..];
    }
};

/// Given an annotation, generate the corrosponding type based on the CIR
///
/// This function will write the type into the type var node at `anno_idx`
fn generateAnnoType(self: *Self, free_vars_ctx: FreeVarCtx, anno_idx: CIR.TypeAnno.Idx) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First, check if we've seen this anno before
    // This guards against recursive types
    if (self.seen_annos.get(anno_idx)) |var_| {
        return var_;
    }

    // Get the annotation
    const anno = self.cir.store.getTypeAnno(anno_idx);
    const anno_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(anno_idx));

    // Create a placeholder and put it into the seen variables
    const placeholder_var = try self.fresh(Rank.generalized, anno_region);
    try self.seen_annos.put(anno_idx, placeholder_var);

    const anno_var = blk: {
        switch (anno) {
            .rigid_var => |rigid| {
                // If we have a rigid var, first check if we've seen it before
                // If so, we redirect this instance to the previously seen instance
                for (free_vars_ctx.sliceFreeVars()) |cached_var| {
                    if (cached_var.ident.idx == rigid.name.idx) {
                        break :blk try self.freshRedirect(cached_var.var_, anno_region);
                    }
                }

                // If this is the first time we've seen this var, create it and add
                // it to the cache
                const var_ = inner_blk: {
                    switch (free_vars_ctx.mode) {
                        .rigid => break :inner_blk try self.freshFromContent(.{ .rigid_var = rigid.name }, Rank.generalized, anno_region),
                        .flex => break :inner_blk try self.fresh(Rank.generalized, anno_region),
                    }
                };
                try free_vars_ctx.scratch.append(self.gpa, .{ .ident = rigid.name, .var_ = var_ });
                break :blk var_;
            },
            .underscore => {
                break :blk try self.fresh(Rank.generalized, anno_region);
            },
            .lookup => |lookup| {
                switch (lookup.base) {
                    .builtin => |builtin_type| {
                        break :blk try self.generateBuiltinTypeInstance(lookup.name, builtin_type, &.{}, anno_region);
                    },
                    .local => |local| {
                        break :blk try self.generateTypeDeclInstance(&.{}, anno_region, local.decl_idx);
                    },
                    .external => |_| {
                        @panic("TODO: External type lookups");
                        // // TODO External
                        // const resolved_external = try self.resolveVarFromExternal(external.module_idx, external.target_node_idx) orelse {
                        //     // TODO?
                        //     break :blk try self.freshFromContent(.err, Rank.generalized, anno_region);
                        // };
                        // break :blk try self.instantiateVarAnon(resolved_external.local_var, .{ .explicit = anno_region });
                    },
                }
            },
            .apply => |a| {
                const scratch_vars_top = self.scratch_vars.top();
                defer self.scratch_vars.clearFrom(scratch_vars_top);

                // Generate the types for the arguments
                const anno_args = self.cir.store.sliceTypeAnnos(a.args);
                for (anno_args) |anno_arg| {
                    try self.scratch_vars.append(self.gpa, try self.generateAnnoType(
                        free_vars_ctx,
                        anno_arg,
                    ));
                }
                const args_var_slice = self.scratch_vars.sliceFromStart(scratch_vars_top);

                switch (a.base) {
                    .builtin => |builtin_type| {
                        break :blk try self.generateBuiltinTypeInstance(a.name, builtin_type, args_var_slice, anno_region);
                    },
                    .local => |local| {
                        break :blk try self.generateTypeDeclInstance(args_var_slice, anno_region, local.decl_idx);
                    },
                    .external => |_| {
                        @panic("TODO: External type apply");
                        // // TODO External
                        // const resolved_external = try self.resolveVarFromExternal(external.module_idx, external.target_node_idx) orelse {
                        //     // TODO?
                        //     break :blk try self.freshFromContent(.err, Rank.generalized, anno_region);
                        // };
                        // break :blk try self.instantiateVarAnon(resolved_external.local_var, .{ .explicit = anno_region });
                    },
                }
            },
            .@"fn" => |func| {
                const scratch_vars_top = self.scratch_vars.top();
                defer self.scratch_vars.clearFrom(scratch_vars_top);

                const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
                for (args_anno_slice) |arg_anno_idx| {
                    try self.scratch_vars.append(self.gpa, try self.generateAnnoType(
                        free_vars_ctx,
                        arg_anno_idx,
                    ));
                }
                const args_var_slice = self.scratch_vars.sliceFromStart(scratch_vars_top);

                const fn_ret_var = try self.generateAnnoType(free_vars_ctx, func.ret);

                const fn_type = inner_blk: {
                    if (func.effectful) {
                        break :inner_blk try self.types.mkFuncEffectful(args_var_slice, fn_ret_var);
                    } else {
                        break :inner_blk try self.types.mkFuncPure(args_var_slice, fn_ret_var);
                    }
                };
                break :blk try self.freshFromContent(fn_type, Rank.generalized, anno_region);
            },
            .tag_union => |tag_union| {
                const scratch_tags_top = self.scratch_tags.top();
                defer self.scratch_tags.clearFrom(scratch_tags_top);

                const tag_anno_slices = self.cir.store.sliceTypeAnnos(tag_union.tags);
                for (tag_anno_slices) |tag_anno_idx| {
                    // Get the tag anno
                    const tag_type_anno = self.cir.store.getTypeAnno(tag_anno_idx);
                    std.debug.assert(tag_type_anno == .tag);
                    const tag = tag_type_anno.tag;

                    const scratch_vars_top = self.scratch_vars.top();
                    defer self.scratch_vars.clearFrom(scratch_vars_top);

                    // Generate the types for each tag arg
                    const tag_anno_args_slice = self.cir.store.sliceTypeAnnos(tag.args);
                    for (tag_anno_args_slice) |tag_arg_idx| {
                        try self.scratch_vars.append(self.gpa, try self.generateAnnoType(
                            free_vars_ctx,
                            tag_arg_idx,
                        ));
                    }
                    const tag_vars_slice = self.scratch_vars.sliceFromStart(scratch_vars_top);

                    // Add the processed tag to scratch
                    try self.scratch_tags.append(self.gpa, try self.types.mkTag(
                        tag.name,
                        tag_vars_slice,
                    ));
                }

                // Get the slice of tags
                const tags_slice = self.scratch_tags.sliceFromStart(scratch_tags_top);
                std.mem.sort(types_mod.Tag, tags_slice, self.cir.common.getIdentStore(), comptime types_mod.Tag.sortByNameAsc);

                // Process the ext if it exists. Absence means it's a closed union
                const ext_var = inner_blk: {
                    if (tag_union.ext) |ext_anno_idx| {
                        break :inner_blk try self.generateAnnoType(free_vars_ctx, ext_anno_idx);
                    } else {
                        break :inner_blk try self.freshFromContent(.{ .structure = .empty_tag_union }, Rank.generalized, anno_region);
                    }
                };

                // Create the type for the anno in the store
                break :blk try self.freshFromContent(try self.types.mkTagUnion(tags_slice, ext_var), Rank.generalized, anno_region);
            },
            .tag => {
                // This indicates a malformed type annotation. Tags should only
                // exist as direct childen of tag_unions
                std.debug.assert(false);
                break :blk try self.freshFromContent(.err, Rank.generalized, anno_region);
            },
            .record => |rec| {
                const scratch_record_fields_top = self.scratch_record_fields.top();
                defer self.scratch_record_fields.clearFrom(scratch_record_fields_top);

                const recs_anno_slice = self.cir.store.sliceAnnoRecordFields(rec.fields);

                for (recs_anno_slice) |rec_anno_idx| {
                    const rec_field = self.cir.store.getAnnoRecordField(rec_anno_idx);

                    const record_field_var = try self.generateAnnoType(free_vars_ctx, rec_field.ty);

                    // Add the processed tag to scratch
                    try self.scratch_record_fields.append(self.gpa, types_mod.RecordField{
                        .name = rec_field.name,
                        .var_ = record_field_var,
                    });
                }

                // Get the slice of record_fields
                const record_fields_slice = self.scratch_record_fields.sliceFromStart(scratch_record_fields_top);
                std.mem.sort(types_mod.RecordField, record_fields_slice, self.cir.common.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
                const fields_type_range = try self.types.appendRecordFields(record_fields_slice);

                // Process the ext if it exists. Absence means it's a closed union
                // TODO: Capture ext in record field CIR
                // const ext_var = inner_blk: {
                //     if (rec.ext) |ext_anno_idx| {
                //         try self.generateAnnoType(rigid_vars_ctx, ext_anno_idx);
                //         break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                //     } else {
                //         break :inner_blk try self.freshFromContent(.{ .structure = .empty_record }, Rank.generalized, anno_region);
                //     }
                // };
                const ext_var = try self.freshFromContent(.{ .structure = .empty_record }, Rank.generalized, anno_region);

                // Create the type for the anno in the store
                break :blk try self.freshFromContent(
                    .{ .structure = types_mod.FlatType{ .record = .{
                        .fields = fields_type_range,
                        .ext = ext_var,
                    } } },
                    Rank.generalized,
                    anno_region,
                );
            },
            .tuple => |tuple| {
                const scratch_vars_top = self.scratch_vars.top();
                defer self.scratch_vars.clearFrom(scratch_vars_top);

                const elems_anno_slice = self.cir.store.sliceTypeAnnos(tuple.elems);
                for (elems_anno_slice) |arg_anno_idx| {
                    try self.scratch_vars.append(self.gpa, try self.generateAnnoType(
                        free_vars_ctx,
                        arg_anno_idx,
                    ));
                }
                const elems_range = try self.types.appendVars(@ptrCast(elems_anno_slice));
                break :blk try self.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems_range } } }, Rank.generalized, anno_region);
            },
            .parens => |parens| {
                break :blk try self.generateAnnoType(free_vars_ctx, parens.anno);
            },
            .malformed => {
                break :blk try self.freshFromContent(.err, Rank.generalized, anno_region);
            },
        }
    };

    try self.types.setVarRedirect(placeholder_var, anno_var);
    return placeholder_var;
}

/// Generate a type variable from the provided type declaration, substituting
/// the type arguments in the declaration with the type arguments provided
///
/// Steps:
/// 1. Lookup the provided declaration
/// 2. Assert arities from the decl match the actual provided args from the annotation
/// 3. Iterate over the decl args & the actual args together
///     * Build up a substituion map of decl_arg -> actual_arg
/// 4. Generate the instance of the decl type using the substitution map
fn generateTypeDeclInstance(
    self: *Self,
    anno_args: []Var,
    anno_region: Region,
    decl_idx: CIR.Statement.Idx,
) std.mem.Allocator.Error!Var {
    const decl_free_vars_top = self.decl_free_vars.top();
    defer self.decl_free_vars.clearFrom(decl_free_vars_top);

    const decl = self.cir.store.getStatement(decl_idx);
    switch (decl) {
        .s_alias_decl => |alias| {
            // Get the type header's args
            const header = self.cir.store.getTypeHeader(alias.header);
            const header_args = self.cir.store.sliceTypeAnnos(header.args);

            // Then check arity
            if (header_args.len != anno_args.len) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = header.name,
                    .region = anno_region,
                    .num_expected_args = @intCast(header_args.len),
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Next, generate the provided arg types and build the map of rigid variables in the header
            for (header_args, anno_args) |header_arg_idx, anno_arg_var| {
                const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
                switch (header_arg) {
                    .rigid_var => |rigid| {
                        // Then, if it is a rigid var, add to our mapping
                        try self.decl_free_vars.append(self.gpa, .{ .ident = rigid.name, .var_ = anno_arg_var });
                    },
                    else => {},
                }
            }

            // Now we have a built of list of rigid variables for the decl lhs (header).
            // With this in hand, we can now generate the type for the lhs (body).
            const backing_var = try self.generateAnnoType(
                FreeVarCtx{ .scratch = &self.decl_free_vars, .start = decl_free_vars_top, .mode = .rigid },
                alias.anno,
            );

            return try self.freshFromContent(try self.types.mkAlias(
                .{ .ident_idx = header.name },
                backing_var,
                anno_args,
            ), Rank.generalized, anno_region);
        },
        .s_nominal_decl => |nominal| {
            // Get the type header's args
            const header = self.cir.store.getTypeHeader(nominal.header);
            const header_args = self.cir.store.sliceTypeAnnos(header.args);

            // Then check arity
            if (header_args.len != anno_args.len) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = header.name,
                    .region = anno_region,
                    .num_expected_args = @intCast(header_args.len),
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Next, generate the provided arg types and build the map of rigid variables in the header
            for (header_args, anno_args) |header_arg_idx, anno_arg_var| {
                const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
                switch (header_arg) {
                    .rigid_var => |rigid| {
                        // Then, if it is a rigid var, add to our mapping
                        try self.decl_free_vars.append(self.gpa, .{ .ident = rigid.name, .var_ = anno_arg_var });
                    },
                    else => {},
                }
            }

            // Now we have a built of list of rigid variables for the decl lhs (header).
            // With this in hand, we can now generate the type for the lhs (body).
            //
            // This is necessary because even though nominal types unify only by
            // name, args, and origin module, if the rhs of the nominal type is
            // invalid (ie an .err) then that error must propgate "through" the
            // nominal type. So the whole thing must be  materialized here.
            //
            // TODO: Should this be flex instead of rigid?
            const backing_var = try self.generateAnnoType(
                FreeVarCtx{ .scratch = &self.decl_free_vars, .start = decl_free_vars_top, .mode = .rigid },
                nominal.anno,
            );

            return try self.freshFromContent(try self.types.mkNominal(
                .{ .ident_idx = header.name },
                backing_var,
                anno_args,
                self.common_idents.module_name,
            ), Rank.generalized, anno_region);
        },
        .s_runtime_error => {
            return try self.freshFromContent(.err, Rank.generalized, anno_region);
        },
        else => {
            // This indicates a malformed type anno, where the decl is not a real decl
            std.debug.assert(false);
            return try self.freshFromContent(.err, Rank.generalized, anno_region);
        },
    }
}

/// Generate a type variable from the builtin
///
/// Writes the resulting type into the slot at `ret_var`
fn generateBuiltinTypeInstance(
    self: *Self,
    anno_builtin_name: Ident.Idx,
    anno_builtin_type: CIR.TypeAnno.Builtin,
    anno_args: []Var,
    anno_region: Region,
) std.mem.Allocator.Error!Var {
    switch (anno_builtin_type) {
        .str => return try self.freshFromContent(.{ .structure = .str }, Rank.generalized, anno_region),
        .u8 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u8 } }, Rank.generalized, anno_region),
        .u16 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u16 } }, Rank.generalized, anno_region),
        .u32 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u32 } }, Rank.generalized, anno_region),
        .u64 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u64 } }, Rank.generalized, anno_region),
        .u128 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_u128 } }, Rank.generalized, anno_region),
        .i8 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i8 } }, Rank.generalized, anno_region),
        .i16 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i16 } }, Rank.generalized, anno_region),
        .i32 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i32 } }, Rank.generalized, anno_region),
        .i64 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i64 } }, Rank.generalized, anno_region),
        .i128 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.int_i128 } }, Rank.generalized, anno_region),
        .f32 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.frac_f32 } }, Rank.generalized, anno_region),
        .f64 => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.frac_f64 } }, Rank.generalized, anno_region),
        .dec => return try self.freshFromContent(.{ .structure = .{ .num = types_mod.Num.frac_dec } }, Rank.generalized, anno_region),
        .list => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Create the type
            return try self.freshFromContent(.{ .structure = .{ .list = anno_args[0] } }, Rank.generalized, anno_region);
        },
        .box => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Create the type
            return try self.freshFromContent(.{ .structure = .{ .box = anno_args[0] } }, Rank.generalized, anno_region);
        },
        .num => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Create the type
            return try self.freshFromContent(.{ .structure = .{
                .num = .{ .num_poly = .{
                    .var_ = anno_args[0],
                    .int_requirements = Num.IntRequirements.init(),
                    .frac_requirements = Num.FracRequirements.init(),
                } },
            } }, Rank.generalized, anno_region);
        },
        .frac => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Create the type
            const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .frac_unbound } }, Rank.generalized, anno_region);
            return try self.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = .{
                .var_ = frac_var,
                .int_requirements = Num.IntRequirements.init(),
                .frac_requirements = Num.FracRequirements.init(),
            } } } }, Rank.generalized, anno_region);
        },
        .int => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error and return
                return try self.freshFromContent(.err, Rank.generalized, anno_region);
            }

            // Create the type
            const int_var = try self.freshFromContent(.{ .structure = .{ .num = .int_unbound } }, Rank.generalized, anno_region);
            return try self.freshFromContent(.{ .structure = .{ .num = .{ .num_poly = .{
                .var_ = int_var,
                .int_requirements = Num.IntRequirements.init(),
                .frac_requirements = Num.FracRequirements.init(),
            } } } }, Rank.generalized, anno_region);
        },
    }
}

// expr NEW //

pub const Expected = union(enum) {
    no_expectation,
    expected: struct { var_: Var, from_annotation: bool },
};

fn checkExprNew(self: *Self, expr_idx: CIR.Expr.Idx, rank: types_mod.Rank, expected: Expected) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(rank == self.var_pool.current_rank);

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_var = ModuleEnv.varFrom(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));

    var does_fx = false; // Does this expression potentially perform any side effects?

    switch (expr) {
        // str //
        .e_str_segment => |_| {
            try self.updateVar(expr_var, .{ .structure = .str }, rank);
        },
        .e_str => |str| {
            // Iterate over the string segments, capturing if any error'd
            const segment_expr_idx_slice = self.cir.store.sliceExpr(str.span);
            var did_err = false;
            for (segment_expr_idx_slice) |seg_expr_idx| {
                // Check the segment
                does_fx = try self.checkExprNew(seg_expr_idx, rank, .no_expectation) or does_fx;

                // Check if it errored
                const seg_var = ModuleEnv.varFrom(seg_expr_idx);
                did_err = did_err or self.types.resolveVar(seg_var).desc.content == .err;
            }

            if (did_err) {
                // If any segment errored, propgate that error to the root string
                try self.updateVar(expr_var, .err, rank);
            } else {
                // Otherwise, set the type of this expr to be string
                try self.updateVar(expr_var, .{ .structure = .str }, rank);
            }
        },
        // nums //
        .e_num => |num| {
            // TODO: Derive requirements here?
            try self.updateVar(expr_var, .{ .structure = .{ .num = .{
                .num_unbound = .{
                    .int_requirements = num.requirements,
                    .frac_requirements = Num.FracRequirements.init(),
                },
            } } }, rank);
        },
        .e_int => |num| {
            // TODO: Derive requirements here?
            if (num.suffix) |suffix| {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .int = suffix } } } }, rank);
            } else {
                const int_var = try self.freshFromContent(.{ .structure = .{ .num = .int_unbound } }, rank, expr_region);
                try self.var_pool.addVarToRank(int_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_poly = .{
                    .var_ = int_var,
                    .int_requirements = num.requirements,
                    .frac_requirements = Num.FracRequirements.init(),
                } } } }, rank);
            }
        },
        .e_frac_f32 => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } }, rank);
            } else {
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = true,
                    .fits_in_dec = can.Can.fitsInDec(@floatCast(frac.value)),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .frac_unbound } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_poly = .{
                    .var_ = frac_var,
                    .int_requirements = Num.IntRequirements.init(),
                    .frac_requirements = requirements,
                } } } }, rank);
            }
        },
        .e_frac_f64 => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } }, rank);
            } else {
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.Can.fitsInF32(@floatCast(frac.value)),
                    .fits_in_dec = can.Can.fitsInDec(@floatCast(frac.value)),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .frac_unbound } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_poly = .{
                    .var_ = frac_var,
                    .int_requirements = Num.IntRequirements.init(),
                    .frac_requirements = requirements,
                } } } }, rank);
            }
        },
        .e_frac_dec => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const f64_val = frac.value.toF64();
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.Can.fitsInF32(f64_val),
                    .fits_in_dec = can.Can.fitsInDec(f64_val),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .frac_unbound } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_poly = .{
                    .var_ = frac_var,
                    .int_requirements = Num.IntRequirements.init(),
                    .frac_requirements = requirements,
                } } } }, rank);
            }
        },
        .e_dec_small => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const f64_val = frac.toF64();
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.Can.fitsInF32(f64_val),
                    .fits_in_dec = can.Can.fitsInDec(f64_val),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .frac_unbound } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_poly = .{
                    .var_ = frac_var,
                    .int_requirements = Num.IntRequirements.init(),
                    .frac_requirements = requirements,
                } } } }, rank);
            }
        },
        // list //
        .e_list => |list| {
            const elems = self.cir.store.exprSlice(list.elems);

            if (elems.len == 0) {
                // If we have no elems, then set the type and move on
                try self.updateVar(expr_var, .{ .structure = .list_unbound }, rank);
            } else {
                // Here, we use the list's 1st element as the element var to
                // constrain the rest of the list

                // Check the first elem
                does_fx = try self.checkExprNew(elems[0], rank, .no_expectation) or does_fx;

                // Iterate over the remaining elements
                const elem_var = ModuleEnv.varFrom(elems[0]);
                var last_elem_expr_idx = elems[0];
                for (elems[1..], 1..) |elem_expr_idx, i| {
                    does_fx = try self.checkExprNew(elem_expr_idx, rank, .no_expectation) or does_fx;
                    const cur_elem_var = ModuleEnv.varFrom(elem_expr_idx);

                    // Unify each element's var with the list's elem var
                    const result = try self.unify(elem_var, cur_elem_var, rank);
                    self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                        .last_elem_expr = last_elem_expr_idx,
                        .incompatible_elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            does_fx = try self.checkExprNew(remaining_elem_expr_idx, rank, .no_expectation) or does_fx;
                        }

                        // Break to avoid cascading errors
                        break;
                    }

                    last_elem_expr_idx = elem_expr_idx;
                }

                try self.updateVar(expr_var, .{ .structure = .{ .list = elem_var } }, rank);
            }
        },
        // tuple //
        .e_tuple => |tuple| {
            // Check tuple elements
            const elems_slice = self.cir.store.exprSlice(tuple.elems);
            for (elems_slice) |single_elem_expr_idx| {
                does_fx = try self.checkExprNew(single_elem_expr_idx, rank, .no_expectation) or does_fx;
                try self.scratch_vars.append(self.gpa, ModuleEnv.varFrom(single_elem_expr_idx));
            }

            // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
            const elem_vars_slice = try self.types.appendVars(@ptrCast(elems_slice));

            // Set the type in the store
            try self.updateVar(expr_var, .{ .structure = .{
                .tuple = .{ .elems = elem_vars_slice },
            } }, rank);
        },
        // record //
        .e_record => |e| {
            // Create a record type in the type system and assign it the expr_var

            // Write down the top of the scratch records array
            const record_fields_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(record_fields_top);

            // Process each field
            for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
                const field = self.cir.store.getRecordField(field_idx);

                // Check the field value expression
                does_fx = try self.checkExprNew(field.value, rank, .no_expectation) or does_fx;

                // Append it to the scratch records array
                try self.scratch_record_fields.append(self.gpa, types_mod.RecordField{
                    .name = field.name,
                    .var_ = ModuleEnv.varFrom(field.value),
                });
            }

            // Copy the scratch fields into the types store
            const record_fields_range = try self.types.appendRecordFields(
                self.scratch_record_fields.sliceFromStart(record_fields_top),
            );

            // Check if we have an ext
            if (e.ext) |ext_expr| {
                does_fx = try self.checkExprNew(ext_expr, rank, .no_expectation) or does_fx;
                try self.updateVar(expr_var, .{ .structure = .{ .record = .{
                    .ext = ModuleEnv.varFrom(ext_expr),
                    .fields = record_fields_range,
                } } }, rank);
            } else {
                try self.updateVar(expr_var, .{ .structure = .{
                    .record_unbound = record_fields_range,
                } }, rank);
            }
        },
        .e_empty_record => {
            try self.updateVar(expr_var, .{ .structure = .{
                .record_unbound = types_mod.RecordField.SafeMultiList.Range.empty(),
            } }, rank);
        },
        // tags //
        .e_tag => |e| {
            // Create a tag type in the type system and assign it the expr_var

            // Process each tag arg
            const arg_expr_idx_slice = self.cir.store.sliceExpr(e.args);
            for (arg_expr_idx_slice) |arg_expr_idx| {
                does_fx = try self.checkExprNew(arg_expr_idx, rank, .no_expectation) or does_fx;
            }

            // Create the type
            const ext_var = try self.fresh(rank, expr_region);
            try self.var_pool.addVarToRank(ext_var, rank);

            const tag = try self.types.mkTag(e.name, @ptrCast(arg_expr_idx_slice));
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.updateVar(expr_var, tag_union_content, rank);
        },
        // nominal //
        .e_nominal => |nominal| blk: {
            // First, check the type inside the expr
            does_fx = try self.checkExprNew(nominal.backing_expr, rank, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            // Then, we need to generate an instnace of the type of the nominal
            // type that this expr is referencing
            const nominal_backing_var, const nominal_var = inner_blk: {
                const stmt = self.cir.store.getStatement(nominal.nominal_type_decl);
                std.debug.assert(stmt == .s_nominal_decl);
                const nominal_stmt = stmt.s_nominal_decl;

                // Then, get the headers
                const nominal_header = self.cir.store.getTypeHeader(nominal_stmt.header);
                const nominal_header_args = self.cir.store.sliceTypeAnnos(nominal_header.args);

                // Setup our scratch arrays
                const decl_free_vars_top = self.decl_free_vars.top();
                defer self.decl_free_vars.clearFrom(decl_free_vars_top);

                const scratch_vars_top = self.scratch_vars.top();
                defer self.scratch_vars.clearFrom(scratch_vars_top);

                // Next, create a map of header variables to flex vars
                // We want flex var here because the actual arguments will be inferred from use
                for (nominal_header_args) |header_arg_idx| {
                    const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
                    switch (header_arg) {
                        .rigid_var => |rigid| {
                            // Then, if it is a rigid var, add to our mapping
                            const arg_var = try self.fresh(rank, expr_region);
                            try self.var_pool.addVarToRank(arg_var, rank);

                            try self.decl_free_vars.append(self.gpa, .{ .ident = rigid.name, .var_ = arg_var });
                            try self.scratch_vars.append(self.gpa, arg_var);
                        },
                        else => {
                            // If we had anything else as a header var, this nominal
                            // type is malformed. Canonicalization should have found
                            // this and reported a diagnostic. So here, we just set
                            // the entire expr to be an error and break.
                            try self.updateVar(expr_var, .err, rank);
                            break :blk;
                        },
                    }
                }

                // Now, create the actual backing variable
                const backing_var = try self.generateAnnoType(
                    FreeVarCtx{ .scratch = &self.decl_free_vars, .start = decl_free_vars_top, .mode = .rigid },
                    nominal_stmt.anno,
                );
                const nominal_var = try self.freshFromContent(try self.types.mkNominal(
                    .{ .ident_idx = nominal_header.name },
                    backing_var,
                    self.scratch_vars.sliceFromStart(scratch_vars_top),
                    self.common_idents.module_name,
                ), rank, expr_region);
                try self.var_pool.addVarToRank(nominal_var, rank);

                break :inner_blk .{ backing_var, nominal_var };
            };

            // Now, we unify what's inside the nominal type with what's inside
            // the nominal expr
            const result = try self.unify(nominal_backing_var, actual_backing_var, rank);

            // Step 4: Handle the unification result
            switch (result) {
                .ok => {
                    // If that unify call succeeded, then we this is a valid instance
                    // of this nominal type. So we set the expr's type to be the
                    // nominal type
                    try self.types.setVarRedirect(expr_var, nominal_var);
                },
                .problem => |problem_idx| {
                    // Unification failed - the constructor is incompatible with the nominal type
                    // Set a specific error message based on the backing type kind
                    switch (nominal.backing_type) {
                        .tag => {
                            // Constructor doesn't exist or has wrong arity/types
                            self.setProblemTypeMismatchDetail(problem_idx, .invalid_nominal_tag);
                        },
                        else => {
                            // TODO: Add specific error messages for records, tuples, etc.
                        },
                    }

                    // Mark the entire expression as having a type error
                    try self.updateVar(expr_var, .err, rank);
                },
            }
        },
        // lookup //
        .e_lookup_local => |lookup| {
            const pat_var = ModuleEnv.varFrom(lookup.pattern_idx);
            const resolved_pat = self.types.resolveVar(pat_var).desc;

            if (resolved_pat.rank == Rank.generalized) {
                const instantiated = try self.instantiateVarWithRank(pat_var, rank, .use_last_var);
                _ = try self.types.setVarRedirect(expr_var, instantiated);
            } else {
                _ = try self.types.setVarRedirect(expr_var, pat_var);
            }

            // Unify this expression with the referenced pattern
        },
        // block //
        .e_block => |block| {
            const anno_free_vars_top = self.anno_free_vars.top();
            defer self.anno_free_vars.clearFrom(anno_free_vars_top);

            // Enter a new rank
            try self.var_pool.pushRank();
            defer self.var_pool.popRank();

            const next_rank = rank.next();
            std.debug.assert(next_rank == self.var_pool.current_rank);

            // Check all statements in the block
            const statements = self.cir.store.sliceStatements(block.stmts);
            for (statements) |stmt_idx| {
                const stmt = self.cir.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl_stmt| {
                        // Check the pattern
                        try self.checkPatternNew(decl_stmt.pattern, next_rank, .no_expectation);
                        const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                        // Check the annotation, if it exists
                        const check_mode = blk: {
                            if (decl_stmt.anno) |anno_idx| {
                                const annotation = self.cir.store.getAnnotation(anno_idx);
                                const anno_var = try self.generateAnnoType(
                                    FreeVarCtx{ .scratch = &self.anno_free_vars, .start = 0, .mode = .rigid },
                                    annotation.type_anno,
                                );
                                break :blk Expected{
                                    .expected = .{ .var_ = anno_var, .from_annotation = true },
                                };
                            } else {
                                break :blk Expected.no_expectation;
                            }
                        };
                        does_fx = try self.checkExprNew(decl_stmt.expr, next_rank, check_mode) or does_fx;
                        const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);

                        // Unify the pattern with the expression
                        _ = try self.unify(decl_pattern_var, decl_expr_var, next_rank);
                    },
                    .s_reassign => |reassign| {
                        does_fx = try self.checkExprNew(reassign.expr, next_rank, .no_expectation) or does_fx;
                    },
                    .s_expr => |expr_stmt| {
                        does_fx = try self.checkExprNew(expr_stmt.expr, next_rank, .no_expectation) or does_fx;
                    },
                    else => {
                        // TODO
                    },
                }
            }

            // Check the final expression
            does_fx = try self.checkExprNew(block.final_expr, next_rank, .no_expectation) or does_fx;

            // Link the root expr with the final expr
            try self.types.setVarRedirect(expr_var, ModuleEnv.varFrom(block.final_expr));

            // Now that we are existing the scope, we must generalize then pop this rank
            try self.generalizer.generalize(&self.var_pool, next_rank);
        },
        // function //
        .e_lambda => |lambda| {
            // Annotation-aware lambda tpye checking produces much better error
            // messages, so first we have to determine if we have an expected
            // type to validate against
            const mb_expected_var: ?Var, const is_expected_from_anno: bool = blk: {
                switch (expected) {
                    .no_expectation => break :blk .{ null, false },
                    .expected => |expected_type| {
                        break :blk .{ expected_type.var_, expected_type.from_annotation };
                    },
                }
            };

            // Then, even if we have an expected type, it may not actually be a function
            const mb_expected_func: ?types_mod.Func = blk: {
                if (mb_expected_var) |expected_var| {
                    // Here, we unwrap the function, following aliases, to get
                    // the actual function we want to check against
                    var var_ = expected_var;
                    while (true) {
                        switch (self.types.resolveVar(var_).desc.content) {
                            .structure => |flat_type| {
                                switch (flat_type) {
                                    .fn_pure => |func| break :blk func,
                                    .fn_unbound => |func| break :blk func,
                                    .fn_effectful => |func| break :blk func,
                                    else => break :blk null,
                                }
                            },
                            .alias => |alias| {
                                var_ = self.types.getAliasBackingVar(alias);
                            },
                            else => break :blk null,
                        }
                    }
                } else {
                    break :blk null;
                }
            };

            // Check the argument patterns
            const arg_pattern_idxs = self.cir.store.slicePatterns(lambda.args);

            // Enter the next rank
            try self.var_pool.pushRank();
            defer self.var_pool.popRank();

            const next_rank = rank.next();
            std.debug.assert(next_rank == self.var_pool.current_rank);

            // Now, check if the expected function has the the same number of
            // arguments as  what the function was actually called with
            if (mb_expected_func) |expected_func| {
                const expected_func_args = self.types.sliceVars(expected_func.args);
                if (expected_func_args.len == arg_pattern_idxs.len) {
                    // If so, check each argument, passing in the expected type
                    for (expected_func_args, arg_pattern_idxs) |expected_arg_var, pattern_idx| {
                        try self.checkPatternNew(pattern_idx, next_rank, .{
                            .expected = .{ .var_ = expected_arg_var, .from_annotation = is_expected_from_anno },
                        });
                    }
                } else {
                    // Otherwise, check the function arguments like normal
                    for (arg_pattern_idxs) |arg_pattern_idx| {
                        try self.checkPatternNew(arg_pattern_idx, next_rank, .no_expectation);
                    }

                    // The arity mismatch error will be caught by the regular
                    // expectation checking code at the bottom of this function
                }
            } else {
                // Otherwise, check the function arguments like normal
                for (arg_pattern_idxs) |arg_pattern_idx| {
                    try self.checkPatternNew(arg_pattern_idx, next_rank, .no_expectation);
                }
            }
            const arg_vars: []Var = @ptrCast(arg_pattern_idxs);

            // Check the the body of the expr
            // If we have an expected function, use that as the expr's expected type
            if (mb_expected_func) |expected_func| {
                does_fx = try self.checkExprNew(lambda.body, next_rank, .{
                    .expected = .{ .var_ = expected_func.ret, .from_annotation = is_expected_from_anno },
                }) or does_fx;
            } else {
                does_fx = try self.checkExprNew(lambda.body, next_rank, .no_expectation) or does_fx;
            }
            const body_var = ModuleEnv.varFrom(lambda.body);

            // Create the function type
            if (does_fx) {
                _ = try self.updateVar(expr_var, try self.types.mkFuncEffectful(arg_vars, body_var), next_rank);
            } else {
                _ = try self.updateVar(expr_var, try self.types.mkFuncUnbound(arg_vars, body_var), next_rank);
            }
            try self.var_pool.addVarToRank(expr_var, next_rank);

            // Note that so far, we have not yet unified against the
            // annotation's effectfulness/pureness. This is intentional!
            // Below this large switch statement, there's the regular expr
            // <-> expected unification. This will catch any difference in
            // effectfullness, and it'll link the root expected var with the
            // expr_var

            // Now that we are existing the scope, we must generalize then pop this rank
            try self.generalizer.generalize(&self.var_pool, next_rank);
        },
        .e_closure => |closure| {
            does_fx = try self.checkExprNew(closure.lambda_idx, rank, expected) or does_fx;
            _ = try self.types.setVarRedirect(expr_var, ModuleEnv.varFrom(closure.lambda_idx));
        },
        // function calling //
        .e_call => |call| {
            switch (call.called_via) {
                .apply => blk: {
                    // First, check the function being called
                    // It could be effectful, e.g. `(mk_fn!())(arg)`
                    does_fx = try self.checkExprNew(call.func, rank, .no_expectation) or does_fx;
                    const func_var = ModuleEnv.varFrom(call.func);

                    // Second, check the arguments being called
                    // It could be effectful, e.g. `fn(mk_arg!())`
                    const call_arg_expr_idxs = self.cir.store.sliceExpr(call.args);
                    for (call_arg_expr_idxs) |call_arg_idx| {
                        does_fx = try self.checkExprNew(call_arg_idx, rank, .no_expectation) or does_fx;
                    }

                    // From the base function type, extract the actual function  info
                    const mb_func: ?types_mod.Func = inner_blk: {
                        // Here, we unwrap the function, following aliases, to get
                        // the actual function we want to check against
                        var var_ = func_var;
                        while (true) {
                            switch (self.types.resolveVar(var_).desc.content) {
                                .structure => |flat_type| {
                                    switch (flat_type) {
                                        .fn_pure => |func| break :inner_blk func,
                                        .fn_unbound => |func| break :inner_blk func,
                                        .fn_effectful => |func| break :inner_blk func,
                                        else => break :inner_blk null,
                                    }
                                },
                                .alias => |alias| {
                                    var_ = self.types.getAliasBackingVar(alias);
                                },
                                else => break :inner_blk null,
                            }
                        }
                    };

                    // Get the name of the function (for error messages)
                    const func_name: ?Ident.Idx = inner_blk: {
                        const func_expr = self.cir.store.getExpr(call.func);
                        switch (func_expr) {
                            .e_lookup_local => |lookup| {
                                // Get the pattern that defines this identifier
                                const pattern = self.cir.store.getPattern(lookup.pattern_idx);
                                switch (pattern) {
                                    .assign => |assign| break :inner_blk assign.ident,
                                    else => break :inner_blk null,
                                }
                            },
                            else => break :inner_blk null,
                        }
                    };

                    // Now, check the call args against the type of function
                    if (mb_func) |func| {
                        const func_args = self.types.sliceVars(func.args);

                        if (func_args.len == call_arg_expr_idxs.len) {

                            // First, find all the "rigid" variables in a the function's type
                            // and unify the matching corrosponding call arguments together.
                            //
                            // Here, "rigid" is in quotes because at this point, the expected function
                            // has been instantiated such that the rigid variables should all resolve
                            // to the same exact flex variable. So we are actually checking for flex
                            // variables here.
                            for (func_args, 0..) |expected_arg_1, i| {
                                const expected_resolved_1 = self.types.resolveVar(expected_arg_1);

                                // Ensure the above comment is true. That is, that all
                                // rigid vars for this function have been instantiated to
                                // flex vars by the time we get here.
                                std.debug.assert(expected_resolved_1.desc.content != .rigid_var);

                                // Skip any concrete arguments
                                if (expected_resolved_1.desc.content != .flex_var) {
                                    continue;
                                }

                                // Look for other arguments with the same type variable
                                for (func_args[i + 1 ..], i + 1..) |expected_arg_2, j| {
                                    const expected_resolved_2 = self.types.resolveVar(expected_arg_2);
                                    if (expected_resolved_2.var_ == expected_resolved_2.var_) {
                                        // These two argument indexes in the called *function's*
                                        // type have the same rigid variable! So, we unify
                                        // the corrosponding *call args*

                                        const arg_1 = @as(Var, ModuleEnv.varFrom(call_arg_expr_idxs[i]));
                                        const arg_2 = @as(Var, ModuleEnv.varFrom(call_arg_expr_idxs[j]));

                                        const unify_result = try self.unify(arg_1, arg_2, rank);
                                        if (unify_result.isProblem()) {
                                            // Use the new error detail for bound type variable incompatibility
                                            self.setProblemTypeMismatchDetail(unify_result.problem, .{
                                                .incompatible_fn_args_bound_var = .{
                                                    .fn_name = func_name,
                                                    .first_arg_var = arg_1,
                                                    .second_arg_var = arg_2,
                                                    .first_arg_index = @intCast(i),
                                                    .second_arg_index = @intCast(j),
                                                    .num_args = @intCast(call_arg_expr_idxs.len),
                                                },
                                            });

                                            // Step execution
                                            _ = try self.updateVar(expr_var, .err, rank);
                                            break :blk;
                                        }
                                    }
                                }
                            }

                            // Check the function's arguments against the actual
                            // called arguments, unifying each one
                            for (func_args, call_arg_expr_idxs, 0..) |expected_arg_var, call_expr_idx, arg_index| {
                                does_fx = try self.checkExprNew(call_expr_idx, rank, .no_expectation) or does_fx;

                                const unify_result = try self.unify(expected_arg_var, ModuleEnv.varFrom(call_expr_idx), rank);
                                if (unify_result.isProblem()) {
                                    // Use the new error detail for bound type variable incompatibility
                                    self.setProblemTypeMismatchDetail(unify_result.problem, .{
                                        .incompatible_fn_call_arg = .{
                                            .fn_name = func_name,
                                            .arg_var = ModuleEnv.varFrom(call_expr_idx),
                                            .incompatible_arg_index = @intCast(arg_index),
                                            .num_args = @intCast(call_arg_expr_idxs.len),
                                        },
                                    });

                                    // Step execution
                                    _ = try self.updateVar(expr_var, .err, rank);
                                    break :blk;
                                }
                            }

                            // Redirect the expr to the function's return type
                            _ = try self.types.setVarRedirect(expr_var, func.ret);
                        } else {
                            // TODO(jared): Better arity difference error message

                            // If the expected function's arity doesn't match
                            // the actual arguments provoided, unify the
                            // inferred function tpye with the expected function
                            // type to get  the regulare error message
                            const call_arg_vars: []Var = @ptrCast(call_arg_expr_idxs);
                            const call_func_ret = try self.fresh(rank, expr_region);
                            const call_func_content = try self.types.mkFuncUnbound(call_arg_vars, call_func_ret);
                            const call_func_var = try self.freshFromContent(call_func_content, rank, expr_region);

                            try self.var_pool.addVarToRank(call_func_ret, rank);
                            try self.var_pool.addVarToRank(call_func_var, rank);

                            _ = try self.unify(func_var, call_func_var, rank);
                            _ = try self.types.setVarRedirect(expr_var, call_func_ret);
                        }
                    } else {
                        // We get here if the type of expr being called
                        // (`mk_fn` in `(mk_fn())(arg)`) is NOT already
                        // inferred to be a function type.

                        // This can mean a regular type mismatch, but it can also
                        // mean that the thing being called yet has not yet been
                        // inferred (like if this is an anonymous function param)

                        // Either way, we know what the type  *should* be, based
                        // on how it's being used here. So we create that func
                        // type and unify the function being called against it

                        const call_arg_vars: []Var = @ptrCast(call_arg_expr_idxs);
                        const call_func_ret = try self.fresh(rank, expr_region);
                        const call_func_content = try self.types.mkFuncUnbound(call_arg_vars, call_func_ret);
                        const call_func_var = try self.freshFromContent(call_func_content, rank, expr_region);

                        try self.var_pool.addVarToRank(call_func_ret, rank);
                        try self.var_pool.addVarToRank(call_func_var, rank);

                        _ = try self.unify(func_var, call_func_var, rank);

                        // Then, we set the root expr to redirect to the return
                        // type of that function, since a call expr ultimate
                        // resolve to the  returned type
                        _ = try self.types.setVarRedirect(expr_var, call_func_ret);
                    }
                },
                else => {
                    // No other call types are currently supported in czer
                    std.debug.assert(false);
                    try self.updateVar(expr_var, .err, rank);
                },
            }
        },
        .e_runtime_error => {
            try self.updateVar(expr_var, .err, rank);
        },
        else => {
            try self.updateVar(expr_var, .err, rank);
        },
    }

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyWithAnnotation(expected_type.var_, expr_var, rank);
            } else {
                _ = try self.unify(expected_type.var_, expr_var, rank);
            }
        },
    }

    return does_fx;
}

// pattern //

/// Check the types for the provided pattern
fn checkPatternNew(self: *Self, pattern_idx: CIR.Pattern.Idx, rank: types_mod.Rank, expected: Expected) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern = self.cir.store.getPattern(pattern_idx);
    // const pattern_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(pattern_idx));
    const pattern_var = ModuleEnv.varFrom(pattern_idx);

    switch (pattern) {
        .assign => |_| {
            // In the case of an assigned variable, set it to be a flex var
            try self.updateVar(pattern_var, .{ .flex_var = null }, rank);
        },
        else => {
            // TODO
        },
    }

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyWithAnnotation(expected_type.var_, pattern_var, rank);
            } else {
                _ = try self.unify(expected_type.var_, pattern_var, rank);
            }
        },
    }
}

// pattern OLD //

// /// Check the types for the provided pattern
// pub fn checkPatternaself: *Self, pattern_idx: CIR.Pattern.Idx) std.mem.Allocator.Error!void {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     const pattern = self.cir.store.getPattern(pattern_idx);
//     const pattern_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(pattern_idx));
//     switch (pattern) {
//         .nominal => |p| {
//             const real_nominal_var = ModuleEnv.varFrom(p.nominal_type_decl);
//             const pattern_backing_var = ModuleEnv.varFrom(p.backing_pattern);
//             try self.checkNominal(
//                 ModuleEnv.varFrom(pattern_idx),
//                 pattern_region,
//                 pattern_backing_var,
//                 p.backing_type,
//                 real_nominal_var,
//             );
//         },
//         .nominal_external => |p| {
//             const resolved_external = try self.resolveVarFromExternal(p.module_idx, p.target_node_idx) orelse {
//                 // If we could not copy the type, set error and continue
//                 try self.types.setVarContent(ModuleEnv.varFrom(pattern_idx), .err);
//                 return;
//             };
//             const pattern_backing_var = ModuleEnv.varFrom(p.backing_pattern);
//             try self.checkNominal(
//                 ModuleEnv.varFrom(pattern_idx),
//                 pattern_region,
//                 pattern_backing_var,
//                 p.backing_type,
//                 resolved_external.local_var,
//             );
//         },
//         .int_literal => |_| {
//             // Integer literal patterns have their type constraints (bits_needed, sign_needed)
//             // created during canonicalization. The type variable for this pattern was already
//             // created with the appropriate num_unbound or int_unbound content.
//             // When this pattern is unified with the match scrutinee, the numeric constraints
//             // will be checked and produce NumberDoesNotFit or NegativeUnsignedInt errors
//             // if there's a mismatch.
//         },
//         .as => |p| {
//             try self.checkPattern(p.pattern);
//         },
//         .applied_tag => |p| {
//             const args_slice = self.cir.store.slicePatterns(p.args);
//             for (args_slice) |pat_idx| {
//                 try self.checkPattern(pat_idx);
//             }
//         },
//         .tuple => |p| {
//             const args_slice = self.cir.store.slicePatterns(p.patterns);
//             for (args_slice) |pat_idx| {
//                 try self.checkPattern(pat_idx);
//             }
//         },
//         .record_destructure => |p| {
//             const destructs_slice = self.cir.store.sliceRecordDestructs(p.destructs);
//             for (destructs_slice) |destruct_idx| {
//                 const destruct = self.cir.store.getRecordDestruct(destruct_idx);
//                 try self.checkPattern(destruct.kind.toPatternIdx());
//             }
//         },
//         else => {},
//     }
// }

// // expr OLD //

// /// Check the types for an exprexpression. Returns whether evaluating the expr might perform side effects.
// pub fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!bool {
//     return self.checkExprWithExpected(expr_idx, null);
// }

// /// Check expression with an optional expected type for bidirectional type checking
// pub fn checkExprWithExpected(self: *Self, expr_idx: CIR.Expr.Idx, expected_type: ?Var) std.mem.Allocator.Error!bool {
//     return self.checkExprWithExpectedAndAnnotation(expr_idx, expected_type, false);
// }

// fn checkExprWithExpectedAndAnnotation(self: *Self, expr_idx: CIR.Expr.Idx, expected_type: ?Var, from_annotation: bool) std.mem.Allocator.Error!bool {
//     const does_fx = self.checkExprWithExpectedAndAnnotationHelp(expr_idx, expected_type, from_annotation);
//     if (expected_type) |expected| {
//         if (from_annotation) {
//             _ = try self.unifyWithAnnotation(expected, ModuleEnv.varFrom(expr_idx));
//         } else {
//             _ = try self.unify(expected, ModuleEnv.varFrom(expr_idx));
//         }
//     }
//     return does_fx;
// }

// /// Do not use directly, use `checkExprWithExpectedAndAnnotation`
// ///
// /// Checks the types of an expression, optionally against
// fn checkExprWithExpectedAndAnnotationHelp(self: *Self, expr_idx: CIR.Expr.Idx, expected_type: ?Var, from_annotation: bool) std.mem.Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     const expr = self.cir.store.getExpr(expr_idx);
//     const expr_var = ModuleEnv.varFrom(expr_idx);
//     const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));

//     var does_fx = false; // Does this expression potentially perform any side effects?
//     switch (expr) {
//         .e_int => |_| {
//             // Integer literals have their type constraints (bits_needed, sign_needed)
//             // created during canonicalization. Here we just need to ensure those
//             // constraints will be checked when unified with expected types.
//             // The type variable for this expression was already created with the
//             // appropriate num_unbound or int_unbound content during canonicalization.

//             // If we have an expected type, unify immediately to constrain the literal
//             if (expected_type) |expected| {
//                 const literal_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                 if (from_annotation) {
//                     _ = try self.unifyWithAnnotation(literal_var, expected);
//                 } else {
//                     _ = try self.unify(literal_var, expected);
//                 }
//             }
//         },
//         .e_num => |_| {},
//         .e_frac_f32 => |_| {
//             // Fractional literals have their type constraints (fits_in_f32, fits_in_dec)
//             // created during canonicalization. No additional checking needed here.
//         },
//         .e_frac_f64 => |_| {
//             // Fractional literals have their type constraints (fits_in_f32, fits_in_dec)
//             // created during canonicalization. No additional checking needed here.
//         },
//         .e_frac_dec => |_| {
//             // Decimal literals are similar to frac_f64.
//         },
//         .e_dec_small => |_| {
//             // Small decimal literals are similar to frac_f64.
//         },
//         .e_str_segment => |_| {},
//         .e_str => |_| {},
//         .e_lookup_local => |local| {
//             // For lookups, we need to connect the lookup expression to the actual variable
//             // The lookup expression should have the same type as the pattern it refers to
//             const lookup_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//             const pattern_var = @as(Var, @enumFromInt(@intFromEnum(local.pattern_idx)));

//             _ = try self.unify(lookup_var, pattern_var);
//         },
//         .e_lookup_external => |e| {
//             const module_idx = @intFromEnum(e.module_idx);
//             if (module_idx < self.other_modules.len) {
//                 const other_module_cir = self.other_modules[module_idx];
//                 const other_module_env = other_module_cir;

//                 // The idx of the expression in the other module
//                 const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(e.target_node_idx));

//                 // Check if we've already copied this import
//                 const cache_key = ImportCacheKey{
//                     .module_idx = e.module_idx,
//                     .node_idx = target_node_idx,
//                 };

//                 const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
//                     // Reuse the previously copied type.
//                     cached_var
//                 else blk: {
//                     // First time importing this type - copy it and cache the result
//                     const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
//                     const new_copy = try self.copyVar(imported_var, other_module_env);
//                     try self.import_cache.put(self.gpa, cache_key, new_copy);
//                     break :blk new_copy;
//                 };
//                 const instantiated_copy = try self.instantiateVarAnon(copied_var, .use_last_var);

//                 // Unify our expression with the copied type
//                 const result = try self.unify(expr_var, instantiated_copy);
//                 if (result.isProblem()) {
//                     self.setProblemTypeMismatchDetail(result.problem, .{
//                         .cross_module_import = .{
//                             .import_region = expr_idx,
//                             .module_idx = e.module_idx,
//                         },
//                     });

//                     try self.types.setVarContent(expr_var, .err);
//                 }
//             } else {
//                 // Import not found
//                 try self.types.setVarContent(expr_var, .err);
//             }
//         },
//         .e_list => |list| {
//             const elem_var = @as(Var, @enumFromInt(@intFromEnum(list.elem_var)));
//             const elems = self.cir.store.exprSlice(list.elems);

//             std.debug.assert(elems.len > 0); // Should never be 0 here, because this is not an .empty_list

//             // We need to type-check the first element, but we don't need to unify it with
//             // anything because we already pre-unified the list's elem var with it.
//             const first_elem_idx = elems[0];
//             var last_elem_idx: CIR.Expr.Idx = first_elem_idx;
//             does_fx = try self.checkExpr(first_elem_idx) or does_fx;

//             for (elems[1..], 1..) |elem_expr_id, i| {
//                 does_fx = try self.checkExpr(elem_expr_id) or does_fx;

//                 // Unify each element's var with the list's elem var
//                 const result = try self.unify(elem_var, @enumFromInt(@intFromEnum(elem_expr_id)));
//                 self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
//                     .last_elem_expr = last_elem_idx,
//                     .incompatible_elem_index = @intCast(i),
//                     .list_length = @intCast(elems.len),
//                 } });

//                 if (!result.isOk()) {
//                     // Check remaining elements to catch their individual errors
//                     for (elems[i + 1 ..]) |remaining_elem_id| {
//                         does_fx = try self.checkExpr(remaining_elem_id) or does_fx;
//                     }

//                     // Break to avoid cascading errors
//                     break;
//                 }

//                 last_elem_idx = elem_expr_id;
//             }
//         },
//         .e_empty_list => |_| {},
//         .e_match => |match| {
//             does_fx = try self.checkMatchExpr(expr_idx, match);
//         },
//         .e_if => |if_expr| {
//             does_fx = try self.checkIfElseExpr(expr_idx, expr_region, if_expr);
//         },
//         .e_call => |call| {
//             // Get all expressions - first is function, rest are arguments
//             // First expression is the function being called; the rest are args.
//             const func_expr_idx = call.func;
//             does_fx = try self.checkExpr(func_expr_idx) or does_fx; // func_expr could be effectful, e.g. `(mk_fn!())(arg)`

//             // Then, check all the arguments
//             const actual_args = self.cir.store.sliceExpr(call.args);
//             for (actual_args) |arg_expr_idx| {
//                 // Each arg could also be effectful, e.g. `fn(mk_arg!(), mk_arg!())`
//                 does_fx = try self.checkExpr(arg_expr_idx) or does_fx;
//             }

//             // Don't try to unify with the function if the function is a runtime error.
//             const func_expr = self.cir.store.getExpr(func_expr_idx);
//             if (func_expr != .e_runtime_error) {
//                 const func_expr_region = self.cir.store.getRegionAt(ModuleEnv.nodeIdxFrom(func_expr_idx));

//                 const ret_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                 const expected_fn_var = @as(Var, @enumFromInt(@intFromEnum(func_expr_idx)));
//                 const resolved_func = self.types.resolveVar(expected_fn_var);

//                 // Check if this is an annotated function that needs instantiation
//                 // We only instantiate if the function actually contains type variables
//                 var cur_call_func_var = expected_fn_var;
//                 var current_content = resolved_func.desc.content;

//                 content_switch: switch (current_content) {
//                     .structure => |flat_type| switch (flat_type) {
//                         .fn_effectful => |_| {
//                             does_fx = true;
//                             if (self.types.needsInstantiation(cur_call_func_var)) {
//                                 const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
//                                 const resolved_expected_func = self.types.resolveVar(expected_func_var);

//                                 std.debug.assert(resolved_expected_func.desc.content == .structure);
//                                 std.debug.assert(resolved_expected_func.desc.content.structure == .fn_effectful);
//                                 const expected_fn = resolved_expected_func.desc.content.structure.fn_effectful;

//                                 does_fx = try self.unifyFunctionCall(actual_args, ret_var, expected_fn_var, expected_fn, expr_region, func_expr_idx) or does_fx;
//                                 return does_fx;
//                             }
//                         },
//                         .fn_pure => |_| {
//                             if (self.types.needsInstantiation(cur_call_func_var)) {
//                                 const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
//                                 const resolved_expected_func = self.types.resolveVar(expected_func_var);

//                                 std.debug.assert(resolved_expected_func.desc.content == .structure);
//                                 std.debug.assert(resolved_expected_func.desc.content.structure == .fn_pure);
//                                 const expected_fn = resolved_expected_func.desc.content.structure.fn_pure;

//                                 does_fx = try self.unifyFunctionCall(actual_args, ret_var, expected_fn_var, expected_fn, func_expr_region, func_expr_idx) or does_fx;
//                                 return does_fx;
//                             }
//                         },
//                         .fn_unbound => |_| {
//                             if (self.types.needsInstantiation(cur_call_func_var)) {
//                                 const expected_func_var = try self.instantiateVarAnon(cur_call_func_var, .{ .explicit = expr_region });
//                                 const resolved_expected_func = self.types.resolveVar(expected_func_var);

//                                 std.debug.assert(resolved_expected_func.desc.content == .structure);
//                                 std.debug.assert(resolved_expected_func.desc.content.structure == .fn_unbound);
//                                 const expected_fn = resolved_expected_func.desc.content.structure.fn_unbound;

//                                 does_fx = try self.unifyFunctionCall(actual_args, ret_var, expected_fn_var, expected_fn, expr_region, func_expr_idx) or does_fx;
//                                 return does_fx;
//                             }
//                         },
//                         else => {
//                             // Non-function structure - fall through
//                         },
//                     },
//                     .alias => |alias| {
//                         // Resolve the alias, then continue on to the appropriate branch.
//                         // (It might be another alias, or we might be done and ready to proceed.)
//                         const backing_var = self.types.getAliasBackingVar(alias);
//                         cur_call_func_var = backing_var;
//                         current_content = self.types.resolveVar(backing_var).desc.content;
//                         continue :content_switch current_content;
//                     },
//                     else => {
//                         // Non-structure content - fall through
//                     },
//                 }

//                 // We didn't handle the function call above (either because it wasn't a function
//                 // or it didn't need instantiation), so fall back on this logic.
//                 const arg_vars: []Var = @constCast(@ptrCast(@alignCast(actual_args)));

//                 // Create an unbound function type with the call result as return type
//                 // The unification will propagate the actual return type to the call
//                 const func_content = try self.types.mkFuncUnbound(arg_vars, ret_var);
//                 const expected_func_var = try self.freshFromContent(func_content, expr_region);
//                 _ = try self.unify(expected_func_var, cur_call_func_var);
//             }
//         },
//         .e_record => |e| {
//             // Perform field-by-field unification between the record structure's
//             // field type variables and the actual field value expression types.
//             //
//             // 1. Resolve the expression var to get the record structure
//             // 2. Type check each field value expression (to get concrete types)
//             // 3. For each field, unify the field type var with the field value type var
//             // 4. Unification propagates concrete types through the type system

//             const record_var_resolved = self.types.resolveVar(expr_var);
//             const record_var_content = record_var_resolved.desc.content;

//             // Process each field
//             for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
//                 const field = self.cir.store.getRecordField(field_idx);

//                 // STEP 1: Check the field value expression first
//                 // This ensures the field value has a concrete type to unify with
//                 does_fx = try self.checkExpr(field.value) or does_fx;

//                 // STEP 2: Find the corresponding field type in the record structure
//                 // This only works if record_var_content is .structure.record
//                 if (record_var_content == .structure and record_var_content.structure == .record) {
//                     const record_fields = self.types.getRecordFieldsSlice(record_var_content.structure.record.fields);

//                     // STEP 3: Find the field with matching name and unify types
//                     const field_names = record_fields.items(.name);
//                     const field_vars = record_fields.items(.var_);
//                     for (field_names, field_vars) |type_field_name, type_field_var| {
//                         if (type_field_name.idx == field.name.idx) {
//                             // Extract the type variable from the field value expression
//                             // Different expression types store their type variables in different places
//                             const field_expr_type_var = @as(Var, @enumFromInt(@intFromEnum(field.value)));

//                             // STEP 4: Unify field type variable with field value type variable
//                             // This is where concrete types (like Str, Num) get propagated
//                             // from field values to the record structure
//                             _ = try self.unify(type_field_var, field_expr_type_var);
//                             break;
//                         }
//                     }
//                 }
//                 // If record_var_content is NOT .structure.record, unification is skipped
//                 // This typically happens when canonicalization didn't set the record structure properly
//             }
//         },
//         .e_empty_record => |_| {},
//         .e_tag => |_| {},
//         .e_nominal => |e| {
//             const real_nominal_var = ModuleEnv.varFrom(e.nominal_type_decl);
//             const expr_backing_var = ModuleEnv.varFrom(e.backing_expr);

//             try self.checkNominal(
//                 ModuleEnv.varFrom(expr_idx),
//                 expr_region,
//                 expr_backing_var,
//                 e.backing_type,
//                 real_nominal_var,
//             );
//         },
//         .e_nominal_external => |e| {
//             const resolved_external = try self.resolveVarFromExternal(e.module_idx, e.target_node_idx) orelse {
//                 // If we could not copy the type, set error and continue
//                 try self.types.setVarContent(ModuleEnv.varFrom(expr_idx), .err);
//                 return false;
//             };
//             const expr_backing_var = ModuleEnv.varFrom(e.backing_expr);
//             try self.checkNominal(
//                 ModuleEnv.varFrom(expr_idx),
//                 expr_region,
//                 expr_backing_var,
//                 e.backing_type,
//                 resolved_external.local_var,
//             );
//         },
//         .e_zero_argument_tag => |_| {},
//         .e_binop => |binop| {
//             does_fx = try self.checkBinopExpr(expr_idx, expr_region, binop, expected_type, from_annotation);
//         },
//         .e_unary_minus => |unary| {
//             does_fx = try self.checkUnaryMinusExpr(expr_idx, expr_region, unary);
//         },
//         .e_unary_not => |unary| {
//             does_fx = try self.checkUnaryNotExpr(expr_idx, expr_region, unary);
//         },
//         .e_block => |block| {
//             const anno_free_vars_top = self.anno_free_vars.top();
//             defer self.anno_free_vars.clearFrom(anno_free_vars_top);

//             // Check all statements in the block
//             const statements = self.cir.store.sliceStatements(block.stmts);
//             for (statements) |stmt_idx| {
//                 const stmt = self.cir.store.getStatement(stmt_idx);
//                 switch (stmt) {
//                     .s_decl => |decl_stmt| {
//                         // Check pattern and expression, then unify
//                         try self.checkPattern(decl_stmt.pattern);
//                         if (decl_stmt.anno) |anno_idx| {
//                             const annotation = self.cir.store.getAnnotation(anno_idx);
//                             const anno_var = try self.generateAnnoType(
//                                 FreeVarCtx{ .scratch = &self.anno_free_vars, .start = 0, .mode = .rigid },
//                                 annotation.type_anno,
//                             );

//                             does_fx = try self.checkExprWithExpectedAndAnnotation(decl_stmt.expr, anno_var, true) or does_fx;
//                         } else {
//                             does_fx = try self.checkExpr(decl_stmt.expr) or does_fx;
//                         }

//                         // Unify the pattern with the expression
//                         const decl_pattern_var: Var = @enumFromInt(@intFromEnum(decl_stmt.pattern));
//                         const decl_expr_var: Var = @enumFromInt(@intFromEnum(decl_stmt.expr));
//                         _ = try self.unify(decl_pattern_var, decl_expr_var);
//                     },
//                     .s_reassign => |reassign| {
//                         does_fx = try self.checkExpr(reassign.expr) or does_fx;
//                     },
//                     .s_expr => |expr_stmt| {
//                         does_fx = try self.checkExpr(expr_stmt.expr) or does_fx;
//                     },
//                     else => {
//                         // Other statement types don't need expression checking
//                     },
//                 }
//             }
//             // Check the final expression
//             does_fx = try self.checkExpr(block.final_expr) or does_fx;

//             // Link the root expr with the final expr
//             _ = try self.unify(
//                 @enumFromInt(@intFromEnum(expr_idx)),
//                 @enumFromInt(@intFromEnum(block.final_expr)),
//             );
//         },
//         .e_closure => |closure| {
//             // The type of a closure is the type of the lambda it wraps.
//             // The lambda's type is determined by its arguments and body.
//             // We need to check the lambda expression itself to get its type.
//             // If we have an expected type, pass it to the lambda to catch type errors early
//             if (expected_type) |expected| {
//                 does_fx = try self.checkExprWithExpectedAndAnnotation(closure.lambda_idx, expected, from_annotation);
//             } else {
//                 does_fx = try self.checkExpr(closure.lambda_idx);
//             }
//             const lambda_var = ModuleEnv.varFrom(closure.lambda_idx);
//             const closure_var = ModuleEnv.varFrom(expr_idx);
//             _ = try self.unify(closure_var, lambda_var);
//         },
//         .e_lambda => |lambda| {
//             does_fx = try self.checkLambdaWithAnno(expr_idx, expr_region, lambda, expected_type);
//         },
//         .e_tuple => |tuple| {
//             // Check tuple elements
//             const elems_slice = self.cir.store.exprSlice(tuple.elems);
//             for (elems_slice) |single_elem_expr_idx| {
//                 does_fx = try self.checkExpr(single_elem_expr_idx) or does_fx;
//             }

//             // The tuple type is created in the type store in canonicalize, so
//             // nothing more needs to be done here
//         },
//         .e_dot_access => |dot_access| {
//             // Check the receiver expression
//             does_fx = try self.checkExpr(dot_access.receiver) or does_fx;

//             // Get the type of the receiver
//             const receiver_var = @as(Var, @enumFromInt(@intFromEnum(dot_access.receiver)));
//             const resolved_receiver = self.types.resolveVar(receiver_var);

//             // Handle different receiver types
//             switch (resolved_receiver.desc.content) {
//                 .structure => |structure| switch (structure) {
//                     .nominal_type => |nominal| {
//                         // This is a static dispatch on a nominal type
//                         if (dot_access.args) |args_span| {
//                             // Method call with arguments
//                             // Get the origin module path
//                             const origin_module_path = self.cir.getIdent(nominal.origin_module);

//                             // Find which imported module matches this path
//                             var origin_module_idx: ?CIR.Import.Idx = null;
//                             var origin_module: ?*const ModuleEnv = null;

//                             // Check if it's the current module
//                             if (std.mem.eql(u8, origin_module_path, self.cir.module_name)) {
//                                 origin_module = self.cir;
//                             } else {
//                                 // Search through imported modules
//                                 for (self.other_modules, 0..) |other_module, idx| {
//                                     if (std.mem.eql(u8, origin_module_path, other_module.module_name)) {
//                                         origin_module_idx = @enumFromInt(idx);
//                                         origin_module = other_module;
//                                         break;
//                                     }
//                                 }
//                             }

//                             if (origin_module) |module| {
//                                 // Look up the method in the origin module's exports
//                                 // Need to convert identifier from current module to target module
//                                 const method_name_str = self.cir.getIdent(dot_access.field_name);

//                                 // Search through the module's exposed items
//                                 const node_idx_opt = if (module.common.findIdent(method_name_str)) |target_ident|
//                                     module.getExposedNodeIndexById(target_ident)
//                                 else
//                                     null;

//                                 if (node_idx_opt) |node_idx| {
//                                     // Found the method!
//                                     const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

//                                     // Check if we've already copied this import
//                                     const cache_key = ImportCacheKey{
//                                         .module_idx = origin_module_idx orelse @enumFromInt(0), // Current module
//                                         .node_idx = target_node_idx,
//                                     };

//                                     const method_var = if (self.import_cache.get(cache_key)) |cached_var|
//                                         cached_var
//                                     else blk: {
//                                         // Copy the method's type from the origin module to our type store
//                                         const source_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
//                                         const new_copy = try self.copyVar(source_var, @constCast(module));
//                                         try self.import_cache.put(self.gpa, cache_key, new_copy);
//                                         break :blk new_copy;
//                                     };
//                                     const method_instantiated = try self.instantiateVarAnon(method_var, .use_last_var);

//                                     // Check all arguments
//                                     var i: u32 = 0;
//                                     while (i < args_span.span.len) : (i += 1) {
//                                         const arg_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(args_span.span.start + i));
//                                         does_fx = try self.checkExpr(arg_expr_idx) or does_fx;
//                                     }

//                                     // Create argument list for the function call
//                                     var args = std.ArrayList(Var).init(self.gpa);
//                                     defer args.deinit();

//                                     // Add the receiver (the nominal type) as the first argument
//                                     try args.append(receiver_var);

//                                     // Add the remaining arguments
//                                     i = 0;
//                                     while (i < args_span.span.len) : (i += 1) {
//                                         const arg_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(args_span.span.start + i));
//                                         const arg_var = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));
//                                         try args.append(arg_var);
//                                     }

//                                     // Create a function type for the method call
//                                     const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                                     const func_content = try self.types.mkFuncUnbound(args.items, dot_access_var);
//                                     const expected_func_var = try self.freshFromContent(func_content, expr_region);

//                                     // Unify with the imported method type
//                                     _ = try self.unify(expected_func_var, method_instantiated);

//                                     // Store the resolved method info for code generation
//                                     // This will be used by the code generator to emit the correct function call
//                                     // For now, the type information in the expression variable is sufficient
//                                 } else {
//                                     // Method not found in origin module
//                                     // TODO: Add a proper error type for method not found on nominal type
//                                     try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
//                                 }
//                             } else {
//                                 // Origin module not found
//                                 // TODO: Add a proper error type for origin module not found
//                                 try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
//                             }
//                         } else {
//                             // No arguments - this might be a field access on a nominal type's backing type
//                             // TODO: Handle field access on nominal types
//                             try self.types.setVarContent(@enumFromInt(@intFromEnum(expr_idx)), .err);
//                         }
//                     },
//                     .record => |record| {
//                         // Receiver is already a record, find the field
//                         const fields = self.types.getRecordFieldsSlice(record.fields);

//                         // Find the field with the matching name
//                         for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
//                             if (field_name == dot_access.field_name) {
//                                 // Unify the dot access expression with the field type
//                                 const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                                 _ = try self.unify(dot_access_var, field_var);
//                                 break;
//                             }
//                         }
//                     },
//                     .record_unbound => |record_unbound| {
//                         // Receiver is an unbound record, find the field
//                         const fields = self.types.getRecordFieldsSlice(record_unbound);

//                         // Find the field with the matching name
//                         for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
//                             if (field_name == dot_access.field_name) {
//                                 // Unify the dot access expression with the field type
//                                 const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                                 _ = try self.unify(dot_access_var, field_var);
//                                 break;
//                             }
//                         }
//                     },
//                     else => {
//                         // Receiver is not a record, this is a type error
//                         // For now, we'll let unification handle the error
//                     },
//                 },
//                 .flex_var => {
//                     // Receiver is unbound, we need to constrain it to be a record with the field
//                     // Create a fresh variable for the field type
//                     const field_var = try self.fresh(expr_region);
//                     const dot_access_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
//                     _ = try self.unify(dot_access_var, field_var);

//                     // Create a record type with this field
//                     const field_idx = try self.types.appendRecordField(.{
//                         .name = dot_access.field_name,
//                         .var_ = field_var,
//                     });
//                     const fields_range = types_mod.RecordField.SafeMultiList.Range{
//                         .start = field_idx,
//                         .count = 1,
//                     };

//                     // Create an extension variable for other possible fields
//                     // Create the record content
//                     const record_content = types_mod.Content{
//                         .structure = .{
//                             .record_unbound = fields_range,
//                         },
//                     };

//                     // Unify the receiver with this record type
//                     //
//                     // TODO: Do we need to insert a CIR placeholder node here as well?
//                     // What happens if later this type variable has a problem, and we
//                     // try to look up it's region in CIR?
//                     const record_var = try self.freshFromContent(record_content, expr_region);

//                     // Use the dot access expression as the constraint origin for better error reporting
//                     _ = try self.unifyWithConstraintOrigin(receiver_var, record_var, dot_access_var);

//                     // Record that this variable was constrained by this dot access expression
//                     try self.constraint_origins.put(receiver_var, dot_access_var);
//                     // Constraint origin recorded for better error reporting
//                 },
//                 else => {
//                     // Other cases (rigid_var, alias, etc.) - let unification handle errors
//                 },
//             }
//         },
//         .e_runtime_error => {},
//         .e_crash => {},
//         .e_dbg => {},
//         .e_ellipsis => {},
//         .e_expect => {},
//     }

//     return does_fx;
// }

// // func //

// /// Helper function to unify a call expression with a function type
// fn unifyFunctionCall(
//     self: *Self,
//     args_vars: []const CIR.Expr.Idx, // var for the fn args
//     ret_var: Var, // var for the return type
//     expected_fn_var: Var, // var for the fn itself (must be instantiated)
//     expected_func: types_mod.Func, // the expected type of the fn (must be instantiated)
//     region: Region,
//     func_expr_idx: CIR.Expr.Idx, // the function expression for name extraction
// ) std.mem.Allocator.Error!bool {
//     // Extract function name if possible
//     const func_name: ?Ident.Idx = blk: {
//         const func_expr = self.cir.store.getExpr(func_expr_idx);
//         switch (func_expr) {
//             .e_lookup_local => |lookup| {
//                 // Get the pattern that defines this identifier
//                 const pattern = self.cir.store.getPattern(lookup.pattern_idx);
//                 switch (pattern) {
//                     .assign => |assign| break :blk assign.ident,
//                     else => break :blk null,
//                 }
//             },
//             else => break :blk null,
//         }
//     };

//     // Check arguments with expected types using bidirectional type checking
//     const expected_args = self.types.sliceVars(expected_func.args);

//     // Only unify arguments if counts match - otherwise let the normal
//     // unification process handle the arity mismatch error
//     if (expected_args.len == args_vars.len) {
//         // For function signatures with bound type variables, unify arguments with each other first
//         // This ensures proper error placement for cases like mk_pair("1", 2) where a, a -> Pair(a)

//         // Find arguments that share the same type variable
//         //
//         // TODO(jared): This is not doing what it's intended to do, I think.
//         // It's unifying variables if they're the the same var, but if they're
//         // the same var they don't need to be unified? I think setting up
//         // scope correct so all flex vars  are redirected to the same var in
//         // canonicalize shouold do the same thing.
//         for (expected_args, 0..) |expected_arg_1, i| {
//             const expected_resolved_1 = self.types.resolveVar(expected_arg_1);

//             // Only check type variables, skip concrete types
//             if (expected_resolved_1.desc.content != .flex_var and expected_resolved_1.desc.content != .rigid_var) {
//                 continue;
//             }

//             // Look for other arguments with the same type variable
//             for (expected_args[i + 1 ..], i + 1..) |expected_arg_2, j| {
//                 if (expected_arg_1 == expected_arg_2) {
//                     // These two arguments are bound by the same type variable - unify them first
//                     const arg_1 = @as(Var, @enumFromInt(@intFromEnum(args_vars[i])));
//                     const arg_2 = @as(Var, @enumFromInt(@intFromEnum(args_vars[j])));

//                     const unify_result = try self.unify(arg_1, arg_2);

//                     if (unify_result.isProblem()) {
//                         // Use the new error detail for bound type variable incompatibility
//                         self.setProblemTypeMismatchDetail(unify_result.problem, .{
//                             .incompatible_fn_args_bound_var = .{
//                                 .fn_name = func_name,
//                                 .first_arg_var = arg_1,
//                                 .second_arg_var = arg_2,
//                                 .first_arg_index = @intCast(i),
//                                 .second_arg_index = @intCast(j),
//                                 .num_args = @intCast(args_vars.len),
//                             },
//                         });
//                         return false; // Early return on error
//                     }
//                 }
//             }
//         }

//         // Apply constraint propagation for numeric literals with concrete types
//         for (expected_args, args_vars) |expected_arg, arg_expr_idx| {
//             const expected_resolved = self.types.resolveVar(expected_arg);

//             // Only apply constraint propagation for concrete types, not type variables
//             if (expected_resolved.desc.content != .flex_var and expected_resolved.desc.content != .rigid_var) {
//                 const arg_expr = self.cir.store.getExpr(arg_expr_idx);

//                 if (arg_expr == .e_int) {
//                     const literal_var = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));
//                     _ = try self.unify(literal_var, expected_arg);
//                 }
//             }
//         }

//         // Regular unification using the argument vars
//         var arg_index: u32 = 0;
//         for (expected_args, args_vars) |expected_arg, arg_expr_idx| {
//             const actual_arg = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));

//             // TODO: Do we need this?
//             // Instantiate polymorphic arguments before unification
//             // var arg_to_unify = actual_arg;
//             // if (self.types.needsInstantiation(actual_arg)) {
//             //     arg_to_unify = try self.instantiateVarAnon(actual_arg, .{ .explicit = region });
//             // }

//             const arg_result = try self.unify(expected_arg, actual_arg);
//             self.setDetailIfTypeMismatch(arg_result, .{
//                 .incompatible_fn_call_arg = .{
//                     .fn_name = func_name,
//                     .arg_var = actual_arg,
//                     .incompatible_arg_index = arg_index,
//                     .num_args = @intCast(args_vars.len),
//                 },
//             });
//             arg_index += 1;
//         }
//         // The call's type is the instantiated return type
//         _ = try self.unify(ret_var, expected_func.ret);
//     } else {
//         // Arity mismatch - arguments already checked

//         // Fall back to normal unification to get proper error message
//         // Use the original func_var to avoid issues with instantiated variables in error reporting
//         const actual_arg_vars: []Var = @constCast(@ptrCast(@alignCast(args_vars)));
//         const actual_func_content = try self.types.mkFuncUnbound(actual_arg_vars, ret_var);
//         const actual_func_var = try self.freshFromContent(actual_func_content, region);
//         _ = try self.unify(actual_func_var, expected_fn_var);
//         try self.types.setVarRedirect(ret_var, actual_func_var);
//     }

//     return false;
// }

// /// Performs bidirectional type checking for lambda expressions with optional type annotations.
// ///
// /// ## Constraint Ordering
// ///
// /// Parameter constraints from annotations are applied BEFORE checking the lambda body.
// /// This ensures rigid variables are properly constrained during body analysis, causing
// /// type errors to surface at their actual source rather than during later unification.
// ///
// /// **Why this ordering matters:**
// /// ```
// /// Pair(a) := [Pair(a, a)]
// /// mk_pair_invalid : a, b -> Pair(a)
// /// mk_pair_invalid = |x, y| Pair.Pair(x, y)
// ///
// /// // Step 1: Constrain parameters from annotation first
// /// x := a[rigid], y := b[rigid]
// ///
// /// // Step 2: Check body - error detected immediately at constructor
// /// Pair.Pair(x, y)  // ERROR: Pair requires same type, but x=a[rigid], y=b[rigid]
// /// ```
// ///
// /// Without upfront constraints, the error would only surface during final annotation
// /// validation, making it harder to locate the actual problem in the source code.
// /// Similar to checkLambdaWithAnno but only validates return type, not the entire function.
// /// This is used for lambdas to preserve error locations while still catching type errors.
// fn checkLambdaForClosure(
//     self: *Self,
//     expr_idx: CIR.Expr.Idx,
//     lambda: std.meta.FieldType(CIR.Expr, .e_lambda),
//     anno_type: ?Var,
// ) std.mem.Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     // Get the actual lambda arguments
//     const arg_patterns = self.cir.store.slicePatterns(lambda.args);
//     const arg_vars: []Var = @ptrCast(@alignCast(arg_patterns));

//     var mb_expected_func: ?types_mod.Func = null;

//     // STEP 1: Apply annotation constraints to parameters FIRST
//     if (anno_type) |anno_var| {
//         const expected_resolved = self.types.resolveVar(anno_var);
//         mb_expected_func = switch (expected_resolved.desc.content) {
//             .structure => |s| switch (s) {
//                 .fn_pure, .fn_effectful, .fn_unbound => |func| func,
//                 else => null,
//             },
//             else => null,
//         };

//         if (mb_expected_func) |func| {
//             const expected_args = self.types.sliceVars(func.args);
//             if (expected_args.len == arg_patterns.len) {
//                 // Constrain parameters with annotation types
//                 for (arg_patterns, expected_args) |pattern_idx, expected_arg| {
//                     // Check the pattern
//                     try self.checkPattern(pattern_idx);

//                     // Unify against the annotation
//                     const pattern_var = ModuleEnv.varFrom(pattern_idx);
//                     _ = try self.unify(pattern_var, expected_arg);
//                 }
//             }
//         }
//     }

//     // STEP 2: Check the body with return type constraint propagation
//     const is_effectful = if (mb_expected_func) |func|
//         try self.checkExprWithExpectedAndAnnotation(lambda.body, func.ret, true)
//     else
//         try self.checkExpr(lambda.body);

//     // STEP 3: Build the function type
//     const fn_var = ModuleEnv.varFrom(expr_idx);
//     const return_var = @as(Var, @enumFromInt(@intFromEnum(lambda.body)));

//     // Ensure the return variable has the correct region for error reporting
//     const body_region = self.cir.store.getExprRegion(lambda.body);
//     try self.fillInRegionsThrough(return_var);
//     self.setRegionAt(return_var, body_region);

//     if (is_effectful) {
//         _ = try self.types.setVarContent(fn_var, try self.types.mkFuncEffectful(arg_vars, return_var));
//     } else {
//         _ = try self.types.setVarContent(fn_var, try self.types.mkFuncUnbound(arg_vars, return_var));
//     }

//     // STEP 4: Don't validate here - let checkDef do the final unification
//     // This preserves error locations correctly

//     // NOTE: We don't do the final unification with the full annotation here
//     // The caller (checkDef) will handle that to preserve error locations

//     return is_effectful;
// }

// fn checkLambdaWithAnno(
//     self: *Self,
//     expr_idx: CIR.Expr.Idx,
//     _: Region,
//     lambda: std.meta.FieldType(CIR.Expr, .e_lambda),
//     anno_type: ?Var,
// ) std.mem.Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     // Get the actual lambda arguments
//     const arg_patterns = self.cir.store.slicePatterns(lambda.args);
//     const arg_vars: []Var = @ptrCast(@alignCast(arg_patterns));

//     var mb_expected_var: ?Var = null;
//     var mb_expected_func: ?types_mod.Func = null;

//     // STEP 1: Apply annotation constraints to parameters FIRST
//     if (anno_type) |anno_var| {
//         mb_expected_var = anno_var;

//         const expected_resolved = self.types.resolveVar(anno_var);
//         mb_expected_func = switch (expected_resolved.desc.content) {
//             .structure => |s| switch (s) {
//                 .fn_pure, .fn_effectful, .fn_unbound => |func| func,
//                 else => null,
//             },
//             else => null,
//         };

//         if (mb_expected_func) |func| {
//             const expected_args = self.types.sliceVars(func.args);
//             if (expected_args.len == arg_patterns.len) {
//                 // Constrain parameters with annotation types
//                 for (arg_patterns, expected_args) |pattern_idx, expected_arg| {
//                     // Check the pattern
//                     try self.checkPattern(pattern_idx);

//                     // Unify against the annotation
//                     const pattern_var = ModuleEnv.varFrom(pattern_idx);
//                     _ = try self.unify(pattern_var, expected_arg);
//                 }
//             }
//         }
//     }

//     // STEP 2: Check the body with return type constraint propagation for literals
//     const is_effectful = if (mb_expected_func) |func|
//         try self.checkExprWithExpectedAndAnnotation(lambda.body, func.ret, true)
//     else
//         try self.checkExpr(lambda.body);

//     // STEP 3: Build the function type
//     const fn_var = ModuleEnv.varFrom(expr_idx);
//     const return_var = @as(Var, @enumFromInt(@intFromEnum(lambda.body)));

//     // Ensure the return variable has the correct region for error reporting
//     const body_region = self.cir.store.getExprRegion(lambda.body);
//     try self.fillInRegionsThrough(return_var);
//     self.setRegionAt(return_var, body_region);

//     if (is_effectful) {
//         _ = try self.types.setVarContent(fn_var, try self.types.mkFuncEffectful(arg_vars, return_var));
//     } else {
//         _ = try self.types.setVarContent(fn_var, try self.types.mkFuncUnbound(arg_vars, return_var));
//     }

//     // STEP 4: Validate the function body against the annotation return type
//     if (mb_expected_func) |func| {
//         _ = try self.unify(return_var, func.ret);
//     }

//     // STEP 5: Validate the entire function against the annotation
//     if (mb_expected_var) |expected_var| {
//         _ = try self.unifyWithAnnotation(fn_var, expected_var);
//     }

//     return is_effectful;
// }

// // binop //

// /// Check the types for a binary operation expression
// fn checkBinopExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, binop: CIR.Expr.Binop, expected_type: ?Var, from_annotation: bool) Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     switch (binop.op) {
//         .add, .sub, .mul, .div, .rem, .pow, .div_trunc => {
//             // Check operands first
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;

//             // For now, we'll constrain both operands to be numbers
//             // In the future, this will use static dispatch based on the lhs type
//             const lhs_var = @as(Var, @enumFromInt(@intFromEnum(binop.lhs)));
//             const rhs_var = @as(Var, @enumFromInt(@intFromEnum(binop.rhs)));
//             const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

//             // For bidirectional type checking: if we have an expected type,
//             // we need to unify all operands and result with it.
//             // This ensures literals like `2` in `|x| x + 2` get properly constrained
//             // when the lambda has a type annotation like `I64 -> I64`.
//             if (expected_type) |expected| {
//                 // All three must be the same type for arithmetic operations
//                 if (from_annotation) {
//                     _ = try self.unifyWithAnnotation(lhs_var, expected);
//                     _ = try self.unifyWithAnnotation(rhs_var, expected);
//                     _ = try self.unifyWithAnnotation(result_var, expected);
//                 } else {
//                     _ = try self.unify(lhs_var, expected);
//                     _ = try self.unify(rhs_var, expected);
//                     _ = try self.unify(result_var, expected);
//                 }
//             } else {
//                 // No expected type - use fresh number variables to maintain polymorphism
//                 const num_content = Content{ .structure = .{ .num = .{
//                     .num_unbound = .{
//                         .int_requirements = Num.IntRequirements.init(),
//                         .frac_requirements = Num.FracRequirements.init(),
//                     },
//                 } } };
//                 const num_var_lhs = try self.freshFromContent(num_content, expr_region);
//                 const num_var_rhs = try self.freshFromContent(num_content, expr_region);
//                 const num_var_result = try self.freshFromContent(num_content, expr_region);

//                 // Unify lhs, rhs, and result with the number type
//                 _ = try self.unify(num_var_lhs, lhs_var);
//                 _ = try self.unify(num_var_rhs, rhs_var);
//                 _ = try self.unify(result_var, num_var_result);
//             }

//             return does_fx;
//         },
//         .lt, .gt, .le, .ge, .eq, .ne => {
//             // Check operands first
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;

//             // Comparison operators always return Bool
//             const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

//             const fresh_bool = try self.freshBool(expr_region);
//             _ = try self.unify(expr_var, fresh_bool);

//             return does_fx;
//         },
//         .@"and" => {
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;

//             const lhs_fresh_bool = try self.freshBool(expr_region);
//             const lhs_result = try self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
//             self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
//                 .binop_expr = expr_idx,
//                 .problem_side = .lhs,
//                 .binop = .@"and",
//             } });

//             if (lhs_result.isOk()) {
//                 const rhs_fresh_bool = try self.freshBool(expr_region);
//                 const rhs_result = try self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
//                 self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
//                     .binop_expr = expr_idx,
//                     .problem_side = .rhs,
//                     .binop = .@"and",
//                 } });
//             }

//             return does_fx;
//         },
//         .@"or" => {
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;

//             const lhs_fresh_bool = try self.freshBool(expr_region);
//             const lhs_result = try self.unify(lhs_fresh_bool, @enumFromInt(@intFromEnum(binop.lhs)));
//             self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
//                 .binop_expr = expr_idx,
//                 .problem_side = .lhs,
//                 .binop = .@"or",
//             } });

//             if (lhs_result.isOk()) {
//                 const rhs_fresh_bool = try self.freshBool(expr_region);
//                 const rhs_result = try self.unify(rhs_fresh_bool, @enumFromInt(@intFromEnum(binop.rhs)));
//                 self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
//                     .binop_expr = expr_idx,
//                     .problem_side = .rhs,
//                     .binop = .@"or",
//                 } });
//             }

//             return does_fx;
//         },
//         .pipe_forward => {
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;
//             return does_fx;
//         },
//         .null_coalesce => {
//             var does_fx = try self.checkExpr(binop.lhs);
//             does_fx = try self.checkExpr(binop.rhs) or does_fx;
//             return does_fx;
//         },
//     }
// }

// fn checkUnaryMinusExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, unary: CIR.Expr.UnaryMinus) Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     // Check the operand expression
//     const does_fx = try self.checkExpr(unary.expr);

//     // For unary minus, we constrain the operand and result to be numbers
//     const operand_var = @as(Var, @enumFromInt(@intFromEnum(unary.expr)));
//     const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

//     // Create a fresh number variable for the operation
//     const num_content = Content{ .structure = .{ .num = .{
//         .num_unbound = .{
//             .int_requirements = Num.IntRequirements.init(),
//             .frac_requirements = Num.FracRequirements.init(),
//         },
//     } } };
//     const num_var = try self.freshFromContent(num_content, expr_region);

//     // Unify operand and result with the number type
//     _ = try self.unify(operand_var, num_var);
//     _ = try self.unify(result_var, num_var);

//     return does_fx;
// }

// fn checkUnaryNotExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, unary: CIR.Expr.UnaryNot) Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     // Check the operand expression
//     const does_fx = try self.checkExpr(unary.expr);

//     // For unary not, we constrain the operand and result to be booleans
//     const operand_var = @as(Var, @enumFromInt(@intFromEnum(unary.expr)));
//     const result_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

//     // Create a fresh boolean variable for the operation
//     const bool_var = try self.freshBool(expr_region);

//     // Unify operand and result with the boolean type
//     _ = try self.unify(operand_var, bool_var);
//     _ = try self.unify(result_var, bool_var);

//     return does_fx;
// }

// // nominal //

// // nominal //

// /// Check that a nominal type constructor usage is valid and perform type inference.
// ///
// /// This function validates that a user's constructor application (like `Maybe.Just(1)`)
// /// is compatible with the nominal type's definition and infers the concrete types.
// ///
// /// Example:
// ///   Maybe a := [Just(a), None]
// ///   val = Maybe.Just(1)  // This call validates Just(1) against Maybe's definition
// ///
// /// Parameters:
// /// * `node_var` - Variable representing the entire expression (initially a placeholder).
// ///                This will be redirected to the final inferred type.
// /// * `node_region` - Source region of the entire expression for error reporting
// /// * `node_backing_var` - Variable for the constructor application's internal type (e.g., `Just(1)`)
// /// * `node_backing_type` - Kind of nominal backing (tag, record, tuple, or val)
// /// * `real_nominal_var` - Variable of the nominal type declaration (e.g., `Maybe a`)
// fn checkNominal(
//     self: *Self,
//     node_var: Var,
//     node_region: Region,
//     node_backing_var: Var,
//     node_backing_type: CIR.Expr.NominalBackingType,
//     real_nominal_var: Var,
// ) Allocator.Error!void {
//     // Clear the rigid variable substitution map to ensure fresh instantiation
//     self.anonymous_rigid_var_subs.items.clearRetainingCapacity();

//     // Algorithm overview:
//     // 1. Validate that the nominal type exists and is well-formed
//     // 2. Instantiate the nominal type's backing type with fresh variables
//     // 3. Unify the instantiated backing type against the user's constructor
//     // 4. If unification succeeds, instantiate the nominal type and assign to node_var
//     // 5. If unification fails, report appropriate error and mark node_var as error
//     //
//     // The key insight is using a shared rigid variable map so that type parameters
//     // in the backing type and nominal type get the same substitutions.

//     // Step 1: Extract and validate the nominal type
//     const nominal_content = self.types.resolveVar(real_nominal_var).desc.content;
//     const nominal_type = switch (nominal_content) {
//         .structure => |structure| switch (structure) {
//             .nominal_type => |nt| nt,
//             else => {
//                 // The supposed nominal type is actually some other structure
//                 try self.types.setVarContent(node_var, .err);
//                 return;
//             },
//         },
//         else => {
//             // The nominal type is in an error state or unresolved
//             try self.types.setVarContent(node_var, .err);
//             return;
//         },
//     };

//     // Step 2: Instantiate the nominal type's backing type
//     // This converts rigid type variables (like `a[r]`) to fresh flexible variables
//     // that can be unified. The backing type defines the structure (e.g., [Just(a), None])
//     const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);
//     const instantiated_backing_var = try self.instantiateVar(
//         nominal_backing_var,
//         &self.anonymous_rigid_var_subs,
//         .{ .explicit = node_region },
//     );

//     // TODO: If we can, unify arguments directly to get better error messages
//     // Unifying by the whole thing works, but doesn't have great error messages
//     // always

//     // Step 3: Unify the instantiated backing type with the user's constructor
//     // This checks if `Just(1)` is compatible with `[Just(a), None]` and if so,
//     // unifies `a` with the type of `1` (e.g., Num(_size))
//     const result = try self.unify(instantiated_backing_var, node_backing_var);

//     // Step 4: Handle the unification result
//     switch (result) {
//         .ok => {
//             // Unification succeeded - the constructor is valid for this nominal type
//             // Now instantiate the full nominal type using the same rigid variable map
//             // so it gets the same type substitutions as the backing type
//             const instantiated_qualified_var = try self.instantiateVar(
//                 real_nominal_var,
//                 &self.anonymous_rigid_var_subs,
//                 .{ .explicit = node_region },
//             );

//             // Assign the inferred concrete type to the expression variable
//             // After this, `node_var` will resolve to something like `Maybe(Num(_size))`
//             try self.types.setVarRedirect(node_var, instantiated_qualified_var);
//         },
//         .problem => |problem_idx| {
//             // Unification failed - the constructor is incompatible with the nominal type
//             // Set a specific error message based on the backing type kind
//             switch (node_backing_type) {
//                 .tag => {
//                     // Constructor doesn't exist or has wrong arity/types
//                     self.setProblemTypeMismatchDetail(problem_idx, .invalid_nominal_tag);
//                 },
//                 else => {
//                     // TODO: Add specific error messages for records, tuples, etc.
//                 },
//             }

//             // Mark the entire expression as having a type error
//             try self.types.setVarContent(node_var, .err);
//         },
//     }
// }

// // if-else //

// /// Check the types for an if-else expr
// fn checkIfElseExpr(
//     self: *Self,
//     if_expr_idx: CIR.Expr.Idx,
//     expr_region: Region,
//     if_: std.meta.FieldType(CIR.Expr, .e_if),
// ) std.mem.Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     const branches = self.cir.store.sliceIfBranches(if_.branches);

//     // Should never be 0
//     std.debug.assert(branches.len > 0);

//     // Get the first branch
//     const first_branch_idx = branches[0];
//     const first_branch = self.cir.store.getIfBranch(first_branch_idx);

//     // Check the condition of the 1st branch
//     var does_fx = try self.checkExpr(first_branch.cond);
//     const first_cond_var: Var = @enumFromInt(@intFromEnum(first_branch.cond));
//     const bool_var = try self.freshBool(expr_region);
//     const first_cond_result = try self.unify(bool_var, first_cond_var);
//     self.setDetailIfTypeMismatch(first_cond_result, .incompatible_if_cond);

//     // Then we check the 1st branch's body
//     does_fx = try self.checkExpr(first_branch.body) or does_fx;

//     // The 1st branch's body is the type all other branches must match
//     const branch_var = @as(Var, @enumFromInt(@intFromEnum(first_branch.body)));

//     // Total number of branches (including final else)
//     const num_branches: u32 = @intCast(branches.len + 1);

//     var last_if_branch = first_branch_idx;
//     for (branches[1..], 1..) |branch_idx, cur_index| {
//         const branch = self.cir.store.getIfBranch(branch_idx);

//         // Check the branches condition
//         does_fx = try self.checkExpr(branch.cond) or does_fx;
//         const cond_var: Var = @enumFromInt(@intFromEnum(branch.cond));
//         const branch_bool_var = try self.freshBool(expr_region);
//         const cond_result = try self.unify(branch_bool_var, cond_var);
//         self.setDetailIfTypeMismatch(cond_result, .incompatible_if_cond);

//         // Check the branch body
//         does_fx = try self.checkExpr(branch.body) or does_fx;
//         const body_var: Var = @enumFromInt(@intFromEnum(branch.body));
//         const body_result = try self.unify(branch_var, body_var);
//         self.setDetailIfTypeMismatch(body_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
//             .parent_if_expr = if_expr_idx,
//             .last_if_branch = last_if_branch,
//             .num_branches = num_branches,
//             .problem_branch_index = @intCast(cur_index),
//         } });

//         if (!body_result.isOk()) {
//             // Check remaining branches to catch their individual errors
//             for (branches[cur_index + 1 ..]) |remaining_branch_idx| {
//                 const remaining_branch = self.cir.store.getIfBranch(remaining_branch_idx);

//                 does_fx = try self.checkExpr(remaining_branch.cond) or does_fx;
//                 const remaining_cond_var: Var = @enumFromInt(@intFromEnum(remaining_branch.cond));

//                 const fresh_bool = try self.freshBool(expr_region);
//                 const remaining_cond_result = try self.unify(fresh_bool, remaining_cond_var);
//                 self.setDetailIfTypeMismatch(remaining_cond_result, .incompatible_if_cond);

//                 does_fx = try self.checkExpr(remaining_branch.body) or does_fx;
//                 try self.types.setVarContent(@enumFromInt(@intFromEnum(remaining_branch.body)), .err);
//             }

//             // Break to avoid cascading errors
//             break;
//         }

//         last_if_branch = branch_idx;
//     }

//     // Check the final else
//     does_fx = try self.checkExpr(if_.final_else) or does_fx;
//     const final_else_var: Var = @enumFromInt(@intFromEnum(if_.final_else));
//     const final_else_result = try self.unify(branch_var, final_else_var);
//     self.setDetailIfTypeMismatch(final_else_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
//         .parent_if_expr = if_expr_idx,
//         .last_if_branch = last_if_branch,
//         .num_branches = num_branches,
//         .problem_branch_index = num_branches - 1,
//     } });

//     // Unify the if expression's type variable with the branch type
//     const if_expr_var: Var = @enumFromInt(@intFromEnum(if_expr_idx));
//     _ = try self.unify(if_expr_var, branch_var);

//     return does_fx;
// }

// // match //

// /// Check the types for an if-else expr
// fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, match: CIR.Expr.Match) Allocator.Error!bool {
//     const trace = tracy.trace(@src());
//     defer trace.end();

//     // Check the match's condition
//     var does_fx = try self.checkExpr(match.cond);
//     const cond_var = ModuleEnv.varFrom(match.cond);

//     // Bail if we somehow have 0 branches
//     // TODO: Should this be an error? Here or in Can?
//     if (match.branches.span.len == 0) return does_fx;

//     // Get slice of branches
//     const branch_idxs = self.cir.store.sliceMatchBranches(match.branches);

//     // Manually check the 1st branch
//     // The type of the branch's body becomes the var other branch bodies must unify
//     // against.
//     const first_branch_idx = branch_idxs[0];
//     const first_branch = self.cir.store.getMatchBranch(first_branch_idx);

//     const first_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(first_branch.patterns);

//     for (first_branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
//         const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
//         try self.checkPattern(branch_ptrn.pattern);
//         const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);

//         const ptrn_result = try self.unify(cond_var, branch_ptrn_var);
//         self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
//             .match_expr = expr_idx,
//             .num_branches = @intCast(match.branches.span.len),
//             .problem_branch_index = 0,
//             .num_patterns = @intCast(first_branch_ptrn_idxs.len),
//             .problem_pattern_index = @intCast(cur_ptrn_index),
//         } });
//     }

//     // Check the first branch's value, then use that at the branch_var
//     does_fx = try self.checkExpr(first_branch.value) or does_fx;
//     const branch_var = ModuleEnv.varFrom(first_branch.value);

//     // Then iterate over the rest of the branches
//     for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
//         const branch = self.cir.store.getMatchBranch(branch_idx);

//         // First, check the patterns of this branch
//         const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
//         for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
//             // Check the pattern's sub types
//             const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
//             try self.checkPattern(branch_ptrn.pattern);

//             // Check the pattern against the cond
//             const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);
//             const ptrn_result = try self.unify(cond_var, branch_ptrn_var);
//             self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
//                 .match_expr = expr_idx,
//                 .num_branches = @intCast(match.branches.span.len),
//                 .problem_branch_index = @intCast(branch_cur_index),
//                 .num_patterns = @intCast(branch_ptrn_idxs.len),
//                 .problem_pattern_index = @intCast(cur_ptrn_index),
//             } });
//         }

//         // Then, check the body
//         does_fx = try self.checkExpr(branch.value) or does_fx;
//         const branch_result = try self.unify(branch_var, ModuleEnv.varFrom(branch.value));
//         self.setDetailIfTypeMismatch(branch_result, problem.TypeMismatchDetail{ .incompatible_match_branches = .{
//             .match_expr = expr_idx,
//             .num_branches = @intCast(match.branches.span.len),
//             .problem_branch_index = @intCast(branch_cur_index),
//         } });

//         if (!branch_result.isOk()) {
//             // If there was a body mismatch, do not check other branches to stop
//             // cascading errors. But still check each other branch's sub types
//             for (branch_idxs[branch_cur_index + 1 ..], branch_cur_index + 1..) |other_branch_idx, other_branch_cur_index| {
//                 const other_branch = self.cir.store.getMatchBranch(other_branch_idx);

//                 // Still check the other patterns
//                 const other_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(other_branch.patterns);
//                 for (other_branch_ptrn_idxs, 0..) |other_branch_ptrn_idx, other_cur_ptrn_index| {
//                     // Check the pattern's sub types
//                     const other_branch_ptrn = self.cir.store.getMatchBranchPattern(other_branch_ptrn_idx);
//                     try self.checkPattern(other_branch_ptrn.pattern);

//                     // Check the pattern against the cond
//                     const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
//                     const ptrn_result = try self.unify(cond_var, other_branch_ptrn_var);
//                     self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
//                         .match_expr = expr_idx,
//                         .num_branches = @intCast(match.branches.span.len),
//                         .problem_branch_index = @intCast(other_branch_cur_index),
//                         .num_patterns = @intCast(other_branch_ptrn_idxs.len),
//                         .problem_pattern_index = @intCast(other_cur_ptrn_index),
//                     } });
//                 }

//                 // Then check the other branch's exprs
//                 does_fx = try self.checkExpr(other_branch.value) or does_fx;
//                 try self.types.setVarContent(ModuleEnv.varFrom(other_branch.value), .err);
//             }

//             // Then stop type checking for this branch
//             break;
//         }
//     }

//     return does_fx;
// }

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

// instantiate - OLD //

// const RigidVarBehavior = union(enum) {
//     use_cached_rigid_vars,
//     rollback_rigid_vars,
// };

// /// Instantiate a variable
// fn instantiateVar(
//     self: *Self,
//     var_to_instantiate: Var,
//     rigid_to_flex_subs: *Instantiate.RigidToFlexSubs,
//     region_behavior: InstantiateRegionBehavior,
// ) std.mem.Allocator.Error!Var {
//     self.var_map.clearRetainingCapacity();

//     var instantiate = Instantiate.init(self.types, self.cir.getIdentStore(), &self.var_map);
//     var instantiate_ctx = Instantiate.Ctx{
//         .rigid_var_subs = rigid_to_flex_subs,
//     };
//     const instantiated_var = try instantiate.instantiateVar(var_to_instantiate, &instantiate_ctx);

//     // If we had to insert any new type variables, ensure that we have
//     // corresponding regions for them. This is essential for error reporting.
//     const root_instantiated_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;
//     if (self.var_map.count() > 0) {
//         var iterator = self.var_map.iterator();
//         while (iterator.next()) |x| {
//             // Get the newly created var
//             const fresh_var = x.value_ptr.*;
//             try self.fillInRegionsThrough(fresh_var);

//             switch (region_behavior) {
//                 .explicit => |region| {
//                     self.setRegionAt(fresh_var, region);
//                 },
//                 .use_root_instantiated => {
//                     self.setRegionAt(fresh_var, root_instantiated_region);
//                 },
//                 .use_last_var => {
//                     const old_var = x.key_ptr.*;
//                     const old_region = self.regions.get(@enumFromInt(@intFromEnum(old_var))).*;
//                     self.setRegionAt(fresh_var, old_region);
//                 },
//             }
//         }
//     }

//     // Assert that we have regions for every type variable
//     self.debugAssertArraysInSync();

//     return instantiated_var;
// }

// /// Instantiate a variable
// fn instantiateVarAnon(
//     self: *Self,
//     var_to_instantiate: Var,
//     region_behavior: InstantiateRegionBehavior,
// ) std.mem.Allocator.Error!Var {
//     self.anonymous_rigid_var_subs.items.clearRetainingCapacity();
//     const var_ = self.instantiateVar(var_to_instantiate, &self.anonymous_rigid_var_subs, region_behavior);
//     return var_;
// }

// // copy type from other module //

// /// Instantiate a variable, writing su
// fn copyVar(
//     self: *Self,
//     other_module_var: Var,
//     other_module_env: *ModuleEnv,
// ) std.mem.Allocator.Error!Var {
//     self.var_map.clearRetainingCapacity();
//     const copied_var = try copy_import.copyVar(
//         &other_module_env.*.types,
//         self.types,
//         other_module_var,
//         &self.var_map,
//         other_module_env.getIdentStore(),
//         self.cir.getIdentStore(),
//         self.gpa,
//     );

//     // If we had to insert any new type variables, ensure that we have
//     // corresponding regions for them. This is essential for error reporting.
//     if (self.var_map.count() > 0) {
//         var iterator = self.var_map.iterator();
//         while (iterator.next()) |x| {
//             // Get the newly created var
//             const fresh_var = x.value_ptr.*;
//             try self.fillInRegionsThrough(fresh_var);

//             self.setRegionAt(fresh_var, base.Region.zero());
//         }
//     }

//     // Assert that we have regions for every type variable
//     self.debugAssertArraysInSync();

//     return copied_var;
// }

// // external type lookups //

// const ExternalType = struct {
//     local_var: Var,
//     other_cir_node_idx: CIR.Node.Idx,
//     other_cir: *ModuleEnv,
// };

// /// Copy a variable from a different module into this module's types store.
// ///
// /// IMPORTANT: The caller must instantiate this variable before unifing
// /// against it. This avoid poisoning the copied variable in the types store if
// /// unification fails.
// fn resolveVarFromExternal(
//     self: *Self,
//     module_idx: CIR.Import.Idx,
//     node_idx: u16,
// ) std.mem.Allocator.Error!?ExternalType {
//     const module_idx_int = @intFromEnum(module_idx);
//     if (module_idx_int < self.other_modules.len) {
//         const other_module_cir = self.other_modules[module_idx_int];
//         const other_module_env = other_module_cir;

//         // The idx of the expression in the other module
//         const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

//         // Check if we've already copied this import
//         const cache_key = ImportCacheKey{
//             .module_idx = module_idx,
//             .node_idx = target_node_idx,
//         };

//         const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
//             // Reuse the previously copied type.
//             cached_var
//         else blk: {
//             // First time importing this type - copy it and cache the result
//             const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));
//             const new_copy = try self.copyVar(imported_var, other_module_env);
//             try self.import_cache.put(self.gpa, cache_key, new_copy);
//             break :blk new_copy;
//         };

//         return .{
//             .local_var = copied_var,
//             .other_cir_node_idx = target_node_idx,
//             .other_cir = other_module_env,
//         };
//     } else {
//         return null;
//     }
// }
