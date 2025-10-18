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
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const Rank = types_mod.Rank;
const Num = types_mod.Num;
const testing = std.testing;
const Instantiator = types_mod.instantiate.Instantiator;
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
other_modules: []const *const ModuleEnv,
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
/// annos we've already seen when generation a type from an annotation
seen_annos: std.AutoHashMap(CIR.TypeAnno.Idx, Var),
/// pool of variables that need to be generalized, built up during checking
var_pool: VarPool,
/// wrapper around generalization, contains some internal state used to do it's work
generalizer: Generalizer,
/// A map from one var to another. Used in instantiation and var copying
var_map: std.AutoHashMap(Var, Var),
/// A map from one var to another. Used to apply type arguments in instantiation
rigid_var_substitutions: std.AutoHashMapUnmanaged(Ident.Idx, Var),
/// scratch vars used to build up intermediate lists, used for various things
scratch_vars: base.Scratch(Var),
/// scratch tags used to build up intermediate lists, used for various things
scratch_tags: base.Scratch(types_mod.Tag),
/// scratch record fields used to build up intermediate lists, used for various things
scratch_record_fields: base.Scratch(types_mod.RecordField),
// Cache for imported types. This cache lives for the entire type-checking session
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
    other_modules: []const *const ModuleEnv,
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
        .anno_free_vars = try base.Scratch(FreeVar).init(gpa),
        .decl_free_vars = try base.Scratch(FreeVar).init(gpa),
        .seen_annos = std.AutoHashMap(CIR.TypeAnno.Idx, Var).init(gpa),
        .var_pool = try VarPool.init(gpa),
        .generalizer = try Generalizer.init(gpa, types),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .rigid_var_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){},
        .scratch_vars = try base.Scratch(types_mod.Var).init(gpa),
        .scratch_tags = try base.Scratch(types_mod.Tag).init(gpa),
        .scratch_record_fields = try base.Scratch(types_mod.RecordField).init(gpa),
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
    self.anno_free_vars.deinit(self.gpa);
    self.decl_free_vars.deinit(self.gpa);
    self.seen_annos.deinit();
    self.var_pool.deinit();
    self.generalizer.deinit();
    self.var_map.deinit();
    self.rigid_var_substitutions.deinit(self.gpa);
    self.scratch_vars.deinit(self.gpa);
    self.scratch_tags.deinit(self.gpa);
    self.scratch_record_fields.deinit(self.gpa);
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

/// Fills the type store with fresh variables up to the number of regions
inline fn ensureTypeStoreIsFilled(self: *Self) Allocator.Error!void {
    const region_nodes: usize = @intCast(self.regions.len());
    const type_nodes: usize = @intCast(self.types.len());
    try self.types.ensureTotalCapacity(region_nodes);
    for (type_nodes..region_nodes) |_| {
        _ = self.types.appendFromContentAssumeCapacity(.err);
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
fn unify(self: *Self, a: Var, b: Var, rank: Rank) std.mem.Allocator.Error!unifier.Result {
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

    for (self.unify_scratch.fresh_vars.items.items) |fresh_var| {
        try self.var_pool.addVarToRank(fresh_var, rank);
        const region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(a));
        try self.fillInRegionsThrough(fresh_var);
        self.setRegionAt(fresh_var, region);
    }
    self.debugAssertArraysInSync();
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
fn unifyFromAnno(self: *Self, a: Var, b: Var, rank: Rank) std.mem.Allocator.Error!unifier.Result {
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
    for (self.unify_scratch.fresh_vars.items.items) |fresh_var| {
        const region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(a));
        try self.fillInRegionsThrough(fresh_var);
        self.setRegionAt(fresh_var, region);
    }

    self.debugAssertArraysInSync();

    return result;
}

/// Unify two variables with a specific constraint origin for better error reporting.
/// The constraint_origin_var should point to the expression that created the constraint.
fn unifyWithConstraintOrigin(self: *Self, a: Var, b: Var, constraint_origin_var: Var) std.mem.Allocator.Error!unifier.Result {
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

/// Instantiate a variable, substituting any encountered rigids with flex vars
///
/// Note that the the rigid var structure will be preserved.
/// E.g. `a -> a`, `a` will reference the same new flex var
fn instantiateVar(
    self: *Self,
    var_to_instantiate: Var,
    rank: types_mod.Rank,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = rank,
        .rigid_behavior = .fresh_flex,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, region_behavior);
}

/// Instantiate a variable, substituting any encountered rigids with *new* rigid vars
///
/// Note that the the rigid var structure will be preserved.
/// E.g. `a -> a`, `a` will reference the same new rigid var
fn instantiateVarPreserveRigids(
    self: *Self,
    var_to_instantiate: Var,
    rank: types_mod.Rank,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = rank,
        .rigid_behavior = .fresh_flex,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, region_behavior);
}

/// Instantiate a variable
fn instantiateVarWithSubs(
    self: *Self,
    var_to_instantiate: Var,
    subs: *std.AutoHashMapUnmanaged(Ident.Idx, Var),
    rank: types_mod.Rank,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = rank,
        .rigid_behavior = .{ .substitute_rigids = subs },
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, region_behavior);
}

/// Instantiate a variable
fn instantiateVarHelp(
    self: *Self,
    var_to_instantiate: Var,
    instantiator: *Instantiator,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    // First, reset state
    instantiator.var_map.clearRetainingCapacity();

    // Then, instantiate the variable with the provided context
    const instantiated_var = try instantiator.instantiateVar(var_to_instantiate);

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    const root_instantiated_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;
    if (instantiator.var_map.count() > 0) {
        var iterator = instantiator.var_map.iterator();
        while (iterator.next()) |x| {
            // Get the newly created var
            const fresh_var = x.value_ptr.*;

            // Add to pool
            try self.var_pool.addVarToRank(fresh_var, instantiator.current_rank);

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

    // Return the instantiated var
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

/// Set the region for a var
fn setRegionAt(self: *Self, target_var: Var, new_region: Region) void {
    self.regions.set(@enumFromInt(@intFromEnum(target_var)), new_region);
}

/// Get the region for a var
fn getRegionAt(self: *Self, target_var: Var) Region {
    self.regions.get(@enumFromInt(@intFromEnum(target_var)));
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
fn freshBool(self: *Self, rank: Rank, new_region: Region) Allocator.Error!Var {
    // Look up Bool's actual index from builtin_statements (should be first)
    const builtin_stmts_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    std.debug.assert(builtin_stmts_slice.len >= 1); // Must have at least Bool
    const bool_stmt_idx = builtin_stmts_slice[0]; // Bool is always the first builtin
    // Debug assertion: verify this is a nominal type declaration
    if (std.debug.runtime_safety) {
        const stmt = self.cir.store.getStatement(bool_stmt_idx);
        std.debug.assert(stmt == .s_nominal_decl);
    }
    return try self.instantiateVar(ModuleEnv.varFrom(bool_stmt_idx), rank, .{ .explicit = new_region });
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

    try ensureTypeStoreIsFilled(self);

    // First, iterate over the statements, generating types for each type declaration
    const builtin_stmts_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (builtin_stmts_slice) |builtin_stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(builtin_stmt_idx);
    }

    // First, iterate over the statements, generating types for each type declaration
    const stmts_slice = self.cir.store.sliceStatements(self.cir.all_statements);
    for (stmts_slice) |stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(stmt_idx);
    }

    const defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
    for (defs_slice) |def_idx| {
        try self.checkDef(def_idx);
    }

    // Freeze interners after type-checking is complete
    self.cir.freezeInterners();
}

// repl //

/// Check an expr for the repl
pub fn checkExprRepl(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    try ensureTypeStoreIsFilled(self);
    // First, iterate over the statements, generating types for each type declaration
    const stms_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (stms_slice) |stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(stmt_idx);
    }

    // Push the rank for this definition
    try self.var_pool.pushRank();
    defer self.var_pool.popRank();

    // Ensure that the current rank in the pool is top-level
    const rank = types_mod.Rank.top_level;
    std.debug.assert(rank == self.var_pool.current_rank);

    _ = try self.checkExpr(expr_idx, rank, .no_expectation);

    // Now that we are existing the scope, we must generalize then pop this rank
    try self.generalizer.generalize(&self.var_pool, rank);
}

// defs //

/// Check the types for a single definition
fn checkDef(self: *Self, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Push the rank for this definition
    try self.var_pool.pushRank();
    defer self.var_pool.popRank();

    // Ensure that the current rank in the pool is top-level
    const rank = types_mod.Rank.top_level;
    std.debug.assert(rank == self.var_pool.current_rank);

    const def = self.cir.store.getDef(def_idx);
    const def_var = ModuleEnv.varFrom(def_idx);
    const ptrn_var = ModuleEnv.varFrom(def.pattern);
    const expr_var = ModuleEnv.varFrom(def.expr);

    // Check the pattern
    try self.checkPattern(def.pattern, rank, .no_expectation);

    // Handle if there's an annotation associated with this def
    if (def.annotation) |anno_idx| {
        const annotation = self.cir.store.getAnnotation(anno_idx);

        self.anno_free_vars.items.clearRetainingCapacity();
        try self.generateAnnoTypeInPlace(annotation.type_anno, .annotation);

        // TODO: Duplicate anno var so if the body results in type mismatch, the
        // annotation isn't corrupted
        // We can instantiate, but how do we deal with rigid vars?
        // const anno_var = try self.instantiateVaPreserveRigids(ModuleEnv.varFrom(annotation.type_anno), Rank.generalized, .use_last_var);
        const anno_var = ModuleEnv.varFrom(annotation.type_anno);

        _ = try self.checkExpr(def.expr, rank, .{
            .expected = .{ .var_ = anno_var, .from_annotation = true },
        });
    } else {
        _ = try self.checkExpr(def.expr, rank, .no_expectation);
    }

    // Also unify the pattern with the expr - needed so lookups work correctly
    _ = try self.unify(ptrn_var, expr_var, rank);

    // Set the def var to redirect to the pattern
    _ = try self.types.setVarRedirect(def_var, ptrn_var);

    // Now that we are existing the scope, we must generalize then pop this rank
    try self.generalizer.generalize(&self.var_pool, rank);
}

// create types for type decls //

/// Generate a type variable from the provided type statement.
/// If the stmt is not an alias or nominal dec, then do nothing
///
/// The created variable is put in-place at the var slot at `decl_idx`
/// The created variable will be generalized
fn generateStmtTypeDeclType(
    self: *Self,
    decl_idx: CIR.Statement.Idx,
) std.mem.Allocator.Error!void {
    const decl_free_vars_top = self.decl_free_vars.top();
    defer self.decl_free_vars.clearFrom(decl_free_vars_top);

    const decl = self.cir.store.getStatement(decl_idx);
    const decl_var = ModuleEnv.varFrom(decl_idx);

    switch (decl) {
        .s_alias_decl => |alias| {
            // Get the type header's args
            const header = self.cir.store.getTypeHeader(alias.header);
            const header_args = self.cir.store.sliceTypeAnnos(header.args);

            // Next, generate the provided arg types and build the map of rigid variables in the header
            for (header_args) |header_arg_idx| {
                const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
                const header_var = ModuleEnv.varFrom(header_arg_idx);
                switch (header_arg) {
                    .rigid_var => |rigid| {
                        try self.updateVar(header_var, .{ .rigid = Rigid.init(rigid.name) }, Rank.generalized);
                    },
                    .underscore, .malformed => {
                        try self.updateVar(header_var, .err, Rank.generalized);
                    },
                    else => {
                        // This should never be possible
                        std.debug.assert(false);
                        try self.updateVar(header_var, .err, Rank.generalized);
                    },
                }
            }

            const header_vars: []Var = @ptrCast(header_args);

            // Now we have a built of list of rigid variables for the decl lhs (header).
            // With this in hand, we can now generate the type for the lhs (body).
            self.seen_annos.clearRetainingCapacity();
            const backing_var: Var = ModuleEnv.varFrom(alias.anno);
            try self.generateAnnoTypeInPlace(alias.anno, .{ .type_decl = .{
                .idx = decl_idx,
                .name = header.name,
                .type_ = .alias,
                .backing_var = backing_var,
                .num_args = @intCast(header_args.len),
            } });

            try self.updateVar(
                decl_var,
                try self.types.mkAlias(
                    .{ .ident_idx = header.name },
                    backing_var,
                    header_vars,
                ),
                Rank.generalized,
            );
        },
        .s_nominal_decl => |nominal| {
            // Get the type header's args
            const header = self.cir.store.getTypeHeader(nominal.header);
            const header_args = self.cir.store.sliceTypeAnnos(header.args);

            // Next, generate the provided arg types and build the map of rigid variables in the header
            for (header_args) |header_arg_idx| {
                const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
                const header_var = ModuleEnv.varFrom(header_arg_idx);
                switch (header_arg) {
                    .rigid_var => |rigid| {
                        try self.updateVar(header_var, .{ .rigid = Rigid.init(rigid.name) }, Rank.generalized);
                    },
                    .underscore, .malformed => {
                        try self.updateVar(header_var, .err, Rank.generalized);
                    },
                    else => {
                        // This should never be possible
                        std.debug.assert(false);
                        try self.updateVar(header_var, .err, Rank.generalized);
                    },
                }
            }

            const header_vars: []Var = @ptrCast(header_args);

            // Now we have a built of list of rigid variables for the decl lhs (header).
            // With this in hand, we can now generate the type for the lhs (body).
            self.seen_annos.clearRetainingCapacity();
            const backing_var: Var = ModuleEnv.varFrom(nominal.anno);
            try self.generateAnnoTypeInPlace(nominal.anno, .{ .type_decl = .{
                .idx = decl_idx,
                .name = header.name,
                .type_ = .nominal,
                .backing_var = backing_var,
                .num_args = @intCast(header_args.len),
            } });

            try self.updateVar(
                decl_var,
                try self.types.mkNominal(
                    .{ .ident_idx = header.name },
                    backing_var,
                    header_vars,
                    self.common_idents.module_name,
                ),
                Rank.generalized,
            );
        },
        .s_runtime_error => {
            try self.updateVar(decl_var, .err, Rank.generalized);
        },
        else => {
            // Do nothing
        },
    }
}

// annotations //

/// The context use for free var generation
const GenTypeAnnoCtx = union(enum) {
    annotation,
    type_decl: struct {
        idx: CIR.Statement.Idx,
        name: Ident.Idx,
        type_: enum { nominal, alias },
        backing_var: Var,
        num_args: u32,
    },
};

/// Given an annotation, generate the corresponding type based on the CIR
///
/// This function will write the type into the type var node at `anno_idx`
fn generateAnnoTypeInPlace(self: *Self, anno_idx: CIR.TypeAnno.Idx, ctx: GenTypeAnnoCtx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First, check if we've seen this anno before
    // This guards against recursive types
    if (self.seen_annos.get(anno_idx)) |_| {
        return;
    }

    // Get the annotation
    const anno = self.cir.store.getTypeAnno(anno_idx);
    const anno_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(anno_idx));
    const anno_var = ModuleEnv.varFrom(anno_idx);

    // Put this anno in the "seen" map immediately, to support recursive references
    try self.seen_annos.put(anno_idx, anno_var);

    switch (anno) {
        .rigid_var => |rigid| {
            try self.updateVar(anno_var, .{ .rigid = Rigid.init(rigid.name) }, Rank.generalized);
        },
        .rigid_var_lookup => |rigid_lookup| {
            try self.types.setVarRedirect(anno_var, ModuleEnv.varFrom(rigid_lookup.ref));
        },
        .underscore => {
            try self.updateVar(anno_var, .{ .flex = Flex.init() }, Rank.generalized);
        },
        .lookup => |lookup| {
            switch (lookup.base) {
                .builtin => |builtin_type| {
                    // TODO: Don't generate a new type var here, reuse anno var
                    const builtin_var = try self.generateBuiltinTypeInstance(lookup.name, builtin_type, &.{}, anno_region);
                    try self.types.setVarRedirect(anno_var, builtin_var);
                },
                .local => |local| {
                    // Check if we're in a declaration or an annotation
                    switch (ctx) {
                        .type_decl => |this_decl| {
                            // If so, check if this is a recursive reference
                            if (this_decl.idx == local.decl_idx) {

                                // If it is a recursive ref, check that there are
                                // no arguments (since this is a lookup, not an apply)
                                if (this_decl.num_args != 0) {
                                    _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                                        .type_name = this_decl.name,
                                        .region = anno_region,
                                        .num_expected_args = 0,
                                        .num_actual_args = this_decl.num_args,
                                    } });
                                    try self.updateVar(anno_var, .err, Rank.generalized);
                                    return;
                                }

                                // If so, then update this annotation to be an instance
                                // of this type using the same backing variable
                                try self.updateVar(anno_var, blk: {
                                    switch (this_decl.type_) {
                                        .alias => {
                                            break :blk try self.types.mkAlias(
                                                .{ .ident_idx = this_decl.name },
                                                this_decl.backing_var,
                                                &.{},
                                            );
                                        },
                                        .nominal => {
                                            break :blk try self.types.mkNominal(
                                                .{ .ident_idx = this_decl.name },
                                                this_decl.backing_var,
                                                &.{},
                                                self.common_idents.module_name,
                                            );
                                        },
                                    }
                                }, Rank.generalized);

                                return;
                            }
                        },
                        .annotation => {
                            // Otherwise, we're in an annotation and this cannot
                            // be recursive
                        },
                    }

                    const instantiated_var = try self.instantiateVar(
                        ModuleEnv.varFrom(local.decl_idx),
                        Rank.generalized,
                        .{ .explicit = anno_region },
                    );
                    try self.types.setVarRedirect(anno_var, instantiated_var);
                },
                .external => |ext| {
                    if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                        const ext_instantiated_var = try self.instantiateVar(
                            ext_ref.local_var,
                            Rank.generalized,
                            .{ .explicit = anno_region },
                        );
                        try self.types.setVarRedirect(anno_var, ext_instantiated_var);
                    } else {
                        // If this external type is unresolved, can should've reported
                        // an error. So we set to error and continue
                        try self.updateVar(anno_var, .err, Rank.generalized);
                    }
                },
            }
        },
        .apply => |a| {
            // Generate the types for the arguments
            const anno_args = self.cir.store.sliceTypeAnnos(a.args);
            for (anno_args) |anno_arg| {
                try self.generateAnnoTypeInPlace(anno_arg, ctx);
            }
            const anno_arg_vars: []Var = @ptrCast(anno_args);

            switch (a.base) {
                .builtin => |builtin_type| {
                    // TODO: Don't generate a new type var here, reuse anno var
                    const builtin_var = try self.generateBuiltinTypeInstance(a.name, builtin_type, anno_arg_vars, anno_region);
                    try self.types.setVarRedirect(anno_var, builtin_var);
                },
                .local => |local| {
                    // Check if we're in a declaration or an annotation
                    switch (ctx) {
                        .type_decl => |this_decl| {
                            // If so, check if this is a recursive reference
                            if (this_decl.idx == local.decl_idx) {

                                // If it is a recursive ref, check that the args being
                                // applied here match the number of args of the decl
                                if (anno_arg_vars.len != this_decl.num_args) {
                                    _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                                        .type_name = this_decl.name,
                                        .region = anno_region,
                                        .num_expected_args = this_decl.num_args,
                                        .num_actual_args = @intCast(anno_args.len),
                                    } });
                                    try self.updateVar(anno_var, .err, Rank.generalized);
                                    return;
                                }

                                // If so, then update this annotation to be an instance
                                // of this type using the same backing variable
                                try self.updateVar(anno_var, blk: {
                                    switch (this_decl.type_) {
                                        .alias => {
                                            break :blk try self.types.mkAlias(
                                                .{ .ident_idx = this_decl.name },
                                                this_decl.backing_var,
                                                anno_arg_vars,
                                            );
                                        },
                                        .nominal => {
                                            break :blk try self.types.mkNominal(
                                                .{ .ident_idx = this_decl.name },
                                                this_decl.backing_var,
                                                anno_arg_vars,
                                                self.common_idents.module_name,
                                            );
                                        },
                                    }
                                }, Rank.generalized);

                                return;
                            }
                        },
                        .annotation => {
                            // Otherwise, we're in an annotation and this cannot
                            // be recursive
                        },
                    }

                    // Resolve the referenced type
                    const decl_var = ModuleEnv.varFrom(local.decl_idx);
                    const decl_resolved = self.types.resolveVar(decl_var).desc.content;

                    // Get the arguments & name the referenced type
                    const decl_arg_vars, const decl_name = blk: {
                        if (decl_resolved == .alias) {
                            const decl_alias = decl_resolved.alias;
                            break :blk .{ self.types.sliceAliasArgs(decl_alias), decl_alias.ident.ident_idx };
                        } else if (decl_resolved == .structure and decl_resolved.structure == .nominal_type) {
                            const decl_nominal = decl_resolved.structure.nominal_type;
                            break :blk .{ self.types.sliceNominalArgs(decl_nominal), decl_nominal.ident.ident_idx };
                        } else if (decl_resolved == .err) {
                            try self.updateVar(anno_var, .err, Rank.generalized);
                            return;
                        } else {
                            std.debug.assert(false);
                            try self.updateVar(anno_var, .err, Rank.generalized);
                            return;
                        }
                    };

                    // Check for an arity mismatch
                    if (decl_arg_vars.len != anno_arg_vars.len) {
                        _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                            .type_name = decl_name,
                            .region = anno_region,
                            .num_expected_args = @intCast(decl_arg_vars.len),
                            .num_actual_args = @intCast(anno_args.len),
                        } });
                        try self.updateVar(anno_var, .err, Rank.generalized);
                        return;
                    }

                    // Then, built the map of applied variables
                    self.rigid_var_substitutions.clearRetainingCapacity();
                    for (decl_arg_vars, anno_arg_vars) |decl_arg_var, anno_arg_var| {
                        const decl_arg_resolved = self.types.resolveVar(decl_arg_var).desc.content;

                        std.debug.assert(decl_arg_resolved == .rigid);
                        const decl_arg_rigid = decl_arg_resolved.rigid;

                        try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name, anno_arg_var);
                    }

                    // Then instantiate the variable, substituting the rigid
                    // variables in the definition with the applied args from
                    // the annotation
                    const instantiated_var = try self.instantiateVarWithSubs(
                        decl_var,
                        &self.rigid_var_substitutions,
                        Rank.generalized,
                        .{ .explicit = anno_region },
                    );
                    try self.types.setVarRedirect(anno_var, instantiated_var);
                },
                .external => |ext| {
                    if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                        // Resolve the referenced type
                        const ext_resolved = self.types.resolveVar(ext_ref.local_var).desc.content;

                        // Get the arguments & name the referenced type
                        const ext_arg_vars, const ext_name = blk: {
                            if (ext_resolved == .alias) {
                                const decl_alias = ext_resolved.alias;
                                break :blk .{ self.types.sliceAliasArgs(decl_alias), decl_alias.ident.ident_idx };
                            } else if (ext_resolved == .structure and ext_resolved.structure == .nominal_type) {
                                const decl_nominal = ext_resolved.structure.nominal_type;
                                break :blk .{ self.types.sliceNominalArgs(decl_nominal), decl_nominal.ident.ident_idx };
                            } else if (ext_resolved == .err) {
                                try self.updateVar(anno_var, .err, Rank.generalized);
                                return;
                            } else {
                                std.debug.assert(false);
                                try self.updateVar(anno_var, .err, Rank.generalized);
                                return;
                            }
                        };

                        // Check for an arity mismatch
                        if (ext_arg_vars.len != anno_arg_vars.len) {
                            _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                                .type_name = ext_name,
                                .region = anno_region,
                                .num_expected_args = @intCast(ext_arg_vars.len),
                                .num_actual_args = @intCast(anno_args.len),
                            } });
                            try self.updateVar(anno_var, .err, Rank.generalized);
                            return;
                        }

                        // Then, built the map of applied variables
                        self.rigid_var_substitutions.clearRetainingCapacity();
                        for (ext_arg_vars, anno_arg_vars) |decl_arg_var, anno_arg_var| {
                            const decl_arg_resolved = self.types.resolveVar(decl_arg_var).desc.content;

                            std.debug.assert(decl_arg_resolved == .rigid);
                            const decl_arg_rigid = decl_arg_resolved.rigid;

                            try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name, anno_arg_var);
                        }

                        // Then instantiate the variable, substituting the rigid
                        // variables in the definition with the applied args from
                        // the annotation
                        const instantiated_var = try self.instantiateVarWithSubs(
                            ext_ref.local_var,
                            &self.rigid_var_substitutions,
                            Rank.generalized,
                            .{ .explicit = anno_region },
                        );
                        try self.types.setVarRedirect(anno_var, instantiated_var);
                    } else {
                        // If this external type is unresolved, can should've reported
                        // an error. So we set to error and continue
                        try self.updateVar(anno_var, .err, Rank.generalized);
                    }
                },
            }
        },
        .@"fn" => |func| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, ctx);
            }
            const args_var_slice: []Var = @ptrCast(args_anno_slice);

            try self.generateAnnoTypeInPlace(func.ret, ctx);

            const fn_type = inner_blk: {
                if (func.effectful) {
                    break :inner_blk try self.types.mkFuncEffectful(args_var_slice, ModuleEnv.varFrom(func.ret));
                } else {
                    break :inner_blk try self.types.mkFuncPure(args_var_slice, ModuleEnv.varFrom(func.ret));
                }
            };
            try self.updateVar(anno_var, fn_type, Rank.generalized);
        },
        .tag_union => |tag_union| {
            const scratch_tags_top = self.scratch_tags.top();
            defer self.scratch_tags.clearFrom(scratch_tags_top);

            const tag_anno_slices = self.cir.store.sliceTypeAnnos(tag_union.tags);
            for (tag_anno_slices) |tag_anno_idx| {
                // Get the tag anno
                const tag_type_anno = self.cir.store.getTypeAnno(tag_anno_idx);

                // If the child of the tag union is not a tag, then set as error
                // Canonicalization should have reported this error
                if (tag_type_anno != .tag) {
                    try self.updateVar(anno_var, .err, Rank.generalized);
                    return;
                }
                const tag = tag_type_anno.tag;

                // Generate the types for each tag arg
                const tag_anno_args_slice = self.cir.store.sliceTypeAnnos(tag.args);
                for (tag_anno_args_slice) |tag_arg_idx| {
                    try self.generateAnnoTypeInPlace(tag_arg_idx, ctx);
                }
                const tag_vars_slice: []Var = @ptrCast(tag_anno_args_slice);

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
                    try self.generateAnnoTypeInPlace(ext_anno_idx, ctx);
                    break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                } else {
                    break :inner_blk try self.freshFromContent(.{ .structure = .empty_tag_union }, Rank.generalized, anno_region);
                }
            };

            // Set the anno's type
            try self.updateVar(anno_var, try self.types.mkTagUnion(tags_slice, ext_var), Rank.generalized);
        },
        .tag => {
            // This indicates a malformed type annotation. Tags should only
            // exist as direct children of tag_unions
            std.debug.assert(false);
            try self.updateVar(anno_var, .err, Rank.generalized);
        },
        .record => |rec| {
            const scratch_record_fields_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(scratch_record_fields_top);

            const recs_anno_slice = self.cir.store.sliceAnnoRecordFields(rec.fields);

            for (recs_anno_slice) |rec_anno_idx| {
                const rec_field = self.cir.store.getAnnoRecordField(rec_anno_idx);

                try self.generateAnnoTypeInPlace(rec_field.ty, ctx);
                const record_field_var = ModuleEnv.varFrom(rec_field.ty);

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
            try self.updateVar(
                anno_var,
                .{ .structure = types_mod.FlatType{ .record = .{
                    .fields = fields_type_range,
                    .ext = ext_var,
                } } },
                Rank.generalized,
            );
        },
        .tuple => |tuple| {
            const elems_anno_slice = self.cir.store.sliceTypeAnnos(tuple.elems);
            for (elems_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, ctx);
            }
            const elems_range = try self.types.appendVars(@ptrCast(elems_anno_slice));
            try self.updateVar(anno_var, .{ .structure = .{ .tuple = .{ .elems = elems_range } } }, Rank.generalized);
        },
        .parens => |parens| {
            try self.generateAnnoTypeInPlace(parens.anno, ctx);
            try self.types.setVarRedirect(anno_var, ModuleEnv.varFrom(parens.anno));
        },
        .malformed => {
            try self.updateVar(anno_var, .err, Rank.generalized);
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
                .num = .{ .num_poly = anno_args[0] },
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
            const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                .frac_unbound = Num.FracRequirements.init(),
            } } }, Rank.generalized, anno_region);
            return try self.freshFromContent(.{ .structure = .{ .num = .{
                .num_poly = frac_var,
            } } }, Rank.generalized, anno_region);
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
            const int_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                .int_unbound = Num.IntRequirements.init(),
            } } }, Rank.generalized, anno_region);
            return try self.freshFromContent(.{ .structure = .{ .num = .{
                .num_poly = int_var,
            } } }, Rank.generalized, anno_region);
        },
    }
}

// pattern //

/// Check the types for the provided pattern
fn checkPattern(self: *Self, pattern_idx: CIR.Pattern.Idx, rank: types_mod.Rank, expected: Expected) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern = self.cir.store.getPattern(pattern_idx);
    const pattern_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(pattern_idx));
    const pattern_var = ModuleEnv.varFrom(pattern_idx);

    switch (pattern) {
        .assign => |_| {
            // In the case of an assigned variable, set it to be a flex var initially.
            // This will be refined based on how it's used.
            try self.updateVar(pattern_var, .{ .flex = Flex.init() }, rank);
        },
        .underscore => |_| {
            // Underscore can be anything
            try self.updateVar(pattern_var, .{ .flex = Flex.init() }, rank);
        },
        // str //
        .str_literal => {
            try self.updateVar(pattern_var, .{ .structure = .str }, rank);
        },
        // as //
        .as => |p| {
            try self.checkPattern(p.pattern, rank, expected);
        },
        // tuple //
        .tuple => |tuple| {
            // Check tuple elements
            const elems_slice = self.cir.store.slicePatterns(tuple.patterns);
            for (elems_slice) |single_elem_ptrn_idx| {
                try self.checkPattern(single_elem_ptrn_idx, rank, .no_expectation);
            }

            // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
            const elem_vars_slice = try self.types.appendVars(@ptrCast(elems_slice));

            // Set the type in the store
            try self.updateVar(pattern_var, .{ .structure = .{
                .tuple = .{ .elems = elem_vars_slice },
            } }, rank);
        },
        // list //
        .list => |list| {
            const elems = self.cir.store.slicePatterns(list.patterns);
            if (elems.len == 0) {
                // If we have no elems, then set the type and move on
                try self.updateVar(pattern_var, .{ .structure = .list_unbound }, rank);
            } else {
                // Here, we use the list's 1st element as the element var to
                // constrain the rest of the list

                // Check the first elem
                try self.checkPattern(elems[0], rank, .no_expectation);

                // Iterate over the remaining elements
                const elem_var = ModuleEnv.varFrom(elems[0]);
                var last_elem_ptrn_idx = elems[0];
                for (elems[1..], 1..) |elem_ptrn_idx, i| {
                    try self.checkPattern(elem_ptrn_idx, rank, .no_expectation);
                    const cur_elem_var = ModuleEnv.varFrom(elem_ptrn_idx);

                    // Unify each element's var with the list's elem var
                    const result = try self.unify(elem_var, cur_elem_var, rank);
                    self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_ptrn_idx),
                        .incompatible_elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            try self.checkPattern(remaining_elem_expr_idx, rank, .no_expectation);
                        }

                        // Break to avoid cascading errors
                        break;
                    }

                    last_elem_ptrn_idx = elem_ptrn_idx;
                }

                // Now, set the type of the root variable t
                try self.updateVar(pattern_var, .{ .structure = .{ .list = elem_var } }, rank);

                // Then, check the "rest" pattern is bound to a variable
                // This is if the pattern is like `.. as x`
                if (list.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern_idx| {
                        try self.checkPattern(rest_pattern_idx, rank, .no_expectation);
                        const rest_pattern_var = ModuleEnv.varFrom(rest_pattern_idx);

                        _ = try self.unify(pattern_var, rest_pattern_var, rank);
                    }
                }
            }
        },
        // applied tag //
        .applied_tag => |applied_tag| {
            // Create a tag type in the type system and assign it the expr_var

            // Process each tag arg
            const arg_ptrn_idx_slice = self.cir.store.slicePatterns(applied_tag.args);
            for (arg_ptrn_idx_slice) |arg_expr_idx| {
                try self.checkPattern(arg_expr_idx, rank, .no_expectation);
            }

            // Create the type
            const ext_var = try self.fresh(rank, pattern_region);
            try self.var_pool.addVarToRank(ext_var, rank);

            const tag = try self.types.mkTag(applied_tag.name, @ptrCast(arg_ptrn_idx_slice));
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.updateVar(pattern_var, tag_union_content, rank);
        },
        // nominal //
        .nominal => |nominal| {
            // TODO: Merge this with e_nominal_external

            // First, check the type inside the expr
            try self.checkPattern(nominal.backing_pattern, rank, .no_expectation);
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_pattern);

            // Then, we need an instance of the nominal type being referenced
            // E.g. ConList.Cons(...)
            //      ^^^^^^^
            const nominal_var = try self.instantiateVar(ModuleEnv.varFrom(nominal.nominal_type_decl), rank, .{ .explicit = pattern_region });
            const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

            if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                // Then, we extract the variable of the nominal type
                // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                const nominal_backing_var = self.types.getNominalBackingVar(nominal_resolved.structure.nominal_type);

                // Now we unify what the user wrote with the backing type of the nominal was
                // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                const result = try self.unify(nominal_backing_var, actual_backing_var, rank);

                // Then, we handle the result of unification
                switch (result) {
                    .ok => {
                        // If that unify call succeeded, then we this is a valid instance
                        // of this nominal type. So we set the expr's type to be the
                        // nominal type
                        try self.types.setVarRedirect(pattern_var, nominal_var);
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
                        try self.updateVar(pattern_var, .err, rank);
                    },
                }
            } else {
                // If the nominal type is actually something else, then set the
                // whole expression to be an error.
                //
                // TODO: Report a nice problem here
                try self.updateVar(pattern_var, .err, rank);
            }
        },
        .nominal_external => |nominal| {
            // TODO: Merge this with e_nominal

            // First, check the type inside the expr
            try self.checkPattern(nominal.backing_pattern, rank, .no_expectation);
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_pattern);

            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Then, we need an instance of the nominal type being referenced
                // E.g. ConList.Cons(...)
                //      ^^^^^^^
                const nominal_var = try self.instantiateVar(ext_ref.local_var, Rank.generalized, .{ .explicit = pattern_region });
                const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

                if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                    // Then, we extract the variable of the nominal type
                    // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                    //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const nominal_backing_var = self.types.getNominalBackingVar(nominal_resolved.structure.nominal_type);

                    // Now we unify what the user wrote with the backing type of the nominal was
                    // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                    //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const result = try self.unify(nominal_backing_var, actual_backing_var, rank);

                    // Then, we handle the result of unification
                    switch (result) {
                        .ok => {
                            // If that unify call succeeded, then we this is a valid instance
                            // of this nominal type. So we set the expr's type to be the
                            // nominal type
                            try self.types.setVarRedirect(pattern_var, nominal_var);
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
                            try self.updateVar(pattern_var, .err, rank);
                        },
                    }
                } else {
                    // If the nominal type is actually something else, then set the
                    // whole expression to be an error.
                    //
                    // TODO: Report a nice problem here
                    try self.updateVar(pattern_var, .err, rank);
                }
            } else {
                try self.updateVar(pattern_var, .err, rank);
            }
        },
        // record destructure //
        .record_destructure => |destructure| {
            const scratch_records_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(scratch_records_top);

            for (self.cir.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                const destruct_var = ModuleEnv.varFrom(destruct_idx);

                // Check the sub pattern
                const field_pattern_idx = blk: {
                    switch (destruct.kind) {
                        .Required => |sub_pattern_idx| {
                            try self.checkPattern(sub_pattern_idx, rank, .no_expectation);
                            break :blk sub_pattern_idx;
                        },
                        .SubPattern => |sub_pattern_idx| {
                            try self.checkPattern(sub_pattern_idx, rank, .no_expectation);
                            break :blk sub_pattern_idx;
                        },
                    }
                };
                const field_pattern_var = ModuleEnv.varFrom(field_pattern_idx);

                // Set the destruct var to redirect to the field pattern var
                try self.types.setVarRedirect(destruct_var, field_pattern_var);

                // Append it to the scratch records array
                try self.scratch_record_fields.append(self.gpa, types_mod.RecordField{
                    .name = destruct.label,
                    .var_ = ModuleEnv.varFrom(destruct_var),
                });
            }

            // Copy the scratch record fields into the types store
            const record_fields_scratch = self.scratch_record_fields.sliceFromStart(scratch_records_top);
            std.mem.sort(types_mod.RecordField, record_fields_scratch, self.cir.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
            const record_fields_range = try self.types.appendRecordFields(record_fields_scratch);

            // Update the pattern var
            try self.updateVar(pattern_var, .{ .structure = .{
                .record_unbound = record_fields_range,
            } }, rank);
        },
        // nums //
        .num_literal => |num| {
            const num_type = blk: {
                switch (num.kind) {
                    .num_unbound => {
                        const int_reqs = num.value.toIntRequirements();
                        const frac_reqs = num.value.toFracRequirements();
                        break :blk Num{ .num_unbound = .{ .int_requirements = int_reqs, .frac_requirements = frac_reqs } };
                    },
                    .int_unbound => {
                        const int_reqs = num.value.toIntRequirements();
                        const int_var = try self.freshFromContent(.{ .structure = .{ .num = .{ .int_unbound = int_reqs } } }, rank, pattern_region);
                        try self.var_pool.addVarToRank(int_var, rank);
                        break :blk Num{ .num_poly = int_var };
                    },
                    .u8 => break :blk Num{ .num_compact = Num.Compact{ .int = .u8 } },
                    .i8 => break :blk Num{ .num_compact = Num.Compact{ .int = .i8 } },
                    .u16 => break :blk Num{ .num_compact = Num.Compact{ .int = .u16 } },
                    .i16 => break :blk Num{ .num_compact = Num.Compact{ .int = .i16 } },
                    .u32 => break :blk Num{ .num_compact = Num.Compact{ .int = .u32 } },
                    .i32 => break :blk Num{ .num_compact = Num.Compact{ .int = .i32 } },
                    .u64 => break :blk Num{ .num_compact = Num.Compact{ .int = .u64 } },
                    .i64 => break :blk Num{ .num_compact = Num.Compact{ .int = .i64 } },
                    .u128 => break :blk Num{ .num_compact = Num.Compact{ .int = .u128 } },
                    .i128 => break :blk Num{ .num_compact = Num.Compact{ .int = .i128 } },
                    .f32 => break :blk Num{ .num_compact = Num.Compact{ .frac = .f32 } },
                    .f64 => break :blk Num{ .num_compact = Num.Compact{ .frac = .f64 } },
                    .dec => break :blk Num{ .num_compact = Num.Compact{ .frac = .dec } },
                }
            };

            // Update the pattern var
            try self.updateVar(pattern_var, .{ .structure = .{ .num = num_type } }, rank);
        },
        .frac_f32_literal => |_| {
            try self.updateVar(pattern_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } }, rank);
        },
        .frac_f64_literal => |_| {
            try self.updateVar(pattern_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } }, rank);
        },
        .dec_literal => |dec| {
            if (dec.has_suffix) {
                try self.updateVar(pattern_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const f64_val = dec.value.toF64();
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.CIR.fitsInF32(f64_val),
                    .fits_in_dec = can.CIR.fitsInDec(f64_val),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = requirements,
                } } }, rank, pattern_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(pattern_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        .small_dec_literal => |dec| {
            if (dec.has_suffix) {
                try self.updateVar(pattern_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const reqs = dec.value.toFracRequirements();
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = reqs,
                } } }, rank, pattern_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(pattern_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        .runtime_error => {
            try self.updateVar(pattern_var, .err, rank);
        },
    }

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyFromAnno(expected_type.var_, pattern_var, rank);
            } else {
                _ = try self.unify(expected_type.var_, pattern_var, rank);
            }
        },
    }
}

// expr //

const Expected = union(enum) {
    no_expectation,
    expected: struct { var_: Var, from_annotation: bool },
};

fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx, rank: types_mod.Rank, expected: Expected) std.mem.Allocator.Error!bool {
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
                does_fx = try self.checkExpr(seg_expr_idx, rank, .no_expectation) or does_fx;

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
            const num_type = blk: {
                switch (num.kind) {
                    .num_unbound => {
                        const int_reqs = num.value.toIntRequirements();
                        const frac_reqs = num.value.toFracRequirements();
                        break :blk Num{ .num_unbound = .{ .int_requirements = int_reqs, .frac_requirements = frac_reqs } };
                    },
                    .int_unbound => {
                        const int_reqs = num.value.toIntRequirements();
                        const int_var = try self.freshFromContent(.{ .structure = .{ .num = .{ .int_unbound = int_reqs } } }, rank, expr_region);
                        try self.var_pool.addVarToRank(int_var, rank);
                        break :blk Num{ .num_poly = int_var };
                    },
                    .u8 => break :blk Num{ .num_compact = Num.Compact{ .int = .u8 } },
                    .i8 => break :blk Num{ .num_compact = Num.Compact{ .int = .i8 } },
                    .u16 => break :blk Num{ .num_compact = Num.Compact{ .int = .u16 } },
                    .i16 => break :blk Num{ .num_compact = Num.Compact{ .int = .i16 } },
                    .u32 => break :blk Num{ .num_compact = Num.Compact{ .int = .u32 } },
                    .i32 => break :blk Num{ .num_compact = Num.Compact{ .int = .i32 } },
                    .u64 => break :blk Num{ .num_compact = Num.Compact{ .int = .u64 } },
                    .i64 => break :blk Num{ .num_compact = Num.Compact{ .int = .i64 } },
                    .u128 => break :blk Num{ .num_compact = Num.Compact{ .int = .u128 } },
                    .i128 => break :blk Num{ .num_compact = Num.Compact{ .int = .i128 } },
                    .f32 => break :blk Num{ .num_compact = Num.Compact{ .frac = .f32 } },
                    .f64 => break :blk Num{ .num_compact = Num.Compact{ .frac = .f64 } },
                    .dec => break :blk Num{ .num_compact = Num.Compact{ .frac = .dec } },
                }
            };

            // Update the pattern var
            try self.updateVar(expr_var, .{ .structure = .{ .num = num_type } }, rank);
        },
        .e_frac_f32 => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } }, rank);
            } else {
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = true,
                    .fits_in_dec = can.CIR.fitsInDec(@floatCast(frac.value)),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = requirements,
                } } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        .e_frac_f64 => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } }, rank);
            } else {
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.CIR.fitsInF32(@floatCast(frac.value)),
                    .fits_in_dec = can.CIR.fitsInDec(@floatCast(frac.value)),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = requirements,
                } } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        .e_dec => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const f64_val = frac.value.toF64();
                const requirements = types_mod.Num.FracRequirements{
                    .fits_in_f32 = can.CIR.fitsInF32(f64_val),
                    .fits_in_dec = can.CIR.fitsInDec(f64_val),
                };
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = requirements,
                } } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        .e_dec_small => |frac| {
            if (frac.has_suffix) {
                try self.updateVar(expr_var, .{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } }, rank);
            } else {
                const reqs = frac.value.toFracRequirements();
                const frac_var = try self.freshFromContent(.{ .structure = .{ .num = .{
                    .frac_unbound = reqs,
                } } }, rank, expr_region);
                try self.var_pool.addVarToRank(frac_var, rank);

                try self.updateVar(expr_var, .{ .structure = .{ .num = .{
                    .num_poly = frac_var,
                } } }, rank);
            }
        },
        // list //
        .e_empty_list => {
            try self.updateVar(expr_var, .{ .structure = .list_unbound }, rank);
        },
        .e_list => |list| {
            const elems = self.cir.store.exprSlice(list.elems);

            if (elems.len == 0) {
                // If we have no elems, then set the type and move on
                try self.updateVar(expr_var, .{ .structure = .list_unbound }, rank);
            } else {
                // Here, we use the list's 1st element as the element var to
                // constrain the rest of the list

                // Check the first elem
                does_fx = try self.checkExpr(elems[0], rank, .no_expectation) or does_fx;

                // Iterate over the remaining elements
                const elem_var = ModuleEnv.varFrom(elems[0]);
                var last_elem_expr_idx = elems[0];
                for (elems[1..], 1..) |elem_expr_idx, i| {
                    does_fx = try self.checkExpr(elem_expr_idx, rank, .no_expectation) or does_fx;
                    const cur_elem_var = ModuleEnv.varFrom(elem_expr_idx);

                    // Unify each element's var with the list's elem var
                    const result = try self.unify(elem_var, cur_elem_var, rank);
                    self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_expr_idx),
                        .incompatible_elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            does_fx = try self.checkExpr(remaining_elem_expr_idx, rank, .no_expectation) or does_fx;
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
                does_fx = try self.checkExpr(single_elem_expr_idx, rank, .no_expectation) or does_fx;
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
                does_fx = try self.checkExpr(field.value, rank, .no_expectation) or does_fx;

                // Append it to the scratch records array
                try self.scratch_record_fields.append(self.gpa, types_mod.RecordField{
                    .name = field.name,
                    .var_ = ModuleEnv.varFrom(field.value),
                });
            }

            // Copy the scratch fields into the types store
            const record_fields_scratch = self.scratch_record_fields.sliceFromStart(record_fields_top);
            std.mem.sort(types_mod.RecordField, record_fields_scratch, self.cir.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
            const record_fields_range = try self.types.appendRecordFields(record_fields_scratch);

            // Check if we have an ext
            if (e.ext) |ext_expr| {
                does_fx = try self.checkExpr(ext_expr, rank, .no_expectation) or does_fx;
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
            try self.updateVar(expr_var, .{ .structure = .empty_record }, rank);
        },
        // tags //
        .e_zero_argument_tag => |e| {
            const ext_var = try self.fresh(rank, expr_region);
            try self.var_pool.addVarToRank(ext_var, rank);

            const tag = try self.types.mkTag(e.name, &.{});
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.updateVar(expr_var, tag_union_content, rank);
        },
        .e_tag => |e| {
            // Create a tag type in the type system and assign it the expr_var

            // Process each tag arg
            const arg_expr_idx_slice = self.cir.store.sliceExpr(e.args);
            for (arg_expr_idx_slice) |arg_expr_idx| {
                does_fx = try self.checkExpr(arg_expr_idx, rank, .no_expectation) or does_fx;
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
        .e_nominal => |nominal| {
            // TODO: Merge this with e_nominal_external

            // First, check the type inside the expr
            does_fx = try self.checkExpr(nominal.backing_expr, rank, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            // Then, we need an instance of the nominal type being referenced
            // E.g. ConList.Cons(...)
            //      ^^^^^^^
            const nominal_var = try self.instantiateVar(ModuleEnv.varFrom(nominal.nominal_type_decl), rank, .{ .explicit = expr_region });
            const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

            if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                // Then, we extract the variable of the nominal type
                // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                const nominal_backing_var = self.types.getNominalBackingVar(nominal_resolved.structure.nominal_type);

                // Now we unify what the user wrote with the backing type of the nominal was
                // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                const result = try self.unify(nominal_backing_var, actual_backing_var, rank);

                // Then, we handle the result of unification
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
            } else {
                // If the nominal type is actually something else, then set the
                // whole expression to be an error.
                //
                // TODO: Report a nice problem here
                try self.updateVar(expr_var, .err, rank);
            }
        },
        .e_nominal_external => |nominal| {
            // TODO: Merge this with e_nominal

            // First, check the type inside the expr
            does_fx = try self.checkExpr(nominal.backing_expr, rank, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Then, we need an instance of the nominal type being referenced
                // E.g. ConList.Cons(...)
                //      ^^^^^^^
                const nominal_var = try self.instantiateVar(ext_ref.local_var, Rank.generalized, .{ .explicit = expr_region });
                const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

                if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                    // Then, we extract the variable of the nominal type
                    // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                    //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const nominal_backing_var = self.types.getNominalBackingVar(nominal_resolved.structure.nominal_type);

                    // Now we unify what the user wrote with the backing type of the nominal was
                    // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                    //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const result = try self.unify(nominal_backing_var, actual_backing_var, rank);

                    // Then, we handle the result of unification
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
                } else {
                    // If the nominal type is actually something else, then set the
                    // whole expression to be an error.
                    //
                    // TODO: Report a nice problem here
                    try self.updateVar(expr_var, .err, rank);
                }
            } else {
                try self.updateVar(expr_var, .err, rank);
            }
        },
        // lookup //
        .e_lookup_local => |lookup| {
            const pat_var = ModuleEnv.varFrom(lookup.pattern_idx);
            const resolved_pat = self.types.resolveVar(pat_var).desc;

            // We never instantiate rigid variables
            if (resolved_pat.rank == Rank.generalized and resolved_pat.content != .rigid) {
                const instantiated = try self.instantiateVar(pat_var, rank, .use_last_var);
                _ = try self.types.setVarRedirect(expr_var, instantiated);
            } else {
                _ = try self.types.setVarRedirect(expr_var, pat_var);
            }

            // Unify this expression with the referenced pattern
        },
        .e_lookup_external => |ext| {
            if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                const ext_instantiated_var = try self.instantiateVar(
                    ext_ref.local_var,
                    Rank.generalized,
                    .{ .explicit = expr_region },
                );
                try self.types.setVarRedirect(expr_var, ext_instantiated_var);
            } else {
                try self.updateVar(expr_var, .err, rank);
            }
        },
        // block //
        .e_block => |block| {
            const anno_free_vars_top = self.anno_free_vars.top();
            defer self.anno_free_vars.clearFrom(anno_free_vars_top);

            // Check all statements in the block
            const statements = self.cir.store.sliceStatements(block.stmts);
            for (statements) |stmt_idx| {
                const stmt = self.cir.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl_stmt| {
                        // Check the pattern
                        try self.checkPattern(decl_stmt.pattern, rank, .no_expectation);
                        const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                        // Check the annotation, if it exists
                        const check_mode = blk: {
                            if (decl_stmt.anno) |anno_idx| {
                                const annotation = self.cir.store.getAnnotation(anno_idx);

                                // Generate the annotation type var in-place
                                self.seen_annos.clearRetainingCapacity();
                                try self.generateAnnoTypeInPlace(annotation.type_anno, .annotation);

                                // Update the outer anno to redirect to the inner anno
                                const anno_var = ModuleEnv.varFrom(anno_idx);
                                const type_anno_var = ModuleEnv.varFrom(annotation.type_anno);
                                try self.types.setVarRedirect(anno_var, type_anno_var);

                                // Return the expectation
                                break :blk Expected{
                                    .expected = .{ .var_ = type_anno_var, .from_annotation = true },
                                };
                            } else {
                                break :blk Expected.no_expectation;
                            }
                        };

                        {
                            // Enter a new rank
                            try self.var_pool.pushRank();
                            defer self.var_pool.popRank();

                            const next_rank = rank.next();
                            std.debug.assert(next_rank == self.var_pool.current_rank);

                            does_fx = try self.checkExpr(decl_stmt.expr, next_rank, check_mode) or does_fx;

                            // Now that we are existing the scope, we must generalize then pop this rank
                            try self.generalizer.generalize(&self.var_pool, next_rank);
                        }

                        // Unify the pattern with the expression
                        const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);
                        _ = try self.unify(decl_pattern_var, decl_expr_var, rank);
                    },
                    .s_reassign => |reassign| {
                        // Check the pattern
                        try self.checkPattern(reassign.pattern_idx, rank, .no_expectation);
                        const reassign_pattern_var: Var = ModuleEnv.varFrom(reassign.pattern_idx);

                        {
                            // Enter a new rank
                            try self.var_pool.pushRank();
                            defer self.var_pool.popRank();

                            const next_rank = rank.next();
                            std.debug.assert(next_rank == self.var_pool.current_rank);

                            does_fx = try self.checkExpr(reassign.expr, next_rank, .no_expectation) or does_fx;

                            // Now that we are existing the scope, we must generalize then pop this rank
                            try self.generalizer.generalize(&self.var_pool, next_rank);
                        }

                        // Unify the pattern with the expression
                        const reassign_expr_var: Var = ModuleEnv.varFrom(reassign.expr);
                        _ = try self.unify(reassign_pattern_var, reassign_expr_var, rank);
                    },
                    .s_expr => |expr_stmt| {
                        does_fx = try self.checkExpr(expr_stmt.expr, rank, .no_expectation) or does_fx;
                    },
                    .s_expect => |expr_stmt| {
                        does_fx = try self.checkExpr(expr_stmt.body, rank, .no_expectation) or does_fx;
                        const stmt_expr: Var = ModuleEnv.varFrom(expr_stmt.body);

                        const bool_var = try self.freshBool(rank, expr_region);
                        _ = try self.unify(bool_var, stmt_expr, rank);
                    },
                    .s_var => |var_stmt| {

                        // Check the pattern
                        try self.checkPattern(var_stmt.pattern_idx, rank, .no_expectation);
                        const var_pattern_var: Var = ModuleEnv.varFrom(var_stmt.pattern_idx);

                        {
                            // Enter a new rank
                            try self.var_pool.pushRank();
                            defer self.var_pool.popRank();

                            const next_rank = rank.next();
                            std.debug.assert(next_rank == self.var_pool.current_rank);

                            does_fx = try self.checkExpr(var_stmt.expr, next_rank, Expected.no_expectation) or does_fx;

                            // Now that we are existing the scope, we must generalize then pop this rank
                            try self.generalizer.generalize(&self.var_pool, next_rank);
                        }

                        // Unify the pattern with the expression
                        const var_expr_var: Var = ModuleEnv.varFrom(var_stmt.expr);
                        _ = try self.unify(var_pattern_var, var_expr_var, rank);
                    },
                    else => {
                        // TODO
                    },
                }
            }

            // Check the final expression
            does_fx = try self.checkExpr(block.final_expr, rank, expected) or does_fx;

            // Link the root expr with the final expr
            try self.types.setVarRedirect(expr_var, ModuleEnv.varFrom(block.final_expr));
        },
        // function //
        .e_lambda => |lambda| {
            // Annotation-aware lambda type checking produces much better error
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

            // Enter the next rank
            try self.var_pool.pushRank();
            defer self.var_pool.popRank();

            const next_rank = rank.next();
            std.debug.assert(next_rank == self.var_pool.current_rank);

            // Check the argument patterns
            // This must happen *before* checking against the expected type so
            // all the pattern types are inferred
            const arg_pattern_idxs = self.cir.store.slicePatterns(lambda.args);
            for (arg_pattern_idxs) |pattern_idx| {
                try self.checkPattern(pattern_idx, next_rank, .no_expectation);
            }

            // Now, check if we have an expected function to validate against
            if (mb_expected_func) |expected_func| {
                const expected_func_args = self.types.sliceVars(expected_func.args);

                // Next, check if the arguments arities match
                if (expected_func_args.len == arg_pattern_idxs.len) {
                    // If so, check each argument, passing in the expected type

                    // First, find all the rigid variables in a the function's type
                    // and unify the matching corresponding lambda arguments together.
                    for (expected_func_args, 0..) |expected_arg_1, i| {
                        const expected_resolved_1 = self.types.resolveVar(expected_arg_1);

                        // The expected type is an annotation and as such,
                        // should never contain a flex var. If it did, that
                        // would indicate that the annotation is malformed
                        // std.debug.assert(expected_resolved_1.desc.content != .flex);

                        // Skip any concrete arguments
                        if (expected_resolved_1.desc.content != .rigid) {
                            continue;
                        }

                        // Look for other arguments with the same type variable
                        for (expected_func_args[i + 1 ..], i + 1..) |expected_arg_2, j| for_blk: {
                            const expected_resolved_2 = self.types.resolveVar(expected_arg_2);
                            if (expected_resolved_1.var_ == expected_resolved_2.var_) {
                                // These two argument indexes in the called *function's*
                                // type have the same rigid variable! So, we unify
                                // the corresponding *lambda args*

                                const arg_1 = @as(Var, ModuleEnv.varFrom(arg_pattern_idxs[i]));
                                const arg_2 = @as(Var, ModuleEnv.varFrom(arg_pattern_idxs[j]));

                                const unify_result = try self.unify(arg_1, arg_2, rank);
                                if (unify_result.isProblem()) {
                                    // Use the new error detail for bound type variable incompatibility
                                    self.setProblemTypeMismatchDetail(unify_result.problem, .{
                                        .incompatible_fn_args_bound_var = .{
                                            .fn_name = null, // TODO: Use function name?
                                            .first_arg_var = arg_1,
                                            .second_arg_var = arg_2,
                                            .first_arg_index = @intCast(i),
                                            .second_arg_index = @intCast(j),
                                            .num_args = @intCast(arg_pattern_idxs.len),
                                        },
                                    });

                                    // Stop execution
                                    _ = try self.updateVar(expr_var, .err, rank);
                                    break :for_blk;
                                }
                            }
                        }
                    }

                    // Then, lastly, we unify the annotation types against the
                    // actual type
                    for (expected_func_args, arg_pattern_idxs) |expected_arg_var, pattern_idx| {
                        if (is_expected_from_anno) {
                            _ = try self.unifyFromAnno(expected_arg_var, ModuleEnv.varFrom(pattern_idx), next_rank);
                        } else {
                            _ = try self.unify(expected_arg_var, ModuleEnv.varFrom(pattern_idx), next_rank);
                        }
                    }
                } else {
                    // This means the expected type and the actual lambda have
                    // an arity mismatch. This will be caught by the regular
                    // expectation checking code at the bottom of this function
                }
            }
            const arg_vars: []Var = @ptrCast(arg_pattern_idxs);

            // Check the the body of the expr
            // If we have an expected function, use that as the expr's expected type
            if (mb_expected_func) |expected_func| {
                does_fx = try self.checkExpr(lambda.body, next_rank, .{
                    .expected = .{ .var_ = expected_func.ret, .from_annotation = is_expected_from_anno },
                }) or does_fx;
            } else {
                does_fx = try self.checkExpr(lambda.body, next_rank, .no_expectation) or does_fx;
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
            does_fx = try self.checkExpr(closure.lambda_idx, rank, expected) or does_fx;
            _ = try self.types.setVarRedirect(expr_var, ModuleEnv.varFrom(closure.lambda_idx));
        },
        // function calling //
        .e_call => |call| {
            switch (call.called_via) {
                .apply => blk: {
                    // First, check the function being called
                    // It could be effectful, e.g. `(mk_fn!())(arg)`
                    does_fx = try self.checkExpr(call.func, rank, .no_expectation) or does_fx;
                    const func_var = ModuleEnv.varFrom(call.func);

                    // Second, check the arguments being called
                    // It could be effectful, e.g. `fn(mk_arg!())`
                    const call_arg_expr_idxs = self.cir.store.sliceExpr(call.args);
                    for (call_arg_expr_idxs) |call_arg_idx| {
                        does_fx = try self.checkExpr(call_arg_idx, rank, .no_expectation) or does_fx;
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
                            // and unify the matching corresponding call arguments together.
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
                                std.debug.assert(expected_resolved_1.desc.content != .rigid);

                                // Skip any concrete arguments
                                if (expected_resolved_1.desc.content != .flex) {
                                    continue;
                                }

                                // Look for other arguments with the same type variable
                                for (func_args[i + 1 ..], i + 1..) |expected_arg_2, j| {
                                    const expected_resolved_2 = self.types.resolveVar(expected_arg_2);
                                    if (expected_resolved_1.var_ == expected_resolved_2.var_) {
                                        // These two argument indexes in the called *function's*
                                        // type have the same rigid variable! So, we unify
                                        // the corresponding *call args*

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

                                            // Stop execution
                                            _ = try self.updateVar(expr_var, .err, rank);
                                            break :blk;
                                        }
                                    }
                                }
                            }

                            // Check the function's arguments against the actual
                            // called arguments, unifying each one
                            for (func_args, call_arg_expr_idxs, 0..) |expected_arg_var, call_expr_idx, arg_index| {
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

                                    // Stop execution
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
                            // inferred function type with the expected function
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
        .e_if => |if_expr| {
            does_fx = try self.checkIfElseExpr(expr_idx, expr_region, rank, if_expr) or does_fx;
        },
        .e_match => |match| {
            does_fx = try self.checkMatchExpr(expr_idx, rank, match) or does_fx;
        },
        .e_binop => |binop| {
            does_fx = try self.checkBinopExpr(expr_idx, expr_region, rank, binop, expected) or does_fx;
        },
        .e_unary_minus => |unary| {
            does_fx = try self.checkUnaryMinusExpr(expr_idx, expr_region, rank, unary) or does_fx;
        },
        .e_unary_not => |unary| {
            does_fx = try self.checkUnaryNotExpr(expr_idx, expr_region, rank, unary) or does_fx;
        },
        .e_dot_access => |dot_access| {
            // Check the receiver expression
            does_fx = try self.checkExpr(dot_access.receiver, rank, .no_expectation) or does_fx;

            const receiver_var = ModuleEnv.varFrom(dot_access.receiver);

            if (dot_access.args) |args_span| {
                // If this dot access has args, then it's static dispatch

                const resolved_receiver = self.types.resolveVar(receiver_var);
                switch (resolved_receiver.desc.content) {
                    .err => {
                        // If the receiver type is an error, then propgate it and  break
                        try self.updateVar(expr_var, .err, rank);
                    },
                    .structure => |structure| switch (structure) {
                        .nominal_type => |nominal| {
                            // This is a static dispatch on a nominal type

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
                                        const source_var = ModuleEnv.varFrom(target_node_idx);
                                        const new_copy = try self.copyVar(source_var, @constCast(module));
                                        try self.import_cache.put(self.gpa, cache_key, new_copy);
                                        break :blk new_copy;
                                    };
                                    const method_instantiated = try self.instantiateVar(method_var, rank, .use_last_var);

                                    // Check all arguments
                                    const arg_expr_idxs = self.cir.store.sliceExpr(args_span);
                                    for (arg_expr_idxs) |arg_expr_idx| {
                                        does_fx = try self.checkExpr(arg_expr_idx, rank, .no_expectation) or does_fx;
                                    }
                                    const arg_expr_vars: []Var = @ptrCast(arg_expr_idxs);

                                    // Create a function type for the method call
                                    const ret_var = try self.fresh(rank, expr_region);
                                    const func_content = try self.types.mkFuncUnbound(arg_expr_vars, ret_var);
                                    const expected_func_var = try self.freshFromContent(func_content, rank, expr_region);

                                    // Unify with the imported method type
                                    const result = try self.unify(method_instantiated, expected_func_var, rank);

                                    // If the inferred function type matches the referenced type
                                    // then set the expr type to be the return type
                                    if (result.isOk()) {
                                        try self.types.setVarRedirect(expr_var, ret_var);
                                    } else {
                                        try self.updateVar(expr_var, .err, rank);
                                    }
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
                        },
                        else => {
                            // Receiver is not a nominal var, this is a type error
                            // TODO: Add a proper error when receiver is not a nominal var
                        },
                    },
                    else => {
                        // Receiver is not a nominal var, this is a type error
                        // TODO: Add a proper error when receiver is not a nominal var

                    },
                }
            } else {
                // Otherwise, this is dot access on a record

                // Create a type for the inferred type of this record access
                // E.g. foo.bar -> { bar: flex } a
                const record_field_var = try self.fresh(rank, expr_region);
                const record_field_range = try self.types.appendRecordFields(&.{types_mod.RecordField{
                    .name = dot_access.field_name,
                    .var_ = record_field_var,
                }});
                const record_being_accessed = try self.freshFromContent(.{ .structure = .{
                    .record_unbound = record_field_range,
                } }, rank, expr_region);

                // Then, unify the actual receiver type with the expected record
                _ = try self.unify(record_being_accessed, receiver_var, rank);
                try self.types.setVarRedirect(expr_var, record_field_var);
            }
        },
        .e_crash => {
            try self.updateVar(expr_var, .{ .flex = Flex.init() }, rank);
        },
        .e_dbg => |dbg| {
            does_fx = try self.checkExpr(dbg.expr, rank, expected) or does_fx;
            _ = try self.types.setVarRedirect(expr_var, ModuleEnv.varFrom(dbg.expr));
        },
        .e_expect => |expect| {
            does_fx = try self.checkExpr(expect.body, rank, expected) or does_fx;
            try self.updateVar(expr_var, .{ .structure = .empty_record }, rank);
        },
        .e_ellipsis => {
            try self.updateVar(expr_var, .{ .flex = Flex.init() }, rank);
        },
        .e_runtime_error => {
            try self.updateVar(expr_var, .err, rank);
        },
    }

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyFromAnno(expected_type.var_, expr_var, rank);
            } else {
                _ = try self.unify(expected_type.var_, expr_var, rank);
            }
        },
    }

    return does_fx;
}

// if-else //

/// Check the types for an if-else expr
fn checkIfElseExpr(
    self: *Self,
    if_expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    rank: types_mod.Rank,
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
    var does_fx = try self.checkExpr(first_branch.cond, rank, .no_expectation);
    const first_cond_var: Var = ModuleEnv.varFrom(first_branch.cond);
    const bool_var = try self.freshBool(rank, expr_region);
    const first_cond_result = try self.unify(bool_var, first_cond_var, rank);
    self.setDetailIfTypeMismatch(first_cond_result, .incompatible_if_cond);

    // Then we check the 1st branch's body
    does_fx = try self.checkExpr(first_branch.body, rank, .no_expectation) or does_fx;

    // The 1st branch's body is the type all other branches must match
    const branch_var = @as(Var, ModuleEnv.varFrom(first_branch.body));

    // Total number of branches (including final else)
    const num_branches: u32 = @intCast(branches.len + 1);

    var last_if_branch = first_branch_idx;
    for (branches[1..], 1..) |branch_idx, cur_index| {
        // TODO: Each branch body get it's own rank??

        const branch = self.cir.store.getIfBranch(branch_idx);

        // Check the branches condition
        does_fx = try self.checkExpr(branch.cond, rank, .no_expectation) or does_fx;
        const cond_var: Var = ModuleEnv.varFrom(branch.cond);
        const branch_bool_var = try self.freshBool(rank, expr_region);
        const cond_result = try self.unify(branch_bool_var, cond_var, rank);
        self.setDetailIfTypeMismatch(cond_result, .incompatible_if_cond);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body, rank, .no_expectation) or does_fx;
        const body_var: Var = ModuleEnv.varFrom(branch.body);
        const body_result = try self.unify(branch_var, body_var, rank);
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

                does_fx = try self.checkExpr(remaining_branch.cond, rank, .no_expectation) or does_fx;
                const remaining_cond_var: Var = ModuleEnv.varFrom(remaining_branch.cond);

                const fresh_bool = try self.freshBool(rank, expr_region);
                const remaining_cond_result = try self.unify(fresh_bool, remaining_cond_var, rank);
                self.setDetailIfTypeMismatch(remaining_cond_result, .incompatible_if_cond);

                does_fx = try self.checkExpr(remaining_branch.body, rank, .no_expectation) or does_fx;
                try self.types.setVarContent(ModuleEnv.varFrom(remaining_branch.body), .err);
            }

            // Break to avoid cascading errors
            break;
        }

        last_if_branch = branch_idx;
    }

    // Check the final else
    does_fx = try self.checkExpr(if_.final_else, rank, .no_expectation) or does_fx;
    const final_else_var: Var = ModuleEnv.varFrom(if_.final_else);
    const final_else_result = try self.unify(branch_var, final_else_var, rank);
    self.setDetailIfTypeMismatch(final_else_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
        .parent_if_expr = if_expr_idx,
        .last_if_branch = last_if_branch,
        .num_branches = num_branches,
        .problem_branch_index = num_branches - 1,
    } });

    // Set the entire expr's type to be the type of the branch
    const if_expr_var: Var = ModuleEnv.varFrom(if_expr_idx);
    try self.types.setVarRedirect(if_expr_var, branch_var);

    return does_fx;
}

// match //

/// Check the types for an if-else expr
fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, rank: Rank, match: CIR.Expr.Match) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond, rank, .no_expectation);
    const cond_var = ModuleEnv.varFrom(match.cond);

    // Assert we have at least 1 branch
    std.debug.assert(match.branches.span.len > 0);

    // Get slice of branches
    const branch_idxs = self.cir.store.sliceMatchBranches(match.branches);

    // Manually check the 1st branch
    // The type of the branch's body becomes the var other branch bodies must unify
    // against.
    const first_branch_idx = branch_idxs[0];
    const first_branch = self.cir.store.getMatchBranch(first_branch_idx);

    const first_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(first_branch.patterns);

    for (first_branch_ptrn_idxs) |branch_ptrn_idx| {
        const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
        try self.checkPattern(branch_ptrn.pattern, rank, .no_expectation);
        const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);

        const ptrn_result = try self.unify(cond_var, branch_ptrn_var, rank);
        self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_cond_pattern = .{
            .match_expr = expr_idx,
        } });
    }

    // Check the first branch's value, then use that at the branch_var
    does_fx = try self.checkExpr(first_branch.value, rank, .no_expectation) or does_fx;
    const branch_var = ModuleEnv.varFrom(first_branch.value);

    // Redirect the match expr to the first branch.
    try self.types.setVarRedirect(ModuleEnv.varFrom(expr_idx), branch_var);

    // Then iterate over the rest of the branches
    for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
        const branch = self.cir.store.getMatchBranch(branch_idx);

        // First, check the patterns of this branch
        const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
        for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
            // Check the pattern's sub types
            const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
            try self.checkPattern(branch_ptrn.pattern, rank, .no_expectation);

            // Check the pattern against the cond
            const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);
            const ptrn_result = try self.unify(cond_var, branch_ptrn_var, rank);
            self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                .match_expr = expr_idx,
                .num_branches = @intCast(match.branches.span.len),
                .problem_branch_index = @intCast(branch_cur_index),
                .num_patterns = @intCast(branch_ptrn_idxs.len),
                .problem_pattern_index = @intCast(cur_ptrn_index),
            } });
        }

        // Then, check the body
        does_fx = try self.checkExpr(branch.value, rank, .no_expectation) or does_fx;
        const branch_result = try self.unify(branch_var, ModuleEnv.varFrom(branch.value), rank);
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
                    try self.checkPattern(other_branch_ptrn.pattern, rank, .no_expectation);

                    // Check the pattern against the cond
                    const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
                    const ptrn_result = try self.unify(cond_var, other_branch_ptrn_var, rank);
                    self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                        .match_expr = expr_idx,
                        .num_branches = @intCast(match.branches.span.len),
                        .problem_branch_index = @intCast(other_branch_cur_index),
                        .num_patterns = @intCast(other_branch_ptrn_idxs.len),
                        .problem_pattern_index = @intCast(other_cur_ptrn_index),
                    } });
                }

                // Then check the other branch's exprs
                does_fx = try self.checkExpr(other_branch.value, rank, .no_expectation) or does_fx;
                try self.types.setVarContent(ModuleEnv.varFrom(other_branch.value), .err);
            }

            // Then stop type checking for this branch
            break;
        }
    }

    return does_fx;
}

// unary minus //

fn checkUnaryMinusExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, rank: Rank, unary: CIR.Expr.UnaryMinus) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, rank, .no_expectation);

    // For unary minus, we constrain the operand and result to be numbers
    const operand_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const result_var = @as(Var, ModuleEnv.varFrom(expr_idx));

    // Create a fresh number variable for the operation
    const num_content = Content{ .structure = .{ .num = .{
        .num_unbound = .{
            .int_requirements = Num.IntRequirements.init(),
            .frac_requirements = Num.FracRequirements.init(),
        },
    } } };
    const num_var = try self.freshFromContent(num_content, rank, expr_region);

    // Redirect the result to the number type
    try self.types.setVarRedirect(result_var, num_var);

    // Unify result with the number type
    _ = try self.unify(num_var, operand_var, rank);

    return does_fx;
}

// unary not //

fn checkUnaryNotExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, rank: Rank, unary: CIR.Expr.UnaryNot) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, rank, .no_expectation);

    // For unary not, we constrain the operand and result to be booleans
    const operand_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const result_var = @as(Var, ModuleEnv.varFrom(expr_idx));

    // Create a fresh boolean variable for the operation
    const bool_var = try self.freshBool(rank, expr_region);

    // Redirect the result to the boolean type
    try self.types.setVarRedirect(result_var, bool_var);

    // Unify result with the boolean type
    _ = try self.unify(bool_var, operand_var, rank);

    return does_fx;
}

// binop //

/// Check the types for a binary operation expression
fn checkBinopExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    rank: Rank,
    binop: CIR.Expr.Binop,
    expected: Expected,
) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_var = ModuleEnv.varFrom(expr_idx);
    const lhs_var = @as(Var, ModuleEnv.varFrom(binop.lhs));
    const rhs_var = @as(Var, ModuleEnv.varFrom(binop.rhs));

    // Check operands first
    var does_fx = false;
    does_fx = try self.checkExpr(binop.lhs, rank, .no_expectation) or does_fx;
    does_fx = try self.checkExpr(binop.rhs, rank, .no_expectation) or does_fx;

    switch (binop.op) {
        .add, .sub, .mul, .div, .rem, .pow, .div_trunc => {
            // For now, we'll constrain both operands to be numbers
            // In the future, this will use static dispatch based on the lhs type

            // We check the lhs and the rhs independently, then unify them with
            // each other. This ensures that all errors are surfaced and the
            // operands are the same type
            switch (expected) {
                .expected => |expectation| {
                    const lhs_instantiated = try self.instantiateVar(expectation.var_, rank, .{ .explicit = expr_region });
                    const rhs_instantiated = try self.instantiateVar(expectation.var_, rank, .{ .explicit = expr_region });

                    if (expectation.from_annotation) {
                        _ = try self.unifyFromAnno(lhs_instantiated, lhs_var, rank);
                        _ = try self.unifyFromAnno(rhs_instantiated, rhs_var, rank);
                    } else {
                        _ = try self.unify(lhs_instantiated, lhs_var, rank);
                        _ = try self.unify(rhs_instantiated, rhs_var, rank);
                    }
                },
                .no_expectation => {
                    // Start with empty requirements that can be constrained by operands
                    const num_content = Content{ .structure = .{ .num = .{
                        .num_unbound = .{
                            .int_requirements = Num.IntRequirements.init(),
                            .frac_requirements = Num.FracRequirements.init(),
                        },
                    } } };
                    const lhs_num_var = try self.freshFromContent(num_content, rank, expr_region);
                    const rhs_num_var = try self.freshFromContent(num_content, rank, expr_region);

                    // Unify left and right operands with num
                    _ = try self.unify(lhs_num_var, lhs_var, rank);
                    _ = try self.unify(rhs_num_var, rhs_var, rank);
                },
            }

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, rank);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propgate error
            try self.types.setVarRedirect(expr_var, lhs_var);
        },
        .lt, .gt, .le, .ge, .eq, .ne => {
            // Ensure the operands are the same type
            const result = try self.unify(lhs_var, rhs_var, rank);

            if (result.isOk()) {
                const fresh_bool = try self.freshBool(rank, expr_region);
                try self.types.setVarRedirect(expr_var, fresh_bool);
            } else {
                try self.updateVar(expr_var, .err, rank);
            }
        },
        .@"and" => {
            const lhs_fresh_bool = try self.freshBool(rank, expr_region);
            const lhs_result = try self.unify(lhs_fresh_bool, lhs_var, rank);
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            const rhs_fresh_bool = try self.freshBool(rank, expr_region);
            const rhs_result = try self.unify(rhs_fresh_bool, rhs_var, rank);
            self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .rhs,
                .binop = .@"and",
            } });

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, rank);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propgate error
            try self.types.setVarRedirect(expr_var, lhs_var);
        },
        .@"or" => {
            const lhs_fresh_bool = try self.freshBool(rank, expr_region);
            const lhs_result = try self.unify(lhs_fresh_bool, lhs_var, rank);
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            const rhs_fresh_bool = try self.freshBool(rank, expr_region);
            const rhs_result = try self.unify(rhs_fresh_bool, rhs_var, rank);
            self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .rhs,
                .binop = .@"and",
            } });

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, rank);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propagate error
            try self.types.setVarRedirect(expr_var, lhs_var);
        },
        .pipe_forward => {
            // TODO
        },
        .null_coalesce => {
            // TODO
        },
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

// instantiate - OLD //

// const RigidVarBehavior = union(enum) {
//     use_cached_rigid_vars,
//     rollback_rigid_vars,
// };

// /// Instantiate a variable
// fn instantiateVar(
//     self: *Self,
//     var_to_instantiate: Var,
//     rigid_to_flex_subs: *Instantiate.RigidSubstitutions,
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

// copy type from other module //

// external type lookups //

const ExternalType = struct {
    local_var: Var,
    other_cir_node_idx: CIR.Node.Idx,
    other_cir: *const ModuleEnv,
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

/// Instantiate a variable, writing su
fn copyVar(
    self: *Self,
    other_module_var: Var,
    other_module_env: *const ModuleEnv,
) std.mem.Allocator.Error!Var {
    // First, reset state
    self.var_map.clearRetainingCapacity();

    // Then, copy the var from the dest type store into this type store
    const copied_var = try copy_import.copyVar(
        &other_module_env.*.types,
        self.types,
        other_module_var,
        &self.var_map,
        other_module_env.getIdentStoreConst(),
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
