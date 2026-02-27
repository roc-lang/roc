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
const snapshot_mod = @import("snapshot.zig");
const exhaustive = @import("exhaustive.zig");

const MkSafeList = collections.SafeList;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const DeferredConstraintCheck = unifier.DeferredConstraintCheck;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const Func = types_mod.Func;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Rank = types_mod.Rank;
const Instantiator = types_mod.instantiate.Instantiator;
const Generalizer = types_mod.generalize.Generalizer;
const VarPool = types_mod.generalize.VarPool;
const SnapshotStore = snapshot_mod.Store;
const ProblemStore = @import("problem.zig").Store;

/// Deferred numeric literal for compile-time validation
/// These are collected during type checking and validated during comptime evaluation
pub const DeferredNumericLiteral = struct {
    /// The e_num expression index
    expr_idx: CIR.Expr.Idx,
    /// The type variable that the literal unified with
    type_var: Var,
    /// The from_numeral constraint attached to this literal
    constraint: StaticDispatchConstraint,
    /// Source region for error reporting
    region: Region,

    pub const SafeList = collections.SafeList(@This());
};

const Self = @This();

gpa: std.mem.Allocator,
// This module's types store
types: *types_mod.Store,
/// This module's env
cir: *ModuleEnv,
/// A list of regions. Owned copy, cloned from NodeStore at init time.
regions: Region.List,
/// List of directly imported  module. Import indexes in CIR refer to this list
imported_modules: []const *const ModuleEnv,
/// Map of auto-imported type names (like "Str", "List", "Bool") to their defining modules.
/// This is used to resolve type names that are automatically available without explicit imports.
auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
/// Builtin type context for the module being type-checked
builtin_ctx: BuiltinContext,
/// type snapshots used in error messages
snapshots: SnapshotStore,
/// type problems
problems: ProblemStore,
/// import mapping for auto-imported builtin types (for error display)
import_mapping: @import("types").import_mapping.ImportMapping,
/// reusable scratch arrays used in unification
unify_scratch: unifier.Scratch,
/// reusable scratch arrays used in occurs check
occurs_scratch: occurs.Scratch,
/// annos we've already seen when generation a type from an annotation
seen_annos: std.AutoHashMap(CIR.TypeAnno.Idx, Var),
/// A pool of solver envs
env_pool: EnvPool,
/// wrapper around generalization, contains some internal state used to do it's work
generalizer: Generalizer,
/// A list of generated constraints with solving deferred
constraints: Constraint.SafeList,
/// A map from one var to another. Used in instantiation and var copying
var_map: std.AutoHashMap(Var, Var),
/// A map from one var to another. Used in instantiation and var copying
var_set: std.AutoHashMap(Var, void),
/// A map from one var to another. Used to apply type arguments in instantiation
rigid_var_substitutions: std.AutoHashMapUnmanaged(Ident.Idx, Var),
/// scratch vars used to build up intermediate lists, used for various things
scratch_vars: base.Scratch(Var),
/// scratch tags used to build up intermediate lists, used for various things
scratch_tags: base.Scratch(types_mod.Tag),
/// scratch record fields used to build up intermediate lists, used for various things
scratch_record_fields: base.Scratch(types_mod.RecordField),
/// scratch static dispatch constraints used to build up intermediate lists, used for various things
scratch_static_dispatch_constraints: base.Scratch(ScratchStaticDispatchConstraint),
/// scratch deferred static dispatch constraints
scratch_deferred_static_dispatch_constraints: base.Scratch(DeferredConstraintCheck),
/// Stack of type variables currently being constraint-checked, used to detect recursive constraints
/// When a var appears in this stack while we're checking its constraints, we've detected recursion
constraint_check_stack: std.ArrayList(Var),
// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,
/// Copied Bool type from Bool module (for use in if conditions, etc.)
bool_var: Var,
/// Copied Str type from Builtin module (for use in string literals, etc.)
str_var: Var,
/// Map representation of Ident -> Var, used in checking static dispatch constraints
ident_to_var_map: std.AutoHashMap(Ident.Idx, Var),
/// Map representation all top level patterns, and if we've processed them yet
top_level_ptrns: std.AutoHashMap(CIR.Pattern.Idx, DefProcessed),
/// The name of the enclosing function, if known.
/// Used to provide better error messages when type checking lambda arguments.
enclosing_func_name: ?Ident.Idx,
/// Type writer for formatting types at snapshot time
type_writer: types_mod.TypeWriter,
/// --- Lazy cycle detection state ---
///
/// Only one cycle can be active at a time because defs are processed
/// sequentially. `defer_generalize` is global: once set, it affects all
/// defs processed while active, including non-cycle functions that happen
/// to be checked during the cycle (e.g. a diamond branch that calls into
/// the cycle). This is safe because those extra vars are still correctly
/// typed; they just get generalized at the cycle root instead of
/// independently.
/// The def currently being type-checked (innermost in the call stack)
current_processing_def: ?CIR.Def.Idx = null,
/// When a dispatch cycle is detected, the .processing def that is the
/// outermost participant (the "root" that will handle generalization)
cycle_root_def: ?CIR.Def.Idx = null,
/// True when generalization should be deferred (a dispatch cycle was detected)
defer_generalize: bool = false,
/// Deferred def-level unifications (def_var = ptrn_var = expr_var).
/// These must happen AFTER generalization to avoid lowering expr_var's rank
/// before generalization can process it, but BEFORE eql constraint resolution
/// so that def/ptrn vars point to generalized expr vars when cross-function
/// constraints resolve. This ordering requirement is why these can't be
/// stored in the `constraints` list (which runs after both steps).
deferred_def_unifications: std.ArrayListUnmanaged(DeferredDefUnification),
/// Envs from cycle participants whose vars need to be merged at the cycle root.
/// Stored here instead of merging eagerly so that ranks remain correct
/// (no `popRankRetainingVars` needed).
deferred_cycle_envs: std.ArrayListUnmanaged(Env),

/// A def + processing data
const DefProcessed = struct {
    def_idx: CIR.Def.Idx,
    def_name: ?Ident.Idx,
    status: HasProcessed,
};

/// Indicates if something has been processed or not
const HasProcessed = enum { processed, processing, not_processed };

/// A deferred def-level unification (def_var = ptrn_var = expr_var).
const DeferredDefUnification = struct {
    def_var: Var,
    ptrn_var: Var,
    expr_var: Var,
};

/// A struct scratch info about a static dispatch constraint
const ScratchStaticDispatchConstraint = struct {
    var_: Var,
    constraint: types_mod.StaticDispatchConstraint,
};

/// A constraint generated during type checking, to be checked at the end
///
/// In most cases, we don't defer constraint checking and unify things
/// immediately. However, there are some cases where it's necessary.
const Constraint = union(enum) {
    eql: struct { expected: Var, actual: Var, ctx: problem.Context },

    pub const SafeList = MkSafeList(@This());
};

/// Context for type checking: module identity, builtin type references, and the Builtin module itself.
/// This is passed to Check.init() to provide access to auto-imported types from Builtin.
pub const BuiltinContext = struct {
    /// The name of the module being type-checked
    module_name: base.Ident.Idx,
    /// Statement index of Bool type in the current module (injected from Builtin.bin)
    bool_stmt: can.CIR.Statement.Idx,
    /// Statement index of Try type in the current module (injected from Builtin.bin)
    try_stmt: can.CIR.Statement.Idx,
    /// Statement index of Str type in the current module (injected from Builtin.bin)
    str_stmt: can.CIR.Statement.Idx,
    /// Direct reference to the Builtin module env (null when compiling Builtin module itself)
    builtin_module: ?*const ModuleEnv,
    /// Indices of auto-imported types in the Builtin module (null when compiling Builtin module itself)
    builtin_indices: ?can.CIR.BuiltinIndices,
};

/// Init type solver
/// Does *not* own types_store or cir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *const ModuleEnv,
    imported_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    regions: *const Region.List,
    builtin_ctx: BuiltinContext,
) std.mem.Allocator.Error!Self {
    const mutable_cir = @constCast(cir);
    var import_mapping = try createImportMapping(
        gpa,
        mutable_cir.getIdentStore(),
        cir,
        builtin_ctx.builtin_module,
        builtin_ctx.builtin_indices,
        auto_imported_types,
    );
    errdefer import_mapping.deinit();

    return .{
        .gpa = gpa,
        .types = types,
        .cir = mutable_cir,
        .imported_modules = imported_modules,
        .auto_imported_types = auto_imported_types,
        .regions = blk: {
            var owned = Region.List{};
            _ = try owned.appendSlice(gpa, regions.items.items);
            break :blk owned;
        },
        .builtin_ctx = builtin_ctx,
        .snapshots = try SnapshotStore.initCapacity(gpa, 512),
        .problems = try ProblemStore.initCapacity(gpa, 64),
        .import_mapping = import_mapping,
        .unify_scratch = try unifier.Scratch.init(gpa),
        .occurs_scratch = try occurs.Scratch.init(gpa),
        .seen_annos = std.AutoHashMap(CIR.TypeAnno.Idx, Var).init(gpa),
        .env_pool = try EnvPool.init(gpa),
        .generalizer = try Generalizer.init(gpa, types),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .constraints = try Constraint.SafeList.initCapacity(gpa, 32),
        .var_set = std.AutoHashMap(Var, void).init(gpa),
        .rigid_var_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){},
        .scratch_vars = try base.Scratch(types_mod.Var).init(gpa),
        .scratch_tags = try base.Scratch(types_mod.Tag).init(gpa),
        .scratch_record_fields = try base.Scratch(types_mod.RecordField).init(gpa),
        .scratch_static_dispatch_constraints = try base.Scratch(ScratchStaticDispatchConstraint).init(gpa),
        .scratch_deferred_static_dispatch_constraints = try base.Scratch(DeferredConstraintCheck).init(gpa),
        .constraint_check_stack = try std.ArrayList(Var).initCapacity(gpa, 0),
        .import_cache = ImportCache{},
        .bool_var = undefined, // Will be initialized in copyBuiltinTypes()
        .str_var = undefined, // Will be initialized in copyBuiltinTypes()
        .ident_to_var_map = std.AutoHashMap(Ident.Idx, Var).init(gpa),
        .top_level_ptrns = std.AutoHashMap(CIR.Pattern.Idx, DefProcessed).init(gpa),
        .enclosing_func_name = null,
        // Initialize with null import_mapping - caller should call fixupTypeWriter() after storing Check
        .type_writer = try types_mod.TypeWriter.initFromParts(gpa, types, mutable_cir.getIdentStore(), null),
        .deferred_def_unifications = .{},
        .deferred_cycle_envs = .{},
    };
}

/// Call this after Check has been stored at its final location to set up the import_mapping pointer.
/// This is needed because returning Check by value invalidates the pointer set during init.
pub fn fixupTypeWriter(self: *Self) void {
    self.type_writer.setImportMapping(&self.import_mapping);
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.regions.deinit(self.gpa);
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.import_mapping.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.seen_annos.deinit();
    // Release any stored cycle envs before deiniting the pool
    for (self.deferred_cycle_envs.items) |deferred_env| {
        self.env_pool.release(deferred_env);
    }
    self.deferred_cycle_envs.deinit(self.gpa);
    self.env_pool.deinit();
    self.generalizer.deinit(self.gpa);
    self.var_map.deinit();
    self.constraints.deinit(self.gpa);
    self.var_set.deinit();
    self.rigid_var_substitutions.deinit(self.gpa);
    self.scratch_vars.deinit();
    self.scratch_tags.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_static_dispatch_constraints.deinit();
    self.scratch_deferred_static_dispatch_constraints.deinit();
    self.constraint_check_stack.deinit(self.gpa);
    self.import_cache.deinit(self.gpa);
    self.ident_to_var_map.deinit();
    self.top_level_ptrns.deinit();
    self.type_writer.deinit();
    self.deferred_def_unifications.deinit(self.gpa);
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
    if (type_nodes >= region_nodes) return;
    try self.types.ensureTotalCapacity(region_nodes);
    for (type_nodes..region_nodes) |_| {
        _ = self.types.appendFromContentAssumeCapacity(.{ .flex = Flex.init() }, undefined);
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

// env //

/// Solver env
const Env = struct {
    /// Pool of variables created during solving, use by let-polymorphism
    var_pool: VarPool,
    /// Deferred static dispatch constraints - accumulated during type checking,
    /// then solved for at the end
    deferred_static_dispatch_constraints: DeferredConstraintCheck.SafeList,

    fn init(
        gpa: std.mem.Allocator,
        at: Rank,
    ) std.mem.Allocator.Error!Env {
        var pool = try VarPool.init(gpa);
        pool.current_rank = at;
        try pool.ensureRanksThrough(at);

        return .{
            .var_pool = pool,
            .deferred_static_dispatch_constraints = try DeferredConstraintCheck.SafeList.initCapacity(gpa, 32),
        };
    }

    fn deinit(self: *Env, gpa: std.mem.Allocator) void {
        self.var_pool.deinit();
        self.deferred_static_dispatch_constraints.deinit(gpa);
    }

    /// Resets internal state of env and set rank to generalized
    fn reset(self: *Env, to: Rank) !void {
        self.var_pool.current_rank = to;
        self.var_pool.clearRetainingCapacity();
        try self.var_pool.ensureRanksThrough(to);
        self.deferred_static_dispatch_constraints.items.clearRetainingCapacity();
    }

    fn rank(self: *const Env) Rank {
        return self.var_pool.current_rank;
    }
};

// unify //

/// Unify two types where `a` is the expected type and `b` is the actual type
fn unify(self: *Self, a: Var, b: Var, env: *Env) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    return self.unifyInContext(a, b, env, .none);
}

/// Unify two types where `a` is the expected type and `b` is the actual type
/// Accepts a full unifier config for fine-grained control
fn unifyInContext(self: *Self, a: Var, b: Var, env: *Env, ctx: problem.Context) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Unify
    const result = try unifier.unifyInContext(
        self.cir,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.type_writer,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        ctx,
    );

    // Set regions and add to the current rank all variables created during unification.
    //
    // We assign all fresh variables the region of `b` (the "actual" type), since `a` is
    // typically the "expected" type from an annotation. This heuristic works well for
    // most cases but can be imprecise for deeply nested unifications where fresh variables
    // are created for sub-components (e.g., record fields, tag payloads). In those cases,
    // error messages may point to the outer expression rather than the specific field.
    //
    // A more precise solution would track the origin of each fresh variable during
    // unification and propagate that back, but the current approach is sufficient for
    // typical error reporting scenarios.
    const region = self.getRegionAt(b);
    for (self.unify_scratch.fresh_vars.items.items) |fresh_var| {
        // Set the rank
        const fresh_rank = self.types.resolveVar(fresh_var).desc.rank;
        try env.var_pool.addVarToRank(fresh_var, fresh_rank);

        // Set the region
        try self.fillInRegionsThrough(fresh_var);
        self.setRegionAt(fresh_var, region);
    }

    // Copy any constraints created during unification into our own array
    for (self.unify_scratch.deferred_constraints.items.items) |deferred_constraint| {
        _ = try env.deferred_static_dispatch_constraints.append(self.gpa, deferred_constraint);
    }

    // Ensure arrays are in sync
    self.debugAssertArraysInSync();

    return result;
}

/// Check if a variable contains an infinite type after solving a definition.
/// This catches cases like `f = |x| f([x])` which creates `a = List(a)`.
/// Similar to Rust's check_for_infinite_type called after LetCon.
fn checkForInfiniteType(self: *Self, comptime Idx: anytype, idx: Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(Idx == CIR.Def.Idx or Idx == CIR.Expr.Idx);

    const var_ = ModuleEnv.varFrom(idx);
    const occurs_result = try occurs.occurs(self.types, &self.occurs_scratch, var_);

    switch (occurs_result) {
        .not_recursive, .recursive_nominal => {
            // These are fine - no cycle, or valid recursion through a nominal type
        },
        .recursive_anonymous => {
            const err_var = if (self.occurs_scratch.err_chain.len() > 0)
                self.occurs_scratch.err_chain.items.items[0]
            else
                var_;

            // Anonymous recursion (recursive type not through a nominal type)
            const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, err_var);
            _ = try self.problems.appendProblem(self.gpa, .{ .anonymous_recursion = .{
                .var_ = var_,
                .snapshot = snapshot,
                .def_name = if (comptime Idx == CIR.Def.Idx) blk: {
                    const def = self.cir.store.getDef(idx);
                    break :blk self.getPatternIdent(def.pattern);
                } else blk: {
                    break :blk null;
                },
            } });
            try self.types.setVarContent(var_, .err);
        },
        .infinite => {
            const err_var = if (self.occurs_scratch.err_chain.len() > 0)
                self.occurs_scratch.err_chain.items.items[0]
            else
                var_;

            // Infinite type (like `a = List(a)`)
            const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, err_var);
            _ = try self.problems.appendProblem(self.gpa, .{ .infinite_recursion = .{
                .var_ = var_,
                .snapshot = snapshot,
                .def_name = if (comptime Idx == CIR.Def.Idx) blk: {
                    const def = self.cir.store.getDef(idx);
                    break :blk self.getPatternIdent(def.pattern);
                } else blk: {
                    break :blk null;
                },
            } });
            try self.types.setVarContent(var_, .err);
        },
    }
}
// instantiate  //

const InstantiateRegionBehavior = union(enum) {
    explicit: Region,
    use_root_instantiated,
    use_last_var,
};

/// Instantiate a variable
///
/// * Substituting generalized flex vars with fresh flex vars
/// * Substituting generalized rigid vars with fresh flex vars
///
/// Note that the the rigid var structure will be preserved.
/// E.g. In `a -> a`, all `a` will reference the same new flex var
fn instantiateVar(
    self: *Self,
    var_to_instantiate: Var,
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = env.rank(),
        .rigid_behavior = .fresh_flex,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}

/// You probably are looking for `instantiateVar`.
///
/// Copy/paste a variable. This does NOT respect rank or normal instantiation rules.
fn instantiateVarOrphan(
    self: *Self,
    var_to_instantiate: Var,
    env: *Env,
    rank: Rank,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();
    std.debug.assert(@intFromEnum(rank) <= @intFromEnum(env.rank()));
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,
        .current_rank = env.rank(),
        .rigid_behavior = .fresh_rigid,
        .rank_behavior = .ignore_rank,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}

/// Instantiate a variable, substituting any encountered rigids with
/// user-provided variables.
///
/// Based on the provided map, the caller can specifically set specified rigids
/// to be a specific var. This is used when evaluating type annotation.
///
/// If a rigid is is encountered that's not in the provided map, a debug assertion
/// will fail. In production mode, that rigid var will be set as an `.err`
fn instantiateVarWithSubs(
    self: *Self,
    var_to_instantiate: Var,
    subs: *std.AutoHashMapUnmanaged(Ident.Idx, Var),
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = env.rank(),
        .rigid_behavior = .{ .substitute_rigids = subs },
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}

/// Instantiate a variable
fn instantiateVarHelp(
    self: *Self,
    var_to_instantiate: Var,
    instantiator: *Instantiator,
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

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

            const fresh_resolved = self.types.resolveVar(fresh_var);

            // Track newly instantiated from_numeral flex vars so
            // finalizeNumericDefaults knows about them.
            if (fresh_resolved.desc.content == .flex) {
                const flex = fresh_resolved.desc.content.flex;
                if (flex.constraints.len() > 0) {
                    const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
                    for (constraints) |c| {
                        if (c.origin == .from_numeral) {
                            self.types.from_numeral_flex_count += 1;
                            break;
                        }
                    }
                }
            }

            // Add to pool
            try env.var_pool.addVarToRank(fresh_var, fresh_resolved.desc.rank);

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

    // Add the var to the right rank
    try env.var_pool.addVarToRank(instantiated_var, instantiator.current_rank);

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
        // Use Check's allocator since regions is owned by Check
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
    return self.regions.get(@enumFromInt(@intFromEnum(target_var))).*;
}

// fresh vars //

/// Create fresh flex var
fn fresh(self: *Self, env: *Env, new_region: Region) Allocator.Error!Var {
    return self.freshFromContent(.{ .flex = Flex.init() }, env, new_region);
}

/// Create fresh var with the provided content
fn freshFromContent(self: *Self, content: Content, env: *Env, new_region: Region) Allocator.Error!Var {
    const var_ = try self.types.freshFromContentWithRank(content, env.rank());
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    try env.var_pool.addVarToRank(var_, env.rank());
    return var_;
}

/// Create a bool var
fn freshBool(self: *Self, env: *Env, new_region: Region) Allocator.Error!Var {
    // Use the copied Bool type from the type store (set by copyBuiltinTypes)
    return try self.instantiateVar(self.bool_var, env, .{ .explicit = new_region });
}

/// Create a str var
fn freshStr(self: *Self, env: *Env, new_region: Region) Allocator.Error!Var {
    // Use the copied Str type from the type store (set by copyBuiltinTypes)
    return try self.instantiateVar(self.str_var, env, .{ .explicit = new_region });
}

/// Create a nominal List type with the given element type
fn mkListContent(self: *Self, elem_var: Var, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Use the cached builtin_module_ident from the current module's ident store.
    // This represents the "Builtin" module where List is defined.
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    const list_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.list,
    };

    // List's backing is [ProvidedByCompiler] with closed extension
    // The element type is a type parameter, not the backing
    const empty_tag_union_content = Content{ .structure = .empty_tag_union };
    const ext_var = try self.freshFromContent(empty_tag_union_content, env, Region.zero());

    // Create the [ProvidedByCompiler] tag
    const provided_tag_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("ProvidedByCompiler"));
    const provided_tag = try self.types.mkTag(provided_tag_ident, &.{});

    const tag_union = types_mod.TagUnion{
        .tags = try self.types.appendTags(&[_]types_mod.Tag{provided_tag}),
        .ext = ext_var,
    };
    const backing_content = Content{ .structure = .{ .tag_union = tag_union } };
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    const type_args = [_]Var{elem_var};

    return try self.types.mkNominal(
        list_ident,
        backing_var,
        &type_args,
        origin_module_id,
        false, // List is nominal (not opaque)
    );
}

/// Create a nominal number type content (e.g., U8, I32, Dec)
/// Number types are defined in Builtin.roc nested inside Num module: Num.U8 :: [].{...}
/// They have no type parameters and their backing is the empty tag union []
fn mkNumberTypeContent(self: *Self, type_name: []const u8, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    // Use fully-qualified type name "Builtin.Num.U8" etc.
    // This allows method lookup to work correctly (getMethodIdent builds "Builtin.Num.U8.method_name")
    const qualified_type_name = try std.fmt.allocPrint(self.gpa, "Builtin.Num.{s}", .{type_name});
    defer self.gpa.free(qualified_type_name);
    const type_name_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text(qualified_type_name));
    const type_ident = types_mod.TypeIdent{
        .ident_idx = type_name_ident,
    };

    // Number types backing is [] (empty tag union with closed extension)
    const empty_tag_union_content = Content{ .structure = .empty_tag_union };
    const ext_var = try self.freshFromContent(empty_tag_union_content, env, Region.zero());
    const empty_tag_union = types_mod.TagUnion{
        .tags = types_mod.Tag.SafeMultiList.Range.empty(),
        .ext = ext_var,
    };
    const backing_content = Content{ .structure = .{ .tag_union = empty_tag_union } };
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    // Number types have no type arguments
    const no_type_args: []const Var = &.{};

    return try self.types.mkNominal(
        type_ident,
        backing_var,
        no_type_args,
        origin_module_id,
        true, // Number types are opaque (defined with ::)
    );
}

/// Create a Dec nominal type content using the stored ident index rather than
/// constructing the qualified name from a string literal.
fn mkDecContent(self: *Self, env: *Env) Allocator.Error!Content {
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name;

    const type_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.dec_type,
    };

    const empty_tag_union_content = Content{ .structure = .empty_tag_union };
    const ext_var = try self.freshFromContent(empty_tag_union_content, env, Region.zero());
    const empty_tag_union = types_mod.TagUnion{
        .tags = types_mod.Tag.SafeMultiList.Range.empty(),
        .ext = ext_var,
    };
    const backing_content = Content{ .structure = .{ .tag_union = empty_tag_union } };
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    const no_type_args: []const Var = &.{};

    return try self.types.mkNominal(
        type_ident,
        backing_var,
        no_type_args,
        origin_module_id,
        true,
    );
}

/// Create a flex variable with a from_numeral constraint for numeric literals.
/// This constraint will be checked during deferred constraint checking to validate
/// that the numeric literal can be converted to the unified type.
/// Returns the flex var which has the constraint attached, and the dispatcher var
/// (first arg of from_numeral) is unified with the flex var so they share the same name.
fn mkFlexWithFromNumeralConstraint(
    self: *Self,
    num_literal_info: types_mod.NumeralInfo,
    env: *Env,
) !Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const from_numeral_ident = self.cir.idents.from_numeral;

    // Create the flex var first - this represents the target type `a`
    const flex_var = try self.fresh(env, num_literal_info.region);

    // Create the argument type: Numeral (from Builtin.Num.Numeral)
    // For from_numeral, the actual method signature is: Numeral -> Try(a, [InvalidNumeral(Str)])
    const numeral_content = try self.mkNumeralContent(env);
    const arg_var = try self.freshFromContent(numeral_content, env, num_literal_info.region);

    // Create the error type: [InvalidNumeral(Str)] (closed tag union)
    const str_var = self.str_var;
    const invalid_numeral_tag_ident = try @constCast(self.cir).insertIdent(
        base.Ident.for_text("InvalidNumeral"),
    );
    const invalid_numeral_tag = try self.types.mkTag(
        invalid_numeral_tag_ident,
        &.{str_var},
    );
    // Use empty_tag_union as extension to create a closed tag union [InvalidNumeral(Str)]
    const err_ext_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, num_literal_info.region);
    const err_type = try self.types.mkTagUnion(&.{invalid_numeral_tag}, err_ext_var);
    const err_var = try self.freshFromContent(err_type, env, num_literal_info.region);

    // Create Try(flex_var, err_var) as the return type
    // Try is a nominal type with two type args: the success type and the error type
    const try_type_content = try self.mkTryContent(flex_var, err_var, env);
    const ret_var = try self.freshFromContent(try_type_content, env, num_literal_info.region);

    const func_content = types_mod.Content{
        .structure = types_mod.FlatType{
            .fn_unbound = types_mod.Func{
                .args = try self.types.appendVars(&.{arg_var}),
                .ret = ret_var,
                .needs_instantiation = false,
            },
        },
    };
    const fn_var = try self.freshFromContent(func_content, env, num_literal_info.region);

    // Create the constraint with numeric literal info
    const constraint = types_mod.StaticDispatchConstraint{
        .fn_name = from_numeral_ident,
        .fn_var = fn_var,
        .origin = .from_numeral,
        .num_literal = num_literal_info,
    };

    // Store it in the types store
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

    // Update the flex var to have the constraint attached
    const flex_content = types_mod.Content{
        .flex = types_mod.Flex{
            .name = null,
            .constraints = constraint_range,
        },
    };
    try self.unifyWith(flex_var, flex_content, env);
    self.types.from_numeral_flex_count += 1;

    return flex_var;
}

/// Create a nominal Box type with the given element type
fn mkBoxContent(self: *Self, elem_var: Var) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Use the cached builtin_module_ident from the current module's ident store.
    // This represents the "Builtin" module where Box is defined.
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    const box_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.box,
    };

    // The backing var is the element type var
    const backing_var = elem_var;
    const type_args = [_]Var{elem_var};

    return try self.types.mkNominal(
        box_ident,
        backing_var,
        &type_args,
        origin_module_id,
        false, // Box is nominal (not opaque)
    );
}

/// Create a nominal Try type with the given success and error types.
/// This is used for creating Try types in function signatures (e.g., from_numeral).
fn mkTryContent(self: *Self, ok_var: Var, err_var: Var, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    const try_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.builtin_try,
    };

    // Create the backing tag union [Ok(ok), Err(err)]
    // Tags are created in source order (matching Try definition in Builtin.roc).
    // The layout generator will sort them alphabetically later.
    const ok_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("Ok"));
    const err_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("Err"));
    const ok_tag = try self.types.mkTag(ok_ident, &.{ok_var});
    const err_tag = try self.types.mkTag(err_ident, &.{err_var});
    const ext_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, Region.zero());
    const backing_content = try self.types.mkTagUnion(&.{ ok_tag, err_tag }, ext_var);
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    const type_args = [_]Var{ ok_var, err_var };

    return try self.types.mkNominal(
        try_ident,
        backing_var,
        &type_args,
        origin_module_id,
        false, // Try is nominal (not opaque)
    );
}

/// Create a nominal Numeral type (from Builtin.Num.Numeral)
/// Numeral has no type parameters - it's a concrete record type wrapped in Self tag
fn mkNumeralContent(self: *Self, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Use the cached builtin_module_ident from the current module's ident store.
    // This represents the "Builtin" module where Numeral is defined.
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    // Use the relative name "Num.Numeral" with origin_module Builtin
    // Use the pre-interned ident from builtin_module to avoid string comparison
    const numeral_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.builtin_numeral,
    };

    // The backing var doesn't matter here. Nominal types unify based on their ident
    // and type args only - the backing is never examined during unification.
    // Creating the real backing type ([Self({is_negative: Bool, ...})]) would be a waste of time.
    const empty_tag_union_content = Content{ .structure = .empty_tag_union };
    const ext_var = try self.freshFromContent(empty_tag_union_content, env, Region.zero());
    const empty_tag_union = types_mod.TagUnion{
        .tags = types_mod.Tag.SafeMultiList.Range.empty(),
        .ext = ext_var,
    };
    const backing_content = Content{ .structure = .{ .tag_union = empty_tag_union } };
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    return try self.types.mkNominal(
        numeral_ident,
        backing_var,
        &.{}, // No type args
        origin_module_id,
        true, // Numeral is opaque (defined with ::)
    );
}

// updating vars //

/// Unify the provided variable with the provided content
///
/// If the var is a flex at the current rank, skip unifcation and simply update
/// the type descriptor
///
/// This should primarily be use to set CIR node vars that were initially filled with placeholders
fn unifyWith(self: *Self, target_var: Var, content: types_mod.Content, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const resolved_target = self.types.resolveVar(target_var);
    if (resolved_target.is_root and resolved_target.desc.rank == env.rank() and resolved_target.desc.content == .flex) {
        // The vast majority of the time, we call unify with on a placeholder
        // CIR var. In this case, we can safely override the type descriptor
        // directly, saving a typeslot and unifcation run
        var desc = resolved_target.desc;
        desc.content = content;
        try self.types.dangerousSetVarDesc(target_var, desc);
    } else {
        const fresh_var = try self.freshFromContent(content, env, self.getRegionAt(target_var));
        if (builtin.mode == .Debug) {
            const target_var_rank = self.types.resolveVar(target_var).desc.rank;
            const fresh_var_rank = self.types.resolveVar(fresh_var).desc.rank;
            if (@intFromEnum(target_var_rank) > @intFromEnum(fresh_var_rank)) {
                std.debug.panic("trying unifyWith unexpected ranks {} & {}", .{ @intFromEnum(target_var_rank), @intFromEnum(fresh_var_rank) });
            }
        }
        _ = try self.unify(target_var, fresh_var, env);
    }
}

/// Give a var, ensure it's not a redirect and set its rank.
/// If the var is already a redirect, this is a no-op - the root's rank was set when
/// the redirect was created during unification. This can happen when a variable is
/// unified with another before its rank is explicitly set, which is benign.
fn setVarRank(self: *Self, target_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(target_var);
    if (resolved.is_root) {
        self.types.setDescRank(resolved.desc_idx, env.rank());
        try env.var_pool.addVarToRank(target_var, env.rank());
    }
    // If not root, the variable is a redirect - its rank is determined by the root
    // variable it points to, which was handled when the redirect was created.
}

// file //

/// Check the types for all defs
/// Copy builtin types from their modules into the current module's type store
/// This is necessary because type variables are module-specific - we can't use Vars from
/// other modules directly. The Bool and Try types are used in language constructs like
/// `if` conditions and need to be available in every module's type store.
fn copyBuiltinTypes(self: *Self) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const bool_stmt_idx = self.builtin_ctx.bool_stmt;
    const str_stmt_idx = self.builtin_ctx.str_stmt;

    if (self.builtin_ctx.builtin_module) |builtin_env| {
        // Copy Bool type from Builtin module using the direct reference
        const bool_type_var = ModuleEnv.varFrom(bool_stmt_idx);
        self.bool_var = try self.copyVar(bool_type_var, builtin_env, Region.zero());

        // Copy Str type from Builtin module using the direct reference
        const str_type_var = ModuleEnv.varFrom(str_stmt_idx);
        self.str_var = try self.copyVar(str_type_var, builtin_env, Region.zero());
    } else {
        // If Builtin module reference is null, use the statement from the current module
        // This happens when compiling the Builtin module itself
        self.bool_var = ModuleEnv.varFrom(bool_stmt_idx);
        self.str_var = ModuleEnv.varFrom(str_stmt_idx);
    }

    // Try type is accessed via external references, no need to copy it here
}

/// Check the types for all defs in a file.
/// Set `skip_numeric_defaults` to true for app modules that have platform requirements -
/// in that case, `finalizeNumericDefaults()` should be called AFTER `checkPlatformRequirements()`
/// so that numeric literals can be constrained by platform types first.
pub fn checkFile(self: *Self) std.mem.Allocator.Error!void {
    return self.checkFileInternal(false);
}

/// Check the types for all defs in a file, optionally skipping numeric defaults finalization.
/// Use this for app modules with platform requirements, then call `finalizeNumericDefaults()`
/// after `checkPlatformRequirements()`.
pub fn checkFileSkipNumericDefaults(self: *Self) std.mem.Allocator.Error!void {
    return self.checkFileInternal(true);
}

fn checkFileInternal(self: *Self, skip_numeric_defaults: bool) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Fill in types store up to the size of CIR nodes
    try ensureTypeStoreIsFilled(self);

    // Create a solver env
    var env = try self.env_pool.acquire();
    defer self.env_pool.release(env);

    std.debug.assert(env.rank() == .generalized);

    // Copy builtin types (Bool, Try) into this module's type store
    // Note that bool_var and try_var will have generalized rank
    try self.copyBuiltinTypes();

    // First, iterate over the builtin statements, generating types for each type declaration
    // Note that any types generated will be generalized
    const builtin_stmts_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (builtin_stmts_slice) |builtin_stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(builtin_stmt_idx, &env);
    }

    const stmts_slice = self.cir.store.sliceStatements(self.cir.all_statements);

    // First pass: generate types for each type declaration
    // Note that any types generated will be generalized
    for (stmts_slice) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);

        try self.setVarRank(stmt_var, &env);

        switch (stmt) {
            .s_alias_decl => |alias| {
                try self.generateAliasDecl(stmt_idx, stmt_var, alias, &env);
            },
            .s_nominal_decl => |nominal| {
                try self.generateNominalDecl(stmt_idx, stmt_var, nominal, &env);
            },
            .s_runtime_error => {
                try self.unifyWith(stmt_var, .err, &env);
            },
            .s_type_anno => |type_anno| {
                try self.generateStandaloneTypeAnno(stmt_var, type_anno, &env);
            },
            else => {
                // All other stmt types are invalid at the top level
            },
        }
    }

    // Next, capture all top level defs
    // This is used to support out-of-order defs
    const defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.cir.store.getDef(def_idx);
        try self.top_level_ptrns.put(def.pattern, DefProcessed{
            .def_idx = def_idx,
            .def_name = null,
            .status = .not_processed,
        });
    }

    // Set the rank to be outermost
    try env.var_pool.pushRank();
    std.debug.assert(env.rank() == .outermost);

    // Process requires_types annotations for platforms
    // This ensures the type store has the actual types for platform requirements
    try self.processRequiresTypes(&env);

    // Then, iterate over defs again, inferring types
    for (defs_slice) |def_idx| {
        try self.checkDef(def_idx, &env);

        // Ensure that after processing a def, checkDef correctly restores the
        // rank to outermost for the next level of processing
        std.debug.assert(env.rank() == .outermost);
    }

    // Finally, type-check top-level statements (like expect)
    // These are separate from defs and need to be checked after all defs are processed
    // so that lookups can find their definitions
    for (stmts_slice) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);
        const stmt_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(stmt_idx));

        switch (stmt) {
            .s_expect => |expr_stmt| {
                // Check the body expression
                _ = try self.checkExpr(expr_stmt.body, &env, .no_expectation);
                const body_var: Var = ModuleEnv.varFrom(expr_stmt.body);

                // Unify with Bool (expects must be bool expressions)
                const bool_var = try self.freshBool(&env, stmt_region);
                _ = try self.unifyInContext(bool_var, body_var, &env, .expect);

                // Unify statement var with body var
                _ = try self.unify(stmt_var, body_var, &env);
            },
            else => {
                // Other statement types are handled elsewhere (type decls, defs, etc.)
            },
        }
    }

    // Check any accumulated constraints
    try self.checkAllConstraints(&env);
    try self.resolveNumericLiteralsFromContext(&env);

    // Finalize numeric defaults unless skipped (for app modules with platform requirements,
    // this should be called after checkPlatformRequirements() so platform types can
    // constrain numeric literals first)
    if (!skip_numeric_defaults) {
        try self.finalizeNumericDefaultsInternal(&env);
    }

    // After solving all deferred constraints, check for infinite types
    for (defs_slice) |def_idx| {
        try self.checkForInfiniteType(CIR.Def.Idx, def_idx);
    }

    // Note that we can't use SCCs to determine the order to resolve defs
    // because anonymous static dispatch makes function order not knowable
    // before type inference

    // TODO: Check for any exposed types that are generalized that are NOT functions
}

/// Process the requires_types annotations for platform modules, like:
///
///   { [Model : model] for main : { init : model, ... } }
///
/// For each required type, we first process the introduced alias variables:
///   { [Model : model] for main : { init : model, ... } }
///     ^^^^^^^^^^^^^^
/// Here, we create `model` as a *rigid* var, and a type alias `Model` pointing to
/// that exact rigid var.
///
/// We create this variable at the `.outermost` rank, so that every places
/// that references this var gets the same instance
///
/// Then, we generate the type for the actual required type
///   { [Model : model] for main : { init : model, ... } }
///                                ^^^^^^^^^^^^^^^^^^^^^^
///
/// Note on scoping: Type scopes are defined in czer. So in the example above,
///   { [Model : model] for main : { init : model, ... } }
///             a^^^^^                     b^^^^^
/// So `a` get the node CIR.TypeAnno.rigid_var{ .. }
/// So `b` get the node CIR.TypeAnno.rigid_var_lookup{ .ref = <a_id> }
/// Then, any reference to `b` replaced with `a` in `generateAnnoTypeInPlace`.
fn processRequiresTypes(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Ensure we are generalized
    // This is because we do not want the type checking we do here to be let-polymorphic
    std.debug.assert(env.rank() == .outermost);

    const requires_types_slice = self.cir.requires_types.items.items;
    for (requires_types_slice) |required_type| {

        // First, processes the required type aliases
        //   { [Model : model] for main : { init : model, ... } }
        //     ^^^^^^^^^^^^^^
        const required_type_aliases_slice = self.cir.for_clause_aliases.sliceRange(required_type.type_aliases);
        for (required_type_aliases_slice) |type_alias| {
            const stmt = self.cir.store.getStatement(type_alias.alias_stmt_idx);
            const stmt_var = ModuleEnv.varFrom(type_alias.alias_stmt_idx);

            // We should only ever have alias decls here
            std.debug.assert(stmt == .s_alias_decl);
            const alias = stmt.s_alias_decl;

            // Assert that this alias header is well formed
            const alias_lhs = self.cir.store.getTypeHeader(alias.header);
            std.debug.assert(alias_lhs.name == alias_lhs.relative_name);
            std.debug.assert(alias_lhs.args.span.len == 0);

            // Assert that this alias body is well formed
            const alias_rhs_var = ModuleEnv.varFrom(alias.anno);
            const alias_rhs = self.cir.store.getTypeAnno(alias.anno);
            std.debug.assert(alias_rhs == .rigid_var);

            // Set ranks to generalized
            try self.setVarRank(stmt_var, env);
            try self.setVarRank(alias_rhs_var, env);

            // Set the rhs of the expr to be a rigid var
            try self.unifyWith(alias_rhs_var, .{
                .rigid = Rigid.init(type_alias.rigid_name),
            }, env);

            // IMPORTANT!
            // We *do not* create a real alias here. Instead, we unify the alias
            // stmt directly with the backing variable not the alias wrapper,
            // so that it can be substituted with the app's concrete type during
            // checkPlatformRequirements.
            _ = try self.unify(stmt_var, alias_rhs_var, env);
        }

        // Then, generate the type for the actual required type
        //   { [Model : model] for main : { init : model, ... } }
        //                                ^^^^^^^^^^^^^^^^^^^^^^
        try self.generateAnnoTypeInPlace(required_type.type_anno, env, .annotation);
    }
}

/// Check that the app's exported values match the platform's required types.
///
/// This should be called after checkFile() to verify that app exports conform
/// to the platform's requirements.
///
/// The `platform_to_app_idents` map translates platform ident indices to app ident indices,
/// built by the caller to avoid string lookups during type checking.
///
/// TODO: There are some non-type errors that this function produces (like
/// if the required alias or definition) are not found These errors could be
/// reporter in czer.
pub fn checkPlatformRequirements(
    self: *Self,
    platform_env: *const ModuleEnv,
    platform_to_app_idents: *const std.AutoHashMap(Ident.Idx, Ident.Idx),
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Ensure the type store is filled to match the number of regions.
    // This is necessary because checkPlatformRequirements may be called with a
    // fresh Check instance that hasn't had checkFile() called on it.
    try ensureTypeStoreIsFilled(self);

    // Create a solver env for type operations
    var env = try self.env_pool.acquire();
    defer self.env_pool.release(env);

    // Push thru to outermost
    std.debug.assert(env.rank() == .generalized);
    try env.var_pool.pushRank();

    // Iterate over the platform's required types
    const requires_types_slice = platform_env.requires_types.items.items;
    for (requires_types_slice) |required_type| {
        // Look up the pre-translated app ident for this platform requirement
        const app_required_ident = platform_to_app_idents.get(required_type.ident);

        // Find the matching export in the app
        const app_exports_slice = self.cir.store.sliceDefs(self.cir.exports);
        var found_export: ?CIR.Def.Idx = null;

        for (app_exports_slice) |def_idx| {
            const def = self.cir.store.getDef(def_idx);
            const pattern = self.cir.store.getPattern(def.pattern);

            if (pattern == .assign) {
                // Compare ident indices - if app_required_ident is null, there's no match
                if (app_required_ident != null and pattern.assign.ident == app_required_ident.?) {
                    found_export = def_idx;
                    break;
                }
            }
        }

        if (found_export) |export_def_idx| {
            // Get the app export's type variable
            const export_def = self.cir.store.getDef(export_def_idx);
            const export_var = ModuleEnv.varFrom(export_def.pattern);

            // Copy the required type from the platform's type store into the app's type store
            // First, convert the type annotation to a type variable in the platform's context
            const required_type_var = ModuleEnv.varFrom(required_type.type_anno);

            // Copy the type from the platform's type store
            const copied_required_var = try self.copyVar(required_type_var, platform_env, required_type.region);

            // Instantiate the copied variable before unifying (to avoid poisoning the cached copy)
            // IMPORTANT: When we instantiate this rigid here, it is instantiated as a flex
            const instantiated_required_var = try self.instantiateVar(copied_required_var, &env, .{ .explicit = required_type.region });

            // Get the type aliases (eg [Model : model]) for this required type
            const type_aliases_range = required_type.type_aliases;
            const all_aliases = platform_env.for_clause_aliases.items.items;
            const type_aliases_slice = all_aliases[@intFromEnum(type_aliases_range.start)..][0..type_aliases_range.count];

            // Extract flex name -> instantiated var mappings from the var_map.
            // Only process flex vars that are declared in the for-clause type aliases.
            // Other flex vars (like those from open tag union extensions `..others`)
            // are polymorphic and don't need to be unified with app-provided aliases.
            var var_map_iter = self.var_map.iterator();
            while (var_map_iter.next()) |entry| {
                const fresh_var = entry.value_ptr.*;
                const resolved = self.types.resolveVar(fresh_var);
                switch (resolved.desc.content) {
                    // Note that here we match on a flex var. Because the
                    // type is instantiated any rigid in the platform
                    // required type become flex
                    .flex => |flex| {
                        // Named flex vars come from rigid vars or named extensions (like `.._others`).
                        // Anonymous flex vars (from `..` syntax) have no name and are skipped.
                        const flex_name = flex.name orelse continue;

                        // Check if this flex var is in the list of rigid vars declared
                        // in the for-clause type aliases. If not, it's from an open tag
                        // union extension (like `..others`) and doesn't need to be stored.
                        var found_in_required_aliases = false;
                        for (type_aliases_slice) |alias| {
                            const app_rigid_name = platform_to_app_idents.get(alias.rigid_name) orelse continue;
                            if (app_rigid_name == flex_name) {
                                found_in_required_aliases = true;
                                break;
                            }
                        }

                        if (found_in_required_aliases) {
                            // Store the rigid (now instantiated flex) name -> instantiated var mapping in the app's module env
                            // Use cir.gpa since rigid_vars belongs to the ModuleEnv
                            try self.cir.rigid_vars.put(self.cir.gpa, flex_name, fresh_var);
                        }
                    },
                    else => {},
                }
            }

            // For each for-clause type alias (e.g., [Model : model]), look up the app's
            // corresponding type alias and unify it with the rigid type variable.
            // This substitutes concrete app types for platform rigid type variables.
            for (type_aliases_slice) |alias| {
                // Translate the platform's alias name to the app's namespace
                const app_alias_name = platform_to_app_idents.get(alias.alias_name) orelse {
                    const expected_alias_ident = try self.cir.insertIdent(
                        Ident.for_text(platform_env.getIdentText(alias.alias_name)),
                    );
                    _ = try self.problems.appendProblem(self.gpa, .{ .platform_alias_not_found = .{
                        .expected_alias_ident = expected_alias_ident,
                        .ctx = .not_found,
                    } });
                    _ = try self.unifyWith(instantiated_required_var, .err, &env);
                    _ = try self.unifyWith(export_var, .err, &env);
                    return;
                };

                // Look up the rigid var we stored earlier.
                // rigid_vars is keyed by the APP's ident index (the rigid name was translated when copied),
                // so we translate the platform's rigid_name to the app's ident space using the pre-built map.
                const app_rigid_name = platform_to_app_idents.get(alias.rigid_name) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("Expected to find platform alias rigid var ident {s} in module", .{
                            platform_env.getIdentText(alias.rigid_name),
                        });
                    }
                    _ = try self.unifyWith(instantiated_required_var, .err, &env);
                    _ = try self.unifyWith(export_var, .err, &env);
                    return;
                };
                const rigid_var = self.cir.rigid_vars.get(app_rigid_name) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("Expected to find rigid var in map {s} in instantiate platform required type", .{
                            platform_env.getIdentText(alias.rigid_name),
                        });
                    }
                    _ = try self.unifyWith(instantiated_required_var, .err, &env);
                    _ = try self.unifyWith(export_var, .err, &env);
                    return;
                };

                // Look up the app's type alias's (eg Model) body (the underlying type, not the alias wrapper)
                const app_type_var = self.findTypeAliasBodyVar(app_alias_name) orelse {
                    const expected_alias_ident = try self.cir.insertIdent(
                        Ident.for_text(platform_env.getIdentText(alias.alias_name)),
                    );
                    _ = try self.problems.appendProblem(self.gpa, .{ .platform_alias_not_found = .{
                        .expected_alias_ident = expected_alias_ident,
                        .ctx = .found_but_not_alias,
                    } });
                    _ = try self.unifyWith(instantiated_required_var, .err, &env);
                    _ = try self.unifyWith(export_var, .err, &env);
                    return;
                };

                // Now unify the (now-flex) var with the app's type alias body.
                // This properly handles rank propagation (unlike dangerousSetVarRedirect).
                _ = try self.unify(rigid_var, app_type_var, &env);
            }

            // Unify the platform's required type with the app's export type.
            // This constrains type variables in the export (e.g., closure params)
            // to match the platform's expected types. After this, the fresh vars
            // stored in rigid_vars will redirect to the concrete app types.
            // Context is set for error messages about which platform requirement wasn't satisfied.
            const app_ident = app_required_ident orelse try self.cir.insertIdent(
                Ident.for_text(platform_env.getIdentText(required_type.ident)),
            );
            _ = try self.unifyInContext(instantiated_required_var, export_var, &env, .{
                .platform_requirement = .{ .required_ident = app_ident },
            });
        } else {
            // If we got here, it means that the the definition was not found in
            // the module's *export* list
            const expected_def_ident = try self.cir.insertIdent(
                Ident.for_text(platform_env.getIdentText(required_type.ident)),
            );
            _ = try self.problems.appendProblem(self.gpa, .{
                .platform_def_not_found = .{
                    .expected_def_ident = expected_def_ident,
                    .ctx = blk: {
                        // We know the def is not exported, but here we check
                        // if it's defined *but not exported* in the module so
                        // we can show a nicer error message

                        var found_def: ?CIR.Def.Idx = null;

                        // Check all defs in the module
                        const app_defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
                        for (app_defs_slice) |def_idx| {
                            const def = self.cir.store.getDef(def_idx);
                            const pattern = self.cir.store.getPattern(def.pattern);

                            if (pattern == .assign) {
                                // Compare ident indices - if app_required_ident is null, there's no match
                                if (app_required_ident != null and pattern.assign.ident == app_required_ident.?) {
                                    found_def = def_idx;
                                    break;
                                }
                            }
                        }

                        // Break with more specific context
                        if (found_def == null) {
                            break :blk .not_found;
                        } else {
                            break :blk .found_but_not_exported;
                        }
                    },
                },
            });
        }
        // Note: If the export is not found, the canonicalizer should have already reported an error
    }
}

/// Find a type alias declaration by name and return the var for its underlying type.
/// This returns the var for the alias's body (e.g., for `Model : { value: I64 }` returns the var for `{ value: I64 }`),
/// not the var for the alias declaration itself.
/// Returns null if no type alias declaration with the given name is found.
fn findTypeAliasBodyVar(self: *Self, name: Ident.Idx) ?Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stmts_slice = self.cir.store.sliceStatements(self.cir.all_statements);
    for (stmts_slice) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_alias_decl => |alias| {
                const header = self.cir.store.getTypeHeader(alias.header);
                if (header.relative_name == name) {
                    // Return the var for the alias body annotation, not the statement
                    return ModuleEnv.varFrom(alias.anno);
                }
            },
            else => {},
        }
    }
    return null;
}

/// Check if a statement index is a for-clause alias statement.
/// For-clause alias statements are created during platform header processing
/// for type aliases like [Model : model] in the requires clause.
///
/// When these are looked up, we need to *not* instantiate the alias, so all
/// references in the module Point to the same var.
fn isForClauseAliasStatement(self: *Self, stmt_idx: CIR.Statement.Idx) bool {
    // Slice the for-clause alias statements and check if stmt_idx is in the list
    for (self.cir.for_clause_aliases.items.items) |for_clause| {
        if (stmt_idx == for_clause.alias_stmt_idx) {
            return true;
        }
    }
    return false;
}

// repl //

/// Check an expr for the repl
pub fn checkExprRepl(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    try ensureTypeStoreIsFilled(self);

    // Copy builtin types into this module's type store
    try self.copyBuiltinTypes();

    // Create a solver env
    var env = try self.env_pool.acquire();
    defer self.env_pool.release(env);
    std.debug.assert(env.rank() == .generalized);

    // First, iterate over the statements, generating types for each type declaration
    const stms_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (stms_slice) |stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(stmt_idx, &env);
    }

    // Set the rank to be outermost
    try env.var_pool.pushRank();
    std.debug.assert(env.rank() == .outermost);

    // Check the expr
    _ = try self.checkExpr(expr_idx, &env, .no_expectation);

    // Check any accumulated constraints
    try self.checkAllConstraints(&env);
    try self.resolveNumericLiteralsFromContext(&env);
    try self.finalizeNumericDefaultsInternal(&env);

    // Check if the expression's type has incompatible constraints (e.g., !3)
    const expr_var = ModuleEnv.varFrom(expr_idx);
    try self.checkFlexVarConstraintCompatibility(expr_var, &env);

    // Check for infinite types
    try self.checkForInfiniteType(CIR.Expr.Idx, expr_idx);
}

/// Check a REPL expression, also type-checking any definitions (for local type declarations)
pub fn checkExprReplWithDefs(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    try ensureTypeStoreIsFilled(self);

    // Copy builtin types into this module's type store
    try self.copyBuiltinTypes();

    // Create a solver env
    var env = try self.env_pool.acquire();
    defer self.env_pool.release(env);
    std.debug.assert(env.rank() == .generalized);

    // First, iterate over the statements, generating types for each type declaration
    // Note that any types generated will be generalized
    const stms_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (stms_slice) |stmt_idx| {
        try self.generateStmtTypeDeclType(stmt_idx, &env);
    }

    // Initialize top_level_ptrns with any defs from local type declarations
    const defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.cir.store.getDef(def_idx);
        try self.top_level_ptrns.put(def.pattern, DefProcessed{
            .def_idx = def_idx,
            .def_name = null,
            .status = .not_processed,
        });
    }

    // Set the rank to be outermost
    try env.var_pool.pushRank();
    std.debug.assert(env.rank() == .outermost);

    // Type-check defs from local type declarations (their associated blocks)
    for (defs_slice) |def_idx| {
        try self.checkDef(def_idx, &env);

        // Ensure that after processing a def, checkDef correctly restores the
        // rank to outermost for the next level of processing
        std.debug.assert(env.rank() == .outermost);
    }

    // Check the expr
    _ = try self.checkExpr(expr_idx, &env, .no_expectation);

    // Check any accumulated constraints
    try self.checkAllConstraints(&env);
    try self.resolveNumericLiteralsFromContext(&env);
    try self.finalizeNumericDefaultsInternal(&env);

    // After finalizing numeric defaults, resolve any remaining deferred
    // static dispatch constraints. finalizeNumericDefaults unifies from_numeral
    // flex vars with Dec, which may make deferred method_call constraints
    // resolvable (e.g., Dec.to_str returns Str). Without this step, the
    // return type of methods on defaulted numerics remains an unconstrained
    // flex var, causing incorrect .zst layouts.
    if (env.deferred_static_dispatch_constraints.items.items.len > 0) {
        try self.checkStaticDispatchConstraints(&env);
        try self.checkAllConstraints(&env);
    }

    // After solving all deferred constraints, check for infinite types
    for (defs_slice) |def_idx| {
        try self.checkForInfiniteType(CIR.Def.Idx, def_idx);
    }
}

// defs //

/// Check the types for a single definition
fn checkDef(self: *Self, def_idx: CIR.Def.Idx, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const def = self.cir.store.getDef(def_idx);

    if (self.top_level_ptrns.get(def.pattern)) |processing_def| {
        if (processing_def.status == .processed) {
            // If we've already processed this def, return immediately
            return;
        }
    }

    // Track which def is currently being processed (for cycle detection)
    const saved_processing_def = self.current_processing_def;
    self.current_processing_def = def_idx;
    defer self.current_processing_def = saved_processing_def;

    // Make as processing
    const def_name = self.getPatternIdent(def.pattern);
    try self.top_level_ptrns.put(def.pattern, .{
        .def_idx = def_idx,
        .def_name = def_name,
        .status = .processing,
    });

    // Check if this expr is one that should be generalized
    const def_var = ModuleEnv.varFrom(def_idx);
    const ptrn_var = ModuleEnv.varFrom(def.pattern);
    const expr_var = ModuleEnv.varFrom(def.expr);

    // Ensure that initially we are at top level
    std.debug.assert(env.rank() == .outermost);

    // Set the ptrn and expr rank
    try self.setVarRank(def_var, env);
    try self.setVarRank(ptrn_var, env);

    // Check the pattern
    try self.checkPattern(def.pattern, env);

    // Extract function name from the pattern (for better error messages)
    const saved_func_name = self.enclosing_func_name;
    self.enclosing_func_name = def_name;
    defer self.enclosing_func_name = saved_func_name;

    // Check the annotation, if it exists
    const expectation = blk: {
        if (def.annotation) |annotation_idx| {
            break :blk Expected{ .expected = annotation_idx };
        } else {
            break :blk Expected.no_expectation;
        }
    };

    // Infer types for the body, checking against the instantiated annotation
    _ = try self.checkExpr(def.expr, env, expectation);

    if (self.defer_generalize) {
        // defer_generalize is only set when a cycle root has been identified.
        std.debug.assert(self.cycle_root_def != null);

        // Defer unifications until after generalization.
        // If we unify now, def_var(R1) with expr_var(R2) lowers expr_var
        // to R1 in the type store, and generalize at R2 would skip it.
        try self.deferred_def_unifications.append(self.gpa, .{
            .def_var = def_var,
            .ptrn_var = ptrn_var,
            .expr_var = expr_var,
        });
    } else {
        // Unify the ptrn and the expr
        _ = try self.unify(ptrn_var, expr_var, env);

        // Unify the def and ptrn
        _ = try self.unify(def_var, ptrn_var, env);
    }

    // Mark as processed
    try self.top_level_ptrns.put(def.pattern, .{
        .def_idx = def_idx,
        .def_name = def_name,
        .status = .processed,
    });
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
    env: *Env,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const decl = self.cir.store.getStatement(decl_idx);
    const decl_var = ModuleEnv.varFrom(decl_idx);

    switch (decl) {
        .s_alias_decl => |alias| {
            try self.generateAliasDecl(decl_idx, decl_var, alias, env);
        },
        .s_nominal_decl => |nominal| {
            try self.generateNominalDecl(decl_idx, decl_var, nominal, env);
        },
        .s_runtime_error => {
            try self.unifyWith(decl_var, .err, env);
        },
        else => {
            // Do nothing
        },
    }
}

/// Generate types for an alias type declaration
fn generateAliasDecl(
    self: *Self,
    decl_idx: CIR.Statement.Idx,
    decl_var: Var,
    alias: std.meta.fieldInfo(CIR.Statement, .s_alias_decl).type,
    env: *Env,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Get the type header's args
    const header = self.cir.store.getTypeHeader(alias.header);
    const header_args = self.cir.store.sliceTypeAnnos(header.args);

    // Next, generate the provided arg types and build the map of rigid variables in the header
    const header_vars = try self.generateHeaderVars(header_args, env);

    // Now we have a built of list of rigid variables for the decl lhs (header).
    // With this in hand, we can now generate the type for the lhs (body).
    self.seen_annos.clearRetainingCapacity();
    const backing_var: Var = ModuleEnv.varFrom(alias.anno);
    try self.generateAnnoTypeInPlace(alias.anno, env, .{ .type_decl = .{
        .idx = decl_idx,
        .name = header.relative_name,
        .type_ = .alias,
        .backing_var = backing_var,
        .num_args = @intCast(header_args.len),
    } });

    // Use the cached builtin_module_ident from the current module's ident store.
    // This represents the "Builtin" module where List is defined.
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    try self.unifyWith(
        decl_var,
        try self.types.mkAlias(
            .{ .ident_idx = header.relative_name },
            backing_var,
            header_vars,
            origin_module_id,
        ),
        env,
    );
}

/// Generate types for nominal type declaration
fn generateNominalDecl(
    self: *Self,
    decl_idx: CIR.Statement.Idx,
    decl_var: Var,
    nominal: std.meta.fieldInfo(CIR.Statement, .s_nominal_decl).type,
    env: *Env,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Get the type header's args
    const header = self.cir.store.getTypeHeader(nominal.header);
    const header_args = self.cir.store.sliceTypeAnnos(header.args);

    // Next, generate the provided arg types and build the map of rigid variables in the header
    const header_vars = try self.generateHeaderVars(header_args, env);

    // Now we have a built of list of rigid variables for the decl lhs (header).
    // With this in hand, we can now generate the type for the lhs (body).
    self.seen_annos.clearRetainingCapacity();
    const backing_var: Var = ModuleEnv.varFrom(nominal.anno);
    try self.generateAnnoTypeInPlace(nominal.anno, env, .{ .type_decl = .{
        .idx = decl_idx,
        .name = header.relative_name,
        .type_ = .nominal,
        .backing_var = backing_var,
        .num_args = @intCast(header_args.len),
    } });

    try self.unifyWith(
        decl_var,
        try self.types.mkNominal(
            .{ .ident_idx = header.relative_name },
            backing_var,
            header_vars,
            self.builtin_ctx.module_name,
            nominal.is_opaque,
        ),
        env,
    );
}

/// Generate types for a standalone type annotation (one without a corresponding definition).
/// These are typically used for FFI function declarations or forward declarations.
fn generateStandaloneTypeAnno(
    self: *Self,
    stmt_var: Var,
    type_anno: std.meta.fieldInfo(CIR.Statement, .s_type_anno).type,
    env: *Env,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Reset seen type annos
    self.seen_annos.clearRetainingCapacity();

    // Save top of scratch static dispatch constraints
    const scratch_static_dispatch_constraints_top = self.scratch_static_dispatch_constraints.top();
    defer self.scratch_static_dispatch_constraints.clearFrom(scratch_static_dispatch_constraints_top);

    // Iterate over where clauses (if they exist), adding them to scratch_static_dispatch_constraints
    if (type_anno.where) |where_span| {
        const where_slice = self.cir.store.sliceWhereClauses(where_span);
        for (where_slice) |where_idx| {
            try self.generateStaticDispatchConstraintFromWhere(where_idx, env);
        }
    }

    // Generate the type from the annotation
    const anno_var: Var = ModuleEnv.varFrom(type_anno.anno);
    try self.generateAnnoTypeInPlace(type_anno.anno, env, .annotation);

    // Unify the statement variable with the generated annotation type
    _ = try self.unify(stmt_var, anno_var, env);
}

/// Generate types for type anno args
fn generateHeaderVars(
    self: *Self,
    header_args: []CIR.TypeAnno.Idx,
    env: *Env,
) std.mem.Allocator.Error![]Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    for (header_args) |header_arg_idx| {
        const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
        const header_var = ModuleEnv.varFrom(header_arg_idx);
        try self.setVarRank(header_var, env);

        switch (header_arg) {
            .rigid_var => |rigid| {
                try self.unifyWith(header_var, .{ .rigid = Rigid.init(rigid.name) }, env);
            },
            .underscore, .malformed => {
                try self.unifyWith(header_var, .err, env);
            },
            else => {
                // The canonicalizer should only produce rigid_var, underscore, or malformed
                // for header args. If we hit this, there's a compiler bug.
                std.debug.assert(false);
                try self.unifyWith(header_var, .err, env);
            },
        }
    }

    return @ptrCast(header_args);
}

// type gen config //

const OutVar = enum {
    in_place,
    fresh,

    pub fn voidOrVar(comptime out_var: OutVar) type {
        return switch (out_var) {
            .in_place => void,
            .fresh => Var,
        };
    }
};

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

fn generateAnnotationType(self: *Self, annotation_idx: CIR.Annotation.Idx, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const annotation_var = ModuleEnv.varFrom(annotation_idx);
    try self.setVarRank(annotation_var, env);

    const annotation = self.cir.store.getAnnotation(annotation_idx);

    // Reset seen type annos
    self.seen_annos.clearRetainingCapacity();

    // Save top of scratch static dispatch constraints
    const scratch_static_dispatch_constraints_top = self.scratch_static_dispatch_constraints.top();
    defer self.scratch_static_dispatch_constraints.clearFrom(scratch_static_dispatch_constraints_top);

    // Iterate over where clauses (if they exist), adding them to scratch_static_dispatch_constraints
    if (annotation.where) |where_span| {
        const where_slice = self.cir.store.sliceWhereClauses(where_span);
        for (where_slice) |where_idx| {
            try self.generateStaticDispatchConstraintFromWhere(where_idx, env);
        }
    }

    // Then, generate the type for the annotation
    try self.generateAnnoTypeInPlace(annotation.anno, env, .annotation);

    // Redirect the root annotation to inner annotation
    _ = try self.unify(annotation_var, ModuleEnv.varFrom(annotation.anno), env);
}

/// Given a where clause, generate static dispatch constraints and add to scratch_static_dispatch_constraints
fn generateStaticDispatchConstraintFromWhere(self: *Self, where_idx: CIR.WhereClause.Idx, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const where = self.cir.store.getWhereClause(where_idx);
    const where_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(where_idx));

    switch (where) {
        .w_method => |method| {
            // Generate type of the thing dispatch receiver
            try self.generateAnnoTypeInPlace(method.var_, env, .annotation);
            const method_var = ModuleEnv.varFrom(method.var_);

            // Generate the arguments
            const args_anno_slice = self.cir.store.sliceTypeAnnos(method.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, env, .annotation);
            }
            const anno_arg_vars: []Var = @ptrCast(args_anno_slice);

            // Generate return type
            try self.generateAnnoTypeInPlace(method.ret, env, .annotation);
            const ret_var = ModuleEnv.varFrom(method.ret);

            // Create the function var
            const func_content = try self.types.mkFuncUnbound(anno_arg_vars, ret_var);
            const func_var = try self.freshFromContent(func_content, env, where_region);

            // Add to scratch list
            try self.scratch_static_dispatch_constraints.append(ScratchStaticDispatchConstraint{
                .var_ = method_var,
                .constraint = StaticDispatchConstraint{
                    .fn_name = method.method_name,
                    .fn_var = func_var,
                    .origin = .where_clause,
                },
            });
        },
        .w_alias => |alias| {
            // Alias syntax in where clauses (e.g., `where [a.Decode]`) was used for abilities,
            // which have been removed from Roc. Emit an error.
            _ = try self.problems.appendProblem(self.gpa, .{ .unsupported_alias_where_clause = .{
                .alias_name = alias.alias_name,
                .region = where_region,
            } });
        },
        .w_malformed => {
            // If it's malformed, just ignore
        },
    }
}

/// Given an annotation, generate the corresponding type based on the CIR
///
/// This is used both for generation annotation types and type declaration types
///
/// This function will write the type into the type var node at `anno_idx`
///
/// Note on scoping for type decls: Type scopes are defined in czer
///   Point(x) : [Point(x, x)]
///        a^          b^  ^c
///
/// So `a` get the node CIR.TypeAnno.rigid_var{ .. }
/// And `b` & `c` get the node CIR.TypeAnno.rigid_var_lookup{ .ref = <a_id> }
/// Then, any reference to `b` or `c` are replaced with `a` in `generateAnnoTypeInPlace`.
fn generateAnnoTypeInPlace(self: *Self, anno_idx: CIR.TypeAnno.Idx, env: *Env, ctx: GenTypeAnnoCtx) std.mem.Allocator.Error!void {
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
    try self.setVarRank(anno_var, env);

    // Put this anno in the "seen" map immediately, to support recursive references
    try self.seen_annos.put(anno_idx, anno_var);

    switch (anno) {
        .rigid_var => |rigid| {
            const static_dispatch_constraints_start = self.types.static_dispatch_constraints.len();
            switch (ctx) {
                .annotation => {
                    // If this an annotation, then check all where constraints
                    // and see if any reference this rigid var
                    for (self.scratch_static_dispatch_constraints.items.items) |scratch_constraint| {
                        const resolved_scratch_var = self.types.resolveVar(scratch_constraint.var_).var_;
                        if (resolved_scratch_var == anno_var) {
                            _ = try self.types.static_dispatch_constraints.append(self.types.gpa, scratch_constraint.constraint);
                        }
                    }
                },
                .type_decl => {},
            }
            const static_dispatch_constraints_end = self.types.static_dispatch_constraints.len();
            const static_dispatch_constraints_range = StaticDispatchConstraint.SafeList.Range{ .start = @enumFromInt(static_dispatch_constraints_start), .count = @intCast(static_dispatch_constraints_end - static_dispatch_constraints_start) };

            try self.unifyWith(anno_var, .{ .rigid = Rigid{
                .name = rigid.name,
                .constraints = static_dispatch_constraints_range,
            } }, env);
        },
        .rigid_var_lookup => |rigid_lookup| {
            _ = try self.unify(anno_var, ModuleEnv.varFrom(rigid_lookup.ref), env);
        },
        .underscore => {
            try self.unifyWith(anno_var, .{ .flex = Flex.init() }, env);
        },
        .lookup => |lookup| {
            switch (lookup.base) {
                .builtin => |builtin_type| {
                    try self.setBuiltinTypeContent(anno_var, lookup.name, builtin_type, &.{}, anno_region, env);
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
                                        .num_expected_args = this_decl.num_args,
                                        .num_actual_args = 0,
                                    } });
                                    try self.unifyWith(anno_var, .err, env);
                                    return;
                                }

                                // If so, then update this annotation to be an instance
                                // of this type using the same backing variable
                                switch (this_decl.type_) {
                                    .alias => {
                                        // Recursion is not allowed in aliases - emit error
                                        _ = try self.problems.appendProblem(self.gpa, .{ .recursive_alias = .{
                                            .type_name = this_decl.name,
                                            .region = anno_region,
                                        } });
                                        try self.unifyWith(anno_var, .err, env);
                                        return;
                                    },
                                    .nominal => {
                                        // Nominal types can be recursive
                                        try self.unifyWith(anno_var, try self.types.mkNominal(
                                            .{ .ident_idx = this_decl.name },
                                            this_decl.backing_var,
                                            &.{},
                                            self.builtin_ctx.module_name,
                                            false, // Default to non-opaque for error case
                                        ), env);
                                    },
                                }

                                return;
                            }
                        },
                        .annotation => {
                            // Otherwise, we're in an annotation and this cannot
                            // be recursive
                        },
                    }

                    const local_decl_var = ModuleEnv.varFrom(local.decl_idx);

                    // Check if this is a for-clause alias (eg Model [Model : model]).
                    // For for-clause aliases, we do not want to instantiate the
                    // variable - each place that references it should reference
                    // the same var.
                    const is_for_clause_alias = self.isForClauseAliasStatement(local.decl_idx);
                    if (is_for_clause_alias) {
                        _ = try self.unify(anno_var, local_decl_var, env);
                    } else {
                        const instantiated_var = try self.instantiateVar(local_decl_var, env, .{ .explicit = anno_region });
                        _ = try self.unify(anno_var, instantiated_var, env);
                    }
                },
                .external => |ext| {
                    if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                        const ext_instantiated_var = try self.instantiateVar(
                            ext_ref.local_var,
                            env,
                            .{ .explicit = anno_region },
                        );
                        _ = try self.unify(anno_var, ext_instantiated_var, env);
                    } else {
                        // If this external type is unresolved, can should've reported
                        // an error. So we set to error and continue
                        try self.unifyWith(anno_var, .err, env);
                    }
                },
                .pending => {
                    // If an import references a non-existent module (e.g., missing from
                    // platform bundle), the pending lookup can't be resolved. Treat as error.
                    try self.unifyWith(anno_var, .err, env);
                },
            }
        },
        .apply => |a| {
            // Generate the types for the arguments
            const anno_args = self.cir.store.sliceTypeAnnos(a.args);
            for (anno_args) |anno_arg| {
                try self.generateAnnoTypeInPlace(anno_arg, env, ctx);
            }
            const anno_arg_vars: []Var = @ptrCast(anno_args);

            switch (a.base) {
                .builtin => |builtin_type| {
                    try self.setBuiltinTypeContent(anno_var, a.name, builtin_type, anno_arg_vars, anno_region, env);
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
                                    try self.unifyWith(anno_var, .err, env);
                                    return;
                                }

                                // If so, then update this annotation to be an instance
                                // of this type using the same backing variable
                                switch (this_decl.type_) {
                                    .alias => {
                                        // Recursion is not allowed in aliases - emit error
                                        _ = try self.problems.appendProblem(self.gpa, .{ .recursive_alias = .{
                                            .type_name = this_decl.name,
                                            .region = anno_region,
                                        } });
                                        try self.unifyWith(anno_var, .err, env);
                                        return;
                                    },
                                    .nominal => {
                                        // Nominal types can be recursive
                                        try self.unifyWith(anno_var, try self.types.mkNominal(
                                            .{ .ident_idx = this_decl.name },
                                            this_decl.backing_var,
                                            anno_arg_vars,
                                            self.builtin_ctx.module_name,
                                            false, // Default to non-opaque for error case
                                        ), env);
                                    },
                                }

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
                            try self.unifyWith(anno_var, .err, env);
                            return;
                        } else {
                            // Type applications should only reference aliases or nominal types.
                            // If we hit this, there's a compiler bug.
                            std.debug.assert(false);
                            try self.unifyWith(anno_var, .err, env);
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
                        try self.unifyWith(anno_var, .err, env);
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
                        env,
                        .{ .explicit = anno_region },
                    );
                    _ = try self.unify(anno_var, instantiated_var, env);
                },
                .external => |ext| {
                    if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                        // Resolve the referenced type
                        const ext_resolved = self.types.resolveVar(ext_ref.local_var).desc.content;

                        // Get the arguments & name the referenced type
                        const ext_arg_vars, const ext_name = blk: {
                            switch (ext_resolved) {
                                .alias => |decl_alias| {
                                    break :blk .{ self.types.sliceAliasArgs(decl_alias), decl_alias.ident.ident_idx };
                                },
                                .structure => |flat_type| {
                                    if (flat_type == .nominal_type) {
                                        const decl_nominal = flat_type.nominal_type;
                                        break :blk .{ self.types.sliceNominalArgs(decl_nominal), decl_nominal.ident.ident_idx };
                                    } else {
                                        // External type resolved to a non-nominal structure (e.g., record, func, etc.)
                                        // This shouldn't happen for type applications, treat as error
                                        try self.unifyWith(anno_var, .err, env);
                                        return;
                                    }
                                },
                                .err => {
                                    try self.unifyWith(anno_var, .err, env);
                                    return;
                                },
                                .flex, .rigid => {
                                    // External type resolved to a flex or rigid.
                                    // This can happen when the external type is polymorphic but hasn't been
                                    // instantiated yet. We need to use the variable as-is, but this means
                                    // we can't get the arity/name information. This is likely a bug in how
                                    // the external type was set up. For now, treat it as an error.
                                    try self.unifyWith(anno_var, .err, env);
                                    return;
                                },
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
                            try self.unifyWith(anno_var, .err, env);
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
                            env,
                            .{ .explicit = anno_region },
                        );
                        _ = try self.unify(anno_var, instantiated_var, env);
                    } else {
                        // If this external type is unresolved, can should've reported
                        // an error. So we set to error and continue
                        try self.unifyWith(anno_var, .err, env);
                    }
                },
                .pending => {
                    // If an import references a non-existent module (e.g., missing from
                    // platform bundle), the pending lookup can't be resolved. Treat as error.
                    try self.unifyWith(anno_var, .err, env);
                },
            }
        },
        .@"fn" => |func| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, env, ctx);
            }
            const args_var_slice: []Var = @ptrCast(args_anno_slice);

            try self.generateAnnoTypeInPlace(func.ret, env, ctx);

            const fn_type = inner_blk: {
                if (func.effectful) {
                    break :inner_blk try self.types.mkFuncEffectful(args_var_slice, ModuleEnv.varFrom(func.ret));
                } else {
                    break :inner_blk try self.types.mkFuncPure(args_var_slice, ModuleEnv.varFrom(func.ret));
                }
            };
            try self.unifyWith(anno_var, fn_type, env);
        },
        .tag_union => |tag_union| {
            const scratch_tags_top = self.scratch_tags.top();
            defer self.scratch_tags.clearFrom(scratch_tags_top);

            const tag_anno_slices = self.cir.store.sliceTypeAnnos(tag_union.tags);
            for (tag_anno_slices) |tag_anno_idx| {
                // Get the tag anno
                const tag_type_anno = self.cir.store.getTypeAnno(tag_anno_idx);
                try self.setVarRank(ModuleEnv.varFrom(tag_anno_idx), env);

                // If the child of the tag union is not a tag, then set as error
                // Canonicalization should have reported this error
                if (tag_type_anno != .tag) {
                    try self.unifyWith(anno_var, .err, env);
                    return;
                }
                const tag = tag_type_anno.tag;

                // Generate the types for each tag arg
                const tag_anno_args_slice = self.cir.store.sliceTypeAnnos(tag.args);
                for (tag_anno_args_slice) |tag_arg_idx| {
                    try self.generateAnnoTypeInPlace(tag_arg_idx, env, ctx);
                }
                const tag_vars_slice: []Var = @ptrCast(tag_anno_args_slice);

                // Add the processed tag to scratch
                try self.scratch_tags.append(try self.types.mkTag(
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
                    try self.generateAnnoTypeInPlace(ext_anno_idx, env, ctx);
                    break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                } else {
                    break :inner_blk try self.freshFromContent(.{ .structure = .empty_tag_union }, env, anno_region);
                }
            };

            // Set the anno's type
            try self.unifyWith(anno_var, try self.types.mkTagUnion(tags_slice, ext_var), env);
        },
        .tag => {
            // Tags should only exist as direct children of tag_unions in type annotations.
            // If we encounter a standalone tag here, it's a compiler bug in canonicalization.
            std.debug.assert(false);
            try self.unifyWith(anno_var, .err, env);
        },
        .record => |rec| {
            const scratch_record_fields_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(scratch_record_fields_top);

            const recs_anno_slice = self.cir.store.sliceAnnoRecordFields(rec.fields);

            for (recs_anno_slice) |rec_anno_idx| {
                const rec_field = self.cir.store.getAnnoRecordField(rec_anno_idx);

                try self.generateAnnoTypeInPlace(rec_field.ty, env, ctx);
                const record_field_var = ModuleEnv.varFrom(rec_field.ty);

                // Add the processed tag to scratch
                try self.scratch_record_fields.append(types_mod.RecordField{
                    .name = rec_field.name,
                    .var_ = record_field_var,
                });
            }

            // Get the slice of record_fields
            const record_fields_slice = self.scratch_record_fields.sliceFromStart(scratch_record_fields_top);
            std.mem.sort(types_mod.RecordField, record_fields_slice, self.cir.common.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
            const fields_type_range = try self.types.appendRecordFields(record_fields_slice);

            // Process the ext if it exists. Absence (null) means it's a closed record.
            const ext_var = if (rec.ext) |ext_anno_idx| blk: {
                try self.generateAnnoTypeInPlace(ext_anno_idx, env, ctx);
                break :blk ModuleEnv.varFrom(ext_anno_idx);
            } else blk: {
                break :blk try self.freshFromContent(.{ .structure = .empty_record }, env, anno_region);
            };

            // Create the type for the anno in the store
            try self.unifyWith(
                anno_var,
                .{ .structure = types_mod.FlatType{ .record = .{
                    .fields = fields_type_range,
                    .ext = ext_var,
                } } },
                env,
            );
        },
        .tuple => |tuple| {
            const elems_anno_slice = self.cir.store.sliceTypeAnnos(tuple.elems);
            for (elems_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, env, ctx);
            }
            const elems_range = try self.types.appendVars(@ptrCast(elems_anno_slice));
            try self.unifyWith(anno_var, .{ .structure = .{ .tuple = .{ .elems = elems_range } } }, env);
        },
        .parens => |parens| {
            try self.generateAnnoTypeInPlace(parens.anno, env, ctx);
            _ = try self.unify(anno_var, ModuleEnv.varFrom(parens.anno), env);
        },
        .malformed => {
            try self.unifyWith(anno_var, .err, env);
        },
    }
}

/// Set the content of anno_var to the builtin type.
///
/// Uses unifyWith to efficiently set content directly when possible,
/// avoiding the creation of an intermediate type variable.
fn setBuiltinTypeContent(
    self: *Self,
    anno_var: Var,
    anno_builtin_name: Ident.Idx,
    anno_builtin_type: CIR.TypeAnno.Builtin,
    anno_args: []Var,
    anno_region: Region,
    env: *Env,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (anno_builtin_type) {
        // Phase 5: Use nominal types from Builtin instead of special .num content
        .u8 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("U8", env), env),
        .u16 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("U16", env), env),
        .u32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("U32", env), env),
        .u64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("U64", env), env),
        .u128 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("U128", env), env),
        .i8 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("I8", env), env),
        .i16 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("I16", env), env),
        .i32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("I32", env), env),
        .i64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("I64", env), env),
        .i128 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("I128", env), env),
        .f32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("F32", env), env),
        .f64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent("F64", env), env),
        .dec => try self.unifyWith(anno_var, try self.mkNumberTypeContent("Dec", env), env),
        .list => {
            // Then check arity
            if (anno_args.len != 1) {
                _ = try self.problems.appendProblem(self.gpa, .{ .type_apply_mismatch_arities = .{
                    .type_name = anno_builtin_name,
                    .region = anno_region,
                    .num_expected_args = 1,
                    .num_actual_args = @intCast(anno_args.len),
                } });

                // Set error
                try self.unifyWith(anno_var, .err, env);
                return;
            }

            // Create the nominal List type
            const list_content = try self.mkListContent(anno_args[0], env);
            try self.unifyWith(anno_var, list_content, env);
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

                // Set error
                try self.unifyWith(anno_var, .err, env);
                return;
            }

            // Create the nominal Box type
            const box_content = try self.mkBoxContent(anno_args[0]);
            try self.unifyWith(anno_var, box_content, env);
        },
        // Polymorphic Num type is a module, not a type itself
        .num => {
            // Set error - Num is a module containing numeric types, not a type
            try self.unifyWith(anno_var, .err, env);
        },
    }
}

// types //

const Expected = union(enum) {
    no_expectation,
    expected: CIR.Annotation.Idx,
};

// pattern //

/// Check the types for the provided pattern, saving the type in-place
fn checkPattern(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    env: *Env,
) std.mem.Allocator.Error!void {
    _ = try self.checkPatternHelp(pattern_idx, env, .in_place);
}

/// Check the types for the provided pattern, either as fresh var or in-place
fn checkPatternHelp(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    env: *Env,
    comptime out_var: OutVar,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const pattern = self.cir.store.getPattern(pattern_idx);
    const pattern_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(pattern_idx));
    const pattern_var = switch (comptime out_var) {
        .fresh => try self.fresh(env, pattern_region),
        .in_place => blk: {
            try self.setVarRank(ModuleEnv.varFrom(pattern_idx), env);
            break :blk ModuleEnv.varFrom(pattern_idx);
        },
    };

    switch (pattern) {
        .assign => |_| {
            // In the case of an assigned variable, set it to be a flex var initially.
            // This will be refined based on how it's used.
            try self.unifyWith(pattern_var, .{ .flex = Flex.init() }, env);
        },
        .underscore => |_| {
            // Underscore can be anything
            try self.unifyWith(pattern_var, .{ .flex = Flex.init() }, env);
        },
        // str //
        .str_literal => {
            const str_var = try self.freshStr(env, pattern_region);
            _ = try self.unify(pattern_var, str_var, env);
        },
        // as //
        .as => |p| {
            const var_ = try self.checkPatternHelp(p.pattern, env, out_var);
            _ = try self.unify(var_, pattern_var, env);
        },
        // tuple //
        .tuple => |tuple| {
            const elem_vars_slice = blk: {
                switch (comptime out_var) {
                    .fresh => {
                        const scratch_vars_top = self.scratch_vars.top();
                        defer self.scratch_vars.clearFrom(scratch_vars_top);

                        // Check tuple elements
                        const elems_slice = self.cir.store.slicePatterns(tuple.patterns);
                        for (elems_slice) |single_elem_ptrn_idx| {
                            const elem_var = try self.checkPatternHelp(single_elem_ptrn_idx, env, out_var);
                            try self.scratch_vars.append(elem_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },
                    .in_place => {
                        // Check tuple elements
                        const elems_slice = self.cir.store.slicePatterns(tuple.patterns);
                        for (elems_slice) |single_elem_ptrn_idx| {
                            _ = try self.checkPatternHelp(single_elem_ptrn_idx, env, out_var);
                        }

                        // Add to types store
                        // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
                        break :blk try self.types.appendVars(@ptrCast(elems_slice));
                    },
                }
            };

            // Set the type in the store
            try self.unifyWith(pattern_var, .{ .structure = .{
                .tuple = .{ .elems = elem_vars_slice },
            } }, env);
        },
        // list //
        .list => |list| {
            const elems = self.cir.store.slicePatterns(list.patterns);
            if (elems.len == 0) {
                // Create a nominal List with a fresh unbound element type
                const elem_var = try self.fresh(env, pattern_region);
                const list_content = try self.mkListContent(elem_var, env);
                try self.unifyWith(pattern_var, list_content, env);
            } else {

                // Here, we use the list's 1st element as the element var to
                // constrain the rest of the list

                // Check the first elem
                const elem_var = try self.checkPatternHelp(elems[0], env, out_var);

                // Iterate over the remaining elements
                var last_elem_ptrn_idx = elems[0];
                for (elems[1..], 1..) |elem_ptrn_idx, i| {
                    const cur_elem_var = try self.checkPatternHelp(elem_ptrn_idx, env, out_var);

                    // Unify each element's var with the list's elem var
                    const result = try self.unifyInContext(elem_var, cur_elem_var, env, .{ .list_entry = .{
                        .elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_ptrn_idx),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            _ = try self.checkPatternHelp(remaining_elem_expr_idx, env, out_var);
                        }

                        // Break to avoid cascading errors
                        break;
                    }

                    last_elem_ptrn_idx = elem_ptrn_idx;
                }

                // Create a nominal List type with the inferred element type
                const list_content = try self.mkListContent(elem_var, env);
                try self.unifyWith(pattern_var, list_content, env);

                // Then, check the "rest" pattern is bound to a variable
                // This is if the pattern is like `.. as x`
                if (list.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern_idx| {
                        const rest_pattern_var = try self.checkPatternHelp(rest_pattern_idx, env, out_var);

                        _ = try self.unify(pattern_var, rest_pattern_var, env);
                    }
                }
            }
        },
        // applied tag //
        .applied_tag => |applied_tag| {
            // Create a tag type in the type system and assign it the expr_var

            const arg_vars_slice = blk: {
                switch (comptime out_var) {
                    .fresh => {
                        const scratch_vars_top = self.scratch_vars.top();
                        defer self.scratch_vars.clearFrom(scratch_vars_top);

                        // Check tuple elements
                        const arg_ptrn_idx_slice = self.cir.store.slicePatterns(applied_tag.args);
                        for (arg_ptrn_idx_slice) |arg_expr_idx| {
                            const arg_var = try self.checkPatternHelp(arg_expr_idx, env, out_var);
                            try self.scratch_vars.append(arg_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },

                    .in_place => {
                        // Process each tag arg
                        const arg_ptrn_idx_slice = self.cir.store.slicePatterns(applied_tag.args);
                        for (arg_ptrn_idx_slice) |arg_expr_idx| {
                            _ = try self.checkPatternHelp(arg_expr_idx, env, out_var);
                        }

                        // Add to types store
                        // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
                        break :blk try self.types.appendVars(@ptrCast(arg_ptrn_idx_slice));
                    },
                }
            };

            // Create the type
            const ext_var = try self.fresh(env, pattern_region);

            const tag = types_mod.Tag{ .name = applied_tag.name, .args = arg_vars_slice };
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.unifyWith(pattern_var, tag_union_content, env);
        },
        // nominal //
        .nominal => |nominal| {
            // Check the backing pattern first
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, env, out_var);

            // Use shared nominal type checking logic
            _ = try self.checkNominalTypeUsage(
                pattern_var,
                actual_backing_var,
                ModuleEnv.varFrom(nominal.nominal_type_decl),
                nominal.backing_type,
                pattern_region,
                env,
            );
        },
        .nominal_external => |nominal| {
            // Check the backing pattern first
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, env, out_var);

            // Resolve the external type declaration
            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Use shared nominal type checking logic
                _ = try self.checkNominalTypeUsage(
                    pattern_var,
                    actual_backing_var,
                    ext_ref.local_var,
                    nominal.backing_type,
                    pattern_region,
                    env,
                );
            } else {
                try self.unifyWith(pattern_var, .err, env);
            }
        },
        // record destructure //
        .record_destructure => |destructure| {
            const scratch_records_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(scratch_records_top);

            var mb_ext_var: ?Var = null;

            for (self.cir.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                const destruct_var = ModuleEnv.varFrom(destruct_idx);
                try self.setVarRank(destruct_var, env);

                // Check the sub pattern
                const field_pattern_var = blk: {
                    switch (destruct.kind) {
                        .Required => |sub_pattern_idx| {
                            break :blk try self.checkPatternHelp(sub_pattern_idx, env, out_var);
                        },
                        .SubPattern => |sub_pattern_idx| {
                            break :blk try self.checkPatternHelp(sub_pattern_idx, env, out_var);
                        },
                        .Rest => |sub_pattern_idx| {
                            // If this pattern is rest pattern:
                            // eg { name, ...rest }
                            //               ^^^^
                            //
                            // Then capture this as the ext var, then  continue
                            const ext_var = try self.checkPatternHelp(sub_pattern_idx, env, out_var);
                            _ = try self.unify(destruct_var, ext_var, env);
                            mb_ext_var = ext_var;

                            continue;
                        },
                    }
                };

                // Set the destruct var to redirect to the field pattern var
                _ = try self.unify(destruct_var, field_pattern_var, env);

                // Append it to the scratch records array
                try self.scratch_record_fields.append(types_mod.RecordField{
                    .name = destruct.label,
                    .var_ = ModuleEnv.varFrom(destruct_var),
                });
            }

            // Copy the scratch record fields into the types store
            const record_fields_scratch = self.scratch_record_fields.sliceFromStart(scratch_records_top);
            std.mem.sort(types_mod.RecordField, record_fields_scratch, self.cir.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
            const record_fields_range = try self.types.appendRecordFields(record_fields_scratch);

            // Update the pattern var
            if (mb_ext_var) |ext| {
                try self.unifyWith(pattern_var, .{ .structure = .{
                    .record = .{
                        .fields = record_fields_range,
                        .ext = ext,
                    },
                } }, env);
            } else {
                try self.unifyWith(pattern_var, .{ .structure = .{
                    .record_unbound = record_fields_range,
                } }, env);
            }
        },
        // nums //
        .num_literal => |num| {
            // For unannotated literals (.num_unbound, .int_unbound), create a flex var with from_numeral constraint
            switch (num.kind) {
                .num_unbound, .int_unbound => {
                    // Create NumeralInfo for constraint checking
                    const num_literal_info = switch (num.value.kind) {
                        .u128 => types_mod.NumeralInfo.fromU128(@bitCast(num.value.bytes), false, pattern_region),
                        .i128 => types_mod.NumeralInfo.fromI128(num.value.toI128(), num.value.toI128() < 0, false, pattern_region),
                    };

                    // Create flex var with from_numeral constraint
                    const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                    _ = try self.unify(pattern_var, flex_var, env);
                },
                // Phase 5: For explicitly typed literals, use nominal types from Builtin
                .u8 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("U8", env), env),
                .i8 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("I8", env), env),
                .u16 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("U16", env), env),
                .i16 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("I16", env), env),
                .u32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("U32", env), env),
                .i32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("I32", env), env),
                .u64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("U64", env), env),
                .i64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("I64", env), env),
                .u128 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("U128", env), env),
                .i128 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("I128", env), env),
                .f32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("F32", env), env),
                .f64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("F64", env), env),
                .dec => try self.unifyWith(pattern_var, try self.mkNumberTypeContent("Dec", env), env),
            }
        },
        .frac_f32_literal => |_| {
            // Phase 5: Use nominal F32 type
            try self.unifyWith(pattern_var, try self.mkNumberTypeContent("F32", env), env);
        },
        .frac_f64_literal => |_| {
            // Phase 5: Use nominal F64 type
            try self.unifyWith(pattern_var, try self.mkNumberTypeContent("F64", env), env);
        },
        .dec_literal => |dec| {
            if (dec.has_suffix) {
                // Explicit suffix like `3.14dec` - use nominal Dec type
                try self.unifyWith(pattern_var, try self.mkNumberTypeContent("Dec", env), env);
            } else {
                // Unannotated decimal literal - create flex var with from_numeral constraint
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    dec.value.num, // RocDec has .num field which is i128 scaled by 10^18
                    dec.value.num < 0,
                    true, // Decimal literals are always fractional
                    pattern_region,
                );

                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(pattern_var, flex_var, env);
            }
        },
        .small_dec_literal => |dec| {
            if (dec.has_suffix) {
                // Explicit suffix - use nominal Dec type
                try self.unifyWith(pattern_var, try self.mkNumberTypeContent("Dec", env), env);
            } else {
                // Unannotated decimal literal - create flex var with from_numeral constraint
                // SmallDecValue stores a numerator (i16) and power of ten
                // We need to convert this to an i128 scaled by 10^18 for consistency
                const scaled_value = @as(i128, dec.value.numerator) * std.math.pow(i128, 10, 18 - dec.value.denominator_power_of_ten);
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    scaled_value,
                    dec.value.numerator < 0,
                    true,
                    pattern_region,
                );

                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(pattern_var, flex_var, env);
            }
        },
        .runtime_error => {
            try self.unifyWith(pattern_var, .err, env);
        },
    }

    return pattern_var;
}

fn getPatternIdent(self: *const Self, ptrn_idx: CIR.Pattern.Idx) ?Ident.Idx {
    const pattern = self.cir.store.getPattern(ptrn_idx);
    switch (pattern) {
        .assign => |assign| return assign.ident,
        else => return null,
    }
}

// expr //

fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));
    const expr_var_raw = ModuleEnv.varFrom(expr_idx);

    // Value restriction: only generalize at the inner lambda level, not the
    // outer e_closure wrapper (which delegates to e_lambda's own checkExpr).
    const should_generalize = isFunctionDef(&self.cir.store, expr) and expr != .e_closure;

    // Push/pop ranks based on if we should generalize
    if (should_generalize) try env.var_pool.pushRank();
    defer if (should_generalize) {
        // For an intermediate cycle participant's top-level lambda,
        // don't pop: rank and vars are preserved for the caller to
        // store and merge at the cycle root before generalization.
        // Inner lambdas (rank > outermost+1) always pop normally.
        const at_def_top_level = env.rank() == Rank.outermost.next();
        const is_cycle_root = if (self.cycle_root_def) |root_def|
            self.current_processing_def != null and root_def == self.current_processing_def.?
        else
            false;
        const is_intermediate = self.cycle_root_def != null and !is_cycle_root;

        if (is_intermediate and at_def_top_level) {
            // Don't pop — vars will be merged by cycle root.
        } else {
            env.var_pool.popRank();
        }
    };

    try self.setVarRank(expr_var_raw, env);

    // Generate the expr var and the annotation type vars
    //
    // If we have an annotation, then we create a fresh one, so if we hit an
    // error we don't poison the variable
    const expr_var: Var, const mb_anno_vars: ?AnnoVars = blk: {
        if (expr == .e_closure) {
            // Closures delegate to their inner lambda's checkExpr, which handles
            // annotation and generalization. Forward expected so the annotation
            // type is created at the lambda's rank.
            // (The e_closure-wraps-e_lambda invariant is asserted by isFunctionDef.)
            break :blk .{ expr_var_raw, null };
        }

        switch (expected) {
            .no_expectation => {
                break :blk .{ expr_var_raw, null };
            },
            .expected => |annotation_idx| {
                // Generate the type for the annotation
                try self.generateAnnotationType(annotation_idx, env);
                const anno_var = ModuleEnv.varFrom(annotation_idx);

                // Copy/paste the variable. This will be used if the expr errors to
                // preserve the type annotation for places that reference this def.
                const anno_var_backup = try self.instantiateVarOrphan(
                    anno_var,
                    env,
                    env.rank(),
                    .use_last_var,
                );

                break :blk .{
                    try self.fresh(env, expr_region),
                    .{ .anno_var = anno_var, .anno_var_backup = anno_var_backup },
                };
            },
        }
    };

    var does_fx = false; // Does this expression potentially perform any side effects?

    switch (expr) {
        // str //
        .e_str_segment => |_| {
            const str_var = try self.freshStr(env, expr_region);
            _ = try self.unify(expr_var, str_var, env);
        },
        .e_str => |str| {
            // Iterate over the string segments, checking each one
            const segment_expr_idx_slice = self.cir.store.sliceExpr(str.span);
            var did_err = false;
            for (segment_expr_idx_slice) |seg_expr_idx| {
                const seg_expr = self.cir.store.getExpr(seg_expr_idx);

                // String literal segments are already Str type
                switch (seg_expr) {
                    .e_str_segment => {
                        does_fx = try self.checkExpr(seg_expr_idx, env, .no_expectation) or does_fx;
                    },
                    else => {
                        does_fx = try self.checkExpr(seg_expr_idx, env, .no_expectation) or does_fx;
                        const seg_var = ModuleEnv.varFrom(seg_expr_idx);

                        // Interpolated expressions must be of type Str
                        const seg_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(seg_expr_idx));
                        const expected_str_var = try self.freshStr(env, seg_region);

                        const unify_result = try self.unify(expected_str_var, seg_var, env);
                        if (!unify_result.isOk()) {
                            // Unification failed - mark as error
                            try self.unifyWith(seg_var, .err, env);
                            did_err = true;
                        }
                    },
                }

                // Check if it errored (for non-interpolation segments)
                if (!did_err) {
                    const seg_var = ModuleEnv.varFrom(seg_expr_idx);
                    did_err = self.types.resolveVar(seg_var).desc.content == .err;
                }
            }

            if (did_err) {
                // If any segment errored, propagate that error to the root string
                try self.unifyWith(expr_var, .err, env);
            } else {
                // Otherwise, set the type of this expr to be nominal Str
                const str_var = try self.freshStr(env, expr_region);
                _ = try self.unify(expr_var, str_var, env);
            }
        },
        // nums //
        .e_num => |num| {
            switch (num.kind) {
                .num_unbound, .int_unbound => {
                    // For unannotated literals, create a flex var with from_numeral constraint
                    const num_literal_info = switch (num.value.kind) {
                        .u128 => types_mod.NumeralInfo.fromU128(@bitCast(num.value.bytes), false, expr_region),
                        .i128 => types_mod.NumeralInfo.fromI128(num.value.toI128(), num.value.toI128() < 0, false, expr_region),
                    };

                    // Create flex var with from_numeral constraint
                    const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                    _ = try self.unify(expr_var, flex_var, env);

                    const resolved = self.types.resolveVar(flex_var);
                    const constraint_range = resolved.desc.content.flex.constraints;
                    const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

                    // Record this literal for deferred validation during comptime eval
                    // Use cir.gpa since deferred_numeric_literals belongs to the ModuleEnv
                    _ = try self.cir.deferred_numeric_literals.append(self.cir.gpa, .{
                        .expr_idx = expr_idx,
                        .type_var = flex_var,
                        .constraint = constraint,
                        .region = expr_region,
                    });
                },
                .u8 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("U8", env), env),
                .i8 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("I8", env), env),
                .u16 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("U16", env), env),
                .i16 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("I16", env), env),
                .u32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("U32", env), env),
                .i32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("I32", env), env),
                .u64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("U64", env), env),
                .i64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("I64", env), env),
                .u128 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("U128", env), env),
                .i128 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("I128", env), env),
                .f32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("F32", env), env),
                .f64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent("F64", env), env),
                .dec => try self.unifyWith(expr_var, try self.mkNumberTypeContent("Dec", env), env),
            }
        },
        .e_frac_f32 => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent("F32", env), env);
            } else {
                // Unsuffixed fractional literal - create constrained flex var
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    @as(i128, @as(u32, @bitCast(frac.value))),
                    frac.value < 0,
                    true,
                    expr_region,
                );
                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);

                const resolved = self.types.resolveVar(flex_var);
                const constraint_range = resolved.desc.content.flex.constraints;
                const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

                _ = try self.cir.deferred_numeric_literals.append(self.cir.gpa, .{
                    .expr_idx = expr_idx,
                    .type_var = flex_var,
                    .constraint = constraint,
                    .region = expr_region,
                });
            }
        },
        .e_frac_f64 => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent("F64", env), env);
            } else {
                // Unsuffixed fractional literal - create constrained flex var
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    @as(i128, @as(u64, @bitCast(frac.value))),
                    frac.value < 0,
                    true,
                    expr_region,
                );
                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);

                const resolved = self.types.resolveVar(flex_var);
                const constraint_range = resolved.desc.content.flex.constraints;
                const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

                _ = try self.cir.deferred_numeric_literals.append(self.cir.gpa, .{
                    .expr_idx = expr_idx,
                    .type_var = flex_var,
                    .constraint = constraint,
                    .region = expr_region,
                });
            }
        },
        .e_dec => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent("Dec", env), env);
            } else {
                // Unsuffixed Dec literal - create constrained flex var
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    frac.value.num,
                    frac.value.num < 0,
                    true,
                    expr_region,
                );
                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);

                const resolved = self.types.resolveVar(flex_var);
                const constraint_range = resolved.desc.content.flex.constraints;
                const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

                _ = try self.cir.deferred_numeric_literals.append(self.cir.gpa, .{
                    .expr_idx = expr_idx,
                    .type_var = flex_var,
                    .constraint = constraint,
                    .region = expr_region,
                });
            }
        },
        .e_dec_small => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent("Dec", env), env);
            } else {
                // Unsuffixed small Dec literal - create constrained flex var
                // Scale the value to i128 representation
                const scaled_value = @as(i128, frac.value.numerator) * std.math.pow(i128, 10, 18 - frac.value.denominator_power_of_ten);
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    scaled_value,
                    scaled_value < 0,
                    true,
                    expr_region,
                );
                const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);

                const resolved = self.types.resolveVar(flex_var);
                const constraint_range = resolved.desc.content.flex.constraints;
                const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

                _ = try self.cir.deferred_numeric_literals.append(self.cir.gpa, .{
                    .expr_idx = expr_idx,
                    .type_var = flex_var,
                    .constraint = constraint,
                    .region = expr_region,
                });
            }
        },
        .e_typed_int => |typed_num| {
            // Typed integer literal like 123.U64
            // Create from_numeral constraint and unify with the explicit type
            const num_literal_info = switch (typed_num.value.kind) {
                .u128 => types_mod.NumeralInfo.fromU128(@bitCast(typed_num.value.bytes), false, expr_region),
                .i128 => types_mod.NumeralInfo.fromI128(typed_num.value.toI128(), typed_num.value.toI128() < 0, false, expr_region),
            };

            // Create flex var with from_numeral constraint
            const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);

            // Capture the constraint BEFORE unification (unification will change the content)
            const resolved = self.types.resolveVar(flex_var);
            const constraint_range = resolved.desc.content.flex.constraints;
            const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

            // Look up the explicit type name and unify with it
            const idents = self.cir.getIdentStoreConst();
            const type_name = idents.getText(typed_num.type_name);
            const type_content = try self.mkNumberTypeContent(type_name, env);
            try self.unifyWith(flex_var, type_content, env);

            // Unify expr_var with the flex_var (which is now constrained to the explicit type)
            _ = try self.unify(expr_var, flex_var, env);

            // Record for deferred validation during comptime eval
            _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
                .expr_idx = expr_idx,
                .type_var = flex_var,
                .constraint = constraint,
                .region = expr_region,
            });
        },
        .e_typed_frac => |typed_num| {
            // Typed fractional literal like 3.14.Dec
            // The value is stored as scaled i128 (like Dec)
            const num_literal_info = types_mod.NumeralInfo.fromI128(
                typed_num.value.toI128(),
                typed_num.value.toI128() < 0,
                true, // is_fractional
                expr_region,
            );

            // Create flex var with from_numeral constraint
            const flex_var = try self.mkFlexWithFromNumeralConstraint(num_literal_info, env);

            // Capture the constraint BEFORE unification (unification will change the content)
            const resolved = self.types.resolveVar(flex_var);
            const constraint_range = resolved.desc.content.flex.constraints;
            const constraint = self.types.sliceStaticDispatchConstraints(constraint_range)[0];

            // Look up the explicit type name and unify with it
            const idents_frac = self.cir.getIdentStoreConst();
            const type_name = idents_frac.getText(typed_num.type_name);
            const type_content = try self.mkNumberTypeContent(type_name, env);
            try self.unifyWith(flex_var, type_content, env);

            // Unify expr_var with the flex_var (which is now constrained to the explicit type)
            _ = try self.unify(expr_var, flex_var, env);

            // Record for deferred validation during comptime eval
            _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
                .expr_idx = expr_idx,
                .type_var = flex_var,
                .constraint = constraint,
                .region = expr_region,
            });
        },
        // list //
        .e_empty_list => {
            // Create a nominal List with a fresh unbound element type
            const elem_var = try self.fresh(env, expr_region);
            const list_content = try self.mkListContent(elem_var, env);
            try self.unifyWith(expr_var, list_content, env);
        },
        .e_list => |list| {
            const elems = self.cir.store.exprSlice(list.elems);

            if (elems.len == 0) {
                // Create a nominal List with a fresh unbound element type
                const elem_var = try self.fresh(env, expr_region);
                const list_content = try self.mkListContent(elem_var, env);
                try self.unifyWith(expr_var, list_content, env);
            } else {
                // Here, we use the list's 1st element as the element var to
                // constrain the rest of the list

                // Check the first elem
                does_fx = try self.checkExpr(elems[0], env, .no_expectation) or does_fx;

                // Iterate over the remaining elements
                const elem_var = ModuleEnv.varFrom(elems[0]);
                var last_elem_expr_idx = elems[0];
                for (elems[1..], 1..) |elem_expr_idx, i| {
                    does_fx = try self.checkExpr(elem_expr_idx, env, .no_expectation) or does_fx;
                    const cur_elem_var = ModuleEnv.varFrom(elem_expr_idx);

                    // Unify each element's var with the list's elem var
                    const result = try self.unifyInContext(elem_var, cur_elem_var, env, .{ .list_entry = .{
                        .elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_expr_idx),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            does_fx = try self.checkExpr(remaining_elem_expr_idx, env, .no_expectation) or does_fx;
                        }

                        // Break to avoid cascading errors
                        break;
                    }

                    last_elem_expr_idx = elem_expr_idx;
                }

                // Create a nominal List type with the inferred element type
                const list_content = try self.mkListContent(elem_var, env);
                try self.unifyWith(expr_var, list_content, env);
            }
        },
        // tuple //
        .e_tuple => |tuple| {
            // Check tuple elements
            const elems_slice = self.cir.store.exprSlice(tuple.elems);
            for (elems_slice) |single_elem_expr_idx| {
                does_fx = try self.checkExpr(single_elem_expr_idx, env, .no_expectation) or does_fx;
            }

            // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
            const elem_vars_slice = try self.types.appendVars(@ptrCast(elems_slice));

            // Set the type in the store
            try self.unifyWith(expr_var, .{ .structure = .{
                .tuple = .{ .elems = elem_vars_slice },
            } }, env);
        },
        .e_tuple_access => |tuple_access| {
            // Check the tuple expression
            does_fx = try self.checkExpr(tuple_access.tuple, env, .no_expectation) or does_fx;

            const tuple_var = ModuleEnv.varFrom(tuple_access.tuple);
            const resolved = self.types.resolveVar(tuple_var);

            switch (resolved.desc.content) {
                .structure => |s| switch (s) {
                    .tuple => |t| {
                        // Access the element at the given index
                        const elem_index = tuple_access.elem_index;
                        const elems = self.types.sliceVars(t.elems);
                        if (elem_index < elems.len) {
                            const elem_var = elems[elem_index];
                            _ = try self.unify(expr_var, elem_var, env);
                        } else {
                            // Index out of bounds
                            try self.unifyWith(expr_var, .err, env);
                        }
                    },
                    else => {
                        // Not a tuple - create a flex var with expected tuple constraint
                        // The elem_index + 1 gives us the minimum tuple size needed
                        const min_elems = tuple_access.elem_index + 1;
                        const scratch_vars_top = self.scratch_vars.top();
                        defer self.scratch_vars.clearFrom(scratch_vars_top);

                        for (0..min_elems) |_| {
                            const fresh_var = try self.fresh(env, expr_region);
                            try self.scratch_vars.append(fresh_var);
                        }
                        const elem_vars = try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));

                        const expected_tuple_var = try self.freshFromContent(.{ .structure = .{
                            .tuple = .{ .elems = elem_vars },
                        } }, env, expr_region);

                        _ = try self.unify(tuple_var, expected_tuple_var, env);

                        // The result type is the element at the index
                        const result_var = self.types.sliceVars(elem_vars)[tuple_access.elem_index];
                        _ = try self.unify(expr_var, result_var, env);
                    },
                },
                .flex => {
                    // The tuple is still a flex var - create tuple constraint
                    const min_elems = tuple_access.elem_index + 1;
                    const scratch_vars_top = self.scratch_vars.top();
                    defer self.scratch_vars.clearFrom(scratch_vars_top);

                    for (0..min_elems) |_| {
                        const fresh_var = try self.fresh(env, expr_region);
                        try self.scratch_vars.append(fresh_var);
                    }
                    const elem_vars = try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));

                    const expected_tuple_var = try self.freshFromContent(.{ .structure = .{
                        .tuple = .{ .elems = elem_vars },
                    } }, env, expr_region);

                    _ = try self.unify(tuple_var, expected_tuple_var, env);

                    // The result type is the element at the index
                    const result_var = self.types.sliceVars(elem_vars)[tuple_access.elem_index];
                    _ = try self.unify(expr_var, result_var, env);
                },
                .err => {
                    // Propagate error
                    try self.unifyWith(expr_var, .err, env);
                },
                else => {
                    // Not a tuple
                    try self.unifyWith(expr_var, .err, env);
                },
            }
        },
        // record //
        .e_record => |e| {
            // Check if this is a record update
            if (e.ext) |record_being_updated_expr| {
                // Create a record type in the type system and assign it the expr_var

                // Check the record being updated
                does_fx = try self.checkExpr(record_being_updated_expr, env, .no_expectation) or does_fx;

                const record_being_updated_var = ModuleEnv.varFrom(record_being_updated_expr);
                const record_being_updated_name: ?Ident.Idx = self.getExprPatternIdent(record_being_updated_expr);

                // Process each field
                for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
                    const field = self.cir.store.getRecordField(field_idx);

                    // Check the field value expression
                    does_fx = try self.checkExpr(field.value, env, .no_expectation) or does_fx;

                    // Create an unbound record with this field
                    const single_field_record = try self.freshFromContent(.{ .structure = .{
                        .record_unbound = try self.types.appendRecordFields(&.{types_mod.RecordField{
                            .name = field.name,
                            .var_ = ModuleEnv.varFrom(field.value),
                        }}),
                    } }, env, expr_region);

                    // Unify this record update with the record we're updating
                    _ = try self.unifyInContext(record_being_updated_var, single_field_record, env, .{ .record_update = .{
                        .field_name = field.name,
                        .field_region_idx = @enumFromInt(@intFromEnum(field.value)),
                        .record_region_idx = @enumFromInt(@intFromEnum(record_being_updated_var)),
                        .record_name = record_being_updated_name,
                    } });
                }

                // Then unify with the actual expression
                _ = try self.unify(record_being_updated_var, expr_var, env);
            } else {
                // Write down the top of the scratch records array
                const record_fields_top = self.scratch_record_fields.top();
                defer self.scratch_record_fields.clearFrom(record_fields_top);

                // Process each field
                for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
                    const field = self.cir.store.getRecordField(field_idx);

                    // Check the field value expression
                    does_fx = try self.checkExpr(field.value, env, .no_expectation) or does_fx;

                    // Append it to the scratch records array
                    try self.scratch_record_fields.append(types_mod.RecordField{
                        .name = field.name,
                        .var_ = ModuleEnv.varFrom(field.value),
                    });
                }

                // Copy the scratch fields into the types store
                const record_fields_scratch = self.scratch_record_fields.sliceFromStart(record_fields_top);
                std.mem.sort(types_mod.RecordField, record_fields_scratch, self.cir.getIdentStore(), comptime types_mod.RecordField.sortByNameAsc);
                const record_fields_range = try self.types.appendRecordFields(record_fields_scratch);

                // Create an unbound record with the provided fields
                const ext_var = try self.freshFromContent(.{ .structure = .empty_record }, env, expr_region);
                try self.unifyWith(expr_var, .{ .structure = .{ .record = .{
                    .fields = record_fields_range,
                    .ext = ext_var,
                } } }, env);
            }
        },
        .e_empty_record => {
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        // tags //
        .e_zero_argument_tag => |e| {
            const ext_var = try self.fresh(env, expr_region);

            const tag = try self.types.mkTag(e.name, &.{});
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.unifyWith(expr_var, tag_union_content, env);
        },
        .e_tag => |e| {
            // Create a tag type in the type system and assign it the expr_var

            // Process each tag arg
            const arg_expr_idx_slice = self.cir.store.sliceExpr(e.args);
            for (arg_expr_idx_slice) |arg_expr_idx| {
                does_fx = try self.checkExpr(arg_expr_idx, env, .no_expectation) or does_fx;
            }

            // Create the type
            const ext_var = try self.fresh(env, expr_region);

            const tag = try self.types.mkTag(e.name, @ptrCast(arg_expr_idx_slice));
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.unifyWith(expr_var, tag_union_content, env);
        },
        // nominal //
        .e_nominal => |nominal| {
            // Check the backing expression first
            does_fx = try self.checkExpr(nominal.backing_expr, env, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            // Use shared nominal type checking logic
            _ = try self.checkNominalTypeUsage(
                expr_var,
                actual_backing_var,
                ModuleEnv.varFrom(nominal.nominal_type_decl),
                nominal.backing_type,
                expr_region,
                env,
            );
        },
        .e_nominal_external => |nominal| {
            // Check the backing expression first
            does_fx = try self.checkExpr(nominal.backing_expr, env, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            // Resolve the external type declaration
            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Use shared nominal type checking logic
                _ = try self.checkNominalTypeUsage(
                    expr_var,
                    actual_backing_var,
                    ext_ref.local_var,
                    nominal.backing_type,
                    expr_region,
                    env,
                );
            } else {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        // lookup //
        .e_lookup_local => |lookup| blk: {
            const pat_var = ModuleEnv.varFrom(lookup.pattern_idx);

            const mb_processing_def = self.top_level_ptrns.get(lookup.pattern_idx);
            if (mb_processing_def) |processing_def| {
                switch (processing_def.status) {
                    .not_processed => {
                        var sub_env = try self.env_pool.acquire();
                        errdefer self.env_pool.release(sub_env);

                        // Push through to top_level
                        try sub_env.var_pool.pushRank();
                        std.debug.assert(sub_env.rank() == .outermost);

                        try self.checkDef(processing_def.def_idx, &sub_env);

                        if (self.defer_generalize) {
                            std.debug.assert(self.cycle_root_def != null);

                            // Cycle detected: store env for merge at cycle root.
                            try self.deferred_cycle_envs.append(self.gpa, sub_env);

                            // Use the def's closure/expr var directly. After
                            // checkDef, e_closure rank elevation has already run,
                            // so the closure var is at rank 2 — safe for
                            // unification without pulling body vars below the
                            // generalization rank.
                            const def = self.cir.store.getDef(processing_def.def_idx);
                            const def_expr_var = ModuleEnv.varFrom(def.expr);
                            _ = try self.unify(expr_var, def_expr_var, env);

                            break :blk;
                        } else {
                            std.debug.assert(sub_env.rank() == .outermost);
                            self.env_pool.release(sub_env);
                        }
                    },
                    .processing => {
                        // This is a recursive reference
                        //
                        // In this case, we simply assign the pattern to be a
                        // flex var, then write down an eql constraint for later
                        // validation. This deferred approach is necessary for
                        // good error messages.

                        // Assert that this def is NOT generalized nor outermost
                        std.debug.assert(self.types.resolveVar(pat_var).desc.rank != .generalized);

                        // Set the expr to be a flex
                        try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);

                        // Write down this constraint for later validation
                        _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
                            .expected = pat_var,
                            .actual = expr_var,
                            .ctx = .{ .recursive_def = .{ .def_name = processing_def.def_name } },
                        } });

                        // Detect mutual recursion through local lookups.
                        // If the referenced def is different from the current one,
                        // we have a cycle: current → ... → this_def → ... → current.
                        // Only trigger deferred generalization for function defs
                        // (closures/lambdas), since only they are generalized and
                        // have the cycle root cleanup code in their checkExpr.
                        // Non-closure circular refs (e.g. associated item values)
                        // are handled by the eql constraint above.
                        if (self.current_processing_def) |current_def| {
                            if (current_def != processing_def.def_idx) {
                                const ref_def = self.cir.store.getDef(processing_def.def_idx);
                                if (isFunctionDef(&self.cir.store, self.cir.store.getExpr(ref_def.expr))) {
                                    if (self.cycle_root_def == null) {
                                        // First cycle detection: no prior cycle should be in progress.
                                        std.debug.assert(!self.defer_generalize);
                                        std.debug.assert(self.deferred_cycle_envs.items.len == 0);
                                        std.debug.assert(self.deferred_def_unifications.items.len == 0);
                                        self.cycle_root_def = processing_def.def_idx;
                                    }
                                    self.defer_generalize = true;
                                }
                            }
                        }

                        break :blk;
                    },
                    .processed => {},
                }
            }

            // Instantiate if generalized, otherwise just use the pattern var
            const resolved_pat = self.types.resolveVar(pat_var);
            if (resolved_pat.desc.rank == Rank.generalized) {
                const instantiated = try self.instantiateVar(pat_var, env, .use_last_var);
                _ = try self.unify(expr_var, instantiated, env);
            } else {
                _ = try self.unify(expr_var, pat_var, env);
            }
        },
        .e_lookup_external => |ext| {
            // With WaitingForDependencies phase, dependencies are guaranteed to be Done
            // before canonicalization, so target_node_idx is always valid.
            if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                const ext_instantiated_var = try self.instantiateVar(
                    ext_ref.local_var,
                    env,
                    .{ .explicit = expr_region },
                );
                _ = try self.unify(expr_var, ext_instantiated_var, env);
            } else {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_lookup_pending => {
            // Pending lookups should normally be resolved before type-checking.
            // However, if an import references a non-existent package shorthand
            // (e.g., "import f.S" where "f" is not defined), the pending lookup
            // cannot be resolved because there's no target module to look up from.
            // In this case, treat it as an error type - the user will get an
            // error about the unresolved identifier elsewhere.
            try self.unifyWith(expr_var, .err, env);
        },
        .e_lookup_required => |req| {
            // Look up the type from the platform's requires clause
            const requires_items = self.cir.requires_types.items.items;
            const idx = req.requires_idx.toU32();
            if (idx < requires_items.len) {
                const required_type = requires_items[idx];
                const type_var = ModuleEnv.varFrom(required_type.type_anno);
                const instantiated_var = try self.instantiateVar(
                    type_var,
                    env,
                    .{ .explicit = expr_region },
                );
                _ = try self.unify(expr_var, instantiated_var, env);
            } else {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        // block //
        .e_block => |block| {
            // Check all statements in the block
            const statements = self.cir.store.sliceStatements(block.stmts);
            const stmt_result = try self.checkBlockStatements(statements, env, expr_region);
            does_fx = stmt_result.does_fx or does_fx;

            // Check the final expression
            does_fx = try self.checkExpr(block.final_expr, env, .no_expectation) or does_fx;

            // If the block diverges (has a return/crash), use a flex var for the block's type
            // since the final expression is unreachable
            if (stmt_result.diverges) {
                try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
            } else {
                // Link the root expr with the final expr
                _ = try self.unify(expr_var, ModuleEnv.varFrom(block.final_expr), env);
            }
        },
        // function //
        .e_lambda => |lambda| {
            // Then, even if we have an expected type, it may not actually be a function
            const mb_anno_func: ?types_mod.Func = blk: {
                if (mb_anno_vars) |anno_vars| {
                    // Here, we unwrap the function, following aliases, to get
                    // the actual function we want to check against
                    var var_ = anno_vars.anno_var;
                    var guard = types_mod.debug.IterationGuard.init("checkExpr.lambda.unwrapExpectedFunc");
                    while (true) {
                        guard.tick();
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
            // This must happen *before* checking against the expected type so
            // all the pattern types are inferred
            const arg_pattern_idxs = self.cir.store.slicePatterns(lambda.args);
            for (arg_pattern_idxs) |pattern_idx| {
                try self.checkPattern(pattern_idx, env);
            }

            // Now, check if we have an expected function to validate against
            if (mb_anno_func) |anno_func| {
                // Use index-based iteration instead of slices because unifyInContext
                // may trigger reallocations that would invalidate slice pointers
                const anno_func_args_range = anno_func.args;
                const anno_func_args_len = anno_func_args_range.len();

                // Next, check if the arguments arities match
                if (anno_func_args_len == arg_pattern_idxs.len) {
                    // If so, check each argument, passing in the expected type

                    // First, find all the rigid variables in a the function's type
                    // and unify the matching corresponding lambda arguments together.
                    for (0..anno_func_args_len) |i| {
                        const anno_arg_1 = self.types.getVarAt(anno_func_args_range, @intCast(i));
                        const anno_resolved_1 = self.types.resolveVar(anno_arg_1);

                        // The expected type is an annotation and as such,
                        // should never contain a flex var. If it did, that
                        // would indicate that the annotation is malformed
                        // std.debug.assert(expected_resolved_1.desc.content != .flex);

                        // Skip any concrete arguments
                        if (anno_resolved_1.desc.content != .rigid) {
                            continue;
                        }

                        // Look for other arguments with the same type variable
                        for (i + 1..anno_func_args_len) |j| for_blk: {
                            const anno_arg_2 = self.types.getVarAt(anno_func_args_range, @intCast(j));
                            const anno_resolved_2 = self.types.resolveVar(anno_arg_2);
                            if (anno_resolved_1.var_ == anno_resolved_2.var_) {
                                // These two argument indexes in the called *function's*
                                // type have the same rigid variable! So, we unify
                                // the corresponding *lambda args*

                                const arg_1 = @as(Var, ModuleEnv.varFrom(arg_pattern_idxs[i]));
                                const arg_2 = @as(Var, ModuleEnv.varFrom(arg_pattern_idxs[j]));

                                const unify_result = try self.unifyInContext(arg_1, arg_2, env, .{
                                    .fn_args_bound_var = .{
                                        .fn_name = self.enclosing_func_name,
                                        .first_arg_var = arg_1,
                                        .second_arg_var = arg_2,
                                        .first_arg_index = @intCast(i),
                                        .second_arg_index = @intCast(j),
                                        .num_args = @intCast(arg_pattern_idxs.len),
                                    },
                                });
                                if (unify_result.isProblem()) {
                                    // Context already set by unifyInContext
                                    // Stop execution
                                    _ = try self.unifyWith(expr_var, .err, env);
                                    break :for_blk;
                                }
                            }
                        }
                    }

                    // Then, lastly, we unify the annotation types against the
                    // actual type
                    for (arg_pattern_idxs, 0..) |pattern_idx, i| {
                        const expected_arg_var = self.types.getVarAt(anno_func_args_range, @intCast(i));
                        _ = try self.unifyInContext(expected_arg_var, ModuleEnv.varFrom(pattern_idx), env, .type_annotation);
                    }
                } else {
                    // This means the expected type and the actual lambda have
                    // an arity mismatch. This will be caught by the regular
                    // expectation checking code at the bottom of this function
                }
            }

            const arg_vars: []Var = @ptrCast(arg_pattern_idxs);
            const body_var = ModuleEnv.varFrom(lambda.body);

            // Check the the body of the expr
            // If we have an expected function, use that as the expr's expected type
            if (mb_anno_func) |expected_func| {
                does_fx = try self.checkExpr(lambda.body, env, .no_expectation) or does_fx;
                _ = try self.unifyInContext(expected_func.ret, body_var, env, .type_annotation);
            } else {
                does_fx = try self.checkExpr(lambda.body, env, .no_expectation) or does_fx;
            }

            // Process any pending return constraints (from early returns / ? operator) before
            // creating the function type. This must happen after the body is fully checked
            // (for correct error reporting) but before the function type is generalized
            // (so instantiated copies at call sites have the complete type, including
            // both Ok and Err variants from the ? operator).
            // Only processes early_return/try_suffix_return constraints; anonymous
            // constraints (e.g. from recursive lookups) are left for later.
            try self.processReturnConstraints(env);

            // Create the function type
            if (does_fx) {
                _ = try self.unifyWith(expr_var, try self.types.mkFuncEffectful(arg_vars, body_var), env);
            } else {
                _ = try self.unifyWith(expr_var, try self.types.mkFuncUnbound(arg_vars, body_var), env);
            }

            // Note that so far, we have not yet unified against the
            // annotation's effectfulness/pureness. This is intentional!
            // Below this large switch statement, there's the regular expr
            // <-> expected unification. This will catch any difference in
            // effectfullness, and it'll link the root expected var with the
            // expr_var

        },
        .e_closure => |closure| {
            // Here, we must forward the expected valued to the inner lambda, so
            // the annotation type is created at the same rank as the expr
            does_fx = try self.checkExpr(closure.lambda_idx, env, expected) or does_fx;
            const lambda_var = ModuleEnv.varFrom(closure.lambda_idx);

            // For intermediate cycle participants, the inner lambda skipped
            // generalization and kept its rank (2). The closure var was set
            // at the outer rank (1) before the lambda pushed. Elevate the
            // closure var to match so unification doesn't pull to min(1,2)=1,
            // which would prevent generalization at the cycle root.
            const lambda_rank = self.types.resolveVar(lambda_var).desc.rank;
            if (lambda_rank != .generalized) {
                const expr_resolved = self.types.resolveVar(expr_var);
                if (@intFromEnum(lambda_rank) > @intFromEnum(expr_resolved.desc.rank)) {
                    // Elevation only fires for intermediate cycle participants
                    // whose lambda skipped generalization (kept rank 2). In the
                    // non-cycle case, the lambda is generalized (rank 0) so we
                    // never enter this branch.
                    std.debug.assert(self.defer_generalize);
                    self.types.setDescRank(expr_resolved.desc_idx, lambda_rank);
                }
            }

            _ = try self.unify(expr_var, lambda_var, env);
        },
        // function calling //
        .e_call => |call| {
            switch (call.called_via) {
                .apply => blk: {
                    // First, check the function being called
                    // It could be effectful, e.g. `(mk_fn!())(arg)`
                    does_fx = try self.checkExpr(call.func, env, .no_expectation) or does_fx;
                    const func_var = ModuleEnv.varFrom(call.func);

                    // Resolve the func var
                    const resolved_func = self.types.resolveVar(func_var).desc.content;
                    var did_err = resolved_func == .err;

                    // Second, check the arguments being called
                    // It could be effectful, e.g. `fn(mk_arg!())`
                    const call_arg_expr_idxs = self.cir.store.sliceExpr(call.args);
                    for (call_arg_expr_idxs) |call_arg_idx| {
                        does_fx = try self.checkExpr(call_arg_idx, env, .no_expectation) or does_fx;

                        // Check if this arg errored
                        did_err = did_err or (self.types.resolveVar(ModuleEnv.varFrom(call_arg_idx)).desc.content == .err);
                    }

                    if (did_err) {
                        // If the fn or any args had error, propagate the error
                        // without doing any additional work
                        try self.unifyWith(expr_var, .err, env);
                    } else {
                        // From the base function type, extract the actual function info
                        // and also track whether the function is effectful
                        const FuncInfo = struct { func: types_mod.Func, is_effectful: bool };
                        const mb_func_info: ?FuncInfo = inner_blk: {
                            // Here, we unwrap the function, following aliases, to get
                            // the actual function we want to check against
                            var var_ = func_var;
                            var guard = types_mod.debug.IterationGuard.init("checkExpr.call.unwrapFuncVar");
                            while (true) {
                                guard.tick();
                                switch (self.types.resolveVar(var_).desc.content) {
                                    .structure => |flat_type| {
                                        switch (flat_type) {
                                            .fn_pure => |func| break :inner_blk FuncInfo{ .func = func, .is_effectful = false },
                                            .fn_unbound => |func| break :inner_blk FuncInfo{ .func = func, .is_effectful = false },
                                            .fn_effectful => |func| break :inner_blk FuncInfo{ .func = func, .is_effectful = true },
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
                        const mb_func = if (mb_func_info) |info| info.func else null;

                        // If the function being called is effectful, mark this expression as effectful
                        if (mb_func_info) |info| {
                            if (info.is_effectful) {
                                does_fx = true;
                            }
                        }

                        // Get the name of the function (for error messages)
                        const func_name: ?Ident.Idx = self.getExprPatternIdent(call.func);

                        // Now, check the call args against the type of function
                        if (mb_func) |func| {
                            // Use index-based iteration instead of slices because unifyInContext
                            // may trigger reallocations that would invalidate slice pointers
                            const func_args_range = func.args;
                            const func_args_len = func_args_range.len();

                            if (func_args_len == call_arg_expr_idxs.len) {
                                // First, find all the "rigid" variables in a the function's type
                                // and unify the matching corresponding call arguments together.
                                //
                                // Here, "rigid" is in quotes because at this point, the expected function
                                // has been instantiated such that the rigid variables should all resolve
                                // to the same exact flex variable. So we are actually checking for flex
                                // variables here.
                                for (0..func_args_len) |i| {
                                    const expected_arg_1 = self.types.getVarAt(func_args_range, @intCast(i));
                                    const expected_resolved_1 = self.types.resolveVar(expected_arg_1);

                                    // Ensure the above comment is true. That is, that all
                                    // rigid vars for this function have been instantiated to
                                    // flex vars by the time we get here.
                                    // std.debug.assert(expected_resolved_1.desc.content != .rigid);

                                    // Skip any concrete arguments
                                    if (expected_resolved_1.desc.content != .flex and expected_resolved_1.desc.content != .rigid) {
                                        continue;
                                    }

                                    // Look for other arguments with the same type variable
                                    for (i + 1..func_args_len) |j| {
                                        const expected_arg_2 = self.types.getVarAt(func_args_range, @intCast(j));
                                        const expected_resolved_2 = self.types.resolveVar(expected_arg_2);
                                        if (expected_resolved_1.var_ == expected_resolved_2.var_) {
                                            // These two argument indexes in the called *function's*
                                            // type have the same rigid variable! So, we unify
                                            // the corresponding *call args*

                                            const arg_1 = @as(Var, ModuleEnv.varFrom(call_arg_expr_idxs[i]));
                                            const arg_2 = @as(Var, ModuleEnv.varFrom(call_arg_expr_idxs[j]));

                                            const unify_result = try self.unifyInContext(arg_1, arg_2, env, .{
                                                .fn_args_bound_var = .{
                                                    .fn_name = func_name,
                                                    .first_arg_var = arg_1,
                                                    .second_arg_var = arg_2,
                                                    .first_arg_index = @intCast(i),
                                                    .second_arg_index = @intCast(j),
                                                    .num_args = @intCast(call_arg_expr_idxs.len),
                                                },
                                            });
                                            if (unify_result.isProblem()) {
                                                // Context already set by unifyInContext
                                                // Stop execution
                                                _ = try self.unifyWith(expr_var, .err, env);
                                                break :blk;
                                            }
                                        }
                                    }
                                }

                                // Check the function's arguments against the actual
                                // called arguments, unifying each one
                                for (call_arg_expr_idxs, 0..) |call_expr_idx, arg_index| {
                                    const expected_arg_var = self.types.getVarAt(func_args_range, @intCast(arg_index));
                                    const unify_result = try self.unifyInContext(expected_arg_var, ModuleEnv.varFrom(call_expr_idx), env, .{ .fn_call_arg = .{
                                        .fn_name = func_name,
                                        .call_expr = expr_idx,
                                        .arg_index = @intCast(arg_index),
                                        .num_args = @intCast(call_arg_expr_idxs.len),
                                        .arg_var = ModuleEnv.varFrom(call_expr_idx),
                                    } });
                                    if (unify_result.isProblem()) {
                                        // Stop execution
                                        _ = try self.unifyWith(expr_var, .err, env);
                                        break :blk;
                                    }
                                }

                                // Redirect the expr to the function's return type
                                _ = try self.unify(expr_var, func.ret, env);
                            } else {
                                // We get here, then the arity of the function
                                // being called and the callsite do not match.
                                // This means it's a  regular type mismatch

                                // In this case, we fall back to a regular
                                // mismatch to show the actual vs expected, and
                                // allow the problem reporting  hint mechanism
                                // to add some context

                                const call_arg_vars: []Var = @ptrCast(call_arg_expr_idxs);
                                const call_func_ret = try self.fresh(env, expr_region);
                                const call_func_content = try self.types.mkFuncUnbound(call_arg_vars, call_func_ret);
                                const call_func_var = try self.freshFromContent(call_func_content, env, expr_region);

                                _ = try self.unifyInContext(func_var, call_func_var, env, .{ .fn_call_arity = .{
                                    .fn_name = func_name,
                                    .expected_args = @intCast(func_args_len),
                                    .actual_args = @intCast(call_arg_expr_idxs.len),
                                } });

                                // Then, we set the root expr to redirect to the return
                                // type of that function, since a call expr ultimate
                                // resolve to the  returned type
                                _ = try self.unify(expr_var, call_func_ret, env);
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
                            const call_func_ret = try self.fresh(env, expr_region);
                            const call_func_content = try self.types.mkFuncUnbound(call_arg_vars, call_func_ret);
                            const call_func_var = try self.freshFromContent(call_func_content, env, expr_region);

                            _ = try self.unify(func_var, call_func_var, env);

                            // Then, we set the root expr to redirect to the return
                            // type of that function, since a call expr ultimate
                            // resolve to the  returned type
                            _ = try self.unify(expr_var, call_func_ret, env);
                        }
                    }
                },
                else => {
                    // The canonicalizer currently only produces CalledVia.apply for e_call expressions.
                    // Other call types (binop, unary_op, string_interpolation, record_builder) are
                    // represented as different expression types. If we hit this, there's a compiler bug.
                    std.debug.assert(false);
                    try self.unifyWith(expr_var, .err, env);
                },
            }
        },
        .e_if => |if_expr| {
            does_fx = try self.checkIfElseExpr(expr_idx, expr_region, env, if_expr) or does_fx;
        },
        .e_match => |match| {
            does_fx = try self.checkMatchExpr(expr_idx, env, match) or does_fx;
        },
        .e_binop => |binop| {
            does_fx = try self.checkBinopExpr(expr_idx, expr_region, env, binop) or does_fx;
        },
        .e_unary_minus => |unary| {
            does_fx = try self.checkUnaryMinusExpr(expr_idx, expr_region, env, unary) or does_fx;
        },
        .e_unary_not => |unary| {
            does_fx = try self.checkUnaryNotExpr(expr_idx, expr_region, env, unary) or does_fx;
        },
        .e_dot_access => |dot_access| {
            // Dot access can either indicate record access or static dispatch

            // Check the receiver expression
            // E.g. thing.val
            //      ^^^^^
            does_fx = try self.checkExpr(dot_access.receiver, env, .no_expectation) or does_fx;
            const receiver_var = ModuleEnv.varFrom(dot_access.receiver);

            if (dot_access.args) |dispatch_args| {
                // If this dot access has args, then it's static dispatch

                // Resolve the receiver var
                const resolved_receiver = self.types.resolveVar(receiver_var);
                var did_err = resolved_receiver.desc.content == .err;

                // Check the args
                // E.g. thing.dispatch(a, b)
                //                     ^  ^
                const dispatch_arg_expr_idxs = self.cir.store.sliceExpr(dispatch_args);
                for (dispatch_arg_expr_idxs) |dispatch_arg_expr_idx| {
                    does_fx = try self.checkExpr(dispatch_arg_expr_idx, env, .no_expectation) or does_fx;

                    // Check if this arg errored
                    did_err = did_err or (self.types.resolveVar(ModuleEnv.varFrom(dispatch_arg_expr_idx)).desc.content == .err);
                }

                if (did_err) {
                    // If the receiver or any arguments are errors, then
                    // propagate the error without doing any static dispatch work
                    try self.unifyWith(expr_var, .err, env);
                } else {
                    // For static dispatch to be used like `thing.dispatch(...)` the
                    // method being dispatched on must accept the type of `thing` as
                    // it's first arg. So, we prepend the `receiver_var` to the args list
                    const first_arg_range = try self.types.appendVars(&.{receiver_var});
                    const rest_args_range = try self.types.appendVars(@ptrCast(dispatch_arg_expr_idxs));
                    const dispatch_arg_vars_range = Var.SafeList.Range{
                        .start = first_arg_range.start,
                        .count = rest_args_range.count + 1,
                    };

                    // Since the return type of this dispatch is unknown, create a
                    // flex to represent it
                    const dispatch_ret_var = try self.fresh(env, expr_region);

                    // Now, create the function being dispatched
                    // Use field_name_region so error messages point at the method name, not the whole expression
                    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                        .args = dispatch_arg_vars_range,
                        .ret = dispatch_ret_var,
                        .needs_instantiation = false,
                    } } }, env, dot_access.field_name_region);

                    // Then, create the static dispatch constraint
                    const constraint = StaticDispatchConstraint{
                        .fn_name = dot_access.field_name,
                        .fn_var = constraint_fn_var,
                        .origin = .method_call,
                    };
                    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

                    // Create our constrained flex, and unify it with the receiver
                    // Use field_name_region so error messages point at the method name, not the whole expression
                    const constrained_var = try self.freshFromContent(
                        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
                        env,
                        dot_access.field_name_region,
                    );

                    _ = try self.unify(constrained_var, receiver_var, env);

                    // Then, set the root expr to redirect to the ret var
                    _ = try self.unify(expr_var, dispatch_ret_var, env);
                }
            } else {
                // Otherwise, this is dot access on a record

                // Create a type for the inferred type of this record access
                // E.g. foo.bar -> { bar: flex } a
                const record_field_var = try self.fresh(env, expr_region);
                const record_field_range = try self.types.appendRecordFields(&.{types_mod.RecordField{
                    .name = dot_access.field_name,
                    .var_ = record_field_var,
                }});
                const record_ext_var = try self.fresh(env, expr_region);
                const record_being_accessed = try self.freshFromContent(.{ .structure = .{
                    .record = .{ .fields = record_field_range, .ext = record_ext_var },
                } }, env, expr_region);

                // Then, unify the actual receiver type with the expected record
                _ = try self.unifyInContext(record_being_accessed, receiver_var, env, .{ .record_access = .{
                    .field_name = dot_access.field_name,
                    .field_region = dot_access.field_name_region,
                } });
                _ = try self.unify(expr_var, record_field_var, env);
            }
        },
        .e_crash => {
            try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
        },
        .e_dbg => |dbg| {
            // dbg evaluates its inner expression but returns {} (like expect)
            _ = try self.checkExpr(dbg.expr, env, .no_expectation);
            does_fx = true;
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_expect => |expect| {
            does_fx = try self.checkExpr(expect.body, env, expected) or does_fx;
            const body_var = ModuleEnv.varFrom(expect.body);

            const bool_var = try self.freshBool(env, expr_region);
            _ = try self.unifyInContext(bool_var, body_var, env, .expect);

            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_for => |for_expr| {
            // Check the pattern
            try self.checkPattern(for_expr.patt, env);
            const for_ptrn_var: Var = ModuleEnv.varFrom(for_expr.patt);

            // Check the list expression
            does_fx = try self.checkExpr(for_expr.expr, env, .no_expectation) or does_fx;
            const for_expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(for_expr.expr));
            const for_expr_var: Var = ModuleEnv.varFrom(for_expr.expr);

            // Check that the expr is list of the ptrn
            const list_content = try self.mkListContent(for_ptrn_var, env);
            const list_var = try self.freshFromContent(list_content, env, for_expr_region);
            _ = try self.unify(list_var, for_expr_var, env);

            // Check the body
            does_fx = try self.checkExpr(for_expr.body, env, .no_expectation) or does_fx;
            const for_body_var: Var = ModuleEnv.varFrom(for_expr.body);

            // Check that the for body evaluates to {}
            const body_ret = try self.freshFromContent(.{ .structure = .empty_record }, env, for_expr_region);
            _ = try self.unify(body_ret, for_body_var, env);

            // The for expression itself evaluates to {}
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_ellipsis => {
            try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
        },
        .e_anno_only => {
            // For annotation-only expressions, the type comes from the annotation.
            // This case should only occur when the expression has an annotation (which is
            // enforced during canonicalization), so the expected type should be set.
            switch (expected) {
                .no_expectation => {
                    // This shouldn't happen since we always create e_anno_only with an annotation
                    try self.unifyWith(expr_var, .err, env);
                },
                .expected => |_| {
                    // The expr will be unified with the expected type below
                    // expr_var is a flex var by default, so no action is need here
                },
            }
        },
        .e_return => |ret| {
            does_fx = try self.checkExpr(ret.expr, env, .no_expectation) or does_fx;
            const ret_var = ModuleEnv.varFrom(ret.expr);

            // Write down this constraint for later validation.
            // We assert the lambda's body type and the return value type are equivalent.
            // This constraint is processed at the end of e_lambda (after the body is
            // fully checked) to ensure proper error reporting while also running before
            // generalization to prevent layout mismatches at instantiated call sites.
            const lambda_expr = self.cir.store.getExpr(ret.lambda);
            std.debug.assert(lambda_expr == .e_lambda);
            _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
                .expected = ModuleEnv.varFrom(lambda_expr.e_lambda.body),
                .actual = ret_var,
                .ctx = switch (ret.context) {
                    .return_expr => .early_return,
                    .try_suffix => .try_operator,
                },
            } });

            // Note that we DO NOT unify the return type with the expr here.
            // This is so this expr can unify with anything (like {} in the an implicit `else` branch)
        },
        .e_hosted_lambda => {
            // For hosted lambda expressions, the type comes from the annotation.
            // This is similar to e_anno_only - the implementation is provided by the host.
            switch (expected) {
                .no_expectation => {
                    // This shouldn't happen since hosted lambdas always have annotations
                    try self.unifyWith(expr_var, .err, env);
                },
                .expected => |_| {
                    // The expr will be unified with the expected type below
                    // expr_var is a flex var by default, so no action is need here
                },
            }
        },
        .e_run_low_level => |run_ll| {
            // Check each argument expression in the run_low_level node
            for (self.cir.store.exprSlice(run_ll.args)) |arg_idx| {
                does_fx = try self.checkExpr(arg_idx, env, .no_expectation) or does_fx;
            }
        },
        .e_type_var_dispatch => |tvd| {
            // Type variable dispatch expression: Thing.method(args) where Thing is a type var alias.
            // This is similar to static dispatch (e_dot_access with args) but dispatches on a
            // type variable rather than on the type of a receiver expression.

            // Check the args and track errors
            const dispatch_arg_expr_idxs = self.cir.store.exprSlice(tvd.args);
            var did_err = false;
            for (dispatch_arg_expr_idxs) |dispatch_arg_expr_idx| {
                does_fx = try self.checkExpr(dispatch_arg_expr_idx, env, .no_expectation) or does_fx;
                did_err = did_err or (self.types.resolveVar(ModuleEnv.varFrom(dispatch_arg_expr_idx)).desc.content == .err);
            }

            if (did_err) {
                // If any arguments are errors, propagate the error
                try self.unifyWith(expr_var, .err, env);
            } else {
                // Get the type var alias statement to access the type variable
                const type_var_alias_stmt = self.cir.store.getStatement(tvd.type_var_alias_stmt);
                const type_var_anno = type_var_alias_stmt.s_type_var_alias.type_var_anno;
                const type_var = ModuleEnv.varFrom(type_var_anno);

                // For type var dispatch, the arguments are just the explicit args (no receiver)
                const dispatch_arg_vars_range = try self.types.appendVars(@ptrCast(dispatch_arg_expr_idxs));

                // Since the return type of this dispatch is unknown, create a flex to represent it
                const dispatch_ret_var = try self.fresh(env, expr_region);

                // Create the function being dispatched
                const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                    .args = dispatch_arg_vars_range,
                    .ret = dispatch_ret_var,
                    .needs_instantiation = false,
                } } }, env, expr_region);

                // Create the static dispatch constraint
                const constraint = StaticDispatchConstraint{
                    .fn_name = tvd.method_name,
                    .fn_var = constraint_fn_var,
                    .origin = .method_call,
                };
                const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

                // Create a constrained flex and unify it with the type variable
                const constrained_var = try self.freshFromContent(
                    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
                    env,
                    expr_region,
                );

                _ = try self.unify(constrained_var, type_var, env);

                // Set the expression type to the return type of the dispatch
                _ = try self.unify(expr_var, dispatch_ret_var, env);
            }
        },
        .e_runtime_error => {
            try self.unifyWith(expr_var, .err, env);
        },
    }

    // Check if we have an annotation
    if (mb_anno_vars) |anno_vars| {
        // Unify the anno with the expr var
        _ = try self.unifyInContext(anno_vars.anno_var, expr_var, env, .type_annotation);

        // Check if the expression type contains any errors anywhere in its
        // structure. If it does and we have an annotation, use the annotation
        // type for the pattern instead of the expression type. This preserves
        // the annotation type for other code that references this identifier,
        // even when the expression has errors.
        //
        // For example, if the annotation is `I64 -> Str` and the expression has
        // an error in the return type (making it `I64 -> Error`), the pattern
        // should still get `I64 -> Str` from the annotation.
        self.var_set.clearRetainingCapacity();
        if (self.varContainsError(expr_var, &self.var_set)) {
            // If there was an annotation AND the expr contains errors, then unify the
            // raw expr var against the annotation
            _ = try self.unify(expr_var_raw, anno_vars.anno_var_backup, env);
        } else {
            // Otherwise, unify the raw var with the intermediate var
            _ = try self.unify(expr_var_raw, expr_var, env);
        }
    }

    // Check any accumulated static dispatch constraints
    try self.checkStaticDispatchConstraints(env);

    // If this type of expr should be generalized, generalize it!
    if (should_generalize) {
        const at_def_top_level = env.rank() == Rank.outermost.next();
        const is_cycle_root = if (self.cycle_root_def) |root_def|
            self.current_processing_def != null and root_def == self.current_processing_def.?
        else
            false;
        const is_intermediate = self.cycle_root_def != null and !is_cycle_root;

        if (is_cycle_root and at_def_top_level) {
            // Cycle root's top-level lambda: merge all stored cycle envs,
            // generalize, then run deferred unifications.
            for (self.deferred_cycle_envs.items) |*deferred_env| {
                std.debug.assert(deferred_env.rank() == Rank.outermost.next());
                try env.var_pool.mergeFrom(&deferred_env.var_pool);
            }

            try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

            // Execute deferred def-level unifications (now safe since
            // expr_vars are generalized and won't be lowered by Rank.min)
            for (self.deferred_def_unifications.items) |u| {
                _ = try self.unify(u.ptrn_var, u.expr_var, env);
                _ = try self.unify(u.def_var, u.ptrn_var, env);
            }
            self.deferred_def_unifications.clearRetainingCapacity();

            // Resolve eql constraints accumulated during cycle body checks
            // (from .processing handlers). This must happen now — before
            // subsequent defs use the generalized types — so that cross-
            // function constraints are propagated into the generalized vars
            // before instantiation creates independent copies.
            try self.checkConstraints(env);

            // Release stored envs back to pool
            for (self.deferred_cycle_envs.items) |deferred_env| {
                self.env_pool.release(deferred_env);
            }
            self.deferred_cycle_envs.clearRetainingCapacity();

            self.cycle_root_def = null;
            self.defer_generalize = false;
        } else if (is_intermediate and at_def_top_level) {
            // Intermediate's top-level lambda: skip generalization.
            // Vars are preserved and will be merged by the cycle root.
        } else {
            // Normal generalization (no cycle, or inner lambda within a cycle participant).
            try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());
        }
    }

    return does_fx;
}

fn getExprPatternIdent(self: *const Self, expr_idx: CIR.Expr.Idx) ?Ident.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const func_expr = self.cir.store.getExpr(expr_idx);
    switch (func_expr) {
        .e_lookup_local => |lookup| {
            // Get the pattern that defines this identifier
            const pattern = self.cir.store.getPattern(lookup.pattern_idx);
            switch (pattern) {
                .assign => |assign| return assign.ident,
                else => return null,
            }
        },
        else => return null,
    }
}

const AnnoVars = struct { anno_var: Var, anno_var_backup: Var };

/// Check if an expression represents a function definition that should be generalized.
/// This includes lambdas and function declarations (even those without bodies).
/// Returns true if the expression is a function definition: a closure wrapping
/// a lambda, a bare lambda, an annotation-only signature, or a hosted lambda.
/// Used for both generalization (value restriction) and cycle detection (deferred
/// generalization only applies to function defs).
fn isFunctionDef(store: *const CIR.NodeStore, expr: CIR.Expr) bool {
    return switch (expr) {
        .e_closure => |closure| {
            std.debug.assert(store.getExpr(closure.lambda_idx) == .e_lambda);
            return true;
        },
        .e_lambda => true,
        .e_anno_only => true,
        .e_hosted_lambda => true,
        else => false,
    };
}

// stmts //

const BlockStatementsResult = struct {
    does_fx: bool,
    diverges: bool,
};

/// Given a slice of stmts, type check each one
/// Returns whether any statement has effects and whether the block diverges (return/crash)
fn checkBlockStatements(self: *Self, statements: []const CIR.Statement.Idx, env: *Env, _: Region) std.mem.Allocator.Error!BlockStatementsResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var does_fx = false;
    var diverges = false;
    for (statements) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);
        const stmt_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(stmt_idx));

        try self.setVarRank(stmt_var, env);

        switch (stmt) {
            .s_decl => |decl_stmt| {
                const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);
                const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                // Check the pattern
                try self.checkPattern(decl_stmt.pattern, env);

                // Extract function name from the pattern (for better error messages)
                const saved_func_name = self.enclosing_func_name;
                self.enclosing_func_name = self.getPatternIdent(decl_stmt.pattern);
                defer self.enclosing_func_name = saved_func_name;

                // Check the annotation, if it exists
                const expectation = blk: {
                    if (decl_stmt.anno) |annotation_idx| {
                        break :blk Expected{ .expected = annotation_idx };
                    } else {
                        break :blk Expected.no_expectation;
                    }
                };

                does_fx = try self.checkExpr(decl_stmt.expr, env, expectation) or does_fx;

                _ = try self.unify(decl_pattern_var, decl_expr_var, env);
                _ = try self.unify(stmt_var, decl_pattern_var, env);
            },
            .s_var => |var_stmt| {
                // Check the pattern
                try self.checkPattern(var_stmt.pattern_idx, env);
                const var_pattern_var: Var = ModuleEnv.varFrom(var_stmt.pattern_idx);

                // Check the annotation, if it exists
                const expectation = blk: {
                    if (var_stmt.anno) |annotation_idx| {
                        // Return the expectation
                        break :blk Expected{ .expected = annotation_idx };
                    } else {
                        break :blk Expected.no_expectation;
                    }
                };

                does_fx = try self.checkExpr(var_stmt.expr, env, expectation) or does_fx;
                const var_expr: Var = ModuleEnv.varFrom(var_stmt.expr);

                _ = try self.unify(var_pattern_var, var_expr, env);
                _ = try self.unify(stmt_var, var_expr, env);
            },
            .s_reassign => |reassign| {
                // We don't need to check the pattern here since it was already
                // checked when this var was created.
                //
                // try self.checkPattern(reassign.pattern_idx, env, .no_expectation);

                const reassign_pattern_var: Var = ModuleEnv.varFrom(reassign.pattern_idx);

                does_fx = try self.checkExpr(reassign.expr, env, .no_expectation) or does_fx;
                const reassign_expr_var: Var = ModuleEnv.varFrom(reassign.expr);

                // Unify the pattern with the expression
                //
                // TODO: if there's a mismatch here, the region of the error is
                // the original assignment pattern, not the reassignment region
                _ = try self.unify(reassign_pattern_var, reassign_expr_var, env);

                _ = try self.unify(stmt_var, reassign_expr_var, env);
            },
            .s_for => |for_stmt| {
                // Check the pattern
                // for item in [1,2,3] {
                //     ^^^^
                try self.checkPattern(for_stmt.patt, env);
                const for_ptrn_var: Var = ModuleEnv.varFrom(for_stmt.patt);

                // Check the expr
                // for item in [1,2,3] {
                //             ^^^^^^^
                does_fx = try self.checkExpr(for_stmt.expr, env, .no_expectation) or does_fx;
                const for_expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(for_stmt.expr));
                const for_expr_var: Var = ModuleEnv.varFrom(for_stmt.expr);

                // Check that the expr is list of the ptrn
                const list_content = try self.mkListContent(for_ptrn_var, env);
                const list_var = try self.freshFromContent(list_content, env, for_expr_region);
                _ = try self.unify(list_var, for_expr_var, env);

                // Check the body
                // for item in [1,2,3] {
                //     print!(item.toStr())  <<<<
                // }
                does_fx = try self.checkExpr(for_stmt.body, env, .no_expectation) or does_fx;
                const for_body_var: Var = ModuleEnv.varFrom(for_stmt.body);

                // Check that the for body evaluates to {}
                const body_ret = try self.freshFromContent(.{ .structure = .empty_record }, env, for_expr_region);
                _ = try self.unify(body_ret, for_body_var, env);

                _ = try self.unify(stmt_var, for_body_var, env);
            },
            .s_while => |while_stmt| {
                // Check the condition
                // while $count < 10 {
                //       ^^^^^^^^^^^
                does_fx = try self.checkExpr(while_stmt.cond, env, .no_expectation) or does_fx;
                const cond_var: Var = ModuleEnv.varFrom(while_stmt.cond);
                const cond_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(while_stmt.cond));

                // Check that condition is Bool
                const bool_var = try self.freshBool(env, cond_region);
                _ = try self.unify(bool_var, cond_var, env);

                // Check the body
                // while $count < 10 {
                //     print!($count.toStr())  <<<<
                //     $count = $count + 1
                // }
                does_fx = try self.checkExpr(while_stmt.body, env, .no_expectation) or does_fx;
                const while_body_var: Var = ModuleEnv.varFrom(while_stmt.body);

                // Check that the while body evaluates to {}
                const body_ret = try self.freshFromContent(.{ .structure = .empty_record }, env, cond_region);
                _ = try self.unify(body_ret, while_body_var, env);

                _ = try self.unify(stmt_var, while_body_var, env);
            },
            .s_expr => |expr| {
                does_fx = try self.checkExpr(expr.expr, env, .no_expectation) or does_fx;
                const expr_var: Var = ModuleEnv.varFrom(expr.expr);

                // Statements must evaluate to {}. Add a constraint to unify with empty record.
                // If unification fails, we get a nice type mismatch error explaining that
                // statement expressions must return {}.
                const empty_rec = try self.freshFromContent(.{ .structure = .empty_record }, env, stmt_region);
                _ = try self.unifyInContext(empty_rec, expr_var, env, .statement_value);

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_dbg => |expr| {
                does_fx = try self.checkExpr(expr.expr, env, .no_expectation) or does_fx;
                const expr_var: Var = ModuleEnv.varFrom(expr.expr);

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_expect => |expr_stmt| {
                does_fx = try self.checkExpr(expr_stmt.body, env, .no_expectation) or does_fx;
                const body_var: Var = ModuleEnv.varFrom(expr_stmt.body);

                const bool_var = try self.freshBool(env, stmt_region);
                _ = try self.unifyInContext(bool_var, body_var, env, .expect);

                try self.unifyWith(stmt_var, .{ .structure = .empty_record }, env);
            },
            .s_crash => |_| {
                try self.unifyWith(stmt_var, .{ .flex = Flex.init() }, env);
                diverges = true;
            },
            .s_return => |ret| {
                // Type check the return expression
                does_fx = try self.checkExpr(ret.expr, env, .no_expectation) or does_fx;
                const ret_var = ModuleEnv.varFrom(ret.expr);

                // Write down this constraint for later validation
                // We assert the lambda's type and the return type are equiv
                const lambda_expr = self.cir.store.getExpr(ret.lambda);
                std.debug.assert(lambda_expr == .e_lambda);
                _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
                    .expected = ModuleEnv.varFrom(lambda_expr.e_lambda.body),
                    .actual = ret_var,
                    .ctx = .early_return,
                } });

                // A return statement's type should be a flex var so it can unify with any type.
                // This allows branches containing early returns to match any other branch type.
                try self.unifyWith(stmt_var, .{ .flex = Flex.init() }, env);
                diverges = true;
            },
            .s_nominal_decl => |nominal| {
                // Local nominal type declaration - generate the type properly
                try self.generateNominalDecl(stmt_idx, stmt_var, nominal, env);
            },
            .s_import, .s_alias_decl, .s_type_anno => {
                // These are only valid at the top level, czer reports error
                try self.unifyWith(stmt_var, .err, env);
            },
            .s_type_var_alias => {
                // Type var alias introduces no new constraints during type checking
                // The alias is already registered in scope by canonicalization
                // The type var it references is a rigid var from the enclosing function
                try self.unifyWith(stmt_var, .{ .structure = .empty_record }, env);
            },
            .s_runtime_error => {
                try self.unifyWith(stmt_var, .err, env);
            },
            .s_break => |_| {
                // Nothing to do for break
                // try self.unifyWith(stmt_var, .{ .structure = .empty_record }, env);
            },
        }
    }
    return .{ .does_fx = does_fx, .diverges = diverges };
}

// if-else //

/// Check the types for an if-else expr
fn checkIfElseExpr(
    self: *Self,
    if_expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    env: *Env,
    if_: @FieldType(CIR.Expr, @tagName(.e_if)),
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
    var does_fx = try self.checkExpr(first_branch.cond, env, .no_expectation);
    const first_cond_var: Var = ModuleEnv.varFrom(first_branch.cond);
    const bool_var = try self.freshBool(env, expr_region);
    _ = try self.unifyInContext(bool_var, first_cond_var, env, .if_condition);

    // Then we check the 1st branch's body
    does_fx = try self.checkExpr(first_branch.body, env, .no_expectation) or does_fx;

    // The 1st branch's body is the type all other branches must match
    const branch_var = @as(Var, ModuleEnv.varFrom(first_branch.body));

    // Total number of branches (including final else)
    const num_branches: u32 = @intCast(branches.len + 1);

    var last_if_branch = first_branch_idx;
    for (branches[1..], 1..) |branch_idx, cur_index| {
        const branch = self.cir.store.getIfBranch(branch_idx);

        // Check the branches condition
        does_fx = try self.checkExpr(branch.cond, env, .no_expectation) or does_fx;
        const cond_var: Var = ModuleEnv.varFrom(branch.cond);
        const branch_bool_var = try self.freshBool(env, expr_region);
        _ = try self.unifyInContext(branch_bool_var, cond_var, env, .if_condition);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body, env, .no_expectation) or does_fx;
        const body_var: Var = ModuleEnv.varFrom(branch.body);
        const body_result = try self.unifyInContext(branch_var, body_var, env, .{ .if_branch = .{
            .branch_index = @intCast(cur_index),
            .num_branches = num_branches,
            .is_else = false,
            .parent_if_expr = if_expr_idx,
            .last_if_branch = last_if_branch,
        } });

        if (!body_result.isOk()) {
            // Check remaining branches to catch their individual errors
            for (branches[cur_index + 1 ..]) |remaining_branch_idx| {
                const remaining_branch = self.cir.store.getIfBranch(remaining_branch_idx);

                does_fx = try self.checkExpr(remaining_branch.cond, env, .no_expectation) or does_fx;
                const remaining_cond_var: Var = ModuleEnv.varFrom(remaining_branch.cond);

                const fresh_bool = try self.freshBool(env, expr_region);
                _ = try self.unifyInContext(fresh_bool, remaining_cond_var, env, .if_condition);

                does_fx = try self.checkExpr(remaining_branch.body, env, .no_expectation) or does_fx;
                try self.unifyWith(ModuleEnv.varFrom(remaining_branch.body), .err, env);
            }

            // Break to avoid cascading errors
            break;
        }

        last_if_branch = branch_idx;
    }

    // Check the final else
    does_fx = try self.checkExpr(if_.final_else, env, .no_expectation) or does_fx;
    const final_else_var: Var = ModuleEnv.varFrom(if_.final_else);
    _ = try self.unifyInContext(branch_var, final_else_var, env, .{ .if_branch = .{
        .branch_index = num_branches - 1,
        .num_branches = num_branches,
        .is_else = true,
        .parent_if_expr = if_expr_idx,
        .last_if_branch = last_if_branch,
    } });

    // Set the entire expr's type to be the type of the branch
    const if_expr_var: Var = ModuleEnv.varFrom(if_expr_idx);
    _ = try self.unify(if_expr_var, branch_var, env);

    return does_fx;
}

// match //

/// Check the types for a match expr
fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, match: CIR.Expr.Match) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond, env, .no_expectation);
    const cond_var = ModuleEnv.varFrom(match.cond);

    // Assert we have at least 1 branch
    std.debug.assert(match.branches.span.len > 0);

    // Get slice of branches
    const branch_idxs = self.cir.store.sliceMatchBranches(match.branches);

    // Track whether we encountered any type errors during pattern checking
    // If so, we'll skip exhaustiveness checking since the types may be invalid
    var had_type_error = false;

    // For matches desugared from `?` operator, verify the condition unifies with Try type FIRST.
    // If it doesn't, report the specific error and skip pattern checking to avoid confusing errors.
    var has_invalid_try = false;
    if (match.is_try_suffix) {
        // Get the actual Try type from builtins and instantiate it with fresh type vars
        const try_type_var = ModuleEnv.varFrom(self.builtin_ctx.try_stmt);
        const copied_try_var = if (self.builtin_ctx.builtin_module) |builtin_env|
            try self.copyVar(try_type_var, builtin_env, Region.zero())
        else
            try_type_var;
        const try_var = try self.instantiateVar(copied_try_var, env, .use_root_instantiated);

        // Unify the condition with Try type
        const try_result = try self.unifyInContext(try_var, cond_var, env, .{ .try_operator_expr = .{
            .expr = match.cond,
        } });
        if (!try_result.isOk()) {
            has_invalid_try = true;
        }
    }

    // Manually check the 1st branch
    // The type of the branch's body becomes the var other branch bodies must unify
    // against.
    const first_branch_idx = branch_idxs[0];
    const first_branch = self.cir.store.getMatchBranch(first_branch_idx);
    const first_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(first_branch.patterns);

    // Skip pattern checking if we already know the condition isn't a Try type
    // This prevents confusing cascading errors about pattern incompatibility
    for (first_branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
        const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
        try self.checkPattern(branch_ptrn.pattern, env);
        const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);

        const ptrn_result = try self.unifyInContext(cond_var, branch_ptrn_var, env, .{ .match_pattern = .{
            .branch_index = 0,
            .pattern_index = @intCast(cur_ptrn_index),
            .num_branches = @intCast(match.branches.span.len),
            .num_patterns = @intCast(first_branch_ptrn_idxs.len),
            .match_expr = expr_idx,
        } });
        if (!ptrn_result.isOk()) had_type_error = true;
    }

    // Check guard if present
    if (first_branch.guard) |guard_idx| {
        does_fx = try self.checkExpr(guard_idx, env, .no_expectation) or does_fx;
        const guard_var = ModuleEnv.varFrom(guard_idx);
        const guard_bool_var = try self.freshBool(env, expr_region);
        _ = try self.unifyInContext(guard_bool_var, guard_var, env, .if_condition);
    }

    // Check the first branch's value, then use that at the branch_var
    does_fx = try self.checkExpr(first_branch.value, env, .no_expectation) or does_fx;
    const val_var = ModuleEnv.varFrom(first_branch.value);

    // Then iterate over the rest of the branches
    for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
        const branch = self.cir.store.getMatchBranch(branch_idx);

        // First, check the patterns of this branch (skip if invalid try to avoid confusing errors)
        const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
        for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
            // Check the pattern's sub types
            const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
            try self.checkPattern(branch_ptrn.pattern, env);

            // Check the pattern against the cond
            const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);
            const ptrn_result = try self.unifyInContext(cond_var, branch_ptrn_var, env, .{ .match_pattern = .{
                .branch_index = @intCast(branch_cur_index),
                .pattern_index = @intCast(cur_ptrn_index),
                .num_branches = @intCast(match.branches.span.len),
                .num_patterns = @intCast(branch_ptrn_idxs.len),
                .match_expr = expr_idx,
            } });
            if (!ptrn_result.isOk()) had_type_error = true;
        }

        // Check guard if present
        if (branch.guard) |guard_idx| {
            does_fx = try self.checkExpr(guard_idx, env, .no_expectation) or does_fx;
            const guard_var = ModuleEnv.varFrom(guard_idx);
            const branch_guard_bool_var = try self.freshBool(env, expr_region);
            _ = try self.unifyInContext(branch_guard_bool_var, guard_var, env, .if_condition);
        }

        // Then, check the body
        does_fx = try self.checkExpr(branch.value, env, .no_expectation) or does_fx;
        const branch_result = try self.unifyInContext(val_var, ModuleEnv.varFrom(branch.value), env, .{ .match_branch = .{
            .branch_index = @intCast(branch_cur_index),
            .num_branches = @intCast(match.branches.span.len),
            .match_expr = expr_idx,
        } });

        if (!branch_result.isOk()) {
            // If there was a body mismatch, do not check other branches to stop
            // cascading errors. But still check each other branch's sub types
            for (branch_idxs[branch_cur_index + 1 ..], branch_cur_index + 1..) |other_branch_idx, other_branch_cur_index| {
                const other_branch = self.cir.store.getMatchBranch(other_branch_idx);

                // Still check the other patterns (skip if invalid try to avoid confusing errors)
                const other_branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(other_branch.patterns);
                for (other_branch_ptrn_idxs, 0..) |other_branch_ptrn_idx, other_cur_ptrn_index| {
                    // Check the pattern's sub types
                    const other_branch_ptrn = self.cir.store.getMatchBranchPattern(other_branch_ptrn_idx);
                    try self.checkPattern(other_branch_ptrn.pattern, env);

                    // Check the pattern against the cond
                    const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
                    _ = try self.unifyInContext(cond_var, other_branch_ptrn_var, env, .{ .match_pattern = .{
                        .branch_index = @intCast(other_branch_cur_index),
                        .pattern_index = @intCast(other_cur_ptrn_index),
                        .num_branches = @intCast(match.branches.span.len),
                        .num_patterns = @intCast(other_branch_ptrn_idxs.len),
                        .match_expr = expr_idx,
                    } });
                }

                // Then check the other branch's exprs
                does_fx = try self.checkExpr(other_branch.value, env, .no_expectation) or does_fx;
                try self.unifyWith(ModuleEnv.varFrom(other_branch.value), .err, env);
            }

            // Then stop type checking for this branch
            break;
        }
    }

    // Unify the root expr with the match value
    _ = try self.unify(ModuleEnv.varFrom(expr_idx), val_var, env);

    // Perform exhaustiveness and redundancy checking
    // Only do this if there were no type errors - type errors can lead to invalid types
    // that confuse the exhaustiveness checker
    // Also skip if the condition type is an error type (can happen with complex inference)
    // Also skip if we already reported an invalid try operator error
    const resolved_cond = self.types.resolveVar(cond_var);
    const cond_is_error = resolved_cond.desc.content == .err;

    if (!had_type_error and !cond_is_error and !has_invalid_try) {
        const match_region = self.getRegionAt(@enumFromInt(@intFromEnum(expr_idx)));
        const builtin_idents = exhaustive.BuiltinIdents{
            .builtin_module = self.cir.idents.builtin_module,
            .u8_type = self.cir.idents.u8_type,
            .i8_type = self.cir.idents.i8_type,
            .u16_type = self.cir.idents.u16_type,
            .i16_type = self.cir.idents.i16_type,
            .u32_type = self.cir.idents.u32_type,
            .i32_type = self.cir.idents.i32_type,
            .u64_type = self.cir.idents.u64_type,
            .i64_type = self.cir.idents.i64_type,
            .u128_type = self.cir.idents.u128_type,
            .i128_type = self.cir.idents.i128_type,
            .f32_type = self.cir.idents.f32_type,
            .f64_type = self.cir.idents.f64_type,
            .dec_type = self.cir.idents.dec_type,
        };

        const result = exhaustive.checkMatch(
            self.cir.gpa,
            self.types,
            &self.cir.store,
            builtin_idents,
            match.branches,
            cond_var,
            match_region,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.TypeError => {
                // Type error in pattern - exhaustiveness checking can't proceed
                // This is expected when there are polymorphic types or type mismatches
                // Don't report exhaustiveness errors in this case
                return does_fx;
            },
        };
        defer result.deinit(self.cir.gpa);

        // Report non-exhaustive match if any patterns are missing
        if (!result.is_exhaustive) {
            const condition_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, cond_var);

            // Format missing patterns and store in problems store for lifecycle management
            // Track the start position for the missing patterns range
            const missing_patterns_start = self.problems.missing_patterns_backing.items.len;

            for (result.missing_patterns) |pattern| {
                const idx = try exhaustive.formatPattern(&self.problems.extra_strings_backing, &self.cir.common.idents, &self.cir.common.strings, pattern);
                try self.problems.missing_patterns_backing.append(idx);
            }

            const missing_patterns_range = problem.MissingPatternsRange{
                .start = missing_patterns_start,
                .count = self.problems.missing_patterns_backing.items.len - missing_patterns_start,
            };

            _ = try self.problems.appendProblem(self.gpa, .{ .non_exhaustive_match = .{
                .match_expr = expr_idx,
                .condition_snapshot = condition_snapshot,
                .missing_patterns = missing_patterns_range,
            } });
        }

        // Report redundant patterns
        for (result.redundant_indices) |idx| {
            _ = try self.problems.appendProblem(self.gpa, .{ .redundant_pattern = .{
                .match_expr = expr_idx,
                .num_branches = @intCast(match.branches.span.len),
                .problem_branch_index = idx,
            } });
        }

        // Report unmatchable patterns (patterns on uninhabited types)
        for (result.unmatchable_indices) |idx| {
            _ = try self.problems.appendProblem(self.gpa, .{ .unmatchable_pattern = .{
                .match_expr = expr_idx,
                .num_branches = @intCast(match.branches.span.len),
                .problem_branch_index = idx,
            } });
        }
    }

    return does_fx;
}

// unary minus //

/// Check the unary expr.
/// Desugars `-a` to `a.negate() : a -> a`,
fn checkUnaryMinusExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, env: *Env, unary: CIR.Expr.UnaryMinus) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_var = @as(Var, ModuleEnv.varFrom(expr_idx));

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, env, .no_expectation);

    // Get the not method + ret var
    // Here, we assert that the arg and ret of `not` are same type
    const not_method_name = self.cir.idents.negate;
    const not_arg_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const not_ret_var = not_arg_var;

    // Create the not static dispatch function on the not_arg + not_ret
    // This function attaches the dispatch fn to the not_arg
    try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region);

    // Redirect the result to the boolean type
    _ = try self.unify(expr_var, not_ret_var, env);

    return does_fx;
}

// unary not //

/// Check the unary expr.
/// Desugars `!a` to `a.not() : a -> a`,
fn checkUnaryNotExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, env: *Env, unary: CIR.Expr.UnaryNot) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_var = @as(Var, ModuleEnv.varFrom(expr_idx));

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, env, .no_expectation);

    // Get the not method + ret var
    // Here, we assert that the arg and ret of `not` are same type
    const not_method_name = self.cir.idents.not;
    const not_arg_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const not_ret_var = not_arg_var;

    // Create the not static dispatch function on the not_arg + not_ret
    // This function attaches the dispatch fn to the not_arg
    try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region);

    // Redirect the result to the boolean type
    _ = try self.unify(expr_var, not_ret_var, env);

    return does_fx;
}

// binop //

/// Check the types for a binary operation expression
fn checkBinopExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    env: *Env,
    binop: CIR.Expr.Binop,
) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_var = ModuleEnv.varFrom(expr_idx);
    const lhs_var = @as(Var, ModuleEnv.varFrom(binop.lhs));
    const rhs_var = @as(Var, ModuleEnv.varFrom(binop.rhs));

    // Check operands first
    var does_fx = false;
    does_fx = try self.checkExpr(binop.lhs, env, .no_expectation) or does_fx;
    does_fx = try self.checkExpr(binop.rhs, env, .no_expectation) or does_fx;

    switch (binop.op) {
        .add, .sub, .mul, .div, .rem, .div_trunc => {
            const method_name =
                switch (binop.op) {
                    .add => self.cir.idents.plus,
                    .sub => self.cir.idents.minus,
                    .mul => self.cir.idents.times,
                    .div => self.cir.idents.div_by,
                    .div_trunc => self.cir.idents.div_trunc_by,
                    .rem => self.cir.idents.rem_by,
                    else => unreachable,
                };

            // Return type equals lhs type - e.g. Duration.times : Duration, I64 -> Duration
            const ret_var = lhs_var;

            // Create the binop static dispatch function: lhs.method(rhs) -> ret
            try self.mkBinopConstraint(
                lhs_var,
                rhs_var,
                ret_var,
                method_name,
                env,
                expr_region,
            );

            // Set the expression to redirect to the return type
            _ = try self.unify(expr_var, ret_var, env);
        },
        .lt, .gt, .le, .ge, .eq => {
            // For comparison binops, lhs and rhs must have the same type.
            // Unify lhs and rhs first to ensure both operands have the same type
            const arg_unify_result = try self.unify(lhs_var, rhs_var, env);

            // If unification failed, short-circuit and set the expression to error
            if (!arg_unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            // Now that we've unified the rhs and lhs, use the unified type for the constraint
            const arg_var = rhs_var;

            const method_name, const ret_var =
                switch (binop.op) {
                    .lt => .{ self.cir.idents.is_lt, try self.freshBool(env, expr_region) },
                    .gt => .{ self.cir.idents.is_gt, try self.freshBool(env, expr_region) },
                    .le => .{ self.cir.idents.is_lte, try self.freshBool(env, expr_region) },
                    .ge => .{ self.cir.idents.is_gte, try self.freshBool(env, expr_region) },
                    .eq => .{ self.cir.idents.is_eq, try self.freshBool(env, expr_region) },
                    else => unreachable,
                };

            // Create the binop constraint with unified arg type
            try self.mkBinopConstraint(
                arg_var,
                arg_var,
                ret_var,
                method_name,
                env,
                expr_region,
            );

            // Set the expression to redirect to the return type
            _ = try self.unify(expr_var, ret_var, env);
        },
        .ne => {
            // `a != b` desugars to `a.is_eq(b).not()`.
            //
            // a.is_eq(b) : x, x -> y
            // y.not() : y -> y
            //
            // Currently, we required `y` to be a `Bool`. This is more
            // restrictive, but makes inferred types easier to understand. We
            // may revisit this in the future, but relaxing the restriction
            // should be a non-breaking change.

            // Unify lhs and rhs to ensure both operands have the same type
            const arg_unify_result = try self.unify(lhs_var, rhs_var, env);

            // If unification failed, short-circuit and set the expression to error
            if (!arg_unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            // Get the eq method + ret var
            const eq_method_name = self.cir.idents.is_eq;
            const eq_arg_var = rhs_var;
            const eq_ret_var = try self.freshBool(env, expr_region);

            // Create the eq static dispatch function: arg.is_eq(arg) -> Bool
            try self.mkBinopConstraint(eq_arg_var, eq_arg_var, eq_ret_var, eq_method_name, env, expr_region);

            // Get the not method + ret var
            const not_method_name = self.cir.idents.not;
            const not_arg_var = eq_ret_var;
            const not_ret_var = eq_ret_var;

            // Create the not static dispatch function on the not_arg + not_ret
            // This function attaches the dispatch fn to the not_arg
            try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region);

            // IMPORTANT: We currently required the eq_ret_var to be  a bool.
            // This is more restrictive, but makes inferred types easier to
            // understand. We may revisit this in the future, but relaxing the
            // restriction should be a non-breaking change.

            // The expression type is the return type of not
            _ = try self.unify(expr_var, not_ret_var, env);
        },
        .@"and", .@"or" => {
            const lhs_fresh_bool = try self.freshBool(env, expr_region);

            const binop_ctx: problem.Context.BinopContext.Binop = switch (binop.op) {
                .@"and" => .@"and",
                .@"or" => .@"or",
                else => unreachable,
            };
            const lhs_result = try self.unifyInContext(lhs_fresh_bool, lhs_var, env, .{ .binop_lhs = .{
                .operator = binop_ctx,
                .binop_expr = expr_idx,
            } });

            // If lhs unified successfully, then reuse that var, otherwise
            // create a fresh one. This is so we can get nice errors on both
            // sides of the binop.
            const rhs_fresh_bool = if (lhs_result.isOk()) lhs_fresh_bool else try self.freshBool(env, expr_region);

            _ = try self.unifyInContext(rhs_fresh_bool, rhs_var, env, .{ .binop_rhs = .{
                .operator = binop_ctx,
                .binop_expr = expr_idx,
            } });

            // Unify left and right together to ensure both are bools
            _ = try self.unify(lhs_var, rhs_var, env);

            // Set the expression to redirect to the return type
            _ = try self.unify(expr_var, lhs_var, env);
        },
    }

    return does_fx;
}

// binop + unary op exprs //

/// Create a static dispatch fn like: `lhs, rhs -> ret` and assert the
/// constraint to the lhs (receiver) var.
fn mkBinopConstraint(
    self: *Self,
    lhs_var: Var,
    rhs_var: Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create the function type: lhs_type, rhs_type -> ret_type
    const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });

    // Create the constraint function type
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    // Create the static dispatch constraint
    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .desugared_binop,
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

    // Create a constrained flex and unify it with the lhs (receiver)
    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, lhs_var, env);
}

/// Create a static dispatch fn like: `arg, arg -> ret` and assert the
/// constraint to the argument var.
fn mkUnaryOp(
    self: *Self,
    arg_var: Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create the function type: lhs_type, rhs_type -> ret_type
    const args_range = try self.types.appendVars(&.{arg_var});

    // Create the constraint function type
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    // Create the static dispatch constraint
    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .desugared_unaryop,
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

    // Create a constrained flex and unify it with the arg
    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, arg_var, env);
}

// problems //

// copy type from other module //

// external type lookups //

const ExternalType = struct {
    local_var: Var,
    other_cir_node_idx: CIR.Node.Idx,
    other_cir: *const ModuleEnv,
};

/// Copy a variable from a different module into this module's types store.
///
/// IMPORTANT: The caller must instantiate this variable before unifying
/// against it. This avoid poisoning the copied variable in the types store if
/// unification fails.
fn resolveVarFromExternal(
    self: *Self,
    import_idx: CIR.Import.Idx,
    node_idx: u16,
) std.mem.Allocator.Error!?ExternalType {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First try to use the resolved module index from the imports store
    // This is the proper way to map import indices to module positions
    const module_idx = self.cir.imports.getResolvedModule(import_idx) orelse blk: {
        // Fallback: if not resolved, use the import index directly
        // This maintains backwards compatibility with tests that don't call resolveImports
        break :blk @intFromEnum(import_idx);
    };
    if (module_idx < self.imported_modules.len) {
        const other_module_cir = self.imported_modules[module_idx];
        const other_module_env = other_module_cir;

        // The idx of the expression in the other module
        const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

        // Check if we've already copied this import
        const cache_key = ImportCacheKey{
            .module_idx = import_idx,
            .node_idx = target_node_idx,
        };

        const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
            // Reuse the previously copied type.
            cached_var
        else blk: {
            // First time importing this type - copy it and cache the result
            const imported_var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));

            // Every node should have a corresponding type entry
            std.debug.assert(@intFromEnum(imported_var) < other_module_env.types.len());

            const new_copy = try self.copyVar(imported_var, other_module_env, null);
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

/// Copy a variable from another module into this module
/// The ranks of all variables copied will be generalized
fn copyVar(self: *Self, other_module_var: Var, other_module_env: *const ModuleEnv, mb_region: ?Region) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First, reset state
    self.var_map.clearRetainingCapacity();

    // Copy the var from the dest type store into this type store
    const copied_var = try copy_import.copyVar(
        &other_module_env.*.types,
        self.types,
        other_module_var,
        &self.var_map,
        other_module_env.getIdentStoreConst(),
        self.cir.getIdentStore(),
        self.gpa,
    );

    const region = if (mb_region) |region| region else base.Region.zero();

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    if (self.var_map.count() > 0) {
        var iterator = self.var_map.iterator();
        while (iterator.next()) |x| {
            // Get the newly created var
            const fresh_var = x.value_ptr.*;
            try self.fillInRegionsThrough(fresh_var);

            self.setRegionAt(fresh_var, region);
        }
    }

    // Assert that we have regions for every type variable
    self.debugAssertArraysInSync();

    return copied_var;
}

// nominal type checking helpers //

/// Result of checking a nominal type usage
const NominalCheckResult = enum {
    /// Successfully checked the nominal type
    ok,
    /// An error occurred (already reported and target_var set to error)
    err,
};

/// Check a nominal type usage (either in pattern or expression context).
/// This is the shared logic for `.nominal`, `.nominal_external`, `.e_nominal`, and `.e_nominal_external`.
///
/// Parameters:
/// - target_var: The type variable to unify with (pattern_var or expr_var)
/// - actual_backing_var: The type variable of the backing expression/pattern
/// - nominal_type_decl_var: The type variable from the nominal type declaration
/// - backing_type: The kind of backing type (tag, record, tuple, value)
/// - region: The source region for instantiation
/// - env: The type checking environment
fn checkNominalTypeUsage(
    self: *Self,
    target_var: Var,
    actual_backing_var: Var,
    nominal_type_decl_var: Var,
    backing_type: CIR.Expr.NominalBackingType,
    region: Region,
    env: *Env,
) std.mem.Allocator.Error!NominalCheckResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Instantiate the nominal type declaration
    const nominal_var = try self.instantiateVar(nominal_type_decl_var, env, .{ .explicit = region });
    const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

    if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
        const nominal_type = nominal_resolved.structure.nominal_type;

        // If this nominal type is opaque and we're not in the defining module
        // then report an error
        if (!nominal_type.canLiftInner(self.cir.qualified_module_ident)) {
            _ = try self.problems.appendProblem(self.cir.gpa, .{ .cannot_access_opaque_nominal = .{
                .var_ = target_var,
                .nominal_type_name = nominal_type.ident.ident_idx,
            } });

            // Mark the entire expression as having a type error
            try self.unifyWith(target_var, .err, env);
            return .err;
        }

        // Extract the backing type variable from the nominal type
        // E.g. ConList(a) := [Cons(a, ConstList), Nil]
        //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
        const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);

        // Unify what the user wrote with the backing type of the nominal
        // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
        //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
        // Convert CIR.Expr.NominalBackingType to Context.NominalConstructorContext.BackingType
        const context_backing_type: problem.Context.NominalConstructorContext.BackingType = @enumFromInt(@intFromEnum(backing_type));
        const result = try self.unifyInContext(nominal_backing_var, actual_backing_var, env, .{
            .nominal_constructor = .{ .backing_type = context_backing_type },
        });

        // Handle the result of unification
        switch (result) {
            .ok => {
                // If unification succeeded, this is a valid instance of the nominal type
                // So we set the target's type to be the nominal type
                _ = try self.unify(target_var, nominal_var, env);
                return .ok;
            },
            .problem => {
                // Unification failed - the constructor is incompatible with the nominal type
                // Context is already set by unifyInContext
                // Mark the entire expression as having a type error
                try self.unifyWith(target_var, .err, env);
                return .err;
            },
        }
    } else {
        // If the nominal type resolves to something other than a nominal_type structure,
        // report the error and set the expression to error type
        _ = try self.problems.appendProblem(self.cir.gpa, .{ .nominal_type_resolution_failed = .{
            .var_ = target_var,
            .nominal_type_decl_var = nominal_type_decl_var,
        } });
        try self.unifyWith(target_var, .err, env);
        return .err;
    }
}

// validate constraints //

/// Check all constraints
/// We loop here because checkStaticDispatchConstraints and add new regular
/// constraints.
fn checkAllConstraints(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    while (self.constraints.items.items.len > 0) {
        try self.checkConstraints(env);
        try self.checkStaticDispatchConstraints(env);
    }
}

/// After type checking, resolve remaining from_numeral flex vars: first by inferring
/// the type from peer arguments in dispatch constraints (e.g., U64 from List.len),
/// then defaulting to Dec if no concrete peer is found.
/// Resolve from_numeral flex vars using type information from their dispatch
/// constraints, before falling back to Dec defaulting.
///
/// When a numeric literal appears in an arithmetic expression like
/// `0 + List.len(tail)`, the binop creates a dispatch constraint
/// `plus(F, U64) -> F` on the from_numeral flex var F. The normal dispatch
/// resolution can't process this because F (the dispatcher) is still flex.
/// But the constraint already contains the answer: the peer argument U64
/// tells us F must be U64, since all built-in numeric arithmetic is
/// homogeneous ((T, T) -> T) and from_numeral vars can only be numeric.
///
/// This pass walks from_numeral flex vars, finds concrete peer arguments
/// in their desugared_binop constraints, and unifies — letting the normal
/// dispatch resolution complete in the subsequent checkAllConstraints call.
fn resolveNumericLiteralsFromContext(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    if (self.types.from_numeral_flex_count == 0) return;

    const num_vars: u32 = @intCast(self.types.len());
    var i: u32 = 0;
    while (i < num_vars) : (i += 1) {
        const var_: types_mod.Var = @enumFromInt(i);
        const resolved = self.types.resolveVar(var_);
        if (resolved.desc.content != .flex) continue;

        const flex = resolved.desc.content.flex;
        if (flex.constraints.len() == 0) continue;

        const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);

        // Only process from_numeral flex vars.
        var has_from_numeral = false;
        for (constraints) |c| {
            if (c.origin == .from_numeral) {
                has_from_numeral = true;
                break;
            }
        }
        if (!has_from_numeral) continue;

        // Look for a desugared_binop constraint with a concrete peer argument.
        for (constraints) |c| {
            if (c.origin != .desugared_binop) continue;
            const fn_content = self.types.resolveVar(c.fn_var).desc.content;
            const func = fn_content.unwrapFunc() orelse continue;
            var found_peer = false;
            for (self.types.sliceVars(func.args)) |arg| {
                const resolved_arg = self.types.resolveVar(arg);
                if (resolved_arg.var_ == resolved.var_) continue; // skip self
                if (resolved_arg.desc.content.unwrapNominalType() == null) continue;
                _ = try self.unify(resolved.var_, resolved_arg.var_, env);
                found_peer = true;
                break;
            }
            if (found_peer) break;
        }
    }

    // Process constraints generated by the unifications above.
    // The from_numeral flex vars that were unified with concrete peers
    // now have their dispatch constraints deferred, and checkAllConstraints
    // will resolve them through the normal dispatch machinery.
    try self.checkAllConstraints(env);
}

/// Default any remaining from_numeral flex vars to Dec.
///
/// By the time this runs, resolveNumericLiteralsFromContext has already
/// unified from_numeral vars that had concrete peers in their binop
/// constraints (e.g., U64 from List.len). The only vars still flex here
/// are those with genuinely no numeric context, so Dec is correct.
///
/// For app modules with platform requirements, this should be called AFTER
/// `checkPlatformRequirements()` so that platform types can constrain
/// numeric literals first. Use `checkFileSkipNumericDefaults()` in that case.
pub fn finalizeNumericDefaults(self: *Self) std.mem.Allocator.Error!void {
    var env = try self.env_pool.acquire();
    defer self.env_pool.release(env);
    try self.finalizeNumericDefaultsInternal(&env);
}

fn finalizeNumericDefaultsInternal(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    if (self.types.from_numeral_flex_count == 0) return;

    const num_vars: u32 = @intCast(self.types.len());
    var i: u32 = 0;
    while (i < num_vars) : (i += 1) {
        const var_: types_mod.Var = @enumFromInt(i);
        const resolved = self.types.resolveVar(var_);
        if (resolved.desc.content != .flex) continue;

        const flex = resolved.desc.content.flex;
        const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
        var has_from_numeral = false;
        for (constraints) |c| {
            if (c.origin == .from_numeral) {
                has_from_numeral = true;
                break;
            }
        }
        if (!has_from_numeral) continue;

        const dec_var = try self.freshFromContent(try self.mkDecContent(env), env, Region.zero());
        _ = try self.unify(resolved.var_, dec_var, env);
    }

    // Process the newly created constraints from the unification
    try self.checkAllConstraints(env);
}

/// Process only early_return and try_operator constraints, keeping other
/// constraints for later processing. Called at the end of e_lambda to ensure return type
/// information is unified with the body type before the function type is generalized,
/// without prematurely processing other constraints from recursive lookups.
fn processReturnConstraints(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const original_len = self.constraints.items.items.len;
    var write_idx: usize = 0;
    for (0..original_len) |read_idx| {
        const constraint = self.constraints.items.items[read_idx];
        switch (constraint) {
            .eql => |eql| switch (eql.ctx) {
                .early_return => {
                    _ = try self.unifyInContext(eql.expected, eql.actual, env, .early_return);
                },
                .try_operator => {
                    _ = try self.unifyInContext(eql.expected, eql.actual, env, .try_operator);
                },
                else => {
                    self.constraints.items.items[write_idx] = constraint;
                    write_idx += 1;
                },
            },
        }
    }
    std.debug.assert(self.constraints.items.items.len == original_len);
    self.constraints.items.shrinkRetainingCapacity(write_idx);
}

/// Check any accumulated constraints
fn checkConstraints(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var iter = self.constraints.iterIndices();
    while (iter.next()) |idx| {
        const constraint = self.constraints.get(idx);
        switch (constraint.*) {
            .eql => |eql| {
                _ = try self.unifyInContext(eql.expected, eql.actual, env, eql.ctx);
            },
        }
    }
    self.constraints.items.clearRetainingCapacity();
}

/// Check static dispatch constraints
///
/// Note that new constraints can be added as we are processing. For example:
///
///  Test := [Val(Str)].{
///    to_str = |Test.Val(s)| s
///    to_str2 = |test| test.to_str()
///  }
///  main = Test.Val("hello").to_str2()
///
/// Initially, we only have to check constraint for `Test.to_str2`. But when we
/// process that, we then have to check `Test.to_str`.
fn checkStaticDispatchConstraints(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // During this pass, we want to hold onto any flex vars we encounter and
    // check them again later, when maybe they've been resolved
    const scratch_deferred_top = self.scratch_deferred_static_dispatch_constraints.top();
    defer self.scratch_deferred_static_dispatch_constraints.clearFrom(scratch_deferred_top);

    var deferred_constraint_index: usize = 0;
    while (deferred_constraint_index < env.deferred_static_dispatch_constraints.items.items.len) : (deferred_constraint_index += 1) {
        const deferred_constraint = env.deferred_static_dispatch_constraints.items.items[deferred_constraint_index];
        const dispatcher_resolved = self.types.resolveVar(deferred_constraint.var_);
        const dispatcher_content = dispatcher_resolved.desc.content;

        // Verify no recursive constraints - recursion should be handled through
        // nominal types which break the cycle naturally.
        for (self.constraint_check_stack.items) |stack_var| {
            std.debug.assert(stack_var != dispatcher_resolved.var_);
        }

        try self.constraint_check_stack.append(self.gpa, dispatcher_resolved.var_);
        defer _ = self.constraint_check_stack.pop();

        if (dispatcher_content == .err) {
            // If the root type is an error, then skip constraint checking
            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            for (constraints) |constraint| {
                try self.markConstraintFunctionAsError(constraint, env);
            }
            try self.unifyWith(deferred_constraint.var_, .err, env);
        } else if (dispatcher_content == .rigid) {
            // Get the rigid variable and the constraints it has defined
            const rigid = dispatcher_content.rigid;
            const rigid_constraints = self.types.sliceStaticDispatchConstraints(rigid.constraints);

            // Get the deferred constraints to validate against
            const deferred_constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);

            // Build a map of constraints the rigid has
            self.ident_to_var_map.clearRetainingCapacity();
            try self.ident_to_var_map.ensureUnusedCapacity(@intCast(rigid_constraints.len));
            for (rigid_constraints) |rigid_constraint| {
                self.ident_to_var_map.putAssumeCapacity(rigid_constraint.fn_name, rigid_constraint.fn_var);
            }

            // Iterate over the constraints
            for (deferred_constraints) |constraint| {
                // Extract the function and return type from the constraint
                const resolved_constraint = self.types.resolveVar(constraint.fn_var);
                const mb_resolved_func = resolved_constraint.desc.content.unwrapFunc();
                std.debug.assert(mb_resolved_func != null);
                const resolved_func = mb_resolved_func.?;

                // Then, lookup the inferred constraint in the actual list of rigid constraints
                if (self.ident_to_var_map.get(constraint.fn_name)) |rigid_var| {
                    // Unify the actual function var against the inferred var
                    //
                    // TODO: For better error messages, we should check if these
                    // types are functions, unify each arg, etc. This should look
                    // similar to e_call
                    const result = try self.unify(rigid_var, constraint.fn_var, env);
                    if (result.isProblem()) {
                        try self.unifyWith(deferred_constraint.var_, .err, env);
                        try self.unifyWith(resolved_func.ret, .err, env);
                    }
                } else {
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .{ .missing_method = .nominal },
                        env,
                    );
                    continue;
                }
            }
        } else if (dispatcher_content == .structure and dispatcher_content.structure == .nominal_type) {
            // If the root type is a nominal type, then this is valid static dispatch
            const nominal_type = dispatcher_content.structure.nominal_type;

            // Get the module ident that this type was defined in
            const original_module_ident = nominal_type.origin_module;

            // Check if the nominal type in question is defined in this module
            const is_this_module = original_module_ident == self.builtin_ctx.module_name;

            // Get the list of exposed items to check
            const original_env: *const ModuleEnv = blk: {
                if (is_this_module) {
                    break :blk self.cir;
                } else if (original_module_ident == self.cir.idents.builtin_module) {
                    // For builtin types, use the builtin module environment directly
                    if (self.builtin_ctx.builtin_module) |builtin_env| {
                        break :blk builtin_env;
                    } else {
                        // This happens when compiling the Builtin module itself
                        break :blk self.cir;
                    }
                } else {
                    // For types from other modules (not this module, not builtin), find the
                    // module environment from imported_modules by matching the qualified module name.
                    // We use qualified_module_ident (package-qualified) for comparison since origin_module
                    // is also package-qualified (e.g., "pf.Builder" rather than just "Builder").
                    for (self.imported_modules) |imported_env| {
                        const imported_name = if (!imported_env.qualified_module_ident.isNone())
                            imported_env.getIdent(imported_env.qualified_module_ident)
                        else
                            imported_env.module_name;
                        const imported_module_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text(imported_name));
                        if (imported_module_ident == original_module_ident) {
                            break :blk imported_env;
                        }
                    }

                    // Could not find the module environment. This is an internal compiler error.
                    std.debug.panic("Unable to find module environment for type {s} from module {s}", .{ self.cir.getIdent(nominal_type.ident.ident_idx), self.cir.getIdent(original_module_ident) });
                }
            };

            // Get some data about the nominal type
            const region = self.getRegionAt(deferred_constraint.var_);

            // Iterate over the constraints
            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            for (constraints) |constraint| {
                const constraint_fn_resolved = self.types.resolveVar(constraint.fn_var).desc.content;
                if (constraint_fn_resolved == .err) {
                    // If this constraint is already an error, the skip this pass
                    continue;
                }

                // Look up the method in the original env using index-based lookup.
                // Methods are stored with qualified names like "Type.method" (or "Module.Type.method" for builtins).
                const method_ident = original_env.lookupMethodIdentFromEnvConst(self.cir, nominal_type.ident.ident_idx, constraint.fn_name) orelse {
                    // Method name doesn't exist in target module
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .{ .missing_method = .nominal },
                        env,
                    );
                    continue;
                };

                // Get the def index in the original env
                const node_idx_in_original_env = original_env.getExposedNodeIndexById(method_ident) orelse {
                    // The ident exists but isn't exposed as a def
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .{ .missing_method = .nominal },
                        env,
                    );
                    continue;
                };

                const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(node_idx_in_original_env)));
                const def_var: Var = ModuleEnv.varFrom(def_idx);

                // Track whether we just processed a cycle participant
                var cycle_method_expr_var: ?Var = null;

                if (is_this_module) {
                    // Check if we've processed this def already.
                    const def = original_env.store.getDef(def_idx);
                    const mb_processing_def = self.top_level_ptrns.get(def.pattern);
                    if (mb_processing_def) |processing_def| {
                        std.debug.assert(processing_def.def_idx == def_idx);
                        switch (processing_def.status) {
                            .not_processed => {
                                var sub_env = try self.env_pool.acquire();
                                errdefer self.env_pool.release(sub_env);

                                try sub_env.var_pool.pushRank();
                                std.debug.assert(sub_env.rank() == .outermost);

                                try self.checkDef(processing_def.def_idx, &sub_env);

                                if (self.defer_generalize) {
                                    std.debug.assert(self.cycle_root_def != null);

                                    // Cycle detected: store env for merge at cycle root.
                                    try self.deferred_cycle_envs.append(self.gpa, sub_env);
                                    // Use the def's closure/expr var directly (same
                                    // as e_lookup_local .not_processed). After checkDef,
                                    // e_closure rank elevation has already run, so the
                                    // closure var is at rank 2 — safe for unification.
                                    const def_expr_var = ModuleEnv.varFrom(def.expr);
                                    cycle_method_expr_var = def_expr_var;
                                } else {
                                    std.debug.assert(sub_env.rank() == .outermost);
                                    self.env_pool.release(sub_env);
                                }
                            },
                            .processing => {
                                // Create a fresh flex var at the current rank for
                                // the method type. Using def_var directly (rank
                                // outermost) would pull body vars to a lower rank
                                // and prevent generalization.
                                cycle_method_expr_var = try self.fresh(env, region);

                                // Check if this is mutual recursion through dispatch.
                                // Only trigger for function defs (closures/lambdas).
                                if (self.current_processing_def) |current_def| {
                                    if (current_def != processing_def.def_idx) {
                                        const ref_def = self.cir.store.getDef(processing_def.def_idx);
                                        if (isFunctionDef(&self.cir.store, self.cir.store.getExpr(ref_def.expr))) {
                                            if (self.cycle_root_def == null) {
                                                // First cycle detection: no prior cycle should be in progress.
                                                std.debug.assert(!self.defer_generalize);
                                                std.debug.assert(self.deferred_cycle_envs.items.len == 0);
                                                std.debug.assert(self.deferred_def_unifications.items.len == 0);
                                                self.cycle_root_def = processing_def.def_idx;
                                            }
                                            self.defer_generalize = true;
                                        }
                                    }
                                }
                            },
                            .processed => {},
                        }
                    }
                }

                // Copy the actual method from the dest module env to this module env
                const method_var = if (cycle_method_expr_var) |expr_var_for_method| blk: {
                    // Cycle participant or recursive self-dispatch: use the
                    // fresh flex var instead of def_var to avoid rank lowering.
                    break :blk expr_var_for_method;
                } else if (is_this_module) blk: {
                    if (self.types.resolveVar(def_var).desc.rank == .generalized)
                        break :blk try self.instantiateVar(def_var, env, .use_last_var)
                    else
                        break :blk def_var;
                } else blk: {
                    // Copy the method from the other module's type store
                    const copied_var = try self.copyVar(def_var, original_env, region);
                    break :blk try self.instantiateVar(copied_var, env, .{ .explicit = region });
                };

                // Unwrap the constraint type
                const constraint_fn = constraint_fn_resolved.unwrapFunc() orelse {
                    _ = try self.unifyInContext(method_var, constraint.fn_var, env, .{
                        .method_type = .{
                            .constraint_var = constraint.fn_var,
                            .dispatcher_name = nominal_type.ident.ident_idx,
                            .method_name = constraint.fn_name,
                        },
                    });
                    try self.unifyWith(deferred_constraint.var_, .err, env);
                    continue;
                };

                const fn_result = try self.unifyInContext(method_var, constraint.fn_var, env, .{
                    .method_type = .{
                        .constraint_var = deferred_constraint.var_,
                        .dispatcher_name = nominal_type.ident.ident_idx,
                        .method_name = constraint.fn_name,
                    },
                });

                // If there was a problem, then ensure the error gets propagated
                // to all args and return types.
                if (fn_result.isProblem()) {
                    // Use iterator instead of slice because unifyWith may trigger reallocations
                    var args_iter = self.types.iterVars(constraint_fn.args);
                    while (args_iter.next()) |arg| {
                        // Propagate the error to args — necessary because constraint fn args
                        // are shared with actual expression vars (e.g., binop lhs/rhs), and
                        // leaving them non-err after a dispatch failure causes type confusion.
                        try self.unifyWith(arg, .err, env);
                    }
                    try self.unifyWith(deferred_constraint.var_, .err, env);
                    try self.unifyWith(constraint_fn.ret, .err, env);
                }

                // Note: from_numeral constraint validation happens during comptime evaluation
                // in ComptimeEvaluator.validateDeferredNumericLiterals()
            }
        } else if (dispatcher_content == .structure and
            (dispatcher_content.structure == .record or
                dispatcher_content.structure == .tuple or
                dispatcher_content.structure == .tag_union or
                dispatcher_content.structure == .empty_record or
                dispatcher_content.structure == .empty_tag_union))
        {
            // Anonymous structural types (records, tuples, tag unions) have implicit is_eq
            // only if all their components also support is_eq
            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            for (constraints) |constraint| {
                // Check if this is a call to is_eq (anonymous types have implicit structural equality)
                if (constraint.fn_name == self.cir.idents.is_eq) {
                    // Check if all components of this anonymous type support is_eq
                    if (self.typeSupportsIsEq(dispatcher_content.structure)) {
                        // All components support is_eq, unify return type with Bool
                        const resolved_constraint = self.types.resolveVar(constraint.fn_var);
                        const mb_resolved_func = resolved_constraint.desc.content.unwrapFunc();
                        if (mb_resolved_func) |resolved_func| {
                            const region = self.getRegionAt(deferred_constraint.var_);
                            const bool_var = try self.freshBool(env, region);
                            _ = try self.unify(bool_var, resolved_func.ret, env);
                        }
                    } else {
                        // Some component doesn't support is_eq (e.g., contains a function)
                        try self.reportEqualityError(
                            deferred_constraint.var_,
                            constraint,
                            env,
                        );
                    }
                } else {
                    // Structural types (other than is_eq) cannot have methods called on them.
                    // The user must explicitly wrap the value in a nominal type.
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .not_nominal,
                        env,
                    );
                }
            }
        } else if (dispatcher_content == .flex) {
            // If the dispatcher is a flex, hold onto the constraint to try again later.
            // Note: flex vars with from_numeral constraints are validated separately
            // in checkFlexVarConstraintCompatibility after type checking completes.
            _ = try self.scratch_deferred_static_dispatch_constraints.append(deferred_constraint);
        } else {
            // If the root type is anything but a nominal type or anonymous structural type, push an error
            // This handles function types, which do not support any methods

            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            if (constraints.len > 0) {
                // Report errors for ALL failing constraints, not just the first one
                for (constraints) |constraint| {
                    // For is_eq constraints, use the specific equality error message
                    // Use ident index comparison instead of string comparison
                    if (constraint.fn_name == self.cir.idents.is_eq) {
                        try self.reportEqualityError(
                            deferred_constraint.var_,
                            constraint,
                            env,
                        );
                    } else {
                        try self.reportConstraintError(
                            deferred_constraint.var_,
                            constraint,
                            .not_nominal,
                            env,
                        );
                    }
                }
            } else {
                // Deferred constraint checks should always have at least one constraint.
                // If we hit this, there's a compiler bug in how constraints are tracked.
                std.debug.assert(false);
            }
        }
    }

    // Now that we've processed all constraints, reset the array
    env.deferred_static_dispatch_constraints.items.clearRetainingCapacity();

    // Copy any flex constraints to try again later
    try env.deferred_static_dispatch_constraints.items.appendSlice(
        self.gpa,
        self.scratch_deferred_static_dispatch_constraints.sliceFromStart(scratch_deferred_top),
    );
}

/// Check if a structural type supports is_eq.
/// A type supports is_eq if:
/// - It's not a function type
/// - All of its components (record fields, tuple elements, tag payloads) also support is_eq
/// - For nominal types, check if their backing type supports is_eq
fn typeSupportsIsEq(self: *Self, flat_type: types_mod.FlatType) bool {
    return switch (flat_type) {
        // Function types do not support is_eq
        .fn_pure, .fn_effectful, .fn_unbound => false,

        // Empty types trivially support is_eq
        .empty_record, .empty_tag_union => true,

        // Records support is_eq if all field types support is_eq
        .record => |record| {
            const fields_slice = self.types.getRecordFieldsSlice(record.fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (!self.varSupportsIsEq(field_var)) return false;
            }
            return true;
        },

        // Tuples support is_eq if all element types support is_eq
        .tuple => |tuple| {
            const elems = self.types.sliceVars(tuple.elems);
            for (elems) |elem_var| {
                if (!self.varSupportsIsEq(elem_var)) return false;
            }
            return true;
        },

        // Tag unions support is_eq if all payload types support is_eq
        .tag_union => |tag_union| {
            const tags_slice = self.types.getTagsSlice(tag_union.tags);
            for (tags_slice.items(.args)) |tag_args| {
                const args = self.types.sliceVars(tag_args);
                for (args) |arg_var| {
                    if (!self.varSupportsIsEq(arg_var)) return false;
                }
            }
            return true;
        },

        // Nominal types support is_eq if their backing type supports is_eq
        .nominal_type => |nominal| {
            const backing_var = self.types.getNominalBackingVar(nominal);
            return self.varSupportsIsEq(backing_var);
        },

        // Unbound records: resolve and check the resolved type
        .record_unbound => |fields| {
            // Check each field in the unbound record
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (!self.varSupportsIsEq(field_var)) return false;
            }
            return true;
        },
    };
}

/// Check if a type variable supports is_eq by resolving it and checking its content
fn varSupportsIsEq(self: *Self, var_: Var) bool {
    const resolved = self.types.resolveVar(var_);
    return switch (resolved.desc.content) {
        .structure => |s| self.typeSupportsIsEq(s),
        // Flex/rigid vars: we optimistically assume they support is_eq.
        // This is sound because if the variable is later unified with a type
        // that doesn't support is_eq (like a function), unification will fail.
        .flex, .rigid => true,
        // Aliases: check the underlying type
        .alias => |alias| self.varSupportsIsEq(self.types.getAliasBackingVar(alias)),
        // Error types: allow them to proceed
        .err => true,
    };
}

/// Check if a flex var has incompatible constraints and report errors.
/// This is called after type-checking to catch cases like `!3` where a flex var
/// has both `from_numeral` (numeric) and `not` (Bool only) constraints.
/// If the flex var has a from_numeral constraint (meaning it will default to a numeric
/// type like Dec), we validate that all other constraints can be satisfied by Dec.
fn checkFlexVarConstraintCompatibility(self: *Self, var_: Var, env: *Env) Allocator.Error!void {
    const resolved = self.types.resolveVar(var_);
    if (resolved.desc.content != .flex) return;

    const flex = resolved.desc.content.flex;
    const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
    if (constraints.len == 0) return;

    // Check if this flex var has from_numeral constraint (indicating numeric type)
    var has_from_numeral = false;
    for (constraints) |c| {
        if (c.origin == .from_numeral) {
            has_from_numeral = true;
            break;
        }
    }

    if (has_from_numeral) {
        // This flex will default to Dec. Validate that all other constraints
        // can be satisfied by Dec.
        const builtin_env = self.builtin_ctx.builtin_module orelse return;
        const indices = self.builtin_ctx.builtin_indices orelse return;

        for (constraints) |constraint| {
            // Skip from_numeral - that's satisfied by Dec by definition
            if (constraint.origin == .from_numeral) continue;

            // Check if Dec has this method
            const method_ident = builtin_env.lookupMethodIdentFromEnvConst(self.cir, indices.dec_ident, constraint.fn_name);
            if (method_ident == null) {
                // Dec doesn't have this method - report error
                try self.reportConstraintError(
                    var_,
                    constraint,
                    .{ .missing_method = .nominal },
                    env,
                );
            }
        }
    }
}

/// Check if a type variable contains any error types anywhere in its structure.
/// This is used to determine if an expression's type contains errors, in which case
/// we should use the annotation type for the pattern instead of the expression type.
/// This handles cases like `Error -> Error` where the root is a function but the
/// argument/return types are errors.
fn varContainsError(self: *Self, var_: Var, visited: *std.AutoHashMap(Var, void)) bool {
    const resolved = self.types.resolveVar(var_);

    // Check if we've already visited this var (cycle detection)
    if (visited.contains(resolved.var_)) {
        return false;
    }
    visited.put(resolved.var_, {}) catch return false;

    return switch (resolved.desc.content) {
        .err => true,
        .flex, .rigid => false,
        .alias => |alias| self.varContainsError(self.types.getAliasBackingVar(alias), visited),
        .structure => |flat_type| self.flatTypeContainsError(flat_type, visited),
    };
}

/// Check if a flat type contains any error types
fn flatTypeContainsError(self: *Self, flat_type: FlatType, visited: *std.AutoHashMap(Var, void)) bool {
    return switch (flat_type) {
        .tuple => |tuple| self.varsContainError(self.types.sliceVars(tuple.elems), visited),
        .nominal_type => |nominal| blk: {
            var arg_iter = self.types.iterNominalArgs(nominal);
            while (arg_iter.next()) |arg_var| {
                if (self.varContainsError(arg_var, visited)) break :blk true;
            }
            break :blk self.varContainsError(self.types.getNominalBackingVar(nominal), visited);
        },
        .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
            if (self.varsContainError(self.types.sliceVars(func.args), visited)) break :blk true;
            break :blk self.varContainsError(func.ret, visited);
        },
        .record => |record| blk: {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            if (self.varsContainError(fields.items(.var_), visited)) break :blk true;
            break :blk self.varContainsError(record.ext, visited);
        },
        .record_unbound => |fields| blk: {
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            break :blk self.varsContainError(fields_slice.items(.var_), visited);
        },
        .tag_union => |tag_union| blk: {
            const tags = self.types.getTagsSlice(tag_union.tags);
            for (tags.items(.args)) |tag_args| {
                if (self.varsContainError(self.types.sliceVars(tag_args), visited)) break :blk true;
            }
            break :blk self.varContainsError(tag_union.ext, visited);
        },
        .empty_record, .empty_tag_union => false,
    };
}

/// Check if any of the given vars contain errors
fn varsContainError(self: *Self, vars: []const Var, visited: *std.AutoHashMap(Var, void)) bool {
    for (vars) |v| {
        if (self.varContainsError(v, visited)) return true;
    }
    return false;
}

/// Mark a constraint function's return type as error
fn markConstraintFunctionAsError(self: *Self, constraint: StaticDispatchConstraint, env: *Env) !void {
    const resolved_constraint = self.types.resolveVar(constraint.fn_var);
    const mb_resolved_func = resolved_constraint.desc.content.unwrapFunc();
    std.debug.assert(mb_resolved_func != null);
    const resolved_func = mb_resolved_func.?;
    // Use unify instead of unifyWith because the constraint's return type may be at a
    // different rank than the current env (e.g., from a local declaration that wasn't
    // generalized due to the value restriction).
    const err_var = try self.freshFromContent(.err, env, self.getRegionAt(resolved_func.ret));
    _ = try self.unify(resolved_func.ret, err_var, env);
}

/// Report a constraint validation error
fn reportConstraintError(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    kind: union(enum) {
        missing_method: problem.DispatcherDoesNotImplMethod.DispatcherType,
        not_nominal,
    },
    env: *Env,
) !void {
    const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, dispatcher_var);
    const constraint_problem = switch (kind) {
        .missing_method => |dispatcher_type| problem.Problem{ .static_dispatch = .{
            .dispatcher_does_not_impl_method = .{
                .dispatcher_var = dispatcher_var,
                .dispatcher_snapshot = snapshot,
                .dispatcher_type = dispatcher_type,
                .fn_var = constraint.fn_var,
                .method_name = constraint.fn_name,
                .origin = constraint.origin,
                .num_literal = constraint.num_literal,
            },
        } },
        .not_nominal => problem.Problem{ .static_dispatch = .{
            .dispatcher_not_nominal = .{
                .dispatcher_var = dispatcher_var,
                .dispatcher_snapshot = snapshot,
                .fn_var = constraint.fn_var,
                .method_name = constraint.fn_name,
            },
        } },
    };
    _ = try self.problems.appendProblem(self.cir.gpa, constraint_problem);

    try self.markConstraintFunctionAsError(constraint, env);
}

/// Report an error when an anonymous type doesn't support equality
fn reportEqualityError(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
) !void {
    const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, dispatcher_var);
    const equality_problem = problem.Problem{ .static_dispatch = .{
        .type_does_not_support_equality = .{
            .dispatcher_var = dispatcher_var,
            .dispatcher_snapshot = snapshot,
            .fn_var = constraint.fn_var,
        },
    } };
    _ = try self.problems.appendProblem(self.cir.gpa, equality_problem);

    try self.markConstraintFunctionAsError(constraint, env);
}

/// Pool for reusing Env instances to avoid repeated allocations
const EnvPool = struct {
    available: std.ArrayList(Env),
    gpa: Allocator,

    fn init(gpa: Allocator) std.mem.Allocator.Error!EnvPool {
        var pool = try std.ArrayList(Env).initCapacity(gpa, 8);
        for (0..8) |_| {
            pool.appendAssumeCapacity(try Env.init(gpa, .generalized));
        }
        return .{
            .available = pool,
            .gpa = gpa,
        };
    }

    fn deinit(self: *EnvPool) void {
        for (self.available.items) |*env| {
            env.deinit(self.gpa);
        }
        self.available.deinit(self.gpa);
    }

    /// Acquire an Env from the pool, or create a new one if none available
    fn acquire(self: *EnvPool) std.mem.Allocator.Error!Env {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (self.available.pop()) |env| {
            // Reset the env for reuse
            var reused_env = env;
            try reused_env.reset(.generalized);
            return reused_env;
        } else {
            // Otherwise init a new one and ensure there's room to put it back
            // into the pool when we're done using it
            try self.available.ensureUnusedCapacity(self.gpa, 1);
            return try Env.init(self.gpa, .generalized);
        }
    }

    /// Return an Env to the pool for reuse.
    /// If the pool cannot fit it, then deinit the env
    ///
    /// * If we acquire an existing env from the pool, there should be a slot
    ///   available to return it
    /// * If we init a new env when we acquire, the acquire func should expand the
    ///   pool so we have room to return it
    fn release(self: *EnvPool, env: Env) void {
        const trace = tracy.trace(@src());
        defer trace.end();

        var releasable_env = env;
        releasable_env.reset(.generalized) catch {
            // If we can't add to the pool, just deinit this env
            releasable_env.deinit(self.gpa);
        };
        self.available.append(self.gpa, releasable_env) catch {
            // If we can't add to the pool, just deinit this env
            releasable_env.deinit(self.gpa);
        };
    }
};

/// Create the import mapping for type display names in error messages.
///
/// This builds a mapping from fully-qualified type identifiers to their shortest display names
/// based on what's in scope. The mapping is built from:
/// 1. Auto-imported builtin types (e.g., Bool, Str, Dec, U64, etc.)
/// 2. User import statements with their aliases
///
/// When multiple imports could refer to the same type, the shortest name wins.
/// This ensures error messages show types the way users would write them in their code.
pub fn createImportMapping(
    gpa: std.mem.Allocator,
    idents: *Ident.Store,
    cir: *const ModuleEnv,
    builtin_module: ?*const ModuleEnv,
    builtin_indices: ?CIR.BuiltinIndices,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
) std.mem.Allocator.Error!types_mod.import_mapping.ImportMapping {
    var mapping = types_mod.import_mapping.ImportMapping.init(gpa);
    errdefer mapping.deinit();

    // Step 1: Add auto-imported builtin types
    if (builtin_module) |builtin_env| {
        if (builtin_indices) |indices| {
            const fields = @typeInfo(CIR.BuiltinIndices).@"struct".fields;
            inline for (fields) |field| {
                // Only process Statement.Idx fields (skip Ident.Idx fields)
                if (field.type == CIR.Statement.Idx) {
                    const stmt_idx: CIR.Statement.Idx = @field(indices, field.name);

                    // Skip invalid statement indices (index 0 is typically invalid/sentinel)
                    if (@intFromEnum(stmt_idx) != 0) {
                        const stmt = builtin_env.store.getStatement(stmt_idx);
                        const header_idx = switch (stmt) {
                            .s_nominal_decl => |decl| decl.header,
                            .s_alias_decl => |alias| alias.header,
                            else => null,
                        };
                        if (header_idx) |hdr_idx| {
                            const header = builtin_env.store.getTypeHeader(hdr_idx);
                            const qualified_name = builtin_env.getIdentText(header.name);
                            const relative_name = builtin_env.getIdentText(header.relative_name);

                            // Extract display name (last component after dots)
                            const display_name = blk: {
                                var last_dot: usize = 0;
                                for (qualified_name, 0..) |c, i| {
                                    if (c == '.') last_dot = i + 1;
                                }
                                break :blk qualified_name[last_dot..];
                            };

                            const qualified_ident = try idents.insert(gpa, Ident.for_text(qualified_name));
                            const relative_ident = try idents.insert(gpa, Ident.for_text(relative_name));
                            const display_ident = try idents.insert(gpa, Ident.for_text(display_name));

                            // Add mapping for qualified_name -> display_name
                            if (mapping.get(qualified_ident)) |existing_ident| {
                                const existing_name = idents.getText(existing_ident);
                                if (displayNameIsBetter(display_name, existing_name)) {
                                    try mapping.put(qualified_ident, display_ident);
                                }
                            } else {
                                try mapping.put(qualified_ident, display_ident);
                            }

                            // Also add mapping for relative_name -> display_name
                            // This ensures types stored with relative_name (like "Num.Numeral") also map to display_name
                            if (mapping.get(relative_ident)) |existing_ident| {
                                const existing_name = idents.getText(existing_ident);
                                if (displayNameIsBetter(display_name, existing_name)) {
                                    try mapping.put(relative_ident, display_ident);
                                }
                            } else {
                                try mapping.put(relative_ident, display_ident);
                            }
                        }
                        // else: Skip non-nominal/alias statements (e.g., nested types that aren't directly importable)
                    }
                }
            }
        }
    }

    // Step 2: Copy user import mappings from the ModuleEnv
    // These were built during canonicalization when processing import statements
    var iter = cir.import_mapping.iterator();
    while (iter.next()) |entry| {
        const qualified_ident = entry.key_ptr.*;
        const local_ident = entry.value_ptr.*;

        // Get the text for comparison
        const local_name = cir.getIdentText(local_ident);

        if (mapping.get(qualified_ident)) |existing_display| {
            // Only replace if the new name is "better"
            const existing_name = idents.getText(existing_display);
            if (displayNameIsBetter(local_name, existing_name)) {
                try mapping.put(qualified_ident, local_ident);
            }
        } else {
            try mapping.put(qualified_ident, local_ident);
        }
    }

    _ = auto_imported_types; // Not needed anymore - mapping is built during canonicalization

    return mapping;
}

/// Determine if `new_name` is a "better" display name than `existing_name`.
/// Returns true if new_name should replace existing_name.
///
/// The rules are:
/// 1. Shorter names are better (fewer characters to read in error messages)
/// 2. For equal lengths, lexicographically smaller wins (deterministic regardless of import order)
pub fn displayNameIsBetter(new_name: []const u8, existing_name: []const u8) bool {
    // Shorter is better
    if (new_name.len != existing_name.len) {
        return new_name.len < existing_name.len;
    }
    // Equal length: lexicographic comparison (lower byte value wins)
    for (new_name, existing_name) |new_byte, existing_byte| {
        if (new_byte != existing_byte) {
            return new_byte < existing_byte;
        }
    }
    // Identical strings - no replacement needed
    return false;
}
