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
const snapshot_mod = @import("snapshot.zig");

const ExposedItems = collections.ExposedItems;
const CIR = can.CIR;
const CommonEnv = base.CommonEnv;
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
const Rank = types_mod.Rank;
const Mark = types_mod.Mark;
const Num = types_mod.Num;
const testing = std.testing;
const Instantiator = types_mod.instantiate.Instantiator;
const Generalizer = types_mod.generalize.Generalizer;
const VarPool = types_mod.generalize.VarPool;
const SnapshotStore = @import("snapshot.zig").Store;
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
/// A list of regions. Parallel with type vars & CIR nodes
regions: *Region.List,
/// List of directly imported  module. Import indexes in CIR refer to this list
imported_modules: []const *const ModuleEnv,
/// Map of module name identifiers to their env. This includes all modules
/// "below" this one in the dependency graph
module_envs: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
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
/// free vars collected when generation types from annotation
anno_free_vars: base.Scratch(FreeVar),
/// free vars collected when generation types from type decls
decl_free_vars: base.Scratch(FreeVar),
/// annos we've already seen when generation a type from an annotation
seen_annos: std.AutoHashMap(CIR.TypeAnno.Idx, Var),
/// A pool of solver envs
env_pool: EnvPool,
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
/// scratch static dispatch constraints used to build up intermediate lists, used for various things
scratch_static_dispatch_constraints: base.Scratch(ScratchStaticDispatchConstraint),
/// Stack of type variables currently being constraint-checked, used to detect recursive constraints
/// When a var appears in this stack while we're checking its constraints, we've detected recursion
constraint_check_stack: std.ArrayList(Var),
// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,
/// Maps variables to the expressions that constrained them (for better error regions)
constraint_origins: std.AutoHashMap(Var, Var),
/// Copied Bool type from Bool module (for use in if conditions, etc.)
bool_var: Var,
/// Copied Str type from Builtin module (for use in string literals, etc.)
str_var: Var,
/// Map representation of Ident -> Var, used in checking static dispatch constraints
ident_to_var_map: std.AutoHashMap(Ident.Idx, Var),
/// Map representation all top level patterns, and if we've processed them yet
top_level_ptrns: std.AutoHashMap(CIR.Pattern.Idx, DefProcessed),
/// The expected return type of the enclosing function, if any.
/// Used to correctly type-check `return` expressions inside loops etc.
enclosing_func_return_type: ?Var,
/// Type writer for formatting types at snapshot time
type_writer: types_mod.TypeWriter,

/// A map of rigid variables that we build up during a branch of type checking
const FreeVar = struct { ident: base.Ident.Idx, var_: Var };

/// A def + processing data
const DefProcessed = struct { def_idx: CIR.Def.Idx, status: HasProcessed };

/// Indicates if something has been processed or not
const HasProcessed = enum { processed, processing, not_processed };

/// A struct scratch info about a static dispatch constraint
const ScratchStaticDispatchConstraint = struct {
    var_: Var,
    constraint: types_mod.StaticDispatchConstraint,
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
    module_envs: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    regions: *Region.List,
    builtin_ctx: BuiltinContext,
) std.mem.Allocator.Error!Self {
    const mutable_cir = @constCast(cir);
    var import_mapping = try createImportMapping(
        gpa,
        mutable_cir.getIdentStore(),
        cir,
        builtin_ctx.builtin_module,
        builtin_ctx.builtin_indices,
        module_envs,
    );
    errdefer import_mapping.deinit();

    return .{
        .gpa = gpa,
        .types = types,
        .cir = mutable_cir,
        .imported_modules = imported_modules,
        .module_envs = module_envs,
        .regions = regions,
        .builtin_ctx = builtin_ctx,
        .snapshots = try SnapshotStore.initCapacity(gpa, 512),
        .problems = try ProblemStore.initCapacity(gpa, 64),
        .import_mapping = import_mapping,
        .unify_scratch = try unifier.Scratch.init(gpa),
        .occurs_scratch = try occurs.Scratch.init(gpa),
        .anno_free_vars = try base.Scratch(FreeVar).init(gpa),
        .decl_free_vars = try base.Scratch(FreeVar).init(gpa),
        .seen_annos = std.AutoHashMap(CIR.TypeAnno.Idx, Var).init(gpa),
        .env_pool = try EnvPool.init(gpa),
        .generalizer = try Generalizer.init(gpa, types),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .rigid_var_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){},
        .scratch_vars = try base.Scratch(types_mod.Var).init(gpa),
        .scratch_tags = try base.Scratch(types_mod.Tag).init(gpa),
        .scratch_record_fields = try base.Scratch(types_mod.RecordField).init(gpa),
        .scratch_static_dispatch_constraints = try base.Scratch(ScratchStaticDispatchConstraint).init(gpa),
        .constraint_check_stack = try std.ArrayList(Var).initCapacity(gpa, 0),
        .import_cache = ImportCache{},
        .constraint_origins = std.AutoHashMap(Var, Var).init(gpa),
        .bool_var = undefined, // Will be initialized in copyBuiltinTypes()
        .str_var = undefined, // Will be initialized in copyBuiltinTypes()
        .ident_to_var_map = std.AutoHashMap(Ident.Idx, Var).init(gpa),
        .top_level_ptrns = std.AutoHashMap(CIR.Pattern.Idx, DefProcessed).init(gpa),
        .enclosing_func_return_type = null,
        // Initialize with null import_mapping - caller should call fixupTypeWriter() after storing Check
        .type_writer = try types_mod.TypeWriter.initFromParts(gpa, types, mutable_cir.getIdentStore(), null),
    };
}

/// Call this after Check has been stored at its final location to set up the import_mapping pointer.
/// This is needed because returning Check by value invalidates the pointer set during init.
pub fn fixupTypeWriter(self: *Self) void {
    self.type_writer.setImportMapping(&self.import_mapping);
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.import_mapping.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.anno_free_vars.deinit();
    self.decl_free_vars.deinit();
    self.seen_annos.deinit();
    self.env_pool.deinit();
    self.generalizer.deinit(self.gpa);
    self.var_map.deinit();
    self.rigid_var_substitutions.deinit(self.gpa);
    self.scratch_vars.deinit();
    self.scratch_tags.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_static_dispatch_constraints.deinit();
    self.constraint_check_stack.deinit(self.gpa);
    self.import_cache.deinit(self.gpa);
    self.constraint_origins.deinit();
    self.ident_to_var_map.deinit();
    self.top_level_ptrns.deinit();
    self.type_writer.deinit();
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
        _ = self.types.appendFromContentAssumeCapacity(.{ .flex = Flex.init() }, @enumFromInt(15));
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
    fn reset(self: *Env) void {
        self.var_pool.current_rank = .generalized;
        self.var_pool.clearRetainingCapacity();
        self.deferred_static_dispatch_constraints.items.clearRetainingCapacity();
    }

    fn rank(self: *const Env) Rank {
        return self.var_pool.current_rank;
    }
};

// unify //

/// Unify two types where `a` is the expected type and `b` is the actual type
fn unify(self: *Self, a: Var, b: Var, env: *Env) std.mem.Allocator.Error!unifier.Result {
    return self.unifyWithCtx(a, b, env, .anon);
}

/// Unify two types where `a` is the expected type and `b` is the actual type
/// In error messages, this function will indicate that `a` as "from an annotation"
fn unifyFromAnno(self: *Self, a: Var, b: Var, env: *Env) std.mem.Allocator.Error!unifier.Result {
    return self.unifyWithCtx(a, b, env, .anno);
}

/// Unify two types where `a` is the expected type and `b` is the actual type
/// Accepts a config that indicates if `a` is from an annotation or not
fn unifyWithCtx(self: *Self, a: Var, b: Var, env: *Env, ctx: unifier.Conf.Ctx) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Before unification, check if either variable has constraint origins
    // We need to look up constraint origins by walking through the type structure
    const constraint_origin_var = self.findConstraintOriginForVars(a, b);

    // Unify
    const result = try unifier.unifyWithConf(
        self.cir,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.type_writer,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        unifier.Conf{
            .ctx = ctx,
            .constraint_origin_var = constraint_origin_var,
        },
    );

    // After successful unification, propagate constraint origins to both variables
    if (result == .ok) {
        if (constraint_origin_var) |origin| {
            try self.constraint_origins.put(a, origin);
            try self.constraint_origins.put(b, origin);
        }
    }

    // Set regions and add to the current rank all variables created during unification
    //
    // TODO: Setting all fresh var regions to be the same as the root var region
    // is fine if this unification doesn't go very deep (ie doesn't recurse
    // that much).
    //
    // But if it does, this region may be imprecise. We can explore
    // ways around this (like maybe capurting the origin var for each of unify's
    // fresh var) and setting region that way
    //
    // Note that we choose `b`s region here, since `b` is the "actual" type
    // (whereas `a` is the "expected" type, like from an annotation)
    const region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(b));
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
    // if (self.constraint_origins.count() > 0) {
    //     var it = self.constraint_origins.iterator();
    //     while (it.next()) |entry| {
    //         const origin = entry.value_ptr.*;
    //         // Return the first constraint origin we find - this is specifically for the Color.md case
    //         // where constraint origins exist but don't directly match the unification variables
    //         return origin;
    //     }
    // }

    return null;
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
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = env.rank(),
        .rigid_behavior = .fresh_flex,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}

/// Instantiate a variable, substituting any encountered rigids with *new* rigid vars
///
/// Note that the the rigid var structure will be preserved.
/// E.g. `a -> a`, `a` will reference the same new rigid var
fn instantiateVarPreserveRigids(
    self: *Self,
    var_to_instantiate: Var,
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = env.rank(),
        .rigid_behavior = .fresh_rigid,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}

/// Instantiate a variable
fn instantiateVarWithSubs(
    self: *Self,
    var_to_instantiate: Var,
    subs: *std.AutoHashMapUnmanaged(Ident.Idx, Var),
    env: *Env,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
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
    const try_type_content = try self.mkTryContent(flex_var, err_var);
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

    return flex_var;
}

/// Create a nominal Box type with the given element type
fn mkBoxContent(self: *Self, elem_var: Var) Allocator.Error!Content {
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

/// Create a nominal Try type with the given success and error types
fn mkTryContent(self: *Self, ok_var: Var, err_var: Var) Allocator.Error!Content {
    // Use the cached builtin_module_ident from the current module's ident store.
    // This represents the "Builtin" module where Try is defined.
    const origin_module_id = if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name; // We're compiling Builtin module itself

    // Use the relative name "Try" (not "Builtin.Try") to match the relative_name in TypeHeader
    // The origin_module field already captures that this type is from Builtin
    const try_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.builtin_try,
    };

    // The backing var doesn't matter here. Nominal types unify based on their ident
    // and type args only - the backing is never examined during unification.
    // Creating the real backing type ([Ok(ok), Err(err)]) would be a waste of time.
    const backing_var = ok_var;
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
    const resolved_target = self.types.resolveVar(target_var);
    if (resolved_target.is_root and resolved_target.desc.rank == env.rank() and resolved_target.desc.content == .flex) {
        // The vast majority of the time, we call unify with on a placeholder
        // CIR var. In this case, we can safely override the type descriptor
        // directly, saving a typeslot and unifcation run
        var desc = resolved_target.desc;
        desc.content = content;
        try self.types.setVarDesc(target_var, desc);
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

/// Give a var, ensure it's not a redirect and set it's rank
fn setVarRank(self: *Self, target_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(target_var);
    if (resolved.is_root) {
        self.types.setDescRank(resolved.desc_idx, env.rank());
        try env.var_pool.addVarToRank(target_var, env.rank());
    } else {
        // TODO: Unclear if this is an error or not
        // try self.unifyWith(target_var, .err, env);
    }
}

// file //

/// Check the types for all defs
/// Copy builtin types from their modules into the current module's type store
/// This is necessary because type variables are module-specific - we can't use Vars from
/// other modules directly. The Bool and Try types are used in language constructs like
/// `if` conditions and need to be available in every module's type store.
fn copyBuiltinTypes(self: *Self) !void {
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

/// Check the types for all defs in a file
pub fn checkFile(self: *Self) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Fill in types store up to the size of CIR nodes
    try ensureTypeStoreIsFilled(self);

    // Create a solver env
    var env = try self.env_pool.acquire(.generalized);
    defer self.env_pool.release(env);

    // TODO: Generating type from type stmts writes types into the env, but i
    // don't think it _needs_ to. We reset before solving each def. We may be able
    // to save some perf by not passing `env` into the type stmt functions

    // Copy builtin types (Bool, Try) into this module's type store
    try self.copyBuiltinTypes();

    // First, iterate over the builtin statements, generating types for each type declaration
    const builtin_stmts_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (builtin_stmts_slice) |builtin_stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(builtin_stmt_idx, &env);
    }

    const stmts_slice = self.cir.store.sliceStatements(self.cir.all_statements);

    // First pass: generate types for each type declaration
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
            .s_type_anno => |_| {
                // TODO: Handle standalone type annotations
                try self.unifyWith(stmt_var, .err, &env);
            },
            else => {
                // All other stmt types are invalid at the top level
            },
        }
    }

    // Next, capture all top level defs
    // This is used to support out-of-order defts
    const defs_slice = self.cir.store.sliceDefs(self.cir.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.cir.store.getDef(def_idx);
        try self.top_level_ptrns.put(def.pattern, .{ .def_idx = def_idx, .status = .not_processed });
    }

    // Then, iterate over defs again, inferring types
    for (defs_slice) |def_idx| {
        env.reset();
        try self.checkDef(def_idx, &env);
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
                env.reset();

                // Enter a new rank for this expect
                try env.var_pool.pushRank();
                defer env.var_pool.popRank();

                // Check the body expression
                _ = try self.checkExpr(expr_stmt.body, &env, .no_expectation);
                const body_var: Var = ModuleEnv.varFrom(expr_stmt.body);

                // Unify with Bool (expects must be bool expressions)
                const bool_var = try self.freshBool(&env, stmt_region);
                _ = try self.unify(bool_var, body_var, &env);

                // Unify statement var with body var
                _ = try self.unify(stmt_var, body_var, &env);

                // Generalize and check deferred constraints
                try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());
                try self.checkDeferredStaticDispatchConstraints(&env);
            },
            else => {
                // Other statement types are handled elsewhere (type decls, defs, etc.)
            },
        }
    }

    // Note that we can't use SCCs to determine the order to resolve defs
    // because anonymous static dispatch makes function order not knowable
    // before type inference

    // Process requires_types annotations for platforms
    // This ensures the type store has the actual types for platform requirements
    try self.processRequiresTypes(&env);
}

/// Process the requires_types annotations for platform modules.
/// This generates the actual types from the type annotations stored in requires_types.
fn processRequiresTypes(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    const requires_types_slice = self.cir.requires_types.items.items;
    for (requires_types_slice) |required_type| {
        // Generate the type from the annotation
        try self.generateAnnoTypeInPlace(required_type.type_anno, env, .annotation);
    }
}

/// Check that the app's exported values match the platform's required types.
/// This should be called after checkFile() to verify that app exports conform
/// to the platform's requirements.
/// The `platform_to_app_idents` map translates platform ident indices to app ident indices,
/// built by the caller to avoid string lookups during type checking.
pub fn checkPlatformRequirements(
    self: *Self,
    platform_env: *const ModuleEnv,
    platform_to_app_idents: *const std.AutoHashMap(Ident.Idx, Ident.Idx),
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Create a solver env for type operations
    var env = try self.env_pool.acquire(.generalized);
    defer self.env_pool.release(env);

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
            const instantiated_required_var = try self.instantiateVar(copied_required_var, &env, .{ .explicit = required_type.region });

            // Unify the platform's required type with the app's export type.
            // This constrains type variables in the export (e.g., closure params)
            // to match the platform's expected types.
            _ = try self.unifyFromAnno(instantiated_required_var, export_var, &env);
        }
        // Note: If the export is not found, the canonicalizer should have already reported an error
    }
}

// repl //

/// Check an expr for the repl
pub fn checkExprRepl(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    try ensureTypeStoreIsFilled(self);

    // Copy builtin types into this module's type store
    try self.copyBuiltinTypes();

    // Create a solver env
    var env = try self.env_pool.acquire(.generalized);
    defer self.env_pool.release(env);

    // First, iterate over the statements, generating types for each type declaration
    const stms_slice = self.cir.store.sliceStatements(self.cir.builtin_statements);
    for (stms_slice) |stmt_idx| {
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(stmt_idx, &env);
    }

    {
        try env.var_pool.pushRank();
        defer env.var_pool.popRank();

        // Check the expr
        _ = try self.checkExpr(expr_idx, &env, .no_expectation);

        // Now that we are existing the scope, we must generalize then pop this rank
        try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

        // Check any accumulated static dispatch constraints
        try self.checkDeferredStaticDispatchConstraints(&env);
    }
}

// defs //

/// Check the types for a single definition
fn checkDef(self: *Self, def_idx: CIR.Def.Idx, env: *Env) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Ensure that initiailly we're at the generalized level
    std.debug.assert(env.rank() == .generalized);

    const def = self.cir.store.getDef(def_idx);
    const def_var = ModuleEnv.varFrom(def_idx);
    const ptrn_var = ModuleEnv.varFrom(def.pattern);
    const expr_var = ModuleEnv.varFrom(def.expr);

    if (self.top_level_ptrns.get(def.pattern)) |processing_def| {
        if (processing_def.status == .processed) {
            // If we've already processed this def, return immediately
            return;
        }
    }

    // Make as processing
    try self.top_level_ptrns.put(def.pattern, .{ .def_idx = def_idx, .status = .processing });

    {
        try env.var_pool.pushRank();
        defer env.var_pool.popRank();

        std.debug.assert(env.rank() == .top_level);

        try self.setVarRank(def_var, env);
        try self.setVarRank(ptrn_var, env);
        try self.setVarRank(expr_var, env);

        // Check the pattern
        try self.checkPattern(def.pattern, env, .no_expectation);

        // Handle if there's an annotation associated with this def
        if (def.annotation) |annotation_idx| {
            // Generate the annotation type
            self.anno_free_vars.items.clearRetainingCapacity();
            try self.generateAnnotationType(annotation_idx, env);
            const annotation_var = ModuleEnv.varFrom(annotation_idx);

            // TODO: If we instantiate here, then var lookups break. But if we don't
            // then the type anno gets corrupted if we have an error in the body
            // const instantiated_anno_var = try self.instantiateVarPreserveRigids(
            //     annotation_var,
            //     rank,
            //     .use_last_var,
            // );

            // Infer types for the body, checking against the instantaited annotation
            _ = try self.checkExpr(def.expr, env, .{
                .expected = .{ .var_ = annotation_var, .from_annotation = true },
            });

            // Check that the annotation matches the definition
            _ = try self.unify(annotation_var, def_var, env);
        } else {
            // Check the expr
            _ = try self.checkExpr(def.expr, env, .no_expectation);
        }

        // Now that we are existing the scope, we must generalize then pop this rank
        try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

        // Check any accumulated static dispatch constraints
        try self.checkDeferredStaticDispatchConstraints(env);

        // Check that the ptrn and the expr match
        _ = try self.unify(ptrn_var, expr_var, env);

        // Check that the def and ptrn match
        _ = try self.unify(def_var, ptrn_var, env);
    }

    // Mark as processed
    try self.top_level_ptrns.put(def.pattern, .{ .def_idx = def_idx, .status = .processed });
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
    const decl_free_vars_top = self.decl_free_vars.top();
    defer self.decl_free_vars.clearFrom(decl_free_vars_top);

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

    try self.unifyWith(
        decl_var,
        try self.types.mkAlias(
            .{ .ident_idx = header.relative_name },
            backing_var,
            header_vars,
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

/// Generate types for type anno args
fn generateHeaderVars(
    self: *Self,
    header_args: []CIR.TypeAnno.Idx,
    env: *Env,
) std.mem.Allocator.Error![]Var {
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
                // This should never be possible
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
    _ = try self.unify(ModuleEnv.varFrom(annotation_idx), ModuleEnv.varFrom(annotation.anno), env);
}

/// Given a where clause, generate static dispatch constraints and add to scratch_static_dispatch_constraints
fn generateStaticDispatchConstraintFromWhere(self: *Self, where_idx: CIR.WhereClause.Idx, env: *Env) std.mem.Allocator.Error!void {
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
        .w_alias => {
            // TODO: Recursively unwrap alias
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
                    // TODO: Don't generate a new type var here, reuse anno var
                    const builtin_var = try self.generateBuiltinTypeInstance(lookup.name, builtin_type, &.{}, anno_region, env);
                    _ = try self.unify(anno_var, builtin_var, env);
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
                                try self.unifyWith(anno_var, blk: {
                                    switch (this_decl.type_) {
                                        .alias => {
                                            // TODO: Recursion is not allowed in aliases.
                                            //
                                            // If this type i used anywhere,
                                            // then the user _should_ get an
                                            // error, but we should proactively
                                            // emit on here too
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
                                                self.builtin_ctx.module_name,
                                                false, // Default to non-opaque for error case
                                            );
                                        },
                                    }
                                }, env);

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
                        env,
                        .{ .explicit = anno_region },
                    );
                    _ = try self.unify(anno_var, instantiated_var, env);
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
                    // TODO: Don't generate a new type var here, reuse anno var
                    const builtin_var = try self.generateBuiltinTypeInstance(a.name, builtin_type, anno_arg_vars, anno_region, env);
                    _ = try self.unify(anno_var, builtin_var, env);
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
                                try self.unifyWith(anno_var, blk: {
                                    switch (this_decl.type_) {
                                        .alias => {
                                            // TODO: Recursion is not allowed in aliases.
                                            //
                                            // If this type i used anywhere,
                                            // then the user _should_ get an
                                            // error, but we should proactively
                                            // emit on here too
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
                                                self.builtin_ctx.module_name,
                                                false, // Default to non-opaque for error case
                                            );
                                        },
                                    }
                                }, env);

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
                                .flex, .rigid, .recursion_var => {
                                    // External type resolved to a flex, rigid, or recursion var.
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
            // This indicates a malformed type annotation. Tags should only
            // exist as direct children of tag_unions
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

            // Process the ext if it exists. Absence means it's a closed union
            // TODO: Capture ext in record field CIR
            // const ext_var = inner_blk: {
            //     if (rec.ext) |ext_anno_idx| {
            //         try self.generateAnnoType(rigid_vars_ctx, ext_anno_idx);
            //         break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
            //     } else {
            //         break :inner_blk try self.freshFromContent(.{ .structure = .empty_record }, rank, anno_region);
            //     }
            // };
            const ext_var = try self.freshFromContent(.{ .structure = .empty_record }, env, anno_region);

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

/// Generate a type variable from the builtin
///
/// Writes the resulting type into the slot at `ret_var`
fn generateBuiltinTypeInstance(
    self: *Self,
    anno_builtin_name: Ident.Idx,
    anno_builtin_type: CIR.TypeAnno.Builtin,
    anno_args: []Var,
    anno_region: Region,
    env: *Env,
) std.mem.Allocator.Error!Var {
    switch (anno_builtin_type) {
        // Phase 5: Use nominal types from Builtin instead of special .num content
        .u8 => return try self.freshFromContent(try self.mkNumberTypeContent("U8", env), env, anno_region),
        .u16 => return try self.freshFromContent(try self.mkNumberTypeContent("U16", env), env, anno_region),
        .u32 => return try self.freshFromContent(try self.mkNumberTypeContent("U32", env), env, anno_region),
        .u64 => return try self.freshFromContent(try self.mkNumberTypeContent("U64", env), env, anno_region),
        .u128 => return try self.freshFromContent(try self.mkNumberTypeContent("U128", env), env, anno_region),
        .i8 => return try self.freshFromContent(try self.mkNumberTypeContent("I8", env), env, anno_region),
        .i16 => return try self.freshFromContent(try self.mkNumberTypeContent("I16", env), env, anno_region),
        .i32 => return try self.freshFromContent(try self.mkNumberTypeContent("I32", env), env, anno_region),
        .i64 => return try self.freshFromContent(try self.mkNumberTypeContent("I64", env), env, anno_region),
        .i128 => return try self.freshFromContent(try self.mkNumberTypeContent("I128", env), env, anno_region),
        .f32 => return try self.freshFromContent(try self.mkNumberTypeContent("F32", env), env, anno_region),
        .f64 => return try self.freshFromContent(try self.mkNumberTypeContent("F64", env), env, anno_region),
        .dec => return try self.freshFromContent(try self.mkNumberTypeContent("Dec", env), env, anno_region),
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
                return try self.freshFromContent(.err, env, anno_region);
            }

            // Create the nominal List type
            const list_content = try self.mkListContent(anno_args[0], env);
            return try self.freshFromContent(list_content, env, anno_region);
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
                return try self.freshFromContent(.err, env, anno_region);
            }

            // Create the nominal Box type
            const box_content = try self.mkBoxContent(anno_args[0]);
            return try self.freshFromContent(box_content, env, anno_region);
        },
        // Polymorphic number types (Num, Int, Frac) are no longer supported
        // They have been replaced with concrete nominal types (U8, I32, F64, Dec, etc.)
        .num, .int, .frac => {
            // Return error - these should not be used anymore
            return try self.freshFromContent(.err, env, anno_region);
        },
    }
}

// types //

const Expected = union(enum) {
    no_expectation,
    expected: struct { var_: Var, from_annotation: bool },
};

// pattern //

/// Check the types for the provided pattern, saving the type in-place
fn checkPattern(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    env: *Env,
    expected: Expected,
) std.mem.Allocator.Error!void {
    _ = try self.checkPatternHelp(pattern_idx, env, expected, .in_place);
}

/// Check the types for the provided pattern, either as fresh var or in-place
fn checkPatternHelp(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    env: *Env,
    expected: Expected,
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
            const var_ = try self.checkPatternHelp(p.pattern, env, expected, out_var);
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
                            const elem_var = try self.checkPatternHelp(single_elem_ptrn_idx, env, .no_expectation, out_var);
                            try self.scratch_vars.append(elem_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },
                    .in_place => {
                        // Check tuple elements
                        const elems_slice = self.cir.store.slicePatterns(tuple.patterns);
                        for (elems_slice) |single_elem_ptrn_idx| {
                            _ = try self.checkPatternHelp(single_elem_ptrn_idx, env, .no_expectation, out_var);
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
                const elem_var = try self.checkPatternHelp(elems[0], env, .no_expectation, out_var);

                // Iterate over the remaining elements
                var last_elem_ptrn_idx = elems[0];
                for (elems[1..], 1..) |elem_ptrn_idx, i| {
                    const cur_elem_var = try self.checkPatternHelp(elem_ptrn_idx, env, .no_expectation, out_var);

                    // Unify each element's var with the list's elem var
                    const result = try self.unify(elem_var, cur_elem_var, env);
                    self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_ptrn_idx),
                        .incompatible_elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
                    } });

                    // If we errored, check the rest of the elements without comparing
                    // to the elem_var to catch their individual errors
                    if (!result.isOk()) {
                        for (elems[i + 1 ..]) |remaining_elem_expr_idx| {
                            _ = try self.checkPatternHelp(remaining_elem_expr_idx, env, .no_expectation, out_var);
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
                        const rest_pattern_var = try self.checkPatternHelp(rest_pattern_idx, env, .no_expectation, out_var);

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
                            const arg_var = try self.checkPatternHelp(arg_expr_idx, env, .no_expectation, out_var);
                            try self.scratch_vars.append(arg_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },

                    .in_place => {
                        // Process each tag arg
                        const arg_ptrn_idx_slice = self.cir.store.slicePatterns(applied_tag.args);
                        for (arg_ptrn_idx_slice) |arg_expr_idx| {
                            _ = try self.checkPatternHelp(arg_expr_idx, env, .no_expectation, out_var);
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
        .nominal => |nominal| blk: {
            // TODO: Merge this with e_nominal_external

            // First, check the type inside the expr
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, env, .no_expectation, out_var);

            // Then, we need an instance of the nominal type being referenced
            // E.g. ConList.Cons(...)
            //      ^^^^^^^
            const nominal_var = try self.instantiateVar(ModuleEnv.varFrom(nominal.nominal_type_decl), env, .{ .explicit = pattern_region });
            const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

            if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                const nominal_type = nominal_resolved.structure.nominal_type;

                // If this nominal type is opaque and we're not in the defining module
                // then report an error
                if (!nominal_type.canLiftInner(self.cir.module_name_idx)) {
                    _ = try self.problems.appendProblem(self.cir.gpa, .{ .cannot_access_opaque_nominal = .{
                        .var_ = pattern_var,
                        .nominal_type_name = nominal_type.ident.ident_idx,
                    } });

                    // Mark the entire expression as having a type error
                    try self.unifyWith(pattern_var, .err, env);
                    break :blk;
                }

                // Then, we extract the variable of the nominal type
                // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);

                // Now we unify what the user wrote with the backing type of the nominal was
                // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                const result = try self.unify(nominal_backing_var, actual_backing_var, env);

                // Then, we handle the result of unification
                switch (result) {
                    .ok => {
                        // If that unify call succeeded, then we this is a valid instance
                        // of this nominal type. So we set the expr's type to be the
                        // nominal type
                        _ = try self.unify(pattern_var, nominal_var, env);
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
                        try self.unifyWith(pattern_var, .err, env);
                    },
                }
            } else {
                // If the nominal type is actually something else, then set the
                // whole expression to be an error.
                //
                // TODO: Report a nice problem here
                try self.unifyWith(pattern_var, .err, env);
            }
        },
        .nominal_external => |nominal| blk: {
            // TODO: Merge this with e_nominal

            // First, check the type inside the expr
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, env, .no_expectation, out_var);

            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Then, we need an instance of the nominal type being referenced
                // E.g. ConList.Cons(...)
                //      ^^^^^^^
                const nominal_var = try self.instantiateVar(ext_ref.local_var, env, .{ .explicit = pattern_region });
                const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

                if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                    const nominal_type = nominal_resolved.structure.nominal_type;

                    // If this nominal type is opaque and we're not in the defining module
                    // then report an error
                    if (!nominal_type.canLiftInner(self.cir.module_name_idx)) {
                        _ = try self.problems.appendProblem(self.cir.gpa, .{ .cannot_access_opaque_nominal = .{
                            .var_ = pattern_var,
                            .nominal_type_name = nominal_type.ident.ident_idx,
                        } });

                        // Mark the entire expression as having a type error
                        try self.unifyWith(pattern_var, .err, env);
                        break :blk;
                    }

                    // Then, we extract the variable of the nominal type
                    // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                    //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);

                    // Now we unify what the user wrote with the backing type of the nominal was
                    // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                    //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const result = try self.unify(nominal_backing_var, actual_backing_var, env);

                    // Then, we handle the result of unification
                    switch (result) {
                        .ok => {
                            // If that unify call succeeded, then we this is a valid instance
                            // of this nominal type. So we set the expr's type to be the
                            // nominal type
                            _ = try self.unify(pattern_var, nominal_var, env);
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
                            try self.unifyWith(pattern_var, .err, env);
                        },
                    }
                } else {
                    // If the nominal type is actually something else, then set the
                    // whole expression to be an error.
                    //
                    // TODO: Report a nice problem here
                    try self.unifyWith(pattern_var, .err, env);
                }
            } else {
                try self.unifyWith(pattern_var, .err, env);
            }
        },
        // record destructure //
        .record_destructure => |destructure| {
            const scratch_records_top = self.scratch_record_fields.top();
            defer self.scratch_record_fields.clearFrom(scratch_records_top);

            for (self.cir.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                const destruct_var = ModuleEnv.varFrom(destruct_idx);
                try self.setVarRank(destruct_var, env);

                // Check the sub pattern
                const field_pattern_var = blk: {
                    switch (destruct.kind) {
                        .Required => |sub_pattern_idx| {
                            break :blk try self.checkPatternHelp(sub_pattern_idx, env, .no_expectation, out_var);
                        },
                        .SubPattern => |sub_pattern_idx| {
                            break :blk try self.checkPatternHelp(sub_pattern_idx, env, .no_expectation, out_var);
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
            try self.unifyWith(pattern_var, .{ .structure = .{
                .record_unbound = record_fields_range,
            } }, env);
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

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyWithCtx(expected_type.var_, pattern_var, env, .anno);
            } else {
                _ = try self.unify(expected_type.var_, pattern_var, env);
            }
        },
    }

    return pattern_var;
}

// expr //

fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_var = ModuleEnv.varFrom(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));

    // Set the rank of the expr var, if it is not a lambda
    //
    // Lambdas push a new rank, so the var must be added to _that_ rank
    if (expr != .e_lambda) {
        try self.setVarRank(expr_var, env);
    }

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
                        // Interpolated expressions must be of type Str
                        const seg_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(seg_expr_idx));
                        const expected_str_var = try self.freshStr(env, seg_region);
                        does_fx = try self.checkExpr(seg_expr_idx, env, .{ .expected = .{ .var_ = expected_str_var, .from_annotation = false } }) or does_fx;

                        // Unify the segment's type with Str to produce a type error if it doesn't match
                        const seg_var = ModuleEnv.varFrom(seg_expr_idx);
                        const unify_result = try self.unify(seg_var, expected_str_var, env);
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
                // If any segment errored, propgate that error to the root string
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
                    _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
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

                _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
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

                _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
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

                _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
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

                _ = try self.cir.deferred_numeric_literals.append(self.gpa, .{
                    .expr_idx = expr_idx,
                    .type_var = flex_var,
                    .constraint = constraint,
                    .region = expr_region,
                });
            }
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
                    const result = try self.unify(elem_var, cur_elem_var, env);
                    self.setDetailIfTypeMismatch(result, problem.TypeMismatchDetail{ .incompatible_list_elements = .{
                        .last_elem_idx = ModuleEnv.nodeIdxFrom(last_elem_expr_idx),
                        .incompatible_elem_index = @intCast(i),
                        .list_length = @intCast(elems.len),
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

            // Check if this is a record update
            if (e.ext) |record_being_updated_expr| {
                // Create an unbound record with the provided fields
                const ext_var = try self.fresh(env, expr_region);
                try self.unifyWith(expr_var, .{ .structure = .{
                    .record = .{
                        .fields = record_fields_range,
                        .ext = ext_var,
                    },
                } }, env);

                does_fx = try self.checkExpr(record_being_updated_expr, env, .no_expectation) or does_fx;
                const record_being_updated_var = ModuleEnv.varFrom(record_being_updated_expr);

                _ = try self.unify(record_being_updated_var, expr_var, env);
            } else {
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
        .e_nominal => |nominal| blk: {
            // TODO: Merge this with e_nominal_external

            // First, check the type inside the expr
            does_fx = try self.checkExpr(nominal.backing_expr, env, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            // Then, we need an instance of the nominal type being referenced
            // E.g. ConList.Cons(...)
            //      ^^^^^^^
            const nominal_var = try self.instantiateVar(ModuleEnv.varFrom(nominal.nominal_type_decl), env, .{ .explicit = expr_region });
            const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

            if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                const nominal_type = nominal_resolved.structure.nominal_type;

                // If this nominal type is opaque and we're not in the defining module
                // then report an error
                if (!nominal_type.canLiftInner(self.cir.module_name_idx)) {
                    _ = try self.problems.appendProblem(self.cir.gpa, .{ .cannot_access_opaque_nominal = .{
                        .var_ = expr_var,
                        .nominal_type_name = nominal_type.ident.ident_idx,
                    } });

                    // Mark the entire expression as having a type error
                    try self.unifyWith(expr_var, .err, env);
                    break :blk;
                }

                // Then, we extract the variable of the nominal type
                // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);

                // Now we unify what the user wrote with the backing type of the nominal was
                // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                const result = try self.unify(nominal_backing_var, actual_backing_var, env);

                // Then, we handle the result of unification
                switch (result) {
                    .ok => {
                        // If that unify call succeeded, then we this is a valid instance
                        // of this nominal type. So we set the expr's type to be the
                        // nominal type
                        _ = try self.unify(expr_var, nominal_var, env);
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
                        try self.unifyWith(expr_var, .err, env);
                    },
                }
            } else {
                // If the nominal type is actually something else, then set the
                // whole expression to be an error.
                //
                // TODO: Report a nice problem here
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_nominal_external => |nominal| blk: {
            // TODO: Merge this with e_nominal

            // First, check the type inside the expr
            does_fx = try self.checkExpr(nominal.backing_expr, env, .no_expectation) or does_fx;
            const actual_backing_var = ModuleEnv.varFrom(nominal.backing_expr);

            if (try self.resolveVarFromExternal(nominal.module_idx, nominal.target_node_idx)) |ext_ref| {
                // Then, we need an instance of the nominal type being referenced
                // E.g. ConList.Cons(...)
                //      ^^^^^^^
                const nominal_var = try self.instantiateVar(ext_ref.local_var, env, .{ .explicit = expr_region });
                const nominal_resolved = self.types.resolveVar(nominal_var).desc.content;

                if (nominal_resolved == .structure and nominal_resolved.structure == .nominal_type) {
                    const nominal_type = nominal_resolved.structure.nominal_type;

                    // If this nominal type is opaque and we're not in the defining module
                    // then report an error
                    if (!nominal_type.canLiftInner(self.cir.module_name_idx)) {
                        _ = try self.problems.appendProblem(self.cir.gpa, .{ .cannot_access_opaque_nominal = .{
                            .var_ = expr_var,
                            .nominal_type_name = nominal_type.ident.ident_idx,
                        } });

                        // Mark the entire expression as having a type error
                        try self.unifyWith(expr_var, .err, env);
                        break :blk;
                    }

                    // Then, we extract the variable of the nominal type
                    // E.g. ConList(a) := [Cons(a, ConstList), Nil]
                    //                    ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const nominal_backing_var = self.types.getNominalBackingVar(nominal_type);

                    // Now we unify what the user wrote with the backing type of the nominal was
                    // E.g. ConList.Cons(...) <-> [Cons(a, ConsList(a)), Nil]
                    //              ^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                    const result = try self.unify(nominal_backing_var, actual_backing_var, env);

                    // Then, we handle the result of unification
                    switch (result) {
                        .ok => {
                            // If that unify call succeeded, then we this is a valid instance
                            // of this nominal type. So we set the expr's type to be the
                            // nominal type
                            _ = try self.unify(expr_var, nominal_var, env);
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
                            try self.unifyWith(expr_var, .err, env);
                        },
                    }
                } else {
                    // If the nominal type is actually something else, then set the
                    // whole expression to be an error.
                    //
                    // TODO: Report a nice problem here
                    try self.unifyWith(expr_var, .err, env);
                }
            } else {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        // lookup //
        .e_lookup_local => |lookup| {
            const mb_processing_def = self.top_level_ptrns.get(lookup.pattern_idx);
            if (mb_processing_def) |processing_def| {
                switch (processing_def.status) {
                    .not_processed => {
                        var sub_env = try self.env_pool.acquire(.generalized);
                        defer self.env_pool.release(sub_env);

                        try self.checkDef(processing_def.def_idx, &sub_env);
                    },
                    .processing => {
                        // TODO: Handle recursive defs
                    },
                    .processed => {},
                }
            }

            const pat_var = ModuleEnv.varFrom(lookup.pattern_idx);
            const resolved_pat = self.types.resolveVar(pat_var).desc;

            if (resolved_pat.rank == Rank.generalized) {
                const instantiated = try self.instantiateVar(pat_var, env, .use_last_var);
                _ = try self.unify(expr_var, instantiated, env);
            } else {
                _ = try self.unify(expr_var, pat_var, env);
            }

            // Unify this expression with the referenced pattern
        },
        .e_lookup_external => |ext| {
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
            const anno_free_vars_top = self.anno_free_vars.top();
            defer self.anno_free_vars.clearFrom(anno_free_vars_top);

            // Check all statements in the block
            const statements = self.cir.store.sliceStatements(block.stmts);
            const stmt_result = try self.checkBlockStatements(statements, env, expr_region);
            does_fx = stmt_result.does_fx or does_fx;

            // Check the final expression
            does_fx = try self.checkExpr(block.final_expr, env, expected) or does_fx;

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

            {
                // Enter the next rank
                try env.var_pool.pushRank();
                defer env.var_pool.popRank();

                // IMPORTANT: expr_var must be added to the new rank, not the
                // outer rank
                try self.setVarRank(expr_var, env);

                // Check the argument patterns
                // This must happen *before* checking against the expected type so
                // all the pattern types are inferred
                const arg_pattern_idxs = self.cir.store.slicePatterns(lambda.args);
                for (arg_pattern_idxs) |pattern_idx| {
                    try self.checkPattern(pattern_idx, env, .no_expectation);
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

                                    const unify_result = try self.unify(arg_1, arg_2, env);
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
                                        _ = try self.unifyWith(expr_var, .err, env);
                                        break :for_blk;
                                    }
                                }
                            }
                        }

                        // Then, lastly, we unify the annotation types against the
                        // actual type
                        for (expected_func_args, arg_pattern_idxs) |expected_arg_var, pattern_idx| {
                            if (is_expected_from_anno) {
                                _ = try self.unifyWithCtx(expected_arg_var, ModuleEnv.varFrom(pattern_idx), env, .anno);
                            } else {
                                _ = try self.unify(expected_arg_var, ModuleEnv.varFrom(pattern_idx), env);
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
                // Also track the return type so `return` expressions can use it
                const saved_return_type = self.enclosing_func_return_type;
                if (mb_expected_func) |expected_func| {
                    self.enclosing_func_return_type = expected_func.ret;
                    does_fx = try self.checkExpr(lambda.body, env, .{
                        .expected = .{ .var_ = expected_func.ret, .from_annotation = is_expected_from_anno },
                    }) or does_fx;
                } else {
                    // When no expected type, the body's type becomes the return type.
                    // We need a fresh var so early returns can unify with it.
                    const body_var = ModuleEnv.varFrom(lambda.body);
                    self.enclosing_func_return_type = body_var;
                    does_fx = try self.checkExpr(lambda.body, env, .no_expectation) or does_fx;
                }
                self.enclosing_func_return_type = saved_return_type;
                const body_var = ModuleEnv.varFrom(lambda.body);

                // Unify all early returns with the body's return type.
                // This ensures that `return x` has the same type as the implicit return.
                try self.unifyEarlyReturns(lambda.body, body_var, env);

                // Create the function type
                if (does_fx) {
                    _ = try self.unifyWith(expr_var, try self.types.mkFuncEffectful(arg_vars, body_var), env);
                } else {
                    _ = try self.unifyWith(expr_var, try self.types.mkFuncUnbound(arg_vars, body_var), env);
                }

                // Now that we are existing the scope, we must generalize then pop this rank
                try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

                // Check any accumulated static dispatch constraints
                try self.checkDeferredStaticDispatchConstraints(env);
            }

            // Note that so far, we have not yet unified against the
            // annotation's effectfulness/pureness. This is intentional!
            // Below this large switch statement, there's the regular expr
            // <-> expected unification. This will catch any difference in
            // effectfullness, and it'll link the root expected var with the
            // expr_var

        },
        .e_closure => |closure| {
            does_fx = try self.checkExpr(closure.lambda_idx, env, expected) or does_fx;
            _ = try self.unify(expr_var, ModuleEnv.varFrom(closure.lambda_idx), env);
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
                        // If the fn or any args had error, propgate the error
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
                                    // std.debug.assert(expected_resolved_1.desc.content != .rigid);

                                    // Skip any concrete arguments
                                    if (expected_resolved_1.desc.content != .flex and expected_resolved_1.desc.content != .rigid) {
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

                                            const unify_result = try self.unify(arg_1, arg_2, env);
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
                                                _ = try self.unifyWith(expr_var, .err, env);
                                                break :blk;
                                            }
                                        }
                                    }
                                }

                                // Check the function's arguments against the actual
                                // called arguments, unifying each one
                                for (func_args, call_arg_expr_idxs, 0..) |expected_arg_var, call_expr_idx, arg_index| {
                                    const unify_result = try self.unify(expected_arg_var, ModuleEnv.varFrom(call_expr_idx), env);
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
                                        _ = try self.unifyWith(expr_var, .err, env);
                                        break :blk;
                                    }
                                }

                                // Redirect the expr to the function's return type
                                _ = try self.unify(expr_var, func.ret, env);
                            } else {
                                // TODO(jared): Better arity difference error message

                                // If the expected function's arity doesn't match
                                // the actual arguments provoided, unify the
                                // inferred function type with the expected function
                                // type to get  the regulare error message
                                const call_arg_vars: []Var = @ptrCast(call_arg_expr_idxs);
                                const call_func_ret = try self.fresh(env, expr_region);
                                const call_func_content = try self.types.mkFuncUnbound(call_arg_vars, call_func_ret);
                                const call_func_var = try self.freshFromContent(call_func_content, env, expr_region);

                                _ = try self.unify(func_var, call_func_var, env);
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
                    // No other call types are currently supported in czer
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
            does_fx = try self.checkBinopExpr(expr_idx, expr_region, env, binop, expected) or does_fx;
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
                    // propgate the error without doing any static dispatch work
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

                    // TODO Why do we have to create the static dispatch fn at the
                    // receiver rank instead of the  cur rank?

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
                const record_being_accessed = try self.freshFromContent(.{ .structure = .{
                    .record_unbound = record_field_range,
                } }, env, expr_region);

                // Then, unify the actual receiver type with the expected record
                _ = try self.unify(record_being_accessed, receiver_var, env);
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
        .e_inspect => |inspect| {
            // inspect evaluates its inner expression and returns Str
            // Note: does NOT set does_fx because inspect is pure
            _ = try self.checkExpr(inspect.expr, env, .no_expectation);
            const str_var = try self.freshStr(env, expr_region);
            _ = try self.unify(expr_var, str_var, env);
        },
        .e_expect => |expect| {
            does_fx = try self.checkExpr(expect.body, env, expected) or does_fx;
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_for => |for_expr| {
            // Check the pattern
            try self.checkPattern(for_expr.patt, env, .no_expectation);
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
                .expected => |expected_type| {
                    // Redirect expr_var to the annotation var so that lookups get the correct type
                    try self.types.setVarRedirect(expr_var, expected_type.var_);
                },
            }
        },
        .e_return => |ret| {
            // Early return expression - check the inner expression against enclosing function's return type
            // If we're inside a function, use its return type. Otherwise fall back to expected.
            const return_expected: Expected = if (self.enclosing_func_return_type) |ret_var|
                .{ .expected = .{ .var_ = ret_var, .from_annotation = false } }
            else
                expected;
            does_fx = try self.checkExpr(ret.expr, env, return_expected) or does_fx;
            // e_return "never returns" - it exits the function, so it can unify with any expected type.
            // This allows it to be used in if branches alongside other expressions.
            switch (expected) {
                .expected => |exp| {
                    _ = try self.unify(expr_var, exp.var_, env);
                },
                .no_expectation => {
                    // No expected type, leave expr_var as a flex var
                },
            }
        },
        .e_hosted_lambda => {
            // For hosted lambda expressions, the type comes from the annotation.
            // This is similar to e_anno_only - the implementation is provided by the host.
            switch (expected) {
                .no_expectation => {
                    // This shouldn't happen since hosted lambdas always have annotations
                    try self.unifyWith(expr_var, .err, env);
                },
                .expected => |expected_type| {
                    // Redirect expr_var to the annotation var so that lookups get the correct type
                    try self.types.setVarRedirect(expr_var, expected_type.var_);
                },
            }
        },
        .e_low_level_lambda => |ll| {
            // For low-level lambda expressions, treat like a lambda with a crash body.
            // Check the body (which will be e_runtime_error or similar)
            does_fx = try self.checkExpr(ll.body, env, .no_expectation) or does_fx;

            // The lambda's type comes from the annotation.
            // Like e_anno_only, this should always have an annotation.
            // The type will be unified with the expected type in the code below.
            switch (expected) {
                .no_expectation => unreachable,
                .expected => {
                    // The expr_var will be unified with the annotation var below
                },
            }
        },
        .e_runtime_error => {
            try self.unifyWith(expr_var, .err, env);
        },
    }

    // If we were provided with an expected type, unify against it
    switch (expected) {
        .no_expectation => {},
        .expected => |expected_type| {
            if (expected_type.from_annotation) {
                _ = try self.unifyWithCtx(expected_type.var_, expr_var, env, .anno);
            } else {
                _ = try self.unify(expected_type.var_, expr_var, env);
            }
        },
    }

    return does_fx;
}

// stmts //

const BlockStatementsResult = struct {
    does_fx: bool,
    diverges: bool,
};

/// Given a slice of stmts, type check each one
/// Returns whether any statement has effects and whether the block diverges (return/crash)
fn checkBlockStatements(self: *Self, statements: []const CIR.Statement.Idx, env: *Env, _: Region) std.mem.Allocator.Error!BlockStatementsResult {
    var does_fx = false;
    var diverges = false;
    for (statements) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);
        const stmt_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(stmt_idx));

        try self.setVarRank(stmt_var, env);

        switch (stmt) {
            .s_decl => |decl_stmt| {
                // Check the pattern
                try self.checkPattern(decl_stmt.pattern, env, .no_expectation);
                const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                // Evaluate the rhs of the expression
                const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);
                {
                    // Check the annotation, if it exists
                    const expectation = blk: {
                        if (decl_stmt.anno) |annotation_idx| {
                            // Generate the annotation type var in-place
                            try self.generateAnnotationType(annotation_idx, env);
                            const annotation_var = ModuleEnv.varFrom(annotation_idx);

                            // TODO: If we instantiate here, then var lookups break. But if we don't
                            // then the type anno gets corrupted if we have an error in the body
                            // const instantiated_anno_var = try self.instantiateVarPreserveRigids(
                            //     annotation_var,
                            //     rank,
                            //     .use_last_var,
                            // );

                            // Return the expectation
                            break :blk Expected{
                                .expected = .{ .var_ = annotation_var, .from_annotation = true },
                            };
                        } else {
                            break :blk Expected.no_expectation;
                        }
                    };

                    // Enter a new rank
                    try env.var_pool.pushRank();
                    defer env.var_pool.popRank();

                    does_fx = try self.checkExpr(decl_stmt.expr, env, expectation) or does_fx;

                    // Now that we are existing the scope, we must generalize then pop this rank
                    try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

                    // Check any accumulated static dispatch constraints
                    try self.checkDeferredStaticDispatchConstraints(env);
                }

                _ = try self.unify(decl_pattern_var, decl_expr_var, env);

                // Unify the pattern with the expression

                _ = try self.unify(stmt_var, decl_pattern_var, env);
            },
            .s_decl_gen => |decl_stmt| {
                // Generalized declarations (let-polymorphism) - handled same as s_decl for type checking
                // Check the pattern
                try self.checkPattern(decl_stmt.pattern, env, .no_expectation);
                const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                // Evaluate the rhs of the expression
                const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);
                {
                    // Check the annotation, if it exists
                    const expectation = blk: {
                        if (decl_stmt.anno) |annotation_idx| {
                            // Generate the annotation type var in-place
                            try self.generateAnnotationType(annotation_idx, env);
                            const annotation_var = ModuleEnv.varFrom(annotation_idx);

                            // Return the expectation
                            break :blk Expected{
                                .expected = .{ .var_ = annotation_var, .from_annotation = true },
                            };
                        } else {
                            break :blk Expected.no_expectation;
                        }
                    };

                    // Enter a new rank
                    try env.var_pool.pushRank();
                    defer env.var_pool.popRank();

                    does_fx = try self.checkExpr(decl_stmt.expr, env, expectation) or does_fx;

                    // Now that we are existing the scope, we must generalize then pop this rank
                    try self.generalizer.generalize(self.gpa, &env.var_pool, env.rank());

                    // Check any accumulated static dispatch constraints
                    try self.checkDeferredStaticDispatchConstraints(env);
                }

                _ = try self.unify(decl_pattern_var, decl_expr_var, env);

                // Unify the pattern with the expression
                _ = try self.unify(stmt_var, decl_pattern_var, env);
            },
            .s_var => |var_stmt| {
                // Check the pattern
                try self.checkPattern(var_stmt.pattern_idx, env, .no_expectation);
                const reassign_pattern_var: Var = ModuleEnv.varFrom(var_stmt.pattern_idx);

                does_fx = try self.checkExpr(var_stmt.expr, env, .no_expectation) or does_fx;
                const var_expr: Var = ModuleEnv.varFrom(var_stmt.expr);

                // Unify the pattern with the expression
                _ = try self.unify(reassign_pattern_var, var_expr, env);

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
                _ = try self.unify(reassign_pattern_var, reassign_expr_var, env);

                _ = try self.unify(stmt_var, reassign_expr_var, env);
            },
            .s_for => |for_stmt| {
                // Check the pattern
                // for item in [1,2,3] {
                //     ^^^^
                try self.checkPattern(for_stmt.patt, env, .no_expectation);
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

                const resolved = self.types.resolveVar(expr_var).desc.content;
                const is_empty_record = blk: {
                    if (resolved == .err) break :blk true;
                    if (resolved != .structure) break :blk false;
                    switch (resolved.structure) {
                        .empty_record => break :blk true,
                        .record => |record| {
                            // A record is effectively empty if it has no fields
                            const fields_slice = self.types.getRecordFieldsSlice(record.fields);
                            break :blk fields_slice.len == 0;
                        },
                        else => break :blk false,
                    }
                };
                if (!is_empty_record) {
                    const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, expr_var);
                    _ = try self.problems.appendProblem(self.cir.gpa, .{ .unused_value = .{
                        .var_ = expr_var,
                        .snapshot = snapshot,
                    } });
                }

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_dbg => |expr| {
                does_fx = try self.checkExpr(expr.expr, env, .no_expectation) or does_fx;
                const expr_var: Var = ModuleEnv.varFrom(expr.expr);

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_inspect => |expr| {
                // inspect returns Str (not the inner expression type)
                _ = try self.checkExpr(expr.expr, env, .no_expectation);
                const str_var = try self.freshStr(env, stmt_region);
                _ = try self.unify(stmt_var, str_var, env);
            },
            .s_expect => |expr_stmt| {
                does_fx = try self.checkExpr(expr_stmt.body, env, .no_expectation) or does_fx;
                const body_var: Var = ModuleEnv.varFrom(expr_stmt.body);

                const bool_var = try self.freshBool(env, stmt_region);
                _ = try self.unify(bool_var, body_var, env);

                _ = try self.unify(stmt_var, body_var, env);
            },
            .s_crash => |_| {
                try self.unifyWith(stmt_var, .{ .flex = Flex.init() }, env);
                diverges = true;
            },
            .s_return => |ret| {
                // Type check the return expression
                does_fx = try self.checkExpr(ret.expr, env, .no_expectation) or does_fx;

                // A return statement's type should be a flex var so it can unify with any type.
                // This allows branches containing early returns to match any other branch type.
                // The actual unification with the function return type happens in unifyEarlyReturns.
                try self.unifyWith(stmt_var, .{ .flex = Flex.init() }, env);
                diverges = true;
            },
            .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno => {
                // These are only valid at the top level, czer reports error
                try self.unifyWith(stmt_var, .err, env);
            },
            .s_runtime_error => {
                try self.unifyWith(stmt_var, .err, env);
            },
        }
    }
    return .{ .does_fx = does_fx, .diverges = diverges };
}

/// Traverse an expression to find s_return statements and unify them with the expected return type.
/// This is called after type-checking a lambda body to ensure all early returns have matching types.
fn unifyEarlyReturns(self: *Self, expr_idx: CIR.Expr.Idx, return_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const expr = self.cir.store.getExpr(expr_idx);
    switch (expr) {
        .e_block => |block| {
            // Check all statements in the block for returns
            for (self.cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                try self.unifyEarlyReturnsInStmt(stmt_idx, return_var, env);
            }
            // Also recurse into the final expression
            try self.unifyEarlyReturns(block.final_expr, return_var, env);
        },
        .e_if => |if_expr| {
            // Check all branches
            for (self.cir.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = self.cir.store.getIfBranch(branch_idx);
                try self.unifyEarlyReturns(branch.body, return_var, env);
            }
            // Check the final else
            try self.unifyEarlyReturns(if_expr.final_else, return_var, env);
        },
        .e_match => |match| {
            // Check all branches
            for (self.cir.store.sliceMatchBranches(match.branches)) |branch_idx| {
                const branch = self.cir.store.getMatchBranch(branch_idx);
                try self.unifyEarlyReturns(branch.value, return_var, env);
            }
        },
        .e_for => |for_expr| {
            // Check the list expression and body for returns
            try self.unifyEarlyReturns(for_expr.expr, return_var, env);
            try self.unifyEarlyReturns(for_expr.body, return_var, env);
        },
        // Lambdas create a new scope for returns - don't recurse into them
        .e_lambda, .e_closure => {},
        // All other expressions don't contain statements
        else => {},
    }
}

/// Check a statement for s_return and unify with the expected return type.
fn unifyEarlyReturnsInStmt(self: *Self, stmt_idx: CIR.Statement.Idx, return_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const stmt = self.cir.store.getStatement(stmt_idx);
    switch (stmt) {
        .s_return => |ret| {
            const return_expr_var = ModuleEnv.varFrom(ret.expr);
            _ = try self.unify(return_expr_var, return_var, env);
        },
        .s_decl => |decl| {
            // Recurse into the declaration's expression
            try self.unifyEarlyReturns(decl.expr, return_var, env);
        },
        .s_decl_gen => |decl| {
            // Recurse into the generalized declaration's expression
            try self.unifyEarlyReturns(decl.expr, return_var, env);
        },
        .s_var => |var_stmt| {
            // Recurse into the var's expression
            try self.unifyEarlyReturns(var_stmt.expr, return_var, env);
        },
        .s_reassign => |reassign| {
            try self.unifyEarlyReturns(reassign.expr, return_var, env);
        },
        .s_for => |for_stmt| {
            try self.unifyEarlyReturns(for_stmt.expr, return_var, env);
            try self.unifyEarlyReturns(for_stmt.body, return_var, env);
        },
        .s_while => |while_stmt| {
            try self.unifyEarlyReturns(while_stmt.cond, return_var, env);
            try self.unifyEarlyReturns(while_stmt.body, return_var, env);
        },
        .s_expr => |s| {
            // Recurse into the expression (could contain blocks with returns)
            try self.unifyEarlyReturns(s.expr, return_var, env);
        },
        .s_expect => |s| {
            try self.unifyEarlyReturns(s.body, return_var, env);
        },
        .s_dbg => |s| {
            try self.unifyEarlyReturns(s.expr, return_var, env);
        },
        .s_inspect => |s| {
            try self.unifyEarlyReturns(s.expr, return_var, env);
        },
        // These statements don't contain expressions with potential returns
        .s_crash, .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_runtime_error => {},
    }
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
    const first_cond_result = try self.unify(bool_var, first_cond_var, env);
    self.setDetailIfTypeMismatch(first_cond_result, .incompatible_if_cond);

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
        const cond_result = try self.unify(branch_bool_var, cond_var, env);
        self.setDetailIfTypeMismatch(cond_result, .incompatible_if_cond);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body, env, .no_expectation) or does_fx;
        const body_var: Var = ModuleEnv.varFrom(branch.body);
        const body_result = try self.unify(branch_var, body_var, env);
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

                does_fx = try self.checkExpr(remaining_branch.cond, env, .no_expectation) or does_fx;
                const remaining_cond_var: Var = ModuleEnv.varFrom(remaining_branch.cond);

                const fresh_bool = try self.freshBool(env, expr_region);
                const remaining_cond_result = try self.unify(fresh_bool, remaining_cond_var, env);
                self.setDetailIfTypeMismatch(remaining_cond_result, .incompatible_if_cond);

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
    const final_else_result = try self.unify(branch_var, final_else_var, env);
    self.setDetailIfTypeMismatch(final_else_result, problem.TypeMismatchDetail{ .incompatible_if_branches = .{
        .parent_if_expr = if_expr_idx,
        .last_if_branch = last_if_branch,
        .num_branches = num_branches,
        .problem_branch_index = num_branches - 1,
    } });

    // Set the entire expr's type to be the type of the branch
    const if_expr_var: Var = ModuleEnv.varFrom(if_expr_idx);
    _ = try self.unify(if_expr_var, branch_var, env);

    return does_fx;
}

// match //

/// Check the types for an if-else expr
fn checkMatchExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, match: CIR.Expr.Match) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond, env, .no_expectation);
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
        try self.checkPattern(branch_ptrn.pattern, env, .no_expectation);
        const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);

        const ptrn_result = try self.unify(cond_var, branch_ptrn_var, env);
        self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_cond_pattern = .{
            .match_expr = expr_idx,
        } });
    }

    // Check the first branch's value, then use that at the branch_var
    does_fx = try self.checkExpr(first_branch.value, env, .no_expectation) or does_fx;
    const val_var = ModuleEnv.varFrom(first_branch.value);

    // Then iterate over the rest of the branches
    for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
        const branch = self.cir.store.getMatchBranch(branch_idx);

        // First, check the patterns of this branch
        const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
        for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
            // Check the pattern's sub types
            const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
            try self.checkPattern(branch_ptrn.pattern, env, .no_expectation);

            // Check the pattern against the cond
            const branch_ptrn_var = ModuleEnv.varFrom(branch_ptrn.pattern);
            const ptrn_result = try self.unify(cond_var, branch_ptrn_var, env);
            self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                .match_expr = expr_idx,
                .num_branches = @intCast(match.branches.span.len),
                .problem_branch_index = @intCast(branch_cur_index),
                .num_patterns = @intCast(branch_ptrn_idxs.len),
                .problem_pattern_index = @intCast(cur_ptrn_index),
            } });
        }

        // Then, check the body
        does_fx = try self.checkExpr(branch.value, env, .no_expectation) or does_fx;
        const branch_result = try self.unify(val_var, ModuleEnv.varFrom(branch.value), env);
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
                    try self.checkPattern(other_branch_ptrn.pattern, env, .no_expectation);

                    // Check the pattern against the cond
                    const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
                    const ptrn_result = try self.unify(cond_var, other_branch_ptrn_var, env);
                    self.setDetailIfTypeMismatch(ptrn_result, problem.TypeMismatchDetail{ .incompatible_match_patterns = .{
                        .match_expr = expr_idx,
                        .num_branches = @intCast(match.branches.span.len),
                        .problem_branch_index = @intCast(other_branch_cur_index),
                        .num_patterns = @intCast(other_branch_ptrn_idxs.len),
                        .problem_pattern_index = @intCast(other_cur_ptrn_index),
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

    return does_fx;
}

// unary minus //

fn checkUnaryMinusExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, env: *Env, unary: CIR.Expr.UnaryMinus) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, env, .no_expectation);

    const expr_var = ModuleEnv.varFrom(expr_idx);
    const operand_var = @as(Var, ModuleEnv.varFrom(unary.expr));

    // Desugar -a to a.negate()
    // Get the negate identifier
    const method_name = self.cir.idents.negate;

    // Create the function type: operand_type -> ret_type
    const args_range = try self.types.appendVars(&.{operand_var});

    // The return type is unknown, so create a fresh variable
    const ret_var = try self.fresh(env, expr_region);
    try env.var_pool.addVarToRank(ret_var, env.rank());

    // Create the constraint function type
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, expr_region);
    try env.var_pool.addVarToRank(constraint_fn_var, env.rank());

    // Create the static dispatch constraint
    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .desugared_binop,
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

    // Create a constrained flex and unify it with the operand
    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        expr_region,
    );
    try env.var_pool.addVarToRank(constrained_var, env.rank());

    _ = try self.unify(constrained_var, operand_var, env);

    // Set the expression to redirect to the return type
    try self.types.setVarRedirect(expr_var, ret_var);

    return does_fx;
}

// unary not //

fn checkUnaryNotExpr(self: *Self, expr_idx: CIR.Expr.Idx, expr_region: Region, env: *Env, unary: CIR.Expr.UnaryNot) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_var = @as(Var, ModuleEnv.varFrom(expr_idx));

    // Check the operand expression
    const does_fx = try self.checkExpr(unary.expr, env, .no_expectation);

    // For unary not, we constrain the operand and result to be booleans
    const operand_var = @as(Var, ModuleEnv.varFrom(unary.expr));

    // Create a fresh boolean variable for the operation
    const bool_var = try self.freshBool(env, expr_region);

    // Unify result with the boolean type
    _ = try self.unify(bool_var, operand_var, env);

    // Redirect the result to the boolean type
    _ = try self.unify(expr_var, bool_var, env);

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
    expected: Expected,
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
        .add => {
            // For builtin numeric types, use the efficient special-cased numeric constraint logic
            // For user-defined nominal types, desugar `a + b` to `a.plus(b)` using static dispatch

            // Check if lhs is a nominal type
            const lhs_resolved = self.types.resolveVar(lhs_var).desc.content;
            const is_nominal = switch (lhs_resolved) {
                .structure => |s| s == .nominal_type,
                else => false,
            };

            if (is_nominal) {
                // User-defined nominal type: use static dispatch to call the plus method
                // Get the pre-cached "plus" identifier from the ModuleEnv
                const method_name = self.cir.idents.plus;

                // Unify lhs and rhs to ensure both operands have the same type
                const unify_result = try self.unify(lhs_var, rhs_var, env);

                // If unification failed, short-circuit and set the expression to error
                if (!unify_result.isOk()) {
                    try self.unifyWith(expr_var, .err, env);
                    return does_fx;
                }

                // Create the function type: lhs_type, rhs_type -> ret_type
                const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });

                // The return type is unknown, so create a fresh variable
                const ret_var = try self.fresh(env, expr_region);

                // Create the constraint function type
                const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                    .args = args_range,
                    .ret = ret_var,
                    .needs_instantiation = false,
                } } }, env, expr_region);

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
                    expr_region,
                );

                _ = try self.unify(constrained_var, lhs_var, env);

                // Set the expression to redirect to the return type
                try self.types.setVarRedirect(expr_var, ret_var);
            } else {
                // Builtin numeric type: use standard numeric constraints
                // This is the same as the other arithmetic operators
                switch (expected) {
                    .expected => |expectation| {
                        const lhs_instantiated = try self.instantiateVar(expectation.var_, env, .{ .explicit = expr_region });
                        const rhs_instantiated = try self.instantiateVar(expectation.var_, env, .{ .explicit = expr_region });

                        if (expectation.from_annotation) {
                            _ = try self.unifyWithCtx(lhs_instantiated, lhs_var, env, .anno);
                            _ = try self.unifyWithCtx(rhs_instantiated, rhs_var, env, .anno);
                        } else {
                            _ = try self.unify(lhs_instantiated, lhs_var, env);
                            _ = try self.unify(rhs_instantiated, rhs_var, env);
                        }
                    },
                    .no_expectation => {
                        // No expectation - operand types will be inferred
                        // The unification of lhs and rhs below will ensure they're the same type
                    },
                }

                // Unify left and right together
                const unify_result = try self.unify(lhs_var, rhs_var, env);

                // If unification failed, short-circuit
                if (!unify_result.isOk()) {
                    try self.unifyWith(expr_var, .err, env);
                    return does_fx;
                }

                // Set root expr. If unifications succeeded this will the the
                // num, otherwise the propgate error
                try self.types.setVarRedirect(expr_var, lhs_var);
            }
        },
        .sub, .mul, .div, .rem, .div_trunc => {
            // For now, we'll constrain both operands to be numbers
            // In the future, this will use static dispatch based on the lhs type

            // We check the lhs and the rhs independently, then unify them with
            // each other. This ensures that all errors are surfaced and the
            // operands are the same type
            switch (expected) {
                .expected => |expectation| {
                    const lhs_instantiated = try self.instantiateVar(expectation.var_, env, .{ .explicit = expr_region });
                    const rhs_instantiated = try self.instantiateVar(expectation.var_, env, .{ .explicit = expr_region });

                    if (expectation.from_annotation) {
                        _ = try self.unifyWithCtx(lhs_instantiated, lhs_var, env, .anno);
                        _ = try self.unifyWithCtx(rhs_instantiated, rhs_var, env, .anno);
                    } else {
                        _ = try self.unify(lhs_instantiated, lhs_var, env);
                        _ = try self.unify(rhs_instantiated, rhs_var, env);
                    }
                },
                .no_expectation => {
                    // No expectation - operand types will be inferred
                    // The unification of lhs and rhs below will ensure they're the same type
                },
            }

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, env);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propgate error
            _ = try self.unify(expr_var, lhs_var, env);
        },
        .lt, .gt, .le, .ge => {
            // Ensure the operands are the same type
            const result = try self.unify(lhs_var, rhs_var, env);

            if (result.isOk()) {
                const fresh_bool = try self.freshBool(env, expr_region);
                _ = try self.unify(expr_var, fresh_bool, env);
            } else {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .eq => {
            // `a == b` desugars to `a.is_eq(b)` with additional constraint that a and b have the same type
            // Constraint: a.is_eq : a, b -> ret_type (ret_type is NOT hardcoded to Bool)

            // Unify lhs and rhs to ensure both operands have the same type
            const unify_result = try self.unify(lhs_var, rhs_var, env);

            // If unification failed, short-circuit and set the expression to error
            if (!unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            // Create the function type: lhs_type, rhs_type -> ret_type (fresh flex var)
            const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });
            const is_eq_ret_var = try self.fresh(env, expr_region);

            const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                .args = args_range,
                .ret = is_eq_ret_var,
                .needs_instantiation = false,
            } } }, env, expr_region);

            // Create the is_eq constraint
            const is_eq_constraint = StaticDispatchConstraint{
                .fn_name = self.cir.idents.is_eq,
                .fn_var = constraint_fn_var,
                .origin = .desugared_binop,
            };
            const constraint_range = try self.types.appendStaticDispatchConstraints(&.{is_eq_constraint});

            // Create a constrained flex and unify it with the lhs (receiver)
            const constrained_var = try self.freshFromContent(
                .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
                env,
                expr_region,
            );

            _ = try self.unify(constrained_var, lhs_var, env);

            // The expression type is whatever is_eq returns
            _ = try self.unify(expr_var, is_eq_ret_var, env);
        },
        .ne => {
            // `a != b` desugars to `a.is_eq(b).not()` with additional constraint that a and b have the same type
            // Constraint 1: a.is_eq : a, b -> is_eq_ret
            // Constraint 2: is_eq_ret.not : is_eq_ret -> final_ret

            // Unify lhs and rhs to ensure both operands have the same type
            const unify_result = try self.unify(lhs_var, rhs_var, env);

            // If unification failed, short-circuit and set the expression to error
            if (!unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            // Create fresh var for the final return type (result of not)
            const not_ret_var = try self.fresh(env, expr_region);

            // Create is_eq_ret_var as a constrained flex WITH the not constraint
            // We need to create the not constraint first, but it references is_eq_ret_var...
            // Solution: create a plain flex first for the not fn arg, then create the
            // constrained is_eq_ret_var and use it in the is_eq function type

            // Create a placeholder for is_eq_ret that we'll use in the not constraint
            const is_eq_ret_placeholder = try self.fresh(env, expr_region);

            // Create the not constraint referencing the placeholder
            const not_args_range = try self.types.appendVars(&.{is_eq_ret_placeholder});
            const not_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                .args = not_args_range,
                .ret = not_ret_var,
                .needs_instantiation = false,
            } } }, env, expr_region);

            const not_constraint = StaticDispatchConstraint{
                .fn_name = self.cir.idents.not,
                .fn_var = not_fn_var,
                .origin = .desugared_binop,
            };

            // Create is_eq_ret_var WITH the not constraint attached
            const not_constraint_range = try self.types.appendStaticDispatchConstraints(&.{not_constraint});
            const is_eq_ret_var = try self.freshFromContent(
                .{ .flex = Flex{ .name = null, .constraints = not_constraint_range } },
                env,
                expr_region,
            );

            // Unify placeholder with the real constrained var so they're the same
            _ = try self.unify(is_eq_ret_placeholder, is_eq_ret_var, env);

            // Constraint 1: is_eq method on lhs type (returns the constrained is_eq_ret_var)
            const is_eq_args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });
            const is_eq_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                .args = is_eq_args_range,
                .ret = is_eq_ret_var,
                .needs_instantiation = false,
            } } }, env, expr_region);

            const is_eq_constraint = StaticDispatchConstraint{
                .fn_name = self.cir.idents.is_eq,
                .fn_var = is_eq_fn_var,
                .origin = .desugared_binop,
            };

            // Add is_eq constraint to lhs
            const is_eq_constraint_range = try self.types.appendStaticDispatchConstraints(&.{is_eq_constraint});
            const lhs_constrained_var = try self.freshFromContent(
                .{ .flex = Flex{ .name = null, .constraints = is_eq_constraint_range } },
                env,
                expr_region,
            );
            _ = try self.unify(lhs_constrained_var, lhs_var, env);

            // The expression type is the return type of not
            _ = try self.unify(expr_var, not_ret_var, env);
        },
        .@"and" => {
            const lhs_fresh_bool = try self.freshBool(env, expr_region);
            const lhs_result = try self.unify(lhs_fresh_bool, lhs_var, env);
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            const rhs_fresh_bool = try self.freshBool(env, expr_region);
            const rhs_result = try self.unify(rhs_fresh_bool, rhs_var, env);
            self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .rhs,
                .binop = .@"and",
            } });

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, env);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propgate error
            _ = try self.unify(expr_var, lhs_var, env);
        },
        .@"or" => {
            const lhs_fresh_bool = try self.freshBool(env, expr_region);
            const lhs_result = try self.unify(lhs_fresh_bool, lhs_var, env);
            self.setDetailIfTypeMismatch(lhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .lhs,
                .binop = .@"and",
            } });

            const rhs_fresh_bool = try self.freshBool(env, expr_region);
            const rhs_result = try self.unify(rhs_fresh_bool, rhs_var, env);
            self.setDetailIfTypeMismatch(rhs_result, .{ .invalid_bool_binop = .{
                .binop_expr = expr_idx,
                .problem_side = .rhs,
                .binop = .@"and",
            } });

            // Unify left and right together
            _ = try self.unify(lhs_var, rhs_var, env);

            // Set root expr. If unifications succeeded this will the the
            // num, otherwise the propagate error
            _ = try self.unify(expr_var, lhs_var, env);
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
    import_idx: CIR.Import.Idx,
    node_idx: u16,
) std.mem.Allocator.Error!?ExternalType {
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

/// Instantiate a variable, writing su
fn copyVar(self: *Self, other_module_var: Var, other_module_env: *const ModuleEnv, mb_region: ?Region) std.mem.Allocator.Error!Var {
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

// validate static dispatch constraints //

/// Handle a recursive static dispatch constraint by creating a RecursionVar
///
/// When we detect that a constraint check would recurse (the variable is already
/// being checked in the call stack), we create a RecursionVar to represent the
/// recursive structure and prevent infinite loops.
///
/// The RecursionVar points back to the original variable structure, allowing
/// equirecursive unification to properly handle the cycle.
fn handleRecursiveConstraint(
    self: *Self,
    var_: types_mod.Var,
    depth: usize,
    env: *Env,
) std.mem.Allocator.Error!void {
    // Create the RecursionVar content that points to the original structure
    const rec_var_content = types_mod.Content{
        .recursion_var = .{
            .structure = var_,
            .name = null, // Could be enhanced to carry debug name
        },
    };

    // Create a new type variable to represent the recursion point
    // Use the current environment's rank for the recursion var
    const recursion_var = try self.types.freshFromContentWithRank(rec_var_content, env.rank());

    // Create RecursionInfo to track the recursion metadata
    _ = types_mod.RecursionInfo{
        .recursion_var = recursion_var,
        .depth = depth,
    };

    // Store the recursion info in the deferred constraint
    // Note: This will be enhanced in later implementation to properly
    // update the constraint with the recursion info
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
fn checkDeferredStaticDispatchConstraints(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    var deferred_constraint_len = env.deferred_static_dispatch_constraints.items.items.len;
    var deferred_constraint_index: usize = 0;
    while (deferred_constraint_index < deferred_constraint_len) : ({
        deferred_constraint_index += 1;
        deferred_constraint_len = env.deferred_static_dispatch_constraints.items.items.len;
    }) {
        const deferred_constraint = env.deferred_static_dispatch_constraints.items.items[deferred_constraint_index];
        const dispatcher_resolved = self.types.resolveVar(deferred_constraint.var_);
        const dispatcher_content = dispatcher_resolved.desc.content;

        // Detect recursive constraints
        // Check if this var is already in the constraint check stack
        for (self.constraint_check_stack.items, 0..) |stack_var, depth| {
            if (stack_var == dispatcher_resolved.var_) {
                // Found recursion! Create a RecursionVar to handle this properly
                try self.handleRecursiveConstraint(dispatcher_resolved.var_, depth, env);
                continue;
            }
        }

        // Not recursive - push to stack and proceed normally
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

            // First, special case if this rigid has no constraints
            if (deferred_constraints.len > 0 and rigid_constraints.len == 0) {
                const constraint = deferred_constraints[0];
                try self.reportConstraintError(
                    deferred_constraint.var_,
                    constraint,
                    .{ .missing_method = .rigid },
                    env,
                );
                continue;
            }

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
        } else if (dispatcher_content == .flex) {
            // If the root type is aa flex, then we there's nothing to check
            continue;
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
                    // TODO: The name `module_envs` is misleading - it's actually a map of
                    // auto-imported TYPE NAMES to their defining modules, not a map of module names.
                    // This is because when you write `List` in user code (not `Builtin.List`), the
                    // compiler needs to quickly resolve which module defines that type name.
                    //
                    // This creates confusion here in static dispatch: we have an `origin_module`
                    // which stores the MODULE name ("Builtin"), but `module_envs` is keyed by
                    // TYPE names ("List", "Bool", etc.).
                    //
                    // The correct solution would be to have two separate maps:
                    // - auto_imported_types: HashMap(TypeName, ModuleEnv) for canonicalization
                    // - imported_modules: HashMap(ModuleName, ModuleEnv) for module lookups
                    //
                    // For now, we work around this by looking up regular imports in module_envs.
                    std.debug.assert(self.module_envs != null);
                    const module_envs = self.module_envs.?;

                    const mb_original_module_env = module_envs.get(original_module_ident);
                    std.debug.assert(mb_original_module_env != null);
                    break :blk mb_original_module_env.?.env;
                }
            };

            // Get some data about the nominal type
            const region = self.getRegionAt(deferred_constraint.var_);

            // Iterate over the constraints
            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            for (constraints) |constraint| {
                // Extract the function and return type from the constraint
                const resolved_constraint = self.types.resolveVar(constraint.fn_var);
                const mb_resolved_func = resolved_constraint.desc.content.unwrapFunc();
                std.debug.assert(mb_resolved_func != null);
                const resolved_func = mb_resolved_func.?;

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

                if (is_this_module) {
                    // Check if we've processed this def already.
                    const def = original_env.store.getDef(def_idx);
                    const mb_processing_def = self.top_level_ptrns.get(def.pattern);
                    if (mb_processing_def) |processing_def| {
                        std.debug.assert(processing_def.def_idx == def_idx);
                        switch (processing_def.status) {
                            .not_processed => {
                                var sub_env = try self.env_pool.acquire(.generalized);
                                defer self.env_pool.release(sub_env);

                                try self.checkDef(def_idx, &sub_env);
                            },
                            .processing => {
                                // TODO: Handle recursive defs
                            },
                            .processed => {},
                        }
                    }
                }

                // Copy the actual method from the dest module env to this module env
                const real_method_var = if (is_this_module) blk: {
                    break :blk try self.instantiateVar(def_var, env, .{ .explicit = region });
                } else blk: {
                    // Copy the method from the other module's type store
                    const copied_var = try self.copyVar(def_var, original_env, region);
                    // For builtin methods, we need to instantiate the copied var to convert
                    // rigid type variables to flex, so they can unify with the call site
                    const is_builtin = original_module_ident == self.cir.idents.builtin_module;
                    if (is_builtin) {
                        break :blk try self.instantiateVar(copied_var, env, .{ .explicit = region });
                    } else {
                        break :blk copied_var;
                    }
                };

                // Unify the actual function var against the inferred var
                // We break this down into arg-by-arg and return type unification
                // for better error messages (instead of showing the whole function types)

                // Extract the function type from the real method
                const resolved_real = self.types.resolveVar(real_method_var);
                const mb_real_func = resolved_real.desc.content.unwrapFunc();
                if (mb_real_func == null) {
                    // The looked-up definition is not a function - report as missing method
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .{ .missing_method = .nominal },
                        env,
                    );
                    continue;
                }
                const real_func = mb_real_func.?;

                // Check arity matches
                const constraint_args = self.types.sliceVars(resolved_func.args);
                const real_args = self.types.sliceVars(real_func.args);

                if (constraint_args.len != real_args.len) {
                    // Arity mismatch - the method exists but has wrong number of arguments
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .{ .missing_method = .nominal },
                        env,
                    );
                    continue;
                }

                // Unify each argument pair
                var any_arg_failed = false;
                for (constraint_args, real_args) |constraint_arg, real_arg| {
                    const arg_result = try self.unify(real_arg, constraint_arg, env);
                    if (arg_result.isProblem()) {
                        any_arg_failed = true;
                    }
                }

                // Unify return types - this will generate the error with the expression region
                const ret_result = try self.unify(real_func.ret, resolved_func.ret, env);

                if (any_arg_failed or ret_result.isProblem()) {
                    try self.unifyWith(deferred_constraint.var_, .err, env);
                    try self.unifyWith(resolved_func.ret, .err, env);
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
                    // Other methods are not supported on anonymous types
                    try self.reportConstraintError(
                        deferred_constraint.var_,
                        constraint,
                        .not_nominal,
                        env,
                    );
                }
            }
        } else {
            // If the root type is anything but a nominal type or anonymous structural type, push an error
            // This handles function types, which do not support any methods

            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            if (constraints.len > 0) {
                const constraint = constraints[0];

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
            } else {
                // It should be impossible to have a deferred constraint check
                // that has no constraints.
                std.debug.assert(false);
            }
        }
    }

    // Now that we've processed all constraints, reset the array
    env.deferred_static_dispatch_constraints.items.clearRetainingCapacity();
}

/// Check if a structural type supports is_eq.
/// A type supports is_eq if:
/// - It's not a function type
/// - All of its components (record fields, tuple elements, tag payloads) also support is_eq
/// - For nominal types, we assume they support is_eq (TODO: actually check for is_eq method)
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

        // Nominal types: TODO: actually check if they have an is_eq method
        // For now, assume they do (numbers, Bool, Str, etc. all have is_eq)
        .nominal_type => true,

        // Unbound records need to be resolved first
        .record_unbound => true, // TODO: check resolved type
    };
}

/// Check if a type variable supports is_eq by resolving it and checking its content
fn varSupportsIsEq(self: *Self, var_: Var) bool {
    const resolved = self.types.resolveVar(var_);
    return switch (resolved.desc.content) {
        .structure => |s| self.typeSupportsIsEq(s),
        // Flex/rigid vars could be anything, assume they support is_eq for now
        // (the actual constraint will be checked when the type is known)
        .flex, .rigid => true,
        // Aliases: check the underlying type
        .alias => |alias| self.varSupportsIsEq(self.types.getAliasBackingVar(alias)),
        // Recursion vars: assume they support is_eq (recursive types like List are ok)
        .recursion_var => true,
        // Error types: allow them to proceed
        .err => true,
    };
}

/// Mark a constraint function's return type as error
fn markConstraintFunctionAsError(self: *Self, constraint: StaticDispatchConstraint, env: *Env) !void {
    const resolved_constraint = self.types.resolveVar(constraint.fn_var);
    const mb_resolved_func = resolved_constraint.desc.content.unwrapFunc();
    std.debug.assert(mb_resolved_func != null);
    const resolved_func = mb_resolved_func.?;
    try self.unifyWith(resolved_func.ret, .err, env);
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
        .missing_method => |dispatcher_type| problem.Problem{ .static_dispach = .{
            .dispatcher_does_not_impl_method = .{
                .dispatcher_var = dispatcher_var,
                .dispatcher_snapshot = snapshot,
                .dispatcher_type = dispatcher_type,
                .fn_var = constraint.fn_var,
                .method_name = constraint.fn_name,
                .origin = constraint.origin,
            },
        } },
        .not_nominal => problem.Problem{ .static_dispach = .{
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
    const equality_problem = problem.Problem{ .static_dispach = .{
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
    fn acquire(self: *EnvPool, at: Rank) std.mem.Allocator.Error!Env {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (self.available.pop()) |env| {
            // Reset the env for reuse
            var reused_env = env;
            reused_env.reset();
            return reused_env;
        } else {
            // Otherwise init a new one and ensure there's room to put it back
            // into the pool when we're done using it
            try self.available.ensureUnusedCapacity(self.gpa, 1);
            return try Env.init(self.gpa, at);
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
        releasable_env.reset();
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
    module_envs: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
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
                    const stmt_idx_int = @intFromEnum(stmt_idx);
                    if (stmt_idx_int != 0) {
                        const stmt = builtin_env.store.getStatement(stmt_idx);
                        switch (stmt) {
                            .s_nominal_decl => |decl| {
                                const header = builtin_env.store.getTypeHeader(decl.header);
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
                            },
                            else => {
                                // Skip non-nominal statements (e.g., nested types that aren't directly importable)
                            },
                        }
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

    _ = module_envs; // Not needed anymore - mapping is built during canonicalization

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
