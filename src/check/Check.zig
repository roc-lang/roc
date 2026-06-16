//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
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
/// Module envs whose public APIs are semantically visible through imported checked data.
owner_modules: []const *const ModuleEnv,
/// Module envs whose public APIs are semantically visible through direct imports.
/// These are not lexically importable, and CIR import indexes never refer to
/// this set. It exists only for owner lookups on copied imported types.
owner_module_envs: std.StringHashMap(*const ModuleEnv),
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
/// Static-dispatch constraint function vars copied during instantiation.
constraint_fn_var_map: std.AutoHashMap(Var, Var),
/// A map from one var to another. Used in instantiation and var copying
var_set: std.AutoHashMap(Var, void),
/// A map from one var to another. Used to apply type arguments in instantiation
rigid_var_substitutions: std.AutoHashMapUnmanaged(Ident.Idx, Var),
/// Header rigid vars for the currently-processed type declaration.
/// Used to resolve rigid vars in local type decl bodies that were not
/// rewritten as rigid_var_lookup during canonicalization.
type_decl_rigid_vars: std.AutoHashMapUnmanaged(Ident.Idx, Var),
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
// Cache for imported types. This cache lives for the entire type-checking session
/// of a module, so the same imported type can be reused across the entire module.
import_cache: ImportCache,
/// Copied Bool type from Bool module (for use in if conditions, etc.)
bool_var: Var,
/// Copied Str type from Builtin module (for use in string literals, etc.)
str_var: Var,
/// Builtin type vars are initialized during Check.init and may be reused by later entrypoints.
builtin_types_copied: bool,
/// Map representation of Ident -> Var, used in checking static dispatch constraints
ident_to_var_map: std.AutoHashMap(Ident.Idx, Var),
/// Checker-local source-site mapping for method/equality rewrites.
constraint_expr_by_fn_var: std.AutoHashMap(Var, CIR.Expr.Idx),
/// Generated interpolation iterator item type, keyed by its static dispatch constraint function.
interpolation_item_var_by_fn_var: std.AutoHashMap(Var, Var),
/// Custom interpolation item constraints already applied for each constraint function.
checked_interpolation_part_constraints: std.AutoHashMap(Var, void),
/// Dispatcher/method pairs already reported by `reportConstraintError`, so a
/// constraint failing in multiple passes (or reachable through several aliased
/// type variables) is reported once.
reported_constraint_errors: std.AutoHashMap(ReportedConstraintError, void),
/// Static dispatch constraints created while checking an expect body.
expect_region_by_constraint_fn_var: std.AutoHashMap(Var, Region),
/// Region of the expect body currently being checked, if any.
current_expect_region: ?Region,
/// Map representation all top level patterns, and if we've processed them yet
top_level_ptrns: std.AutoHashMap(CIR.Pattern.Idx, DefProcessed),
/// Local block-statement (`s_decl`) function patterns whose body is currently
/// being type-checked. Used to detect self-recursion (and references to an
/// enclosing in-flight def) of LOCAL function defs so their recursive references
/// defer unification (fresh flex + a pending `local_recursive_refs` entry)
/// instead of lowering the pattern var's rank, which would prevent
/// generalization of the def's rigid type variables. Analogous to
/// `top_level_ptrns` but for block-local defs.
///
/// Presence == "currently being checked" (defer); absence == normal
/// lookup/instantiate. A reference can only resolve to the current def or an
/// enclosing in-flight one: forward references to a *later* sibling, and local
/// mutual recursion, are rejected during canonicalization (local defs are
/// sequentially scoped), so this is always a single self/enclosing chain.
local_processing_ptrns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, LocalDefProcessed) = .{},
/// Recursive references recorded while checking a LOCAL block def's body (a
/// self-reference, or a reference to an enclosing in-flight local def), pending
/// validation once the def's lambda has generalized. Kept in a dedicated stack
/// rather than the shared `constraints` list because sequential scoping
/// guarantees local recursion is a single self/enclosing chain — never a mutual
/// group — so these need no cycle machinery, no per-use instantiation, and no
/// entanglement with the other constraint kinds (notably early-return / `?`
/// constraints, which `processReturnConstraints` compacts mid-body).
local_recursive_refs: std.ArrayListUnmanaged(LocalRecursiveRef) = .empty,
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
/// True when checking an immediately-consumed call operand expression. Used to
/// suppress generalization of standalone lambdas that appear either as the
/// direct callee or as a direct call argument, since they don't need
/// independent generalization.
/// This prevents rank pollution where inner lambda generalization pulls outer
/// scope vars to rank 0 via Rank.min in merge.
checking_call_arg: bool = false,
/// True when checking the right-hand side of an immutable binding (a top-level
/// def or a block-local `s_decl`). Used to generalize a binding whose RHS is a
/// bare reference to an already-generalized scheme (e.g. `shorthand = Foo.bar`).
/// Such a reference is non-expansive, so generalizing it is the value-restriction
/// treatment of a variable binding. Deliberately NOT set for mutable `var`
/// bindings, which must never generalize.
checking_binding_rhs: bool = false,
/// Nonzero while checking source that constructs a compile-time value. Static
/// exhaustiveness diagnostics under this depth are empirical candidates: the
/// compile-time finalizer either observes them executing, reports their generated
/// miss path, or discards them if the source was not reached.
empirical_exhaustiveness_depth: u32 = 0,
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
/// Tracks all local lookup exprs so erroneous bindings can be poisoned explicitly
/// after type checking has finished.
value_lookup_tracking: std.ArrayListUnmanaged(ValueLookupEntry),
/// Tracks expressions whose checked type contains an error, even if annotation
/// preservation later gives their raw expr var a non-error type.
erroneous_value_exprs: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void),
/// Tracks bindings whose defining expression is known erroneous and whose
/// subsequent local lookups must therefore become explicit runtime errors.
erroneous_value_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void),
/// True when canonicalization already recorded diagnostics before type checking.
/// In that case, we avoid adding "erroneous value" diagnostics during checking
/// to prevent cascading errors from malformed nodes.
has_can_diagnostics: bool,
/// Per-instantiation static-dispatch receivers, recorded so the end-of-check
/// ambiguity sweep can revisit dispatches hidden inside polymorphic helpers — the
/// expression-keyed def-site sweep never sees these instantiated receiver vars.
/// Every time a generalized scheme carrying a non-`from_literal` static-dispatch
/// constraint is instantiated, the freshly created receiver var is recorded here.
/// After all solving, a recorded receiver that is still flex/rigid, carries a real
/// (non-`from_literal`, non-`is_eq`) dispatch constraint, and does not resolve into
/// the pinnable set can never be pinned, so its dispatch is reported as ambiguous.
instantiation_dispatchers: std.ArrayListUnmanaged(InstantiationDispatcher),
/// Worklist of flex vars created carrying a `from_literal` constraint — open
/// literals that may still need defaulting. Checker bookkeeping, not type data:
/// every registration site lives in this file (literal creation,
/// `instantiateVarHelp`, `copyVar`), and the defaulting passes iterate it
/// (re-resolving each entry, skipping resolved ones) instead of scanning the
/// whole type store. Append-only within a probe scope — `Probe.rollback`
/// truncates it alongside the other Check-side buffers a probe grows.
open_literal_vars: std.ArrayListUnmanaged(Var),
/// Scratch for the end-of-check ambiguity sweep: the pinnable set (every
/// resolved var some caller can still pin). Init-allocated; cleared and reused
/// each `checkFile`.
pinnable_vars: std.AutoHashMap(Var, void),
/// Scratch for the ambiguity sweep: dispatcher vars already reported, shared
/// by the per-instantiation and def-site sweeps to avoid double-reporting.
reported_dispatch_vars: std.AutoHashMap(Var, void),
/// Scratch for `collectArgPositionVars`: guards its function-ret / alias
/// spine walk against unbounded recursion.
pinnable_spine_visited: std.AutoHashMap(Var, void),
/// Scratch for `defaultLiteralsAtGeneralizationBoundary`: reachable-var
/// closure of the def root(s) being generalized (constraint-signature edges
/// included).
boundary_reachable_vars: std.AutoHashMap(Var, void),
/// Scratch for `boundaryDefaultLeaksIntoSignature`: closure of one boundary
/// literal's constraint signatures, intersected with
/// `boundary_reachable_vars` to detect interface leaks.
boundary_leak_vars: std.AutoHashMap(Var, void),
/// Param-pattern spans of every checked `e_lambda` / `e_hosted_lambda`; the
/// pinnable collection consumes this instead of re-walking the NodeStore.
/// Union-find roots resolve at consumption time (eager roots would go stale).
/// Duplicates are harmless — the consumer inserts into a set.
checked_lambda_params: std.ArrayListUnmanaged(CIR.Pattern.Span),
/// Scratch for `beginCommitProbe`: the caller env's var-pool length per rank
/// at probe start, restored on a failed probe's rollback. One buffer suffices
/// because commit-probes never nest — but the type store's trail-based
/// savepoints are themselves nestable, so nothing downstream would catch a
/// nested commit-probe clobbering this buffer. `commit_probe_active` makes the
/// invariant load-bearing-with-a-guard instead of load-bearing-by-convention.
probe_var_pool_lens: std.ArrayListUnmanaged(usize),
/// True while a `CommitProbe` is open. A second `beginCommitProbe` would
/// `clearRetainingCapacity` and repopulate `probe_var_pool_lens`, corrupting the
/// outer probe's saved per-rank lengths; this asserts that never happens.
commit_probe_active: bool = false,
/// A def + processing data
const DefProcessed = struct {
    def_idx: CIR.Def.Idx,
    def_name: ?Ident.Idx,
    status: HasProcessed,
};

/// A static-dispatch receiver var created by instantiating a constrained scheme.
/// The end-of-check ambiguity sweep revisits each such receiver: if it is still a
/// flex/rigid var carrying a real (non-`from_literal`, non-`is_eq`) dispatch
/// constraint and does not resolve into the pinnable set, no caller can ever pin
/// it, so its dispatch is ambiguous.
const InstantiationDispatcher = struct {
    /// The freshly instantiated receiver (dispatcher) var.
    dispatcher_var: Var,
    /// The static-dispatch constraints copied from the generalized scheme.
    constraints: StaticDispatchConstraint.SafeList.Range,
};

fn isLiteralStaticDispatchOrigin(origin: StaticDispatchConstraint.Origin) bool {
    return switch (origin) {
        // `from_literal` covers every literal kind (numeral, quote, interpolation).
        .from_literal => true,
        .desugared_binop,
        .desugared_unaryop,
        .method_call,
        .where_clause,
        => false,
    };
}

const StaticDispatchUse = struct {
    expr_idx: CIR.Expr.Idx,
    region: Region,
};

/// Indicates if something has been processed or not
const HasProcessed = enum { processed, processing, not_processed };

/// A local block-statement (`s_decl`) function def whose body is currently being
/// checked. Block defs are `s_decl` statements (not `CIR.Def`), so there is no
/// `Def.Idx` — only the name (for error context) and pattern are tracked.
const LocalDefProcessed = struct {
    def_name: ?Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
};

/// A recursive reference made inside a LOCAL block def's body, pending
/// validation once the def's lambda has generalized. `pat_var` is the def's
/// (eventually generalized) pattern var; `expr_var` is the reference's flex var.
const LocalRecursiveRef = struct {
    pat_var: Var,
    expr_var: Var,
    def_name: ?Ident.Idx,
};

/// A deferred def-level unification (def_var = ptrn_var = expr_var).
const DeferredDefUnification = struct {
    def_var: Var,
    ptrn_var: Var,
    expr_var: Var,
};

const ValueLookupEntry = struct {
    expr_idx: CIR.Expr.Idx,
    pattern_idx: CIR.Pattern.Idx,
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
    eql: struct {
        expected: Var,
        actual: Var,
        ctx: problem.Context,
        /// True when this is a `recursive_def` cross-reference to a *different*,
        /// annotated member of a recursive group (mutual recursion). Such a
        /// reference is instantiated per use-site at validation time so each
        /// member keeps its own rigid type parameters; a self-reference (or an
        /// unannotated target) stays monomorphic. Drives resolution, not the
        /// error message — kept here rather than in `ctx` (which is purely
        /// diagnostic) so it doesn't have to ride inside the problem context.
        is_cross_reference: bool = false,
    },

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
    try preflightForTypeChecking(mutable_cir);
    return initAssumePrepared(
        gpa,
        types,
        mutable_cir,
        imported_modules,
        imported_modules,
        auto_imported_types,
        regions,
        builtin_ctx,
    );
}

/// Init type solver with an explicit semantic owner set for imported public APIs.
/// `imported_modules` remains the direct-import list used by CIR import indexes.
pub fn initWithOwnerModules(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *const ModuleEnv,
    imported_modules: []const *const ModuleEnv,
    owner_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    regions: *const Region.List,
    builtin_ctx: BuiltinContext,
) std.mem.Allocator.Error!Self {
    const mutable_cir = @constCast(cir);
    try preflightForTypeChecking(mutable_cir);
    return initAssumePrepared(
        gpa,
        types,
        mutable_cir,
        imported_modules,
        owner_modules,
        auto_imported_types,
        regions,
        builtin_ctx,
    );
}

/// Preflight module state required by type-checking.
/// This is intentionally private so `Check.init` is the only public entry point.
fn preflightForTypeChecking(cir: *ModuleEnv) std.mem.Allocator.Error!void {
    try cir.getIdentStore().enableRuntimeInserts(cir.gpa);
    // Type checking rewrites some expressions into dispatch calls, which can
    // append argument spans to the CIR index store. Existing CIR spans are valid
    // by index, but many checker paths borrow them as slices while recursively
    // checking child expressions. Reserve enough room for one appended index per
    // existing node so those borrows cannot be invalidated by dispatch rewrites.
    const index_count: usize = @intCast(cir.store.index_data.len());
    const node_count: usize = @intCast(cir.store.nodes.len());
    try cir.store.index_data.items.ensureTotalCapacity(cir.gpa, index_count + node_count);

    const import_count: usize = @intCast(cir.imports.imports.items.items.len);
    for (0..import_count) |i| {
        const import_idx: can.CIR.Import.Idx = @enumFromInt(i);
        if (cir.imports.getResolvedModule(import_idx) != null) continue;
        if (cir.imports.importFailedBeforeChecking(import_idx)) continue;

        const import_name = cir.getString(cir.imports.imports.items.items[i]);
        std.debug.panic(
            "Check.init requires resolved import mapping before type checking; unresolved import \"{s}\" in module \"{s}\"",
            .{ import_name, cir.module_name },
        );
    }
}

fn initAssumePrepared(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    cir: *ModuleEnv,
    imported_modules: []const *const ModuleEnv,
    owner_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    regions: *const Region.List,
    builtin_ctx: BuiltinContext,
) std.mem.Allocator.Error!Self {
    var owner_module_envs = try buildOwnerModuleEnvMap(gpa, imported_modules, owner_modules);
    errdefer owner_module_envs.deinit();

    var import_mapping = try createImportMapping(
        gpa,
        cir.getIdentStore(),
        cir,
        builtin_ctx.builtin_module,
        builtin_ctx.builtin_indices,
    );
    errdefer import_mapping.deinit();

    const self: Self = .{
        .gpa = gpa,
        .types = types,
        .cir = cir,
        .imported_modules = imported_modules,
        .owner_modules = owner_modules,
        .owner_module_envs = owner_module_envs,
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
        .constraint_fn_var_map = std.AutoHashMap(Var, Var).init(gpa),
        .constraints = try Constraint.SafeList.initCapacity(gpa, 32),
        .var_set = std.AutoHashMap(Var, void).init(gpa),
        .rigid_var_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){},
        .type_decl_rigid_vars = std.AutoHashMapUnmanaged(Ident.Idx, Var){},
        .scratch_vars = try base.Scratch(types_mod.Var).init(gpa),
        .scratch_tags = try base.Scratch(types_mod.Tag).init(gpa),
        .scratch_record_fields = try base.Scratch(types_mod.RecordField).init(gpa),
        .scratch_static_dispatch_constraints = try base.Scratch(ScratchStaticDispatchConstraint).init(gpa),
        .scratch_deferred_static_dispatch_constraints = try base.Scratch(DeferredConstraintCheck).init(gpa),
        .import_cache = ImportCache{},
        .bool_var = undefined,
        .str_var = undefined,
        .builtin_types_copied = false,
        .ident_to_var_map = std.AutoHashMap(Ident.Idx, Var).init(gpa),
        .constraint_expr_by_fn_var = std.AutoHashMap(Var, CIR.Expr.Idx).init(gpa),
        .interpolation_item_var_by_fn_var = std.AutoHashMap(Var, Var).init(gpa),
        .checked_interpolation_part_constraints = std.AutoHashMap(Var, void).init(gpa),
        .reported_constraint_errors = std.AutoHashMap(ReportedConstraintError, void).init(gpa),
        .expect_region_by_constraint_fn_var = std.AutoHashMap(Var, Region).init(gpa),
        .current_expect_region = null,
        .top_level_ptrns = std.AutoHashMap(CIR.Pattern.Idx, DefProcessed).init(gpa),
        .enclosing_func_name = null,
        // Initialize with null import_mapping - caller should call fixupTypeWriter() after storing Check
        .type_writer = try types_mod.TypeWriter.initFromParts(gpa, types, cir.getIdentStore(), null),
        .deferred_def_unifications = .empty,
        .deferred_cycle_envs = .empty,
        .value_lookup_tracking = .empty,
        .erroneous_value_exprs = .empty,
        .erroneous_value_patterns = .empty,
        .has_can_diagnostics = if (cir.store.scratch) |scratch| scratch.diagnostics.top() > 0 else false,
        .instantiation_dispatchers = .empty,
        .open_literal_vars = .empty,
        .pinnable_vars = std.AutoHashMap(Var, void).init(gpa),
        .reported_dispatch_vars = std.AutoHashMap(Var, void).init(gpa),
        .pinnable_spine_visited = std.AutoHashMap(Var, void).init(gpa),
        .boundary_reachable_vars = std.AutoHashMap(Var, void).init(gpa),
        .boundary_leak_vars = std.AutoHashMap(Var, void).init(gpa),
        .checked_lambda_params = .empty,
        .probe_var_pool_lens = .empty,
    };

    return self;
}

fn buildOwnerModuleEnvMap(
    gpa: std.mem.Allocator,
    imported_modules: []const *const ModuleEnv,
    owner_modules: []const *const ModuleEnv,
) std.mem.Allocator.Error!std.StringHashMap(*const ModuleEnv) {
    var map = std.StringHashMap(*const ModuleEnv).init(gpa);
    errdefer map.deinit();

    for (imported_modules) |imported_env| {
        if (imported_env.module_role == .builtin) continue;
        try putOwnerModuleEnvNames(&map, imported_env);
    }
    for (owner_modules) |owner_env| {
        if (owner_env.module_role == .builtin) continue;
        try putOwnerModuleEnvNames(&map, owner_env);
    }

    return map;
}

fn putOwnerModuleEnvNames(
    map: *std.StringHashMap(*const ModuleEnv),
    module_env: *const ModuleEnv,
) std.mem.Allocator.Error!void {
    if (!module_env.qualified_module_ident.isNone()) {
        try map.put(module_env.getIdent(module_env.qualified_module_ident), module_env);
    }
    if (!module_env.display_module_name_idx.isNone()) {
        try map.put(module_env.getIdent(module_env.display_module_name_idx), module_env);
    }
    if (module_env.module_name.len > 0) {
        try map.put(module_env.module_name, module_env);
    }
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
    self.owner_module_envs.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.seen_annos.deinit();
    // Release any stored cycle envs before deiniting the pool
    for (self.deferred_cycle_envs.items) |deferred_env| {
        self.env_pool.release(deferred_env);
    }
    self.deferred_cycle_envs.deinit(self.gpa);
    self.value_lookup_tracking.deinit(self.gpa);
    self.erroneous_value_exprs.deinit(self.gpa);
    self.erroneous_value_patterns.deinit(self.gpa);
    self.env_pool.deinit();
    self.generalizer.deinit(self.gpa);
    self.var_map.deinit();
    self.constraint_fn_var_map.deinit();
    self.constraints.deinit(self.gpa);
    self.var_set.deinit();
    self.rigid_var_substitutions.deinit(self.gpa);
    self.type_decl_rigid_vars.deinit(self.gpa);
    self.scratch_vars.deinit();
    self.scratch_tags.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_static_dispatch_constraints.deinit();
    self.scratch_deferred_static_dispatch_constraints.deinit();
    self.import_cache.deinit(self.gpa);
    self.ident_to_var_map.deinit();
    self.constraint_expr_by_fn_var.deinit();
    self.interpolation_item_var_by_fn_var.deinit();
    self.checked_interpolation_part_constraints.deinit();
    self.reported_constraint_errors.deinit();
    self.expect_region_by_constraint_fn_var.deinit();
    self.top_level_ptrns.deinit();
    self.local_processing_ptrns.deinit(self.gpa);
    self.local_recursive_refs.deinit(self.gpa);
    self.type_writer.deinit();
    self.deferred_def_unifications.deinit(self.gpa);
    self.instantiation_dispatchers.deinit(self.gpa);
    self.open_literal_vars.deinit(self.gpa);
    self.pinnable_vars.deinit();
    self.reported_dispatch_vars.deinit();
    self.pinnable_spine_visited.deinit();
    self.boundary_reachable_vars.deinit();
    self.boundary_leak_vars.deinit();
    self.checked_lambda_params.deinit(self.gpa);
    self.probe_var_pool_lens.deinit(self.gpa);
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
        _ = self.types.appendFromContentAssumeCapacity(.{ .flex = Flex.init() }, Rank.outermost);
    }
}

// import caches //

/// Key for the import cache: module index + expression index in that module
const ImportCacheKey = struct {
    resolved_module_idx: u32,
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
        hasher.update(std.mem.asBytes(&key.resolved_module_idx));
        hasher.update(std.mem.asBytes(&key.node_idx));
        return hasher.final();
    }

    pub fn eql(_: @This(), a: ImportCacheKey, b: ImportCacheKey) bool {
        return a.resolved_module_idx == b.resolved_module_idx and a.node_idx == b.node_idx;
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
    fn reset(self: *Env, to: Rank) Allocator.Error!void {
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

/// Build the borrowed dependency bundle the unifier needs. Cheap (9 pointers);
/// constructed per call and inlined.
fn unifyEnv(self: *Self) unifier.Env {
    return .{
        // problems is owned by self.gpa.
        .problems_gpa = self.gpa,
        .ident_store = self.cir.getIdentStoreConst(),
        .qualified_module_ident = self.cir.qualified_module_ident,
        .types = self.types,
        .problems = &self.problems,
        .snapshots = &self.snapshots,
        .type_writer = &self.type_writer,
        .unify_scratch = &self.unify_scratch,
        .occurs_scratch = &self.occurs_scratch,
    };
}

/// The single core: run unification, then assign ranks/regions to fresh vars,
/// copy out deferred constraints, and assert array sync.
fn runUnify(self: *Self, a: Var, b: Var, env: *Env, opts: unifier.Options) std.mem.Allocator.Error!unifier.Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    const unify_env = self.unifyEnv();
    const result = try unifier.unify(&unify_env, a, b, opts);

    // Set regions and add to the current rank all variables created during unification.
    //
    // We assign all fresh variables the region of `b` (the "actual" type), since `a` is
    // typically the "expected" type from an annotation. This policy works well for
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

/// Unify two types where `a` is the expected type and `b` is the actual type.
fn unify(self: *Self, a: Var, b: Var, env: *Env) std.mem.Allocator.Error!unifier.Result {
    return self.runUnify(a, b, env, .{});
}

/// Unify two types with a context for error reporting.
fn unifyInContext(self: *Self, a: Var, b: Var, env: *Env, ctx: problem.Context) std.mem.Allocator.Error!unifier.Result {
    return self.runUnify(a, b, env, .{ .context = ctx });
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
    self.constraint_fn_var_map.clearRetainingCapacity();

    const copy_constraint_metadata = self.hasConstraintMetadata();
    instantiator.constraint_fn_var_map = if (copy_constraint_metadata) &self.constraint_fn_var_map else null;

    // Then, instantiate the variable with the provided context
    const instantiated_var = try instantiator.instantiateVar(var_to_instantiate);
    instantiator.constraint_fn_var_map = null;

    if (copy_constraint_metadata and self.constraint_fn_var_map.count() > 0) {
        var constraint_iterator = self.constraint_fn_var_map.iterator();
        while (constraint_iterator.next()) |x| {
            try self.copyConstraintMetadata(x.key_ptr.*, x.value_ptr.*);
        }
    }

    if (instantiator.recursion_overflow) {
        // Non-terminating instantiation — e.g. a self-referential static-dispatch
        // `where` constraint nested deeper than the `var_map` memo can collapse.
        // Report an infinite-type error and yield a fresh err var rather than
        // hang or use the partial (err-filled) result.
        const overflow_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;
        const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, var_to_instantiate);
        _ = try self.problems.appendProblem(self.gpa, .{ .infinite_recursion = .{
            .var_ = var_to_instantiate,
            .snapshot = snapshot,
            .def_name = null,
        } });
        return try self.freshFromContent(.err, env, overflow_region);
    }

    // If we had to insert any new type variables, ensure that we have
    // corresponding regions for them. This is essential for error reporting.
    const root_instantiated_region = self.regions.get(@enumFromInt(@intFromEnum(var_to_instantiate))).*;
    if (instantiator.var_map.count() > 0) {
        var iterator = instantiator.var_map.iterator();
        while (iterator.next()) |x| {
            // Get the newly created var
            const fresh_var = x.value_ptr.*;

            const fresh_resolved = self.types.resolveVar(fresh_var);

            // Register newly instantiated open-literal flex vars on the worklist
            // so the defaulting passes see them. Separately, a fresh flex
            // receiver carrying a non-`from_literal` static-dispatch constraint is
            // a per-instantiation dispatcher: record it so the end-of-check sweep
            // can decide its ambiguity per-instantiation. This hook closes the
            // holes where a polymorphic helper hides an ambiguous dispatch that
            // only manifests at an unpinned call site. We only RECORD here, not
            // enqueue a deferred re-check: the normal constraint solver already
            // validates the receiver once this call's arguments unify, so an extra
            // enqueue would only double-process and shift error attribution.
            if (fresh_resolved.desc.content == .flex) {
                const flex = fresh_resolved.desc.content.flex;
                if (flex.constraints.len() > 0) {
                    const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
                    var has_literal_constraint = false;
                    var has_other_constraint = false;
                    for (constraints) |c| {
                        if (isLiteralStaticDispatchOrigin(c.origin)) {
                            has_literal_constraint = true;
                        } else {
                            has_other_constraint = true;
                        }
                    }
                    if (has_literal_constraint) {
                        try self.open_literal_vars.append(self.gpa, fresh_var);
                    }
                    if (has_other_constraint) {
                        try self.instantiation_dispatchers.append(self.gpa, .{
                            .dispatcher_var = fresh_var,
                            .constraints = flex.constraints,
                        });
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

/// Create fresh var with the provided content and an explicit rank.
fn freshFromContentAtRank(
    self: *Self,
    content: Content,
    env: *Env,
    new_region: Region,
    rank: Rank,
) Allocator.Error!Var {
    const var_ = try self.types.freshFromContentWithRank(content, rank);
    try self.fillInRegionsThrough(var_);
    self.setRegionAt(var_, new_region);
    try env.var_pool.addVarToRank(var_, rank);
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

const BuiltinNominalDecl = union(enum) {
    list,
    box,
    try_type,
    numeral,
    num: CIR.NumKind,
};

const BuiltinParseSpecDecl = enum {
    str,
    record,
    tag_union,
};

fn builtinOriginModule(self: *const Self) Ident.Idx {
    return if (self.builtin_ctx.builtin_module) |_|
        self.cir.idents.builtin_module
    else
        self.builtin_ctx.module_name;
}

fn isCheckingBuiltinModuleDirectly(self: *const Self) bool {
    return self.cir.module_role == .builtin;
}

fn builtinNumTypeIdent(self: *const Self, num_kind: CIR.NumKind) Ident.Idx {
    return switch (num_kind) {
        .u8 => self.cir.idents.u8_type,
        .i8 => self.cir.idents.i8_type,
        .u16 => self.cir.idents.u16_type,
        .i16 => self.cir.idents.i16_type,
        .u32 => self.cir.idents.u32_type,
        .i32 => self.cir.idents.i32_type,
        .u64 => self.cir.idents.u64_type,
        .i64 => self.cir.idents.i64_type,
        .u128 => self.cir.idents.u128_type,
        .i128 => self.cir.idents.i128_type,
        .f32 => self.cir.idents.f32_type,
        .f64 => self.cir.idents.f64_type,
        .dec => self.cir.idents.dec_type,
        else => unreachable,
    };
}

fn builtinNumStmtFromIndices(indices: CIR.BuiltinIndices, num_kind: CIR.NumKind) CIR.Statement.Idx {
    return switch (num_kind) {
        .u8 => indices.u8_type,
        .i8 => indices.i8_type,
        .u16 => indices.u16_type,
        .i16 => indices.i16_type,
        .u32 => indices.u32_type,
        .i32 => indices.i32_type,
        .u64 => indices.u64_type,
        .i64 => indices.i64_type,
        .u128 => indices.u128_type,
        .i128 => indices.i128_type,
        .f32 => indices.f32_type,
        .f64 => indices.f64_type,
        .dec => indices.dec_type,
        else => unreachable,
    };
}

fn builtinNominalIdent(self: *const Self, decl: BuiltinNominalDecl) Ident.Idx {
    return switch (decl) {
        .list => self.cir.idents.builtin_list,
        .box => self.cir.idents.builtin_box,
        .try_type => self.cir.idents.builtin_try,
        .numeral => self.cir.idents.builtin_numeral,
        .num => |num_kind| self.builtinNumTypeIdent(num_kind),
    };
}

fn builtinNominalLabel(decl: BuiltinNominalDecl) []const u8 {
    return switch (decl) {
        .list => "List",
        .box => "Box",
        .try_type => "Try",
        .numeral => "Num.Numeral",
        .num => |num_kind| switch (num_kind) {
            .u8 => "Num.U8",
            .i8 => "Num.I8",
            .u16 => "Num.U16",
            .i16 => "Num.I16",
            .u32 => "Num.U32",
            .i32 => "Num.I32",
            .u64 => "Num.U64",
            .i64 => "Num.I64",
            .u128 => "Num.U128",
            .i128 => "Num.I128",
            .f32 => "Num.F32",
            .f64 => "Num.F64",
            .dec => "Num.Dec",
            else => unreachable,
        },
    };
}

const SourceDeclKind = enum {
    alias,
    nominal,
};

fn debugAssertSourceDeclKind(self: *const Self, source_decl: u32, kind: SourceDeclKind) void {
    debugAssertSourceDeclKindInEnv(self.cir, source_decl, kind);
}

fn debugAssertSourceDeclKindInEnv(env: *const ModuleEnv, source_decl: u32, kind: SourceDeclKind) void {
    if (builtin.mode != .Debug) return;

    if (source_decl >= env.store.nodes.len()) {
        std.debug.panic("type checker invariant violated: source declaration {} is outside node store", .{source_decl});
    }

    const node = env.store.nodes.get(@enumFromInt(source_decl));
    const ok = switch (kind) {
        .alias => node.tag == .statement_alias_decl,
        .nominal => node.tag == .statement_nominal_decl,
    };
    if (!ok) {
        std.debug.panic("type checker invariant violated: source declaration {} has tag {}, expected {s}", .{
            source_decl,
            node.tag,
            @tagName(kind),
        });
    }
}

fn sourceDeclForBuiltinNominal(self: *const Self, decl: BuiltinNominalDecl) u32 {
    if (!self.isCheckingBuiltinModuleDirectly() and self.builtin_ctx.builtin_indices != null) {
        const indices = self.builtin_ctx.builtin_indices.?;
        const stmt_idx = switch (decl) {
            .list => indices.list_type,
            .box => indices.box_type,
            .try_type => indices.try_type,
            .numeral => indices.numeral_type,
            .num => |num_kind| builtinNumStmtFromIndices(indices, num_kind),
        };
        const owner_env = self.builtin_ctx.builtin_module orelse self.cir;
        debugAssertSourceDeclKindInEnv(owner_env, @intFromEnum(stmt_idx), .nominal);
        return @intFromEnum(stmt_idx);
    }

    if (!self.isCheckingBuiltinModuleDirectly() and self.builtin_ctx.builtin_module != null) {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: builtin module env present without builtin indices", .{});
        }
        unreachable;
    }

    const stmt_idx = self.findLocalTypeDeclByName(self.builtinNominalIdent(decl)) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: Builtin.{s} declaration not found while checking Builtin", .{builtinNominalLabel(decl)});
        }
        unreachable;
    };
    self.debugAssertSourceDeclKind(@intFromEnum(stmt_idx), .nominal);
    return @intFromEnum(stmt_idx);
}

fn sourceDeclForBuiltinParseSpec(self: *const Self, decl: BuiltinParseSpecDecl) u32 {
    if (!self.isCheckingBuiltinModuleDirectly() and self.builtin_ctx.builtin_indices != null) {
        const indices = self.builtin_ctx.builtin_indices.?;
        const stmt_idx = switch (decl) {
            .str => indices.parse_str_spec_type,
            .record => indices.parse_record_spec_type,
            .tag_union => indices.parse_tag_union_spec_type,
        };
        const owner_env = self.builtin_ctx.builtin_module orelse self.cir;
        debugAssertSourceDeclKindInEnv(owner_env, @intFromEnum(stmt_idx), .nominal);
        return @intFromEnum(stmt_idx);
    }

    if (!self.isCheckingBuiltinModuleDirectly() and self.builtin_ctx.builtin_module != null) {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: builtin module env present without builtin indices", .{});
        }
        unreachable;
    }

    const ident = switch (decl) {
        .str => self.cir.idents.builtin_parse_str_spec,
        .record => self.cir.idents.builtin_parse_record_spec,
        .tag_union => self.cir.idents.builtin_parse_tag_union_spec,
    };
    const stmt_idx = self.findLocalTypeDeclByName(ident) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: Builtin parse spec declaration not found while checking Builtin", .{});
        }
        unreachable;
    };
    self.debugAssertSourceDeclKind(@intFromEnum(stmt_idx), .nominal);
    return @intFromEnum(stmt_idx);
}

/// Create a nominal List type with the given element type
fn mkListContent(self: *Self, elem_var: Var, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

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

    return try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        list_ident,
        backing_var,
        &type_args,
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinNominal(.list),
        false, // List is nominal (not opaque)
        true,
    );
}

/// Instantiate the builtin Iter type declaration and bind its item parameter.
fn mkIterVar(self: *Self, item_var: Var, env: *Env, region: Region) Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const iter_decl_var = if (self.builtin_ctx.builtin_module) |builtin_env| blk: {
        const indices = self.builtin_ctx.builtin_indices orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("type checker invariant violated: builtin module env present without builtin indices", .{});
            }
            unreachable;
        };
        const copied_var = try self.copyVar(ModuleEnv.varFrom(indices.iter_type), builtin_env, region);
        break :blk copied_var;
    } else blk: {
        const iter_stmt_idx = self.findLocalTypeDeclByName(self.cir.idents.builtin_iter) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("type checker invariant violated: Builtin.Iter declaration not found while checking Builtin", .{});
            }
            unreachable;
        };
        break :blk ModuleEnv.varFrom(iter_stmt_idx);
    };

    const iter_var = try self.instantiateVar(iter_decl_var, env, .{ .explicit = region });
    const iter_content = self.types.resolveVar(iter_var).desc.content;
    const nominal = iter_content.unwrapNominalType() orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: Builtin.Iter declaration did not instantiate to a nominal type", .{});
        }
        unreachable;
    };
    const args = self.types.sliceNominalArgs(nominal);
    if (args.len != 1) {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: Builtin.Iter expected one type argument, found {d}", .{args.len});
        }
        unreachable;
    }

    _ = try self.unify(args[0], item_var, env);
    return iter_var;
}

fn mkIteratorStepContent(self: *Self, item_var: Var, iter_var: Var, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    const item_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("item"));
    const rest_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("rest"));
    const record_ext = try self.freshFromContent(.{ .structure = .empty_record }, env, Region.zero());
    const record_fields = [_]types_mod.RecordField{
        .{ .name = item_ident, .var_ = item_var },
        .{ .name = rest_ident, .var_ = iter_var },
    };
    const record_fields_range = try self.types.appendRecordFields(&record_fields);
    const payload_record = try self.freshFromContent(.{ .structure = .{ .record = .{
        .fields = record_fields_range,
        .ext = record_ext,
    } } }, env, Region.zero());

    const skip_record_ext = try self.freshFromContent(.{ .structure = .empty_record }, env, Region.zero());
    const skip_record_fields = [_]types_mod.RecordField{
        .{ .name = rest_ident, .var_ = iter_var },
    };
    const skip_record_fields_range = try self.types.appendRecordFields(&skip_record_fields);
    const skip_payload_record = try self.freshFromContent(.{ .structure = .{ .record = .{
        .fields = skip_record_fields_range,
        .ext = skip_record_ext,
    } } }, env, Region.zero());

    const done_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("Done"));
    const one_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("One"));
    const skip_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("Skip"));

    const tags = [_]types_mod.Tag{
        try self.types.mkTag(done_ident, &.{}),
        try self.types.mkTag(one_ident, &.{payload_record}),
        try self.types.mkTag(skip_ident, &.{skip_payload_record}),
    };
    const ext_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, Region.zero());
    return try self.types.mkTagUnion(&tags, ext_var);
}

/// Create a nominal number type content (e.g., U8, I32, Dec)
/// Number types are defined in Builtin.roc nested inside Num module: Num.U8 :: [].{...}
/// They have no type parameters and their backing is the empty tag union []
fn mkNumberTypeContent(self: *Self, num_kind: CIR.NumKind, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    const type_ident = types_mod.TypeIdent{
        .ident_idx = self.builtinNumTypeIdent(num_kind),
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

    return try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        type_ident,
        backing_var,
        no_type_args,
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinNominal(.{ .num = num_kind }),
        true, // Number types are opaque (defined with ::)
        true,
    );
}

fn builtinNumKindFromTypeName(self: *const Self, type_name: Ident.Idx) ?CIR.NumKind {
    if (type_name.eql(self.cir.idents.u8) or type_name.eql(self.cir.idents.u8_type)) return .u8;
    if (type_name.eql(self.cir.idents.i8) or type_name.eql(self.cir.idents.i8_type)) return .i8;
    if (type_name.eql(self.cir.idents.u16) or type_name.eql(self.cir.idents.u16_type)) return .u16;
    if (type_name.eql(self.cir.idents.i16) or type_name.eql(self.cir.idents.i16_type)) return .i16;
    if (type_name.eql(self.cir.idents.u32) or type_name.eql(self.cir.idents.u32_type)) return .u32;
    if (type_name.eql(self.cir.idents.i32) or type_name.eql(self.cir.idents.i32_type)) return .i32;
    if (type_name.eql(self.cir.idents.u64) or type_name.eql(self.cir.idents.u64_type)) return .u64;
    if (type_name.eql(self.cir.idents.i64) or type_name.eql(self.cir.idents.i64_type)) return .i64;
    if (type_name.eql(self.cir.idents.u128) or type_name.eql(self.cir.idents.u128_type)) return .u128;
    if (type_name.eql(self.cir.idents.i128) or type_name.eql(self.cir.idents.i128_type)) return .i128;
    if (type_name.eql(self.cir.idents.f32) or type_name.eql(self.cir.idents.f32_type)) return .f32;
    if (type_name.eql(self.cir.idents.f64) or type_name.eql(self.cir.idents.f64_type)) return .f64;
    if (type_name.eql(self.cir.idents.dec) or type_name.eql(self.cir.idents.dec_type)) return .dec;
    return null;
}

fn builtinNominalDeclForSourceDecl(source_env: *const ModuleEnv, source_decl: ?u32) ?BuiltinNominalDecl {
    if (source_env.module_role != .builtin) return null;
    const raw_decl = source_decl orelse return null;
    if (raw_decl >= source_env.store.nodes.len()) return null;

    const node: CIR.Node.Idx = @enumFromInt(raw_decl);
    switch (source_env.store.nodes.get(node).tag) {
        .statement_alias_decl, .statement_nominal_decl => {},
        else => return null,
    }
    const statement_idx: CIR.Statement.Idx = @enumFromInt(raw_decl);
    const header_idx = switch (source_env.store.getStatement(statement_idx)) {
        .s_alias_decl => |alias| alias.header,
        .s_nominal_decl => |nominal| nominal.header,
        else => return null,
    };
    const header = source_env.store.getTypeHeader(header_idx);
    return builtinNominalDeclForIdentInEnv(source_env, header.name);
}

fn builtinNominalDeclForIdentInEnv(source_env: *const ModuleEnv, type_ident: Ident.Idx) ?BuiltinNominalDecl {
    const common = source_env.idents;
    if (type_ident.eql(common.list) or type_ident.eql(common.builtin_list)) return .list;
    if (type_ident.eql(common.box) or type_ident.eql(common.builtin_box)) return .box;
    if (type_ident.eql(common.@"try") or type_ident.eql(common.builtin_try)) return .try_type;
    if (type_ident.eql(common.builtin_numeral)) return .numeral;
    if (type_ident.eql(common.u8) or type_ident.eql(common.u8_type)) return .{ .num = .u8 };
    if (type_ident.eql(common.i8) or type_ident.eql(common.i8_type)) return .{ .num = .i8 };
    if (type_ident.eql(common.u16) or type_ident.eql(common.u16_type)) return .{ .num = .u16 };
    if (type_ident.eql(common.i16) or type_ident.eql(common.i16_type)) return .{ .num = .i16 };
    if (type_ident.eql(common.u32) or type_ident.eql(common.u32_type)) return .{ .num = .u32 };
    if (type_ident.eql(common.i32) or type_ident.eql(common.i32_type)) return .{ .num = .i32 };
    if (type_ident.eql(common.u64) or type_ident.eql(common.u64_type)) return .{ .num = .u64 };
    if (type_ident.eql(common.i64) or type_ident.eql(common.i64_type)) return .{ .num = .i64 };
    if (type_ident.eql(common.u128) or type_ident.eql(common.u128_type)) return .{ .num = .u128 };
    if (type_ident.eql(common.i128) or type_ident.eql(common.i128_type)) return .{ .num = .i128 };
    if (type_ident.eql(common.f32) or type_ident.eql(common.f32_type)) return .{ .num = .f32 };
    if (type_ident.eql(common.f64) or type_ident.eql(common.f64_type)) return .{ .num = .f64 };
    if (type_ident.eql(common.dec) or type_ident.eql(common.dec_type)) return .{ .num = .dec };
    return null;
}

fn builtinNominalDeclForBuiltinSourceDecl(self: *const Self, source_decl: ?u32) ?BuiltinNominalDecl {
    const raw_decl = source_decl orelse return null;

    if (self.cir.module_role == .builtin) {
        return builtinNominalDeclForSourceDecl(self.cir, raw_decl);
    }

    const indices = self.builtin_ctx.builtin_indices orelse return null;
    if (raw_decl == @intFromEnum(indices.list_type)) return .list;
    if (raw_decl == @intFromEnum(indices.box_type)) return .box;
    if (raw_decl == @intFromEnum(indices.try_type)) return .try_type;
    if (raw_decl == @intFromEnum(indices.numeral_type)) return .numeral;
    if (raw_decl == @intFromEnum(indices.u8_type)) return .{ .num = .u8 };
    if (raw_decl == @intFromEnum(indices.i8_type)) return .{ .num = .i8 };
    if (raw_decl == @intFromEnum(indices.u16_type)) return .{ .num = .u16 };
    if (raw_decl == @intFromEnum(indices.i16_type)) return .{ .num = .i16 };
    if (raw_decl == @intFromEnum(indices.u32_type)) return .{ .num = .u32 };
    if (raw_decl == @intFromEnum(indices.i32_type)) return .{ .num = .i32 };
    if (raw_decl == @intFromEnum(indices.u64_type)) return .{ .num = .u64 };
    if (raw_decl == @intFromEnum(indices.i64_type)) return .{ .num = .i64 };
    if (raw_decl == @intFromEnum(indices.u128_type)) return .{ .num = .u128 };
    if (raw_decl == @intFromEnum(indices.i128_type)) return .{ .num = .i128 };
    if (raw_decl == @intFromEnum(indices.f32_type)) return .{ .num = .f32 };
    if (raw_decl == @intFromEnum(indices.f64_type)) return .{ .num = .f64 };
    if (raw_decl == @intFromEnum(indices.dec_type)) return .{ .num = .dec };
    return null;
}

fn builtinNumKindFromBuiltinSourceDecl(self: *const Self, source_decl: ?u32) ?CIR.NumKind {
    return switch (self.builtinNominalDeclForBuiltinSourceDecl(source_decl) orelse return null) {
        .num => |num_kind| num_kind,
        else => null,
    };
}

fn mkBuiltinNumberTypeContentFromKind(
    self: *Self,
    num_kind: CIR.NumKind,
    env: *Env,
) Allocator.Error!Content {
    return switch (num_kind) {
        .num_unbound, .int_unbound => unreachable,
        else => try self.mkNumberTypeContent(num_kind, env),
    };
}

fn findLocalTypeDeclByName(self: *const Self, type_name: Ident.Idx) ?CIR.Statement.Idx {
    if (self.findLocalTypeDeclByNameInSpan(self.cir.type_decls, type_name)) |stmt_idx| return stmt_idx;
    if (self.findLocalTypeDeclByNameInSpan(self.cir.forward_type_decls, type_name)) |stmt_idx| return stmt_idx;
    if (self.findLocalTypeDeclByNameInSpan(self.cir.all_statements, type_name)) |stmt_idx| return stmt_idx;
    if (self.findLocalTypeDeclByNameInSpan(self.cir.builtin_statements, type_name)) |stmt_idx| return stmt_idx;
    return null;
}

fn findLocalTypeDeclByNameInSpan(
    self: *const Self,
    span: CIR.Statement.Span,
    type_name: Ident.Idx,
) ?CIR.Statement.Idx {
    const stmts = self.cir.store.sliceStatements(span);
    for (stmts) |stmt_idx| {
        const stmt = self.cir.store.getStatement(stmt_idx);
        const header_idx = switch (stmt) {
            .s_alias_decl => |alias| alias.header,
            .s_nominal_decl => |nominal| nominal.header,
            else => continue,
        };

        const header = self.cir.store.getTypeHeader(header_idx);
        if (header.name.eql(type_name) or header.relative_name.eql(type_name)) return stmt_idx;
    }
    return null;
}

fn unifyTypedLiteralWithExplicitType(
    self: *Self,
    flex_var: Var,
    expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    env: *Env,
) Allocator.Error!void {
    const suffix_type = self.cir.numericSuffixTypeForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("typed numeric literal reached checking without a canonicalized suffix target", .{});
        }
        unreachable;
    };

    switch (suffix_type.target()) {
        .builtin => |num_kind| {
            try self.unifyWith(flex_var, try self.mkBuiltinNumberTypeContentFromKind(num_kind, env), env);
        },
        .local => |stmt_idx| {
            const local_decl_var = ModuleEnv.varFrom(stmt_idx);
            const resolved_var = if (self.isForClauseAliasStatement(stmt_idx))
                local_decl_var
            else
                try self.instantiateVar(local_decl_var, env, .{ .explicit = expr_region });

            _ = try self.unify(flex_var, resolved_var, env);
        },
        .external => |external| {
            if (try self.resolveVarFromExternal(external.import_idx, external.target_node_idx)) |ext_ref| {
                const instantiated_var = try self.instantiateVar(
                    ext_ref.local_var,
                    env,
                    .{ .explicit = expr_region },
                );
                _ = try self.unify(flex_var, instantiated_var, env);
            } else {
                try self.unifyWith(flex_var, .err, env);
            }
        },
    }
}

fn explicitTypeSuffixVar(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    expr_region: Region,
    env: *Env,
) Allocator.Error!?Var {
    const suffix_type = self.cir.numericSuffixTypeForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse return null;
    const suffix_var = try self.fresh(env, expr_region);

    switch (suffix_type.target()) {
        .builtin => |num_kind| {
            try self.unifyWith(suffix_var, try self.mkBuiltinNumberTypeContentFromKind(num_kind, env), env);
        },
        .local => |stmt_idx| {
            const local_decl_var = ModuleEnv.varFrom(stmt_idx);
            const resolved_var = if (self.isForClauseAliasStatement(stmt_idx))
                local_decl_var
            else
                try self.instantiateVar(local_decl_var, env, .{ .explicit = expr_region });

            _ = try self.unify(suffix_var, resolved_var, env);
        },
        .external => |external| {
            if (try self.resolveVarFromExternal(external.import_idx, external.target_node_idx)) |ext_ref| {
                const instantiated_var = try self.instantiateVar(
                    ext_ref.local_var,
                    env,
                    .{ .explicit = expr_region },
                );
                _ = try self.unify(suffix_var, instantiated_var, env);
            } else {
                try self.unifyWith(suffix_var, .err, env);
            }
        },
    }

    return suffix_var;
}

fn typedLiteralTargetsBuiltin(self: *const Self, expr_idx: CIR.Expr.Idx, num_kind: CIR.NumKind) bool {
    const suffix_type = self.cir.numericSuffixTypeForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse return false;
    return switch (suffix_type.target()) {
        .builtin => |target_kind| target_kind == num_kind,
        else => false,
    };
}

/// Create a flex variable with a from_numeral constraint for numeric literals.
/// This constraint will be checked during deferred constraint checking to validate
/// that the numeric literal can be converted to the unified type.
/// Returns the flex var which has the constraint attached, and the dispatcher var
/// (first arg of from_numeral) is unified with the flex var so they share the same name.
fn mkFlexWithFromNumeralConstraint(
    self: *Self,
    source_node: ?CIR.Node.Idx,
    num_literal_info: types_mod.NumeralInfo,
    env: *Env,
) Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const from_numeral_ident = self.cir.idents.from_numeral;

    // Create the flex var first - this represents the target type `a`
    const flex_rank = env.rank();
    const flex_var = try self.freshFromContentAtRank(.{ .flex = Flex.init() }, env, num_literal_info.region, flex_rank);

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
    if (source_node) |node_idx| {
        try self.cir.recordNumeralDispatchPlan(node_idx, flex_var, fn_var);
    }

    // Create the constraint with numeric literal info
    const constraint = types_mod.StaticDispatchConstraint{
        .fn_name = from_numeral_ident,
        .fn_var = fn_var,
        .origin = .{ .from_literal = .{ .numeral = num_literal_info } },
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
    try self.open_literal_vars.append(self.gpa, flex_var);

    return flex_var;
}

/// Create a flex variable with a from_quote constraint for string literals.
/// The constraint's function type is `Str -> Try(a, [BadQuotedBytes(Str)])`,
/// where the Str is the literal's content after escape processing.
fn mkFlexWithFromQuoteConstraint(
    self: *Self,
    source_node: ?CIR.Node.Idx,
    region: Region,
    env: *Env,
) Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    const from_quote_ident = self.cir.idents.from_quote;

    // Create the flex var first - this represents the target type `a`
    const flex_rank = env.rank();
    const flex_var = try self.freshFromContentAtRank(.{ .flex = Flex.init() }, env, region, flex_rank);

    // Create the argument type: Str
    const arg_var = try self.freshStr(env, region);

    // Create the error type: [BadQuotedBytes(Str)] (closed tag union)
    const str_var = self.str_var;
    const bad_quoted_bytes_tag_ident = try @constCast(self.cir).insertIdent(
        base.Ident.for_text("BadQuotedBytes"),
    );
    const bad_quoted_bytes_tag = try self.types.mkTag(
        bad_quoted_bytes_tag_ident,
        &.{str_var},
    );
    const err_ext_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, region);
    const err_type = try self.types.mkTagUnion(&.{bad_quoted_bytes_tag}, err_ext_var);
    const err_var = try self.freshFromContent(err_type, env, region);

    // Create Try(flex_var, err_var) as the return type
    const try_type_content = try self.mkTryContent(flex_var, err_var, env);
    const ret_var = try self.freshFromContent(try_type_content, env, region);

    const func_content = types_mod.Content{
        .structure = types_mod.FlatType{
            .fn_unbound = types_mod.Func{
                .args = try self.types.appendVars(&.{arg_var}),
                .ret = ret_var,
                .needs_instantiation = false,
            },
        },
    };
    const fn_var = try self.freshFromContent(func_content, env, region);
    if (source_node) |node_idx| {
        try self.cir.recordQuoteDispatchPlan(node_idx, flex_var, fn_var);
    }

    const constraint = types_mod.StaticDispatchConstraint{
        .fn_name = from_quote_ident,
        .fn_var = fn_var,
        .origin = .{ .from_literal = .quote },
    };

    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

    const flex_content = types_mod.Content{
        .flex = types_mod.Flex{
            .name = null,
            .constraints = constraint_range,
        },
    };
    try self.unifyWith(flex_var, flex_content, env);
    // Shares the open-literal worklist with numerals: the literal-defaulting
    // and constraint-compatibility sweeps it gates handle every literal kind.
    try self.open_literal_vars.append(self.gpa, flex_var);

    return flex_var;
}

fn recordedNumeralLiteralForExpr(self: *const Self, expr_idx: CIR.Expr.Idx) ModuleEnv.NumeralLiteral {
    return self.cir.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("missing recorded exact numeral for expression {}", .{@intFromEnum(expr_idx)});
        }
        unreachable;
    };
}

fn exactNumeralInfoForExpr(self: *const Self, expr_idx: CIR.Expr.Idx, region: Region) Allocator.Error!types_mod.NumeralInfo {
    const literal = self.recordedNumeralLiteralForExpr(expr_idx);
    const text = try numeralLiteralDecimalText(self.gpa, self.cir, literal);
    defer self.gpa.free(text);
    const fits_dec = builtins.dec.RocDec.fromNonemptySlice(text) != null;
    const is_fractional = literal.after_decimal_digit_count != 0 or literal.hadDecimalPoint();
    return types_mod.NumeralInfo.fromExact(literal.isNegative(), is_fractional, fits_dec, region);
}

fn numeralLiteralDecimalText(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    literal: ModuleEnv.NumeralLiteral,
) Allocator.Error![]const u8 {
    const before = try base256DecimalText(allocator, module_env.numeralDigitsBefore(literal), 1);
    defer allocator.free(before);

    const after_min_digits: usize = std.math.cast(usize, literal.after_decimal_digit_count) orelse {
        @panic("recorded numeral literal decimal digit count exceeded host usize");
    };
    const after = if (after_min_digits == 0)
        try allocator.alloc(u8, 0)
    else
        try base256DecimalText(allocator, module_env.numeralDigitsAfter(literal), after_min_digits);
    defer allocator.free(after);

    const sign_len: usize = @intFromBool(literal.isNegative());
    const dot_len: usize = @intFromBool(after_min_digits > 0);
    const total_len = sign_len + before.len + dot_len + after.len;
    const text = try allocator.alloc(u8, total_len);
    var offset: usize = 0;
    if (literal.isNegative()) {
        text[offset] = '-';
        offset += 1;
    }
    @memcpy(text[offset..][0..before.len], before);
    offset += before.len;
    if (after_min_digits > 0) {
        text[offset] = '.';
        offset += 1;
        @memcpy(text[offset..][0..after.len], after);
    }
    return text;
}

fn base256DecimalText(allocator: Allocator, bytes_be: []const u8, min_digits: usize) Allocator.Error![]const u8 {
    var first_nonzero: usize = 0;
    while (first_nonzero < bytes_be.len and bytes_be[first_nonzero] == 0) : (first_nonzero += 1) {}

    if (first_nonzero == bytes_be.len) {
        const len = @max(min_digits, 1);
        const out = try allocator.alloc(u8, len);
        @memset(out, '0');
        return out;
    }

    var current_buf = try allocator.dupe(u8, bytes_be[first_nonzero..]);
    defer allocator.free(current_buf);
    var current_len = current_buf.len;
    var digits_rev = std.ArrayList(u8).empty;
    defer digits_rev.deinit(allocator);

    while (current_len > 0) {
        const current = current_buf[0..current_len];
        var quotient = try allocator.alloc(u8, current.len);
        var quotient_len: usize = 0;
        var remainder: u16 = 0;
        for (current) |byte| {
            const value = remainder * 256 + byte;
            const digit: u8 = @intCast(value / 10);
            remainder = value % 10;
            if (digit != 0 or quotient_len != 0) {
                quotient[quotient_len] = digit;
                quotient_len += 1;
            }
        }
        // Free the freshly-allocated quotient if recording the digit fails,
        // since it has not yet been adopted into current_buf.
        digits_rev.append(allocator, '0' + @as(u8, @intCast(remainder))) catch |err| {
            allocator.free(quotient);
            return err;
        };
        allocator.free(current_buf);
        current_buf = quotient;
        current_len = quotient_len;
    }

    const digit_count = digits_rev.items.len;
    const total_len = @max(digit_count, min_digits);
    const out = try allocator.alloc(u8, total_len);
    const pad = total_len - digit_count;
    @memset(out[0..pad], '0');
    for (digits_rev.items, 0..) |digit, i| {
        out[pad + digit_count - 1 - i] = digit;
    }
    return out;
}

/// Create a nominal Box type with the given element type
fn mkBoxContent(self: *Self, elem_var: Var) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    const box_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.box,
    };

    // The backing var is the element type var
    const backing_var = elem_var;
    const type_args = [_]Var{elem_var};

    return try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        box_ident,
        backing_var,
        &type_args,
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinNominal(.box),
        false, // Box is nominal (not opaque)
        true,
    );
}

/// Create a nominal Try type with the given success and error types.
/// This is used for creating Try types in function signatures (e.g., from_numeral).
fn mkTryContent(self: *Self, ok_var: Var, err_var: Var, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

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

    return try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        try_ident,
        backing_var,
        &type_args,
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinNominal(.try_type),
        false, // Try is nominal (not opaque)
        true,
    );
}

fn mkParseSpecVar(
    self: *Self,
    decl: BuiltinParseSpecDecl,
    shape_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!Var {
    const ident_idx = switch (decl) {
        .str => self.cir.idents.builtin_parse_str_spec,
        .record => self.cir.idents.builtin_parse_record_spec,
        .tag_union => self.cir.idents.builtin_parse_tag_union_spec,
    };
    const backing_var = try self.freshFromContent(.{ .structure = .empty_record }, env, region);
    return try self.freshFromContent(try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        .{ .ident_idx = ident_idx },
        backing_var,
        &.{shape_var},
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinParseSpec(decl),
        true,
        true,
    ), env, region);
}

/// Create the transparent builtin Numeral type used by from_numeral.
fn mkNumeralContent(self: *Self, env: *Env) Allocator.Error!Content {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Use the pre-interned builtin ident instead of reconstructing the text.
    const numeral_ident = types_mod.TypeIdent{
        .ident_idx = self.cir.idents.builtin_numeral,
    };

    const u8_before = try self.freshFromContent(try self.mkNumberTypeContent(.u8, env), env, Region.zero());
    const u8_after = try self.freshFromContent(try self.mkNumberTypeContent(.u8, env), env, Region.zero());
    const digits_before = try self.freshFromContent(try self.mkListContent(u8_before, env), env, Region.zero());
    const digits_after = try self.freshFromContent(try self.mkListContent(u8_after, env), env, Region.zero());
    const digit_count = try self.freshFromContent(try self.mkNumberTypeContent(.u64, env), env, Region.zero());
    const is_negative = try self.freshBool(env, Region.zero());

    const record_ext = try self.freshFromContent(.{ .structure = .empty_record }, env, Region.zero());
    const fields = try self.types.appendRecordFields(&[_]types_mod.RecordField{
        .{ .name = self.cir.idents.is_negative, .var_ = is_negative },
        .{ .name = self.cir.idents.digits_before_pt, .var_ = digits_before },
        .{ .name = self.cir.idents.digits_after_pt, .var_ = digits_after },
        .{ .name = self.cir.idents.digits_after_pt_count, .var_ = digit_count },
    });
    const record_var = try self.freshFromContent(.{ .structure = .{ .record = .{
        .fields = fields,
        .ext = record_ext,
    } } }, env, Region.zero());

    const literal_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text("Literal"));
    const literal_tag = try self.types.mkTag(literal_ident, &.{record_var});
    const union_ext = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, Region.zero());
    const backing_content = try self.types.mkTagUnion(&.{literal_tag}, union_ext);
    const backing_var = try self.freshFromContent(backing_content, env, Region.zero());

    return try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
        numeral_ident,
        backing_var,
        &.{}, // No type args
        self.builtinOriginModule(),
        self.sourceDeclForBuiltinNominal(.numeral),
        false, // Numeral is transparent so custom from_numeral methods can inspect it.
        true,
    );
}

// updating vars //

/// Whether the fresh var built by `unifyWithFresh` uses the current
/// generalization rank or the target var's own rank.
const FreshRank = enum { current, target };

/// Unify `target_var` against freshly-built `content`. When `target_var` is a
/// root flex placeholder we mutate its descriptor in place (the common case),
/// saving a typeslot and a full unification run.
fn unifyWithFresh(
    self: *Self,
    target_var: Var,
    content: types_mod.Content,
    env: *Env,
    comptime rank_policy: FreshRank,
) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const resolved_target = self.types.resolveVar(target_var);
    switch (rank_policy) {
        .current => {
            if (resolved_target.is_root and resolved_target.desc.rank == env.rank() and resolved_target.desc.content == .flex) {
                // The vast majority of the time, we call unify with on a placeholder
                // CIR var. In this case, we can safely replace the type descriptor
                // directly, saving a typeslot and unifcation run
                var desc = resolved_target.desc;
                desc.content = content;
                try self.types.dangerousSetVarDesc(target_var, desc);
                return;
            }
            const fresh_var = try self.freshFromContent(content, env, self.getRegionAt(target_var));
            if (builtin.mode == .Debug) {
                const target_var_rank = self.types.resolveVar(target_var).desc.rank;
                const fresh_var_rank = self.types.resolveVar(fresh_var).desc.rank;
                if (@intFromEnum(target_var_rank) > @intFromEnum(fresh_var_rank)) {
                    std.debug.panic("trying unifyWith unexpected ranks {} & {}", .{ @intFromEnum(target_var_rank), @intFromEnum(fresh_var_rank) });
                }
            }
            _ = try self.unify(target_var, fresh_var, env);
        },
        .target => {
            if (resolved_target.is_root and resolved_target.desc.content == .flex) {
                var desc = resolved_target.desc;
                desc.content = content;
                try self.types.dangerousSetVarDesc(target_var, desc);
                return;
            }
            const target_rank = resolved_target.desc.rank;
            const fresh_var = try self.freshFromContentAtRank(content, env, self.getRegionAt(target_var), target_rank);
            _ = try self.unify(target_var, fresh_var, env);
        },
    }
}

/// Unify the provided variable with the provided content
///
/// If the var is a flex at the current rank, skip unifcation and simply update
/// the type descriptor
///
/// This should primarily be use to set CIR node vars that were initially filled with placeholders
fn unifyWith(self: *Self, target_var: Var, content: types_mod.Content, env: *Env) std.mem.Allocator.Error!void {
    return self.unifyWithFresh(target_var, content, env, .current);
}

fn unifyWithTargetRank(self: *Self, target_var: Var, content: types_mod.Content, env: *Env) std.mem.Allocator.Error!void {
    return self.unifyWithFresh(target_var, content, env, .target);
}

/// Give a var, ensure it's not a redirect and set its rank.
/// If the var is already a redirect, this is a no-op - the root's rank was set when
/// the redirect was created during unification. This can happen when a variable is
/// unified with another before its rank is explicitly set, which is benign.
fn setVarRank(self: *Self, target_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(target_var);
    if (resolved.is_root) {
        try self.types.setDescRank(resolved.desc_idx, env.rank());
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
fn copyBuiltinTypes(self: *Self) Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (self.builtin_types_copied) return;

    const checking_builtin_directly = self.isCheckingBuiltinModuleDirectly();
    const bool_stmt_idx = if (checking_builtin_directly)
        self.findLocalTypeDeclByName(self.cir.idents.bool_type) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("type checker invariant violated: local Builtin.Bool declaration not found while checking Builtin", .{});
            }
            unreachable;
        }
    else
        self.builtin_ctx.bool_stmt;
    const str_stmt_idx = if (checking_builtin_directly)
        self.findLocalTypeDeclByName(self.cir.idents.builtin_str) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("type checker invariant violated: local Builtin.Str declaration not found while checking Builtin", .{});
            }
            unreachable;
        }
    else
        self.builtin_ctx.str_stmt;

    if (!checking_builtin_directly and self.builtin_ctx.builtin_module != null) {
        const builtin_env = self.builtin_ctx.builtin_module.?;
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
    self.builtin_types_copied = true;
}

/// Public `checkFile` function.
pub fn checkFile(self: *Self) std.mem.Allocator.Error!void {
    return self.checkFileInternal(false);
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

    // When checking the Builtin module directly (e.g., `roc check Builtin.roc`),
    // the normal pipeline assigns a package-qualified module name like "module.Builtin".
    // But the pre-compiled Builtin binary uses the unqualified "Builtin" as origin_module
    // for all its types. This causes type mismatches when locally-defined types (using
    // "module.Builtin") are unified with types from the pre-compiled module (using "Builtin").
    // Fix: use the unqualified "Builtin" to match the pre-compiled module's convention.
    if (self.builtin_ctx.builtin_module != null and self.isCheckingBuiltinModuleDirectly()) {
        self.builtin_ctx.module_name = self.cir.idents.builtin_module;
        // Also update qualified_module_ident so that opaque type checks
        // (canLiftInner) allow pattern matching on types defined in this module.
        self.cir.qualified_module_ident = self.cir.idents.builtin_module;
    }

    // Copy builtin types (Bool, Try) into this module's type store
    // Note that bool_var and try_var will have generalized rank
    try self.copyBuiltinTypes();

    // First, iterate over the builtin statements, generating types for each type declaration
    // Note that any types generated will be generalized
    for (0..self.cir.builtin_statements.span.len) |stmt_offset| {
        const builtin_stmt_idx = self.cir.store.statementAt(self.cir.builtin_statements, stmt_offset);
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(builtin_stmt_idx, &env);
    }

    // Reserve every canonical type constructor from canonicalization's explicit
    // type-declaration table before generating type bodies. Transparent nominal
    // backing types and recursive declarations both rely on these constructor
    // shells being present before any later checking work consumes them.
    for (0..self.cir.type_decls.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(self.cir.type_decls, stmt_offset);
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);

        switch (stmt) {
            .s_alias_decl => |alias| {
                try self.setVarRank(stmt_var, &env);
                try self.predeclareAliasDecl(stmt_var, alias, &env);
            },
            .s_nominal_decl => |nominal| {
                try self.setVarRank(stmt_var, &env);
                try self.predeclareNominalDecl(stmt_var, nominal, &env);
            },
            else => {},
        }
    }

    // First pass: generate types for each type declaration
    // Note that any types generated will be generalized
    for (0..self.cir.all_statements.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(self.cir.all_statements, stmt_offset);
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);

        switch (stmt) {
            .s_alias_decl => |alias| {
                try self.generateAliasDecl(stmt_idx, stmt_var, alias, &env);
            },
            .s_nominal_decl => |nominal| {
                try self.generateNominalDecl(stmt_idx, stmt_var, nominal, &env);
            },
            .s_runtime_error => {
                try self.setVarRank(stmt_var, &env);
                try self.unifyWith(stmt_var, .err, &env);
            },
            .s_type_anno => |type_anno| {
                try self.setVarRank(stmt_var, &env);
                try self.generateStandaloneTypeAnno(stmt_var, type_anno, &env);
            },
            else => {
                // All other stmt types are invalid at the top level
            },
        }
    }

    // Next, capture all top level defs
    // This is used to support out-of-order defs
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
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
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        try self.checkDef(def_idx, &env);

        // Ensure that after processing a def, checkDef correctly restores the
        // rank to outermost for the next level of processing
        std.debug.assert(env.rank() == .outermost);
    }

    // Finally, type-check top-level statements (like expect)
    // These are separate from defs and need to be checked after all defs are processed
    // so that lookups can find their definitions
    for (0..self.cir.all_statements.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(self.cir.all_statements, stmt_offset);
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);
        const stmt_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(stmt_idx));

        switch (stmt) {
            .s_expect => |expr_stmt| {
                // Check the body expression
                const expect_does_fx = try self.checkExpectBody(expr_stmt.body, &env, Expected.none(), stmt_region);
                if (expect_does_fx) {
                    _ = try self.problems.appendProblem(self.gpa, .{ .effectful_expect = .{
                        .region = stmt_region,
                    } });
                }
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

    if (!skip_numeric_defaults) {
        try self.finalizeLiteralDefaults(&env);

        // After finalizing numeric defaults, resolve any remaining deferred
        // static dispatch constraints (e.g., Dec.plus, Dec.to_str).
        if (env.deferred_static_dispatch_constraints.items.items.len > 0) {
            try self.checkStaticDispatchConstraints(&env, true);
        }
    }

    try self.validateToInspectMethodTypes(&env);
    try self.checkAllFromNumeralFlexConstraintCompatibility(&env, true);
    try self.checkInstantiatedStaticDispatchConstraints(&env, true);

    // After solving all deferred constraints, check for infinite types
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        try self.checkForInfiniteType(CIR.Def.Idx, def_idx);
    }

    if (!self.has_can_error_diagnostics()) {
        try self.poisonErroneousValueUses();
        try self.poisonErroneousValueExprs();
    }

    try self.reportPolymorphicTopLevelValues();

    try self.checkPlatformHostedSection();

    // Two coordinated ambiguity sweeps share a `reported` set keyed by resolved
    // dispatcher var so a receiver caught per-instantiation is not also reported
    // by the def-site sweep. The per-instantiation sweep runs first because it
    // produces the more informative two-region diagnostic; the def-site sweep then
    // covers the direct cases (`poly().to_i128()`, `poly() == poly()`) where the
    // dispatch is created at the use site rather than copied by an instantiation.
    self.reported_dispatch_vars.clearRetainingCapacity();

    // The external pinnable set: every resolved var reachable through a function
    // ARGUMENT position of a top-level def, including argument positions of
    // function values returned by that def. These are the vars an outside caller
    // can still pin after this module is checked.
    var external_pinnable = std.AutoHashMap(Var, void).init(self.gpa);
    defer external_pinnable.deinit();
    try self.collectExternalPinnableVars(&external_pinnable);

    // The full pinnable set additionally includes direct lambda parameters. That
    // remains correct for direct dispatch expressions on uncalled function values:
    // a future call can pin such a parameter. Instantiated where-clause contracts
    // use only `external_pinnable`, because the call that instantiated the
    // contract must satisfy it now.
    self.pinnable_vars.clearRetainingCapacity();
    try self.collectPinnableVars(&self.pinnable_vars, &external_pinnable);

    try self.reportAmbiguousStaticDispatchPerInstantiation(&self.reported_dispatch_vars, &self.pinnable_vars, &external_pinnable);
    try self.reportAmbiguousStaticDispatch(&self.reported_dispatch_vars, &self.pinnable_vars);
}

/// Populate `pinnable` with every resolved var that an outside caller can still
/// pin through a top-level def's function argument positions. Curried/returned
/// function arguments count too, because calling the returned function supplies
/// those arguments later.
fn collectExternalPinnableVars(self: *Self, pinnable: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!void {
    // Shared across all defs of this one-shot pass; arg structure is already
    // deduped via `pinnable` inside collectReachableVars.
    self.pinnable_spine_visited.clearRetainingCapacity();

    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        try self.collectArgPositionVars(ModuleEnv.varFrom(def_idx), pinnable, &self.pinnable_spine_visited);
    }
}

/// Populate the broader direct-dispatch pinnable set. It starts with external
/// pinnable vars, then adds every var reachable from lambda parameters because a
/// direct dispatch hidden inside an uncalled function value can still be pinned
/// when that function is called.
fn collectPinnableVars(
    self: *Self,
    pinnable: *std.AutoHashMap(Var, void),
    external_pinnable: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!void {
    var external_iter = external_pinnable.keyIterator();
    while (external_iter.next()) |var_| {
        try pinnable.put(var_.*, {});
    }

    // Open literals are never dead ends: defaulting resolves them, the deferred
    // dispatch fires, and the resolved method's signature pins every var in the
    // constraint fn — return position included. So each still-open literal seeds
    // the closure with everything reachable through its constraint signatures.
    // Without this, an instantiated helper's RETURN var (`add_x(5)` with
    // `add_x : a -> r where [a.plus : (a, x) -> r]`) was falsely reported MISSING
    // METHOD. Generalized entries are deliberately NOT skipped: a generalized
    // literal stays open and resolves per instantiation, which is exactly why its
    // chain is pinnable.
    for (self.open_literal_vars.items) |literal_var| {
        const resolved = self.types.resolveVar(literal_var);
        if (resolved.desc.content != .flex) continue;
        if (self.varLiteralKind(resolved.var_) == null) continue;
        try self.collectReachableVars(resolved.var_, pinnable);
    }

    // Lambda param spans recorded as each lambda was checked — explicit upstream
    // data; union-find roots resolve here, where it is meaningful.
    for (self.checked_lambda_params.items) |arg_span| {
        for (self.cir.store.slicePatterns(arg_span)) |pattern_idx| {
            try self.collectReachableVars(ModuleEnv.varFrom(pattern_idx), pinnable);
        }
    }
}

fn constraintExprForFnVar(self: *Self, fn_var: Var) ?CIR.Expr.Idx {
    if (self.constraint_expr_by_fn_var.count() == 0) return null;

    if (self.constraint_expr_by_fn_var.get(fn_var)) |expr_idx| return expr_idx;

    const resolved = self.types.resolveVar(fn_var).var_;
    if (resolved != fn_var) {
        if (self.constraint_expr_by_fn_var.get(resolved)) |expr_idx| return expr_idx;
    }

    return null;
}

fn expectRegionForFnVar(self: *Self, fn_var: Var) ?Region {
    if (self.expect_region_by_constraint_fn_var.count() == 0) return null;

    if (self.expect_region_by_constraint_fn_var.get(fn_var)) |region| return region;

    const resolved = self.types.resolveVar(fn_var).var_;
    if (resolved != fn_var) {
        if (self.expect_region_by_constraint_fn_var.get(resolved)) |region| return region;
    }

    return null;
}

fn recordConstraintExprForFnVar(
    self: *Self,
    fn_var: Var,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    try self.constraint_expr_by_fn_var.put(fn_var, expr_idx);

    const resolved = self.types.resolveVar(fn_var).var_;
    if (resolved != fn_var) {
        try self.constraint_expr_by_fn_var.put(resolved, expr_idx);
    }
}

fn recordExpectRegionForFnVar(
    self: *Self,
    fn_var: Var,
    region: Region,
) std.mem.Allocator.Error!void {
    try self.expect_region_by_constraint_fn_var.put(fn_var, region);

    const resolved = self.types.resolveVar(fn_var).var_;
    if (resolved != fn_var) {
        try self.expect_region_by_constraint_fn_var.put(resolved, region);
    }
}

fn linkConstraintMetadata(
    self: *Self,
    left: Var,
    right: Var,
) std.mem.Allocator.Error!void {
    if (self.constraintExprForFnVar(left) orelse self.constraintExprForFnVar(right)) |expr_idx| {
        try self.recordConstraintExprForFnVar(left, expr_idx);
        try self.recordConstraintExprForFnVar(right, expr_idx);
    }

    if (self.expectRegionForFnVar(left) orelse self.expectRegionForFnVar(right)) |region| {
        try self.recordExpectRegionForFnVar(left, region);
        try self.recordExpectRegionForFnVar(right, region);
    }
}

fn hasConstraintMetadata(self: *Self) bool {
    return self.constraint_expr_by_fn_var.count() > 0 or self.expect_region_by_constraint_fn_var.count() > 0;
}

fn copyConstraintMetadata(
    self: *Self,
    old_var: Var,
    fresh_var: Var,
) std.mem.Allocator.Error!void {
    std.debug.assert(self.hasConstraintMetadata());

    var maybe_expr_idx = self.constraint_expr_by_fn_var.get(old_var);
    var maybe_region = self.expect_region_by_constraint_fn_var.get(old_var);

    if (maybe_expr_idx == null or maybe_region == null) {
        const resolved_old = self.types.resolveVar(old_var).var_;
        if (resolved_old != old_var) {
            if (maybe_expr_idx == null) {
                maybe_expr_idx = self.constraint_expr_by_fn_var.get(resolved_old);
            }
            if (maybe_region == null) {
                maybe_region = self.expect_region_by_constraint_fn_var.get(resolved_old);
            }
        }
    }

    if (maybe_expr_idx == null and maybe_region == null) return;

    const resolved_fresh = self.types.resolveVar(fresh_var).var_;
    if (maybe_expr_idx) |expr_idx| {
        try self.constraint_expr_by_fn_var.put(fresh_var, expr_idx);
        if (resolved_fresh != fresh_var) {
            try self.constraint_expr_by_fn_var.put(resolved_fresh, expr_idx);
        }
    }

    if (maybe_region) |region| {
        try self.expect_region_by_constraint_fn_var.put(fresh_var, region);
        if (resolved_fresh != fresh_var) {
            try self.expect_region_by_constraint_fn_var.put(resolved_fresh, region);
        }
    }
}

fn findStaticDispatchUseForConstraint(
    self: *Self,
    method_name: Ident.Idx,
    constraint_fn_var: Var,
) Allocator.Error!?StaticDispatchUse {
    const expr_idx = self.constraintExprForFnVar(constraint_fn_var) orelse return null;
    if (self.cir.store.getExpr(expr_idx) == .e_runtime_error) return null;

    switch (self.cir.store.getExpr(expr_idx)) {
        .e_dispatch_call => |dispatch_call| {
            if (!dispatch_call.method_name.eql(method_name)) return null;
        },
        .e_type_dispatch_call => |dispatch_call| {
            if (!dispatch_call.method_name.eql(method_name)) return null;
        },
        else => return null,
    }

    return .{
        .expr_idx = expr_idx,
        .region = self.cir.store.getExprRegion(expr_idx),
    };
}

/// Detect ambiguous static dispatch on a per-INSTANTIATION basis. Every time a
/// generalized scheme carrying a non-`from_literal` static-dispatch constraint
/// is instantiated, `instantiateVarHelp` recorded the freshly created receiver
/// var here. After all solving, a recorded receiver that is still flex/rigid,
/// carries a real (non-`from_literal`, non-`is_eq`) dispatch constraint, and does
/// not resolve into the `pinnable` set can never be pinned by any caller — its
/// dispatch is genuinely ambiguous and would reach the lowering `dispatchTarget`
/// invariant. We report it (`MISSING METHOD`) and mark the offending call
/// expression a runtime error so lowering never reaches that invariant.
///
/// This is the per-instantiation companion to `reportAmbiguousStaticDispatch`:
/// the latter catches direct/def-site dispatches (the receiver appears directly
/// in a checked expression's type), while this catches dispatches hidden inside a
/// polymorphic helper that only become ambiguous at an unpinned call site.
///
/// An uncalled helper is never instantiated, so nothing is recorded for it and it
/// stays clean. A helper instantiated at a concrete type has its recorded receiver
/// resolved (no longer flex/rigid) by the time this runs, so it stays clean too.
fn reportAmbiguousStaticDispatchPerInstantiation(
    self: *Self,
    reported: *std.AutoHashMap(Var, void),
    pinnable: *std.AutoHashMap(Var, void),
    external_pinnable: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!void {
    if (self.instantiation_dispatchers.items.len == 0) return;

    for (self.instantiation_dispatchers.items) |dispatcher| {
        const resolved = self.types.resolveVar(dispatcher.dispatcher_var);
        const constraints_range = switch (resolved.desc.content) {
            .flex => |flex| flex.constraints,
            .rigid => |rigid| rigid.constraints,
            else => continue,
        };
        if (constraints_range.len() == 0) continue;
        if (reported.contains(resolved.var_)) continue;

        // Pick the constraint to report. Instantiated where-clause contracts get
        // priority because they are explicit obligations copied from a signature:
        // once the signature has been instantiated, the current call must either
        // pin the owner or leave it externally pinnable. Literal-only constraints
        // are skipped as before because numeric/string defaulting owns them.
        //
        //  - `from_literal`: an open literal. Checker defaulting owns it (at
        //    finalize and at generalization boundaries); a generalized literal
        //    resolves per instantiation. A receiver carrying ANY `from_literal`
        //    constraint is skipped entirely, matching the def-site sweep —
        //    essential for numeric helpers like `|x| x + y` whose receiver
        //    carries `desugared_binop` (`+`) plus `from_literal`.
        //  - `is_eq`: structural equality. Lowering compares records/tuples/tag
        //    unions/lists structurally with no owner needed, so a flex `is_eq`
        //    receiver placeholder (left at check time by valid code such as
        //    `[1] == [1]` or `Try.Ok(1) == Try.Ok(1)`, whose real value is concrete)
        //    is not a dead end. A genuinely ambiguous equality on a bare flex value
        //    (`poly() == poly()`) is still caught by the def-site sweep, which keys
        //    on an expression whose OWN type is the bare-flex receiver.
        //
        // What remains — and is reported — is a real method dispatch
        // (`method_call`/`desugared_unaryop`, a non-equality `desugared_binop`,
        // or an instantiated `where_clause` contract) that requires a nominal
        // owner the instantiation never supplied. Def-site where clauses remain
        // valid polymorphic contracts; a per-instantiation where-clause receiver
        // has already been copied into a concrete use of that contract, so if it
        // is not pinnable here, no later caller can supply the missing owner.
        const constraints = self.types.sliceStaticDispatchConstraints(constraints_range);
        var first_where_constraint: ?StaticDispatchConstraint = null;
        var first_nonliteral_constraint: ?StaticDispatchConstraint = null;
        var has_literal_constraint = false;
        for (constraints) |c| {
            if (self.types.resolveVar(c.fn_var).desc.content == .err) continue;
            if (isLiteralStaticDispatchOrigin(c.origin)) {
                has_literal_constraint = true;
            } else if (c.fn_name.eql(self.cir.idents.is_eq)) {
                continue;
            } else if (c.origin == .where_clause) {
                if (first_where_constraint == null) first_where_constraint = c;
            } else if (first_nonliteral_constraint == null) {
                first_nonliteral_constraint = c;
            }
        }
        const constraint = first_where_constraint orelse blk: {
            if (has_literal_constraint) continue;
            break :blk first_nonliteral_constraint orelse continue;
        };
        if (has_literal_constraint) continue;
        const is_instantiated_where_clause = constraint.origin == .where_clause;
        const where_dispatch_use = if (is_instantiated_where_clause)
            try self.findStaticDispatchUseForConstraint(constraint.fn_name, constraint.fn_var)
        else
            null;
        if (is_instantiated_where_clause) {
            if (where_dispatch_use == null) continue;
        }

        // If the receiver resolves INTO the pinnable set, some caller — at this
        // call's level or an enclosing one — can pin it, so it is not a dead end.
        // The receiver of a hidden helper dispatch that the call DID pin unifies
        // with a lambda parameter (e.g. `outer`'s `x` in `outer = |x| inner(x)`,
        // pinned at `outer(10)`); the receiver of a genuine hole (`get(none({}))`)
        // is a fresh instantiated var that unifies with nothing concrete, so it is
        // absent from the set and reported. Instantiated where-clause contracts
        // are stricter: lambda parameters inside the already-instantiated call do
        // not count, because that call is exactly what must satisfy the copied
        // contract. Only external function arguments can still pin it later.
        const active_pinnable = if (is_instantiated_where_clause) external_pinnable else pinnable;
        if (active_pinnable.contains(resolved.var_)) continue;

        // Locate the call site that left this receiver undetermined. Instantiated
        // where-clause contracts carry exact dispatch-expression metadata, so
        // they can report directly at that call. Ordinary hidden dispatches need
        // the structural scan: the receiver var is internal to the instantiated
        // callee type, so no expression's own type IS the receiver. Instead it
        // flows into one of a call's ARGUMENTS, whose type structurally CONTAINS
        // it in a DATA position (a tag payload, record field, tuple element, or
        // nominal argument), never inside a function type. That data-position
        // requirement distinguishes a genuine hole from a polymorphic function
        // passed as a value: in `get(none({}))` the receiver is the tag payload
        // of the `FfiOption` argument and is genuinely undetermined; in
        // `Str.inspect(f)` where `f = |x| x + 1`, the receiver is `f`'s parameter,
        // an uncalled function value whose parameter a future call would pin. A
        // higher-order constraint that IS applied, like the `e_higher_order`
        // corpus case, is caught by the def-site sweep instead.
        var primary: ?Region = null;
        var secondary: ?Region = null;
        if (is_instantiated_where_clause) {
            const dispatch_use = where_dispatch_use.?;
            primary = dispatch_use.region;

            if (self.cir.store.getExpr(dispatch_use.expr_idx) != .e_runtime_error) {
                const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
                    .region = dispatch_use.region,
                } });
                self.cir.store.replaceExprWithRuntimeError(dispatch_use.expr_idx, diagnostic_idx);
            }
        } else {
            var raw_node_idx: u32 = 0;
            while (raw_node_idx < self.cir.store.nodes.len()) : (raw_node_idx += 1) {
                const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
                if (!isExprNodeTag(self.cir.store.nodes.get(node_idx).tag)) continue;

                const expr_idx: CIR.Expr.Idx = @enumFromInt(raw_node_idx);
                if (self.cir.store.getExpr(expr_idx) == .e_runtime_error) continue;
                const call = switch (self.cir.store.getExpr(expr_idx)) {
                    .e_call => |c| c,
                    else => continue,
                };

                var arg_region: ?Region = null;
                for (self.cir.store.sliceExpr(call.args)) |arg_idx| {
                    self.var_set.clearRetainingCapacity();
                    try self.collectDataReachableVars(ModuleEnv.varFrom(arg_idx), &self.var_set);
                    if (self.var_set.contains(resolved.var_)) {
                        arg_region = self.cir.store.getExprRegion(arg_idx);
                        break;
                    }
                }
                if (arg_region == null) continue;

                if (primary == null) {
                    primary = self.cir.store.getExprRegion(expr_idx);
                    secondary = arg_region;
                }

                const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
                    .region = self.cir.store.getExprRegion(expr_idx),
                } });
                self.cir.store.replaceExprWithRuntimeError(expr_idx, diagnostic_idx);
            }
        }

        // No call passes the receiver in the relevant position. For ordinary
        // hidden dispatch this means it is a polymorphic function value's own
        // parameter (or otherwise not a real dispatch dead end). For a copied
        // where-clause contract, report at the instantiation region because the
        // unresolved obligation itself is the error.
        if (primary == null) {
            if (!is_instantiated_where_clause) continue;
            const dispatch_use = where_dispatch_use.?;
            primary = dispatch_use.region;

            if (self.cir.store.getExpr(dispatch_use.expr_idx) != .e_runtime_error) {
                const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
                    .region = dispatch_use.region,
                } });
                self.cir.store.replaceExprWithRuntimeError(dispatch_use.expr_idx, diagnostic_idx);
            }
        }

        try reported.put(resolved.var_, {});

        const primary_region = primary.?;
        const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, resolved.var_);
        const is_binop = constraint.origin == .desugared_binop;

        _ = try self.problems.appendProblem(self.gpa, .{ .static_dispatch = .{ .unresolved_dispatcher = .{
            .region = primary_region,
            .secondary_region = secondary,
            .dispatcher_snapshot = snapshot,
            .method_name = constraint.fn_name,
            .is_binop = is_binop,
            .binop_negated = constraint.origin.binopNegated(),
        } } });
    }
}

/// Validate the static-dispatch contracts copied out of generalized schemes
/// after an instantiation's caller has had a chance to pin them. Ordinary
/// unification records deferred checks while a constrained flex unifies with a
/// concrete type, but instantiating a polymorphic where-clause starts with a
/// fresh constrained flex that may only resolve after later call-site evidence.
/// Re-checking the recorded range here makes that concrete owner validation
/// explicit and leaves unresolved cases to the ambiguity sweep below.
fn checkInstantiatedStaticDispatchConstraints(
    self: *Self,
    env: *Env,
    is_numeric_default_pass: bool,
) std.mem.Allocator.Error!void {
    if (self.instantiation_dispatchers.items.len == 0) return;

    const deferred_top = env.deferred_static_dispatch_constraints.items.items.len;
    for (self.instantiation_dispatchers.items) |dispatcher| {
        if (dispatcher.constraints.len() == 0) continue;
        var has_where_constraint = false;
        for (self.types.sliceStaticDispatchConstraints(dispatcher.constraints)) |constraint| {
            if (constraint.origin == .where_clause) {
                has_where_constraint = true;
                break;
            }
        }
        if (!has_where_constraint) continue;

        _ = try env.deferred_static_dispatch_constraints.append(self.gpa, .{
            .var_ = dispatcher.dispatcher_var,
            .constraints = dispatcher.constraints,
        });
    }
    if (env.deferred_static_dispatch_constraints.items.items.len == deferred_top) return;

    try self.checkStaticDispatchConstraints(env, is_numeric_default_pass);
    try self.checkAllConstraints(env);
}

/// Detect ambiguous static dispatch: a static-dispatch-constrained type variable
/// that is reachable through no function ARGUMENT position and is no lambda
/// parameter, so no instantiation can ever pin it down. Such a variable reaches
/// monomorphization as an unresolved flex/rigid dispatcher, which the lowering
/// `dispatchTarget` invariant forbids. We catch it here, emit a `MISSING METHOD`
/// diagnostic, and mark the offending expression a runtime error so lowering
/// never reaches that invariant.
///
/// This runs at the very end of checking, once numeric defaulting, generalization,
/// recursive-cycle resolution, constraint solving, and the poison passes have all
/// settled — the same settled-state invariant `reportPolymorphicTopLevelValues`
/// relies on.
fn reportAmbiguousStaticDispatch(
    self: *Self,
    reported: *std.AutoHashMap(Var, void),
    pinnable: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!void {
    // `pinnable` (built by `collectPinnableVars`) holds every resolved var id that
    // some instantiation can pin: every var reachable through an ARGUMENT position
    // of a top-level def's generalized type, and every var reachable from a lambda
    // parameter pattern (local/nested lambdas included).
    //
    // Sweep every expression. Flag any whose own type var is a flex/rigid var with
    // a non-`from_literal` static-dispatch constraint and whose resolved id is not
    // pinnable. Dedup by resolved var id (using the set shared with the
    // per-instantiation sweep) so a value flowing through multiple expressions — or
    // already reported per-instantiation — is reported once, preferring the first
    // (innermost node-order) occurrence, which lands the underline on the dispatch
    // use.
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < self.cir.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (!isExprNodeTag(self.cir.store.nodes.get(node_idx).tag)) continue;

        const expr_idx: CIR.Expr.Idx = @enumFromInt(raw_node_idx);
        if (self.cir.store.getExpr(expr_idx) == .e_runtime_error) continue;

        const resolved = self.types.resolveVar(ModuleEnv.varFrom(expr_idx));
        const constraints_range = switch (resolved.desc.content) {
            .flex => |flex| flex.constraints,
            .rigid => |rigid| rigid.constraints,
            else => continue,
        };
        if (constraints_range.len() == 0) continue;
        if (pinnable.contains(resolved.var_)) continue;
        if (reported.contains(resolved.var_)) continue;

        // Decide whether to flag based on the constraint origins.
        //
        // - `from_literal`: the var is an open literal. Defaulting resolves it
        //   (first satisfier of its constraints; `Dec` when otherwise
        //   unconstrained) and reports any unsatisfiable conversion separately, so
        //   it is never flagged here — even when it also carries other constraints
        //   (e.g. a literal operand of `+` also has a `desugared_binop`).
        // - `where_clause`: the dispatch is part of an explicit polymorphic
        //   signature (`f : a -> a where [a.method : ...]`). That is a declared
        //   contract pinned when callers instantiate the signature, so it is
        //   legitimate and never flagged.
        //
        // Only the remaining un-annotated dispatch origins (`method_call`,
        // `desugared_binop`, `desugared_unaryop`) can produce an unpinnable flex
        // dispatcher at monomorphization. Flag using the first such constraint.
        const constraints = self.types.sliceStaticDispatchConstraints(constraints_range);
        var has_excluded_origin = false;
        var first_constraint: ?StaticDispatchConstraint = null;
        for (constraints) |c| {
            switch (c.origin) {
                .from_literal, .where_clause => {
                    has_excluded_origin = true;
                },
                .method_call, .desugared_binop, .desugared_unaryop => {
                    if (first_constraint == null) first_constraint = c;
                },
            }
        }
        if (has_excluded_origin) continue;
        const constraint = first_constraint orelse continue;

        try reported.put(resolved.var_, {});

        const region = self.cir.store.getExprRegion(expr_idx);
        const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, resolved.var_);

        const is_binop = constraint.origin == .desugared_binop;

        _ = try self.problems.appendProblem(self.gpa, .{ .static_dispatch = .{ .unresolved_dispatcher = .{
            .region = region,
            .secondary_region = null,
            .dispatcher_snapshot = snapshot,
            .method_name = constraint.fn_name,
            .is_binop = is_binop,
            .binop_negated = constraint.origin.binopNegated(),
        } } });

        // Mark the expression a runtime error so lowering skips it and never
        // reaches the `dispatchTarget` invariant.
        const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
            .region = region,
        } });
        self.cir.store.replaceExprWithRuntimeError(expr_idx, diagnostic_idx);
    }
}

/// Collect, into `out`, every resolved var id reachable through a function
/// ARGUMENT position of `var_`. Argument structure is recursed fully (records,
/// tuples, tag payloads, nested function args AND rets). Return positions of the
/// outer function are NOT collected (a return-only var is not parameter-pinnable),
/// but once inside an argument every nested position counts.
fn collectArgPositionVars(
    self: *Self,
    var_: Var,
    out: *std.AutoHashMap(Var, void),
    spine_visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(var_);
    // Guard the function-ret / alias spine against cycles. Cyclic spines are
    // currently pre-broken (infinite types are poisoned to .err before this
    // pass, recursive aliases are rejected during generation), but the guard
    // keeps this robust if that ever changes.
    if (spine_visited.contains(resolved.var_)) return;
    try spine_visited.put(resolved.var_, {});
    switch (resolved.desc.content) {
        .alias => |alias| try self.collectArgPositionVars(self.types.getAliasBackingVar(alias), out, spine_visited),
        .structure => |flat| switch (flat) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                for (self.types.sliceVars(func.args)) |arg| {
                    try self.collectReachableVars(arg, out);
                }
                // Recurse into the return type: a curried function returns more
                // functions whose own arguments are still parameter-pinnable.
                try self.collectArgPositionVars(func.ret, out, spine_visited);
            },
            else => {},
        },
        else => {},
    }
}

/// Collect, into `out`, `var_`'s resolved id and every resolved var id
/// structurally reachable from it (tuples, records, tag payloads, function args
/// and rets). Used to mark whole argument-position type structures as pinnable.
fn collectReachableVars(self: *Self, var_: Var, out: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(var_);
    if (out.contains(resolved.var_)) return;
    try out.put(resolved.var_, {});

    switch (resolved.desc.content) {
        .err => {},
        // A constrained type variable's `where` constraints relate it to other
        // type variables through the constraint method's signature. For example
        // `c.is_eq : c, d -> f` makes `d` and `f` reachable from `c`; following
        // `f.not : f -> e` then reaches `e`. When `c` is parameter-pinnable, every
        // variable reachable through its constraint signatures is pinnable too,
        // because instantiating the parameter instantiates the whole constraint
        // chain. (This is the spec's "recurse fully into arg structure".)
        .flex, .rigid => {
            const constraints_range = switch (resolved.desc.content) {
                .flex => |flex| flex.constraints,
                .rigid => |rigid| rigid.constraints,
                else => unreachable,
            };
            const constraints = self.types.sliceStaticDispatchConstraints(constraints_range);
            for (constraints) |constraint| {
                try self.collectReachableVars(constraint.fn_var, out);
            }
        },
        .alias => |alias| try self.collectReachableVars(self.types.getAliasBackingVar(alias), out),
        .structure => |flat| switch (flat) {
            .tuple => |tuple| {
                for (self.types.sliceVars(tuple.elems)) |elem| {
                    try self.collectReachableVars(elem, out);
                }
            },
            .nominal_type => |nominal| {
                for (self.types.sliceNominalArgs(nominal)) |arg| {
                    try self.collectReachableVars(arg, out);
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                for (self.types.sliceVars(func.args)) |arg| {
                    try self.collectReachableVars(arg, out);
                }
                try self.collectReachableVars(func.ret, out);
            },
            .record => |record| {
                const fields = self.types.getRecordFieldsSlice(record.fields);
                for (fields.items(.var_)) |field_var| {
                    try self.collectReachableVars(field_var, out);
                }
                try self.collectReachableVars(record.ext, out);
            },
            .record_unbound => |fields_range| {
                const fields = self.types.getRecordFieldsSlice(fields_range);
                for (fields.items(.var_)) |field_var| {
                    try self.collectReachableVars(field_var, out);
                }
            },
            .tag_union => |tag_union| {
                const tags = self.types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |tag_args| {
                    for (self.types.sliceVars(tag_args)) |arg| {
                        try self.collectReachableVars(arg, out);
                    }
                }
                try self.collectReachableVars(tag_union.ext, out);
            },
            .empty_record, .empty_tag_union => {},
        },
    }
}

/// Like `collectReachableVars`, but does NOT descend into FUNCTION types at all —
/// only into data positions (tag payloads, record fields, tuple elements, nominal
/// args). A var reachable this way is part of the actual data flowing through
/// `var_`, not a parameter or result of a function value carried by `var_`. The
/// per-instantiation ambiguity sweep uses this to distinguish a genuinely
/// undetermined dispatch receiver passed as data (report) from a parameter of an
/// uncalled polymorphic function value (do not report) — e.g. in `Str.inspect(f)`
/// where `f = |x| x + 1`, the receiver is `f`'s parameter and the `+`-preserving
/// return both live inside the function type, so neither is reached here.
fn collectDataReachableVars(self: *Self, var_: Var, out: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!void {
    const resolved = self.types.resolveVar(var_);
    if (out.contains(resolved.var_)) return;
    try out.put(resolved.var_, {});

    switch (resolved.desc.content) {
        .alias => |alias| try self.collectDataReachableVars(self.types.getAliasBackingVar(alias), out),
        .structure => |flat| switch (flat) {
            .tuple => |tuple| {
                for (self.types.sliceVars(tuple.elems)) |elem| {
                    try self.collectDataReachableVars(elem, out);
                }
            },
            .nominal_type => |nominal| {
                for (self.types.sliceNominalArgs(nominal)) |arg| {
                    try self.collectDataReachableVars(arg, out);
                }
            },
            // A function value is not data: its parameters and result are
            // determined by a future call, not by the value passed here, so we do
            // not descend into it at all.
            .fn_pure, .fn_effectful, .fn_unbound => {},
            .record => |record| {
                const fields = self.types.getRecordFieldsSlice(record.fields);
                for (fields.items(.var_)) |field_var| {
                    try self.collectDataReachableVars(field_var, out);
                }
                try self.collectDataReachableVars(record.ext, out);
            },
            .record_unbound => |fields_range| {
                const fields = self.types.getRecordFieldsSlice(fields_range);
                for (fields.items(.var_)) |field_var| {
                    try self.collectDataReachableVars(field_var, out);
                }
            },
            .tag_union => |tag_union| {
                const tags = self.types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |tag_args| {
                    for (self.types.sliceVars(tag_args)) |arg| {
                        try self.collectDataReachableVars(arg, out);
                    }
                }
                try self.collectDataReachableVars(tag_union.ext, out);
            },
            .empty_record, .empty_tag_union => {},
        },
        else => {},
    }
}

fn reportPolymorphicTopLevelValues(self: *Self) std.mem.Allocator.Error!void {
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        const def = self.cir.store.getDef(def_idx);
        const def_var = ModuleEnv.varFrom(def_idx);

        if (self.erroneous_value_patterns.contains(def.pattern)) continue;
        // A function — including a zero-arg thunk (`|| …`) — may generalize to a
        // polymorphic type: each call site monomorphizes its return. Only a
        // non-function top-level *value* must be concrete, since it is computed
        // once and needs a single representation. (A thunk whose return is left
        // open is therefore allowed; if it is an app entry the platform's
        // `requires` type unifies it to a concrete type anyway.)
        if (self.varIsFunctionType(def_var)) continue;

        self.var_set.clearRetainingCapacity();
        if (!try self.varHasUnresolvedStaticDispatchConstraints(def_var, &self.var_set)) continue;

        try self.reportPolymorphicValueProblem(def_var, ModuleEnv.varFrom(def.pattern), self.getPatternIdent(def.pattern));
    }
}

fn reportPolymorphicConstrainedExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    const expr_var = ModuleEnv.varFrom(expr_idx);
    if (self.varIsFunctionType(expr_var)) return;

    self.var_set.clearRetainingCapacity();
    if (!try self.varHasUnresolvedStaticDispatchConstraints(expr_var, &self.var_set)) return;

    try self.reportPolymorphicValueProblem(expr_var, expr_var, null);
}

fn reportPolymorphicValueProblem(
    self: *Self,
    snapshot_var: Var,
    region_var: Var,
    def_name: ?Ident.Idx,
) std.mem.Allocator.Error!void {
    const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, snapshot_var);
    _ = try self.problems.appendProblem(self.gpa, .{ .polymorphic_value = .{
        .var_ = region_var,
        .snapshot = snapshot,
        .def_name = def_name,
    } });
}

fn checkExpectBody(
    self: *Self,
    body: CIR.Expr.Idx,
    env: *Env,
    expected: Expected,
    expect_region: Region,
) std.mem.Allocator.Error!bool {
    const saved_expect_region = self.current_expect_region;
    self.current_expect_region = expect_region;
    defer self.current_expect_region = saved_expect_region;

    return try self.checkExpr(body, env, expected);
}

fn varIsFunctionType(self: *Self, var_: Var) bool {
    var current = var_;
    while (true) {
        const resolved = self.types.resolveVar(current);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current = self.types.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| return switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => true,
                else => false,
            },
            .err, .flex, .rigid => return false,
        }
    }
}

fn varIsEffectfulFunction(self: *Self, var_: Var) bool {
    var current = var_;
    while (true) {
        const resolved = self.types.resolveVar(current);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current = self.types.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| return switch (flat) {
                .fn_effectful => true,
                .fn_pure, .fn_unbound => false,
                else => false,
            },
            .err, .flex, .rigid => return false,
        }
    }
}

fn varHasUnresolvedContent(
    self: *Self,
    var_: Var,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    if (visited.contains(resolved.var_)) return false;
    try visited.put(resolved.var_, {});

    return switch (resolved.desc.content) {
        .flex, .rigid => true,
        .err => false,
        .alias => |alias| try self.varHasUnresolvedContent(self.types.getAliasBackingVar(alias), visited),
        .structure => |flat_type| try self.flatTypeHasUnresolvedContent(flat_type, visited),
    };
}

fn flatTypeHasUnresolvedContent(
    self: *Self,
    flat_type: FlatType,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    return switch (flat_type) {
        .tuple => |tuple| try self.varsHaveUnresolvedContent(self.types.sliceVars(tuple.elems), visited),
        .nominal_type => |nominal| try self.varsHaveUnresolvedContent(self.types.sliceNominalArgs(nominal), visited),
        .fn_pure, .fn_effectful, .fn_unbound => false,
        .record => |record| blk: {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            if (try self.varsHaveUnresolvedContent(fields.items(.var_), visited)) break :blk true;
            break :blk try self.varHasUnresolvedContent(record.ext, visited);
        },
        .record_unbound => |fields_range| blk: {
            const fields = self.types.getRecordFieldsSlice(fields_range);
            break :blk try self.varsHaveUnresolvedContent(fields.items(.var_), visited);
        },
        .tag_union => |tag_union| blk: {
            const tags = self.types.getTagsSlice(tag_union.tags);
            for (tags.items(.args)) |args| {
                if (try self.varsHaveUnresolvedContent(self.types.sliceVars(args), visited)) break :blk true;
            }
            break :blk try self.varHasUnresolvedContent(tag_union.ext, visited);
        },
        .empty_record, .empty_tag_union => false,
    };
}

fn varsHaveUnresolvedContent(
    self: *Self,
    vars: []const Var,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    for (vars) |var_| {
        if (try self.varHasUnresolvedContent(var_, visited)) return true;
    }
    return false;
}

fn varHasUnresolvedStaticDispatchConstraints(
    self: *Self,
    var_: Var,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    if (visited.contains(resolved.var_)) return false;
    try visited.put(resolved.var_, {});

    return switch (resolved.desc.content) {
        .flex => |flex| flex.constraints.len() > 0,
        .rigid => |rigid| rigid.constraints.len() > 0,
        .err => false,
        .alias => |alias| try self.varHasUnresolvedStaticDispatchConstraints(self.types.getAliasBackingVar(alias), visited),
        .structure => |flat_type| try self.flatTypeHasUnresolvedStaticDispatchConstraints(flat_type, visited),
    };
}

fn flatTypeHasUnresolvedStaticDispatchConstraints(
    self: *Self,
    flat_type: FlatType,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    return switch (flat_type) {
        .tuple => |tuple| try self.varsHaveUnresolvedStaticDispatchConstraints(self.types.sliceVars(tuple.elems), visited),
        .nominal_type => |nominal| try self.varsHaveUnresolvedStaticDispatchConstraints(self.types.sliceNominalArgs(nominal), visited),
        .fn_pure, .fn_effectful, .fn_unbound => false,
        .record => |record| blk: {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            if (try self.varsHaveUnresolvedStaticDispatchConstraints(fields.items(.var_), visited)) break :blk true;
            break :blk try self.varHasUnresolvedStaticDispatchConstraints(record.ext, visited);
        },
        .record_unbound => |fields_range| blk: {
            const fields = self.types.getRecordFieldsSlice(fields_range);
            break :blk try self.varsHaveUnresolvedStaticDispatchConstraints(fields.items(.var_), visited);
        },
        .tag_union => |tag_union| blk: {
            const tags = self.types.getTagsSlice(tag_union.tags);
            for (tags.items(.args)) |args| {
                if (try self.varsHaveUnresolvedStaticDispatchConstraints(self.types.sliceVars(args), visited)) break :blk true;
            }
            break :blk try self.varHasUnresolvedStaticDispatchConstraints(tag_union.ext, visited);
        },
        .empty_record, .empty_tag_union => false,
    };
}

fn varsHaveUnresolvedStaticDispatchConstraints(
    self: *Self,
    vars: []const Var,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    for (vars) |var_| {
        if (try self.varHasUnresolvedStaticDispatchConstraints(var_, visited)) return true;
    }
    return false;
}

/// Validate a platform module's hosted section against the hosted functions
/// its imported type modules declare: every hosted function must appear
/// exactly once, every entry must name a real hosted function, linker symbols
/// must be unique across the hosted and provides sections, and the fixed
/// runtime symbols plus the internal roc__ namespace are reserved.
///
/// Applications never pay for this: it only runs for platform modules.
fn checkPlatformHostedSection(self: *Self) std.mem.Allocator.Error!void {
    if (self.cir.module_kind != .platform) return;

    const section = self.cir.hosted_entries.items.items;

    // Qualified names of every hosted function declared by imported modules,
    // built the same way the hosted dispatch catalog builds its keys:
    // "Module.func" with a trailing `!` stripped.
    var declared = std.StringHashMap(void).init(self.gpa);
    defer {
        var key_it = declared.keyIterator();
        while (key_it.next()) |key| self.gpa.free(key.*);
        declared.deinit();
    }
    // Walk owner modules (direct imports plus their transitive public
    // dependencies), since hosted functions can live in modules the platform
    // root never imports directly. Within each module, walk global value defs,
    // not all_defs: hosted functions declared as associated items of (possibly
    // nested) type modules are hoisted into global_value_defs only, and the
    // hosted dispatch catalog scans the same list.
    for (self.owner_modules) |imported_env| {
        const all_defs = imported_env.store.sliceDefs(imported_env.global_value_defs);
        for (all_defs) |def_idx| {
            const def = imported_env.store.getDef(def_idx);
            const expr = imported_env.store.getExpr(def.expr);
            if (expr != .e_hosted_lambda) continue;

            var module_name = imported_env.module_name;
            if (Ident.textEndsWith(module_name, ".roc")) {
                module_name = module_name[0 .. module_name.len - 4];
            }
            var func_name = imported_env.getIdent(expr.e_hosted_lambda.symbol_name);
            if (Ident.textEndsWith(func_name, "!")) {
                func_name = func_name[0 .. func_name.len - 1];
            }
            const key = try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ module_name, func_name });
            const gop = try declared.getOrPut(key);
            if (gop.found_existing) self.gpa.free(key);
        }
    }

    // Walk the section: resolve each entry, detect duplicate functions and
    // duplicate/reserved symbols (provides symbols share the namespace).
    var mapped = std.StringHashMap(void).init(self.gpa);
    defer {
        var key_it = mapped.keyIterator();
        while (key_it.next()) |key| self.gpa.free(key.*);
        mapped.deinit();
    }
    var symbols = std.StringHashMap(void).init(self.gpa);
    defer symbols.deinit();
    for (self.cir.provides_entries.items.items) |provides_entry| {
        const symbol_text = self.cir.getString(provides_entry.ffi_symbol);
        try self.checkHostSymbol(symbol_text, &symbols);
    }

    for (section) |entry| {
        var func_name = self.cir.getIdent(entry.func_ident);
        if (Ident.textEndsWith(func_name, "!")) {
            func_name = func_name[0 .. func_name.len - 1];
        }
        const key = if (entry.module_ident) |module_ident|
            try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ self.cir.getIdent(module_ident), func_name })
        else
            try self.gpa.dupe(u8, func_name);

        if (!declared.contains(key)) {
            const name_idx = try self.problems.putExtraString(key);
            _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
                .name = name_idx,
                .reason = .unknown_function,
            } });
        }

        const gop = try mapped.getOrPut(key);
        if (gop.found_existing) {
            const name_idx = try self.problems.putExtraString(key);
            _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
                .name = name_idx,
                .reason = .duplicate_function,
            } });
            self.gpa.free(key);
        }

        const symbol_text = self.cir.getString(entry.symbol);
        try self.checkHostSymbol(symbol_text, &symbols);
    }

    // Every declared hosted function needs exactly one section entry.
    var declared_it = declared.keyIterator();
    while (declared_it.next()) |key| {
        if (mapped.contains(key.*)) continue;
        const name_idx = try self.problems.putExtraString(key.*);
        _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
            .name = name_idx,
            .reason = .function_not_in_section,
        } });
    }
}

/// The fixed runtime symbols every host defines; platform headers may not
/// reuse them for provides or hosted entries.
const reserved_host_symbols = [_][]const u8{
    "roc_alloc",
    "roc_dealloc",
    "roc_realloc",
    "roc_dbg",
    "roc_expect_failed",
    "roc_crashed",
};

fn checkHostSymbol(
    self: *Self,
    symbol_text: []const u8,
    symbols: *std.StringHashMap(void),
) std.mem.Allocator.Error!void {
    for (reserved_host_symbols) |reserved| {
        if (Ident.textEql(symbol_text, reserved)) {
            const name_idx = try self.problems.putExtraString(symbol_text);
            _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
                .name = name_idx,
                .reason = .reserved_symbol,
            } });
            return;
        }
    }
    if (Ident.textStartsWith(symbol_text, "roc__")) {
        const name_idx = try self.problems.putExtraString(symbol_text);
        _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
            .name = name_idx,
            .reason = .reserved_prefix,
        } });
        return;
    }
    const gop = try symbols.getOrPut(symbol_text);
    if (gop.found_existing) {
        const name_idx = try self.problems.putExtraString(symbol_text);
        _ = try self.problems.appendProblem(self.gpa, .{ .platform_hosted_section = .{
            .name = name_idx,
            .reason = .duplicate_symbol,
        } });
    }
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
            std.debug.assert(alias_lhs.name.eql(alias_lhs.relative_name));
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
            // checked artifact co-finalization.
            _ = try self.unify(stmt_var, alias_rhs_var, env);
        }

        // Then, generate the type for the actual required type
        //   { [Model : model] for main : { init : model, ... } }
        //                                ^^^^^^^^^^^^^^^^^^^^^^
        try self.generateAnnoTypeInPlace(required_type.type_anno, env, .annotation);
    }
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
    for (0..self.cir.builtin_statements.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(self.cir.builtin_statements, stmt_offset);
        // If the statement is a type declaration, then generate the it's type
        // The resulting generalized type is saved at the type var slot at `stmt_idx`
        try self.generateStmtTypeDeclType(stmt_idx, &env);
    }

    // Set the rank to be outermost
    try env.var_pool.pushRank();
    std.debug.assert(env.rank() == .outermost);

    // Check the expr
    _ = try self.checkExpr(expr_idx, &env, Expected.none());

    // Check any accumulated constraints
    try self.checkAllConstraints(&env);
    try self.resolveNumericLiteralsFromContext(&env);
    try self.finalizeLiteralDefaults(&env);

    // After finalizing numeric defaults, resolve any remaining deferred
    // static dispatch constraints (e.g., Dec.not for !3).
    if (env.deferred_static_dispatch_constraints.items.items.len > 0) {
        try self.checkStaticDispatchConstraints(&env, true);
    }

    // Check if the expression's type has incompatible constraints (e.g., !3)
    const expr_var = ModuleEnv.varFrom(expr_idx);
    try self.checkFlexVarConstraintCompatibility(expr_var, &env, true);
    try self.reportPolymorphicConstrainedExpr(expr_idx);

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
    for (0..self.cir.builtin_statements.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(self.cir.builtin_statements, stmt_offset);
        try self.generateStmtTypeDeclType(stmt_idx, &env);
    }

    // Initialize top_level_ptrns with any defs from local type declarations
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
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
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        try self.checkDef(def_idx, &env);

        // Ensure that after processing a def, checkDef correctly restores the
        // rank to outermost for the next level of processing
        std.debug.assert(env.rank() == .outermost);
    }

    // Check the expr
    _ = try self.checkExpr(expr_idx, &env, Expected.none());

    // Check any accumulated constraints
    try self.checkAllConstraints(&env);
    try self.resolveNumericLiteralsFromContext(&env);
    try self.finalizeLiteralDefaults(&env);

    // After finalizing literal defaults, resolve any remaining deferred static
    // dispatch constraints: committing a default can generate deferred
    // method_call constraints (e.g. Dec.to_str returns Str). Without this step,
    // the return type of methods on numerics stays an unconstrained flex var,
    // causing incorrect .zst layouts.
    if (env.deferred_static_dispatch_constraints.items.items.len > 0) {
        try self.checkStaticDispatchConstraints(&env, true);
        try self.checkAllConstraints(&env);
    }

    // After solving all deferred constraints, check for infinite types
    for (0..self.cir.all_defs.span.len) |def_offset| {
        const def_idx = self.cir.store.defAt(self.cir.all_defs, def_offset);
        try self.checkForInfiniteType(CIR.Def.Idx, def_idx);
    }

    // Check the result expression itself, matching checkExprRepl: its type may
    // have incompatible constraints (e.g. !3) or be infinite/anonymously
    // recursive, neither of which is covered by the per-def checks above.
    const expr_var = ModuleEnv.varFrom(expr_idx);
    try self.checkFlexVarConstraintCompatibility(expr_var, &env, true);
    try self.checkForInfiniteType(CIR.Expr.Idx, expr_idx);

    try self.reportPolymorphicConstrainedExpr(expr_idx);
    try self.poisonErroneousValueUses();
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

    const def_pattern_ctx: PatternCtx = if (self.patternNeedsExhaustiveness(def.pattern)) .match_branch else .bound;

    // Check the pattern
    try self.checkPattern(def.pattern, def_pattern_ctx, env);

    // Extract function name from the pattern (for better error messages)
    const saved_func_name = self.enclosing_func_name;
    self.enclosing_func_name = def_name;
    defer self.enclosing_func_name = saved_func_name;

    // Check the annotation, if it exists
    const expectation = blk: {
        if (def.annotation) |annotation_idx| {
            break :blk Expected.fromAnnotation(annotation_idx);
        } else {
            break :blk Expected.none();
        }
    };

    self.empirical_exhaustiveness_depth += 1;
    defer self.empirical_exhaustiveness_depth -= 1;

    // Infer types for the body, checking against the instantiated annotation
    self.checking_binding_rhs = true;
    const def_does_fx = try self.checkExpr(def.expr, env, expectation);
    if (def_does_fx) {
        _ = try self.problems.appendProblem(self.gpa, .{ .effectful_top_level = .{
            .region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(def.expr)),
        } });
        try self.unifyWith(expr_var, .err, env);
    }
    if (def.annotation == null and self.exprAlwaysCrashes(def.expr)) {
        try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
    }
    if (def.annotation == null and self.erroneous_value_exprs.contains(def.expr)) {
        try self.erroneous_value_patterns.put(self.gpa, def.pattern, {});
    }
    try self.closeAbsentConstructedPayloadVars(def.expr, expr_var);

    if (self.defer_generalize) {
        // defer_generalize is only set when a cycle root has been identified.
        std.debug.assert(self.cycle_root_def != null);

        // Defer unifications until after generalization.
        // If we unify now, def_var(R1) with expr_var(R2) lowers expr_var
        // to R1 in the type store, and generalize at R2 would skip it.
        _ = try self.deferred_def_unifications.append(self.gpa, .{
            .def_var = def_var,
            .ptrn_var = ptrn_var,
            .expr_var = expr_var,
        });
    } else {
        // Unify the ptrn and the expr
        const ptrn_result = try self.unify(ptrn_var, expr_var, env);

        // Unify the def and ptrn
        _ = try self.unify(def_var, ptrn_var, env);

        if (ptrn_result.isOk()) {
            const def_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(def.pattern));
            try self.checkDestructureExhaustiveness(def.pattern, def.expr, expr_var, env, def_region);
        }
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

fn aliasOriginModule(self: *const Self) Ident.Idx {
    return self.builtin_ctx.module_name;
}

fn predeclareAliasDecl(
    self: *Self,
    decl_var: Var,
    alias: std.meta.fieldInfo(CIR.Statement, .s_alias_decl).type,
    env: *Env,
) std.mem.Allocator.Error!void {
    const header = self.cir.store.getTypeHeader(alias.header);
    const header_args = self.cir.store.sliceTypeAnnos(header.args);
    const header_vars = try self.generateHeaderVars(header_args, env);
    const backing_var: Var = ModuleEnv.varFrom(alias.anno);

    try self.unifyWithTargetRank(
        decl_var,
        try self.types.mkAliasWithSourceDeclAndBuiltinOrigin(
            .{ .ident_idx = header.relative_name },
            backing_var,
            header_vars,
            self.aliasOriginModule(),
            @intFromEnum(decl_var),
            self.cir.module_role == .builtin,
        ),
        env,
    );
}

fn predeclareNominalDecl(
    self: *Self,
    decl_var: Var,
    nominal: std.meta.fieldInfo(CIR.Statement, .s_nominal_decl).type,
    env: *Env,
) std.mem.Allocator.Error!void {
    const header = self.cir.store.getTypeHeader(nominal.header);
    const header_args = self.cir.store.sliceTypeAnnos(header.args);
    const header_vars = try self.generateHeaderVars(header_args, env);
    const backing_var: Var = ModuleEnv.varFrom(nominal.anno);

    try self.unifyWithTargetRank(
        decl_var,
        try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
            .{ .ident_idx = header.relative_name },
            backing_var,
            header_vars,
            self.builtin_ctx.module_name,
            @intFromEnum(decl_var),
            nominal.is_opaque,
            self.cir.module_role == .builtin,
        ),
        env,
    );
}

fn predeclaredAliasArgs(self: *const Self, decl_var: Var) ?[]Var {
    const resolved = self.types.resolveVar(decl_var).desc.content;
    if (resolved != .alias) return null;
    return self.types.sliceAliasArgs(resolved.alias);
}

fn predeclaredNominalArgs(self: *const Self, decl_var: Var) ?[]Var {
    const resolved = self.types.resolveVar(decl_var).desc.content;
    if (resolved != .structure or resolved.structure != .nominal_type) return null;
    return self.types.sliceNominalArgs(resolved.structure.nominal_type);
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
    const predeclared_header_vars = self.predeclaredAliasArgs(decl_var);
    const header_vars = if (predeclared_header_vars) |vars| vars else try self.generateHeaderVars(header_args, env);
    if (predeclared_header_vars == null) {
        try self.unifyWithTargetRank(
            decl_var,
            try self.types.mkAliasWithSourceDeclAndBuiltinOrigin(
                .{ .ident_idx = header.relative_name },
                ModuleEnv.varFrom(alias.anno),
                header_vars,
                self.aliasOriginModule(),
                @intFromEnum(decl_idx),
                self.cir.module_role == .builtin,
            ),
            env,
        );
    }

    self.type_decl_rigid_vars.clearRetainingCapacity();
    defer self.type_decl_rigid_vars.clearRetainingCapacity();
    for (header_args, header_vars) |header_arg_idx, header_var| {
        const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
        if (header_arg == .rigid_var) {
            try self.type_decl_rigid_vars.put(self.gpa, header_arg.rigid_var.name, header_var);
        }
    }

    // Now we have a built of list of rigid variables for the decl lhs (header).
    // With this in hand, we can now generate the type for the lhs (body).
    self.seen_annos.clearRetainingCapacity();
    const backing_var: Var = ModuleEnv.varFrom(alias.anno);
    try self.generateAnnoTypeInPlace(alias.anno, env, .{ .type_decl = .{
        .idx = decl_idx,
        .name = header.relative_name,
        .type_ = .alias,
        .backing_var = backing_var,
        .is_opaque = false,
        .num_args = @intCast(header_args.len),
    } });

    if (!try self.validateAliasRows(backing_var, env, self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(alias.anno)))) {
        try self.unifyWithTargetRank(decl_var, .err, env);
        return;
    }
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
    const predeclared_header_vars = self.predeclaredNominalArgs(decl_var);
    const header_vars = if (predeclared_header_vars) |vars| vars else try self.generateHeaderVars(header_args, env);
    if (predeclared_header_vars == null) {
        try self.unifyWithTargetRank(
            decl_var,
            try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
                .{ .ident_idx = header.relative_name },
                ModuleEnv.varFrom(nominal.anno),
                header_vars,
                self.builtin_ctx.module_name,
                @intFromEnum(decl_idx),
                nominal.is_opaque,
                self.cir.module_role == .builtin,
            ),
            env,
        );
    }

    self.type_decl_rigid_vars.clearRetainingCapacity();
    defer self.type_decl_rigid_vars.clearRetainingCapacity();
    for (header_args, header_vars) |header_arg_idx, header_var| {
        const header_arg = self.cir.store.getTypeAnno(header_arg_idx);
        if (header_arg == .rigid_var) {
            try self.type_decl_rigid_vars.put(self.gpa, header_arg.rigid_var.name, header_var);
        }
    }

    // Now we have a built of list of rigid variables for the decl lhs (header).
    // With this in hand, we can now generate the type for the lhs (body).
    self.seen_annos.clearRetainingCapacity();
    const backing_var: Var = ModuleEnv.varFrom(nominal.anno);
    try self.generateAnnoTypeInPlace(nominal.anno, env, .{ .type_decl = .{
        .idx = decl_idx,
        .name = header.relative_name,
        .type_ = .nominal,
        .backing_var = backing_var,
        .is_opaque = nominal.is_opaque,
        .num_args = @intCast(header_args.len),
    } });
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
        is_opaque: bool,
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
            if (ctx == .type_decl) {
                if (self.type_decl_rigid_vars.get(rigid.name)) |decl_var| {
                    _ = try self.unify(anno_var, decl_var, env);
                    return;
                }
            }
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
                                        try self.unifyWith(anno_var, try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
                                            .{ .ident_idx = this_decl.name },
                                            this_decl.backing_var,
                                            &.{},
                                            self.builtin_ctx.module_name,
                                            @intFromEnum(this_decl.idx),
                                            this_decl.is_opaque,
                                            self.cir.module_role == .builtin,
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
                                        try self.unifyWith(anno_var, try self.types.mkNominalWithSourceDeclAndBuiltinOrigin(
                                            .{ .ident_idx = this_decl.name },
                                            this_decl.backing_var,
                                            anno_arg_vars,
                                            self.builtin_ctx.module_name,
                                            @intFromEnum(this_decl.idx),
                                            this_decl.is_opaque,
                                            self.cir.module_role == .builtin,
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
                    const decl_is_alias = decl_resolved == .alias;

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
                    if (decl_is_alias and !try self.validateAliasRows(instantiated_var, env, anno_region)) {
                        try self.unifyWith(anno_var, .err, env);
                        return;
                    }
                    _ = try self.unify(anno_var, instantiated_var, env);
                },
                .external => |ext| {
                    if (try self.resolveVarFromExternal(ext.module_idx, ext.target_node_idx)) |ext_ref| {
                        // Resolve the referenced type
                        const ext_resolved = self.types.resolveVar(ext_ref.local_var).desc.content;
                        const ext_is_alias = ext_resolved == .alias;

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
                        if (ext_is_alias and !try self.validateAliasRows(instantiated_var, env, anno_region)) {
                            try self.unifyWith(anno_var, .err, env);
                            return;
                        }
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
            std.mem.sort(types_mod.Tag, tags_slice, self, struct {
                fn less(checker: *const Self, a: types_mod.Tag, b: types_mod.Tag) bool {
                    return std.mem.order(u8, checker.cir.getIdentStoreConst().getText(a.name), checker.cir.getIdentStoreConst().getText(b.name)) == .lt;
                }
            }.less);

            // Materialize the tags into the types store before processing the
            // ext. `tags_slice` points into the scratch_tags buffer, and
            // generating the ext recurses and may append to scratch_tags,
            // reallocating that buffer and dangling the slice. Copying into a
            // stable range here mirrors the record case below.
            const tags_range = try self.types.appendTags(tags_slice);

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
            try self.unifyWith(
                anno_var,
                .{ .structure = types_mod.FlatType{ .tag_union = .{
                    .tags = tags_range,
                    .ext = ext_var,
                } } },
                env,
            );
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
            std.mem.sort(types_mod.RecordField, record_fields_slice, self, struct {
                fn less(checker: *const Self, a: types_mod.RecordField, b: types_mod.RecordField) bool {
                    return std.mem.order(u8, checker.cir.getIdentStoreConst().getText(a.name), checker.cir.getIdentStoreConst().getText(b.name)) == .lt;
                }
            }.less);
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

fn validateAliasRows(self: *Self, var_: Var, env: *Env, region: Region) Allocator.Error!bool {
    self.var_set.clearRetainingCapacity();
    return self.validateAliasRowsHelp(var_, env, region, &self.var_set);
}

fn validateAliasRowsHelp(
    self: *Self,
    var_: Var,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    if (visited.contains(resolved.var_)) return true;
    try visited.put(resolved.var_, {});

    return switch (resolved.desc.content) {
        .alias => |alias| blk: {
            if (!try self.validateAliasRowsHelp(self.types.getAliasBackingVar(alias), env, region, visited)) break :blk false;
            for (self.types.sliceAliasArgs(alias)) |arg_var| {
                if (!try self.validateAliasRowsHelp(arg_var, env, region, visited)) break :blk false;
            }
            break :blk true;
        },
        .structure => |flat_type| switch (flat_type) {
            .tuple => |tuple| try self.validateAliasRowVars(self.types.sliceVars(tuple.elems), env, region, visited),
            .nominal_type => |nominal| try self.validateAliasRowVars(self.types.sliceNominalArgs(nominal), env, region, visited),
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                if (!try self.validateAliasRowVars(self.types.sliceVars(func.args), env, region, visited)) break :blk false;
                break :blk try self.validateAliasRowsHelp(func.ret, env, region, visited);
            },
            .record => |record| try self.validateRecordRow(record.fields, record.ext, env, region, visited),
            .record_unbound => |fields| try self.validateRecordFields(fields, env, region, visited),
            .tag_union => |tag_union| try self.validateTagUnionRow(tag_union.tags, tag_union.ext, env, region, visited),
            .empty_record, .empty_tag_union => true,
        },
        .flex, .rigid, .err => true,
    };
}

fn validateAliasRowVars(
    self: *Self,
    vars: []const Var,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    for (vars) |var_| {
        if (!try self.validateAliasRowsHelp(var_, env, region, visited)) return false;
    }
    return true;
}

fn validateRecordFields(
    self: *Self,
    fields: types_mod.RecordField.SafeMultiList.Range,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    const field_slice = self.types.getRecordFieldsSlice(fields);
    for (field_slice.items(.var_)) |field_var| {
        if (!try self.validateAliasRowsHelp(field_var, env, region, visited)) return false;
    }
    return true;
}

fn validateRecordRow(
    self: *Self,
    fields: types_mod.RecordField.SafeMultiList.Range,
    ext_var: Var,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    var names = std.AutoHashMap(Ident.Idx, void).init(self.gpa);
    defer names.deinit();

    const field_slice = self.types.getRecordFieldsSlice(fields);
    for (field_slice.items(.name), field_slice.items(.var_)) |name, field_var| {
        const entry = try names.getOrPut(name);
        if (entry.found_existing) {
            try self.reportInvalidAliasRow(.record, field_var, env, region);
            return false;
        }
        if (!try self.validateAliasRowsHelp(field_var, env, region, visited)) return false;
    }

    return try self.validateRecordExt(ext_var, &names, env, region, visited);
}

fn validateRecordExt(
    self: *Self,
    ext_var: Var,
    names: *std.AutoHashMap(Ident.Idx, void),
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    var current = ext_var;
    var guard = types_mod.debug.IterationGuard.init("validateRecordExt");
    while (true) {
        guard.tick();
        const resolved = self.types.resolveVar(current);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current = self.types.getAliasBackingVar(alias);
            },
            .structure => |flat_type| switch (flat_type) {
                .record => |record| {
                    if (!try self.validateRecordExtFields(record.fields, current, names, env, region, visited)) return false;
                    current = record.ext;
                },
                .record_unbound => |fields| {
                    return try self.validateRecordExtFields(fields, current, names, env, region, visited);
                },
                .empty_record => return true,
                else => {
                    try self.reportInvalidAliasRow(.record, current, env, region);
                    return false;
                },
            },
            .flex, .rigid, .err => return true,
        }
    }
}

fn validateRecordExtFields(
    self: *Self,
    fields: types_mod.RecordField.SafeMultiList.Range,
    ext_source_var: Var,
    names: *std.AutoHashMap(Ident.Idx, void),
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    const field_slice = self.types.getRecordFieldsSlice(fields);
    for (field_slice.items(.name), field_slice.items(.var_)) |name, field_var| {
        const entry = try names.getOrPut(name);
        if (entry.found_existing) {
            try self.reportInvalidAliasRow(.record, ext_source_var, env, region);
            return false;
        }
        if (!try self.validateAliasRowsHelp(field_var, env, region, visited)) return false;
    }
    return true;
}

fn validateTagUnionRow(
    self: *Self,
    tags: types_mod.Tag.SafeMultiList.Range,
    ext_var: Var,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    var names = std.AutoHashMap(Ident.Idx, void).init(self.gpa);
    defer names.deinit();

    const tag_slice = self.types.getTagsSlice(tags);
    for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
        const entry = try names.getOrPut(name);
        if (entry.found_existing) {
            try self.reportInvalidAliasRow(.tag_union, ext_var, env, region);
            return false;
        }
        if (!try self.validateAliasRowVars(self.types.sliceVars(args), env, region, visited)) return false;
    }

    return try self.validateTagUnionExt(ext_var, &names, env, region, visited);
}

fn validateTagUnionExt(
    self: *Self,
    ext_var: Var,
    names: *std.AutoHashMap(Ident.Idx, void),
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    var current = ext_var;
    var guard = types_mod.debug.IterationGuard.init("validateTagUnionExt");
    while (true) {
        guard.tick();
        const resolved = self.types.resolveVar(current);
        switch (resolved.desc.content) {
            .alias => |alias| {
                current = self.types.getAliasBackingVar(alias);
            },
            .structure => |flat_type| switch (flat_type) {
                .tag_union => |tag_union| {
                    if (!try self.validateTagUnionExtTags(tag_union.tags, current, names, env, region, visited)) return false;
                    current = tag_union.ext;
                },
                .empty_tag_union => return true,
                else => {
                    try self.reportInvalidAliasRow(.tag_union, current, env, region);
                    return false;
                },
            },
            .flex, .rigid, .err => return true,
        }
    }
}

fn validateTagUnionExtTags(
    self: *Self,
    tags: types_mod.Tag.SafeMultiList.Range,
    ext_source_var: Var,
    names: *std.AutoHashMap(Ident.Idx, void),
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    const tag_slice = self.types.getTagsSlice(tags);
    for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
        const entry = try names.getOrPut(name);
        if (entry.found_existing) {
            try self.reportInvalidAliasRow(.tag_union, ext_source_var, env, region);
            return false;
        }
        if (!try self.validateAliasRowVars(self.types.sliceVars(args), env, region, visited)) return false;
    }
    return true;
}

fn reportInvalidAliasRow(
    self: *Self,
    comptime row_kind: enum { record, tag_union },
    actual_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const expected_content: Content = switch (row_kind) {
        .record => .{ .structure = .empty_record },
        .tag_union => .{ .structure = .empty_tag_union },
    };
    const expected_var = try self.freshFromContent(expected_content, env, region);
    const expected_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, expected_var);
    const actual_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, actual_var);

    _ = try self.problems.appendProblem(self.gpa, .{ .type_mismatch = .{
        .types = .{
            .expected_var = expected_var,
            .expected_snapshot = expected_snapshot,
            .actual_var = actual_var,
            .actual_snapshot = actual_snapshot,
        },
        .context = .type_annotation,
    } });
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
        .u8 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.u8, env), env),
        .u16 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.u16, env), env),
        .u32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.u32, env), env),
        .u64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.u64, env), env),
        .u128 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.u128, env), env),
        .i8 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.i8, env), env),
        .i16 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.i16, env), env),
        .i32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.i32, env), env),
        .i64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.i64, env), env),
        .i128 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.i128, env), env),
        .f32 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.f32, env), env),
        .f64 => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.f64, env), env),
        .dec => try self.unifyWith(anno_var, try self.mkNumberTypeContent(.dec, env), env),
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

const Expected = struct {
    annotation: ?CIR.Annotation.Idx = null,
    branch_result: ?Var = null,

    fn none() Expected {
        return .{};
    }

    fn fromAnnotation(annotation_idx: CIR.Annotation.Idx) Expected {
        return .{ .annotation = annotation_idx };
    }

    fn withBranchResult(self: Expected, branch_result: Var) Expected {
        return .{
            .annotation = self.annotation,
            .branch_result = branch_result,
        };
    }

    fn forBranchBody(self: Expected) Expected {
        return .{
            .branch_result = self.branch_result,
        };
    }
};

// pattern //

/// The "polarity" of a tag union or record
const Polarity = enum { open, closed };

/// The context that this pattern is being called in
/// This determines if, for tag unions & records, they should be inferred
/// as open or closed
const PatternCtx = enum {
    bound,
    fn_arg,
    from_annotation,
    for_,
    match_branch,

    fn toPolarity(self: PatternCtx) Polarity {
        return switch (self) {
            .bound, .fn_arg, .for_ => .closed,
            .from_annotation, .match_branch => .open,
        };
    }
};

/// Check the types for the provided pattern, saving the type in-place
fn checkPattern(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    ctx: PatternCtx,
    env: *Env,
) std.mem.Allocator.Error!void {
    _ = try self.checkPatternHelp(pattern_idx, ctx, env, .in_place);
}

/// Check the types for the provided pattern, either as fresh var or in-place
fn checkPatternHelp(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    ctx: PatternCtx,
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
        .assign => {
            // Assigned variables start out as flex (initialized in preflight),
            // and their type is refined by usage. Reassignments reuse the same
            // pattern var, so never overwrite existing constraints here.
        },
        .underscore => {
            // Underscore can be anything; leave its placeholder flex intact.
        },
        // str //
        .str_literal => {
            // A literal pattern converts through from_quote and compares the
            // matched value against the converted constant, so the type also
            // needs equality.
            const flex_var = try self.mkFlexWithFromQuoteConstraint(ModuleEnv.nodeIdxFrom(pattern_idx), pattern_region, env);
            _ = try self.unify(pattern_var, flex_var, env);
            try self.mkPatternLiteralEqConstraint(pattern_var, env, pattern_region);
        },
        // as //
        .as => |p| {
            const var_ = try self.checkPatternHelp(p.pattern, ctx, env, out_var);
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
                            const elem_var = try self.checkPatternHelp(single_elem_ptrn_idx, ctx, env, out_var);
                            try self.scratch_vars.append(elem_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },
                    .in_place => {
                        // Check tuple elements
                        const elems_slice = self.cir.store.slicePatterns(tuple.patterns);
                        for (elems_slice) |single_elem_ptrn_idx| {
                            _ = try self.checkPatternHelp(single_elem_ptrn_idx, ctx, env, out_var);
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
                const elem_var = try self.checkPatternHelp(elems[0], ctx, env, out_var);

                // Iterate over the remaining elements
                var last_elem_ptrn_idx = elems[0];
                for (elems[1..], 1..) |elem_ptrn_idx, i| {
                    const cur_elem_var = try self.checkPatternHelp(elem_ptrn_idx, ctx, env, out_var);

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
                            _ = try self.checkPatternHelp(remaining_elem_expr_idx, ctx, env, out_var);
                        }

                        // Break to avoid cascading errors
                        break;
                    }

                    last_elem_ptrn_idx = elem_ptrn_idx;
                }

                // Create a nominal List type with the inferred element type
                const list_content = try self.mkListContent(elem_var, env);
                try self.unifyWith(pattern_var, list_content, env);
            }

            // Then, check the "rest" pattern is bound to the list value.
            // This is if the pattern is like `.. as x`.
            if (list.rest_info) |rest_info| {
                if (rest_info.pattern) |rest_pattern_idx| {
                    const rest_pattern_var = try self.checkPatternHelp(rest_pattern_idx, ctx, env, out_var);

                    _ = try self.unify(pattern_var, rest_pattern_var, env);
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
                            const arg_var = try self.checkPatternHelp(arg_expr_idx, ctx, env, out_var);
                            try self.scratch_vars.append(arg_var);
                        }

                        // Add to types store
                        break :blk try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                    },

                    .in_place => {
                        // Process each tag arg
                        const arg_ptrn_idx_slice = self.cir.store.slicePatterns(applied_tag.args);
                        for (arg_ptrn_idx_slice) |arg_expr_idx| {
                            _ = try self.checkPatternHelp(arg_expr_idx, ctx, env, out_var);
                        }

                        // Add to types store
                        // Cast the elems idxs to vars (this works because Anno Idx are 1-1 with type Vars)
                        break :blk try self.types.appendVars(@ptrCast(arg_ptrn_idx_slice));
                    },
                }
            };

            // Create the type
            const ext_var = switch (ctx.toPolarity()) {
                .open => try self.fresh(env, pattern_region),
                .closed => try self.freshFromContent(.{ .structure = .empty_tag_union }, env, pattern_region),
            };

            const tag = types_mod.Tag{ .name = applied_tag.name, .args = arg_vars_slice };
            const tag_union_content = try self.types.mkTagUnion(&[_]types_mod.Tag{tag}, ext_var);

            // Update the expr to point to the new type
            try self.unifyWith(pattern_var, tag_union_content, env);
        },
        // nominal //
        .nominal => |nominal| {
            // Check the backing pattern first
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, ctx, env, out_var);

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
            const actual_backing_var = try self.checkPatternHelp(nominal.backing_pattern, ctx, env, out_var);

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
                            break :blk try self.checkPatternHelp(sub_pattern_idx, ctx, env, out_var);
                        },
                        .SubPattern => |sub_pattern_idx| {
                            break :blk try self.checkPatternHelp(sub_pattern_idx, ctx, env, out_var);
                        },
                        .Rest => |sub_pattern_idx| {
                            // If this pattern is rest pattern:
                            // eg { name, ...rest }
                            //               ^^^^
                            //
                            // Then capture this as the ext var, then  continue
                            const ext_var = try self.checkPatternHelp(sub_pattern_idx, ctx, env, out_var);
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
            std.mem.sort(types_mod.RecordField, record_fields_scratch, self, struct {
                fn less(checker: *const Self, a: types_mod.RecordField, b: types_mod.RecordField) bool {
                    return std.mem.order(u8, checker.cir.getIdentStoreConst().getText(a.name), checker.cir.getIdentStoreConst().getText(b.name)) == .lt;
                }
            }.less);
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
                    const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(pattern_idx), num_literal_info, env);
                    _ = try self.unify(pattern_var, flex_var, env);
                    try self.mkPatternLiteralEqConstraint(pattern_var, env, pattern_region);
                },
                // Phase 5: For explicitly typed literals, use nominal types from Builtin
                .u8 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.u8, env), env),
                .i8 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.i8, env), env),
                .u16 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.u16, env), env),
                .i16 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.i16, env), env),
                .u32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.u32, env), env),
                .i32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.i32, env), env),
                .u64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.u64, env), env),
                .i64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.i64, env), env),
                .u128 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.u128, env), env),
                .i128 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.i128, env), env),
                .f32 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.f32, env), env),
                .f64 => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.f64, env), env),
                .dec => try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.dec, env), env),
            }
        },
        .frac_f32_literal => {
            // Phase 5: Use nominal F32 type
            try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.f32, env), env);
        },
        .frac_f64_literal => {
            // Phase 5: Use nominal F64 type
            try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.f64, env), env);
        },
        .dec_literal => |dec| {
            if (dec.has_suffix) {
                // Explicit suffix like `3.14dec` - use nominal Dec type
                try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.dec, env), env);
            } else {
                // Unannotated decimal literal - create flex var with from_numeral constraint
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    dec.value.num, // RocDec has .num field which is i128 scaled by 10^18
                    dec.value.num < 0,
                    true, // Decimal literals are always fractional
                    pattern_region,
                );

                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(pattern_idx), num_literal_info, env);
                _ = try self.unify(pattern_var, flex_var, env);
                try self.mkPatternLiteralEqConstraint(pattern_var, env, pattern_region);
            }
        },
        .small_dec_literal => |dec| {
            if (dec.has_suffix) {
                // Explicit suffix - use nominal Dec type
                try self.unifyWith(pattern_var, try self.mkNumberTypeContent(.dec, env), env);
            } else {
                // Unannotated decimal literal - create flex var with from_numeral constraint
                const scaled_value = dec.value.toRocDec().num;
                const num_literal_info = types_mod.NumeralInfo.fromI128(
                    scaled_value,
                    dec.value.numerator < 0,
                    true,
                    pattern_region,
                );

                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(pattern_idx), num_literal_info, env);
                _ = try self.unify(pattern_var, flex_var, env);
                try self.mkPatternLiteralEqConstraint(pattern_var, env, pattern_region);
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
        .as => |as_pattern| return as_pattern.ident,
        else => return null,
    }
}

fn patternNeedsExhaustiveness(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    const pattern = self.cir.store.getPattern(pattern_idx);
    return switch (pattern) {
        .assign, .underscore, .runtime_error => false,
        .as => |as_pattern| self.patternNeedsExhaustiveness(as_pattern.pattern),
        .applied_tag,
        .list,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        => true,
        .tuple => |tuple| {
            for (self.cir.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                if (self.patternNeedsExhaustiveness(elem_pattern_idx)) return true;
            }
            return false;
        },
        .record_destructure => |destructure| {
            for (self.cir.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                const sub_pattern_idx = switch (destruct.kind) {
                    .Required => |sub_pattern| sub_pattern,
                    .SubPattern => |sub_pattern| sub_pattern,
                    .Rest => |sub_pattern| sub_pattern,
                };
                if (self.patternNeedsExhaustiveness(sub_pattern_idx)) return true;
            }
            return false;
        },
        .nominal => |nominal| self.patternNeedsExhaustiveness(nominal.backing_pattern),
        .nominal_external => |nominal| self.patternNeedsExhaustiveness(nominal.backing_pattern),
    };
}

const PatternBinding = struct {
    ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
};

fn reportMatchAltBinderProblem(
    self: *Self,
    expected_pattern_idx: CIR.Pattern.Idx,
    actual_pattern_idx: CIR.Pattern.Idx,
    binder_ident: Ident.Idx,
    branch_index: u32,
    first_pattern_index: u32,
    pattern_index: u32,
    num_branches: u32,
    num_patterns: u32,
    match_expr: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const expected_var = ModuleEnv.varFrom(expected_pattern_idx);
    const actual_var = ModuleEnv.varFrom(actual_pattern_idx);
    const expected_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, expected_var);
    const actual_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, actual_var);

    _ = try self.problems.appendProblem(self.gpa, .{ .type_mismatch = .{
        .types = .{
            .expected_var = expected_var,
            .expected_snapshot = expected_snapshot,
            .actual_var = actual_var,
            .actual_snapshot = actual_snapshot,
        },
        .context = .{ .match_alt_binder = .{
            .branch_index = branch_index,
            .first_pattern_index = first_pattern_index,
            .pattern_index = pattern_index,
            .num_branches = num_branches,
            .num_patterns = num_patterns,
            .binder_ident = binder_ident,
            .match_expr = match_expr,
        } },
    } });
}

fn collectPatternBindings(
    self: *const Self,
    pattern_idx: CIR.Pattern.Idx,
    out: *std.ArrayList(PatternBinding),
) std.mem.Allocator.Error!void {
    const pattern = self.cir.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => |assign| try out.append(self.gpa, .{ .ident = assign.ident, .pattern_idx = pattern_idx }),
        .as => |as_pat| {
            try out.append(self.gpa, .{ .ident = as_pat.ident, .pattern_idx = pattern_idx });
            try self.collectPatternBindings(as_pat.pattern, out);
        },
        .tuple => |tuple| {
            for (self.cir.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(elem_pattern_idx, out);
            }
        },
        .applied_tag => |tag| {
            for (self.cir.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectPatternBindings(arg_pattern_idx, out);
            }
        },
        .record_destructure => |destructure| {
            for (self.cir.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectPatternBindings(sub_pattern_idx, out),
                    .SubPattern => |sub_pattern_idx| try self.collectPatternBindings(sub_pattern_idx, out),
                    .Rest => |sub_pattern_idx| try self.collectPatternBindings(sub_pattern_idx, out),
                }
            }
        },
        .list => |list| {
            for (self.cir.store.slicePatterns(list.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(elem_pattern_idx, out);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.collectPatternBindings(rest_pattern_idx, out);
                }
            }
        },
        .nominal => |nom| try self.collectPatternBindings(nom.backing_pattern, out),
        .nominal_external => |nom| try self.collectPatternBindings(nom.backing_pattern, out),
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
    }
}

fn unifyMatchAltPatternBindings(
    self: *Self,
    branch_ptrn_idxs: []const CIR.Expr.Match.BranchPattern.Idx,
    branch_index: u32,
    num_branches: u32,
    match_expr: CIR.Expr.Idx,
    env: *Env,
) std.mem.Allocator.Error!bool {
    if (branch_ptrn_idxs.len <= 1) return false;

    var baseline = std.AutoHashMap(u32, struct {
        pattern_idx: CIR.Pattern.Idx,
        pattern_index: u32,
    }).init(self.gpa);
    defer baseline.deinit();

    var bindings: std.ArrayList(PatternBinding) = .empty;
    defer bindings.deinit(self.gpa);

    {
        const first_branch_pattern = self.cir.store.getMatchBranchPattern(branch_ptrn_idxs[0]);
        try self.collectPatternBindings(first_branch_pattern.pattern, &bindings);
        for (bindings.items) |binding| {
            try baseline.put(@as(u32, @bitCast(binding.ident)), .{
                .pattern_idx = binding.pattern_idx,
                .pattern_index = 0,
            });
        }
    }

    var had_type_error = false;

    for (branch_ptrn_idxs[1..], 1..) |branch_ptrn_idx, pattern_index| {
        bindings.clearRetainingCapacity();
        const branch_pattern = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
        try self.collectPatternBindings(branch_pattern.pattern, &bindings);

        var current = std.AutoHashMap(u32, CIR.Pattern.Idx).init(self.gpa);
        defer current.deinit();

        for (bindings.items) |binding| {
            const key: u32 = @bitCast(binding.ident);
            try current.put(key, binding.pattern_idx);
            const first = baseline.get(key) orelse {
                const first_branch_pattern = self.cir.store.getMatchBranchPattern(branch_ptrn_idxs[0]);
                try self.reportMatchAltBinderProblem(
                    first_branch_pattern.pattern,
                    binding.pattern_idx,
                    binding.ident,
                    branch_index,
                    0,
                    @intCast(pattern_index),
                    num_branches,
                    @intCast(branch_ptrn_idxs.len),
                    match_expr,
                );
                had_type_error = true;
                continue;
            };
            const result = try self.unifyInContext(
                ModuleEnv.varFrom(first.pattern_idx),
                ModuleEnv.varFrom(binding.pattern_idx),
                env,
                .{ .match_alt_binder = .{
                    .branch_index = branch_index,
                    .first_pattern_index = first.pattern_index,
                    .pattern_index = @intCast(pattern_index),
                    .num_branches = num_branches,
                    .num_patterns = @intCast(branch_ptrn_idxs.len),
                    .binder_ident = binding.ident,
                    .match_expr = match_expr,
                } },
            );
            if (!result.isOk()) had_type_error = true;
        }

        var baseline_iter = baseline.iterator();
        while (baseline_iter.next()) |entry| {
            if (current.contains(entry.key_ptr.*)) continue;

            const ident: Ident.Idx = @bitCast(entry.key_ptr.*);
            try self.reportMatchAltBinderProblem(
                entry.value_ptr.pattern_idx,
                branch_pattern.pattern,
                ident,
                branch_index,
                entry.value_ptr.pattern_index,
                @intCast(pattern_index),
                num_branches,
                @intCast(branch_ptrn_idxs.len),
                match_expr,
            );
            had_type_error = true;
        }
    }

    return had_type_error;
}

// expr //

fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr = self.cir.store.getExpr(expr_idx);
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));
    const expr_var_raw = ModuleEnv.varFrom(expr_idx);

    // Consume the checking_call_arg flag: it applies only to this immediate
    // checkExpr call and must not propagate to recursive calls (e.g. e_closure
    // delegating to its inner e_lambda, or nested call arguments).
    const is_call_arg = self.checking_call_arg;
    self.checking_call_arg = false;

    // Consume the binding-RHS flag: it applies only to this immediate checkExpr
    // call and must not propagate into subexpressions.
    const is_binding_rhs = self.checking_binding_rhs;
    self.checking_binding_rhs = false;

    // Value restriction: only generalize at the inner lambda level, not the
    // outer e_closure wrapper (which delegates to e_lambda's own checkExpr).
    // Direct call-argument lambdas are consumed immediately, so they must not
    // generalize independently. Doing so lets their generalized vars escape
    // into the enclosing value via unification.
    //
    // We also generalize a binding whose RHS is a bare reference to an already-
    // generalized scheme (e.g. `shorthand = FooBar.myfunc`). Such a reference is
    // non-expansive: it performs no work and can hide no `dbg`/`expect`, so it
    // raises none of the duplicate-work or duplicate-effect concerns that motivate
    // restricting generalization to syntactic functions. In practice the only
    // generalized schemes are functions (numeric literals use the separate
    // defaulting path), so this never re-generalizes numbers or tag unions. We
    // restrict this to binding-RHS position so that bare lookups appearing as
    // arbitrary subexpressions don't pay generalization cost or get generalized
    // out from under their surrounding context.
    const is_value_alias = is_binding_rhs and
        (expr == .e_lookup_local or expr == .e_lookup_external);
    const should_generalize = (isFunctionDef(&self.cir.store, expr) and expr != .e_closure and !is_call_arg) or is_value_alias;

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

        if (expected.annotation) |annotation_idx| {
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
        } else {
            break :blk .{ expr_var_raw, null };
        }
    };

    var does_fx = false; // Does this expression potentially perform any side effects?

    switch (expr) {
        // str //
        .e_str_segment => {
            const str_var = try self.freshStr(env, expr_region);
            _ = try self.unify(expr_var, str_var, env);
        },
        .e_bytes_literal => {
            // Create List(U8) type
            const u8_content = try self.mkNumberTypeContent(.u8, env);
            const u8_var = try self.freshFromContent(u8_content, env, expr_region);
            const list_content = try self.mkListContent(u8_var, env);
            try self.unifyWith(expr_var, list_content, env);
        },
        .e_str => |str| {
            // Iterate over the string segments, checking each one
            const segment_expr_idx_slice = self.cir.store.sliceExpr(str.span);
            var did_err = false;
            var has_interpolation = false;
            for (segment_expr_idx_slice) |seg_expr_idx| {
                const seg_expr = self.cir.store.getExpr(seg_expr_idx);

                // String literal segments are already Str type
                switch (seg_expr) {
                    .e_str_segment => {
                        does_fx = try self.checkExpr(seg_expr_idx, env, Expected.none()) or does_fx;
                    },
                    else => {
                        has_interpolation = true;
                        does_fx = try self.checkExpr(seg_expr_idx, env, Expected.none()) or does_fx;
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
            } else if (has_interpolation) {
                // Interpolated strings are Str
                const str_var = try self.freshStr(env, expr_region);
                _ = try self.unify(expr_var, str_var, env);
            } else {
                // A plain literal converts to its target type through from_quote,
                // defaulting to Str if nothing pins it.
                const flex_var = try self.mkFlexWithFromQuoteConstraint(ModuleEnv.nodeIdxFrom(expr_idx), expr_region, env);
                if (self.cir.numericSuffixTypeForNode(ModuleEnv.nodeIdxFrom(expr_idx)) != null) {
                    // Explicit type suffix, e.g. `"foo".MyType`.
                    try self.unifyTypedLiteralWithExplicitType(flex_var, expr_idx, expr_region, env);
                }
                _ = try self.unify(expr_var, flex_var, env);
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
                    const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
                    _ = try self.unify(expr_var, flex_var, env);
                },
                .u8 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.u8, env), env),
                .i8 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.i8, env), env),
                .u16 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.u16, env), env),
                .i16 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.i16, env), env),
                .u32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.u32, env), env),
                .i32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.i32, env), env),
                .u64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.u64, env), env),
                .i64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.i64, env), env),
                .u128 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.u128, env), env),
                .i128 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.i128, env), env),
                .f32 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.f32, env), env),
                .f64 => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.f64, env), env),
                .dec => try self.unifyWith(expr_var, try self.mkNumberTypeContent(.dec, env), env),
            }
        },
        .e_num_from_numeral => {
            const num_literal_info = try self.exactNumeralInfoForExpr(expr_idx, expr_region);
            const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
            _ = try self.unify(expr_var, flex_var, env);
        },
        .e_frac_f32 => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent(.f32, env), env);
            } else {
                // Unsuffixed fractional literal - create constrained flex var
                var num_literal_info = types_mod.NumeralInfo.fromI128(
                    @as(i128, @as(u32, @bitCast(frac.value))),
                    frac.value < 0,
                    true,
                    expr_region,
                );
                num_literal_info.frac_requirements = .{
                    .fits_in_f32 = true,
                    .fits_in_dec = CIR.fitsInDec(@as(f64, @floatCast(frac.value))),
                };
                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);
            }
        },
        .e_frac_f64 => |frac| {
            if (frac.has_suffix) {
                try self.unifyWith(expr_var, try self.mkNumberTypeContent(.f64, env), env);
            } else {
                // Unsuffixed fractional literal - create constrained flex var
                var num_literal_info = types_mod.NumeralInfo.fromI128(
                    @as(i128, @as(u64, @bitCast(frac.value))),
                    frac.value < 0,
                    true,
                    expr_region,
                );
                num_literal_info.frac_requirements = .{
                    .fits_in_f32 = CIR.fitsInF32(frac.value),
                    .fits_in_dec = CIR.fitsInDec(frac.value),
                };
                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);
            }
        },
        .e_dec => |frac| {
            if (frac.has_suffix) {
                const num_literal_info = try self.exactNumeralInfoForExpr(expr_idx, expr_region);
                _ = try self.reportInvalidBuiltinFromNumeralInfo(expr_var, .dec, num_literal_info, env);
                try self.unifyWith(expr_var, try self.mkNumberTypeContent(.dec, env), env);
            } else {
                // Unsuffixed Dec literal - create constrained flex var
                var num_literal_info = types_mod.NumeralInfo.fromI128(
                    frac.value.num,
                    frac.value.num < 0,
                    true,
                    expr_region,
                );
                num_literal_info.frac_requirements = .{
                    .fits_in_f32 = CIR.fitsInF32(frac.value.toF64()),
                    .fits_in_dec = true,
                };
                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);
            }
        },
        .e_dec_small => |frac| {
            if (frac.has_suffix) {
                const num_literal_info = try self.exactNumeralInfoForExpr(expr_idx, expr_region);
                _ = try self.reportInvalidBuiltinFromNumeralInfo(expr_var, .dec, num_literal_info, env);
                try self.unifyWith(expr_var, try self.mkNumberTypeContent(.dec, env), env);
            } else {
                // Unsuffixed small Dec literal - create constrained flex var
                const scaled_value = frac.value.toRocDec().num;
                const literal = self.recordedNumeralLiteralForExpr(expr_idx);
                const is_fractional = literal.hadDecimalPoint() or frac.value.denominator_power_of_ten != 0;
                const literal_value: i128 = if (is_fractional) scaled_value else frac.value.numerator;
                var num_literal_info = types_mod.NumeralInfo.fromI128(
                    literal_value,
                    literal_value < 0,
                    is_fractional,
                    expr_region,
                );
                const f64_val = frac.value.toF64();
                num_literal_info.frac_requirements = .{
                    .fits_in_f32 = CIR.fitsInF32(f64_val),
                    .fits_in_dec = true,
                };
                const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);
                _ = try self.unify(expr_var, flex_var, env);
            }
        },
        .e_typed_int => |typed_num| {
            // Typed integer literal like 123.U64
            // Create from_numeral constraint and unify with the explicit type
            const num_literal_info = if (self.typedLiteralTargetsBuiltin(expr_idx, .dec))
                try self.exactNumeralInfoForExpr(expr_idx, expr_region)
            else switch (typed_num.value.kind) {
                .u128 => types_mod.NumeralInfo.fromU128(@bitCast(typed_num.value.bytes), false, expr_region),
                .i128 => types_mod.NumeralInfo.fromI128(typed_num.value.toI128(), typed_num.value.toI128() < 0, false, expr_region),
            };

            // Create flex var with from_numeral constraint
            const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);

            try self.unifyTypedLiteralWithExplicitType(
                flex_var,
                expr_idx,
                expr_region,
                env,
            );
            if (self.typedLiteralTargetsBuiltin(expr_idx, .dec)) {
                _ = try self.reportInvalidBuiltinFromNumeralInfo(flex_var, .dec, num_literal_info, env);
            }

            // Unify expr_var with the flex_var (which is now constrained to the explicit type)
            _ = try self.unify(expr_var, flex_var, env);
        },
        .e_typed_frac => {
            // Typed fractional literal like 3.14.Dec
            const num_literal_info = try self.exactNumeralInfoForExpr(expr_idx, expr_region);

            // Create flex var with from_numeral constraint
            const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);

            try self.unifyTypedLiteralWithExplicitType(
                flex_var,
                expr_idx,
                expr_region,
                env,
            );
            if (self.typedLiteralTargetsBuiltin(expr_idx, .dec)) {
                _ = try self.reportInvalidBuiltinFromNumeralInfo(flex_var, .dec, num_literal_info, env);
            }

            // Unify expr_var with the flex_var (which is now constrained to the explicit type)
            _ = try self.unify(expr_var, flex_var, env);
        },
        .e_typed_num_from_numeral => {
            const num_literal_info = try self.exactNumeralInfoForExpr(expr_idx, expr_region);
            const flex_var = try self.mkFlexWithFromNumeralConstraint(ModuleEnv.nodeIdxFrom(expr_idx), num_literal_info, env);

            try self.unifyTypedLiteralWithExplicitType(
                flex_var,
                expr_idx,
                expr_region,
                env,
            );
            if (self.typedLiteralTargetsBuiltin(expr_idx, .dec)) {
                _ = try self.reportInvalidBuiltinFromNumeralInfo(flex_var, .dec, num_literal_info, env);
            }

            _ = try self.unify(expr_var, flex_var, env);
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
                does_fx = try self.checkExpr(elems[0], env, Expected.none()) or does_fx;

                // Iterate over the remaining elements
                const elem_var = ModuleEnv.varFrom(elems[0]);
                var last_elem_expr_idx = elems[0];
                for (elems[1..], 1..) |elem_expr_idx, i| {
                    does_fx = try self.checkExpr(elem_expr_idx, env, Expected.none()) or does_fx;
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
                            does_fx = try self.checkExpr(remaining_elem_expr_idx, env, Expected.none()) or does_fx;
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
                does_fx = try self.checkExpr(single_elem_expr_idx, env, Expected.none()) or does_fx;
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
            does_fx = try self.checkExpr(tuple_access.tuple, env, Expected.none()) or does_fx;

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
                            const min_elems = elem_index + 1;
                            const scratch_vars_top = self.scratch_vars.top();
                            defer self.scratch_vars.clearFrom(scratch_vars_top);

                            for (0..min_elems) |_| {
                                const fresh_var = try self.fresh(env, expr_region);
                                try self.scratch_vars.append(fresh_var);
                            }
                            const expected_elems = try self.types.appendVars(self.scratch_vars.sliceFromStart(scratch_vars_top));
                            const expected_tuple_var = try self.freshFromContent(.{ .structure = .{
                                .tuple = .{ .elems = expected_elems },
                            } }, env, expr_region);

                            _ = try self.unify(expected_tuple_var, tuple_var, env);
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

                        // A non-tuple structure can never satisfy a tuple access,
                        // so this unify reports the mismatch. Poison the result to
                        // `.err` (like the out-of-bounds branch above) rather than
                        // leaving it a fresh flex var, so conflicting downstream
                        // uses of the result don't produce cascading errors.
                        _ = try self.unify(tuple_var, expected_tuple_var, env);
                        try self.unifyWith(expr_var, .err, env);
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
                does_fx = try self.checkExpr(record_being_updated_expr, env, Expected.none()) or does_fx;

                const record_being_updated_var = ModuleEnv.varFrom(record_being_updated_expr);
                const record_being_updated_name: ?Ident.Idx = self.getExprPatternIdent(record_being_updated_expr);

                // Process each field
                for (self.cir.store.sliceRecordFields(e.fields)) |field_idx| {
                    const field = self.cir.store.getRecordField(field_idx);

                    // Check the field value expression
                    does_fx = try self.checkExpr(field.value, env, Expected.none()) or does_fx;

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
                    does_fx = try self.checkExpr(field.value, env, Expected.none()) or does_fx;

                    // Append it to the scratch records array
                    try self.scratch_record_fields.append(types_mod.RecordField{
                        .name = field.name,
                        .var_ = ModuleEnv.varFrom(field.value),
                    });
                }

                // Copy the scratch fields into the types store
                const record_fields_scratch = self.scratch_record_fields.sliceFromStart(record_fields_top);
                std.mem.sort(types_mod.RecordField, record_fields_scratch, self, struct {
                    fn less(checker: *const Self, a: types_mod.RecordField, b: types_mod.RecordField) bool {
                        return std.mem.order(u8, checker.cir.getIdentStoreConst().getText(a.name), checker.cir.getIdentStoreConst().getText(b.name)) == .lt;
                    }
                }.less);
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
                does_fx = try self.checkExpr(arg_expr_idx, env, Expected.none()) or does_fx;
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
            does_fx = try self.checkExpr(nominal.backing_expr, env, Expected.none()) or does_fx;
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
            does_fx = try self.checkExpr(nominal.backing_expr, env, Expected.none()) or does_fx;
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

            try self.value_lookup_tracking.append(self.gpa, .{
                .expr_idx = expr_idx,
                .pattern_idx = lookup.pattern_idx,
            });

            const mb_processing_def = self.top_level_ptrns.get(lookup.pattern_idx);
            if (mb_processing_def) |processing_def| {
                const referenced_def = self.cir.store.getDef(processing_def.def_idx);

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
                            _ = try self.deferred_cycle_envs.append(self.gpa, sub_env);

                            const def = self.cir.store.getDef(processing_def.def_idx);
                            const def_expr_var = ModuleEnv.varFrom(def.expr);
                            if (def.annotation != null) {
                                // Forward reference to an ANNOTATED member of the
                                // recursive group (mutual recursion). Decouple the
                                // reference with a flex var and defer a
                                // cross-reference constraint to the cycle root,
                                // where the target is generalized and instantiated
                                // per use-site. Unifying directly with the target's
                                // (not-yet-generalized) type would force the two
                                // members' rigid type parameters to coincide,
                                // producing a spurious `T(k)` != `T(k)`.
                                try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
                                _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
                                    .expected = def_expr_var,
                                    .actual = expr_var,
                                    .ctx = .{ .recursive_def = .{ .def_name = processing_def.def_name } },
                                    .is_cross_reference = true,
                                } });
                            } else {
                                // Unannotated member: the group is inferred together
                                // and shares type variables, so link monomorphically.
                                // After checkDef, e_closure rank elevation has run,
                                // so the closure var is at rank 2 — safe to unify
                                // without pulling body vars below the generalization
                                // rank.
                                _ = try self.unify(expr_var, def_expr_var, env);
                            }

                            break :blk;
                        } else {
                            std.debug.assert(sub_env.rank() == .outermost);
                            self.env_pool.release(sub_env);
                        }
                    },
                    .processing => {
                        if (!isFunctionDef(&self.cir.store, self.cir.store.getExpr(referenced_def.expr))) {
                            try self.poisonRecursiveNonFunctionProcessingDef(processing_def, expr_idx, env);
                            break :blk;
                        }

                        // Recursive function reference. We assign the lookup a
                        // flex var, then record an equality constraint for
                        // later validation at the function-recursion boundary.

                        // Assert that this def is NOT generalized nor outermost
                        std.debug.assert(self.types.resolveVar(pat_var).desc.rank != .generalized);

                        // Set the expr to be a flex
                        try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);

                        // A reference to a *different*, ANNOTATED def in the cycle
                        // (mutual recursion) is instantiated per use-site at
                        // validation time, so the members' rigid type parameters
                        // don't clash. A self-reference, or a reference to an
                        // unannotated member, stays monomorphic: unannotated
                        // mutually-recursive functions are inferred as a group and
                        // must share their (not-yet-generalized) type variables.
                        const is_cross_reference = (referenced_def.annotation != null) and
                            if (self.current_processing_def) |current_def|
                                current_def != processing_def.def_idx
                            else
                                false;

                        // For a cross-reference, target the callee's expr var
                        // rather than its pattern var: at the cycle root the
                        // root's pattern var is not yet linked to its generalized
                        // type (that def-level unification happens after the body
                        // is checked), but every participant's expr var has been
                        // generalized by then — so instantiation can find it.
                        const constraint_expected = if (is_cross_reference)
                            ModuleEnv.varFrom(referenced_def.expr)
                        else
                            pat_var;

                        // Write down this constraint for later validation
                        _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
                            .expected = constraint_expected,
                            .actual = expr_var,
                            .ctx = .{ .recursive_def = .{ .def_name = processing_def.def_name } },
                            .is_cross_reference = is_cross_reference,
                        } });

                        // Detect mutual recursion through local lookups. If the
                        // referenced def is different from the current one, we
                        // have a function cycle: current → ... → this_def → ...
                        // → current.
                        if (self.current_processing_def) |current_def| {
                            if (current_def != processing_def.def_idx) {
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

                        break :blk;
                    },
                    .processed => {},
                }
            }

            // Local block-def recursion. If this lookup targets a local `s_decl`
            // function whose body is currently being checked, it's a recursive
            // reference (to the def itself, or to an enclosing in-flight def).
            // Defer unification — fresh flex now + a pending `local_recursive_refs`
            // entry validated after the def generalizes — so we don't unify with
            // the not-yet-generalized pattern var, which would lower its rank and
            // prevent generalization of the def's rigid type parameters.
            //
            // A reference to an already-finished sibling local def is NOT in this
            // map (removed after it generalizes), so it falls through to the tail
            // below and instantiates normally.
            if (self.local_processing_ptrns.get(lookup.pattern_idx)) |local_def| {
                // The pattern is mid-check, so it must not be generalized yet.
                std.debug.assert(self.types.resolveVar(pat_var).desc.rank != .generalized);

                // Set the expr to be a flex
                try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);

                // Record for validation once the def's lambda has generalized.
                // A dedicated stack, not the shared constraints list (sequential
                // scoping makes this a single self/enclosing chain — see the
                // `local_recursive_refs` field doc).
                try self.local_recursive_refs.append(self.gpa, .{
                    .pat_var = pat_var,
                    .expr_var = expr_var,
                    .def_name = local_def.def_name,
                });

                break :blk;
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
            const stmt_result = try self.checkBlockStatements(block.stmts, env, expr_region);
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
            // Record the parameter span for the end-of-check pinnable
            // collection (see `checked_lambda_params`).
            try self.checked_lambda_params.append(self.gpa, lambda.args);

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
            const arg_count = lambda.args.span.len;
            var arg_vars_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.gpa);
            const arg_vars_alloc = arg_vars_sfa.get();
            const arg_vars = try arg_vars_alloc.alloc(Var, arg_count);
            defer arg_vars_alloc.free(arg_vars);
            const pattern_ctx: PatternCtx = if (mb_anno_func != null) .from_annotation else .fn_arg;
            for (0..arg_count) |i| {
                const pattern_idx = self.cir.store.patternAt(lambda.args, i);
                arg_vars[i] = ModuleEnv.varFrom(pattern_idx);
                try self.checkPattern(pattern_idx, pattern_ctx, env);
            }

            // Now, check if we have an expected function to validate against
            if (mb_anno_func) |anno_func| {
                // Use index-based iteration instead of slices because unifyInContext
                // may trigger reallocations that would invalidate slice pointers
                const anno_func_args_range = anno_func.args;
                const anno_func_args_len = anno_func_args_range.len();

                // Next, check if the arguments arities match
                if (anno_func_args_len == arg_count) {
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

                                const arg_1 = arg_vars[i];
                                const arg_2 = arg_vars[j];

                                const unify_result = try self.unifyInContext(arg_1, arg_2, env, .{
                                    .fn_args_bound_var = .{
                                        .fn_name = self.enclosing_func_name,
                                        .first_arg_var = arg_1,
                                        .second_arg_var = arg_2,
                                        .first_arg_index = @intCast(i),
                                        .second_arg_index = @intCast(j),
                                        .num_args = @intCast(arg_count),
                                    },
                                });
                                if (unify_result.isProblem()) {
                                    // Context already set by unifyInContext
                                    // Stop execution
                                    try self.unifyWith(expr_var, .err, env);
                                    break :for_blk;
                                }
                            }
                        }
                    }

                    // Then, lastly, we unify the annotation types against the
                    // actual type
                    for (arg_vars, 0..) |arg_var, i| {
                        const expected_arg_var = self.types.getVarAt(anno_func_args_range, @intCast(i));
                        _ = try self.unifyInContext(expected_arg_var, arg_var, env, .type_annotation);
                    }
                } else {
                    // This means the expected type and the actual lambda have
                    // an arity mismatch. This will be caught by the regular
                    // expectation checking code at the bottom of this function
                }
            }

            const body_var = ModuleEnv.varFrom(lambda.body);

            // Check the the body of the expr
            // If we have an expected function, use that as the expr's expected type
            const saved_empirical_exhaustiveness_depth = self.empirical_exhaustiveness_depth;
            self.empirical_exhaustiveness_depth = 0;
            defer self.empirical_exhaustiveness_depth = saved_empirical_exhaustiveness_depth;

            const body_does_fx = if (mb_anno_func) |expected_func| blk: {
                const lambda_body_does_fx = try self.checkExpr(lambda.body, env, Expected.none().withBranchResult(expected_func.ret));
                try self.closeAbsentConstructedPayloadVars(lambda.body, body_var);
                _ = try self.unifyInContext(expected_func.ret, body_var, env, .type_annotation);
                break :blk lambda_body_does_fx;
            } else blk: {
                const lambda_body_does_fx = try self.checkExpr(lambda.body, env, Expected.none());
                try self.closeAbsentConstructedPayloadVars(lambda.body, body_var);
                break :blk lambda_body_does_fx;
            };

            // Process any pending return constraints (from early returns / ? operator) before
            // creating the function type. This must happen after the body is fully checked
            // (for correct error reporting) but before the function type is generalized
            // (so instantiated copies at call sites have the complete type, including
            // both Ok and Err variants from the ? operator).
            // Only processes early_return/try_suffix_return constraints; anonymous
            // constraints (e.g. from recursive lookups) are left for later.
            try self.processReturnConstraints(env);

            // Create the function type
            if (body_does_fx) {
                try self.unifyWith(expr_var, try self.types.mkFuncEffectful(arg_vars, body_var), env);
            } else {
                try self.unifyWith(expr_var, try self.types.mkFuncUnbound(arg_vars, body_var), env);
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
            const saved_checking_call_arg = self.checking_call_arg;
            self.checking_call_arg = is_call_arg;
            defer self.checking_call_arg = saved_checking_call_arg;
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
                    try self.types.setDescRank(expr_resolved.desc_idx, lambda_rank);
                }
            }

            _ = try self.unify(expr_var, lambda_var, env);
        },
        // function calling //
        .e_call => |call| {
            switch (call.called_via) {
                .apply, .record_builder, .range => blk: {
                    // First, check the function being called
                    // It could be effectful, e.g. `(mk_fn!())(arg)`
                    self.checking_call_arg = true;
                    does_fx = try self.checkExpr(call.func, env, Expected.none()) or does_fx;
                    const call_func_expr_var = ModuleEnv.varFrom(call.func);

                    // If the function was generalized (e.g. an immediately-invoked
                    // lambda `(|x| ...)(arg)`), instantiate it so the call site gets
                    // fresh type variables. Without this, the generalized vars would
                    // be unified directly with concrete arg types, which can leak
                    // generalization into the enclosing function's types.
                    const func_var = blk_instantiate: {
                        const resolved = self.types.resolveVar(call_func_expr_var);
                        if (resolved.desc.rank == Rank.generalized) {
                            break :blk_instantiate try self.instantiateVar(
                                call_func_expr_var,
                                env,
                                .use_last_var,
                            );
                        } else {
                            break :blk_instantiate call_func_expr_var;
                        }
                    };
                    // Resolve the func var
                    const resolved_func = self.types.resolveVar(func_var).desc.content;
                    var did_err = resolved_func == .err;

                    // Second, check the arguments being called
                    // It could be effectful, e.g. `fn(mk_arg!())`
                    const call_arg_expr_idxs = self.cir.store.sliceExpr(call.args);
                    for (call_arg_expr_idxs) |call_arg_idx| {
                        self.checking_call_arg = true;
                        does_fx = try self.checkExpr(call_arg_idx, env, Expected.none()) or does_fx;

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
                                                try self.unifyWith(expr_var, .err, env);
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
                                        try self.unifyWith(expr_var, .err, env);
                                        break :blk;
                                    }
                                }

                                if (call.called_via == .record_builder) {
                                    const result = try self.enforceRecordBuilderMap2Return(func, env, expr_idx, func_name);
                                    if (result.isProblem()) {
                                        try self.unifyWith(expr_var, .err, env);
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

                        const published_constraint_args: []Var = @ptrCast(call_arg_expr_idxs);
                        const published_constraint_func = Func{
                            .args = try self.types.appendVars(published_constraint_args),
                            .ret = expr_var,
                            .needs_instantiation = false,
                        };
                        const published_constraint_flat: FlatType = if (mb_func_info) |info|
                            if (info.is_effectful)
                                .{ .fn_effectful = published_constraint_func }
                            else
                                .{ .fn_pure = published_constraint_func }
                        else
                            .{ .fn_unbound = published_constraint_func };
                        const published_constraint_fn_var = try self.freshFromContent(.{ .structure = published_constraint_flat }, env, expr_region);

                        try self.cir.store.replaceExprWithCallConstraint(
                            expr_idx,
                            call.func,
                            call.args,
                            call.called_via,
                            published_constraint_fn_var,
                        );
                    }
                },
                else => {
                    // The canonicalizer currently only produces apply, record_builder, or range for e_call expressions.
                    // Other call types (binop, unary_op, string_interpolation) are
                    // represented as different expression types. If we hit this, there's a compiler bug.
                    std.debug.assert(false);
                    try self.unifyWith(expr_var, .err, env);
                },
            }
        },
        .e_if => |if_expr| {
            does_fx = try self.checkIfElseExpr(expr_idx, expr_region, env, if_expr, expected) or does_fx;
        },
        .e_match => |match| {
            does_fx = try self.checkMatchExpr(expr_idx, env, match, expected) or does_fx;
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
        .e_field_access => |field_access| {
            does_fx = try self.checkExpr(field_access.receiver, env, Expected.none()) or does_fx;
            const receiver_var = ModuleEnv.varFrom(field_access.receiver);

            const record_field_var = try self.fresh(env, expr_region);
            const record_field_range = try self.types.appendRecordFields(&.{types_mod.RecordField{
                .name = field_access.field_name,
                .var_ = record_field_var,
            }});
            const record_ext_var = try self.fresh(env, expr_region);
            const record_being_accessed = try self.freshFromContent(.{ .structure = .{
                .record = .{ .fields = record_field_range, .ext = record_ext_var },
            } }, env, expr_region);

            _ = try self.unifyInContext(record_being_accessed, receiver_var, env, .{ .record_access = .{
                .field_name = field_access.field_name,
                .field_region = field_access.field_name_region,
            } });
            _ = try self.unify(expr_var, record_field_var, env);
        },
        .e_interpolation => |interpolation| {
            self.checking_call_arg = true;
            does_fx = try self.checkExpr(interpolation.first, env, Expected.none()) or does_fx;
            const first_var = ModuleEnv.varFrom(interpolation.first);
            const str_var = try self.freshStr(env, expr_region);
            _ = try self.unify(first_var, str_var, env);
            var did_err = self.types.resolveVar(first_var).desc.content == .err;

            const parts = self.cir.store.sliceExpr(interpolation.parts);
            std.debug.assert(parts.len % 2 == 0);
            const item_var = try self.fresh(env, expr_region);
            var part_i: usize = 0;
            while (part_i < parts.len) : (part_i += 2) {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(parts[part_i], env, Expected.none()) or does_fx;
                const interpolated_var = ModuleEnv.varFrom(parts[part_i]);
                did_err = did_err or (self.types.resolveVar(interpolated_var).desc.content == .err);

                self.checking_call_arg = true;
                does_fx = try self.checkExpr(parts[part_i + 1], env, Expected.none()) or does_fx;
                const following_segment_var = ModuleEnv.varFrom(parts[part_i + 1]);
                _ = try self.unify(str_var, following_segment_var, env);
                did_err = did_err or (self.types.resolveVar(following_segment_var).desc.content == .err);
            }

            const pair_elems = try self.types.appendVars(&.{ item_var, str_var });
            const pair_var = try self.freshFromContent(.{ .structure = .{
                .tuple = .{ .elems = pair_elems },
            } }, env, expr_region);
            const rest_var = try self.mkIterVar(pair_var, env, expr_region);
            try self.setVarRank(rest_var, env);

            const step_content = try self.mkIteratorStepContent(pair_var, rest_var, env);
            const step_ret_var = try self.freshFromContent(step_content, env, expr_region);
            const empty_args = try self.types.appendVars(&.{});
            const step_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
                .args = empty_args,
                .ret = step_ret_var,
                .needs_instantiation = false,
            } } }, env, expr_region);

            if (did_err) {
                try self.unifyWith(expr_var, .err, env);
            } else {
                const dispatcher_var = (try self.explicitTypeSuffixVar(expr_idx, expr_region, env)) orelse expr_var;
                const arg_vars = [_]Var{ first_var, rest_var };
                const constraint_fn_var = try self.mkInterpolationConstraint(
                    dispatcher_var,
                    &arg_vars,
                    expr_var,
                    self.cir.idents.from_interpolation,
                    env,
                    interpolation.method_name_region,
                    expr_idx,
                );
                try self.interpolation_item_var_by_fn_var.put(constraint_fn_var, item_var);
                try self.cir.store.replaceExprWithInterpolationConstraint(
                    expr_idx,
                    interpolation.first,
                    interpolation.parts,
                    interpolation.method_name_region,
                    constraint_fn_var,
                    step_fn_var,
                );
            }
        },
        .e_method_call => |method_call| {
            does_fx = try self.checkExpr(method_call.receiver, env, Expected.none()) or does_fx;
            const receiver_var = ModuleEnv.varFrom(method_call.receiver);
            var did_err = self.types.resolveVar(receiver_var).desc.content == .err;

            const arg_expr_idxs = self.cir.store.sliceExpr(method_call.args);
            var arg_vars_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.gpa);
            const arg_vars_alloc = arg_vars_sfa.get();
            const arg_vars = try arg_vars_alloc.alloc(Var, arg_expr_idxs.len);
            defer arg_vars_alloc.free(arg_vars);

            for (arg_expr_idxs, 0..) |arg_expr_idx, i| {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(arg_expr_idx, env, Expected.none()) or does_fx;
                const arg_var = ModuleEnv.varFrom(arg_expr_idx);
                arg_vars[i] = arg_var;
                did_err = did_err or (self.types.resolveVar(arg_var).desc.content == .err);
            }

            if (did_err) {
                try self.unifyWith(expr_var, .err, env);
            } else {
                const constraint_fn_var = try self.mkMethodCallConstraint(
                    receiver_var,
                    arg_vars,
                    expr_var,
                    method_call.method_name,
                    env,
                    method_call.method_name_region,
                    expr_idx,
                );
                try self.cir.store.replaceExprWithDispatchCall(
                    expr_idx,
                    method_call.receiver,
                    method_call.method_name,
                    method_call.method_name_region,
                    method_call.args,
                    constraint_fn_var,
                    .method_call,
                );
            }
        },
        .e_dispatch_call => |method_call| {
            does_fx = try self.checkExpr(method_call.receiver, env, Expected.none()) or does_fx;
            var did_err = self.types.resolveVar(ModuleEnv.varFrom(method_call.receiver)).desc.content == .err;

            for (self.cir.store.sliceExpr(method_call.args)) |arg_expr_idx| {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(arg_expr_idx, env, Expected.none()) or does_fx;
                did_err = did_err or (self.types.resolveVar(ModuleEnv.varFrom(arg_expr_idx)).desc.content == .err);
            }

            if (did_err) {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_structural_eq => |eq| {
            does_fx = try self.checkExpr(eq.lhs, env, Expected.none()) or does_fx;
            does_fx = try self.checkExpr(eq.rhs, env, Expected.none()) or does_fx;

            const lhs_var = ModuleEnv.varFrom(eq.lhs);
            const rhs_var = ModuleEnv.varFrom(eq.rhs);
            _ = try self.unify(lhs_var, rhs_var, env);
            _ = try self.unify(try self.freshBool(env, expr_region), expr_var, env);
        },
        .e_method_eq => |eq| {
            var arg_vars_sfa = std.heap.stackFallback(@sizeOf(Var), self.gpa);
            const arg_vars_alloc = arg_vars_sfa.get();
            const arg_vars = try arg_vars_alloc.alloc(Var, 1);
            defer arg_vars_alloc.free(arg_vars);

            self.checking_call_arg = true;
            does_fx = try self.checkExpr(eq.lhs, env, Expected.none()) or does_fx;
            self.checking_call_arg = true;
            does_fx = try self.checkExpr(eq.rhs, env, Expected.none()) or does_fx;

            const lhs_var = ModuleEnv.varFrom(eq.lhs);
            arg_vars[0] = ModuleEnv.varFrom(eq.rhs);
            if (self.types.resolveVar(lhs_var).desc.content == .err or
                self.types.resolveVar(arg_vars[0]).desc.content == .err)
            {
                try self.unifyWith(expr_var, .err, env);
            } else {
                const constraint_fn_var = try self.mkMethodCallConstraint(
                    lhs_var,
                    arg_vars,
                    expr_var,
                    self.cir.idents.is_eq,
                    env,
                    expr_region,
                    expr_idx,
                );
                self.cir.store.replaceExprWithMethodEq(
                    expr_idx,
                    eq.lhs,
                    eq.rhs,
                    eq.negated,
                    constraint_fn_var,
                );
            }
        },
        .e_type_method_call => |method_call| {
            const arg_expr_idxs = self.cir.store.sliceExpr(method_call.args);
            var arg_vars_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.gpa);
            const arg_vars_alloc = arg_vars_sfa.get();
            const arg_vars = try arg_vars_alloc.alloc(Var, arg_expr_idxs.len);
            defer arg_vars_alloc.free(arg_vars);

            var did_err = false;
            for (arg_expr_idxs, 0..) |arg_expr_idx, i| {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(arg_expr_idx, env, Expected.none()) or does_fx;
                const arg_var = ModuleEnv.varFrom(arg_expr_idx);
                arg_vars[i] = arg_var;
                did_err = did_err or (self.types.resolveVar(arg_var).desc.content == .err);
            }

            if (did_err) {
                try self.unifyWith(expr_var, .err, env);
            } else {
                const type_var_alias_stmt = self.cir.store.getStatement(method_call.type_var_alias_stmt);
                const dispatcher_var = ModuleEnv.varFrom(type_var_alias_stmt.s_type_var_alias.type_var_anno);
                const constraint_fn_var = try self.mkTypeMethodCallConstraint(
                    dispatcher_var,
                    arg_vars,
                    expr_var,
                    method_call.method_name,
                    env,
                    method_call.method_name_region,
                    expr_idx,
                );
                try self.cir.store.replaceExprWithTypeDispatchCall(
                    expr_idx,
                    method_call.type_var_alias_stmt,
                    method_call.method_name,
                    method_call.method_name_region,
                    method_call.args,
                    constraint_fn_var,
                );
            }
        },
        .e_type_dispatch_call => |method_call| {
            var did_err = false;
            for (self.cir.store.sliceExpr(method_call.args)) |arg_expr_idx| {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(arg_expr_idx, env, Expected.none()) or does_fx;
                did_err = did_err or (self.types.resolveVar(ModuleEnv.varFrom(arg_expr_idx)).desc.content == .err);
            }

            if (did_err) {
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_crash => {
            try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
        },
        .e_expect_err => |expect_err| {
            // The Err payload is consumed at runtime when the enclosing expect
            // fails; this expression itself never returns, so its type is free.
            _ = try self.checkExpr(expect_err.expr, env, Expected.none());
            try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
        },
        .e_dbg => |dbg| {
            // dbg evaluates its inner expression but returns {} (like expect)
            _ = try self.checkExpr(dbg.expr, env, Expected.none());
            does_fx = false;
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_expect => |expect| {
            const expect_does_fx = try self.checkExpectBody(expect.body, env, expected, expr_region);
            if (expect_does_fx) {
                _ = try self.problems.appendProblem(self.gpa, .{ .effectful_expect = .{
                    .region = expr_region,
                } });
            }
            does_fx = expect_does_fx or does_fx;
            const body_var = ModuleEnv.varFrom(expect.body);

            const bool_var = try self.freshBool(env, expr_region);
            _ = try self.unifyInContext(bool_var, body_var, env, .expect);

            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_for => |for_expr| {
            does_fx = try self.checkIteratorForLoop(
                ModuleEnv.nodeIdxFrom(expr_idx),
                for_expr.patt,
                for_expr.expr,
                for_expr.body,
                env,
                expr_region,
            ) or does_fx;

            // Like cor, loop bodies are ordinary expressions whose final value is
            // discarded by the loop construct itself. The loop expression still
            // evaluates to {}, but the body is not required to produce {}.
            try self.unifyWith(expr_var, .{ .structure = .empty_record }, env);
        },
        .e_ellipsis => {
            try self.unifyWith(expr_var, .{ .flex = Flex.init() }, env);
        },
        .e_anno_only => |anno| {
            if (expected.annotation != null and
                can.BuiltinLowLevel.isBuiltinModule(self.cir) and
                can.BuiltinLowLevel.isIntrinsicAnnotation(self.cir, anno.ident))
            {
                // Builtin.roc has a small explicit set of compiler-owned intrinsic
                // wrappers that post-check lowering handles from checked data.
            } else {
                _ = try self.problems.appendProblem(self.gpa, .{ .annotation_only_value = .{
                    .region = if (expected.annotation) |annotation_idx|
                        self.cir.store.getAnnotationRegion(annotation_idx)
                    else
                        expr_region,
                } });
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_return => |ret| {
            does_fx = try self.checkExpr(ret.expr, env, Expected.none()) or does_fx;
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
        .e_break => {
            // Nothing to do. `break` diverges, so this expression can unify with
            // any surrounding expected type.
        },
        .e_hosted_lambda => |lambda| {
            // Record the parameter span for the end-of-check pinnable
            // collection (see `checked_lambda_params`).
            try self.checked_lambda_params.append(self.gpa, lambda.args);

            // For hosted lambda expressions, the type comes from the annotation.
            // This is similar to e_anno_only - the implementation is provided by the host.
            if (expected.annotation) |annotation_idx| {
                const annotation_var = ModuleEnv.varFrom(annotation_idx);
                if (try self.varContainsUnboxedFunctionInHostedSignature(annotation_var)) {
                    const region = self.cir.store.getAnnotationRegion(annotation_idx);
                    _ = try self.problems.appendProblem(self.gpa, .{ .hosted_unboxed_function = .{
                        .region = region,
                    } });
                }
                // The expr will be unified with the expected type below
                // expr_var is a flex var by default, so no action is need here
            } else {
                // This shouldn't happen since hosted lambdas always have annotations
                try self.unifyWith(expr_var, .err, env);
            }
        },
        .e_run_low_level => |run_ll| {
            // Check each argument expression in the run_low_level node
            for (self.cir.store.exprSlice(run_ll.args)) |arg_idx| {
                self.checking_call_arg = true;
                does_fx = try self.checkExpr(arg_idx, env, Expected.none()) or does_fx;
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
        if (try self.varContainsError(expr_var, &self.var_set)) {
            // If there was an annotation AND the expr contains errors, then unify the
            // raw expr var against the annotation
            _ = try self.unify(expr_var_raw, anno_vars.anno_var_backup, env);
        } else {
            // Otherwise, make the explicit annotation the checked root for
            // this expression. The body has already constrained the
            // annotation's backing and any underscore variables above.
            _ = try self.unify(expr_var_raw, anno_vars.anno_var, env);
        }
    }

    self.var_set.clearRetainingCapacity();
    if (mb_anno_vars == null) {
        if (try self.varContainsError(expr_var, &self.var_set)) {
            try self.erroneous_value_exprs.put(self.gpa, expr_idx, {});
        }
    }

    // Check any accumulated static dispatch constraints
    try self.checkStaticDispatchConstraints(env, false);

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

            // Boundary defaulting must see the merged cycle vars but run
            // BEFORE ranks are promoted to generalized.
            try self.defaultLiteralsAtGeneralizationBoundary(expr_var, env);

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
            // Boundary defaulting runs first: it must see ranks BEFORE they
            // are promoted to generalized.
            try self.defaultLiteralsAtGeneralizationBoundary(expr_var, env);
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

fn validateToInspectMethodTypes(self: *Self, env: *Env) Allocator.Error!void {
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < self.cir.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (!isExprNodeTag(self.cir.store.nodes.get(node_idx).tag)) continue;

        const expr_idx: CIR.Expr.Idx = @enumFromInt(raw_node_idx);
        const expr = self.cir.store.getExpr(expr_idx);
        switch (expr) {
            .e_call => |call| {
                if (!self.exprIsBuiltinStrInspect(call.func)) continue;
                const args = self.cir.store.sliceExpr(call.args);
                if (args.len != 1) continue;
                try self.validateToInspectMethodTypeForArg(
                    args[0],
                    env,
                    self.cir.store.getExprRegion(args[0]),
                );
            },
            else => {},
        }
    }
}

fn exprIsBuiltinStrInspect(self: *Self, expr_idx: CIR.Expr.Idx) bool {
    const expr = self.cir.store.getExpr(expr_idx);
    switch (expr) {
        .e_lookup_local => |lookup| {
            const ident = self.getPatternIdent(lookup.pattern_idx) orelse return false;
            return ident.eql(self.cir.idents.builtin_str_inspect);
        },
        .e_lookup_external => |ext| {
            const module_idx = self.cir.imports.getResolvedModule(ext.module_idx) orelse return false;
            if (module_idx >= self.imported_modules.len) return false;
            const other_env = self.imported_modules[module_idx];
            const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, ext.target_node_idx));
            const ident = patternIdentInModule(other_env, def_idx) orelse return false;
            return ident.eql(other_env.idents.builtin_str_inspect);
        },
        else => return false,
    }
}

fn validateToInspectMethodTypeForArg(
    self: *Self,
    arg_expr_idx: CIR.Expr.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const arg_var = ModuleEnv.varFrom(arg_expr_idx);
    const resolved = self.types.resolveVar(arg_var);

    if (self.exprIsTopLevelLookup(arg_expr_idx)) {
        self.var_set.clearRetainingCapacity();
        if (try self.varHasUnresolvedContent(arg_var, &self.var_set)) {
            try self.reportPolymorphicValueProblem(arg_var, arg_var, null);
            return;
        }
    }
    switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nominal| try self.validateNominalToInspectMethodType(arg_var, nominal, env, region),
            else => {},
        },
        .alias => |alias| try self.validateAliasToInspectMethodType(arg_var, alias, env, region),
        .flex,
        .rigid,
        .err,
        => {},
    }
}

fn exprIsTopLevelLookup(self: *Self, expr_idx: CIR.Expr.Idx) bool {
    return switch (self.cir.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| self.top_level_ptrns.contains(lookup.pattern_idx),
        else => false,
    };
}

fn validateNominalToInspectMethodType(
    self: *Self,
    arg_var: Var,
    nominal: types_mod.NominalType,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const method = try self.toInspectMethodVarForNominal(nominal, env, region) orelse return;
    try self.validateToInspectMethodVar(arg_var, method.var_, method.dispatcher_name, env, region);
}

fn validateAliasToInspectMethodType(
    self: *Self,
    arg_var: Var,
    alias: types_mod.Alias,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const method = try self.toInspectMethodVarForAlias(alias, env, region) orelse return;
    try self.validateToInspectMethodVar(arg_var, method.var_, method.dispatcher_name, env, region);
}

const ToInspectMethodVar = struct {
    var_: Var,
    dispatcher_name: Ident.Idx,
};

fn validateToInspectMethodVar(
    self: *Self,
    arg_var: Var,
    method_var: Var,
    dispatcher_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const str_var = try self.freshStr(env, region);
    const args_range = try self.types.appendVars(&.{arg_var});
    const expected_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = str_var,
        .needs_instantiation = false,
    } } }, env, region);

    const result = try self.unifyInContext(method_var, expected_fn_var, env, .{ .method_type = .{
        .constraint_var = arg_var,
        .dispatcher_name = dispatcher_name,
        .method_name = self.cir.idents.to_inspect,
    } });
    if (result.isProblem()) {
        try self.unifyWith(expected_fn_var, .err, env);
    }
}

fn toInspectMethodVarForNominal(
    self: *Self,
    nominal: types_mod.NominalType,
    env: *Env,
    region: Region,
) Allocator.Error!?ToInspectMethodVar {
    const original_env, const is_this_module = try self.methodOwnerEnv(
        nominal.origin_module,
        nominal.sourceDeclOptional(),
        nominal.originIsBuiltin(),
    );
    const method_binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
        self.cir,
        nominal.sourceDeclOptional(),
        self.cir.idents.to_inspect,
    ) orelse return null;
    return try self.methodVarFromOriginalEnv(original_env, is_this_module, method_binding.type_node_idx, nominal.ident.ident_idx, env, region);
}

fn toInspectMethodVarForAlias(
    self: *Self,
    alias: types_mod.Alias,
    env: *Env,
    region: Region,
) Allocator.Error!?ToInspectMethodVar {
    const original_env, const is_this_module = try self.methodOwnerEnv(
        alias.origin_module,
        alias.source_decl.toOptional(),
        alias.source_decl.originIsBuiltin(),
    );
    const method_binding = original_env.lookupMethodBindingFromTwoEnvsAndDeclConst(
        alias.source_decl.toOptional(),
        self.cir,
        self.cir.idents.to_inspect,
    ) orelse return null;
    return try self.methodVarFromOriginalEnv(original_env, is_this_module, method_binding.type_node_idx, alias.ident.ident_idx, env, region);
}

fn methodOwnerEnv(
    self: *Self,
    origin_module: Ident.Idx,
    source_decl: ?u32,
    origin_is_builtin: bool,
) Allocator.Error!struct { *const ModuleEnv, bool } {
    return self.ownerEnvForOriginModule(origin_module, source_decl, origin_is_builtin, "to_inspect");
}

const OwnerEnvCandidate = struct {
    env: *const ModuleEnv,
    is_this_module: bool,
};

fn ownerEnvForOriginModule(
    self: *const Self,
    origin_module: Ident.Idx,
    source_decl: ?u32,
    origin_is_builtin: bool,
    context: []const u8,
) struct { *const ModuleEnv, bool } {
    if (origin_is_builtin) {
        return self.builtinOwnerEnvForSourceDecl(source_decl, context);
    }

    const origin_text = self.cir.getIdent(origin_module);
    const owner_source_decl = nonBuiltinOwnerSourceDecl(source_decl, context, origin_text);

    var found: ?OwnerEnvCandidate = null;
    considerOwnerEnvCandidate(&found, self.cir, true, origin_text, owner_source_decl, context);
    for (self.imported_modules) |imported_env| {
        if (imported_env.module_role == .builtin) continue;
        considerOwnerEnvCandidate(&found, imported_env, false, origin_text, owner_source_decl, context);
    }
    for (self.owner_modules) |owner_env| {
        if (owner_env.module_role == .builtin) continue;
        considerOwnerEnvCandidate(&found, owner_env, false, origin_text, owner_source_decl, context);
    }

    if (found) |owner| return .{ owner.env, owner.is_this_module };

    if (self.owner_module_envs.get(origin_text)) |owner_env| {
        if (!ownerModuleEnvSourceDeclMatches(owner_env, owner_source_decl)) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "type checker invariant violated: {s} owner {s} resolved by name to an environment without source_decl={d}",
                    .{ context, origin_text, owner_source_decl },
                );
            }
            unreachable;
        }
        return .{ owner_env, false };
    }

    if (builtin.mode == .Debug) {
        std.debug.panic(
            "type checker invariant violated: unable to find module environment for {s} owner from module {s}, source_decl={d}, origin_is_builtin={}",
            .{ context, origin_text, owner_source_decl, origin_is_builtin },
        );
    }
    unreachable;
}

fn nonBuiltinOwnerSourceDecl(source_decl: ?u32, context: []const u8, origin_text: []const u8) u32 {
    if (source_decl) |raw_decl| return raw_decl;
    if (builtin.mode == .Debug) {
        std.debug.panic(
            "type checker invariant violated: {s} owner {s} has no source declaration",
            .{ context, origin_text },
        );
    }
    unreachable;
}

fn builtinOwnerEnvForSourceDecl(
    self: *const Self,
    source_decl: ?u32,
    context: []const u8,
) struct { *const ModuleEnv, bool } {
    if (source_decl == null) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "type checker invariant violated: {s} compiler-builtin owner has no source declaration",
                .{context},
            );
        }
        unreachable;
    }

    if (self.maybeBuiltinOwnerEnvForSourceDecl(source_decl.?)) |owner| return .{ owner.env, owner.is_this_module };

    if (builtin.mode == .Debug) {
        std.debug.panic(
            "type checker invariant violated: unable to find compiler-builtin module environment for {s} owner",
            .{context},
        );
    }
    unreachable;
}

fn maybeBuiltinOwnerEnvForSourceDecl(
    self: *const Self,
    source_decl: u32,
) ?OwnerEnvCandidate {
    if (self.cir.module_role == .builtin and ownerModuleEnvSourceDeclMatches(self.cir, source_decl)) {
        return .{ .env = self.cir, .is_this_module = true };
    }

    if (self.builtin_ctx.builtin_module) |builtin_env| {
        if (ownerModuleEnvSourceDeclMatches(builtin_env, source_decl)) return .{ .env = builtin_env, .is_this_module = false };
    }

    for (self.imported_modules) |imported_env| {
        if (imported_env.module_role != .builtin) continue;
        if (ownerModuleEnvSourceDeclMatches(imported_env, source_decl)) return .{ .env = imported_env, .is_this_module = false };
    }
    for (self.owner_modules) |owner_env| {
        if (owner_env.module_role != .builtin) continue;
        if (ownerModuleEnvSourceDeclMatches(owner_env, source_decl)) return .{ .env = owner_env, .is_this_module = false };
    }

    return null;
}

fn considerOwnerEnvCandidate(
    found: *?OwnerEnvCandidate,
    candidate: *const ModuleEnv,
    is_this_module: bool,
    origin_text: []const u8,
    source_decl: u32,
    context: []const u8,
) void {
    if (!ownerModuleEnvNameMatches(candidate, origin_text)) return;
    if (!ownerModuleEnvSourceDeclMatches(candidate, source_decl)) return;

    if (found.*) |existing| {
        if (existing.env == candidate) return;
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "type checker invariant violated: {s} owner {s} resolves to multiple module environments",
                .{ context, origin_text },
            );
        }
        unreachable;
    }

    found.* = .{ .env = candidate, .is_this_module = is_this_module };
}

fn ownerModuleEnvNameMatches(candidate: *const ModuleEnv, origin_text: []const u8) bool {
    if (!candidate.qualified_module_ident.isNone() and
        Ident.textEql(candidate.getIdent(candidate.qualified_module_ident), origin_text))
    {
        return true;
    }
    if (!candidate.display_module_name_idx.isNone() and
        Ident.textEql(candidate.getIdent(candidate.display_module_name_idx), origin_text))
    {
        return true;
    }
    return candidate.module_name.len > 0 and Ident.textEql(candidate.module_name, origin_text);
}

fn ownerModuleEnvSourceDeclMatches(candidate: *const ModuleEnv, source_decl: u32) bool {
    if (source_decl >= candidate.store.nodes.len()) return false;

    const node: CIR.Node.Idx = @enumFromInt(source_decl);
    switch (candidate.store.nodes.get(node).tag) {
        .statement_alias_decl, .statement_nominal_decl => {},
        else => return false,
    }
    return true;
}

fn methodVarFromOriginalEnv(
    self: *Self,
    original_env: *const ModuleEnv,
    is_this_module: bool,
    type_node_idx: anytype,
    dispatcher_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!ToInspectMethodVar {
    const def_var: Var = ModuleEnv.varFrom(type_node_idx);
    const method_var = if (is_this_module) blk: {
        if (self.types.resolveVar(def_var).desc.rank == .generalized) {
            break :blk try self.instantiateVar(def_var, env, .use_last_var);
        }
        break :blk def_var;
    } else blk: {
        const copied = try self.copyVar(def_var, original_env, region);
        break :blk try self.instantiateVar(copied, env, .{ .explicit = region });
    };
    return .{ .var_ = method_var, .dispatcher_name = dispatcher_name };
}

fn patternIdentInModule(module_env: *const ModuleEnv, def_idx: CIR.Def.Idx) ?Ident.Idx {
    const def = module_env.store.getDef(def_idx);
    const pattern = module_env.store.getPattern(def.pattern);
    return switch (pattern) {
        .assign => |assign| assign.ident,
        .as => |as_pattern| as_pattern.ident,
        else => null,
    };
}

fn isExprNodeTag(tag: CIR.Node.Tag) bool {
    return Ident.textStartsWith(@tagName(tag), "expr_");
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

fn exprAlwaysCrashes(self: *const Self, expr_idx: CIR.Expr.Idx) bool {
    return switch (self.cir.store.getExpr(expr_idx)) {
        .e_crash,
        .e_ellipsis,
        .e_expect_err,
        .e_break,
        => true,
        .e_block => |block| self.exprAlwaysCrashes(block.final_expr),
        else => false,
    };
}

fn exhaustiveBuiltinIdents(self: *const Self) exhaustive.BuiltinIdents {
    return .{
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
        .u8 = self.cir.idents.u8,
        .i8 = self.cir.idents.i8,
        .u16 = self.cir.idents.u16,
        .i16 = self.cir.idents.i16,
        .u32 = self.cir.idents.u32,
        .i32 = self.cir.idents.i32,
        .u64 = self.cir.idents.u64,
        .i64 = self.cir.idents.i64,
        .u128 = self.cir.idents.u128,
        .i128 = self.cir.idents.i128,
        .f32 = self.cir.idents.f32,
        .f64 = self.cir.idents.f64,
        .dec = self.cir.idents.dec,
    };
}

fn closeExhaustiveVars(
    self: *Self,
    result: exhaustive.CheckResult,
    env: *Env,
    region: Region,
) std.mem.Allocator.Error!void {
    for (result.ext_vars_to_close) |ext_var| {
        const empty_tu_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, region);
        _ = try self.unify(ext_var, empty_tu_var, env);
    }

    for (result.payload_vars_to_close) |payload_var| {
        try self.closePayloadVarToEmpty(payload_var);
    }
}

fn appendUniqueIdent(gpa: std.mem.Allocator, out: *std.ArrayList(Ident.Idx), ident: Ident.Idx) Allocator.Error!void {
    for (out.items) |existing| {
        if (existing.eql(ident)) return;
    }
    try out.append(gpa, ident);
}

fn collectConstructedTagsForExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    out: *std.ArrayList(Ident.Idx),
) Allocator.Error!bool {
    const expr = self.cir.store.getExpr(expr_idx);
    switch (expr) {
        .e_zero_argument_tag => |tag| {
            try appendUniqueIdent(self.gpa, out, tag.name);
            return true;
        },
        .e_tag => |tag| {
            try appendUniqueIdent(self.gpa, out, tag.name);
            return true;
        },
        .e_nominal => |nominal| {
            return self.collectConstructedTagsForExpr(nominal.backing_expr, out);
        },
        .e_nominal_external => |nominal| {
            return self.collectConstructedTagsForExpr(nominal.backing_expr, out);
        },
        .e_block => |block| {
            return self.collectConstructedTagsForExpr(block.final_expr, out);
        },
        .e_if => |if_expr| {
            const branches = self.cir.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = self.cir.store.getIfBranch(branch_idx);
                if (!try self.collectConstructedTagsForExpr(branch.body, out)) return false;
            }
            return self.collectConstructedTagsForExpr(if_expr.final_else, out);
        },
        .e_match => |match_expr| {
            const branches = self.cir.store.sliceMatchBranches(match_expr.branches);
            for (branches) |branch_idx| {
                const branch = self.cir.store.getMatchBranch(branch_idx);
                if (!try self.collectConstructedTagsForExpr(branch.value, out)) return false;
            }
            return true;
        },
        else => return false,
    }
}

fn collectAbsentCtorPayloadBlockers(
    self: *Self,
    target_var: Var,
    constructed_tags: []const Ident.Idx,
    out: *std.ArrayList(Var),
) Allocator.Error!void {
    var arena = std.heap.ArenaAllocator.init(self.gpa);
    defer arena.deinit();

    try exhaustive.collectAbsentCtorPayloadBlockersForConstructedTags(
        arena.allocator(),
        self.types,
        self.exhaustiveBuiltinIdents(),
        target_var,
        constructed_tags,
        out,
    );
}

fn payloadVarCanResolveToEmpty(content: Content) bool {
    return switch (content) {
        .flex => |flex| flex.constraints.len() == 0 and if (flex.name) |name| name.attributes.ignored else true,
        .rigid => |rigid| rigid.constraints.len() == 0 and rigid.name.attributes.ignored,
        else => false,
    };
}

fn closePayloadVarToEmpty(
    self: *Self,
    payload_var: Var,
) Allocator.Error!void {
    const empty_content = Content{ .structure = .empty_tag_union };
    const resolved = self.types.resolveVar(payload_var);
    if (payloadVarCanResolveToEmpty(resolved.desc.content)) {
        try self.types.setVarContent(resolved.var_, empty_content);
    }
}

fn collectKnownEmptyPayloadVarsForExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target_var: Var,
    out: *std.ArrayList(Var),
) Allocator.Error!bool {
    var constructed_tags: std.ArrayList(Ident.Idx) = .empty;
    defer constructed_tags.deinit(self.gpa);

    const known = try self.collectConstructedTagsForExpr(expr_idx, &constructed_tags);
    if (!known) return false;
    if (constructed_tags.items.len == 0) return true;

    try self.collectAbsentCtorPayloadBlockers(target_var, constructed_tags.items, out);
    return true;
}

fn closeAbsentConstructedPayloadVars(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target_var: Var,
) Allocator.Error!void {
    var payload_vars_to_close: std.ArrayList(Var) = .empty;
    defer payload_vars_to_close.deinit(self.gpa);

    _ = try self.collectKnownEmptyPayloadVarsForExpr(expr_idx, target_var, &payload_vars_to_close);

    for (payload_vars_to_close.items) |payload_var| {
        try self.closePayloadVarToEmpty(payload_var);
    }
}

fn pendingExhaustivenessMode(self: *const Self) ProblemStore.PendingStaticExhaustivenessMode {
    return if (self.empirical_exhaustiveness_depth == 0) .static else .empirical;
}

fn checkDestructureExhaustiveness(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    value_expr_idx: CIR.Expr.Idx,
    value_var: Var,
    env: *Env,
    region: Region,
) std.mem.Allocator.Error!void {
    if (!self.patternNeedsExhaustiveness(pattern_idx)) return;

    var known_empty_payload_vars: std.ArrayList(Var) = .empty;
    defer known_empty_payload_vars.deinit(self.gpa);
    const value_constructors_known = try self.collectKnownEmptyPayloadVarsForExpr(value_expr_idx, value_var, &known_empty_payload_vars);

    const result = exhaustive.checkDestructure(
        self.cir.gpa,
        self.types,
        &self.cir.store,
        self.exhaustiveBuiltinIdents(),
        pattern_idx,
        value_var,
        known_empty_payload_vars.items,
        value_constructors_known,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TypeError => return,
    };
    defer result.deinit(self.cir.gpa);

    try self.closeExhaustiveVars(result, env, region);

    if (!result.is_exhaustive) {
        const value_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, value_var);
        const missing_patterns_start = self.problems.missing_patterns_backing.items.len;

        for (result.missing_patterns) |pattern| {
            const idx = try exhaustive.formatPattern(
                &self.problems.extra_strings_backing,
                &self.cir.common.idents,
                &self.cir.common.strings,
                pattern,
            );
            try self.problems.missing_patterns_backing.append(idx);
        }

        const missing_patterns_range = problem.MissingPatternsRange{
            .start = missing_patterns_start,
            .count = self.problems.missing_patterns_backing.items.len - missing_patterns_start,
        };

        try self.problems.appendPendingStaticExhaustiveness(self.gpa, .destructure, self.pendingExhaustivenessMode(), region, .{ .non_exhaustive_destructure = .{
            .pattern = pattern_idx,
            .value_snapshot = value_snapshot,
            .missing_patterns = missing_patterns_range,
        } });
    }
}

// stmts //

const BlockStatementsResult = struct {
    does_fx: bool,
    diverges: bool,
};

/// Given a slice of stmts, type check each one
/// Returns whether any statement has effects and whether the block diverges (return/crash)
fn checkBlockStatements(self: *Self, statements: CIR.Statement.Span, env: *Env, _: Region) std.mem.Allocator.Error!BlockStatementsResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var does_fx = false;
    var diverges = false;
    for (0..statements.span.len) |stmt_offset| {
        const stmt_idx = self.cir.store.statementAt(statements, stmt_offset);
        const stmt = self.cir.store.getStatement(stmt_idx);
        const stmt_var = ModuleEnv.varFrom(stmt_idx);
        const stmt_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(stmt_idx));

        try self.setVarRank(stmt_var, env);

        switch (stmt) {
            .s_decl => |decl_stmt| {
                const decl_expr_var: Var = ModuleEnv.varFrom(decl_stmt.expr);
                const decl_pattern_var: Var = ModuleEnv.varFrom(decl_stmt.pattern);

                const decl_pattern_ctx: PatternCtx = if (self.patternNeedsExhaustiveness(decl_stmt.pattern)) .match_branch else .bound;

                // Check the pattern
                try self.checkPattern(decl_stmt.pattern, decl_pattern_ctx, env);

                // Extract function name from the pattern (for better error messages)
                const saved_func_name = self.enclosing_func_name;
                self.enclosing_func_name = self.getPatternIdent(decl_stmt.pattern);
                defer self.enclosing_func_name = saved_func_name;

                // Check the annotation, if it exists
                const expectation = blk: {
                    if (decl_stmt.anno) |annotation_idx| {
                        break :blk Expected.fromAnnotation(annotation_idx);
                    } else {
                        break :blk Expected.none();
                    }
                };

                // Register function defs as "currently processing" so recursive
                // references in their own body defer unification (see the local
                // recursion branch in `e_lookup_local`). Only function defs can
                // legitimately be self-recursive; value defs (`x = x`) keep their
                // existing diagnostics. Snapshot the pending-recursive-ref stack
                // so we only validate the references recorded while checking THIS
                // def's body.
                const decl_is_fn = isFunctionDef(&self.cir.store, self.cir.store.getExpr(decl_stmt.expr));
                const local_recursive_refs_top = self.local_recursive_refs.items.len;
                if (decl_is_fn) {
                    try self.local_processing_ptrns.put(self.gpa, decl_stmt.pattern, .{
                        .def_name = self.getPatternIdent(decl_stmt.pattern),
                        .pattern_idx = decl_stmt.pattern,
                    });
                }

                self.checking_binding_rhs = true;
                does_fx = try self.checkExpr(decl_stmt.expr, env, expectation) or does_fx;
                if (decl_stmt.anno == null and self.erroneous_value_exprs.contains(decl_stmt.expr)) {
                    try self.erroneous_value_patterns.put(self.gpa, decl_stmt.pattern, {});
                }
                try self.closeAbsentConstructedPayloadVars(decl_stmt.expr, decl_expr_var);

                const decl_pattern_result = try self.unify(decl_pattern_var, decl_expr_var, env);
                _ = try self.unify(stmt_var, decl_pattern_var, env);

                if (decl_pattern_result.isOk()) {
                    try self.checkDestructureExhaustiveness(decl_stmt.pattern, decl_stmt.expr, decl_expr_var, env, stmt_region);
                }

                if (decl_is_fn) {
                    // The def's lambda has generalized (inside checkExpr) and the
                    // pattern var now carries the generalized function type, so it
                    // is safe to validate the recursive references it recorded.
                    try self.validateLocalRecursiveRefs(env, local_recursive_refs_top);
                    _ = self.local_processing_ptrns.remove(decl_stmt.pattern);
                }
            },
            .s_var => |var_stmt| {
                const var_pattern_ctx: PatternCtx = if (self.patternNeedsExhaustiveness(var_stmt.pattern_idx)) .match_branch else .bound;

                // Check the pattern
                try self.checkPattern(var_stmt.pattern_idx, var_pattern_ctx, env);
                const var_pattern_var: Var = ModuleEnv.varFrom(var_stmt.pattern_idx);

                // Check the annotation, if it exists
                const expectation = blk: {
                    if (var_stmt.anno) |annotation_idx| {
                        // Return the expectation
                        break :blk Expected.fromAnnotation(annotation_idx);
                    } else {
                        break :blk Expected.none();
                    }
                };

                does_fx = try self.checkExpr(var_stmt.expr, env, expectation) or does_fx;
                if (var_stmt.anno == null and self.erroneous_value_exprs.contains(var_stmt.expr)) {
                    try self.erroneous_value_patterns.put(self.gpa, var_stmt.pattern_idx, {});
                }
                const var_expr: Var = ModuleEnv.varFrom(var_stmt.expr);
                try self.closeAbsentConstructedPayloadVars(var_stmt.expr, var_expr);

                const var_pattern_result = try self.unify(var_pattern_var, var_expr, env);
                _ = try self.unify(stmt_var, var_expr, env);

                if (var_pattern_result.isOk()) {
                    try self.checkDestructureExhaustiveness(var_stmt.pattern_idx, var_stmt.expr, var_expr, env, stmt_region);
                }
            },
            .s_reassign => |reassign| {
                // Reassignment patterns can mix existing mutable binders with
                // fresh local binders, e.g. `(word, $index) = pair`.
                // The pattern occurrence itself must therefore always be
                // checked here so its structural type and any fresh binders are
                // established explicitly before we unify it with the RHS.
                const reassign_pattern_ctx: PatternCtx = if (self.patternNeedsExhaustiveness(reassign.pattern_idx)) .match_branch else .bound;
                try self.checkPattern(reassign.pattern_idx, reassign_pattern_ctx, env);

                const reassign_pattern_var: Var = ModuleEnv.varFrom(reassign.pattern_idx);

                does_fx = try self.checkExpr(reassign.expr, env, Expected.none()) or does_fx;
                const reassign_expr_var: Var = ModuleEnv.varFrom(reassign.expr);
                try self.closeAbsentConstructedPayloadVars(reassign.expr, reassign_expr_var);

                // Unify the pattern with the expression
                //
                // TODO: if there's a mismatch here, the region of the error is
                // the original assignment pattern, not the reassignment region
                const reassign_pattern_result = try self.unify(reassign_pattern_var, reassign_expr_var, env);

                _ = try self.unify(stmt_var, reassign_expr_var, env);

                if (reassign_pattern_result.isOk()) {
                    try self.checkDestructureExhaustiveness(reassign.pattern_idx, reassign.expr, reassign_expr_var, env, stmt_region);
                }
            },
            .s_for => |for_stmt| {
                const for_region = self.cir.store.getStatementRegion(stmt_idx);
                does_fx = try self.checkIteratorForLoop(
                    ModuleEnv.nodeIdxFrom(stmt_idx),
                    for_stmt.patt,
                    for_stmt.expr,
                    for_stmt.body,
                    env,
                    for_region,
                ) or does_fx;
                const empty_rec = try self.freshFromContent(.{ .structure = .empty_record }, env, for_region);
                _ = try self.unify(stmt_var, empty_rec, env);
            },
            .s_while => |while_stmt| {
                // Check the condition
                // while $count < 10 {
                //       ^^^^^^^^^^^
                does_fx = try self.checkExpr(while_stmt.cond, env, Expected.none()) or does_fx;
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
                does_fx = try self.checkExpr(while_stmt.body, env, Expected.none()) or does_fx;
                const empty_rec = try self.freshFromContent(.{ .structure = .empty_record }, env, cond_region);
                _ = try self.unify(stmt_var, empty_rec, env);
            },
            .s_expr => |expr| {
                does_fx = try self.checkExpr(expr.expr, env, Expected.none()) or does_fx;
                const expr_var: Var = ModuleEnv.varFrom(expr.expr);

                // Statements must evaluate to {}. Add a constraint to unify with empty record.
                // If unification fails, we get a nice type mismatch error explaining that
                // statement expressions must return {}.
                const empty_rec = try self.freshFromContent(.{ .structure = .empty_record }, env, stmt_region);
                _ = try self.unifyInContext(empty_rec, expr_var, env, .statement_value);

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_dbg => |expr| {
                does_fx = try self.checkExpr(expr.expr, env, Expected.none()) or does_fx;
                const expr_var: Var = ModuleEnv.varFrom(expr.expr);

                _ = try self.unify(stmt_var, expr_var, env);
            },
            .s_expect => |expr_stmt| {
                const expect_does_fx = try self.checkExpectBody(expr_stmt.body, env, Expected.none(), stmt_region);
                if (expect_does_fx) {
                    _ = try self.problems.appendProblem(self.gpa, .{ .effectful_expect = .{
                        .region = stmt_region,
                    } });
                }
                does_fx = expect_does_fx or does_fx;
                const body_var: Var = ModuleEnv.varFrom(expr_stmt.body);

                const bool_var = try self.freshBool(env, stmt_region);
                _ = try self.unifyInContext(bool_var, body_var, env, .expect);

                try self.unifyWith(stmt_var, .{ .structure = .empty_record }, env);
            },
            .s_crash => {
                try self.unifyWith(stmt_var, .{ .flex = Flex.init() }, env);
                diverges = true;
            },
            .s_return => |ret| {
                // Type check the return expression
                does_fx = try self.checkExpr(ret.expr, env, Expected.none()) or does_fx;
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
            .s_nominal_decl, .s_alias_decl, .s_type_anno => {
                // Local type declarations are preprocessed before type checking.
                // Avoid re-processing them inside block statements to prevent
                // duplicate unifications and spurious type mismatches.
            },
            .s_import => {
                // Imports are only valid at the top level; canonicalization reports the error.
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
            .s_break => {
                diverges = true;
            },
        }
    }
    return .{ .does_fx = does_fx, .diverges = diverges };
}

fn enforceRecordBuilderMap2Return(
    self: *Self,
    func: Func,
    env: *Env,
    call_expr: CIR.Expr.Idx,
    func_name: ?Ident.Idx,
) std.mem.Allocator.Error!unifier.Result {
    if (func.args.len() != 3) return .ok;

    const return_payload_var = self.singleParameterWrapperPayload(func.ret) orelse return .ok;
    const mapper_var = self.types.getVarAt(func.args, 2);
    const mapper_func = self.functionTypeFromVar(mapper_var) orelse return .ok;

    return try self.unifyInContext(return_payload_var, mapper_func.ret, env, .{ .fn_call_arg = .{
        .fn_name = func_name,
        .call_expr = call_expr,
        .arg_index = 2,
        .num_args = 3,
        .arg_var = mapper_var,
    } });
}

fn singleParameterWrapperPayload(self: *Self, wrapper_var: Var) ?Var {
    const resolved = self.types.resolveVar(wrapper_var);
    return switch (resolved.desc.content) {
        .alias => |alias| blk: {
            const args = self.types.sliceAliasArgs(alias);
            if (args.len != 1) break :blk null;
            break :blk args[0];
        },
        .structure => |flat| switch (flat) {
            .nominal_type => |nominal| blk: {
                const args = self.types.sliceNominalArgs(nominal);
                if (args.len != 1) break :blk null;
                break :blk args[0];
            },
            else => null,
        },
        else => null,
    };
}

fn functionTypeFromVar(self: *Self, fn_var: Var) ?Func {
    var current = fn_var;
    var guard = types_mod.debug.IterationGuard.init("functionTypeFromVar");
    while (true) {
        guard.tick();
        const resolved = self.types.resolveVar(current);
        switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| return func,
                else => return null,
            },
            .alias => |alias| current = self.types.getAliasBackingVar(alias),
            else => return null,
        }
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
    expected: Expected,
) std.mem.Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();
    const expected_branch_ret = expected.branch_result;

    // Fresh accumulator for the meet of all compatible branch bodies. Branches
    // fold into this instead of into the shared `expected_ret`, which is unified
    // with the whole expr (and hence the accumulator) exactly once, at the end.
    const branch_acc: ?Var = if (expected_branch_ret != null) try self.fresh(env, expr_region) else null;

    const branches = self.cir.store.sliceIfBranches(if_.branches);

    // Should never be 0
    std.debug.assert(branches.len > 0);

    // Get the first branch
    const first_branch_idx = branches[0];
    const first_branch = self.cir.store.getIfBranch(first_branch_idx);

    // Check the condition of the 1st branch
    var does_fx = try self.checkExpr(first_branch.cond, env, Expected.none());
    const first_cond_var: Var = ModuleEnv.varFrom(first_branch.cond);
    const bool_var = try self.freshBool(env, expr_region);
    _ = try self.unifyInContext(bool_var, first_cond_var, env, .if_condition);

    // Then we check the 1st branch's body
    does_fx = try self.checkExpr(first_branch.body, env, expected.forBranchBody()) or does_fx;

    if (expected_branch_ret) |expected_ret| {
        const branch_ctx = problem.Context{ .if_branch = .{
            .branch_index = 0,
            .num_branches = @intCast(branches.len + 1),
            .is_else = false,
            .parent_if_expr = if_expr_idx,
            .last_if_branch = first_branch_idx,
        } };
        try self.checkBranchBodyAgainstExpected(first_branch.body, expected_ret, branch_acc.?, branch_ctx, env);
    }

    // The 1st branch's body is the type all other branches must match (when no expected type)
    const branch_var = @as(Var, ModuleEnv.varFrom(first_branch.body));

    // Total number of branches (including final else)
    const num_branches: u32 = @intCast(branches.len + 1);

    var last_if_branch = first_branch_idx;
    for (branches[1..], 1..) |branch_idx, cur_index| {
        const branch = self.cir.store.getIfBranch(branch_idx);

        // Check the branches condition
        does_fx = try self.checkExpr(branch.cond, env, Expected.none()) or does_fx;
        const cond_var: Var = ModuleEnv.varFrom(branch.cond);
        const branch_bool_var = try self.freshBool(env, expr_region);
        _ = try self.unifyInContext(branch_bool_var, cond_var, env, .if_condition);

        // Check the branch body
        does_fx = try self.checkExpr(branch.body, env, expected.forBranchBody()) or does_fx;

        // Check against expected return type BEFORE pairwise unification
        if (expected_branch_ret) |expected_ret| {
            const branch_ctx = problem.Context{ .if_branch = .{
                .branch_index = @intCast(cur_index),
                .num_branches = num_branches,
                .is_else = false,
                .parent_if_expr = if_expr_idx,
                .last_if_branch = last_if_branch,
            } };
            try self.checkBranchBodyAgainstExpected(branch.body, expected_ret, branch_acc.?, branch_ctx, env);
        } else {
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

                    does_fx = try self.checkExpr(remaining_branch.cond, env, Expected.none()) or does_fx;
                    const remaining_cond_var: Var = ModuleEnv.varFrom(remaining_branch.cond);

                    const fresh_bool = try self.freshBool(env, expr_region);
                    _ = try self.unifyInContext(fresh_bool, remaining_cond_var, env, .if_condition);

                    does_fx = try self.checkExpr(remaining_branch.body, env, expected.forBranchBody()) or does_fx;
                    try self.unifyWith(ModuleEnv.varFrom(remaining_branch.body), .err, env);
                }

                // Break to avoid cascading errors
                break;
            }
        }

        last_if_branch = branch_idx;
    }

    // Check the final else
    does_fx = try self.checkExpr(if_.final_else, env, expected.forBranchBody()) or does_fx;

    // Check final else against expected return type before pairwise unification
    if (expected_branch_ret) |expected_ret| {
        const branch_ctx = problem.Context{ .if_branch = .{
            .branch_index = num_branches - 1,
            .num_branches = num_branches,
            .is_else = true,
            .parent_if_expr = if_expr_idx,
            .last_if_branch = last_if_branch,
        } };
        try self.checkBranchBodyAgainstExpected(if_.final_else, expected_ret, branch_acc.?, branch_ctx, env);
        const if_expr_var: Var = ModuleEnv.varFrom(if_expr_idx);
        // Tie the whole expr to the accumulated branch meet, then to the shared
        // expected return type. This is the ONLY place `expected_ret` is merged
        // for the if-expr, and it runs in the load-bearing `(expr, expected)`
        // operand order so `expected_ret` survives as the union-find root (see
        // `store.union_`): flipping it ties recursive type parameters off to
        // duplicate rigids of the same name, which then fail to unify.
        _ = try self.unify(if_expr_var, branch_acc.?, env);
        _ = try self.unify(if_expr_var, expected_ret, env);
    } else {
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
    }

    return does_fx;
}

// match //

/// Check the types for a match expr
fn checkMatchExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    env: *Env,
    match: CIR.Expr.Match,
    expected: Expected,
) Allocator.Error!bool {
    const trace = tracy.trace(@src());
    defer trace.end();

    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));
    const expected_branch_ret = expected.branch_result;

    // Fresh accumulator for the meet of all compatible branch bodies. Branches
    // fold into this instead of into the shared `expected_ret`, which is unified
    // with the whole expr (and hence the accumulator) exactly once, at the end.
    const branch_acc: ?Var = if (expected_branch_ret != null) try self.fresh(env, expr_region) else null;

    // Check the match's condition
    var does_fx = try self.checkExpr(match.cond, env, Expected.none());
    const cond_var = ModuleEnv.varFrom(match.cond);
    const cond_always_crashes = self.exprAlwaysCrashes(match.cond);
    if (!match.is_try_suffix) {
        try self.closeAbsentConstructedPayloadVars(match.cond, cond_var);
    }

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

    // Check each of the first branch's patterns and unify it with the
    // condition type. (A failed unify poisons cond_var to .err, so subsequent
    // pattern unifications short-circuit rather than cascading.)
    for (first_branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
        const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
        try self.checkPattern(branch_ptrn.pattern, .match_branch, env);

        if (!cond_always_crashes) {
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
    }

    if (try self.unifyMatchAltPatternBindings(first_branch_ptrn_idxs, 0, @intCast(match.branches.span.len), expr_idx, env)) {
        had_type_error = true;
    }

    // Check guard if present
    if (first_branch.guard) |guard_idx| {
        does_fx = try self.checkExpr(guard_idx, env, Expected.none()) or does_fx;
        const guard_var = ModuleEnv.varFrom(guard_idx);
        const guard_bool_var = try self.freshBool(env, expr_region);
        _ = try self.unifyInContext(guard_bool_var, guard_var, env, .if_condition);
    }

    // Check the first branch's value, then use that at the branch_var
    does_fx = try self.checkExpr(first_branch.value, env, expected.forBranchBody()) or does_fx;

    // Check first branch body against expected return type
    if (expected_branch_ret) |expected_ret| {
        const branch_ctx = problem.Context{ .match_branch = .{
            .branch_index = 0,
            .num_branches = @intCast(match.branches.span.len),
            .match_expr = expr_idx,
        } };
        try self.checkBranchBodyAgainstExpected(first_branch.value, expected_ret, branch_acc.?, branch_ctx, env);
    }

    const val_var = ModuleEnv.varFrom(first_branch.value);

    // Then iterate over the rest of the branches
    for (branch_idxs[1..], 1..) |branch_idx, branch_cur_index| {
        const branch = self.cir.store.getMatchBranch(branch_idx);

        // First, check the patterns of this branch (skip if invalid try to avoid confusing errors)
        const branch_ptrn_idxs = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
        for (branch_ptrn_idxs, 0..) |branch_ptrn_idx, cur_ptrn_index| {
            // Check the pattern's sub types
            const branch_ptrn = self.cir.store.getMatchBranchPattern(branch_ptrn_idx);
            try self.checkPattern(branch_ptrn.pattern, .match_branch, env);

            // Check the pattern against the cond
            if (!cond_always_crashes) {
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
        }

        if (try self.unifyMatchAltPatternBindings(branch_ptrn_idxs, @intCast(branch_cur_index), @intCast(match.branches.span.len), expr_idx, env)) {
            had_type_error = true;
        }

        // Check guard if present
        if (branch.guard) |guard_idx| {
            does_fx = try self.checkExpr(guard_idx, env, Expected.none()) or does_fx;
            const guard_var = ModuleEnv.varFrom(guard_idx);
            const branch_guard_bool_var = try self.freshBool(env, expr_region);
            _ = try self.unifyInContext(branch_guard_bool_var, guard_var, env, .if_condition);
        }

        // Then, check the body
        does_fx = try self.checkExpr(branch.value, env, expected.forBranchBody()) or does_fx;

        // Check branch body against expected return type BEFORE pairwise unification.
        // Pairwise unification poisons ALL connected vars via union-find on failure,
        // making it impossible to distinguish correct from incorrect branches afterward.
        if (expected_branch_ret) |expected_ret| {
            const branch_ctx = problem.Context{ .match_branch = .{
                .branch_index = @intCast(branch_cur_index),
                .num_branches = @intCast(match.branches.span.len),
                .match_expr = expr_idx,
            } };
            try self.checkBranchBodyAgainstExpected(branch.value, expected_ret, branch_acc.?, branch_ctx, env);
        } else {
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
                        try self.checkPattern(other_branch_ptrn.pattern, .match_branch, env);

                        // Check the pattern against the cond
                        if (!cond_always_crashes) {
                            const other_branch_ptrn_var = ModuleEnv.varFrom(other_branch_ptrn.pattern);
                            _ = try self.unifyInContext(cond_var, other_branch_ptrn_var, env, .{ .match_pattern = .{
                                .branch_index = @intCast(other_branch_cur_index),
                                .pattern_index = @intCast(other_cur_ptrn_index),
                                .num_branches = @intCast(match.branches.span.len),
                                .num_patterns = @intCast(other_branch_ptrn_idxs.len),
                                .match_expr = expr_idx,
                            } });
                        }
                    }

                    // Then check the other branch's exprs
                    does_fx = try self.checkExpr(other_branch.value, env, expected.forBranchBody()) or does_fx;
                    try self.unifyWith(ModuleEnv.varFrom(other_branch.value), .err, env);
                }

                // Then stop type checking for this branch
                break;
            }
        }
    }

    // Unify the root expr with the match value
    if (expected_branch_ret) |expected_ret| {
        // Tie the whole expr to the accumulated branch meet, then to the shared
        // expected return type. This is the ONLY place `expected_ret` is merged
        // for the match expr, and it runs in the load-bearing `(expr, expected)`
        // operand order so `expected_ret` survives as the union-find root (see
        // `store.union_`): flipping it ties recursive type parameters off to
        // duplicate rigids of the same name, which then fail to unify.
        _ = try self.unify(ModuleEnv.varFrom(expr_idx), branch_acc.?, env);
        _ = try self.unify(ModuleEnv.varFrom(expr_idx), expected_ret, env);
    } else {
        _ = try self.unify(ModuleEnv.varFrom(expr_idx), val_var, env);
    }

    // Perform exhaustiveness and redundancy checking
    // Only do this if there were no type errors - type errors can lead to invalid types
    // that confuse the exhaustiveness checker
    // Also skip if the condition type is an error type (can happen with complex inference)
    // Also skip if we already reported an invalid try operator error
    // Also skip if the condition explicitly diverges; no pattern is observed at runtime.
    const resolved_cond = self.types.resolveVar(cond_var);
    const cond_is_error = resolved_cond.desc.content == .err;

    if (!match.skip_exhaustiveness and !had_type_error and !cond_is_error and !has_invalid_try and !cond_always_crashes) {
        const match_region = self.getRegionAt(@enumFromInt(@intFromEnum(expr_idx)));

        var known_empty_payload_vars: std.ArrayList(Var) = .empty;
        defer known_empty_payload_vars.deinit(self.gpa);
        const cond_constructors_known = try self.collectKnownEmptyPayloadVarsForExpr(match.cond, cond_var, &known_empty_payload_vars);

        const result = exhaustive.checkMatch(
            self.cir.gpa,
            self.types,
            &self.cir.store,
            self.exhaustiveBuiltinIdents(),
            match.branches,
            cond_var,
            match_region,
            known_empty_payload_vars.items,
            cond_constructors_known,
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

        try self.closeExhaustiveVars(result, env, match_region);

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

            try self.problems.appendPendingStaticExhaustiveness(self.gpa, .match, self.pendingExhaustivenessMode(), match_region, .{ .non_exhaustive_match = .{
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
    const does_fx = try self.checkExpr(unary.expr, env, Expected.none());

    // Get the not method + ret var
    // Here, we assert that the arg and ret of `not` are same type
    const not_method_name = self.cir.idents.negate;
    const not_arg_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const not_ret_var = not_arg_var;

    // Create the not static dispatch function on the not_arg + not_ret
    // This function attaches the dispatch fn to the not_arg
    try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region, expr_idx);

    // The result type is the operand type (the desugaring is `a -> a`).
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
    const does_fx = try self.checkExpr(unary.expr, env, Expected.none());

    // Get the not method + ret var
    // Here, we assert that the arg and ret of `not` are same type
    const not_method_name = self.cir.idents.not;
    const not_arg_var = @as(Var, ModuleEnv.varFrom(unary.expr));
    const not_ret_var = not_arg_var;

    // Create the not static dispatch function on the not_arg + not_ret
    // This function attaches the dispatch fn to the not_arg
    try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region, expr_idx);

    // The result type is the operand type (the desugaring is `a -> a`).
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
    does_fx = try self.checkExpr(binop.lhs, env, Expected.none()) or does_fx;
    does_fx = try self.checkExpr(binop.rhs, env, Expected.none()) or does_fx;

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

            const lhs_is_numeric = self.isBuiltinNumericNominal(lhs_var);
            const rhs_is_numeric = self.isBuiltinNumericNominal(rhs_var);
            const lhs_is_from_numeral = self.varLiteralKind(lhs_var) == .numeral;
            const rhs_is_from_numeral = self.varLiteralKind(rhs_var) == .numeral;

            if (lhs_is_from_numeral and try self.reportDefinitelyInvalidNumericBinopOperand(rhs_var, expr_var, expr_idx, binop.op, .rhs, env, expr_region)) {
                return does_fx;
            }
            if (rhs_is_from_numeral and try self.reportDefinitelyInvalidNumericBinopOperand(lhs_var, expr_var, expr_idx, binop.op, .lhs, env, expr_region)) {
                return does_fx;
            }

            // Eagerly unify the operands, but ONLY when the dispatcher's
            // arithmetic method is known homogeneous (`T, T -> T`), so one
            // operand FORCES the other — propagation of an implied fact, not a
            // guess. Two gates, each forced:
            //
            //   1. `lhs_is_numeric`: the lhs is the dispatcher and a concrete
            //      builtin numeric nominal. Builtin arithmetic methods are
            //      homogeneous (e.g. `Dec.plus : Dec, Dec -> Dec`), so the rhs
            //      MUST be the same type.
            //   2. `rhs_is_numeric and lhs_is_from_numeral`: the lhs is an open
            //      numeric LITERAL. Every numeral defaulting candidate is a
            //      builtin numeric nominal with homogeneous arithmetic, so
            //      whatever the literal resolves to forces operand equality — the
            //      concrete builtin rhs pins it now.
            //
            // A concrete builtin rhs with a NON-literal, non-numeric lhs must NOT
            // be unified eagerly: the lhs/dispatcher could be a user nominal with
            // a heterogeneous method (e.g. `times : Duration, I64 -> Duration`
            // used as `my_duration * count` with `count : I64`). That case falls
            // through to the dispatch constraint below
            // (`lhs.times : (lhs, rhs) -> lhs`), which resolves against the user
            // method correctly.
            if (lhs_is_numeric or (rhs_is_numeric and lhs_is_from_numeral)) {
                const target = if (lhs_is_numeric) lhs_var else rhs_var;
                const other = if (lhs_is_numeric) rhs_var else lhs_var;
                const arg_unify_result = try self.unify(target, other, env);
                if (!arg_unify_result.isOk()) {
                    try self.unifyWith(expr_var, .err, env);
                    return does_fx;
                }
            }

            if (try self.reportMissingNominalMethodForBinop(lhs_var, rhs_var, expr_var, method_name, env, expr_region)) {
                return does_fx;
            }

            // Arithmetic binops are homogeneous in the RETURN only: the result
            // type IS the receiver type. Do NOT also unify lhs with rhs — user
            // nominal methods may be heterogeneous (e.g.
            // `times : Duration, I64 -> Duration`). Result-position context
            // (`x : I64 = 1 + 2`) flows in here; argument-position context
            // reaches an open literal via constraint-aware defaulting.
            const ret_var = lhs_var;

            // Create the binop static dispatch function: lhs.method(rhs) -> lhs
            try self.mkBinopConstraint(
                lhs_var,
                rhs_var,
                ret_var,
                method_name,
                false,
                env,
                expr_region,
                expr_idx,
            );

            // Set the expression to redirect to the return type
            _ = try self.unify(expr_var, ret_var, env);
        },
        .lt, .gt, .le, .ge => {
            const method_name, const ret_var =
                switch (binop.op) {
                    .lt => .{ self.cir.idents.is_lt, try self.freshBool(env, expr_region) },
                    .gt => .{ self.cir.idents.is_gt, try self.freshBool(env, expr_region) },
                    .le => .{ self.cir.idents.is_lte, try self.freshBool(env, expr_region) },
                    .ge => .{ self.cir.idents.is_gte, try self.freshBool(env, expr_region) },
                    else => unreachable,
                };

            if (try self.reportMissingNominalMethodForBinop(lhs_var, rhs_var, expr_var, method_name, env, expr_region)) {
                return does_fx;
            }

            // For comparison binops, lhs and rhs must have the same type.
            const arg_unify_result = try self.unify(lhs_var, rhs_var, env);

            if (!arg_unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            const arg_var = rhs_var;

            // Create the binop constraint with unified arg type
            try self.mkBinopConstraint(
                arg_var,
                arg_var,
                ret_var,
                method_name,
                false,
                env,
                expr_region,
                expr_idx,
            );

            // Set the expression to redirect to the return type
            _ = try self.unify(expr_var, ret_var, env);
        },
        .eq => {
            if (try self.reportMissingNominalMethodForBinop(lhs_var, rhs_var, expr_var, self.cir.idents.is_eq, env, expr_region)) {
                return does_fx;
            }

            const arg_unify_result = try self.unify(lhs_var, rhs_var, env);
            if (!arg_unify_result.isOk()) {
                try self.unifyWith(expr_var, .err, env);
                return does_fx;
            }

            const eq_ret_var = try self.freshBool(env, expr_region);
            try self.mkBinopConstraint(
                rhs_var,
                rhs_var,
                eq_ret_var,
                self.cir.idents.is_eq,
                false,
                env,
                expr_region,
                expr_idx,
            );

            _ = try self.unify(expr_var, eq_ret_var, env);
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

            if (try self.reportMissingNominalMethodForBinop(lhs_var, rhs_var, expr_var, self.cir.idents.is_eq, env, expr_region)) {
                return does_fx;
            }

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
            try self.mkBinopConstraint(eq_arg_var, eq_arg_var, eq_ret_var, eq_method_name, true, env, expr_region, expr_idx);

            // Get the not method + ret var
            const not_method_name = self.cir.idents.not;
            const not_arg_var = eq_ret_var;
            const not_ret_var = eq_ret_var;

            // Create the not static dispatch function on the not_arg + not_ret
            // This function attaches the dispatch fn to the not_arg
            try self.mkUnaryOp(not_arg_var, not_ret_var, not_method_name, env, expr_region, null);

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

fn reportDefinitelyInvalidNumericBinopOperand(
    self: *Self,
    operand_var: Var,
    expr_var: Var,
    expr_idx: CIR.Expr.Idx,
    op: CIR.Expr.Binop.Op,
    side: enum { lhs, rhs },
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    if (!self.varIsDefinitelyNonNumericOperand(operand_var)) return false;

    const expected_num = try self.freshFromContent(try self.mkBuiltinNumberTypeContentFromKind(.dec, env), env, region);
    const binop_ctx: problem.Context.BinopContext.Binop = switch (op) {
        .add => .plus,
        .sub => .minus,
        .mul => .times,
        .div => .div,
        .rem => .div,
        .div_trunc => .div,
        else => return false,
    };

    const ctx: problem.Context = switch (side) {
        .lhs => .{ .binop_lhs = .{ .operator = binop_ctx, .binop_expr = expr_idx } },
        .rhs => .{ .binop_rhs = .{ .operator = binop_ctx, .binop_expr = expr_idx } },
    };

    _ = try self.unifyInContext(expected_num, operand_var, env, ctx);
    try self.unifyWith(expr_var, .err, env);
    return true;
}

fn varIsDefinitelyNonNumericOperand(self: *Self, var_: Var) bool {
    var current = var_;
    while (true) {
        const resolved = self.types.resolveVar(current);
        return switch (resolved.desc.content) {
            .alias => |alias| {
                current = self.types.getAliasBackingVar(alias);
                continue;
            },
            .structure => |flat| switch (flat) {
                .record,
                .record_unbound,
                .tuple,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => true,
                .nominal_type => false,
            },
            .err, .flex, .rigid => false,
        };
    }
}

fn reportMissingNominalMethodForBinop(
    self: *Self,
    lhs_var: Var,
    rhs_var: Var,
    expr_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    const resolved_lhs = self.types.resolveVar(lhs_var);
    if (resolved_lhs.desc.content == .err) return false;

    if (resolved_lhs.desc.content != .structure or resolved_lhs.desc.content.structure != .nominal_type) {
        return false;
    }

    const nominal_type = resolved_lhs.desc.content.structure.nominal_type;
    if (method_name.eql(self.cir.idents.is_eq) and try self.nominalSupportsImplicitIsEq(nominal_type)) {
        return false;
    }
    const original_env = self.getNominalOriginEnv(nominal_type);
    if (original_env.lookupMethodBindingFromEnvAndDeclConst(self.cir, nominal_type.sourceDeclOptional(), method_name) == null) {
        try self.reportMissingNominalMethodForBinopConstraint(lhs_var, rhs_var, expr_var, method_name, env, region);
        return true;
    }

    return false;
}

fn reportMissingNominalMethodForBinopConstraint(
    self: *Self,
    lhs_var: Var,
    rhs_var: Var,
    expr_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });
    const ret_var = try self.fresh(env, region);
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .{ .desugared_binop = .{ .negated = false } },
    };

    try self.reportConstraintError(lhs_var, constraint, .{ .missing_method = .nominal }, env, false);
    try self.unifyWith(expr_var, .err, env);
}

fn getNominalOriginEnv(self: *Self, nominal_type: types_mod.NominalType) *const ModuleEnv {
    const original_env, _ = self.ownerEnvForOriginModule(
        nominal_type.origin_module,
        nominal_type.sourceDeclOptional(),
        nominal_type.originIsBuiltin(),
        "nominal type",
    );
    return original_env;
}

// binop + unary op exprs //

/// Create a static dispatch fn like: `lhs, rhs -> ret` and assert the
/// constraint to the lhs (receiver) var.
/// Constrain a literal pattern's type to support equality, since matching the
/// pattern compares the scrutinee against the literal's converted value.
fn mkPatternLiteralEqConstraint(
    self: *Self,
    pattern_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const ret_var = try self.freshBool(env, region);
    try self.mkBinopConstraint(pattern_var, pattern_var, ret_var, self.cir.idents.is_eq, false, env, region, null);
}

fn mkBinopConstraint(
    self: *Self,
    lhs_var: Var,
    rhs_var: Var,
    ret_var: Var,
    method_name: Ident.Idx,
    negated: bool,
    env: *Env,
    region: Region,
    binop_expr_idx: ?CIR.Expr.Idx,
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
        .origin = .{ .desugared_binop = .{ .negated = negated } },
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});
    if (binop_expr_idx) |expr_idx| {
        try self.constraint_expr_by_fn_var.put(constraint_fn_var, expr_idx);
        try self.publishBinopDispatchExpr(expr_idx, method_name, region, constraint_fn_var);
    }

    // Create a constrained flex and unify it with the lhs (receiver)
    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, lhs_var, env);
}

fn publishBinopDispatchExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
    region: Region,
    constraint_fn_var: Var,
) Allocator.Error!void {
    switch (self.cir.store.getExpr(expr_idx)) {
        .e_binop => |binop| switch (binop.op) {
            .eq, .ne => {
                self.cir.store.replaceExprWithMethodEq(
                    expr_idx,
                    binop.lhs,
                    binop.rhs,
                    binop.op == .ne,
                    constraint_fn_var,
                );
            },
            .@"and", .@"or" => {},
            else => {
                const args = try self.cir.store.appendExprSpan(&.{binop.rhs});
                try self.cir.store.replaceExprWithDispatchCall(
                    expr_idx,
                    binop.lhs,
                    method_name,
                    region,
                    args,
                    constraint_fn_var,
                    .{ .binop = binop.op },
                );
            },
        },
        else => {},
    }
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
    unary_expr_idx: ?CIR.Expr.Idx,
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
    if (unary_expr_idx) |expr_idx| {
        try self.constraint_expr_by_fn_var.put(constraint_fn_var, expr_idx);
        try self.publishUnaryDispatchExpr(expr_idx, method_name, region, constraint_fn_var);
    }

    // Create a constrained flex and unify it with the arg
    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, arg_var, env);
}

fn publishUnaryDispatchExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
    region: Region,
    constraint_fn_var: Var,
) Allocator.Error!void {
    const receiver: CIR.Expr.Idx, const surface_origin: CIR.Expr.SurfaceOrigin = switch (self.cir.store.getExpr(expr_idx)) {
        .e_unary_minus => |unary| .{ unary.expr, .unary_minus },
        .e_unary_not => |unary| .{ unary.expr, .unary_not },
        else => return,
    };
    try self.cir.store.replaceExprWithDispatchCall(
        expr_idx,
        receiver,
        method_name,
        region,
        .{ .span = base.DataSpan.empty() },
        constraint_fn_var,
        surface_origin,
    );
}

fn checkIteratorForLoop(
    self: *Self,
    loop_node: CIR.Node.Idx,
    pattern: CIR.Pattern.Idx,
    iterable: CIR.Expr.Idx,
    body: CIR.Expr.Idx,
    env: *Env,
    loop_region: Region,
) Allocator.Error!bool {
    var does_fx = false;

    try self.checkPattern(pattern, .for_, env);
    const item_var: Var = ModuleEnv.varFrom(pattern);

    does_fx = try self.checkExpr(iterable, env, Expected.none()) or does_fx;
    const iterable_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(iterable));
    const iterable_var: Var = ModuleEnv.varFrom(iterable);

    const iterator_var = try self.mkIterVar(item_var, env, iterable_region);
    const iter_method = try @constCast(self.cir).insertIdent(base.Ident.for_text("iter"));
    const iter_fn_var = try self.mkSyntheticReceiverDispatchConstraint(
        iterable_var,
        &.{},
        iterator_var,
        iter_method,
        env,
        iterable_region,
    );

    const step_var = try self.freshFromContent(try self.mkIteratorStepContent(item_var, iterator_var, env), env, loop_region);
    const next_method = try @constCast(self.cir).insertIdent(base.Ident.for_text("next"));
    const next_fn_var = try self.mkSyntheticReceiverDispatchConstraint(
        iterator_var,
        &.{},
        step_var,
        next_method,
        env,
        loop_region,
    );

    try self.cir.recordForLoopDispatchPlan(loop_node, ModuleEnv.nodeIdxFrom(pattern), ModuleEnv.nodeIdxFrom(iterable), iter_fn_var, next_fn_var);

    does_fx = try self.checkExpr(body, env, Expected.none()) or does_fx;
    return does_fx;
}

fn mkMethodCallConstraint(
    self: *Self,
    receiver_var: Var,
    arg_vars: []const Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
    method_expr_idx: CIR.Expr.Idx,
) Allocator.Error!Var {
    return try self.mkReceiverDispatchConstraint(
        receiver_var,
        arg_vars,
        ret_var,
        method_name,
        env,
        region,
        method_expr_idx,
    );
}

fn mkSyntheticReceiverDispatchConstraint(
    self: *Self,
    receiver_var: Var,
    arg_vars: []const Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!Var {
    return try self.mkReceiverDispatchConstraint(
        receiver_var,
        arg_vars,
        ret_var,
        method_name,
        env,
        region,
        null,
    );
}

fn mkReceiverDispatchConstraint(
    self: *Self,
    receiver_var: Var,
    arg_vars: []const Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
    method_expr_idx: ?CIR.Expr.Idx,
) Allocator.Error!Var {
    var all_args_sfa = std.heap.stackFallback(16 * @sizeOf(Var), self.gpa);
    const all_args_alloc = all_args_sfa.get();
    const all_args = try all_args_alloc.alloc(Var, arg_vars.len + 1);
    defer all_args_alloc.free(all_args);
    all_args[0] = receiver_var;
    @memcpy(all_args[1..], arg_vars);

    const args_range = try self.types.appendVars(all_args);
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .method_call,
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});
    if (method_expr_idx) |expr_idx| {
        try self.constraint_expr_by_fn_var.put(constraint_fn_var, expr_idx);
    }
    if (self.current_expect_region) |expect_region| {
        try self.expect_region_by_constraint_fn_var.put(constraint_fn_var, expect_region);
    }

    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, receiver_var, env);
    return constraint_fn_var;
}

fn mkTypeMethodCallConstraint(
    self: *Self,
    dispatcher_var: Var,
    arg_vars: []const Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
    method_expr_idx: CIR.Expr.Idx,
) Allocator.Error!Var {
    const args_range = try self.types.appendVars(arg_vars);
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .method_call,
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});
    try self.constraint_expr_by_fn_var.put(constraint_fn_var, method_expr_idx);
    if (self.current_expect_region) |expect_region| {
        try self.expect_region_by_constraint_fn_var.put(constraint_fn_var, expect_region);
    }

    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, dispatcher_var, env);
    return constraint_fn_var;
}

fn mkInterpolationConstraint(
    self: *Self,
    dispatcher_var: Var,
    arg_vars: []const Var,
    ret_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!Var {
    const args_range = try self.types.appendVars(arg_vars);
    const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
        .args = args_range,
        .ret = ret_var,
        .needs_instantiation = false,
    } } }, env, region);

    const constraint = StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = constraint_fn_var,
        .origin = .{ .from_literal = .interpolation },
    };
    const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});
    try self.constraint_expr_by_fn_var.put(constraint_fn_var, expr_idx);
    if (self.current_expect_region) |expect_region| {
        try self.expect_region_by_constraint_fn_var.put(constraint_fn_var, expect_region);
    }

    const constrained_var = try self.freshFromContent(
        .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
        env,
        region,
    );

    _ = try self.unify(constrained_var, dispatcher_var, env);

    // Shares the open-literal worklist with numerals and quoted strings: the
    // literal-defaulting and constraint-compatibility sweeps handle every kind.
    try self.open_literal_vars.append(self.gpa, constrained_var);

    return constraint_fn_var;
}

fn rewriteImplicitEqMethodCallAsStructuralEq(
    self: *Self,
    constraint: StaticDispatchConstraint,
) void {
    const expr_idx = self.constraint_expr_by_fn_var.get(constraint.fn_var) orelse return;

    switch (self.cir.store.getExpr(expr_idx)) {
        .e_method_call => |method_call| {
            const args = self.cir.store.sliceExpr(method_call.args);
            if (args.len != 1) {
                std.debug.panic(
                    "type checker invariant violated: structural equality method call expected exactly one argument, found {d}",
                    .{args.len},
                );
            }

            self.cir.store.replaceExprWithStructuralEq(expr_idx, method_call.receiver, args[0], constraint.origin.binopNegated());
        },
        .e_dispatch_call => |method_call| {
            if (method_call.constraint_fn_var != constraint.fn_var) return;
            const args = self.cir.store.sliceExpr(method_call.args);
            if (args.len != 1) {
                std.debug.panic(
                    "type checker invariant violated: structural equality method call expected exactly one argument, found {d}",
                    .{args.len},
                );
            }

            self.cir.store.replaceExprWithStructuralEq(expr_idx, method_call.receiver, args[0], constraint.origin.binopNegated());
        },
        .e_method_eq => |eq| {
            if (eq.constraint_fn_var != constraint.fn_var) return;
            self.cir.store.replaceExprWithStructuralEq(expr_idx, eq.lhs, eq.rhs, constraint.origin.binopNegated());
        },
        .e_binop => |binop| {
            if (binop.op != .eq and binop.op != .ne) return;
            self.cir.store.replaceExprWithStructuralEq(expr_idx, binop.lhs, binop.rhs, constraint.origin.binopNegated());
        },
        .e_structural_eq => |eq| {
            self.cir.store.replaceExprWithStructuralEq(expr_idx, eq.lhs, eq.rhs, constraint.origin.binopNegated());
        },
        else => {},
    }
}

fn rewriteEqBinopAsMethodEq(self: *Self, constraint: StaticDispatchConstraint) void {
    if (constraint.origin != .desugared_binop) return;
    const expr_idx = self.constraint_expr_by_fn_var.get(constraint.fn_var) orelse return;
    switch (self.cir.store.getExpr(expr_idx)) {
        .e_binop => |binop| {
            if (binop.op != .eq and binop.op != .ne) return;
            self.cir.store.replaceExprWithMethodEq(
                expr_idx,
                binop.lhs,
                binop.rhs,
                constraint.origin.binopNegated(),
                constraint.fn_var,
            );
        },
        .e_method_eq => |eq| {
            self.cir.store.replaceExprWithMethodEq(
                expr_idx,
                eq.lhs,
                eq.rhs,
                constraint.origin.binopNegated(),
                constraint.fn_var,
            );
        },
        else => {},
    }
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
    node_idx: u32,
) std.mem.Allocator.Error!?ExternalType {
    const trace = tracy.trace(@src());
    defer trace.end();

    const module_idx = self.cir.imports.getResolvedModule(import_idx) orelse {
        // Canonicalization should already have reported an import error.
        // Keep checking by treating this type reference as unresolved.
        return null;
    };
    if (module_idx < self.imported_modules.len) {
        const other_module_env = self.imported_modules[module_idx];

        // The idx of the expression in the other module
        const target_node_idx = @as(CIR.Node.Idx, @enumFromInt(node_idx));

        // Check if we've already copied this import
        const cache_key = ImportCacheKey{
            .resolved_module_idx = module_idx,
            .node_idx = target_node_idx,
        };

        const copied_var = if (self.import_cache.get(cache_key)) |cached_var|
            // Reuse the previously copied type.
            cached_var
        else blk: {
            // First time importing this type - copy it and cache the result
            const imported_var: Var = @as(Var, @enumFromInt(@intFromEnum(target_node_idx)));

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
        // Stale/invalid import mapping; surface as unresolved and continue
        // producing type errors instead of crashing.
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

            // Register a copied open literal on the worklist so this module's
            // defaulting passes find it without a whole-store scan — the same
            // bookkeeping the other literal-creation sites do.
            const fresh_content = self.types.resolveVar(fresh_var).desc.content;
            if (fresh_content == .flex) {
                for (self.types.sliceStaticDispatchConstraints(fresh_content.flex.constraints)) |c| {
                    if (c.origin == .from_literal) {
                        try self.open_literal_vars.append(self.gpa, fresh_var);
                        break;
                    }
                }
            }
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
            .problem, .mismatch => {
                // Unification failed - the constructor is incompatible with the nominal type
                // Context is already set by unifyInContext
                // (`.mismatch` is unreachable here — this call uses the poison_to_err
                // wrapper, which only returns `.ok`/`.problem` — grouped for exhaustiveness.)
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
        try self.checkStaticDispatchConstraints(env, false);
    }
}

fn poisonRecursiveNonFunctionProcessingDef(
    self: *Self,
    processing_def: DefProcessed,
    use_expr: ?CIR.Expr.Idx,
    env: *Env,
) Allocator.Error!void {
    const def = self.cir.store.getDef(processing_def.def_idx);
    const diagnostic_idx = if (processing_def.def_name) |ident|
        try self.cir.addDiagnostic(.{ .circular_value_definition = .{
            .ident = ident,
            .region = self.cir.store.getPatternRegion(def.pattern),
        } })
    else
        try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
            .region = self.cir.store.getExprRegion(def.expr),
        } });

    if (self.cir.store.getExpr(def.expr) != .e_runtime_error) {
        self.cir.store.replaceExprWithRuntimeError(def.expr, diagnostic_idx);
    }
    try self.erroneous_value_exprs.put(self.gpa, def.expr, {});
    try self.erroneous_value_patterns.put(self.gpa, def.pattern, {});

    if (use_expr) |expr_idx| {
        if (self.cir.store.getExpr(expr_idx) != .e_runtime_error) {
            self.cir.store.replaceExprWithRuntimeError(expr_idx, diagnostic_idx);
        }
        try self.erroneous_value_exprs.put(self.gpa, expr_idx, {});
        try self.unifyWith(ModuleEnv.varFrom(expr_idx), .err, env);
    }
}

fn freshRecursiveMethodPlaceholder(
    self: *Self,
    processing_def: DefProcessed,
    def: CIR.Def,
    env: *Env,
    region: Region,
) Allocator.Error!Var {
    const method_var = try self.fresh(env, region);

    // Match e_lookup_local recursion semantics: a different annotated def in the
    // same recursive group is instantiated per use-site at cycle validation time,
    // while self references and unannotated group members stay monomorphic.
    const is_cross_reference = (def.annotation != null) and
        if (self.current_processing_def) |current_def|
            current_def != processing_def.def_idx
        else
            false;

    const constraint_expected = if (is_cross_reference)
        ModuleEnv.varFrom(def.expr)
    else
        ModuleEnv.varFrom(def.pattern);

    _ = try self.constraints.append(self.gpa, Constraint{ .eql = .{
        .expected = constraint_expected,
        .actual = method_var,
        .ctx = .{ .recursive_def = .{ .def_name = processing_def.def_name } },
        .is_cross_reference = is_cross_reference,
    } });

    return method_var;
}

fn poisonErroneousValueUses(self: *Self) Allocator.Error!void {
    for (self.value_lookup_tracking.items) |entry| {
        const pattern_var = ModuleEnv.varFrom(entry.pattern_idx);
        if (!self.erroneous_value_patterns.contains(entry.pattern_idx) and
            self.types.resolveVar(pattern_var).desc.content != .err)
        {
            continue;
        }

        if (self.cir.store.getExpr(entry.expr_idx) == .e_runtime_error) continue;

        const ident = self.getPatternIdent(entry.pattern_idx) orelse continue;
        const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_use = .{
            .ident = ident,
            .region = self.cir.store.getExprRegion(entry.expr_idx),
        } });
        self.cir.store.replaceExprWithRuntimeError(entry.expr_idx, diagnostic_idx);
    }
}

fn poisonErroneousValueExprs(self: *Self) Allocator.Error!void {
    var iter = self.erroneous_value_exprs.keyIterator();
    while (iter.next()) |expr_idx| {
        if (self.cir.store.getExpr(expr_idx.*) == .e_runtime_error) continue;
        const diagnostic_idx = try self.cir.addDiagnostic(.{ .erroneous_value_expr = .{
            .region = self.cir.store.getExprRegion(expr_idx.*),
        } });
        self.cir.store.replaceExprWithRuntimeError(expr_idx.*, diagnostic_idx);
    }
}

fn resolveNumericLiteralsFromContext(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    // An open literal resolves only through direct unification (annotations,
    // signatures, the binop return contract) or through defaulting — never by
    // guessing a type from a concrete peer (the removed "peer resolution").
    // This still processes dispatch constraints deferred since the last pass.
    try self.checkAllConstraints(env);
}

/// A speculative-probe scope. Brackets type-store speculation together with the
/// Check-level append-only buffers a probe also grows (`regions`,
/// `instantiation_dispatchers`, `open_literal_vars`), so the probe sites don't
/// repeat the bookkeeping. Begin before invoking the unifier; `rollback`
/// discards everything the probe did to the store and those buffers. (The store's
/// own child lists — vars, tags, etc. — are handled inside
/// begin/rollbackToSavepoint.)
const Probe = struct {
    check: *Self,
    savepoint: types_mod.Store.Savepoint,
    regions_len: usize,
    instantiation_dispatchers_len: usize,
    open_literal_vars_len: usize,

    fn rollback(self: *Probe) void {
        self.check.types.rollbackToSavepoint(&self.savepoint);
        self.check.regions.items.shrinkRetainingCapacity(self.regions_len);
        // Per-instantiation dispatchers recorded during the probe reference fresh
        // receiver vars that were just rolled back with the type store.
        self.check.instantiation_dispatchers.shrinkRetainingCapacity(self.instantiation_dispatchers_len);
        // Likewise, open literals registered during the probe (by in-probe
        // instantiation) reference vars the savepoint rollback just discarded.
        self.check.open_literal_vars.shrinkRetainingCapacity(self.open_literal_vars_len);
    }

    /// Close the probe scope KEEPING everything it did: the type-store
    /// speculation is committed and the appended regions / per-instantiation
    /// dispatchers stay (they describe vars that now survive). Only `CommitProbe`
    /// commits; pure-predicate probes always roll back.
    fn commit(self: *Probe) void {
        self.check.types.commitSavepoint(&self.savepoint);
    }
};

fn beginProbe(self: *Self) std.mem.Allocator.Error!Probe {
    const regions_len = self.regions.items.items.len;
    const instantiation_dispatchers_len = self.instantiation_dispatchers.items.len;
    const open_literal_vars_len = self.open_literal_vars.items.len;
    return .{
        .check = self,
        .regions_len = regions_len,
        .instantiation_dispatchers_len = instantiation_dispatchers_len,
        .open_literal_vars_len = open_literal_vars_len,
        .savepoint = try self.types.createSavepoint(),
    };
}

/// A speculative scope whose SUCCESS is committed in place instead of being
/// rolled back and redone. Unlike a plain `Probe` (whose unifications run against
/// throwaway problem/snapshot stores precisely because they never survive), a
/// commit-probe runs its unifications through the REAL `unify` wrapper — full
/// bookkeeping: fresh vars ranked into the caller env's var pool, regions
/// stamped, deferred dispatch constraints copied out, mismatch problems recorded
/// with snapshots. On failure it must therefore also rewind what that bookkeeping
/// grew:
///   - problems / snapshots recorded by failed in-probe unifications (the
///     store savepoint already un-poisons the `.err`-merged vars themselves;
///     this drops the reports, restoring the throwaway-store behavior);
///   - the caller env's var-pool rank lists (entries for vars the savepoint
///     rollback just discarded would dangle into the generalizer);
///   - the caller env's deferred dispatch constraints (their receivers and
///     constraint ranges were rolled back with the type store).
const CommitProbe = struct {
    check: *Self,
    env: *Env,
    probe: Probe,
    problems_len: usize,
    extra_strings_len: usize,
    missing_patterns_len: usize,
    snapshots_mark: SnapshotStore.Mark,
    deferred_constraints_len: usize,

    fn rollback(self: *CommitProbe) void {
        std.debug.assert(self.check.commit_probe_active);
        self.check.commit_probe_active = false;
        self.probe.rollback();
        // `truncate` rewinds only the problem entries; the string/pattern backing
        // arrays are assumed untouched by probe paths (only exhaustiveness
        // checking writes them, and it never runs inside a probe). Assert that so
        // a future probe path that grows them fails loudly instead of leaking
        // entries past the rollback.
        std.debug.assert(self.check.problems.extra_strings_backing.items.len == self.extra_strings_len);
        std.debug.assert(self.check.problems.missing_patterns_backing.items.len == self.missing_patterns_len);
        self.check.problems.truncate(self.problems_len);
        self.check.snapshots.truncateToMark(self.snapshots_mark);
        self.env.deferred_static_dispatch_constraints.items.shrinkRetainingCapacity(self.deferred_constraints_len);
        for (self.check.probe_var_pool_lens.items, 0..) |pool_len, rank_idx| {
            self.env.var_pool.shrinkRank(@enumFromInt(rank_idx), pool_len);
        }
    }

    fn commit(self: *CommitProbe) void {
        std.debug.assert(self.check.commit_probe_active);
        self.check.commit_probe_active = false;
        self.probe.commit();
    }
};

fn beginCommitProbe(self: *Self, env: *Env) std.mem.Allocator.Error!CommitProbe {
    // Commit-probes must not nest: a nested `beginCommitProbe` would clobber the
    // shared `probe_var_pool_lens` buffer the outer probe's rollback depends on.
    std.debug.assert(!self.commit_probe_active);
    self.commit_probe_active = true;
    // Capture the var pool's per-rank lengths (through the current rank): real
    // unification can rank a fresh var BELOW the current rank (it takes the min of
    // the unified descriptors), so capturing only the current rank's list would
    // miss entries.
    self.probe_var_pool_lens.clearRetainingCapacity();
    const rank_count = @intFromEnum(env.rank()) + 1;
    try self.probe_var_pool_lens.ensureTotalCapacity(self.gpa, rank_count);
    for (0..rank_count) |rank_idx| {
        const rank: Rank = @enumFromInt(rank_idx);
        self.probe_var_pool_lens.appendAssumeCapacity(env.var_pool.getVarsForRank(rank).len);
    }
    return .{
        .check = self,
        .env = env,
        .problems_len = self.problems.len(),
        .extra_strings_len = self.problems.extra_strings_backing.items.len,
        .missing_patterns_len = self.problems.missing_patterns_backing.items.len,
        .snapshots_mark = self.snapshots.mark(),
        .deferred_constraints_len = env.deferred_static_dispatch_constraints.items.items.len,
        .probe = try self.beginProbe(),
    };
}

fn probeUnifyWithoutRecordingProblems(
    self: *Self,
    expected: Var,
    actual: Var,
) Allocator.Error!bool {
    var probe_problems = try ProblemStore.initCapacity(self.gpa, 1);
    defer probe_problems.deinit(self.gpa);

    var probe_snapshots = try SnapshotStore.initCapacity(self.gpa, 8);
    defer probe_snapshots.deinit();

    // Probe against throwaway problem/snapshot stores so a mismatch here is
    // neither recorded nor poisoned — only the ok/not-ok answer matters.
    var env = self.unifyEnv();
    env.problems = &probe_problems;
    env.snapshots = &probe_snapshots;
    const result = try unifier.unify(&env, expected, actual, .{});

    return result.isOk();
}

/// Finalize still-open literal defaults at end of module checking, via the
/// shared interference-component machinery (`runLiteralDefaultingRounds`) over a
/// snapshot of the open-literal worklist.
///
/// HARD REQUIREMENT: top-level def order must never affect whether a program
/// type-checks or which types are inferred. The worklist (`open_literal_vars`) is
/// registered in check order, which IS def order, so the rounds must never let
/// worklist position influence a commit's outcome — see the ORDER-INDEPENDENCE
/// contract on `runLiteralDefaultingRounds`.
///
/// Already-resolved and generalized (let-polymorphic) literals are skipped to
/// preserve let-polymorphism; entries appended mid-finalize by in-probe
/// instantiation are intentionally outside the snapshot, exactly as before.
fn finalizeLiteralDefaults(self: *Self, env: *Env) std.mem.Allocator.Error!void {
    try self.runLiteralDefaultingRounds(env, .{ .finalize = .{
        .literal_count = self.open_literal_vars.items.len,
    } });
}

/// The candidate universe `runLiteralDefaultingRounds` gathers from — the only
/// structural difference between module finalize and a def's generalization
/// boundary. Both lengths are SNAPSHOTS taken before the first round: committing
/// a default appends fresh vars (default candidates, in-probe instantiations) to
/// the same worklist / pool entry, and those must never be rescanned by a later
/// round.
const LiteralDefaultUniverse = union(enum) {
    /// `finalizeLiteralDefaults`: the first `literal_count` entries of the
    /// open-literal worklist; a candidate is still flex, not generalized
    /// (let-polymorphism preserved), with `from_literal` provenance.
    finalize: struct { literal_count: usize },
    /// `defaultLiteralsAtGeneralizationBoundary`: the first `pool_len` entries of
    /// the var-pool rank the boundary's generalize call will promote; a candidate
    /// is still flex AT that rank with `from_literal` provenance AND not
    /// signature-reachable (`boundary_reachable_vars`, populated by the caller for
    /// the current boundary before the rounds run) — signature-reachable literals
    /// must stay open so generalization quantifies them. The pool slice is
    /// re-fetched every round (commits can grow and reallocate the pool), but only
    /// the snapshotted prefix is scanned. Boundary commits additionally carry the
    /// per-literal LITERAL DEFAULTED leak warning (see
    /// `boundaryWarningBeforeCommit`).
    boundary: struct { rank: Rank, pool_len: usize },
};

/// Default every still-open literal in `universe` — ORDER-INDEPENDENT BY
/// CONSTRUCTION. Shared core of `finalizeLiteralDefaults` and
/// `defaultLiteralsAtGeneralizationBoundary`.
///
/// A commit-one-by-one implementation (in worklist or var-pool order) is unsound
/// here: a literal's first-satisfier probe can "satisfy" a constraint by
/// speculatively pinning a still-open PEER literal (or a flex var another open
/// literal's constraints also reach) without consulting that peer's own
/// constraints — so whichever literal came first won, and reordering defs (or
/// statements within a def) flipped programs between accept and reject.
///
/// Structure: rounds, each over the universe snapshot taken at entry.
///
///   1. GATHER the still-open literal roots from the universe (per-universe
///      filters documented on `LiteralDefaultUniverse`); deduped by union-find
///      root.
///   2. PARTITION them into interference components. A literal with
///      non-`from_literal` constraints (a DRIVER) owns a footprint: every
///      still-flex root reachable from those constraints' fn signatures
///      (`collectReachableVars`, which recurses through nested constraint
///      signatures, so the footprint is transitively closed). A literal with only
///      `from_literal` provenance (a PASSIVE) has footprint {itself}. Two
///      literals share a component iff their footprints intersect. Only
///      still-flex roots interfere: a commit pins exactly the flex vars its
///      unifications reach, all inside the committing literal's footprint — so
///      disjoint components cannot observe each other's commits, and
///      concrete/rigid/err vars cannot be re-pinned by anyone.
///   3. COMMIT each component:
///        - lone passive: canonical head default (Dec), as before;
///        - single driver: the existing first-satisfier probe
///          (`commitLiteralDefault`). Its in-probe pinning of the component's
///          passives is now sound: every var it can pin lies inside this
///          component, so no OTHER pending literal's constraints are speculated
///          away;
///        - several drivers: `commitLiteralGroupDefault` — one shared candidate
///          for the whole group, the first candidate satisfying ALL the drivers'
///          constraints simultaneously.
///      Passives inside a driver's component are NOT committed this round: their
///      type is the drivers' business (the probe or the cascade pins them). If
///      the resolved method signatures leave one open, it becomes a lone passive
///      next round and defaults canonically then.
///   4. CASCADE the deferred dispatches the commits unblocked, to fixpoint, so
///      pins propagate across multi-hop dispatch chains before the next round
///      re-gathers. One pass can be too few — a constraint re-defers while its
///      receiver is flex AT VISIT TIME, so a dispatch pinned by a later commit in
///      the same pass waits for the next one. Each pass consumes every resolved
///      receiver, so the loop terminates (runaway recursion capped by
///      `max_deferred_dispatch_iterations`). `is_numeric_default_pass = true`:
///      any receiver that became concrete since the previous pass did so because
///      we just defaulted it, so error reporting must treat it as
///      defaulted-from-a-literal.
///
/// ORDER-INDEPENDENCE: the gathered SET of literal roots, each literal's
/// constraint SET, and therefore the component partition depend only on the
/// program's constraint graph, never on worklist/pool order. Commits of distinct
/// components touch disjoint flex vars (their footprints), so component commit
/// order is unobservable. A multi-driver group assigns the same candidate to
/// every driver before checking anything, then verifies the conjunction of all
/// the drivers' constraints; a conjunction of unifications succeeds iff the
/// equation set is simultaneously unifiable, independent of equation order — so
/// driver enumeration order is unobservable too. The single-driver probe consults
/// only that driver's own constraint set, in canonical candidate order — no order
/// input there either.
///
/// TERMINATION: rounds iterate a universe snapshot of fixed length, and an open
/// literal can only go flex -> resolved (probe rollbacks restore exactly the
/// pre-probe state). Every round with a non-empty gather commits at least one
/// literal: every component contains a lone passive or at least one driver, all
/// committed, and a commit always resolves the var away from flex (candidate on
/// success, the head default — possibly merging to `.err` — otherwise). Passives
/// deferred behind drivers become lone passives once their drivers resolve, so
/// rounds <= snapshot length + 1. The per-round cascade terminates by
/// `checkStaticDispatchConstraints`' own consume-or-re-defer contract (see
/// `anyDeferredDispatchReceiverResolved`), with runaway recursion capped by
/// `max_deferred_dispatch_iterations`.
fn runLiteralDefaultingRounds(self: *Self, env: *Env, universe: LiteralDefaultUniverse) std.mem.Allocator.Error!void {
    // Round-scoped scratch. Components are tiny in practice (most literals are
    // lone passives or single drivers), so plain local containers suffice.
    var open_roots: std.ArrayListUnmanaged(Var) = .empty;
    defer open_roots.deinit(self.gpa);
    var seen_roots = std.AutoHashMap(Var, void).init(self.gpa);
    defer seen_roots.deinit();
    var component_parent: std.ArrayListUnmanaged(usize) = .empty;
    defer component_parent.deinit(self.gpa);
    var is_driver: std.ArrayListUnmanaged(bool) = .empty;
    defer is_driver.deinit(self.gpa);
    var footprint_owner = std.AutoHashMap(Var, usize).init(self.gpa);
    defer footprint_owner.deinit();
    var footprint = std.AutoHashMap(Var, void).init(self.gpa);
    defer footprint.deinit();
    var group_drivers: std.ArrayListUnmanaged(Var) = .empty;
    defer group_drivers.deinit(self.gpa);
    var group_warnings: std.ArrayListUnmanaged(PendingBoundaryWarning) = .empty;
    defer group_warnings.deinit(self.gpa);

    while (true) {
        // --- 1. Gather the still-open literal roots (deduped). ---
        open_roots.clearRetainingCapacity();
        seen_roots.clearRetainingCapacity();
        switch (universe) {
            .finalize => |finalize| {
                for (self.open_literal_vars.items[0..finalize.literal_count]) |literal_var| {
                    const resolved = self.types.resolveVar(literal_var);
                    if (resolved.desc.content != .flex) continue;
                    if (resolved.desc.rank == .generalized) continue;
                    if (self.varLiteralKind(resolved.var_) == null) continue;
                    const gop = try seen_roots.getOrPut(resolved.var_);
                    if (gop.found_existing) continue;
                    try open_roots.append(self.gpa, resolved.var_);
                }
            },
            .boundary => |boundary| {
                // Re-fetch the pool slice each round (commits may have grown and
                // reallocated it) but scan only the snapshotted prefix: defaulting
                // appends fresh default vars to this same pool entry, and those
                // must not be rescanned.
                for (env.var_pool.getVarsForRank(boundary.rank)[0..boundary.pool_len]) |pool_var| {
                    const resolved = self.types.resolveVar(pool_var);
                    if (resolved.desc.content != .flex) continue;
                    if (resolved.desc.rank != boundary.rank) continue;
                    if (self.varLiteralKind(resolved.var_) == null) continue;
                    // A signature-reachable literal stays open (let-polymorphic) and
                    // generalizes — quotes and interpolations included (interpolations
                    // are publishable while open thanks to the checked-interpolation
                    // reachability handling). A zero-arg thunk's return is
                    // signature-reachable too, so its literal also stays open; the
                    // thunk simply generalizes as a polymorphic function (see
                    // `reportPolymorphicTopLevelValues`).
                    if (self.boundary_reachable_vars.contains(resolved.var_)) continue;
                    const gop = try seen_roots.getOrPut(resolved.var_);
                    if (gop.found_existing) continue;
                    try open_roots.append(self.gpa, resolved.var_);
                }
            },
        }
        if (open_roots.items.len == 0) return;

        // --- 1b. Default unambiguous quote literals before ambiguous numerals. ---
        // A quote literal has exactly one possible type (Str), so committing it is
        // never a guess. A numeral's first-satisfier probe IS a guess: when several
        // candidate types satisfy its constraints it commits the head candidate
        // (Dec). Resolving every open quote first — and letting step 4's cascade
        // propagate the dispatches that unblocks — lets a producer dispatch whose
        // receiver is a string (e.g. `s.count_utf8_bytes() : Str -> U64`) pin a
        // numeral's type BEFORE the numeral would otherwise be guessed. Without
        // this, `intermediate.count_utf8_bytes() > 0` commits `0` to Dec (the head
        // `is_gt` candidate, since the `count_utf8_bytes` dispatch is still latent
        // behind the receiver's own open quote literal) and then conflicts when the
        // Str-keyed dispatch resolves to U64.
        //
        // Numerals are only held back while a quote is actually open; once the
        // quote round(s) have cascaded, numerals default through the unchanged
        // component machinery below. The quote/numeral split is a pure function of
        // the gathered kinds, so order-independence is preserved, and termination
        // holds because every non-empty round still commits at least one literal (a
        // quote if any is open, else a numeral).
        {
            var any_quote = false;
            for (open_roots.items) |root| {
                if (self.varLiteralKind(root) == .quote) {
                    any_quote = true;
                    break;
                }
            }
            if (any_quote) {
                var write_idx: usize = 0;
                for (open_roots.items) |root| {
                    if (self.varLiteralKind(root) != .quote) continue;
                    open_roots.items[write_idx] = root;
                    write_idx += 1;
                }
                open_roots.shrinkRetainingCapacity(write_idx);
            }
        }

        // --- 2. Partition into interference components. ---
        component_parent.clearRetainingCapacity();
        is_driver.clearRetainingCapacity();
        footprint_owner.clearRetainingCapacity();
        for (open_roots.items, 0..) |root, idx| {
            try component_parent.append(self.gpa, idx);
            // Seed each literal's own root so any driver whose footprint reaches
            // it is merged into its component. Roots are deduped, so no collision
            // is possible here.
            try footprint_owner.putNoClobber(root, idx);
        }
        for (open_roots.items, 0..) |root, idx| {
            const constraint_range = self.types.resolveVar(root).desc.content.flex.constraints;
            const drives = self.rangeHasNonLiteralConstraint(constraint_range);
            try is_driver.append(self.gpa, drives);
            if (!drives) continue;

            // collectReachableVars only reads the store, so holding the
            // constraint slice across it is safe (same fence as
            // `boundaryDefaultLeaksIntoSignature`). It stores RESOLVED roots.
            footprint.clearRetainingCapacity();
            for (self.types.sliceStaticDispatchConstraints(constraint_range)) |constraint| {
                if (constraint.origin == .from_literal) continue;
                try self.collectReachableVars(constraint.fn_var, &footprint);
            }
            var fp_iter = footprint.keyIterator();
            while (fp_iter.next()) |fp_var| {
                // Only still-flex roots interfere; everything else is immune to
                // pinning. (Union order is irrelevant: unions commute, so the
                // partition is independent of hash-map iteration order.)
                if (self.types.resolveVar(fp_var.*).desc.content != .flex) continue;
                const gop = try footprint_owner.getOrPut(fp_var.*);
                if (gop.found_existing) {
                    componentUnion(component_parent.items, idx, gop.value_ptr.*);
                } else {
                    gop.value_ptr.* = idx;
                }
            }
        }

        // --- 3. Commit every component (disjoint, so order is unobservable). ---
        for (0..open_roots.items.len) |leader| {
            if (componentFind(component_parent.items, leader) != leader) continue;
            group_drivers.clearRetainingCapacity();
            var member_count: usize = 0;
            for (open_roots.items, 0..) |member_root, member_idx| {
                if (componentFind(component_parent.items, member_idx) != leader) continue;
                member_count += 1;
                if (is_driver.items[member_idx]) {
                    try group_drivers.append(self.gpa, member_root);
                }
            }
            // Within a round, every gathered root is still flex with its
            // `from_literal` provenance when its component commits: commits of
            // OTHER components only touch their own (disjoint) footprints, and this
            // component commits exactly once. So the kind lookups below cannot
            // fail — gather guaranteed them non-null.
            if (group_drivers.items.len == 0) {
                // A component without drivers is a lone passive (a passive's
                // footprint is just itself, so only a driver can merge it).
                std.debug.assert(member_count == 1);
                const passive_root = open_roots.items[leader];
                const kind = self.varLiteralKind(passive_root) orelse unreachable;
                try self.commitGatheredLiteral(passive_root, kind, universe, env);
            } else if (group_drivers.items.len == 1) {
                const driver_root = group_drivers.items[0];
                const kind = self.varLiteralKind(driver_root) orelse unreachable;
                try self.commitGatheredLiteral(driver_root, kind, universe, env);
            } else {
                // Several drivers, possibly of mixed literal kinds: quote drivers
                // take their single candidate (Str) on every attempt while the
                // numeral candidate scan iterates its list.
                //
                // BOUNDARY WARNING SEMANTICS FOR GROUPS: the leak check is per
                // literal (it walks one literal's constraint signatures), so it
                // runs for EVERY group member pre-commit — while all drivers are
                // still flex — and each leaking member gets its own LITERAL
                // DEFAULTED warning post-commit, exactly as if it had committed
                // alone. Non-leaking members default silently (def-local), and the
                // component's passives are never committed here, matching the
                // single-commit paths: a passive carries only `from_literal`
                // constraints, so its leak set is empty and it could never warn.
                group_warnings.clearRetainingCapacity();
                if (universe == .boundary) {
                    for (group_drivers.items) |driver| {
                        try group_warnings.append(self.gpa, try self.boundaryWarningBeforeCommit(driver));
                    }
                }
                try self.commitLiteralGroupDefault(group_drivers.items, env);
                if (universe == .boundary) {
                    // The group commit unified each driver with its committed var,
                    // so the driver root itself renders the committed type for the
                    // snapshot.
                    for (group_drivers.items, group_warnings.items) |driver, pending| {
                        try self.emitBoundaryWarningAfterCommit(pending, driver, driver);
                    }
                }
            }
        }

        // --- 4. Cascade the dispatches the commits unblocked. ---
        while (self.anyDeferredDispatchReceiverResolved(env)) {
            try self.checkStaticDispatchConstraints(env, true);
        }
    }
}

/// Commit one gathered literal root (lone passive or single driver) via the
/// single-literal first-satisfier probe — plus, at a generalization boundary, the
/// per-literal LITERAL DEFAULTED leak warning. The leak check and warning region
/// capture run while the literal is still flex (before the commit resolves it);
/// the warning is emitted after, rendered from the committed default var.
fn commitGatheredLiteral(
    self: *Self,
    root: Var,
    kind: StaticDispatchConstraint.LiteralKind,
    universe: LiteralDefaultUniverse,
    env: *Env,
) std.mem.Allocator.Error!void {
    switch (universe) {
        .finalize => _ = try self.commitLiteralDefault(root, kind, env),
        .boundary => {
            const pending = try self.boundaryWarningBeforeCommit(root);
            const default_var = try self.commitLiteralDefault(root, kind, env);
            try self.emitBoundaryWarningAfterCommit(pending, root, default_var);
        },
    }
}

/// Union-find `find` with path compression over the per-round component index
/// array in `runLiteralDefaultingRounds`.
fn componentFind(parent: []usize, idx: usize) usize {
    var root = idx;
    while (parent[root] != root) root = parent[root];
    var cur = idx;
    while (parent[cur] != cur) {
        const next = parent[cur];
        parent[cur] = root;
        cur = next;
    }
    return root;
}

/// Union-find `union` for `componentFind`. Commutative and associative, so the
/// resulting partition does not depend on the order unions are performed.
fn componentUnion(parent: []usize, a: usize, b: usize) void {
    const root_a = componentFind(parent, a);
    const root_b = componentFind(parent, b);
    if (root_a != root_b) parent[root_b] = root_a;
}

/// GROUP DEFAULT POLICY for a component with SEVERAL drivers: mutually
/// constrained open literals where no single literal's first-satisfier scan may
/// run alone (it would speculatively pin its peers while ignoring their
/// constraints — the order-dependence `runLiteralDefaultingRounds` exists to
/// prevent). The group defaults TOGETHER against the canonical candidate list:
/// the first candidate satisfying the WHOLE group's constraints wins.
///
/// MIXED KINDS: a group may contain quote drivers alongside numeral drivers (e.g.
/// `"s".foo(y)` and `1.bar(y)` interfering through `y`). A quote driver's
/// candidate list is the single entry Str, so it is assigned Str on every attempt
/// while the numeral scan iterates its list; an all-quote group needs exactly one
/// attempt.
///
/// Per candidate, inside one commit-probe:
///   phase 1 — assign: unify EVERY driver (all still flex; the round committed
///     nothing else in this component) with its own fresh candidate var. All
///     assignments happen before any constraint is consulted, so no driver's
///     verification can observe a not-yet-assigned peer.
///   phase 2 — verify: check every driver's every non-`from_literal` constraint
///     against the candidate (`staticDispatchConstraintAcceptsCandidate`, the
///     same check the single-literal probe uses). Passives and other in-footprint
///     vars are pinned here by the resolved method signatures — with every driver
///     already concrete, the verification is a plain conjunction of unifications.
/// Success commits the probe in place; failure rolls everything back and the next
/// candidate is tried.
///
/// ORDER-INDEPENDENCE: phase 1's outcome is the same var-disjoint assignment
/// regardless of driver enumeration order; phase 2 succeeds iff its equation set
/// is simultaneously unifiable, which is order-independent. So the chosen
/// candidate — and the types it pins — depend only on the group's constraint set
/// and the canonical candidate order.
///
/// The structural refutation pre-filter (`numeralCandidateStructurallyRefuted`)
/// is deliberately NOT consulted here: its safety-build witness guard is specified
/// against the single-literal probe (`commitLiteralDefault`), and multi-driver
/// groups are rare enough that probing every candidate directly costs nothing
/// measurable. (The bench counters likewise stay singleton-only.)
///
/// When no candidate satisfies the group, each driver falls back to its kind's
/// canonical head default (`commitLiteralDefaultHead` / Dec for numerals,
/// `commitQuoteDefault` / Str for quotes), so the dispatch cascade reports the
/// conflicts against the documented default type — the same failure shape as the
/// single-literal path.
fn commitLiteralGroupDefault(self: *Self, drivers: []const Var, env: *Env) Allocator.Error!void {
    std.debug.assert(drivers.len >= 2);

    // Constraint ranges and literal kinds captured while the drivers are still
    // flex; the ranges index the constraint store, whose entries outlive the
    // unifications below.
    var constraint_ranges: std.ArrayListUnmanaged(StaticDispatchConstraint.SafeList.Range) = .empty;
    defer constraint_ranges.deinit(self.gpa);
    var kinds: std.ArrayListUnmanaged(StaticDispatchConstraint.LiteralKind) = .empty;
    defer kinds.deinit(self.gpa);
    var has_numeral_driver = false;
    for (drivers) |driver| {
        try constraint_ranges.append(self.gpa, self.types.resolveVar(driver).desc.content.flex.constraints);
        // Every group driver is an open literal (the component gather keyed
        // on `varLiteralKind`), so the kind always exists.
        const kind = self.varLiteralKind(driver).?;
        try kinds.append(self.gpa, kind);
        if (kind == .numeral) has_numeral_driver = true;
    }

    var candidate_vars: std.ArrayListUnmanaged(Var) = .empty;
    defer candidate_vars.deinit(self.gpa);

    // Quote drivers have exactly one candidate (Str) and are assigned it on
    // every attempt; only numeral drivers consume the candidate scan, so an
    // all-quote group needs exactly one attempt.
    const candidate_scan: []const CIR.NumKind = if (has_numeral_driver)
        numeral_default_candidates[0..]
    else
        numeral_default_candidates[0..1];

    candidate: for (candidate_scan) |candidate_kind| {
        // Digit-fit precheck for every numeral driver's literal payload — pure
        // arithmetic on the constraint payloads, before any speculation. Quote
        // drivers are assigned Str, which their own `from_literal` provenance
        // accepts by definition, so they have nothing to precheck.
        for (constraint_ranges.items, kinds.items) |range, kind| {
            if (kind != .numeral) continue;
            for (self.types.sliceStaticDispatchConstraints(range)) |constraint| {
                switch (constraint.origin) {
                    .from_literal => |lit| {
                        if (!literalInfoAcceptsBuiltinNumKind(lit, candidate_kind)) continue :candidate;
                    },
                    else => {},
                }
            }
        }

        var commit_probe = try self.beginCommitProbe(env);
        var committed = false;
        // Runs on every exit from this loop body, `continue :candidate` included.
        defer if (!committed) commit_probe.rollback();

        // Phase 1: assign each driver its kind's candidate — the scanned numeric
        // candidate for numeral drivers, Str for quote drivers. Driver roots are
        // distinct and still flex, so these unifications are independent of each
        // other; flex-vs-concrete cannot mismatch (the drivers' dispatch
        // constraints defer onto the candidate as usual).
        candidate_vars.clearRetainingCapacity();
        for (drivers, kinds.items) |driver, kind| {
            const candidate_var = switch (kind) {
                .numeral => try self.freshFromContent(
                    try self.mkBuiltinNumberTypeContentFromKind(candidate_kind, env),
                    env,
                    self.getRegionAt(driver),
                ),
                .quote, .interpolation => try self.freshStr(env, self.getRegionAt(driver)),
            };
            try candidate_vars.append(self.gpa, candidate_var);
            const unify_result = try self.unify(driver, candidate_var, env);
            if (!unify_result.isOk()) continue :candidate;
        }

        // Phase 2: verify every driver's every non-`from_literal` constraint.
        // Store-backed iterator, not a held slice: the verification appends to
        // the constraint store, which can reallocate and dangle a slice.
        for (drivers, 0..) |_, driver_idx| {
            var constraints_iter = self.types.iterStaticDispatchConstraints(constraint_ranges.items[driver_idx]);
            while (constraints_iter.next()) |constraint| {
                switch (constraint.origin) {
                    // Validated digit-fit above, before the probe began.
                    .from_literal => {},
                    else => {
                        if (!try self.staticDispatchConstraintAcceptsCandidate(
                            &commit_probe,
                            constraint,
                            candidate_vars.items[driver_idx],
                            env,
                        )) {
                            continue :candidate;
                        }
                    },
                }
            }
        }

        committed = true;
        commit_probe.commit();
        return;
    }

    // No candidate satisfies the group: commit each driver's documented head
    // default (Dec for numerals, Str for quotes) so the dispatch pass reports the
    // conflicts. (All drivers are flex again — every failed probe rolled back —
    // but re-check defensively.)
    for (drivers, kinds.items) |driver, kind| {
        if (self.types.resolveVar(driver).desc.content != .flex) continue;
        switch (kind) {
            .numeral => _ = try self.commitLiteralDefaultHead(driver, env),
            .quote, .interpolation => _ = try self.commitQuoteDefault(driver, env),
        }
    }
}

/// Boundary defaulting — the Haskell Report §4.3.4 / GHC `-Wtype-defaults`
/// analogue. At a def's generalization boundary, default every still-open literal
/// var NOT reachable from the def's type (constraint-signature edges included),
/// then cascade the deferred dispatches it unblocks BEFORE generalization.
///
/// Why forced: instantiation copies only signature-reachable vars, so an
/// unreachable literal is SHARED by every use of the def — it cannot adapt per
/// call site even in principle. This is Haskell's "ambiguous type variable"
/// (constrained but not appearing in the type).
///
/// A LITERAL DEFAULTED warning fires only when the default leaks into the def's
/// interface (`boundaryDefaultLeaksIntoSignature`); def-local defaults are silent
/// (GHC warns on those too — which is why `-Wtype-defaults` is muted in practice).
///
/// Signature-reachable literals stay open (e.g. `|x| 5 + x`): generalization
/// quantifies them and each use defaults its own instantiated copy.
/// Whether committing a boundary default to `literal_var` would LEAK into the
/// def's interface: some dispatch-constraint signature of the literal reaches a
/// var itself reachable from the def's type, so firing the dispatch after
/// defaulting narrows the def's signature (e.g. defaulting `5` in `add_x(5)` fires
/// `plus : (5, x) -> r`, pinning the enclosing param `x`). That narrowing is the
/// warning-worthy act; constraints touching nothing signature-reachable default
/// silently.
///
/// Must run while `literal_var` is still flex (before the default unify) and after
/// `boundary_reachable_vars` is populated for the current boundary.
fn boundaryDefaultLeaksIntoSignature(self: *Self, literal_var: Var) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(literal_var);
    if (resolved.desc.content != .flex) return false;

    // collectReachableVars only reads the store and inserts into the map (no
    // unify/instantiate), so holding the constraint slice across it is safe.
    self.boundary_leak_vars.clearRetainingCapacity();
    const constraints = self.types.sliceStaticDispatchConstraints(resolved.desc.content.flex.constraints);
    for (constraints) |constraint| {
        if (constraint.origin == .from_literal) continue;
        try self.collectReachableVars(constraint.fn_var, &self.boundary_leak_vars);
    }

    var leak_iter = self.boundary_leak_vars.keyIterator();
    while (leak_iter.next()) |leak_var| {
        if (self.boundary_reachable_vars.contains(leak_var.*)) return true;
    }
    return false;
}

/// Per-literal boundary-warning state, computed BEFORE the literal's commit
/// resolves the flex away: the leak check walks the still-open constraint
/// signatures, and for numerals the warning region comes from the `from_literal`
/// payload (exact even when the root is an instantiated copy), reachable only
/// while the var is flex. Quote constraints carry no payload, so they fall back to
/// the var's own region — right for direct literals, possibly imprecise for
/// instantiated copies.
const PendingBoundaryWarning = struct {
    leaks_into_signature: bool,
    region: Region,
    kind: StaticDispatchConstraint.LiteralKind,
};

/// See `PendingBoundaryWarning` — must be called while `literal_root` is
/// still flex, with `boundary_reachable_vars` populated for the current
/// boundary.
fn boundaryWarningBeforeCommit(self: *Self, literal_root: Var) std.mem.Allocator.Error!PendingBoundaryWarning {
    return .{
        .leaks_into_signature = try self.boundaryDefaultLeaksIntoSignature(literal_root),
        .region = self.literalSourceRegion(literal_root) orelse self.getRegionAt(literal_root),
        // Every commit path only reaches here for a var with `from_literal`
        // provenance, and the var is still flex pre-commit, so the kind is
        // always derivable.
        .kind = self.varLiteralKind(literal_root) orelse unreachable,
    };
}

/// Emit the LITERAL DEFAULTED warning for a just-committed boundary default when
/// it leaks into the def's interface. `snapshot_var` is any var in the committed
/// equivalence class (the returned default var for single commits, the driver
/// root itself for group commits): its union-find root carries the concrete
/// committed content, so the snapshot renders the clean committed type either way
/// — identical to a pre-commit snapshot of the fresh candidate.
fn emitBoundaryWarningAfterCommit(
    self: *Self,
    pending: PendingBoundaryWarning,
    literal_root: Var,
    snapshot_var: Var,
) std.mem.Allocator.Error!void {
    // Only numerals warn on default. A numeral has many sensible types (I8…U64,
    // Dec, F64), so silently committing it to Dec is a real choice worth flagging.
    // A quote/interpolation defaults to Str — almost always what was meant, and how
    // main defaulted them (silently) — so flagging it is just noise.
    if (pending.kind != .numeral) return;
    if (!pending.leaks_into_signature) return;
    const default_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, snapshot_var);
    _ = try self.problems.appendProblem(self.gpa, .{ .literal_defaulted = .{
        .literal_var = literal_root,
        .kind = pending.kind,
        .default_snapshot = default_snapshot,
        .region = pending.region,
    } });
}

fn defaultLiteralsAtGeneralizationBoundary(self: *Self, def_root_var: Var, env: *Env) std.mem.Allocator.Error!void {
    const rank = env.rank();

    // The candidate universe is the var pool entry this generalize call will
    // promote. (The global open-literal worklist is NOT usable here: a sub-def
    // checked mid-flight runs in its own env at the same numeric rank, so rank
    // alone cannot tell this def's literals from an enclosing def's.) A candidate
    // is still flex at this rank with `from_literal` provenance.
    const pool_vars = env.var_pool.getVarsForRank(rank);

    // Fast path: no open literal at this boundary, nothing to do.
    var has_candidate = false;
    for (pool_vars) |pool_var| {
        const resolved = self.types.resolveVar(pool_var);
        if (resolved.desc.content != .flex) continue;
        if (resolved.desc.rank != rank) continue;
        if (self.varLiteralKind(resolved.var_) == null) continue;
        has_candidate = true;
        break;
    }
    if (!has_candidate) return;

    // The def root's reachable closure (recursing into `where`-constraint
    // signatures). A cycle root generalizes its whole group at once, so also seed
    // from every deferred cycle participant's def vars; at a non-cycle boundary
    // that list is empty or an enclosing cycle's, and seeding from it is purely
    // conservative (keeps more literals open, never defaults extra ones).
    self.boundary_reachable_vars.clearRetainingCapacity();
    try self.collectReachableVars(def_root_var, &self.boundary_reachable_vars);
    for (self.deferred_def_unifications.items) |deferred| {
        try self.collectReachableVars(deferred.ptrn_var, &self.boundary_reachable_vars);
        try self.collectReachableVars(deferred.expr_var, &self.boundary_reachable_vars);
        try self.collectReachableVars(deferred.def_var, &self.boundary_reachable_vars);
    }

    // Recursive-reference edges (`ctx == .recursive_def`) link a recursive call's
    // flex reference to the def's own (annotated) type and resolve only after this
    // boundary, at the cycle root. Unlike an early-return edge, the reference is an
    // internal body var unreachable from the def's signature, so the fixpoint below
    // never activates it. Seed the reachable closure of each recursive call's
    // RETURN: a literal whose dispatch hangs off an unresolved recursive result
    // (e.g. `fc(.., fc(..) + 1)` where `fc : I64, I64 -> I64` — `1`'s `plus`
    // receiver is `fc(..)`'s return) stays protected until the recursion resolves
    // and `plus` pins it to I64, instead of defaulting to Dec first. Seed only the
    // RETURN, not the call's arguments: a literal passed directly as a recursive
    // argument (e.g. `fib("bad arg")`) must stay a defaulting/error candidate so a
    // genuine arg-type mismatch is still reported at the argument. Purely
    // conservative for the return path — keeps more literals open, defaults none.
    {
        var rec_iter = self.constraints.iterIndices();
        while (rec_iter.next()) |constraint_idx| {
            switch (self.constraints.get(constraint_idx).*) {
                .eql => |eql| switch (eql.ctx) {
                    .recursive_def => {
                        const ref_resolved = self.types.resolveVar(eql.actual);
                        switch (ref_resolved.desc.content) {
                            .structure => |flat| switch (flat) {
                                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                                    try self.collectReachableVars(func.ret, &self.boundary_reachable_vars);
                                },
                                else => {},
                            },
                            else => {},
                        }
                    },
                    else => {},
                },
            }
        }
    }

    // Pending `eql` constraints are unify-later edges (early returns,
    // recursive-group cross-references); a literal awaiting unification with the
    // def's type through one is NOT ambiguous. Close the set over them — when one
    // side of an edge is in, collect both sides — to fixpoint, since collecting
    // one edge can make another edge's side reachable.
    var changed = true;
    while (changed) {
        changed = false;
        var constraint_iter = self.constraints.iterIndices();
        while (constraint_iter.next()) |constraint_idx| {
            switch (self.constraints.get(constraint_idx).*) {
                .eql => |eql| {
                    const expected_in = self.boundary_reachable_vars.contains(self.types.resolveVar(eql.expected).var_);
                    const actual_in = self.boundary_reachable_vars.contains(self.types.resolveVar(eql.actual).var_);
                    if (expected_in == actual_in) continue;
                    try self.collectReachableVars(eql.expected, &self.boundary_reachable_vars);
                    try self.collectReachableVars(eql.actual, &self.boundary_reachable_vars);
                    changed = true;
                },
            }
        }
    }

    // Default every unreachable candidate through the shared component machinery
    // — identical to `finalizeLiteralDefaults` except for the candidate universe
    // and the per-commit leak warnings — and cascade the dispatches whose
    // receivers each round defaulted, pinning their signatures' other vars before
    // generalization promotes them. The pool length is snapshotted here:
    // defaulting appends fresh default vars to this same pool entry, and those
    // must not be rescanned.
    try self.runLiteralDefaultingRounds(env, .{ .boundary = .{
        .rank = rank,
        .pool_len = pool_vars.len,
    } });
}

/// Whether any deferred static-dispatch constraint's receiver has resolved to
/// something other than a flex var — i.e. whether another
/// `checkStaticDispatchConstraints` pass can make progress (it re-defers only
/// still-flex receivers; every other receiver is consumed: fired or errored).
fn anyDeferredDispatchReceiverResolved(self: *Self, env: *Env) bool {
    for (env.deferred_static_dispatch_constraints.items.items) |deferred| {
        if (self.types.resolveVar(deferred.var_).desc.content != .flex) return true;
    }
    return false;
}

/// The source region of the numeral literal that put a `from_literal` constraint
/// on this var, straight from the numeral payload — explicit upstream data, exact
/// even after the literal var was unified with other vars. Quote constraints carry
/// no payload, so this returns null for them and the caller falls back to the
/// var's own region.
fn literalSourceRegion(self: *Self, var_: Var) ?Region {
    const resolved = self.types.resolveVar(var_);
    if (resolved.desc.content != .flex) return null;
    for (self.types.sliceStaticDispatchConstraints(resolved.desc.content.flex.constraints)) |constraint| {
        if (constraint.origin.numeralInfo()) |info| return info.region;
    }
    return null;
}

/// The literal kind this var is an open literal of — derived from its constraint
/// set, never stored. Returns null if the var carries no `from_literal`
/// constraint.
///
/// DUAL-KIND TIE-BREAK: a var can carry BOTH kinds (a flex/flex merge like
/// `if c 1 else "s"` unions the two literals' constraint sets). Such a var can
/// never type-check, but the kind reported here picks which head default is
/// attempted (Dec vs Str) and hence which literal-kind diagnostic fires — so it
/// must not depend on constraint storage order (which unify side each literal
/// arrived on), or mirror-image programs would get different diagnostics. We scan
/// ALL `from_literal` constraints and prefer `.numeral` over `.quote`, agreeing
/// with the two other sites that encode this choice:
/// `numericDefaultPhaseForConstraints` in src/check/checked_artifact.zig (any
/// numeral selects the Dec mono phase before quote is considered) and
/// `flexLiteralDefaultKind` in src/check/canonical_type_keys.zig (numeral if any,
/// else quote).
fn varLiteralKind(self: *Self, var_: Var) ?StaticDispatchConstraint.LiteralKind {
    const resolved = self.types.resolveVar(var_);
    if (resolved.desc.content != .flex) return null;
    const constraints = self.types.sliceStaticDispatchConstraints(resolved.desc.content.flex.constraints);
    var has_quote = false;
    var has_interpolation = false;
    for (constraints) |constraint| {
        switch (constraint.origin) {
            .from_literal => |lit| switch (lit) {
                .numeral => return .numeral,
                .quote => has_quote = true,
                .interpolation => has_interpolation = true,
            },
            else => {},
        }
    }
    return if (has_quote) .quote else if (has_interpolation) .interpolation else null;
}

/// Whether this flex var carries any interpolation `from_literal` constraint.
// --- Per-kind literal facts, each an exhaustive `switch (LiteralKind)` ---------
//
// Adding a `LiteralKind` variant turns each switch below into a compile error
// until the new kind is handled: the exhaustiveness *is* the checklist for the
// next literal kind (e.g. strings).

// Literal-defaulting probe test-support counters (PERMANENT):
// `bench_probe_attempts` and `bench_probe_refuted` are reset and asserted by the
// refuted-count guard test in src/check/test/type_checking_integration.zig, which
// detects `numeralCandidateStructurallyRefuted` silently going dead (refuting
// nothing). Zig's default test runner executes a binary's tests sequentially
// in-process, so the reset-check-assert pattern needs no synchronization.
/// Number of default-candidate probes attempted by literal-defaulting tests.
pub var bench_probe_attempts: usize = 0;
/// Number of default-candidate probes skipped by structural refutation tests.
pub var bench_probe_refuted: usize = 0;

/// Default a still-open literal var and COMMIT the result, returning the var the
/// literal was resolved against (for the boundary warning's snapshot).
/// Haskell-style: commit the FIRST candidate in the kind's canonical order that
/// satisfies every dispatch constraint the var accumulated — and the successful
/// probe IS the commit (no rollback-and-redo; see `tryCommitNumeralCandidate`). A
/// literal whose only obligation is its own `from_literal` takes the list head
/// directly (the common case, no probing). If no candidate satisfies, the head is
/// committed via the normal unify path anyway, so the post-finalize dispatch pass
/// reports the conflict against the documented default type.
fn commitLiteralDefault(self: *Self, literal_var: Var, kind: StaticDispatchConstraint.LiteralKind, env: *Env) Allocator.Error!Var {
    switch (kind) {
        .numeral => {
            const constraint_range = self.types.resolveVar(literal_var).desc.content.flex.constraints;
            if (self.rangeHasNonLiteralConstraint(constraint_range)) {
                for (numeral_default_candidates) |candidate_kind| {
                    // Cheap structural pre-filter: skip the full probe when
                    // read-only inspection PROVES it would fail. Skipping is
                    // observably identical to probing-and-rolling-back, and the
                    // candidate order (first-satisfier semantics) is untouched.
                    if (self.numeralCandidateStructurallyRefuted(candidate_kind, constraint_range)) {
                        // In safety-checked builds, prove the refutation: run the
                        // skipped probe and assert it fails. A failed probe rolls
                        // back completely (`CommitProbe`), so a passing assertion
                        // leaves observable state — var numbering included —
                        // identical to having skipped. A firing assertion means
                        // the refutation logic has drifted from the unifier's
                        // actual semantics (see the per-pair soundness fence on
                        // `structurallyIncompatiblePair` in src/check/unify.zig and
                        // the range-wide fence on
                        // `numeralCandidateStructurallyRefuted`); the witness
                        // commit it leaves behind is moot since we crash.
                        //
                        // This witness probe is deliberately NOT counted in
                        // `bench_probe_attempts`: this candidate was refuted (and
                        // counted as such below via `bench_probe_refuted`), so
                        // counting the safety-only re-probe as an attempt would make
                        // the refuted-count guard test's attempt/refuted bookkeeping
                        // diverge between safety and release builds.
                        if (comptime std.debug.runtime_safety) {
                            const witness = try self.tryCommitNumeralCandidate(literal_var, candidate_kind, constraint_range, env);
                            std.debug.assert(witness == null);
                        }
                        bench_probe_refuted += 1;
                        continue;
                    }
                    bench_probe_attempts += 1;
                    if (try self.tryCommitNumeralCandidate(literal_var, candidate_kind, constraint_range, env)) |committed_var| {
                        return committed_var;
                    }
                }
            }
            return try self.commitLiteralDefaultHead(literal_var, env);
        },
        // Str is the single candidate for string literals — a one-element
        // candidate list whose head is committed directly, no probing (and no
        // structural pre-filter: there is no scan to prune). If the var's other
        // constraints refute Str, the post-finalize dispatch pass reports the
        // conflict against it.
        .quote, .interpolation => return try self.commitQuoteDefault(literal_var, env),
    }
}

/// Commit Str — the single (and therefore default) candidate for string literals
/// — to `literal_var` through the normal unify path. The committed var reuses the
/// literal's region so later diagnostics stay anchored to the source; the deferred
/// from_quote dispatch fires against Str as usual.
fn commitQuoteDefault(self: *Self, literal_var: Var, env: *Env) Allocator.Error!Var {
    const default_var = try self.freshStr(env, self.getRegionAt(literal_var));
    _ = try self.unify(literal_var, default_var, env);
    return default_var;
}

/// Commit the canonical default (the candidate list head, Dec) to `literal_var`
/// through the normal unify path: the literal's dispatch constraints defer as
/// usual, so when no candidate satisfied them the dispatch pass reports the
/// conflict against the documented default type with problems recorded.
fn commitLiteralDefaultHead(self: *Self, literal_var: Var, env: *Env) Allocator.Error!Var {
    // The synthetic default var reuses the literal's region: if it ends up the
    // union-find root, a later diagnostic would otherwise anchor at Region.zero()
    // — the useless 1:1.
    const literal_region = self.getRegionAt(literal_var);
    const default_var = try self.freshFromContent(
        try self.mkBuiltinNumberTypeContentFromKind(numeral_default_candidates[0], env),
        env,
        literal_region,
    );
    _ = try self.unify(literal_var, default_var, env);
    return default_var;
}

/// Candidate order for numeral defaulting (first satisfier wins). `Dec` (the
/// canonical default) heads the list; then integers — signed before unsigned,
/// `I64` first (Roc's historical integer default), wider before narrower so a tie
/// never lands on a type that overflows sooner than it must; floats last. Order
/// past `Dec` only matters when the constraints refute `Dec` yet accept several
/// candidates — a pinned concrete arg or return admits exactly one regardless of
/// order. That single-admission claim holds because builtin numeric methods are
/// homogeneous (`T, T -> T`, e.g. `Dec.plus : Dec, Dec -> Dec`): every signature
/// position is the dispatcher type itself, so pinning ANY position pins the
/// candidate.
const numeral_default_candidates = [_]CIR.NumKind{ .dec, .i64, .u64, .i128, .u128, .i32, .u32, .i16, .u16, .i8, .u8, .f64, .f32 };

/// Whether the constraint range carries any obligation besides `from_literal`
/// provenance — i.e. whether defaulting must consult the candidate probe at all.
fn rangeHasNonLiteralConstraint(self: *Self, range: StaticDispatchConstraint.SafeList.Range) bool {
    for (self.types.sliceStaticDispatchConstraints(range)) |constraint| {
        if (constraint.origin != .from_literal) return true;
    }
    return false;
}

/// Structural pre-filter for `tryCommitNumeralCandidate`: returns true only when
/// read-only inspection PROVES the full probe would fail, so the probe can be
/// skipped without observable difference (a failed probe is fully rolled back, so
/// skipping it leaves identical state). Resolves vars in the relevant stores but
/// NEVER mutates them. Anything uncertain returns false (fall through to the
/// probe) — refutation is exact logic, probing is the default.
///
/// Two refutation facts, per non-`from_literal` constraint (the `from_literal`
/// ones are pre-filtered by digit-fit inside `tryCommitNumeralCandidate` via
/// `literalInfoAcceptsBuiltinNumKind`):
///
/// 1. Method lookup miss: the candidate's owner env has no method for the
///    constraint's `fn_name`. The probe performs the IDENTICAL lookup
///    (`staticDispatchConstraintAcceptsCandidate` resolves the candidate nominal's
///    origin/source-decl — the very values computed here without minting the
///    candidate var — and calls `lookupMethodBindingFromEnvAndDeclConst`, a pure
///    read of finalized tables) and returns false on a miss, failing the probe.
///    This fact does not depend on any mutable type state, so it refutes
///    unconditionally.
///
/// 2. Dispatcher-position mismatch: a `.refuting` pair from
///    `unifier.structurallyIncompatiblePair` — a signature position where the
///    method is the dispatcher type C itself while the constraint's corresponding
///    position is already a CONCRETE builtin numeric nominal with a different
///    source decl. Nominal unification requires identical identity
///    (`sameNominalIdentity`: origin + source decl), so that position's unify must
///    throw — the probe cannot succeed. (The dispatcher-position check is made per
///    position, not assumed; the filter's EFFECTIVENESS comes from builtin numeric
///    methods being homogeneous (`T, T -> T`, e.g. `Dec.plus : Dec, Dec -> Dec`),
///    which puts the dispatcher type at every position.)
///
/// Fact 2's per-pair soundness fence — the unifier's err-backed-nominal
/// short-circuit (a SUCCESSFUL err merge), flex-side constraint deferral, and
/// never-written nominal backings — is documented on and encoded in
/// `structurallyIncompatiblePair` in src/check/unify.zig, co-located with the
/// unify internals it reasons about. This function supplies the RANGE-WIDE side of
/// that fence: it classifies every argument/return position of EVERY constraint in
/// the range, requires matching arities (so every classified pair is exactly the
/// pair `unifyFunc` would visit), and abandons refutation for the whole candidate
/// on any `.uninspectable` position. Any deviation — aliases, records, tag unions,
/// tuples, err content, polymorphic method positions, unexpected arity, non-fn
/// shapes — makes the whole candidate fall through to the probe rather than
/// weakening soundness.
fn numeralCandidateStructurallyRefuted(
    self: *Self,
    candidate_kind: CIR.NumKind,
    constraint_range: StaticDispatchConstraint.SafeList.Range,
) bool {
    // The candidate's identity, computed without creating its var: the same origin
    // module + source decl `mkNumberTypeContent` embeds in the nominal the probe
    // would mint.
    const candidate_source_decl = self.sourceDeclForBuiltinNominal(.{ .num = candidate_kind });
    const candidate_origin_module = self.builtinOriginModule();
    const original_env, const is_this_module = self.ownerEnvForOriginModule(
        candidate_origin_module,
        candidate_source_decl,
        true,
        "static dispatch candidate",
    );
    const method_types: *const types_mod.Store = if (is_this_module) self.types else &original_env.types;

    var found_refuting_pair = false;
    for (self.types.sliceStaticDispatchConstraints(constraint_range)) |constraint| {
        if (constraint.origin == .from_literal) continue;

        // Method lookup short of instantiation — same env, owner decl, and method
        // ident the probe's lookup uses, via the same const lookup.
        const method_binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
            self.cir,
            candidate_source_decl,
            constraint.fn_name,
        ) orelse return true;
        const def_var: Var = ModuleEnv.varFrom(method_binding.type_node_idx);

        // From here on, refutation needs the closed-world structural walk;
        // any uncertainty falls through to the probe (return false).
        const method_func = method_types.resolveVar(def_var).desc.content.unwrapFunc() orelse return false;
        const constraint_func = self.types.resolveVar(constraint.fn_var).desc.content.unwrapFunc() orelse return false;
        if (method_func.args.len() != constraint_func.args.len()) return false;

        const method_args = method_types.sliceVars(method_func.args);
        const constraint_args = self.types.sliceVars(constraint_func.args);
        for (method_args, constraint_args) |method_pos, constraint_pos| {
            switch (unifier.structurallyIncompatiblePair(method_types, self.types, candidate_source_decl, method_pos, constraint_pos)) {
                .uninspectable => return false,
                .refuting => found_refuting_pair = true,
                .safe => {},
            }
        }
        switch (unifier.structurallyIncompatiblePair(method_types, self.types, candidate_source_decl, method_func.ret, constraint_func.ret)) {
            .uninspectable => return false,
            .refuting => found_refuting_pair = true,
            .safe => {},
        }
    }
    return found_refuting_pair;
}

/// Probe whether committing builtin numeric `candidate_kind` to the open
/// numeral `literal_var` satisfies every constraint in `constraint_range` — and
/// if so, COMMIT the probe's work in place, returning the committed candidate var.
/// The probe runs against the real stores through the normal `unify` wrapper, so
/// success needs no rollback-and-redo: the literal var is already resolved to the
/// candidate, with full bookkeeping (var pool, regions, deferred dispatch
/// constraints — which the later dispatch pass fires exactly as it fires the
/// redo-unify's today). Failure returns null after `CommitProbe.rollback` restores
/// every store and buffer the attempt grew.
fn tryCommitNumeralCandidate(
    self: *Self,
    literal_var: Var,
    candidate_kind: CIR.NumKind,
    constraint_range: StaticDispatchConstraint.SafeList.Range,
    env: *Env,
) Allocator.Error!?Var {
    // The candidate must be able to represent the literal payload itself (for
    // numerals: the digits fit `candidate_kind`). Pure arithmetic on the
    // constraint payloads, checked before any store mutation so refuting a
    // candidate on digits alone costs no speculation at all.
    for (self.types.sliceStaticDispatchConstraints(constraint_range)) |constraint| {
        switch (constraint.origin) {
            .from_literal => |lit| {
                if (!literalInfoAcceptsBuiltinNumKind(lit, candidate_kind)) return null;
            },
            else => {},
        }
    }

    var commit_probe = try self.beginCommitProbe(env);
    var committed = false;
    defer if (!committed) commit_probe.rollback();

    // The candidate (and every other var this attempt creates — the unify's fresh
    // vars, instantiated method vars) registers in the CALLER's env pool: on
    // success the work survives into generalization, so the generalizer must see
    // these vars at their real ranks. (A probe-local pool — the previous design —
    // would be released on success and leave the committed store vars invisible to
    // rank adjustment; conversely, the dangling-entry hazard that design avoided
    // is now closed by `CommitProbe.rollback` truncating the pool's rank lists on
    // failure.) Region: the literal's own, so vars the probe stamps stay anchored
    // to the real source span.
    const candidate_var = try self.freshFromContent(
        try self.mkBuiltinNumberTypeContentFromKind(candidate_kind, env),
        env,
        self.getRegionAt(literal_var),
    );
    const unify_result = try self.unify(literal_var, candidate_var, env);
    if (!unify_result.isOk()) return null;

    // Store-backed iterator, not a held slice: the probe below appends to the
    // constraint store, which can reallocate and dangle a slice.
    var constraints_iter = self.types.iterStaticDispatchConstraints(constraint_range);
    while (constraints_iter.next()) |constraint| {
        switch (constraint.origin) {
            // Validated digit-fit above, before the probe began.
            .from_literal => {},
            else => {
                if (!try self.staticDispatchConstraintAcceptsCandidate(&commit_probe, constraint, candidate_var, env)) {
                    return null;
                }
            },
        }
    }

    committed = true;
    commit_probe.commit();
    return candidate_var;
}

/// Whether the concrete builtin numeric candidate of `num_kind` can represent
/// the literal payload `lit`. The numeral arm validates the digits fit;
/// future kinds fill in their own arm (compiler-enforced).
fn literalInfoAcceptsBuiltinNumKind(lit: StaticDispatchConstraint.LiteralInfo, num_kind: CIR.NumKind) bool {
    return switch (lit) {
        .numeral => |info| validateBuiltinFromNumeralLiteral(num_kind, info) == null,
        // A string or interpolation literal can never be represented by a builtin
        // numeric type; its sole candidate (Str) is not a builtin num kind.
        .quote, .interpolation => false,
    };
}

/// Speculatively mutates the real stores (copyVar/instantiateVar, real
/// unification) and does NOT roll them back itself — the caller's commit-probe
/// rollback does (or its commit keeps them). The `*CommitProbe` parameter is a
/// scope-proof token making that contract compile-time: this function is
/// unreachable without an open commit-probe scope that settles the speculation one
/// way or the other.
fn staticDispatchConstraintAcceptsCandidate(
    self: *Self,
    _: *CommitProbe,
    constraint: StaticDispatchConstraint,
    candidate_var: Var,
    env: *Env,
) Allocator.Error!bool {
    // Scope-proof token only; not otherwise consulted.
    const candidate_resolved = self.types.resolveVar(candidate_var);
    const nominal_type = candidate_resolved.desc.content.unwrapNominalType() orelse return false;
    const original_env, const is_this_module = self.ownerEnvForOriginModule(
        nominal_type.origin_module,
        nominal_type.sourceDeclOptional(),
        nominal_type.originIsBuiltin(),
        "static dispatch candidate",
    );

    const method_binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
        self.cir,
        nominal_type.sourceDeclOptional(),
        constraint.fn_name,
    ) orelse return false;
    const def_var: Var = ModuleEnv.varFrom(method_binding.type_node_idx);

    const method_var = if (is_this_module) blk: {
        if (self.types.resolveVar(def_var).desc.rank == .generalized) {
            break :blk try self.instantiateVar(def_var, env, .use_last_var);
        }
        break :blk def_var;
    } else blk: {
        const copied_var = try self.copyVar(def_var, original_env, self.getRegionAt(candidate_var));
        break :blk try self.instantiateVar(copied_var, env, .{ .explicit = self.getRegionAt(candidate_var) });
    };

    // The real unify wrapper, not the throwaway-store probe unify: on the commit
    // path this merge (and its rank/region/deferred-constraint bookkeeping) is
    // kept. A mismatch records a problem and poisons the operands, all of which the
    // commit-probe rollback rewinds.
    const result = try self.unify(method_var, constraint.fn_var, env);
    return result.isOk();
}

fn isBuiltinNumericNominal(self: *Self, var_: Var) bool {
    const resolved = self.types.resolveVar(var_);
    const nominal = resolved.desc.content.unwrapNominalType() orelse return false;
    const ident = nominal.ident.ident_idx;
    return ident.eql(self.cir.idents.u8_type) or
        ident.eql(self.cir.idents.i8_type) or
        ident.eql(self.cir.idents.u16_type) or
        ident.eql(self.cir.idents.i16_type) or
        ident.eql(self.cir.idents.u32_type) or
        ident.eql(self.cir.idents.i32_type) or
        ident.eql(self.cir.idents.u64_type) or
        ident.eql(self.cir.idents.i64_type) or
        ident.eql(self.cir.idents.u128_type) or
        ident.eql(self.cir.idents.i128_type) or
        ident.eql(self.cir.idents.f32_type) or
        ident.eql(self.cir.idents.f64_type) or
        ident.eql(self.cir.idents.dec_type);
}

/// Whether `var_` resolved to the builtin `Dec` nominal — the canonical head
/// default a numeral literal falls to. Distinguishes a numeral-defaulted
/// dispatcher (Dec) from a quote-defaulted one (Str), so the "this numeric
/// literal was given the type Dec" hint only fires for the former. The hint text
/// hardcodes `Dec`, so a literal pinned to some other numeric type must not
/// claim it.
fn isDecNominal(self: *Self, var_: Var) bool {
    const resolved = self.types.resolveVar(var_);
    const nominal = resolved.desc.content.unwrapNominalType() orelse return false;
    return nominal.ident.ident_idx.eql(self.cir.idents.dec_type);
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

/// Validate the recursive references recorded while checking a LOCAL block def's
/// body (those at index >= `from`), now that the def's lambda has generalized
/// and its pattern var carries the generalized type. Sequential scoping forbids
/// local mutual recursion, so every such reference is a self/enclosing one and a
/// plain unification suffices (no per-use instantiation). The validated refs are
/// then popped.
fn validateLocalRecursiveRefs(self: *Self, env: *Env, from: usize) std.mem.Allocator.Error!void {
    // Capture the end up front: validation only unifies, it never records new
    // local recursive refs, so the range is stable.
    const end = self.local_recursive_refs.items.len;
    var i = from;
    while (i < end) : (i += 1) {
        const ref = self.local_recursive_refs.items[i];
        _ = try self.unifyInContext(ref.pat_var, ref.expr_var, env, .{ .recursive_def = .{ .def_name = ref.def_name } });
    }
    self.local_recursive_refs.shrinkRetainingCapacity(from);
}

/// Resolve one `eql` constraint. For a `recursive_def` cross-reference (mutual
/// recursion) whose target has been generalized, instantiate the target so the
/// reference gets fresh type parameters — otherwise the two members' rigid type
/// variables would be forced to unify, producing a spurious `T(k)` != `T(k)`
/// mismatch. Self-references (and any not-yet-generalized target) keep the
/// monomorphic direct unification.
fn resolveEqlConstraint(self: *Self, eql: anytype, env: *Env) std.mem.Allocator.Error!void {
    if (eql.is_cross_reference and self.types.resolveVar(eql.expected).desc.rank == .generalized) {
        const instantiated = try self.instantiateVar(eql.expected, env, .use_last_var);
        _ = try self.unifyInContext(instantiated, eql.actual, env, eql.ctx);
        return;
    }
    _ = try self.unifyInContext(eql.expected, eql.actual, env, eql.ctx);
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
                try self.resolveEqlConstraint(eql, env);
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
/// Runaway guard for the deferred static-dispatch worklist below. The worklist
/// legitimately grows while solving (resolving one constraint can reveal more),
/// but a self-referential `where` constraint (e.g. `Vec(a) ... where
/// [a.join : Vec(a), a -> a]` used nested) makes each iteration enqueue a fresh
/// constrained receiver, so the list outruns the index forever. No real module
/// reaches this many deferred dispatch constraints in one pass; hitting it means a
/// non-terminating cycle, reported as an infinite type instead of hanging.
const max_deferred_dispatch_iterations: usize = 1 << 14;

fn checkStaticDispatchConstraints(self: *Self, env: *Env, is_numeric_default_pass: bool) std.mem.Allocator.Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // During this pass, we want to hold onto any flex vars we encounter and
    // check them again later, when maybe they've been resolved
    const scratch_deferred_top = self.scratch_deferred_static_dispatch_constraints.top();
    defer self.scratch_deferred_static_dispatch_constraints.clearFrom(scratch_deferred_top);

    var deferred_constraint_index: usize = 0;
    while (deferred_constraint_index < env.deferred_static_dispatch_constraints.items.items.len) : (deferred_constraint_index += 1) {
        const deferred_constraint = env.deferred_static_dispatch_constraints.items.items[deferred_constraint_index];

        if (deferred_constraint_index >= max_deferred_dispatch_iterations) {
            // Runaway: a non-terminating static-dispatch cycle keeps re-deriving an
            // unsatisfiable `where` constraint on the same type because the
            // recursion never bottoms out (the element type's method has the wrong
            // shape). Report the *unsatisfied method* precisely — naming the
            // dispatcher type and method — rather than a generic infinite type,
            // then stop. (See `max_deferred_dispatch_iterations`.)
            const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
            const resolved_content = self.types.resolveVar(deferred_constraint.var_).desc.content;
            const dispatcher_type: ?problem.DispatcherDoesNotImplMethod.DispatcherType =
                if (resolved_content == .structure and resolved_content.structure == .nominal_type)
                    .nominal
                else if (resolved_content == .rigid)
                    .rigid
                else
                    null;
            if (constraints.len > 0 and dispatcher_type != null) {
                try self.reportConstraintError(
                    deferred_constraint.var_,
                    constraints[0],
                    .{ .missing_method = dispatcher_type.? },
                    env,
                    is_numeric_default_pass,
                );
            } else {
                // Fall back to a generic infinite-type error when we can't name a
                // dispatcher type (e.g. the receiver is a flex/structural type).
                const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, deferred_constraint.var_);
                _ = try self.problems.appendProblem(self.gpa, .{ .infinite_recursion = .{
                    .var_ = deferred_constraint.var_,
                    .snapshot = snapshot,
                    .def_name = null,
                } });
            }
            try self.unifyWith(deferred_constraint.var_, .err, env);
            break;
        }

        const dispatcher_resolved = self.types.resolveVar(deferred_constraint.var_);
        const dispatcher_content = dispatcher_resolved.desc.content;

        dispatch_resolution: while (true) {
            if (dispatcher_content == .err) {
                // If the root type is an error, then skip constraint checking
                const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
                for (constraints) |constraint| {
                    try self.markConstraintFunctionAsError(constraint, env);
                }
                try self.unifyWith(deferred_constraint.var_, .err, env);
                break :dispatch_resolution;
            } else if (dispatcher_content == .rigid) {
                // Get the rigid variable and the constraints it has defined
                const rigid = dispatcher_content.rigid;
                const rigid_constraints = self.types.sliceStaticDispatchConstraints(rigid.constraints);

                // Build a map of constraints the rigid has
                self.ident_to_var_map.clearRetainingCapacity();
                try self.ident_to_var_map.ensureUnusedCapacity(@intCast(rigid_constraints.len));
                for (rigid_constraints) |rigid_constraint| {
                    self.ident_to_var_map.putAssumeCapacity(rigid_constraint.fn_name, rigid_constraint.fn_var);
                }

                // Iterate over the deferred constraints to validate against.
                // iterRange re-fetches each item through the SafeList, so it stays valid
                // even if the unify below appends and reallocates the backing array.
                var constraints_iter = self.types.static_dispatch_constraints.iterRange(deferred_constraint.constraints);
                while (constraints_iter.next()) |constraint| {
                    if (constraint.origin == .from_literal) {
                        if (self.builtinNumKindFromTypeName(rigid.name)) |num_kind| {
                            if (skipDefaultedDecIntegerLiteralValidation(is_numeric_default_pass, num_kind, constraint)) {
                                continue;
                            }
                            if (try self.reportInvalidBuiltinFromNumeralLiteral(
                                deferred_constraint.var_,
                                constraint,
                                num_kind,
                                env,
                            )) {
                                continue;
                            }
                        }
                    }
                    if (constraint.origin.literalKind() == .interpolation) {
                        try self.ensureCustomInterpolationPartsChecked(constraint, env);
                    }

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
                        } else {
                            try self.linkConstraintMetadata(rigid_var, constraint.fn_var);
                            try self.reportEffectfulDispatchInExpect(constraint);
                        }
                    } else {
                        try self.reportConstraintError(
                            deferred_constraint.var_,
                            constraint,
                            .{ .missing_method = .nominal },
                            env,
                            is_numeric_default_pass,
                        );
                        continue;
                    }
                }
                break :dispatch_resolution;
            } else if (dispatcher_content == .structure and dispatcher_content.structure == .nominal_type) {
                // If the root type is a nominal type, then this is valid static dispatch
                const nominal_type = dispatcher_content.structure.nominal_type;

                // Get the module ident that this type was defined in
                const original_module_ident = nominal_type.origin_module;

                // Check if the nominal type in question is defined in this module
                const original_env, const is_this_module = self.ownerEnvForOriginModule(
                    original_module_ident,
                    nominal_type.sourceDeclOptional(),
                    nominal_type.originIsBuiltin(),
                    "static dispatch nominal",
                );

                // Get some data about the nominal type
                const region = self.getRegionAt(deferred_constraint.var_);

                // Iterate over the constraints
                const constraints_range = deferred_constraint.constraints;
                const constraints_len = constraints_range.len();
                const constraints_start: usize = @intFromEnum(constraints_range.start);
                var constraint_i: usize = 0;
                while (constraint_i < constraints_len) : (constraint_i += 1) {
                    // Re-fetch by index each iteration because nested unification can append
                    // constraints and reallocate the backing array.
                    const constraint = self.types.static_dispatch_constraints.items.items[constraints_start + constraint_i];
                    const constraint_fn_resolved = self.types.resolveVar(constraint.fn_var).desc.content;
                    if (constraint_fn_resolved == .err) {
                        // If this constraint is already an error, the skip this pass
                        continue;
                    }
                    if (!try self.validateFromNumeralLiteralForBuiltinNominal(
                        deferred_constraint.var_,
                        constraint,
                        nominal_type,
                        env,
                        is_numeric_default_pass,
                    )) {
                        continue;
                    }
                    if (constraint.origin.literalKind() == .interpolation) {
                        if (self.nominalIsBuiltinStrType(nominal_type)) {
                            if (try self.satisfyBuiltinStrInterpolation(deferred_constraint.var_, constraint, env)) {
                                continue;
                            }
                        } else {
                            try self.ensureCustomInterpolationPartsChecked(constraint, env);
                        }
                    }
                    const method_binding = if (constraint.fn_name.eql(self.cir.idents.is_eq) and
                        try self.nominalSupportsImplicitIsEq(nominal_type))
                    blk: {
                        const exact_method_binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
                            self.cir,
                            nominal_type.sourceDeclOptional(),
                            constraint.fn_name,
                        );
                        if (exact_method_binding == null and try self.nominalSupportsImplicitIsEq(nominal_type)) {
                            try self.satisfyImplicitEqualityConstraint(
                                deferred_constraint.var_,
                                constraint,
                                constraint.fn_var,
                                env,
                                region,
                            );
                            continue;
                        }
                        break :blk exact_method_binding orelse {
                            try self.reportConstraintError(
                                deferred_constraint.var_,
                                constraint,
                                .{ .missing_method = .nominal },
                                env,
                                is_numeric_default_pass,
                            );
                            continue;
                        };
                    } else original_env.lookupMethodBindingFromEnvAndDeclConst(self.cir, nominal_type.sourceDeclOptional(), constraint.fn_name) orelse {
                        // Method name doesn't exist in target module
                        try self.reportConstraintError(
                            deferred_constraint.var_,
                            constraint,
                            .{ .missing_method = .nominal },
                            env,
                            is_numeric_default_pass,
                        );
                        continue;
                    };
                    if (constraint.fn_name.eql(self.cir.idents.is_eq)) {
                        self.rewriteEqBinopAsMethodEq(constraint);
                    }

                    const def_idx = method_binding.def_idx;
                    const method_type_var: Var = ModuleEnv.varFrom(method_binding.type_node_idx);
                    const def = original_env.store.getDef(def_idx);
                    // Track whether we just processed or referenced a cycle participant.
                    var cycle_method_expr_var: ?Var = null;

                    if (is_this_module) {
                        // Check if we've processed this def already.
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
                                        if (def.annotation != null) {
                                            cycle_method_expr_var = try self.freshRecursiveMethodPlaceholder(processing_def, def, env, region);
                                        } else {
                                            const def_expr_var = ModuleEnv.varFrom(def.expr);
                                            cycle_method_expr_var = def_expr_var;
                                        }
                                    } else {
                                        std.debug.assert(sub_env.rank() == .outermost);
                                        self.env_pool.release(sub_env);
                                    }
                                },
                                .processing => {
                                    if (!isFunctionDef(&self.cir.store, self.cir.store.getExpr(def.expr))) {
                                        try self.poisonRecursiveNonFunctionProcessingDef(processing_def, null, env);
                                        try self.unifyWith(deferred_constraint.var_, .err, env);
                                        continue;
                                    }

                                    // Create a fresh flex var at the current rank for
                                    // the method type, and validate it against the
                                    // binding at the recursion boundary. Using the
                                    // binding var directly here would pull body vars
                                    // to a lower rank and prevent generalization.
                                    cycle_method_expr_var = try self.freshRecursiveMethodPlaceholder(processing_def, def, env, region);

                                    // Check if this is mutual recursion through dispatch.
                                    if (self.current_processing_def) |current_def| {
                                        if (current_def != processing_def.def_idx) {
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
                        if (self.types.resolveVar(method_type_var).desc.rank == .generalized) {
                            break :blk try self.instantiateVar(method_type_var, env, .use_last_var);
                        }
                        break :blk method_type_var;
                    } else blk: {
                        // Copy the method from the other module's type store
                        const copied_var = try self.copyVar(method_type_var, original_env, region);
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

                    const deferred_len_before = env.deferred_static_dispatch_constraints.items.items.len;
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
                    } else if (try self.reportRecursiveStaticDispatchIfNeeded(
                        deferred_constraint.var_,
                        constraint,
                        deferred_len_before,
                        env,
                    )) {
                        try self.unifyWith(constraint_fn.ret, .err, env);
                    } else {
                        try self.reportEffectfulDispatchInExpect(constraint);
                    }
                }
                break :dispatch_resolution;
            } else if (dispatcher_content == .alias) {
                const alias = dispatcher_content.alias;

                // Get the module ident that this alias type was defined in
                const original_module_ident = alias.origin_module;
                const original_env, const is_this_module = self.ownerEnvForOriginModule(
                    original_module_ident,
                    alias.source_decl.toOptional(),
                    alias.source_decl.originIsBuiltin(),
                    "static dispatch alias",
                );

                const region = self.getRegionAt(deferred_constraint.var_);
                const constraints_range = deferred_constraint.constraints;
                const constraints_len = constraints_range.len();
                const constraints_start: usize = @intFromEnum(constraints_range.start);
                var constraint_i: usize = 0;
                while (constraint_i < constraints_len) : (constraint_i += 1) {
                    const constraint = self.types.static_dispatch_constraints.items.items[constraints_start + constraint_i];
                    const constraint_fn_resolved = self.types.resolveVar(constraint.fn_var).desc.content;
                    if (constraint_fn_resolved == .err) continue;

                    if (!try self.validateFromNumeralLiteralForBuiltinAlias(
                        deferred_constraint.var_,
                        constraint,
                        alias,
                        env,
                        is_numeric_default_pass,
                    )) {
                        continue;
                    }
                    if (constraint.origin.literalKind() == .interpolation) {
                        try self.ensureCustomInterpolationPartsChecked(constraint, env);
                    }
                    if (constraint.fn_name.eql(self.cir.idents.is_eq)) {
                        const method_binding = original_env.lookupMethodBindingFromTwoEnvsAndDeclConst(
                            alias.source_decl.toOptional(),
                            self.cir,
                            constraint.fn_name,
                        );
                        if (method_binding == null) {
                            const backing_var = self.types.getAliasBackingVar(alias);
                            if (try self.varSupportsIsEq(backing_var)) {
                                try self.satisfyImplicitEqualityConstraint(
                                    deferred_constraint.var_,
                                    constraint,
                                    constraint.fn_var,
                                    env,
                                    region,
                                );
                            } else {
                                try self.reportEqualityError(
                                    deferred_constraint.var_,
                                    constraint,
                                    env,
                                );
                            }
                            continue;
                        }
                    }

                    const method_binding = original_env.lookupMethodBindingFromTwoEnvsAndDeclConst(
                        alias.source_decl.toOptional(),
                        self.cir,
                        constraint.fn_name,
                    ) orelse {
                        try self.reportConstraintError(
                            deferred_constraint.var_,
                            constraint,
                            .{ .missing_method = .nominal },
                            env,
                            is_numeric_default_pass,
                        );
                        continue;
                    };
                    const def_idx = method_binding.def_idx;
                    if (constraint.fn_name.eql(self.cir.idents.is_eq)) {
                        self.rewriteEqBinopAsMethodEq(constraint);
                    }

                    const method_type_var: Var = ModuleEnv.varFrom(method_binding.type_node_idx);
                    const def = original_env.store.getDef(def_idx);
                    var cycle_method_expr_var: ?Var = null;
                    if (is_this_module) {
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

                                        try self.deferred_cycle_envs.append(self.gpa, sub_env);
                                        if (def.annotation != null) {
                                            cycle_method_expr_var = try self.freshRecursiveMethodPlaceholder(processing_def, def, env, region);
                                        } else {
                                            const def_expr_var = ModuleEnv.varFrom(def.expr);
                                            cycle_method_expr_var = def_expr_var;
                                        }
                                    } else {
                                        std.debug.assert(sub_env.rank() == .outermost);
                                        self.env_pool.release(sub_env);
                                    }
                                },
                                .processing => {
                                    if (!isFunctionDef(&self.cir.store, self.cir.store.getExpr(def.expr))) {
                                        try self.poisonRecursiveNonFunctionProcessingDef(processing_def, null, env);
                                        try self.unifyWith(deferred_constraint.var_, .err, env);
                                        continue;
                                    }

                                    cycle_method_expr_var = try self.freshRecursiveMethodPlaceholder(processing_def, def, env, region);

                                    if (self.current_processing_def) |current_def| {
                                        if (current_def != processing_def.def_idx) {
                                            if (self.cycle_root_def == null) {
                                                std.debug.assert(!self.defer_generalize);
                                                std.debug.assert(self.deferred_cycle_envs.items.len == 0);
                                                std.debug.assert(self.deferred_def_unifications.items.len == 0);
                                                self.cycle_root_def = processing_def.def_idx;
                                            }
                                            self.defer_generalize = true;
                                        }
                                    }
                                },
                                .processed => {},
                            }
                        }
                    }

                    const method_var = if (cycle_method_expr_var) |expr_var_for_method| blk: {
                        break :blk expr_var_for_method;
                    } else if (is_this_module) blk: {
                        if (self.types.resolveVar(method_type_var).desc.rank == .generalized) {
                            break :blk try self.instantiateVar(method_type_var, env, .use_last_var);
                        }
                        break :blk method_type_var;
                    } else blk: {
                        const copied_var = try self.copyVar(method_type_var, original_env, region);
                        break :blk try self.instantiateVar(copied_var, env, .{ .explicit = region });
                    };

                    const constraint_fn = constraint_fn_resolved.unwrapFunc() orelse {
                        _ = try self.unifyInContext(method_var, constraint.fn_var, env, .{
                            .method_type = .{
                                .constraint_var = constraint.fn_var,
                                .dispatcher_name = alias.ident.ident_idx,
                                .method_name = constraint.fn_name,
                            },
                        });
                        try self.unifyWith(deferred_constraint.var_, .err, env);
                        continue;
                    };

                    const deferred_len_before = env.deferred_static_dispatch_constraints.items.items.len;
                    const fn_result = try self.unifyInContext(method_var, constraint.fn_var, env, .{
                        .method_type = .{
                            .constraint_var = deferred_constraint.var_,
                            .dispatcher_name = alias.ident.ident_idx,
                            .method_name = constraint.fn_name,
                        },
                    });
                    if (fn_result.isProblem()) {
                        var args_iter = self.types.iterVars(constraint_fn.args);
                        while (args_iter.next()) |arg| {
                            try self.unifyWith(arg, .err, env);
                        }
                        try self.unifyWith(deferred_constraint.var_, .err, env);
                        try self.unifyWith(constraint_fn.ret, .err, env);
                    } else if (try self.reportRecursiveStaticDispatchIfNeeded(
                        deferred_constraint.var_,
                        constraint,
                        deferred_len_before,
                        env,
                    )) {
                        try self.unifyWith(constraint_fn.ret, .err, env);
                    } else {
                        try self.reportEffectfulDispatchInExpect(constraint);
                    }
                }
                break :dispatch_resolution;
            } else if (dispatcher_content == .structure and
                (dispatcher_content.structure == .record or
                    dispatcher_content.structure == .tuple or
                    dispatcher_content.structure == .tag_union or
                    dispatcher_content.structure == .empty_record or
                    dispatcher_content.structure == .empty_tag_union))
            {
                // Anonymous structural types (records, tuples, tag unions) have implicit is_eq
                // only if all their components also support is_eq
                // iterRange re-fetches each item through the SafeList, so it stays valid even if
                // satisfyImplicitEqualityConstraint appends and reallocates the backing array.
                var constraints_iter = self.types.static_dispatch_constraints.iterRange(deferred_constraint.constraints);
                while (constraints_iter.next()) |constraint| {
                    // Check if this is a call to is_eq (anonymous types have implicit structural equality)
                    if (constraint.fn_name.eql(self.cir.idents.is_eq)) {
                        // Check if all components of this anonymous type support is_eq
                        if (try self.typeSupportsIsEq(dispatcher_content.structure)) {
                            try self.satisfyImplicitEqualityConstraint(
                                deferred_constraint.var_,
                                constraint,
                                constraint.fn_var,
                                env,
                                self.getRegionAt(deferred_constraint.var_),
                            );
                        } else {
                            // Some component doesn't support is_eq (e.g., contains a function)
                            try self.reportEqualityError(
                                deferred_constraint.var_,
                                constraint,
                                env,
                            );
                        }
                    } else if (constraint.fn_name.eql(self.cir.idents.parse_from)) {
                        const region = self.getRegionAt(deferred_constraint.var_);
                        if (try self.typeSupportsDerivedParse(dispatcher_content.structure, env, region)) {
                            try self.satisfyImplicitParseFromConstraint(
                                deferred_constraint.var_,
                                constraint,
                                constraint.fn_var,
                                env,
                                region,
                            );
                        } else {
                            try self.reportConstraintError(
                                deferred_constraint.var_,
                                constraint,
                                .not_nominal,
                                env,
                                is_numeric_default_pass,
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
                            is_numeric_default_pass,
                        );
                    }
                }
                break :dispatch_resolution;
            } else if (dispatcher_content == .flex) {
                // If the dispatcher is a flex, hold onto the constraint to try again later.
                // Note: flex vars with from_numeral constraints are validated separately
                // in checkFlexVarConstraintCompatibility after type checking completes.
                try self.scratch_deferred_static_dispatch_constraints.append(deferred_constraint);
                break :dispatch_resolution;
            } else {
                // If the root type is anything but a nominal type or anonymous structural type, push an error
                // This handles function types, which do not support any methods

                const constraints = self.types.sliceStaticDispatchConstraints(deferred_constraint.constraints);
                if (constraints.len > 0) {
                    // Report errors for ALL failing constraints, not just the first one
                    for (constraints) |constraint| {
                        // For is_eq constraints, use the specific equality error message
                        // Use ident index comparison instead of string comparison
                        if (constraint.fn_name.eql(self.cir.idents.is_eq)) {
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
                                is_numeric_default_pass,
                            );
                        }
                    }
                } else {
                    // Deferred constraint checks should always have at least one constraint.
                    // If we hit this, there's a compiler bug in how constraints are tracked.
                    std.debug.assert(false);
                }
                break :dispatch_resolution;
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

fn reportEffectfulDispatchInExpect(
    self: *Self,
    constraint: StaticDispatchConstraint,
) std.mem.Allocator.Error!void {
    if (!self.varIsEffectfulFunction(constraint.fn_var)) return;
    if (self.expect_region_by_constraint_fn_var.fetchRemove(constraint.fn_var)) |entry| {
        _ = try self.problems.appendProblem(self.gpa, .{ .effectful_expect = .{
            .region = entry.value,
        } });
    }
}

fn interpolationExprForConstraint(self: *Self, constraint: StaticDispatchConstraint) ?CIR.Expr.Idx {
    if (constraint.origin.literalKind() != .interpolation) return null;
    const expr_idx = self.constraint_expr_by_fn_var.get(constraint.fn_var) orelse return null;
    if (self.cir.store.getExpr(expr_idx) != .e_interpolation) return null;
    return expr_idx;
}

fn recordInterpolationPartTypeMismatch(self: *Self, expected_var: Var, actual_var: Var) Allocator.Error!void {
    const expected_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, expected_var);
    const actual_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, actual_var);
    _ = try self.problems.appendProblem(self.gpa, .{ .type_mismatch = .{
        .types = .{
            .expected_var = expected_var,
            .expected_snapshot = expected_snapshot,
            .actual_var = actual_var,
            .actual_snapshot = actual_snapshot,
        },
        .context = .none,
    } });
}

fn constrainInterpolationExprToStr(self: *Self, expr_idx: CIR.Expr.Idx, expected_str_var: Var, env: *Env) Allocator.Error!bool {
    const expr_var = ModuleEnv.varFrom(expr_idx);
    const resolved_expr = self.types.resolveVar(expr_var);
    if (resolved_expr.desc.content == .err) return false;

    const compatible = blk: {
        var probe = try self.beginProbe();
        defer probe.rollback();
        break :blk try self.probeUnifyWithoutRecordingProblems(expected_str_var, expr_var);
    };

    if (!compatible) {
        try self.recordInterpolationPartTypeMismatch(expected_str_var, expr_var);
        return true;
    }

    const result = try self.unify(expected_str_var, expr_var, env);
    std.debug.assert(result.isOk());
    return false;
}

fn satisfyBuiltinStrInterpolation(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
) Allocator.Error!bool {
    const expr_idx = self.interpolationExprForConstraint(constraint) orelse return false;
    const interpolation = self.cir.store.getExpr(expr_idx).e_interpolation;
    const expr_region = self.cir.store.getNodeRegion(ModuleEnv.nodeIdxFrom(expr_idx));
    const expected_str_var = try self.freshStr(env, expr_region);

    var did_err = false;
    const parts = self.cir.store.sliceExpr(interpolation.parts);
    std.debug.assert(parts.len % 2 == 0);
    var part_i: usize = 0;
    while (part_i < parts.len) : (part_i += 2) {
        did_err = (try self.constrainInterpolationExprToStr(parts[part_i], expected_str_var, env)) or did_err;
    }

    if (did_err) {
        try self.unifyWith(dispatcher_var, .err, env);
        try self.markConstraintFunctionAsError(constraint, env);
    }
    return true;
}

fn ensureCustomInterpolationPartsChecked(
    self: *Self,
    constraint: StaticDispatchConstraint,
    env: *Env,
) Allocator.Error!void {
    const expr_idx = self.interpolationExprForConstraint(constraint) orelse return;
    const entry = try self.checked_interpolation_part_constraints.getOrPut(constraint.fn_var);
    if (entry.found_existing) return;

    const item_var = self.interpolation_item_var_by_fn_var.get(constraint.fn_var) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("type checker invariant violated: checked interpolation constraint had no generated item type", .{});
        }
        unreachable;
    };

    const interpolation = self.cir.store.getExpr(expr_idx).e_interpolation;
    const parts = self.cir.store.sliceExpr(interpolation.parts);
    std.debug.assert(parts.len % 2 == 0);

    var did_err = false;
    var part_i: usize = 0;
    while (part_i < parts.len) : (part_i += 2) {
        const interpolated_var = ModuleEnv.varFrom(parts[part_i]);
        const result = try self.unify(item_var, interpolated_var, env);
        did_err = did_err or result.isProblem() or self.types.resolveVar(interpolated_var).desc.content == .err;
    }

    if (did_err) {
        try self.markConstraintFunctionAsError(constraint, env);
    }
}

/// Check if a structural type supports is_eq.
/// A type supports is_eq if:
/// - It's not a function type
/// - All of its components (record fields, tuple elements, tag payloads) also support is_eq
/// - For nominal types, check if their backing type supports is_eq
fn typeSupportsIsEq(self: *Self, flat_type: types_mod.FlatType) std.mem.Allocator.Error!bool {
    self.var_set.clearRetainingCapacity();
    return try self.typeSupportsIsEqInternal(flat_type, &self.var_set);
}

fn typeSupportsIsEqInternal(
    self: *Self,
    flat_type: types_mod.FlatType,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    return switch (flat_type) {
        // Function types do not support is_eq
        .fn_pure, .fn_effectful, .fn_unbound => false,

        // Empty types trivially support is_eq
        .empty_record, .empty_tag_union => true,

        // Records support is_eq if all field types support is_eq
        .record => |record| {
            const fields_slice = self.types.getRecordFieldsSlice(record.fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (!try self.varSupportsIsEqInternal(field_var, visited)) return false;
            }
            return true;
        },

        // Tuples support is_eq if all element types support is_eq
        .tuple => |tuple| {
            const elems = self.types.sliceVars(tuple.elems);
            for (elems) |elem_var| {
                if (!try self.varSupportsIsEqInternal(elem_var, visited)) return false;
            }
            return true;
        },

        // Tag unions support is_eq if all payload types support is_eq
        .tag_union => |tag_union| {
            const tags_slice = self.types.getTagsSlice(tag_union.tags);
            for (tags_slice.items(.args)) |tag_args| {
                const args = self.types.sliceVars(tag_args);
                for (args) |arg_var| {
                    if (!try self.varSupportsIsEqInternal(arg_var, visited)) return false;
                }
            }
            return true;
        },

        // Nominal types support is_eq if their backing type supports is_eq
        .nominal_type => |nominal| {
            if (self.nominalIsBoxType(nominal)) return false;
            const backing_var = self.types.getNominalBackingVar(nominal);
            return try self.varSupportsIsEqInternal(backing_var, visited);
        },

        // Unbound records: resolve and check the resolved type
        .record_unbound => |fields| {
            // Check each field in the unbound record
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (!try self.varSupportsIsEqInternal(field_var, visited)) return false;
            }
            return true;
        },
    };
}

fn nominalIsBoxType(self: *Self, nominal_type: types_mod.NominalType) bool {
    if (!nominal_type.originIsBuiltin()) return false;
    return switch (self.builtinNominalDeclForBuiltinSourceDecl(nominal_type.sourceDeclOptional()) orelse return false) {
        .box => true,
        else => false,
    };
}

fn varContainsUnboxedFunctionInHostedSignature(self: *Self, var_: Var) std.mem.Allocator.Error!bool {
    self.var_set.clearRetainingCapacity();
    return try self.varContainsUnboxedFunctionInHostedSignatureInternal(var_, true, &self.var_set);
}

fn varContainsUnboxedFunctionInHostedSignatureInternal(
    self: *Self,
    var_: Var,
    allow_top_fn: bool,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    // Cycle guard: recursive nominal/structural types would otherwise recurse
    // forever through their backing vars. A var already on the stack
    // contributes no new unboxed function we haven't already considered.
    if (visited.contains(resolved.var_)) return false;
    try visited.put(resolved.var_, {});
    return switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                if (!allow_top_fn) break :blk true;
                const args = self.types.sliceVars(func.args);
                for (args) |arg_var| {
                    if (try self.varContainsUnboxedFunctionInternal(arg_var, false, visited)) break :blk true;
                }
                if (try self.varContainsUnboxedFunctionInternal(func.ret, false, visited)) break :blk true;
                break :blk false;
            },
            else => try self.flatTypeContainsUnboxedFunction(s, false, visited),
        },
        .alias => |alias| try self.varContainsUnboxedFunctionInHostedSignatureInternal(self.types.getAliasBackingVar(alias), allow_top_fn, visited),
        .flex, .rigid, .err => false,
    };
}

fn varContainsUnboxedFunctionInternal(
    self: *Self,
    var_: Var,
    boxed_allowed: bool,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    if (visited.contains(resolved.var_)) return false;
    try visited.put(resolved.var_, {});
    return switch (resolved.desc.content) {
        .structure => |s| try self.flatTypeContainsUnboxedFunction(s, boxed_allowed, visited),
        .alias => |alias| try self.varContainsUnboxedFunctionInternal(self.types.getAliasBackingVar(alias), boxed_allowed, visited),
        .flex, .rigid, .err => false,
    };
}

fn flatTypeContainsUnboxedFunction(
    self: *Self,
    flat_type: types_mod.FlatType,
    boxed_allowed: bool,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    return switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => !boxed_allowed,
        .empty_record, .empty_tag_union => false,
        .record => |record| blk: {
            const fields_slice = self.types.getRecordFieldsSlice(record.fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (try self.varContainsUnboxedFunctionInternal(field_var, boxed_allowed, visited)) break :blk true;
            }
            break :blk false;
        },
        .record_unbound => |fields| blk: {
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            for (fields_slice.items(.var_)) |field_var| {
                if (try self.varContainsUnboxedFunctionInternal(field_var, boxed_allowed, visited)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |tuple| blk: {
            const elems = self.types.sliceVars(tuple.elems);
            for (elems) |elem_var| {
                if (try self.varContainsUnboxedFunctionInternal(elem_var, boxed_allowed, visited)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tag_union| blk: {
            const tags_slice = self.types.getTagsSlice(tag_union.tags);
            for (tags_slice.items(.args)) |tag_args| {
                const args = self.types.sliceVars(tag_args);
                for (args) |arg_var| {
                    if (try self.varContainsUnboxedFunctionInternal(arg_var, boxed_allowed, visited)) break :blk true;
                }
            }
            break :blk false;
        },
        .nominal_type => |nominal| blk: {
            if (self.nominalIsBoxType(nominal)) break :blk false;
            const backing_var = self.types.getNominalBackingVar(nominal);
            break :blk try self.varContainsUnboxedFunctionInternal(backing_var, boxed_allowed, visited);
        },
    };
}

fn nominalSupportsImplicitIsEq(self: *Self, nominal_type: types_mod.NominalType) std.mem.Allocator.Error!bool {
    if (self.nominalIsBuiltinNumberType(nominal_type)) return true;
    if (self.nominalIsBoxType(nominal_type)) return false;
    self.var_set.clearRetainingCapacity();
    return try self.varSupportsIsEqInternal(self.types.getNominalBackingVar(nominal_type), &self.var_set);
}

fn builtinNumKindFromNominalType(self: *const Self, nominal_type: types_mod.NominalType) ?CIR.NumKind {
    if (!nominal_type.originIsBuiltin()) return null;
    return self.builtinNumKindFromBuiltinSourceDecl(nominal_type.sourceDeclOptional());
}

fn nominalIsBuiltinStrType(self: *const Self, nominal_type: types_mod.NominalType) bool {
    if (!nominal_type.originIsBuiltin()) return false;
    if (nominal_type.sourceDeclOptional()) |source_decl| {
        if (self.builtin_ctx.builtin_indices) |indices| {
            if (source_decl == @intFromEnum(indices.str_type)) return true;
        }
    }
    const ident = nominal_type.ident.ident_idx;
    return ident.eql(self.cir.idents.str) or ident.eql(self.cir.idents.builtin_str);
}

fn typeSupportsDerivedParse(
    self: *Self,
    flat_type: types_mod.FlatType,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    return switch (flat_type) {
        .record => |record| blk: {
            const fields_slice = self.types.getRecordFieldsSlice(record.fields);
            const field_vars = fields_slice.items(.var_);
            for (field_vars) |field_var| {
                if (!try self.varSupportsDerivedParseField(field_var, env, region)) break :blk false;
            }
            break :blk true;
        },
        .tag_union => |tag_union| blk: {
            if (!try self.derivedParseTagUnionHasAnyTag(tag_union)) break :blk false;
            const tags_slice = self.types.getTagsSlice(tag_union.tags);
            for (tags_slice.items(.args)) |tag_args_range| {
                const tag_args = self.types.sliceVars(tag_args_range);
                for (tag_args) |tag_arg| {
                    if (!try self.varSupportsDerivedParseField(tag_arg, env, region)) break :blk false;
                }
            }
            break :blk try self.varSupportsDerivedParseTagExt(tag_union.ext, env, region);
        },
        .empty_record => true,
        .empty_tag_union => false,
        else => false,
    };
}

fn derivedParseTagUnionHasAnyTag(self: *Self, tag_union: types_mod.TagUnion) Allocator.Error!bool {
    if (self.types.getTagsSlice(tag_union.tags).items(.name).len > 0) return true;
    return try self.derivedParseExtHasAnyTag(tag_union.ext);
}

fn derivedParseExtHasAnyTag(self: *Self, ext_var: Var) Allocator.Error!bool {
    return switch (self.types.resolveVar(ext_var).desc.content) {
        .structure => |structure| switch (structure) {
            .empty_tag_union => false,
            .tag_union => |tag_union| try self.derivedParseTagUnionHasAnyTag(tag_union),
            else => false,
        },
        .alias => |alias| try self.derivedParseExtHasAnyTag(self.types.getAliasBackingVar(alias)),
        .err => true,
        .flex, .rigid => false,
    };
}

fn varSupportsDerivedParseTagExt(
    self: *Self,
    var_: Var,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    return switch (self.types.resolveVar(var_).desc.content) {
        .structure => |structure| switch (structure) {
            .empty_tag_union => true,
            .tag_union => |tag_union| try self.typeSupportsDerivedParse(.{ .tag_union = tag_union }, env, region),
            else => false,
        },
        .alias => |alias| try self.varSupportsDerivedParseTagExt(self.types.getAliasBackingVar(alias), env, region),
        .err => true,
        .flex, .rigid => false,
    };
}

fn varSupportsDerivedParseField(
    self: *Self,
    var_: Var,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    return switch (self.types.resolveVar(var_).desc.content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nominal| try self.nominalSupportsDerivedParseField(nominal, env, region),
            .record => |record| try self.typeSupportsDerivedParse(.{ .record = record }, env, region),
            .tag_union => |tag_union| try self.typeSupportsDerivedParse(.{ .tag_union = tag_union }, env, region),
            .empty_record => true,
            .empty_tag_union => false,
            else => false,
        },
        .alias => |alias| try self.varSupportsDerivedParseField(self.types.getAliasBackingVar(alias), env, region),
        .err => true,
        .flex, .rigid => false,
    };
}

fn nominalSupportsDerivedParseField(
    self: *Self,
    nominal: types_mod.NominalType,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    if (self.nominalIsBuiltinStrType(nominal)) return true;
    if (!self.nominalIsBuiltinTryType(nominal)) {
        return true;
    }

    const args = self.types.sliceNominalArgs(nominal);
    if (args.len != 2) return false;
    if (!try self.varIsBuiltinStr(args[0])) return false;
    return try self.varCanParseMissingError(args[1], env, region);
}

fn varIsBuiltinStr(self: *Self, var_: Var) std.mem.Allocator.Error!bool {
    return switch (self.types.resolveVar(var_).desc.content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nominal| self.nominalIsBuiltinStrType(nominal),
            else => false,
        },
        .alias => |alias| try self.varIsBuiltinStr(self.types.getAliasBackingVar(alias)),
        .err => true,
        .flex, .rigid => false,
    };
}

fn nominalIsBuiltinTryType(self: *const Self, nominal_type: types_mod.NominalType) bool {
    if (!nominal_type.originIsBuiltin()) return false;
    return switch (self.builtinNominalDeclForBuiltinSourceDecl(nominal_type.sourceDeclOptional()) orelse return false) {
        .try_type => true,
        else => false,
    };
}

fn varCanParseMissingError(
    self: *Self,
    var_: Var,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    if (self.types.resolveVar(var_).desc.content == .err) return true;

    const missing_tag_name = try @constCast(self.cir).insertIdent(base.Ident.for_text("Missing"));
    const missing_tag = try self.types.mkTag(missing_tag_name, &.{});
    const ext_var = try self.freshFromContent(.{ .structure = .empty_tag_union }, env, region);
    const missing_content = try self.types.mkTagUnion(&.{missing_tag}, ext_var);
    const missing_var = try self.freshFromContent(missing_content, env, region);
    const result = try self.unify(var_, missing_var, env);
    return result.isOk();
}

fn nominalIsBuiltinNumberType(self: *Self, nominal_type: types_mod.NominalType) bool {
    return self.builtinNumKindFromNominalType(nominal_type) != null;
}

const BuiltinFromNumeralLiteralProblem = enum {
    fractional_integer,
    negative_unsigned,
    out_of_range,
};

fn validateBuiltinFromNumeralLiteral(
    num_kind: CIR.NumKind,
    num_literal: types_mod.NumeralInfo,
) ?BuiltinFromNumeralLiteralProblem {
    return switch (num_kind) {
        .u8 => validateUnsignedFromNumeralLiteral(u8, num_literal),
        .u16 => validateUnsignedFromNumeralLiteral(u16, num_literal),
        .u32 => validateUnsignedFromNumeralLiteral(u32, num_literal),
        .u64 => validateUnsignedFromNumeralLiteral(u64, num_literal),
        .u128 => validateUnsignedFromNumeralLiteral(u128, num_literal),
        .i8 => validateSignedFromNumeralLiteral(i8, num_literal),
        .i16 => validateSignedFromNumeralLiteral(i16, num_literal),
        .i32 => validateSignedFromNumeralLiteral(i32, num_literal),
        .i64 => validateSignedFromNumeralLiteral(i64, num_literal),
        .i128 => validateSignedFromNumeralLiteral(i128, num_literal),
        .dec => validateDecFromNumeralLiteral(num_literal),
        .f32, .f64 => null,
        .num_unbound, .int_unbound => null,
    };
}

fn skipDefaultedDecIntegerLiteralValidation(
    is_numeric_default_pass: bool,
    num_kind: CIR.NumKind,
    constraint: StaticDispatchConstraint,
) bool {
    if (!is_numeric_default_pass or num_kind != .dec) return false;
    const num_literal = constraint.origin.numeralInfo() orelse return false;
    return !num_literal.is_fractional;
}

fn validateUnsignedFromNumeralLiteral(
    comptime T: type,
    num_literal: types_mod.NumeralInfo,
) ?BuiltinFromNumeralLiteralProblem {
    if (num_literal.is_fractional) return .fractional_integer;
    // `is_negative` is a syntactic flag (a leading `-`); guard on the actual
    // magnitude so `-0` (a valid unsigned value) is not rejected. Real
    // negatives have a nonzero magnitude, and a large negative whose i128
    // representation overflowed is still nonzero, so this stays correct.
    if (num_literal.is_negative and num_literal.toI128() != 0) return .negative_unsigned;

    const value = if (num_literal.is_u128) blk: {
        break :blk num_literal.toU128();
    } else blk: {
        const signed_value = num_literal.toI128();
        if (signed_value < 0) return .negative_unsigned;
        break :blk @as(u128, @intCast(signed_value));
    };

    if (value > @as(u128, @intCast(std.math.maxInt(T)))) return .out_of_range;
    return null;
}

test "unsuffixed positive integer literal validates against unsigned builtin type" {
    const info = types_mod.NumeralInfo.fromI128(42, false, false, Region.zero());
    try std.testing.expectEqual(@as(?BuiltinFromNumeralLiteralProblem, null), validateBuiltinFromNumeralLiteral(.u64, info));
}

fn validateSignedFromNumeralLiteral(
    comptime T: type,
    num_literal: types_mod.NumeralInfo,
) ?BuiltinFromNumeralLiteralProblem {
    if (num_literal.is_fractional) return .fractional_integer;

    if (num_literal.is_u128) {
        if (num_literal.toU128() > @as(u128, @intCast(std.math.maxInt(T)))) return .out_of_range;
        return null;
    }

    const value = num_literal.toI128();
    if (value < @as(i128, std.math.minInt(T)) or value > @as(i128, std.math.maxInt(T))) {
        return .out_of_range;
    }
    return null;
}

fn validateDecFromNumeralLiteral(
    num_literal: types_mod.NumeralInfo,
) ?BuiltinFromNumeralLiteralProblem {
    if (num_literal.fits_dec) |fits| {
        return if (fits) null else .out_of_range;
    }

    if (num_literal.frac_requirements) |requirements| {
        if (!requirements.fits_in_dec) return .out_of_range;
    }

    if (num_literal.is_fractional) return null;

    const max_whole_dec: u128 = 170141183460469231731;
    if (num_literal.is_u128) {
        if (num_literal.toU128() > max_whole_dec) return .out_of_range;
        return null;
    }

    const value = num_literal.toI128();
    const max_signed: i128 = @intCast(max_whole_dec);
    if (value < -max_signed or value > max_signed) return .out_of_range;
    return null;
}

fn reportInvalidBuiltinFromNumeralLiteral(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    num_kind: CIR.NumKind,
    env: *Env,
) Allocator.Error!bool {
    const num_literal = constraint.origin.numeralInfo() orelse return false;
    if (!try self.reportInvalidBuiltinFromNumeralInfo(dispatcher_var, num_kind, num_literal, env)) return false;

    try self.markConstraintFunctionAsError(constraint, env);
    return true;
}

fn reportInvalidBuiltinFromNumeralInfo(
    self: *Self,
    dispatcher_var: Var,
    num_kind: CIR.NumKind,
    num_literal: types_mod.NumeralInfo,
    env: *Env,
) Allocator.Error!bool {
    const literal_problem = if (num_kind == .dec)
        validateDecFromNumeralLiteral(num_literal)
    else
        validateBuiltinFromNumeralLiteral(num_kind, num_literal);
    if (literal_problem == null) return false;

    const expected_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, dispatcher_var);
    _ = try self.problems.appendProblem(self.gpa, .{ .invalid_numeric_literal = .{
        .literal_var = dispatcher_var,
        .expected_type = expected_snapshot,
        .is_fractional = num_literal.is_fractional,
        .region = num_literal.region,
    } });

    try self.unifyWith(dispatcher_var, .err, env);
    return true;
}

fn validateFromNumeralLiteralForBuiltinNominal(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    nominal_type: types_mod.NominalType,
    env: *Env,
    is_numeric_default_pass: bool,
) Allocator.Error!bool {
    if (constraint.origin != .from_literal) return true;
    const num_kind = self.builtinNumKindFromNominalType(nominal_type) orelse return true;

    if (skipDefaultedDecIntegerLiteralValidation(is_numeric_default_pass, num_kind, constraint)) return true;

    return !try self.reportInvalidBuiltinFromNumeralLiteral(dispatcher_var, constraint, num_kind, env);
}

fn validateFromNumeralLiteralForBuiltinAlias(
    _: *Self,
    _: Var,
    _: StaticDispatchConstraint,
    _: types_mod.Alias,
    _: *Env,
    _: bool,
) Allocator.Error!bool {
    return true;
}

fn satisfyImplicitEqualityConstraint(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    constraint_fn_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const resolved_constraint = self.types.resolveVar(constraint_fn_var);
    const resolved_func = resolved_constraint.desc.content.unwrapFunc() orelse {
        try self.unifyWith(constraint_fn_var, .err, env);
        return;
    };

    const args = self.types.sliceVars(resolved_func.args);
    if (args.len != 2) {
        std.debug.panic(
            "type checker invariant violated: implicit equality constraint expected 2 args, found {d}",
            .{args.len},
        );
    }

    // Read both arg vars before unifying: the first unify can append fresh
    // vars and reallocate the backing array, dangling the `args` slice.
    const arg0 = args[0];
    const arg1 = args[1];
    _ = try self.unify(dispatcher_var, arg0, env);
    _ = try self.unify(dispatcher_var, arg1, env);
    _ = try self.unify(try self.freshBool(env, region), resolved_func.ret, env);
    self.rewriteImplicitEqMethodCallAsStructuralEq(constraint);
}

fn satisfyImplicitParseFromConstraint(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    constraint_fn_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!void {
    const resolved_constraint = self.types.resolveVar(constraint_fn_var);
    const resolved_func = resolved_constraint.desc.content.unwrapFunc() orelse {
        try self.unifyWith(constraint_fn_var, .err, env);
        return;
    };

    const args = self.types.sliceVars(resolved_func.args);
    if (args.len != 1) {
        try self.unifyWith(constraint_fn_var, .err, env);
        return;
    }

    // Copy the slot arg before adding fresh vars; the args slice may dangle.
    const slot_var = args[0];
    const err_var = try self.fresh(env, region);
    const parse_result_var = try self.freshParseResultTryVar(dispatcher_var, slot_var, err_var, env, region);
    const ret_result = try self.unifyInContext(parse_result_var, resolved_func.ret, env, .none);
    if (ret_result.isProblem()) {
        try self.markConstraintFunctionAsError(constraint, env);
        return;
    }

    self.var_set.clearRetainingCapacity();
    if (!try self.validateDerivedParseVar(dispatcher_var, slot_var, err_var, constraint, env, region, &self.var_set, .shape)) {
        try self.reportConstraintError(dispatcher_var, constraint, .not_nominal, env, false);
    }
}

fn freshParseResultTryVar(
    self: *Self,
    value_var: Var,
    rest_var: Var,
    err_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!Var {
    const ok_var = try self.freshParseResultOkVar(value_var, rest_var, env, region);
    return try self.freshFromContent(try self.mkTryContent(ok_var, err_var, env), env, region);
}

fn freshParseResultOkVar(
    self: *Self,
    value_var: Var,
    rest_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!Var {
    const rest_name = try @constCast(self.cir).insertIdent(base.Ident.for_text("rest"));
    const value_name = try @constCast(self.cir).insertIdent(base.Ident.for_text("value"));
    const fields = [_]types_mod.RecordField{
        .{ .name = rest_name, .var_ = rest_var },
        .{ .name = value_name, .var_ = value_var },
    };
    const fields_range = try self.types.appendRecordFields(&fields);
    const ext_var = try self.freshFromContent(.{ .structure = .empty_record }, env, region);
    return try self.freshFromContent(.{ .structure = .{ .record = .{
        .fields = fields_range,
        .ext = ext_var,
    } } }, env, region);
}

const DerivedParseContext = enum {
    shape,
    record_field,
};

const ParseFormatMethodVar = struct {
    var_: Var,
    dispatcher_name: Ident.Idx,
};

fn parseFormatMethodName(self: *Self, decl: BuiltinParseSpecDecl) Allocator.Error!Ident.Idx {
    const text = switch (decl) {
        .str => "parse_str",
        .record => "parse_record",
        .tag_union => "parse_tag_union",
    };
    return try @constCast(self.cir).insertIdent(base.Ident.for_text(text));
}

fn parseFormatMethodVarForSlot(
    self: *Self,
    slot_var: Var,
    method_name: Ident.Idx,
    env: *Env,
    region: Region,
) Allocator.Error!?ParseFormatMethodVar {
    const resolved = self.types.resolveVar(slot_var);
    return switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nominal| blk: {
                const original_env, const is_this_module = self.ownerEnvForOriginModule(
                    nominal.origin_module,
                    nominal.sourceDeclOptional(),
                    nominal.originIsBuiltin(),
                    "parse_from format method",
                );
                const binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
                    self.cir,
                    nominal.sourceDeclOptional(),
                    method_name,
                ) orelse break :blk null;
                break :blk .{
                    .var_ = (try self.methodVarFromOriginalEnv(
                        original_env,
                        is_this_module,
                        binding.type_node_idx,
                        nominal.ident.ident_idx,
                        env,
                        region,
                    )).var_,
                    .dispatcher_name = nominal.ident.ident_idx,
                };
            },
            else => null,
        },
        .alias => |alias| blk: {
            const original_env, const is_this_module = self.ownerEnvForOriginModule(
                alias.origin_module,
                alias.source_decl.toOptional(),
                alias.source_decl.originIsBuiltin(),
                "parse_from format method",
            );
            const binding = original_env.lookupMethodBindingFromTwoEnvsAndDeclConst(
                alias.source_decl.toOptional(),
                self.cir,
                method_name,
            ) orelse break :blk null;
            break :blk .{
                .var_ = (try self.methodVarFromOriginalEnv(
                    original_env,
                    is_this_module,
                    binding.type_node_idx,
                    alias.ident.ident_idx,
                    env,
                    region,
                )).var_,
                .dispatcher_name = alias.ident.ident_idx,
            };
        },
        .err => null,
        .flex, .rigid => null,
    };
}

fn validateParseFormatMethod(
    self: *Self,
    slot_var: Var,
    shape_var: Var,
    spec_decl: BuiltinParseSpecDecl,
    err_var: Var,
    env: *Env,
    region: Region,
) Allocator.Error!bool {
    const method_name = try self.parseFormatMethodName(spec_decl);
    const method = try self.parseFormatMethodVarForSlot(slot_var, method_name, env, region) orelse return false;
    const spec_var = try self.mkParseSpecVar(spec_decl, shape_var, env, region);
    const expected_ret = try self.freshParseResultTryVar(shape_var, slot_var, err_var, env, region);
    const expected_fn = try self.freshFromContent(try self.types.mkFuncUnbound(&.{ spec_var, slot_var }, expected_ret), env, region);
    const result = try self.unifyInContext(method.var_, expected_fn, env, .{
        .method_type = .{
            .constraint_var = slot_var,
            .dispatcher_name = method.dispatcher_name,
            .method_name = method_name,
        },
    });
    return result.isOk();
}

fn validateDerivedParseVar(
    self: *Self,
    var_: Var,
    slot_var: Var,
    err_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
    context: DerivedParseContext,
) Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    return switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .nominal_type => |nominal| try self.validateDerivedParseNominal(var_, nominal, slot_var, err_var, constraint, env, region, context),
            .record => |record| blk: {
                if (visited.contains(resolved.var_)) break :blk true;
                try visited.put(resolved.var_, {});
                break :blk try self.validateDerivedParseRecord(var_, record.fields, slot_var, err_var, constraint, env, region, visited);
            },
            .tag_union => |tag_union| blk: {
                if (visited.contains(resolved.var_)) break :blk true;
                try visited.put(resolved.var_, {});
                break :blk try self.validateDerivedParseTagUnion(var_, tag_union, slot_var, err_var, constraint, env, region, visited);
            },
            .empty_record => true,
            .empty_tag_union => false,
            else => false,
        },
        .alias => |alias| try self.validateDerivedParseVar(self.types.getAliasBackingVar(alias), slot_var, err_var, constraint, env, region, visited, context),
        .err => true,
        .flex, .rigid => false,
    };
}

fn validateDerivedParseRecord(
    self: *Self,
    record_var: Var,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
    slot_var: Var,
    err_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    const fields = self.types.getRecordFieldsSlice(fields_range);
    if (fields.items(.var_).len != 0 and
        !try self.validateParseFormatMethod(slot_var, record_var, .record, err_var, env, region))
    {
        return false;
    }
    for (fields.items(.var_)) |field_var| {
        if (!try self.validateDerivedParseVar(field_var, slot_var, err_var, constraint, env, region, visited, .record_field)) return false;
    }
    return true;
}

fn validateDerivedParseTagUnion(
    self: *Self,
    tag_union_var: Var,
    tag_union: types_mod.TagUnion,
    slot_var: Var,
    err_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    if (!try self.derivedParseTagUnionHasAnyTag(tag_union)) return false;
    if (!try self.validateParseFormatMethod(slot_var, tag_union_var, .tag_union, err_var, env, region)) return false;

    const tags = self.types.getTagsSlice(tag_union.tags);
    for (tags.items(.args)) |tag_args_range| {
        const tag_args = self.types.sliceVars(tag_args_range);
        for (tag_args) |tag_arg| {
            if (!try self.validateDerivedParseVar(tag_arg, slot_var, err_var, constraint, env, region, visited, .shape)) return false;
        }
    }
    return try self.validateDerivedParseTagExt(tag_union.ext, slot_var, err_var, constraint, env, region, visited);
}

fn validateDerivedParseTagExt(
    self: *Self,
    ext_var: Var,
    slot_var: Var,
    err_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
    region: Region,
    visited: *std.AutoHashMap(Var, void),
) Allocator.Error!bool {
    return switch (self.types.resolveVar(ext_var).desc.content) {
        .structure => |structure| switch (structure) {
            .empty_tag_union => true,
            .tag_union => |tag_union| try self.validateDerivedParseTagUnion(ext_var, tag_union, slot_var, err_var, constraint, env, region, visited),
            else => false,
        },
        .alias => |alias| try self.validateDerivedParseTagExt(self.types.getAliasBackingVar(alias), slot_var, err_var, constraint, env, region, visited),
        .err => true,
        .flex, .rigid => false,
    };
}

fn validateDerivedParseNominal(
    self: *Self,
    nominal_var: Var,
    nominal: types_mod.NominalType,
    slot_var: Var,
    err_var: Var,
    constraint: StaticDispatchConstraint,
    env: *Env,
    region: Region,
    context: DerivedParseContext,
) Allocator.Error!bool {
    if (self.nominalIsBuiltinStrType(nominal)) {
        return context == .record_field or
            try self.validateParseFormatMethod(slot_var, nominal_var, .str, err_var, env, region);
    }
    if (self.nominalIsBuiltinTryType(nominal)) {
        if (context != .record_field) return false;
        const args = self.types.sliceNominalArgs(nominal);
        if (args.len != 2) return false;
        if (!try self.varIsBuiltinStr(args[0])) return false;
        return try self.varCanParseMissingError(args[1], env, region);
    }

    const original_env, const is_this_module = self.ownerEnvForOriginModule(
        nominal.origin_module,
        nominal.sourceDeclOptional(),
        nominal.originIsBuiltin(),
        "parse_from nominal field",
    );
    const method_binding = original_env.lookupMethodBindingFromEnvAndDeclConst(
        self.cir,
        nominal.sourceDeclOptional(),
        self.cir.idents.parse_from,
    ) orelse return false;

    const method_type_var: Var = ModuleEnv.varFrom(method_binding.type_node_idx);
    const method_var = if (is_this_module) blk: {
        if (self.types.resolveVar(method_type_var).desc.rank == .generalized) {
            break :blk try self.instantiateVar(method_type_var, env, .use_last_var);
        }
        break :blk method_type_var;
    } else blk: {
        const copied_var = try self.copyVar(method_type_var, original_env, region);
        break :blk try self.instantiateVar(copied_var, env, .{ .explicit = region });
    };

    const expected_ret = try self.freshParseResultTryVar(nominal_var, slot_var, err_var, env, region);
    const expected_fn = try self.freshFromContent(try self.types.mkFuncUnbound(&.{slot_var}, expected_ret), env, region);
    const result = try self.unifyInContext(method_var, expected_fn, env, .{
        .method_type = .{
            .constraint_var = nominal_var,
            .dispatcher_name = nominal.ident.ident_idx,
            .method_name = constraint.fn_name,
        },
    });
    return result.isOk();
}

/// Check if a type variable supports is_eq by resolving it and checking its content
fn varSupportsIsEq(self: *Self, var_: Var) std.mem.Allocator.Error!bool {
    self.var_set.clearRetainingCapacity();
    return try self.varSupportsIsEqInternal(var_, &self.var_set);
}

fn varSupportsIsEqInternal(
    self: *Self,
    var_: Var,
    visited: *std.AutoHashMap(Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);
    if (visited.contains(resolved.var_)) return true;
    try visited.put(resolved.var_, {});

    return switch (resolved.desc.content) {
        .structure => |s| try self.typeSupportsIsEqInternal(s, visited),
        // Flex/rigid vars: we optimistically assume they support is_eq.
        // This is sound because if the variable is later unified with a type
        // that doesn't support is_eq (like a function), unification will fail.
        .flex, .rigid => true,
        // Aliases: check the underlying type
        .alias => |alias| try self.varSupportsIsEqInternal(self.types.getAliasBackingVar(alias), visited),
        // Error types: allow them to proceed
        .err => true,
    };
}

/// Check if a flex var has incompatible constraints and report errors.
/// This is called after type-checking to catch cases like `!3` where a flex var
/// has both `from_literal` (numeric) and `not` (Bool only) constraints. If the
/// flex var carries a `from_literal` constraint, validate that the canonical
/// default (`Dec`) has a method for each of its other constraints.
fn checkFlexVarConstraintCompatibility(self: *Self, var_: Var, env: *Env, is_numeric_default_pass: bool) Allocator.Error!void {
    const resolved = self.types.resolveVar(var_);
    if (resolved.desc.content != .flex) return;

    const flex = resolved.desc.content.flex;
    const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
    if (constraints.len == 0) return;

    // Find the literal-origin constraint that determines the default type.
    var has_from_numeral = false;
    var has_str_defaultable_literal = false;
    for (constraints) |c| {
        switch (c.origin) {
            .from_literal => |lit| switch (lit) {
                .numeral => has_from_numeral = true,
                .quote, .interpolation => has_str_defaultable_literal = true,
            },
            else => {},
        }
    }
    if (!has_from_numeral and !has_str_defaultable_literal) return;

    // This flex will default to Dec (numerals) or Str (quotes/interpolations).
    // Validate that all other constraints can be satisfied by the default type.
    const builtin_env = self.builtin_ctx.builtin_module orelse return;
    const indices = self.builtin_ctx.builtin_indices orelse return;
    const default_type_stmt = if (has_from_numeral) indices.dec_type else indices.str_type;

    for (constraints) |constraint| {
        // Skip the literal-origin constraint the default type satisfies by
        // definition. (With both literal kinds present, the var defaults to Dec and
        // the from_quote constraint must still be validated against it.)
        switch (constraint.origin) {
            .from_literal => |lit| switch (lit) {
                .numeral => if (has_from_numeral) continue,
                .quote, .interpolation => if (!has_from_numeral) continue,
            },
            else => {},
        }

        // Check if the default type has this method
        const method_binding = builtin_env.lookupMethodBindingFromTwoEnvsAndDeclConst(
            @intFromEnum(default_type_stmt),
            self.cir,
            constraint.fn_name,
        );
        if (method_binding == null) {
            try self.reportConstraintError(
                var_,
                constraint,
                .{ .missing_method = .nominal },
                env,
                is_numeric_default_pass,
            );
        }
    }
}

fn checkAllFromNumeralFlexConstraintCompatibility(
    self: *Self,
    env: *Env,
    is_numeric_default_pass: bool,
) Allocator.Error!void {
    // Iterate the tracked open-literal worklist, not the whole store;
    // `checkFlexVarConstraintCompatibility` re-resolves and no-ops on non-literal
    // or already-resolved vars.
    const literal_count = self.open_literal_vars.items.len;
    var i: usize = 0;
    while (i < literal_count) : (i += 1) {
        try self.checkFlexVarConstraintCompatibility(self.open_literal_vars.items[i], env, is_numeric_default_pass);
    }
}

/// Record a branch-body-vs-expected `type_mismatch` with roles fixed by the
/// caller: the branch body is the "actual", the shared return is the "expected".
/// This is what makes the rendered message + region correct regardless of the
/// merge operand order used to commit the types.
fn recordBranchTypeMismatch(self: *Self, body_var: Var, expected_ret: Var, ctx: problem.Context) std.mem.Allocator.Error!void {
    const expected_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, expected_ret);
    const actual_snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, body_var);
    _ = try self.problems.appendProblem(self.gpa, .{ .type_mismatch = .{
        .types = .{
            .expected_var = expected_ret,
            .expected_snapshot = expected_snapshot,
            .actual_var = body_var,
            .actual_snapshot = actual_snapshot,
        },
        .context = ctx,
    } });
}

/// Probe whether `body_var` can unify with `target`, recording and committing
/// nothing: run the non-recording raw-unifier probe inside a `beginProbe` scope,
/// then always roll back. Nothing in the live solver state is disturbed.
fn probeBranchCompatible(self: *Self, body_var: Var, target: Var) std.mem.Allocator.Error!bool {
    var probe = try self.beginProbe();
    defer probe.rollback();
    return try self.probeUnifyWithoutRecordingProblems(body_var, target);
}

/// Record a branch-vs-`mismatch_against` diagnostic (actual = branch body,
/// region on it) and locally poison the branch so it does not cascade.
/// `mismatch_against` is rendered as "the previous branch(es) result" — either
/// the annotated return type or the branch accumulator, whichever the body
/// failed against. `expected_ret` is always used to mark the erroneous branch.
fn reportBranchMismatchAndPoison(
    self: *Self,
    body_expr_idx: CIR.Expr.Idx,
    body_var: Var,
    mismatch_against: Var,
    expected_ret: Var,
    ctx: problem.Context,
    env: *Env,
) std.mem.Allocator.Error!void {
    try self.recordBranchTypeMismatch(body_var, mismatch_against, ctx);
    try self.markErroneousBranchWithExpected(body_expr_idx, expected_ret, env);
    try self.unifyWith(body_var, .err, env);
}

/// Check one if/match branch body against the shared expected return type, and
/// fold a compatible body into the per-expression accumulator `acc`.
///
/// `acc` is a fresh var owned by the enclosing `checkIfElseExpr` /
/// `checkMatchExpr`; it accumulates the meet of every compatible branch body and
/// is unified with the shared `expected_ret` exactly once, at the end of the
/// expression. Branches therefore never merge into `expected_ret` directly: the
/// shared annotated return var is no longer the per-branch union-find hub, so
/// the "which operand survives" question (see `store.union_`) can no longer
/// affect branch checking, and one earlier branch can no longer refine the
/// annotation seen by a later branch.
///
/// Branch bodies are checked in two steps, so a failed fold can never corrupt
/// `acc` (or `expected_ret`):
///   1. A pure, fully-rolled-back probe (see `probeBranchCompatible`) against the
///      pristine `expected_ret`: catches a body that does not match the annotated
///      return type. Reported against `expected_ret`. This MUST stay a
///      rolled-back probe — branches never merge into the shared annotation var
///      (see above).
///   2. One real unify against `acc` inside a `CommitProbe`: the fold and the
///      compatibility check are the same operation. Success commits in place — the
///      wrapper already propagated regions/rank/deferred-constraints into the live
///      `env`, so nothing is redone. Failure catches a body that matches the
///      annotation yet diverges from an earlier sibling already folded in — only
///      observable when the annotation is LOOSER than the accumulated branches,
///      e.g. an inferred `_` return, where step (1) passes for every branch but
///      the branches still disagree. The commit-probe rollback then restores every
///      store and buffer (including the unifier's own mismatch problem and
///      snapshots), leaving `body_var` and `acc` pristine, and the mismatch is
///      reported against `acc`.
/// `expected_ret` stays untouched on every path.
fn checkBranchBodyAgainstExpected(
    self: *Self,
    body_expr_idx: CIR.Expr.Idx,
    expected_ret: Var,
    acc: Var,
    ctx: problem.Context,
    env: *Env,
) std.mem.Allocator.Error!void {
    const body_var = ModuleEnv.varFrom(body_expr_idx);

    // Cheap guards: already-unified or already-erroneous pairs need no work.
    // If body already resolves to expected, it is correctly constrained and we
    // must NOT fold it into `acc` — that would run `unify(expected_ret, acc)` and
    // redirect the shared expected var into the accumulator (the very mutation
    // this accumulator design exists to avoid).
    const rb = self.types.resolveVar(body_var);
    const re = self.types.resolveVar(expected_ret);
    if (rb.var_ == re.var_) return;
    if (rb.desc.content == .err or re.desc.content == .err) return;

    // Probe (1): does the body match the annotated return type?
    if (!try self.probeBranchCompatible(body_var, expected_ret)) {
        try self.reportBranchMismatchAndPoison(body_expr_idx, body_var, expected_ret, expected_ret, ctx, env);
        return;
    }

    // Step (2): fold the body into the accumulator with one real unify inside a
    // commit-probe. Success keeps the merge (regions/rank/deferred constraints
    // already propagated into env); failure falls through to the report below
    // AFTER the scope's rollback, so the snapshots it takes see the pristine vars.
    {
        var commit_probe = try self.beginCommitProbe(env);
        var committed = false;
        defer if (!committed) commit_probe.rollback();

        const result = try self.unifyInContext(body_var, acc, env, ctx);
        if (result.isOk()) {
            committed = true;
            commit_probe.commit();
            return;
        }
    }

    // The body matches the annotation (step 1) yet diverges from an earlier sibling
    // already folded into `acc`; rollback has restored everything the failed unify
    // touched. (Distinct from a step-(1) failure only when the annotation is looser
    // than `acc`.)
    try self.reportBranchMismatchAndPoison(body_expr_idx, body_var, acc, expected_ret, ctx, env);
}

fn markErroneousBranchWithExpected(self: *Self, expr_idx: CIR.Expr.Idx, expected_ret: Var, env: *Env) std.mem.Allocator.Error!void {
    if (self.cir.store.getExpr(expr_idx) == .e_runtime_error) return;

    try self.erroneous_value_exprs.put(self.gpa, expr_idx, {});

    const expr_var = ModuleEnv.varFrom(expr_idx);
    const region = self.cir.store.getExprRegion(expr_idx);
    const redirected_ret = try self.fresh(env, region);
    _ = try self.unifyInContext(redirected_ret, expected_ret, env, .none);

    try self.types.dangerousSetVarRedirect(expr_var, redirected_ret);
}

/// Check if a type variable contains any error types anywhere in its structure.
/// This is used to determine if an expression's type contains errors, in which case
/// we should use the annotation type for the pattern instead of the expression type.
/// This handles cases like `Error -> Error` where the root is a function but the
/// argument/return types are errors.
fn varContainsError(self: *Self, var_: Var, visited: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!bool {
    const resolved = self.types.resolveVar(var_);

    // Check if we've already visited this var (cycle detection)
    if (visited.contains(resolved.var_)) {
        return false;
    }
    try visited.put(resolved.var_, {});

    return switch (resolved.desc.content) {
        .err => true,
        .flex, .rigid => false,
        .alias => |alias| try self.varContainsError(self.types.getAliasBackingVar(alias), visited),
        .structure => |flat_type| try self.flatTypeContainsError(flat_type, visited),
    };
}

/// Check if a flat type contains any error types
fn flatTypeContainsError(self: *Self, flat_type: FlatType, visited: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!bool {
    return switch (flat_type) {
        .tuple => |tuple| try self.varsContainError(self.types.sliceVars(tuple.elems), visited),
        .nominal_type => |nominal| blk: {
            var arg_iter = self.types.iterNominalArgs(nominal);
            while (arg_iter.next()) |arg_var| {
                if (try self.varContainsError(arg_var, visited)) break :blk true;
            }
            break :blk try self.varContainsError(self.types.getNominalBackingVar(nominal), visited);
        },
        .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
            if (try self.varsContainError(self.types.sliceVars(func.args), visited)) break :blk true;
            break :blk try self.varContainsError(func.ret, visited);
        },
        .record => |record| blk: {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            if (try self.varsContainError(fields.items(.var_), visited)) break :blk true;
            break :blk try self.varContainsError(record.ext, visited);
        },
        .record_unbound => |fields| blk: {
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            break :blk try self.varsContainError(fields_slice.items(.var_), visited);
        },
        .tag_union => |tag_union| blk: {
            const tags = self.types.getTagsSlice(tag_union.tags);
            for (tags.items(.args)) |tag_args| {
                if (try self.varsContainError(self.types.sliceVars(tag_args), visited)) break :blk true;
            }
            break :blk try self.varContainsError(tag_union.ext, visited);
        },
        .empty_record, .empty_tag_union => false,
    };
}

fn has_can_error_diagnostics(self: *Self) bool {
    const diagnostics = self.cir.store.sliceDiagnostics(self.cir.diagnostics);
    for (diagnostics) |diagnostic_idx| {
        const diagnostic = self.cir.store.getDiagnostic(diagnostic_idx);
        switch (diagnostic) {
            .shadowing_warning,
            .unused_variable,
            .used_underscore_variable,
            .type_shadowed_warning,
            .unused_type_var_name,
            .type_var_marked_unused,
            .underscore_in_type_declaration,
            .module_header_deprecated,
            .deprecated_number_suffix,
            => {},
            else => return true,
        }
    }
    return false;
}

/// Check if any of the given vars contain errors
fn varsContainError(self: *Self, vars: []const Var, visited: *std.AutoHashMap(Var, void)) std.mem.Allocator.Error!bool {
    for (vars) |v| {
        if (try self.varContainsError(v, visited)) return true;
    }
    return false;
}

/// Mark a constraint function's return type as error
fn markConstraintFunctionAsError(self: *Self, constraint: StaticDispatchConstraint, env: *Env) Allocator.Error!void {
    const resolved_constraint = self.types.resolveVar(constraint.fn_var);
    const resolved_func = resolved_constraint.desc.content.unwrapFunc() orelse {
        try self.unifyWith(constraint.fn_var, .err, env);
        return;
    };
    // Use unify instead of unifyWith because the constraint's return type may be at a
    // different rank than the current env (e.g., from a local declaration that wasn't
    // generalized due to the value restriction).
    const err_var = try self.freshFromContent(.err, env, self.getRegionAt(resolved_func.ret));
    _ = try self.unify(resolved_func.ret, err_var, env);
}

/// Find the source region of the string literal whose from_quote constraint
/// resolved into `dispatcher_var`, for error reporting. The constraint itself
/// carries no region, but the literal's dispatch plan records its source node.
fn quoteLiteralRegionForDispatcher(self: *Self, constraint: StaticDispatchConstraint, dispatcher_var: Var) ?Region {
    const kind = constraint.origin.literalKind() orelse return null;
    if (kind != .quote) return null;
    const resolved_dispatcher = self.types.resolveVar(dispatcher_var).var_;
    for (self.cir.quote_dispatch_plans.items.items) |plan| {
        const target: Var = @enumFromInt(plan.target_var);
        if (self.types.resolveVar(target).var_ != resolved_dispatcher) continue;
        return self.cir.store.getNodeRegion(@enumFromInt(plan.node_idx));
    }
    return null;
}

const ReportedConstraintError = struct {
    dispatcher: Var,
    fn_name: Ident.Idx,
};

fn reportRecursiveStaticDispatchIfNeeded(
    self: *Self,
    dispatcher_var: Var,
    constraint: StaticDispatchConstraint,
    deferred_len_before: usize,
    env: *Env,
) Allocator.Error!bool {
    if (constraint.origin != .where_clause) return false;
    if (!constraint.fn_name.eql(self.cir.idents.from_interpolation)) return false;

    const items = env.deferred_static_dispatch_constraints.items.items;
    if (deferred_len_before == items.len) return false;

    const dispatcher_resolved = self.types.resolveVar(dispatcher_var).var_;
    var deferred_index = deferred_len_before;
    while (deferred_index < items.len) : (deferred_index += 1) {
        const deferred = items[deferred_index];
        if (self.types.resolveVar(deferred.var_).var_ != dispatcher_resolved) continue;

        const new_constraints = self.types.sliceStaticDispatchConstraints(deferred.constraints);
        for (new_constraints) |new_constraint| {
            if (new_constraint.origin != .where_clause) continue;
            if (!new_constraint.fn_name.eql(constraint.fn_name)) continue;
            if (!try self.staticDispatchConstraintFunctionsCanUnify(
                constraint.fn_var,
                new_constraint.fn_var,
            )) {
                continue;
            }

            const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, dispatcher_var);
            _ = try self.problems.appendProblem(self.cir.gpa, .{ .static_dispatch = .{
                .recursive_dispatch = .{
                    .dispatcher_snapshot = snapshot,
                    .fn_var = constraint.fn_var,
                    .method_name = constraint.fn_name,
                },
            } });

            try self.unifyWith(dispatcher_var, .err, env);
            try self.markConstraintFunctionAsError(constraint, env);
            try self.markConstraintFunctionAsError(new_constraint, env);
            return true;
        }
    }
    return false;
}

fn staticDispatchConstraintFunctionsCanUnify(
    self: *Self,
    left: Var,
    right: Var,
) Allocator.Error!bool {
    var probe = try self.beginProbe();
    defer probe.rollback();
    return try self.probeUnifyWithoutRecordingProblems(left, right);
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
    is_numeric_default_pass: bool,
) Allocator.Error!void {
    const dedup_key = ReportedConstraintError{
        .dispatcher = self.types.resolveVar(dispatcher_var).var_,
        .fn_name = constraint.fn_name,
    };
    const dedup_entry = try self.reported_constraint_errors.getOrPut(dedup_key);
    if (dedup_entry.found_existing) {
        try self.markConstraintFunctionAsError(constraint, env);
        return;
    }

    const snapshot = try self.snapshots.snapshotVarForError(self.types, &self.type_writer, dispatcher_var);
    const constraint_problem = switch (kind) {
        .missing_method => |dispatcher_type| problem.Problem{
            .static_dispatch = .{
                .dispatcher_does_not_impl_method = .{
                    .dispatcher_var = dispatcher_var,
                    .dispatcher_snapshot = snapshot,
                    .dispatcher_type = dispatcher_type,
                    .fn_var = constraint.fn_var,
                    .method_name = constraint.fn_name,
                    .origin = constraint.origin,
                    .num_literal = constraint.origin.numeralInfo(),
                    .quote_region = self.quoteLiteralRegionForDispatcher(constraint, dispatcher_var),
                    // Only a numeral literal defaulted to Dec earns the numeric hint.
                    // The cascade pass (`is_numeric_default_pass`) also reports errors
                    // for quote literals defaulted to Str (e.g. `"a" > "b"`); those must
                    // NOT claim "this numeric literal was given the type Dec".
                    .defaulted_from_numeric_literal = is_numeric_default_pass and self.isDecNominal(dispatcher_var),
                },
            },
        },
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
) Allocator.Error!void {
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
            // If we can't reset the env, deinit it and don't return it to the pool
            releasable_env.deinit(self.gpa);
            return;
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
