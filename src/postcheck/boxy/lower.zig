//! `.boxy` lowering: checked modules directly to LIR, one worker per
//! reachable procedure template, lowered once at its checked definition type
//! (design.md "`.boxy` Runtime Lowering").
//!
//! The lowerer runs one worklist over reachable workers, seeded from the
//! requested roots and grown by the call targets, dictionary targets, and
//! function values the bodies it lowers name. Every memo is keyed by checked
//! identity. Body lowering keeps its pending work on explicit stacks (see
//! `body.zig`).

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const check = @import("check");
const can = @import("can");
const layout = @import("layout");
const host_boundary = layout.host_boundary_digest;
const lir_core = @import("lir_core");

const Common = @import("../common.zig");
const types = @import("types.zig");
const Layouts = @import("layouts.zig").Layouts;
const StaticDescs = @import("static_descs.zig").StaticDescs;
const Body = @import("body.zig");
const Codec = @import("codec.zig");
const const_schema = @import("const_schema.zig");
const StoredFn = @import("stored_fn.zig");
const ConstSchema = const_schema.ConstSchema;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const checked_names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;

const TypeId = types.TypeId;
const ModuleIndex = types.ModuleIndex;
const invariant = types.invariant;

/// Everything one `.boxy` lowering reads.
pub const Input = struct {
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    target_usize: base.target.TargetUsize,
    inline_expects: enum { run, omit } = .run,
    /// Name each procedure in the LIR store by the kind of worker it is,
    /// for dumps and tests that read the program.
    proc_debug_names: bool = false,
    /// Allow `List.map` to reuse a unique input list's allocation (see
    /// `SolvedLirLower.Options.list_in_place_map`).
    list_in_place_map: bool = false,
    /// Record each platform entrypoint's host boundary digest.
    host_boundary_digests: bool = false,
    /// The lowering is a compile-time evaluator's: its roots are
    /// compile-time roots, each lowered with the plan and stored type the
    /// store writer keeps its value by (`LirProgram.ConstRootPlan`), and a
    /// read of a compile-time value no evaluation has completed crashes as
    /// a read of a failed value, which the evaluator retries once the value
    /// completes (design.md "Dependency-Ordered Compile-Time Evaluation").
    /// Its program runs before any literal table exists, so every evidence
    /// parameter is a dictionary (`Program.runtimeDictionary`) and each
    /// instance-dependent literal is converted where it is.
    compile_time: bool = false,
    /// The program evaluates roots holding function values: it notes where
    /// each descriptor came from (`LIR.DynOp.desc_note`), and the lowering
    /// keeps the program for reading those values (`Output.retained`).
    stored_functions: bool = false,
    /// What this lowering emits: one program, or one module's object.
    unit: Unit = .program,
};

/// What one lowering emits (design.md "Per-Module Output").
pub const Unit = union(enum) {
    /// Every worker the roots reach, as one program: the interpreter paths,
    /// tests, and compile-time evaluation.
    program,
    /// One module's object: every worker of the module's templates, lowered
    /// as roots, with every other module's worker an imported symbol
    /// (`LIR.LirProcSpec.linked`) its own object defines.
    module: ModuleUnit,
};

/// The module one unit emits: the lowering's root module (`Input.modules`).
pub const ModuleUnit = struct {
    /// The export interface of every imported module's object (what its
    /// unit's ARC solved for each exported worker), by worker identity.
    imports: *ExportInterfaces,
    /// Whether this is the program's root unit: it lowers `Input.roots` as
    /// well, and the workers of every module with platform requirements,
    /// which are lowered at the app's types and belong to no module's own
    /// object (design.md "Host ABI").
    root: bool,
    /// The name each module of the link gives its workers and nominal types.
    names: *const types.ModuleNames,
};

/// The ownership signature and tail-drive summary a unit's ARC solved for
/// one exported worker: what a call to it from another object compiles
/// against (`LIR.LirProcSpec` `rc_*` fields and `may_leave_pending`).
pub const ExportInterface = struct {
    rc_borrowed_params: u64 = 0,
    rc_ret_borrowed: bool = false,
    rc_ret_lenders: u64 = 0,
    rc_read_only_params: u64 = 0,
    rc_ret_unique: bool = false,
    rc_ret_unique_fields: u64 = 0,
    rc_ret_conditions: []const u32 = &.{},
    may_leave_pending: bool = false,
};

/// The export interfaces of every object a unit imports, by worker identity.
/// Units lowered in parallel read and record interfaces concurrently, so
/// every access takes the mutex; an interface, once recorded, never changes.
pub const ExportInterfaces = struct {
    allocator: Allocator,
    io: std.Io,
    map: std.AutoHashMapUnmanaged(LIR.ProcIdentity, ExportInterface) = .empty,
    arena: std.heap.ArenaAllocator,
    mutex: Mutex = .init,

    /// A mutex on hosts with threads; a freestanding compiler runs every
    /// unit on its one thread.
    const Mutex = if (@import("builtin").os.tag == .freestanding) struct {
        pub const init: @This() = .{};
        pub fn lockUncancelable(_: *@This(), _: std.Io) void {}
        pub fn unlock(_: *@This(), _: std.Io) void {}
    } else std.Io.Mutex;

    pub fn init(allocator: Allocator, io: std.Io) ExportInterfaces {
        return .{ .allocator = allocator, .io = io, .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *ExportInterfaces) void {
        self.map.deinit(self.allocator);
        self.arena.deinit();
    }

    /// Forget every interface. No unit may be running.
    pub fn clear(self: *ExportInterfaces) void {
        self.map.clearRetainingCapacity();
        _ = self.arena.reset(.retain_capacity);
    }

    /// Record `interface` for `identity`, copying its condition rows.
    pub fn put(self: *ExportInterfaces, identity: LIR.ProcIdentity, interface: ExportInterface) Allocator.Error!void {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        var owned = interface;
        owned.rc_ret_conditions = try self.arena.allocator().dupe(u32, interface.rc_ret_conditions);
        try self.map.put(self.allocator, identity, owned);
    }

    /// Record the interface ARC solved for `proc` of `store`, under the
    /// procedure's identity, and return it.
    pub fn putProc(self: *ExportInterfaces, store: *const lir_core.LirStore, proc: LIR.LirProcSpecId) Allocator.Error!ExportInterface {
        const spec = store.getProcSpec(proc);
        const words = store.getU32Span(spec.rc_ret_conditions);
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        const conditions = try self.arena.allocator().alloc(u32, collections.GuardedList.borrowLen(words));
        for (conditions, 0..) |*word, index| word.* = collections.GuardedList.at(words, index);
        const interface: ExportInterface = .{
            .rc_borrowed_params = spec.rc_borrowed_params,
            .rc_ret_borrowed = spec.rc_ret_borrowed,
            .rc_ret_lenders = spec.rc_ret_lenders,
            .rc_read_only_params = spec.rc_read_only_params,
            .rc_ret_unique = spec.rc_ret_unique,
            .rc_ret_unique_fields = spec.rc_ret_unique_fields,
            .rc_ret_conditions = conditions,
            .may_leave_pending = spec.may_leave_pending,
        };
        try self.map.put(self.allocator, spec.identity, interface);
        return interface;
    }

    pub fn get(self: *ExportInterfaces, identity: LIR.ProcIdentity) ?ExportInterface {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        return self.map.get(identity);
    }
};

/// The lowered program and the static data it carries.
pub const Output = struct {
    lir_result: LirProgram.Result,
    static_data: []LirProgram.StaticDataExport,
    /// With `Input.stored_functions`, the program's types and workers.
    retained: ?*StoredFn.Retained = null,
};

/// A worker's identity: what it lowers.
pub const WorkerKey = union(enum) {
    /// A procedure template.
    template: struct { module: ModuleIndex, template: checked_names.CheckedProcedureTemplateId },
    /// A function value of type `to` calling one of type `from`, which differ
    /// in the storage of record fields (design.md "Field Kinds"); both are
    /// in the variable space of the worker `outer`, whose descriptors the
    /// value captures after the called function.
    adapter: struct { outer: WorkerId, from: TypeId, to: TypeId },
    /// The `??` default expression `expr` of `module`, evaluated where a
    /// construction omits its field (design.md "Defaulted Fields").
    default: struct { module: ModuleIndex, expr: checked.CheckedExprId },
    /// A lambda or closure expression of the worker `outer`, whose scope
    /// (its type variables and their descriptors) it shares through its
    /// captures.
    /// `captured_scheme` is set for a local procedure's function value whose
    /// own scheme's instantiation it captures (a value compile-time
    /// evaluation stored) rather than taking per call.
    lambda: struct { module: ModuleIndex, expr: checked.CheckedExprId, outer: WorkerId, captured_scheme: bool = false },
    /// The erased-callable entry of a template worker that a dictionary
    /// names: it takes the method's arguments as its argument record, the
    /// target's hidden descriptors and dictionaries as its captures, and
    /// calls the target.
    method_entry: WorkerId,
    /// The thunk of literal requirement `literal requirement` of the own scope of worker
    /// `target` instantiated at the types `bound` (a tuple type of them):
    /// an erased worker with no arguments producing the literal's converted
    /// value (design.md "Literal Arguments"), for an instantiation checking
    /// recorded no edge for.
    literal_thunk: struct { target: WorkerId, bound: TypeId, literal_requirement: u32 },
    /// The thunk of literal argument root `root` of `module` in the
    /// compile-time program evaluating it: an erased worker with no
    /// arguments reading the root's compile-time value slot
    /// (`Program.rootValueSlot`), which readers call once the root completes.
    root_slot: struct { module: ModuleIndex, root: checked.ComptimeRootId },
    /// A compiler-derived structural method over the descriptor its
    /// dictionary captures.
    structural: StructuralMethod,
    /// A derived `map`/`map!` over the tag union its dictionary describes:
    /// the variant `name` of `arity` payloads has payload `payload_index`
    /// transformed, every other value is re-tagged unchanged.
    structural_map: StructuralMap,
    /// The host-visible wrapper of root request `index`: it takes the
    /// root's checked arguments, supplies the static descriptors and
    /// dictionaries the root's instantiation names, and calls its target
    /// (design.md "Host ABI").
    root: u32,
    /// A worker of a generated codec family (`Codec.Piece`, by index).
    codec: u32,
    /// The initializer of static data request `index`: it builds the
    /// requested constant at its type, for static data materialization.
    static_data: u32,
    /// The method of a dictionary whose checked evidence resolves its slot
    /// to a crash: a dispatch checking rejected, or one no value can reach
    /// (design.md "Static Dispatch In Monotype"). Calling it crashes.
    crash_dictionary: CrashDictionary,
    /// The static row conversion of `module` from the closed type `from` to
    /// the closed type `to`, whose rows widen `from`'s along the coercion's
    /// reach (design.md "Sealed Result Rows"). Each module converting a pair
    /// has its own worker for it.
    convert: struct { module: ModuleIndex, from: TypeId, to: TypeId },
};

/// Why a dictionary slot's method crashes.
pub const CrashDictionary = enum { checked_error, unreachable_value };

/// Where a stored constant lives: a checked module's constant store.
pub const ConstSource = union(enum) { module: ModuleIndex };

/// Whether `primitive` is a 128-bit SIMD vector.
fn descFormatIsVector(primitive: types.Primitive) bool {
    return switch (primitive) {
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => true,
        .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => false,
    };
}

/// An exported procedure binding's identity within its module.
pub const ExportedBindingKey = struct { module: ModuleIndex, def: @FieldType(checked.ImportedProcedureBindingRef, "def"), pattern: checked.CheckedPatternId };

/// The structural methods a dictionary can denote.
pub const StructuralMethod = enum { is_eq, to_hash, to_inspect };

/// A derived map's checked payload selection. Its worker's descriptors are
/// the input union's, the output union's, the selected variant's `arity`
/// input payloads', and the transformed payload's output type's.
pub const StructuralMap = struct { name: types.NameId, payload_index: u32, arity: u32, effectful: bool };

/// Names a worker by its index among the program's workers.
pub const WorkerId = enum(u32) { _ };

/// One worker: its procedure and the signature callers use.
pub const Worker = struct {
    key: WorkerKey,
    proc: LIR.LirProcSpecId,
    module: ModuleIndex,
    /// The checked scheme variables of the worker's scope, in hidden
    /// descriptor parameter order. A template's sealed variables (design.md
    /// "Sealed Result Rows") are omitted: they seal to the empty row.
    scheme_vars: []const checked.CheckedTypeId,
    /// For a template worker: the position of each of `scheme_vars` in the
    /// template's checked `scheme_vars`, which a use's substitution is
    /// indexed by, and that checked list's length.
    checked_slots: []const u32 = &.{},
    checked_scheme_count: u32 = 0,
    /// Evidence parameters that are runtime dictionaries, in hidden
    /// dictionary parameter order (indices into the template's params).
    dictionary_params: []const u32,
    params: []const TypeId,
    ret: TypeId,
    lowered: bool = false,
    /// Whether the worker's body is queued for lowering.
    queued: bool = false,
    /// Another module's object defines the worker: its procedure is a
    /// body-less `linked` symbol here (design.md "Per-Module Output").
    imported: bool = false,
    /// For a root wrapper: the root request it wraps.
    root_request: ?checked.RootRequest = null,
    /// For a static data initializer: the request it serves.
    static_data_request: ?Common.StaticDataRequest = null,
    /// For a lambda worker: the checked captures of its closure expression,
    /// whose values follow it in the capture record before the outer
    /// worker's descriptors.
    captures: []const checked.CheckedCapture = &.{},
    /// For a lambda worker: the stored type of each captured value.
    capture_types: []const TypeId = &.{},
    /// The dictionaries in scope, by generalized scope, innermost first:
    /// each scope's runtime evidence-parameter indices. A template has its
    /// own scope; a lambda shares its outer worker's scopes; a generalized
    /// local procedure has its own scope inside its outer worker's.
    dict_scopes: []const []const u32 = &.{},
    /// Each of `dict_scopes`' scopes' evidence-parameter records, by
    /// parameter index.
    dict_scope_params: []const []const static_dispatch.EvidenceParamRecord = &.{},
    /// For a generalized local procedure: how many descriptors and
    /// dictionaries of its own scope its argument record carries before its
    /// explicit arguments (they vary per call, unlike its captures).
    own_descs: u32 = 0,
    /// For a structural worker: how many descriptors its dictionary captures,
    /// each standing for a structural type parameter (`types.Store.variable`
    /// by position) rather than a checked scheme variable.
    structural_descs: u32 = 0,
    own_dicts: u32 = 0,
    /// The literal arguments the worker takes after its dictionaries, one
    /// per literal requirement of its own scheme (design.md "Literal Arguments"):
    /// an immortal callable producing the literal's converted value.
    literal_params: u32 = 0,
    /// The literal requirements in scope, by generalized scope, innermost
    /// first, parallel to `dict_scopes`.
    literal_scopes: []const []const checked.LiteralRequirement = &.{},
    /// For a generalized local procedure: how many literal arguments of its
    /// own scope its argument record carries after its own dictionaries.
    own_literals: u32 = 0,

    /// The bytes of the worker's procedure identity.
    pub fn identityBytes(self: Worker, program: *const Program) [32]u8 {
        return program.result.store.getProcSpec(self.proc).identity.bytes;
    }

    /// The dictionaries a lambda worker captures: every scope's but its own.
    pub fn capturedDictionaryCount(self: Worker) u32 {
        var count: u32 = 0;
        for (self.dict_scopes) |scope| count += @intCast(scope.len);
        return count - self.own_dicts;
    }

    /// The literal arguments a lambda worker captures: every scope's but
    /// its own.
    pub fn capturedLiteralCount(self: Worker) u32 {
        var count: u32 = 0;
        for (self.literal_scopes) |scope| count += @intCast(scope.len);
        return count - self.own_literals;
    }
};

/// A generalized local procedure's own scheme: its quantified variables and
/// its runtime evidence-parameter indices.
pub const LocalScheme = struct {
    scheme_vars: []const checked.CheckedTypeId,
    dictionaries: []const u32,
    /// The scope's evidence-parameter records, by parameter index.
    params: []const static_dispatch.EvidenceParamRecord = &.{},
    /// The scope's literal requirements (design.md "Literal Arguments").
    literal_requirements: []const checked.LiteralRequirement = &.{},
    /// Whether each call passes the scheme's descriptors and dictionaries (a
    /// local procedure's), or the function value captures them at its
    /// construction (any other generalized lambda's).
    per_call: bool,
};

/// A nominal method override: its dictionary entry, the types the method's
/// scheme variables take, and the requirements its dictionaries resolve.
/// A nominal's method override, as its descriptor's `Methods` record holds
/// it.
pub const NominalOverride = union(enum) {
    /// A procedure's dictionary entry, its scheme taking `bound` and its
    /// requirements `dicts`; `target` is the procedure's own worker, whose
    /// literal requirements the entry's literal arguments are derived for.
    procedure: struct { entry: WorkerId, target: WorkerId, bound: []const TypeId, dicts: []const DictRequirement },
    /// A method declared in a function body that checking left a local
    /// procedure of the body (it names the body's type variables): its
    /// function value, at the method's interface `interface`, which only a
    /// body the declaring body encloses can build.
    local: struct { module: ModuleIndex, binding: checked.LocalProcedureBinding, interface: TypeId },
};

/// A dispatch requirement resolved from its dispatcher's type: the method
/// named `method` (of module `module`'s names) on `dispatcher`, at the
/// interface `callable`, with the structural derivation checking permits
/// where the dispatcher registers no method.
pub const DictRequirement = struct {
    module: ModuleIndex,
    method: @FieldType(static_dispatch.EvidenceParamRecord, "method"),
    structural: ?static_dispatch.StructuralKind,
    dispatcher: TypeId,
    callable: TypeId,
};

/// What resolves a `DictRequirement`.
pub const RequirementResolution = union(enum) {
    /// The method a template implements; its own requirements remain.
    target: WorkerId,
    /// A structural method over a type's descriptor, which honors the
    /// type's own overrides.
    structural: struct { method: StructuralMethod, over: TypeId },
    /// A dictionary that crashes as the dispatch would.
    crash: CrashDictionary,
    /// The dictionary a body's scope holds for scope variable `index`.
    scope_variable: u32,
    /// A derived codec or map, which checking selects with its plan at the
    /// dispatch it serves: a requirement resolved from a type alone only
    /// names it.
    derived: static_dispatch.StructuralKind,
};

/// A method target instantiated at a requirement's callable: the types its
/// scheme takes and the requirements of its dictionaries.
pub const TargetInstance = struct { bound: []const TypeId, requirements: []const DictRequirement };

/// The overrides of a nominal type, in `Methods` order.
pub const NominalOverrides = [3]?NominalOverride;

/// A field a record construction omits.
pub const OmittedField = struct { module: ModuleIndex, expr: checked.CheckedExprId, name: types.NameId };

/// A checked expression by its module in this lowering.
pub const ExprKey = struct { module: ModuleIndex, expr: checked.CheckedExprId };

/// A procedure template by its module in this lowering.
pub const TemplateKey = struct { module: ModuleIndex, template: checked_names.CheckedProcedureTemplateId };

/// A hosted procedure's external symbol and its slot in the host's dispatch
/// table, or null when the platform's hosted section omits it.
pub const HostedEntry = struct { symbol: []const u8, dispatch_index: ?u32 };

/// One `.boxy` lowering: its workers, types, layouts, and output.
pub const Program = struct {
    allocator: Allocator,
    input: Input,
    modules: types.Modules,
    store: types.Store,
    layouts: Layouts,
    result: LirProgram.Result,
    static_descs: StaticDescs,
    workers: std.ArrayList(Worker) = .empty,
    worker_ids: std.AutoHashMapUnmanaged(WorkerKey, WorkerId) = .empty,
    worklist: std.ArrayList(WorkerId) = .empty,
    /// The Builtin module, found on first use (`builtinModule`).
    builtin_module: ?ModuleIndex = null,
    /// The program-local source file of each module, by module index.
    source_files: []u32 = &.{},
    lowering_module_ids: []LIR.LoweringModuleId = &.{},
    /// Modules by the checker's `module_idx`.
    by_module_idx: std.AutoHashMapUnmanaged(u32, ModuleIndex) = .empty,
    /// The constant store plans and stored types of the compile-time roots
    /// this lowering evaluates (`ConstSchema`).
    const_schema: ConstSchema = .{},
    /// The app type each platform requirement identity variable takes
    /// (`types.Converter.platform_identities`).
    platform_identities: std.AutoHashMapUnmanaged(types.Converter.IdentityKey, TypeId) = .empty,
    /// The compile-time value slot a compile-time program reads a root it
    /// evaluates later from (`rootValueSlot`), by root.
    root_value_slots: std.AutoHashMapUnmanaged(RootSlotKey, LIR.StaticDataId) = .empty,
    /// The compile-time roots this program evaluates (`Input.roots`), by
    /// module and root (`evaluatesRoot`).
    requested_roots: std.AutoHashMapUnmanaged(RootSlotKey, void) = .empty,
    /// The Builtin `Hasher` type, once asked for (`defaultHasher`).
    default_hasher: ?TypeId = null,
    /// Each module's callable-eval templates, by their binding pattern.
    callable_eval_by_pattern: std.AutoHashMapUnmanaged(struct { module: ModuleIndex, pattern: checked.CheckedPatternId }, checked.CallableEvalTemplateId) = .empty,
    /// Each module's entry wrappers, by the compile-time root they wrap.
    wrapper_by_root: std.AutoHashMapUnmanaged(struct { module: ModuleIndex, root: checked.ComptimeRootId }, checked_names.ProcedureTemplateRef) = .empty,
    /// Each module's exported procedure bindings, by their definition.
    exported_bindings: std.AutoHashMapUnmanaged(ExportedBindingKey, *const checked.ImportedProcedureBindingView) = .empty,
    observe_expects: bool,
    list_in_place_map: bool,
    arena: std.heap.ArenaAllocator,
    /// The host binding of each hosted procedure template.
    hosted: std.AutoHashMapUnmanaged(TemplateKey, HostedEntry) = .empty,
    /// The generalized-local scope a lambda or closure expression owns.
    local_scopes: std.AutoHashMapUnmanaged(ExprKey, checked.DispatchScopeId) = .empty,
    /// The lambda and closure expressions local procedures bind.
    local_procs: std.AutoHashMapUnmanaged(ExprKey, void) = .empty,
    /// Whether each worker asked about stores a record field of its
    /// signature in a presence slot (`signatureHasSlots`).
    signature_slots: collections.DenseMapUnmanaged(WorkerId, bool) = .empty,
    /// `nominalOverrides` of each nominal type asked about.
    nominal_overrides: collections.DenseMapUnmanaged(TypeId, NominalOverrides) = .empty,
    /// `composedInBody` of each type asked about.
    composed_in_body: collections.DenseMapUnmanaged(TypeId, bool) = .empty,
    /// The nested procedure site of each lambda and closure expression.
    sites_by_expr: std.AutoHashMapUnmanaged(ExprKey, u32) = .empty,
    /// Modules by their stable identity hash.
    by_identity: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,
    /// The default a record construction that omits a field materializes.
    omitted_defaults: std.AutoHashMapUnmanaged(OmittedField, checked.CheckedFieldDefault) = .empty,
    /// The generated codec families and their workers.
    codec: Codec.State = .{},

    pub fn converter(self: *Program) types.Converter {
        return .{ .store = &self.store, .modules = &self.modules, .platform_identities = &self.platform_identities };
    }

    /// Whether this lowering emits the workers of module `index`: every
    /// module of a program, and for a module unit, the unit's module and,
    /// for the root unit, each module with platform requirements, whose
    /// workers are lowered at the app's types (design.md "Per-Module
    /// Output").
    pub fn ownsModule(self: *const Program, index: ModuleIndex) bool {
        const unit = switch (self.input.unit) {
            .program => return true,
            .module => |unit| unit,
        };
        if (index == self.modules.views.items[0].index) return true;
        return unit.root and self.module(index).view.platform_type_inputs.requirements.len != 0;
    }

    /// The content identity of a module's worker (design.md "Per-Module
    /// Output"): a digest of the declaring module's link name
    /// (`types.Modules.linkName`) and the worker's checked key, the same in
    /// every lowering, so one object's reference to another's worker names it
    /// by symbol.
    pub fn workerIdentity(self: *Program, key: WorkerKey) LIR.ProcIdentity {
        var hasher = base.TypeDigestHasher.init();
        hasher.update("roc.boxy.worker.v1");
        switch (key) {
            .template => |template| {
                hasher.update("template");
                hasher.update(&self.modules.linkName(template.module));
                hasher.update(std.mem.asBytes(&@as(u32, @backingInt(template.template))));
            },
            .lambda => |lambda| {
                hasher.update("lambda");
                hasher.update(&self.modules.linkName(lambda.module));
                hasher.update(std.mem.asBytes(&@as(u32, @backingInt(lambda.expr))));
                hasher.update(&self.worker(lambda.outer).identityBytes(self));
                hasher.update(&[_]u8{@intFromBool(lambda.captured_scheme)});
            },
            .method_entry => |target| {
                hasher.update("method_entry");
                hasher.update(&self.worker(target).identityBytes(self));
            },
            .default => |default| {
                hasher.update("default");
                hasher.update(&self.modules.linkName(default.module));
                hasher.update(std.mem.asBytes(&self.defaultOrdinal(default.module, default.expr)));
            },
            .adapter, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => invariant("a worker no module's object exports has no content identity"),
        }
        return .{ .bytes = hasher.finalResult() };
    }

    /// The position of default expression `expr` among its module's, which
    /// an edit elsewhere in the module leaves alone.
    fn defaultOrdinal(self: *const Program, module_index: ModuleIndex, expr: checked.CheckedExprId) u32 {
        for (self.module(module_index).bodies().default_exprs, 0..) |default, ordinal| {
            if (default.checked_expr == expr) return @intCast(ordinal);
        }
        invariant("a default worker's expression is no default expression of its module");
    }

    /// Whether workers of `key`'s kind are the workers a module's object
    /// defines for other objects, named by content identity.
    pub fn exportableKind(key: WorkerKey) bool {
        return switch (key) {
            .template, .lambda, .method_entry, .default => true,
            .adapter, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => false,
        };
    }

    /// The module unit this lowering emits, or null for a program.
    pub fn moduleUnit(self: *const Program) ?ModuleUnit {
        return switch (self.input.unit) {
            .program => null,
            .module => |unit| unit,
        };
    }

    /// Give the proc of imported worker `id` the ownership signature its
    /// defining object solved (design.md "Per-Module Output"), once a direct
    /// call to it compiles against that signature.
    fn bindImportedInterface(self: *Program, id: WorkerId) void {
        const imported = self.worker(id);
        const unit = self.moduleUnit() orelse invariant("an imported worker outside a module unit");
        const proc = self.result.store.getProcSpecPtr(imported.proc);
        const interface = unit.imports.get(proc.identity) orelse
            invariant("a call reached a worker of an imported module whose object exports no such worker");
        proc.rc_borrowed_params = interface.rc_borrowed_params;
        proc.rc_ret_borrowed = interface.rc_ret_borrowed;
        proc.rc_ret_lenders = interface.rc_ret_lenders;
        proc.rc_read_only_params = interface.rc_read_only_params;
        proc.rc_ret_unique = interface.rc_ret_unique;
        proc.rc_ret_unique_fields = interface.rc_ret_unique_fields;
        proc.may_leave_pending = interface.may_leave_pending;
        proc.rc_ret_conditions = self.result.store.addU32Span(interface.rc_ret_conditions) catch |err| switch (err) {
            error.OutOfMemory => invariant("out of memory binding an imported worker's interface"),
        };
    }

    /// The procedure of an imported worker: body-less, defined by its
    /// module's own object under its content identity, and reached with the
    /// backend's Roc convention (`LIR.LirProcSpec.linked`).
    fn addImportedProc(self: *Program, identity: LIR.ProcIdentity, module_index: ModuleIndex, abi: LIR.ProcAbi) Allocator.Error!LIR.LirProcSpecId {
        return try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = LIR.EmissionUnit.module(@backingInt(module_index)),
            .args = .empty(),
            .ret_layout = .zst,
            .abi = abi,
            .external = true,
            .linked = true,
        }, base.SourceLoc.none);
    }

    /// Declare the signature of imported template worker `id`, exactly as
    /// its defining unit's body lowering declares it: explicit parameters,
    /// then hidden descriptors and dictionaries, and a dynamic result slot
    /// when the result is dynamic. A dynamic parameter's descriptor local
    /// is never read: the worker has no body here.
    fn declareImportedTemplateSignature(self: *Program, id: WorkerId) Allocator.Error!void {
        const imported = self.worker(id).*;
        const store = &self.result.store;
        var args: std.ArrayList(LIR.LocalId) = .empty;
        defer args.deinit(self.allocator);
        const dynamic_layout = try self.result.layouts.dynamicLayout();
        for (imported.params) |param_ty| {
            if (try self.converter().representationClosed(param_ty)) {
                try args.append(self.allocator, try store.addLocal(.{ .layout_idx = try self.layouts.layoutOf(param_ty) }));
            } else {
                const desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
                try args.append(self.allocator, try store.addLocal(.{ .layout_idx = dynamic_layout, .dyn = .{ .desc = desc, .storage = .param } }));
            }
        }
        for (imported.scheme_vars) |_| try args.append(self.allocator, try store.addLocal(.{ .layout_idx = .opaque_ptr }));
        const erased = try self.result.layouts.insertErasedCallable();
        for (imported.dictionary_params) |_| try args.append(self.allocator, try store.addLocal(.{ .layout_idx = erased }));
        for (0..imported.literal_params) |_| try args.append(self.allocator, try store.addLocal(.{ .layout_idx = erased }));
        const proc = store.getProcSpecPtr(imported.proc);
        proc.args = try store.addLocalSpan(args.items);
        if (try self.converter().representationClosed(imported.ret)) {
            proc.ret_layout = try self.layouts.layoutOf(imported.ret);
        } else {
            proc.ret_layout = dynamic_layout;
            const desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
            proc.dyn_result = try store.addLocal(.{ .layout_idx = dynamic_layout, .dyn = .{ .desc = desc, .storage = .result } });
        }
    }

    pub fn module(self: *const Program, index: ModuleIndex) *const types.ModuleView {
        return self.modules.get(index);
    }

    /// Free everything but the program's `result`, which lowering hands on.
    pub fn release(self: *Program) void {
        self.static_descs.deinit();
        self.layouts.deinit();
        self.store.deinit();
        self.modules.deinit();
        self.workers.deinit(self.allocator);
        self.worker_ids.deinit(self.allocator);
        self.worklist.deinit(self.allocator);
        self.by_module_idx.deinit(self.allocator);
        self.exported_bindings.deinit(self.allocator);
        self.callable_eval_by_pattern.deinit(self.allocator);
        self.platform_identities.deinit(self.allocator);
        self.root_value_slots.deinit(self.allocator);
        self.requested_roots.deinit(self.allocator);
        self.const_schema.deinit(self.allocator);
        self.wrapper_by_root.deinit(self.allocator);
        self.hosted.deinit(self.allocator);
        self.local_scopes.deinit(self.allocator);
        self.local_procs.deinit(self.allocator);
        self.by_identity.deinit(self.allocator);
        self.sites_by_expr.deinit(self.allocator);
        self.nominal_overrides.deinit(self.allocator);
        self.composed_in_body.deinit(self.allocator);
        self.signature_slots.deinit(self.allocator);
        self.omitted_defaults.deinit(self.allocator);
        self.codec.deinit(self.allocator);
        self.arena.deinit();
    }

    /// The constant store of `source`.
    pub fn constStore(self: *const Program, source: ConstSource) *const check.ConstStore.ConstStore {
        return switch (source) {
            .module => |index| self.module(index).view.const_store,
        };
    }

    /// The names `source`'s stored types intern.
    pub fn constNames(self: *const Program, source: ConstSource) *const checked_names.CanonicalNameStore {
        return switch (source) {
            .module => |index| self.module(index).nameStore(),
        };
    }

    pub fn moduleByKey(self: *const Program, key: checked.ModuleId) *const types.ModuleView {
        return self.modules.byKey(key);
    }

    pub fn moduleByIdx(self: *const Program, idx: u32) *const types.ModuleView {
        const index = self.by_module_idx.get(idx) orelse invariant("a checked target named a module index outside this lowering");
        return self.modules.get(index);
    }

    pub fn sourceLoc(self: *const Program, index: ModuleIndex, region: base.Region) base.SourceLoc {
        if (region.isEmpty()) return base.SourceLoc.none;
        const view = self.modules.get(index);
        const line_starts = view.view.module_env.common.getLineStartsAll();
        if (line_starts.len == 0) return base.SourceLoc.none;
        const offset = region.start.offset;
        const line = base.RegionInfo.lineIdx(line_starts, offset);
        const column = base.RegionInfo.columnIdx(line_starts, line, offset) catch
            invariant("checked node region resolved to an invalid line/column position");
        return .{ .file = self.source_files[@backingInt(index)], .line = line + 1, .column = column + 1 };
    }

    /// The worker for a procedure template, requested if new.
    /// The worker of a procedure template, queued for lowering.
    pub fn templateWorker(self: *Program, module_index: ModuleIndex, template_id: checked_names.CheckedProcedureTemplateId) Allocator.Error!WorkerId {
        const id = try self.templateScope(module_index, template_id);
        if (self.worker(id).imported) {
            // A call compiles against the signature the worker's own object
            // solved.
            if (!self.worker(id).queued) {
                self.worker(id).queued = true;
                self.bindImportedInterface(id);
            }
            return id;
        }
        if (!self.worker(id).queued) {
            self.worker(id).queued = true;
            try self.worklist.append(self.allocator, id);
        }
        return id;
    }

    /// The worker of a procedure template as the scope of the lambdas it
    /// contains, lowered only when something calls the template itself. The
    /// owner of a closure that compile-time evaluation stored is such a
    /// scope: its body ran at compile time.
    pub fn templateScope(self: *Program, module_index: ModuleIndex, template_id: checked_names.CheckedProcedureTemplateId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .template = .{ .module = module_index, .template = template_id } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const view = self.module(module_index);
        const template = view.view.checked_procedure_templates.templates.items[@backingInt(template_id)];
        const checked_scheme_vars = poolSlice(checked.CheckedTypeId, view.view.checked_procedure_templates.scheme_vars_pool, template.scheme_vars);
        // A sealed variable (design.md "Sealed Result Rows") is no hidden
        // parameter: it seals to the empty row wherever the body mentions it,
        // and an evidence parameter dispatching on it is no dictionary.
        var scheme_vars: std.ArrayList(checked.CheckedTypeId) = .empty;
        var checked_slots: std.ArrayList(u32) = .empty;
        var sealed_slots: std.ArrayList(u32) = .empty;
        defer sealed_slots.deinit(self.allocator);
        const evidence_params = poolSlice(static_dispatch.EvidenceParamRecord, view.view.checked_procedure_templates.evidence_params_pool, template.evidence_params);
        for (checked_scheme_vars, 0..) |variable, slot| {
            // A platform requirement's identity variable is the app type
            // this program lowers the platform at (`solvePlatformIdentities`):
            // every mention converts to that type, so it is no hidden
            // parameter either.
            if (self.converter().platformIdentity(module_index, variable) != null) continue;
            if (try self.converter().sealedSchemeVar(module_index, variable, template.checked_fn_root, evidence_params)) {
                try sealed_slots.append(self.allocator, @intCast(slot));
                continue;
            }
            try scheme_vars.append(self.arena.allocator(), variable);
            try checked_slots.append(self.arena.allocator(), @intCast(slot));
        }
        const literal_requirements = view.view.checked_procedure_templates.templateLiteralRequirements(&template);
        var dictionary_params: std.ArrayList(u32) = .empty;
        for (evidence_params, 0..) |param, index| {
            const sealed_dispatcher = if (param.slot) |slot| std.mem.findScalar(u32, sealed_slots.items, slot) != null else false;
            if (sealed_dispatcher) continue;
            try dictionary_params.append(self.arena.allocator(), @intCast(index));
        }
        const bound = try self.arena.allocator().alloc(TypeId, scheme_vars.items.len);
        for (bound, 0..) |*slot, index| slot.* = try self.store.variable(@intCast(index));
        const env: types.Env = .{ .module = module_index, .vars = scheme_vars.items, .bound = bound };
        const fn_ty = try self.converter().convert(module_index, template.checked_fn_root, env);
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a procedure template's checked root is not a function type"),
        };
        const params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args));

        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const identity = self.workerIdentity(key);
        const imported = !self.ownsModule(module_index);
        const proc = if (imported) try self.addImportedProc(identity, module_index, .roc) else try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = LIR.EmissionUnit.module(@backingInt(module_index)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = scheme_vars.items,
            .checked_slots = checked_slots.items,
            .checked_scheme_count = @intCast(checked_scheme_vars.len),
            .dictionary_params = dictionary_params.items,
            .params = params,
            .ret = signature.ret,
            .dict_scopes = try self.arena.allocator().dupe([]const u32, &.{dictionary_params.items}),
            .dict_scope_params = try self.arena.allocator().dupe([]const static_dispatch.EvidenceParamRecord, &.{evidence_params}),
            .literal_params = @intCast(literal_requirements.len),
            .literal_scopes = try self.arena.allocator().dupe([]const checked.LiteralRequirement, &.{literal_requirements}),
            .imported = imported,
            .lowered = imported,
        });
        try self.worker_ids.put(self.allocator, key, id);
        if (imported) try self.declareImportedTemplateSignature(id);
        return id;
    }

    /// The entries of a use's checked substitution `subst` (indexed by the
    /// template's checked scheme variables) for the scheme variables of
    /// template worker `id`: the sealed variables' entries are dropped, since
    /// the worker takes no descriptor for them. An empty substitution (a use
    /// checking recorded none for) stays empty.
    pub fn keptSubstitution(self: *Program, id: WorkerId, subst: []const checked.CheckedTypeId) Allocator.Error![]const checked.CheckedTypeId {
        const target = self.worker(id).*;
        if (subst.len == 0 or target.key != .template) return subst;
        if (subst.len != target.checked_scheme_count) invariant("a use's checked substitution differs from its target's scheme");
        if (target.checked_slots.len == subst.len) return subst;
        const kept = try self.arena.allocator().alloc(checked.CheckedTypeId, target.checked_slots.len);
        for (kept, target.checked_slots) |*entry, slot| entry.* = subst[slot];
        return kept;
    }

    pub fn templateRefScope(self: *Program, ref: checked_names.ProcedureTemplateRef) Allocator.Error!WorkerId {
        const view = self.moduleByKey(.{ .bytes = ref.artifact.bytes });
        return try self.templateScope(view.index, ref.template);
    }

    /// The erased-callable worker of a lambda or closure expression of the
    /// worker `outer`.
    pub fn lambdaWorker(
        self: *Program,
        outer: WorkerId,
        lambda_expr: checked.CheckedExprId,
        captures: []const checked.CheckedCapture,
        capture_types: []const TypeId,
        fn_ty: TypeId,
        own: ?LocalScheme,
    ) Allocator.Error!WorkerId {
        const outer_worker = self.worker(outer).*;
        const key: WorkerKey = .{ .lambda = .{ .module = outer_worker.module, .expr = lambda_expr, .outer = outer, .captured_scheme = if (own) |scheme| scheme.per_call == false and self.local_procs.contains(.{ .module = outer_worker.module, .expr = lambda_expr }) else false } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a lambda's type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const identity = self.workerIdentity(key);
        // A lambda of an imported worker is that worker's object's.
        const imported = outer_worker.imported;
        const proc = if (imported) try self.addImportedProc(identity, outer_worker.module, .erased_record) else try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = LIR.EmissionUnit.module(@backingInt(outer_worker.module)),
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = outer_worker.module,
            .scheme_vars = if (own) |scheme| try std.mem.concat(self.arena.allocator(), checked.CheckedTypeId, &.{ outer_worker.scheme_vars, scheme.scheme_vars }) else outer_worker.scheme_vars,
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .captures = try self.arena.allocator().dupe(checked.CheckedCapture, captures),
            .capture_types = try self.arena.allocator().dupe(TypeId, capture_types),
            .dict_scopes = if (own) |scheme| try std.mem.concat(self.arena.allocator(), []const u32, &.{ &.{scheme.dictionaries}, outer_worker.dict_scopes }) else outer_worker.dict_scopes,
            .dict_scope_params = if (own) |scheme| try std.mem.concat(self.arena.allocator(), []const static_dispatch.EvidenceParamRecord, &.{ &.{scheme.params}, outer_worker.dict_scope_params }) else outer_worker.dict_scope_params,
            .own_descs = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.scheme_vars.len) else 0) else 0,
            .own_dicts = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.dictionaries.len) else 0) else 0,
            .literal_scopes = if (own) |scheme| try std.mem.concat(self.arena.allocator(), []const checked.LiteralRequirement, &.{ &.{scheme.literal_requirements}, outer_worker.literal_scopes }) else outer_worker.literal_scopes,
            .own_literals = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.literal_requirements.len) else 0) else 0,
            .imported = imported,
            .lowered = imported,
            .queued = imported,
        });
        try self.worker_ids.put(self.allocator, key, id);
        if (!imported) try self.worklist.append(self.allocator, id);
        return id;
    }

    /// Bool's tags: the tag union `[False, True]` it is.
    pub fn boolUnion(self: *Program) Allocator.Error!TypeId {
        return try self.store.intern(.{ .tag_union = .{ .tags = &.{
            .{ .name = try self.store.internName("False"), .payloads = &.{} },
            .{ .name = try self.store.internName("True"), .payloads = &.{} },
        }, .ext = null } });
    }

    /// The relation of `relateVariablesAll`, leaving variables the types do
    /// not relate null.
    pub fn relateVariablesPartial(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize) Allocator.Error![]?TypeId {
        return try self.relateVariablesRows(allocator, roots, count, null);
    }

    /// `relateVariablesPartial`, also marking in `row_vars` each variable
    /// related as an open tag row's remainder: the actual row's tags the open
    /// row does not name (design.md "Row Joins", `.boxy`).
    pub fn relateVariablesRows(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize, row_vars: ?[]bool) Allocator.Error![]?TypeId {
        const store = &self.store;
        const bound = try allocator.alloc(?TypeId, count);
        defer allocator.free(bound);
        @memset(bound, null);
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(allocator);
        try pending.appendSlice(allocator, roots);
        while (pending.pop()) |pair| {
            const formal = store.get(pair[0]);
            if (formal == .variable) {
                bound[formal.variable] = pair[1];
                continue;
            }
            const other = store.get(pair[1]);
            if (std.meta.activeTag(formal) != std.meta.activeTag(other)) {
                // A nominal shares its backing's representation: relate a
                // structural position through the other side's backing.
                if (other == .nominal and other.nominal.has_backing and formal != .nominal) {
                    try pending.append(allocator, .{ pair[0], try self.converter().backing(pair[1]) });
                } else if (formal == .nominal and formal.nominal.has_backing and other != .nominal) {
                    try pending.append(allocator, .{ try self.converter().backing(pair[0]), pair[1] });
                } else if (formal == .tag_union and other == .primitive and other.primitive == .bool) {
                    // Bool is the tag union `[False, True]`.
                    try pending.append(allocator, .{ pair[0], try self.boolUnion() });
                }
                continue;
            }
            switch (formal) {
                .list => |elem| try pending.append(allocator, .{ elem, other.list }),
                .box => |payload| try pending.append(allocator, .{ payload, other.box }),
                .tuple => |items| for (store.span(items), store.span(other.tuple)) |a, b| try pending.append(allocator, .{ a, b }),
                .function => |function| {
                    for (store.span(function.args), store.span(other.function.args)) |a, b| try pending.append(allocator, .{ a, b });
                    try pending.append(allocator, .{ function.ret, other.function.ret });
                },
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(other.nominal)) |a, b| try pending.append(allocator, .{ a, b }),
                .record => |record| {
                    var residual: std.ArrayList(types.Field) = .empty;
                    defer residual.deinit(allocator);
                    for (store.fieldSpan(other.record.fields)) |candidate| {
                        var named = false;
                        for (store.fieldSpan(record.fields)) |field| {
                            if (candidate.name == field.name) {
                                named = true;
                                try pending.append(allocator, .{ field.ty, candidate.ty });
                            }
                        }
                        if (!named) try residual.append(allocator, candidate);
                    }
                    if (record.ext) |ext| {
                        const remainder = if (residual.items.len == 0 and other.record.ext == null)
                            try store.emptyRecord()
                        else
                            try store.intern(.{ .record = .{ .fields = residual.items, .ext = other.record.ext } });
                        try pending.append(allocator, .{ ext, remainder });
                    }
                },
                .tag_union => |tag_union| {
                    var residual: std.ArrayList(types.TagInput) = .empty;
                    defer residual.deinit(allocator);
                    const actual = other.tag_union;
                    for (store.tagSpan(actual.tags)) |candidate| {
                        var named = false;
                        for (store.tagSpan(tag_union.tags)) |tag| {
                            if (candidate.name == tag.name and candidate.payloads.len == tag.payloads.len) {
                                named = true;
                                for (store.span(tag.payloads), store.span(candidate.payloads)) |a, b| try pending.append(allocator, .{ a, b });
                            }
                        }
                        if (!named) try residual.append(allocator, .{ .name = candidate.name, .payloads = store.span(candidate.payloads) });
                    }
                    // An open formal row's remainder is the actual row's
                    // tags it does not name.
                    if (tag_union.ext) |ext| {
                        if (row_vars) |marks| switch (store.get(ext)) {
                            .variable => |index| if (index < marks.len) {
                                marks[index] = true;
                            },
                            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => {},
                        };
                        try pending.append(allocator, .{ ext, try store.intern(.{ .tag_union = .{ .tags = residual.items, .ext = actual.ext } }) });
                    }
                },
                .primitive, .empty_record, .variable => {},
            }
        }
        return try allocator.dupe(?TypeId, bound);
    }

    /// Whether worker `id`'s parameters or result store a record field in a
    /// presence slot, so a caller may store it differently (design.md
    /// "Field Kinds").
    pub fn signatureHasSlots(self: *Program, id: WorkerId) Allocator.Error!bool {
        if (self.signature_slots.get(id)) |memo| return memo;
        const target = self.worker(id).*;
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        try pending.appendSlice(self.allocator, target.params);
        try pending.append(self.allocator, target.ret);
        const found = walk: while (pending.pop()) |ty| {
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (self.store.get(ty)) {
                .record => |record| {
                    for (self.store.fieldSpan(record.fields)) |field| {
                        if (field.kind == .presence_slot) break :walk true;
                        try pending.append(self.allocator, field.ty);
                    }
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(self.allocator, self.store.span(items)),
                .tag_union => |tag_union| {
                    for (self.store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(self.allocator, self.store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                },
                .list => |elem| try pending.append(self.allocator, elem),
                .box => |payload| try pending.append(self.allocator, payload),
                .nominal => |nominal| try pending.appendSlice(self.allocator, self.store.nominalArgs(nominal)),
                .function => |function| {
                    try pending.appendSlice(self.allocator, self.store.span(function.args));
                    try pending.append(self.allocator, function.ret);
                },
                .primitive, .empty_record, .variable => {},
            }
        } else false;
        try self.signature_slots.put(self.allocator, id, found);
        return found;
    }

    /// The indices of the variables `root`'s representation reads. A function
    /// type's variables are not among them: every function value is an erased
    /// callable.
    pub fn representationVariables(self: *Program, allocator: Allocator, root: TypeId) Allocator.Error![]const u32 {
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(allocator);
        var found: std.ArrayList(u32) = .empty;
        errdefer found.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (self.store.get(ty)) {
                .variable => |index| try found.append(allocator, index),
                .record => |record| {
                    for (self.store.fieldSpan(record.fields)) |field| try pending.append(allocator, field.ty);
                    if (record.ext) |ext| try pending.append(allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, self.store.span(items)),
                .tag_union => |tag_union| {
                    for (self.store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(allocator, self.store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(allocator, ext);
                },
                .list => |elem| try pending.append(allocator, elem),
                .box => |payload| try pending.append(allocator, payload),
                .nominal => |nominal| try pending.appendSlice(allocator, self.store.nominalArgs(nominal)),
                .primitive, .empty_record, .function => {},
            }
        }
        return try found.toOwnedSlice(allocator);
    }

    /// `ty` with each variable `i` replaced by `bound[i]`. A row whose
    /// remainder becomes a row takes that row's tags.
    pub fn substitute(self: *Program, allocator: Allocator, root: TypeId, bound: []const TypeId) Allocator.Error!TypeId {
        return try self.rewriteType(allocator, root, bound, null);
    }

    /// `substitute` over `representationVariables`, for a type only its
    /// representation matters for: each function type becomes `anyFunction`.
    pub fn substituteRepresentation(self: *Program, allocator: Allocator, root: TypeId, bound: []const TypeId) Allocator.Error!TypeId {
        return try self.rewriteType(allocator, root, bound, try self.anyFunction());
    }

    /// A function type, standing for any function value, which is an erased
    /// callable whatever its type.
    pub fn anyFunction(self: *Program) Allocator.Error!TypeId {
        return try self.store.intern(.{ .function = .{ .args = &.{}, .ret = try self.store.emptyRecord(), .effectful = false } });
    }

    fn rewriteType(self: *Program, allocator: Allocator, root: TypeId, bound: []const TypeId, function_as: ?TypeId) Allocator.Error!TypeId {
        var done: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty;
        defer done.deinit(allocator);
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, .{ .ty = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.ty)) continue;
            const node = self.store.get(frame.ty);
            if (!frame.expanded) {
                try stack.append(allocator, .{ .ty = frame.ty, .expanded = true });
                switch (node) {
                    .record => |record| {
                        for (self.store.fieldSpan(record.fields)) |field| try stack.append(allocator, .{ .ty = field.ty, .expanded = false });
                        if (record.ext) |ext| try stack.append(allocator, .{ .ty = ext, .expanded = false });
                    },
                    .tuple => |items| for (self.store.span(items)) |item| try stack.append(allocator, .{ .ty = item, .expanded = false }),
                    .tag_union => |tag_union| {
                        for (self.store.tagSpan(tag_union.tags)) |tag| for (self.store.span(tag.payloads)) |payload| try stack.append(allocator, .{ .ty = payload, .expanded = false });
                        if (tag_union.ext) |ext| try stack.append(allocator, .{ .ty = ext, .expanded = false });
                    },
                    .list => |elem| try stack.append(allocator, .{ .ty = elem, .expanded = false }),
                    .box => |payload| try stack.append(allocator, .{ .ty = payload, .expanded = false }),
                    .nominal => |nominal| for (self.store.nominalArgs(nominal)) |arg| try stack.append(allocator, .{ .ty = arg, .expanded = false }),
                    .function => |function| if (function_as == null) {
                        for (self.store.span(function.args)) |arg| try stack.append(allocator, .{ .ty = arg, .expanded = false });
                        try stack.append(allocator, .{ .ty = function.ret, .expanded = false });
                    },
                    .primitive, .empty_record, .variable => {},
                }
                continue;
            }
            const result: TypeId = switch (node) {
                .primitive, .empty_record => frame.ty,
                .variable => |index| if (index < bound.len) bound[index] else invariant("a substituted type names a variable past its bindings"),
                .list => |elem| try self.store.intern(.{ .list = done.get(elem).? }),
                .box => |payload| try self.store.intern(.{ .box = done.get(payload).? }),
                .tuple => |items| blk: {
                    const out = try allocator.alloc(TypeId, items.len);
                    defer allocator.free(out);
                    for (self.store.span(items), out) |item, *slot| slot.* = done.get(item).?;
                    break :blk try self.store.intern(.{ .tuple = out });
                },
                .function => |function| function_as orelse blk: {
                    const args = try allocator.alloc(TypeId, function.args.len);
                    defer allocator.free(args);
                    for (self.store.span(function.args), args) |arg, *slot| slot.* = done.get(arg).?;
                    break :blk try self.store.intern(.{ .function = .{ .args = args, .ret = done.get(function.ret).?, .effectful = function.effectful } });
                },
                .nominal => |nominal| blk: {
                    const args = try allocator.alloc(TypeId, nominal.args.len);
                    defer allocator.free(args);
                    for (self.store.nominalArgs(nominal), args) |arg, *slot| slot.* = done.get(arg).?;
                    var copy = nominal;
                    _ = &copy;
                    break :blk try self.store.intern(.{ .nominal = .{ .decl = nominal.decl, .args = args, .name = nominal.name, .is_opaque = nominal.is_opaque, .builtin = nominal.builtin, .has_backing = nominal.has_backing } });
                },
                .record => |record| blk: {
                    var fields: std.ArrayList(types.Field) = .empty;
                    defer fields.deinit(allocator);
                    for (self.store.fieldSpan(record.fields)) |field| try fields.append(allocator, .{ .name = field.name, .ty = done.get(field.ty).?, .kind = field.kind });
                    var ext: ?TypeId = if (record.ext) |ext| done.get(ext).? else null;
                    if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                        .empty_record => ext = null,
                        .record => |ext_record| {
                            try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                            ext = ext_record.ext;
                        },
                        .variable => {},
                        .primitive, .tuple, .tag_union, .list, .box, .nominal, .function => invariant("a record row's remainder substituted to a non-record type"),
                    };
                    if (fields.items.len == 0 and ext == null) break :blk try self.store.emptyRecord();
                    const store = &self.store;
                    std.mem.sort(types.Field, fields.items, store, struct {
                        fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try self.store.intern(.{ .record = .{ .fields = fields.items, .ext = ext } });
                },
                .tag_union => |tag_union| blk: {
                    var tags: std.ArrayList(types.TagInput) = .empty;
                    defer tags.deinit(allocator);
                    var payloads: std.ArrayList(TypeId) = .empty;
                    defer payloads.deinit(allocator);
                    const own = self.store.tagSpan(tag_union.tags);
                    for (own) |tag| for (self.store.span(tag.payloads)) |payload| try payloads.append(allocator, done.get(payload).?);
                    var at: usize = 0;
                    for (own) |tag| {
                        try tags.append(allocator, .{ .name = tag.name, .payloads = payloads.items[at..][0..tag.payloads.len] });
                        at += tag.payloads.len;
                    }
                    var ext: ?TypeId = if (tag_union.ext) |ext| done.get(ext).? else null;
                    if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                        .tag_union => |ext_union| {
                            for (self.store.tagSpan(ext_union.tags)) |tag| {
                                const named = for (own) |known| {
                                    if (known.name == tag.name and known.payloads.len == tag.payloads.len) break true;
                                } else false;
                                if (!named) try tags.append(allocator, .{ .name = tag.name, .payloads = self.store.span(tag.payloads) });
                            }
                            ext = ext_union.ext;
                        },
                        .variable => {},
                        .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function => invariant("a tag row's remainder substituted to a non-tag type"),
                    };
                    const store = &self.store;
                    std.mem.sort(types.TagInput, tags.items, store, struct {
                        fn lessThan(s: *types.Store, a: types.TagInput, b: types.TagInput) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try self.store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = ext } });
                },
            };
            try done.put(allocator, frame.ty, result);
        }
        return done.get(root).?;
    }

    /// The override method entries a nominal type's descriptor carries, in
    /// `Methods` order (`is_eq`, `to_hash`, `to_inspect`): each method's
    /// dictionary entry and the types its scheme takes at `nominal_ty`.
    pub fn nominalOverrides(self: *Program, nominal_ty: TypeId) Allocator.Error!NominalOverrides {
        if (self.nominal_overrides.get(nominal_ty)) |memo| return memo;
        const nominal = switch (self.store.get(nominal_ty)) {
            .nominal => |value| value,
            // A SIMD vector inspects through its builtin `to_inspect`.
            .primitive => |primitive| {
                var result: NominalOverrides = .{ null, null, null };
                if (descFormatIsVector(primitive)) {
                    const owner: static_dispatch.MethodOwner = .{ .builtin = checked.builtinOwnerForPrimitive(primitive) };
                    const root_module = self.modules.views.items[0].index;
                    const found = self.registeredMethod(root_module, null, owner, "to_inspect") orelse
                        invariant("a SIMD vector type registers no to_inspect, which Builtin declares for each");
                    const template = switch (found) {
                        .target => |target| switch (target.kind) {
                            .procedure => |procedure| procedure.template,
                            .local_proc, .structural => invariant("a SIMD vector's to_inspect is a Builtin procedure"),
                        },
                        .rejected => invariant("Builtin's SIMD vector to_inspect is rejected"),
                    };
                    const worker_id = try self.templateRefWorker(template);
                    const target_worker = self.worker(worker_id).*;
                    const formal = try self.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                    const instance = try self.instantiateTarget(worker_id, formal, try self.overrideInterface(2, nominal_ty));
                    result[2] = .{ .procedure = .{ .entry = try self.methodEntryWorker(worker_id), .target = worker_id, .bound = instance.bound, .dicts = instance.requirements } };
                }
                try self.nominal_overrides.put(self.allocator, nominal_ty, result);
                return result;
            },
            .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => invariant("asked for the overrides of a non-nominal type"),
        };
        var result: NominalOverrides = .{ null, null, null };
        if (nominal.has_backing) {
            const view = self.module(nominal.decl.module);
            const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
            const owner: static_dispatch.MethodOwner = if (nominal.builtin) |builtin| (if (builtin == .try_)
                .{ .nominal = declaration.nominal }
            else
                .{ .builtin = static_dispatch.builtinOwnerForCheckedBuiltin(builtin) }) else .{ .nominal = declaration.nominal };
            const registry = view.view.method_registry;
            const names = view.nameStore();
            const method_names = [_][]const u8{ "is_eq", "to_hash", "to_inspect" };
            for (method_names, &result, 0..) |method_name, *slot, method_index| {
                const method = names.lookupMethodName(method_name) orelse continue;
                const key: static_dispatch.MethodKey = .{ .owner = owner, .method = method };
                // Inspection calls only an eligible `to_inspect`.
                const target = if (std.mem.eql(u8, method_name, "to_inspect"))
                    (registry.inspectOverride(key) orelse continue).target
                else switch (registry.lookup(key) orelse continue) {
                    // A type whose method checking rejected compares and
                    // hashes as checked-error code.
                    .rejected => {
                        const crash = try self.crashDictionaryWorker(nominal.decl.module, .checked_error);
                        slot.* = .{ .procedure = .{ .entry = crash, .target = crash, .bound = &.{}, .dicts = &.{} } };
                        continue;
                    },
                    .target => |found| found,
                };
                const template = switch (target.kind) {
                    .procedure => |procedure| procedure.template,
                    .structural => continue,
                    .local_proc => |local| {
                        slot.* = .{ .local = .{
                            .module = nominal.decl.module,
                            .binding = .{ .binder = local.binder, .expr = local.expr, .dispatch_scope = local.dispatch_scope },
                            .interface = try self.overrideInterface(method_index, nominal_ty),
                        } };
                        continue;
                    },
                };
                const worker_id = try self.templateRefWorker(template);
                const target_worker = self.worker(worker_id).*;
                const formal = try self.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
                const instance = try self.instantiateTarget(worker_id, formal, try self.overrideInterface(method_index, nominal_ty));
                slot.* = .{ .procedure = .{ .entry = try self.methodEntryWorker(worker_id), .target = worker_id, .bound = instance.bound, .dicts = instance.requirements } };
            }
        }
        try self.nominal_overrides.put(self.allocator, nominal_ty, result);
        return result;
    }

    /// The Builtin `Hasher`: the hasher every runtime `hash` operation
    /// threads (its state is the `u64` the structural `to_hash` worker takes),
    /// and so the hasher a nominal's `to_hash` override is called with. It
    /// is the nominal its declaring module names `Builtin.Hasher`, as
    /// checking's builtin type table names it.
    pub fn defaultHasher(self: *Program) Allocator.Error!TypeId {
        if (self.default_hasher) |known| return known;
        const qualified = can.CIR.builtinTypeSpec("hasher_type").qualified_name;
        for (self.modules.views.items) |*view| {
            const env = view.view.module_env;
            const ident = env.common.findIdent(qualified) orelse continue;
            for (view.types().nominal_declarations) |declaration| {
                if (declaration.source_statement >= env.store.nodes.len()) continue;
                const node: can.CIR.Node.Idx = @fromBackingInt(@intCast(declaration.source_statement));
                if (env.store.nodes.get(node).tag != .statement_nominal_decl) continue;
                const statement_idx: can.CIR.Statement.Idx = @fromBackingInt(@intCast(declaration.source_statement));
                const header = switch (env.store.getStatement(statement_idx)) {
                    .s_nominal_decl => |decl| decl.header,
                    .s_decl, .s_var, .s_var_uninitialized, .s_reassign, .s_crash, .s_dbg, .s_expr, .s_expect, .s_for, .s_while, .s_infinite_loop, .s_breakable_loop, .s_break, .s_return, .s_import, .s_alias_decl, .s_where_alias_decl, .s_type_anno, .s_type_var_alias, .s_runtime_error => continue,
                };
                if (!env.store.getTypeHeader(header).name.eql(ident)) continue;
                const hasher = try self.converter().convert(view.index, declaration.declaration_root, .{ .module = view.index, .vars = &.{}, .bound = &.{} });
                self.default_hasher = hasher;
                return hasher;
            }
        }
        invariant("no module of the program declares Builtin.Hasher, which every program imports");
    }

    /// The type a nominal method override is called at: `is_eq` compares
    /// two values to a Bool, `to_hash` feeds one into the Builtin hasher,
    /// and `to_inspect` renders one as a Str.
    /// Whether a descriptor of `ty` holds a local-procedure override
    /// (`NominalOverride.local`), reached through its structure and its
    /// nominals' backings: only a body the override's declaring body
    /// encloses can compose such a descriptor, so no static descriptor
    /// describes `ty`.
    pub fn composedInBody(self: *Program, root: TypeId) Allocator.Error!bool {
        if (self.composed_in_body.get(root)) |memo| return memo;
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, root);
        const store = &self.store;
        const found = walk: while (pending.pop()) |ty| {
            if (self.composed_in_body.get(ty)) |memo| {
                if (memo) break :walk true else continue;
            }
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (store.get(ty)) {
                .primitive, .empty_record, .variable, .function => {},
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try pending.append(self.allocator, field.ty);
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(self.allocator, store.span(items)),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(self.allocator, store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                },
                .list, .box => |child| try pending.append(self.allocator, child),
                .nominal => |nominal| {
                    for (try self.nominalOverrides(ty)) |maybe| {
                        if (maybe) |override| if (override == .local) break :walk true;
                    }
                    try pending.appendSlice(self.allocator, store.nominalArgs(nominal));
                    if (nominal.has_backing) try pending.append(self.allocator, try self.converter().backing(ty));
                },
            }
        } else false;
        try self.composed_in_body.put(self.allocator, root, found);
        return found;
    }

    fn overrideInterface(self: *Program, method_index: usize, nominal_ty: TypeId) Allocator.Error!TypeId {
        return switch (method_index) {
            0 => try self.store.intern(.{ .function = .{ .args = &.{ nominal_ty, nominal_ty }, .ret = try self.store.primitive(.bool), .effectful = false } }),
            1 => blk: {
                const hasher = try self.defaultHasher();
                break :blk try self.store.intern(.{ .function = .{ .args = &.{ nominal_ty, hasher }, .ret = hasher, .effectful = false } });
            },
            2 => try self.store.intern(.{ .function = .{ .args = &.{nominal_ty}, .ret = try self.store.primitive(.str), .effectful = false } }),
            else => unreachable, // `Methods` has three overrides
        };
    }

    /// `target` (a template's worker) whose scheme takes `bound`, with the
    /// requirements of its dictionaries at those types.
    pub fn targetRequirements(self: *Program, target: WorkerId, bound: []const TypeId) Allocator.Error!TargetInstance {
        const target_worker = self.worker(target).*;
        const requirements = try self.arena.allocator().alloc(DictRequirement, target_worker.dictionary_params.len);
        if (requirements.len == 0) return .{ .bound = bound, .requirements = requirements };
        const key = switch (target_worker.key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => invariant("a worker with dictionary parameters is not a template's"),
        };
        const templates = self.module(key.module).view.checked_procedure_templates;
        const params = poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items[@backingInt(key.template)].evidence_params);
        const env: types.Env = .{ .module = key.module, .vars = target_worker.scheme_vars, .bound = bound };
        for (target_worker.dictionary_params, requirements) |param_index, *requirement| {
            const param = params[param_index];
            requirement.* = .{
                .module = key.module,
                .method = param.method,
                .structural = param.structural,
                .dispatcher = try self.converter().convert(key.module, param.dispatcher_ty, env),
                .callable = try self.converter().convert(key.module, param.callable_ty, env),
            };
        }
        return .{ .bound = bound, .requirements = requirements };
    }

    /// `target` instantiated at the callable `callable` its requirement
    /// names (`instantiateTarget`).
    pub fn targetAt(self: *Program, target: WorkerId, callable: TypeId) Allocator.Error!TargetInstance {
        const target_worker = self.worker(target).*;
        const formal = try self.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } });
        return try self.instantiateTarget(target, formal, callable);
    }

    /// Where placeholder variables start: a variable at or past this index
    /// stands for a target's scheme variable `instantiateTarget` has not yet
    /// determined, never for a worker's scope variable.
    const placeholder_base: u32 = 1 << 30;

    /// `target` (a template's worker) instantiated by relating its type
    /// `formal` (over its scheme variables) to `actual`, as checking relates
    /// a selected target's scheme to the callable it is used at. A variable
    /// `actual` leaves unrelated is one the target's own requirements fix: an
    /// requirement whose dispatcher is determined resolves to a method whose
    /// own type, related back to the requirement's callable, determines the
    /// variables that callable names, until no requirement determines more.
    /// A variable nothing determines seals as a variable no use relates.
    pub fn instantiateTarget(self: *Program, target: WorkerId, formal: TypeId, actual: TypeId) Allocator.Error!TargetInstance {
        const target_worker = self.worker(target).*;
        const count = target_worker.scheme_vars.len;
        const related = try self.relateVariablesPartial(self.allocator, &.{.{ formal, actual }}, count);
        defer self.allocator.free(related);
        if (target_worker.dictionary_params.len != 0) {
            const key = switch (target_worker.key) {
                .template => |key| key,
                .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => invariant("a worker with dictionary parameters is not a template's"),
            };
            const templates = self.module(key.module).view.checked_procedure_templates;
            const all_params = poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items[@backingInt(key.template)].evidence_params);
            const params = try self.allocator.alloc(static_dispatch.EvidenceParamRecord, target_worker.dictionary_params.len);
            defer self.allocator.free(params);
            for (target_worker.dictionary_params, params) |param_index, *param| param.* = all_params[param_index];
            try self.relateThroughRequirements(key.module, target_worker.scheme_vars, params, related);
        }
        const bound = try self.arena.allocator().alloc(TypeId, count);
        for (related, bound, target_worker.scheme_vars) |ty, *slot, variable| slot.* = ty orelse try self.converter().sealedScheme(target_worker.module, variable);
        return try self.targetRequirements(target, bound);
    }

    /// Fill the variables of `vars` (of `module`) that `related` leaves
    /// unrelated from the requirements `params` (evidence parameters over
    /// `vars`): a requirement whose dispatcher is determined resolves to a
    /// method whose own type, related back to the requirement's callable,
    /// determines the variables that callable names, until no requirement
    /// determines more.
    pub fn relateThroughRequirements(self: *Program, module_index: ModuleIndex, vars: []const checked.CheckedTypeId, params: []const static_dispatch.EvidenceParamRecord, related: []?TypeId) Allocator.Error!void {
        const count = vars.len;
        const partial = try self.allocator.alloc(TypeId, count);
        defer self.allocator.free(partial);
        const own = try self.allocator.alloc(TypeId, count);
        defer self.allocator.free(own);
        for (own, 0..) |*slot, index| slot.* = try self.store.variable(@intCast(index));
        const own_env: types.Env = .{ .module = module_index, .vars = vars, .bound = own };
        var changed = true;
        while (changed) {
            changed = false;
            for (related, partial, 0..) |ty, *slot, index| slot.* = ty orelse try self.store.variable(placeholder_base + @as(u32, @intCast(index)));
            const env: types.Env = .{ .module = module_index, .vars = vars, .bound = partial };
            for (params) |param| {
                const dispatcher = try self.converter().convert(module_index, param.dispatcher_ty, env);
                if (try self.mentionsPlaceholder(dispatcher)) continue;
                const callable = try self.converter().convert(module_index, param.callable_ty, env);
                const method_target = switch (try self.resolveRequirement(.{ .module = module_index, .method = param.method, .structural = param.structural, .dispatcher = dispatcher, .callable = callable })) {
                    .target => |method_target| method_target,
                    .structural, .crash, .scope_variable, .derived => continue,
                };
                // The method's own type at the requirement's callable: its
                // variables the callable determines take those types, the
                // rest stay placeholders.
                const method_worker = self.worker(method_target).*;
                const method_formal = try self.store.intern(.{ .function = .{ .args = method_worker.params, .ret = method_worker.ret, .effectful = false } });
                const method_related = try self.relateVariablesPartial(self.allocator, &.{.{ method_formal, callable }}, method_worker.scheme_vars.len);
                defer self.allocator.free(method_related);
                const method_bound = try self.allocator.alloc(TypeId, method_related.len);
                defer self.allocator.free(method_bound);
                for (method_related, method_bound, 0..) |ty, *slot, index| slot.* = ty orelse try self.store.variable(placeholder_base + @as(u32, @intCast(count + index)));
                const method_ty = try self.substitute(self.allocator, method_formal, method_bound);
                const back = try self.relateVariablesPartial(self.allocator, &.{.{ try self.converter().convert(module_index, param.callable_ty, own_env), method_ty }}, count);
                defer self.allocator.free(back);
                for (related, back) |*slot, determined| {
                    if (slot.* != null) continue;
                    const ty = determined orelse continue;
                    if (try self.mentionsPlaceholder(ty)) continue;
                    slot.* = ty;
                    changed = true;
                }
            }
        }
    }

    /// Whether `root` names a placeholder variable (`placeholder_base`).
    fn mentionsPlaceholder(self: *Program, root: TypeId) Allocator.Error!bool {
        var visited: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer visited.deinit(self.allocator);
        var stack: std.ArrayList(TypeId) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |ty| {
            if ((try visited.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (self.store.get(ty)) {
                .variable => |index| if (index >= placeholder_base) return true,
                .primitive, .empty_record => {},
                .record => |record| {
                    for (self.store.fieldSpan(record.fields)) |field| try stack.append(self.allocator, field.ty);
                    if (record.ext) |ext| try stack.append(self.allocator, ext);
                },
                .tuple => |items| try stack.appendSlice(self.allocator, self.store.span(items)),
                .tag_union => |tag_union| {
                    for (self.store.tagSpan(tag_union.tags)) |tag| try stack.appendSlice(self.allocator, self.store.span(tag.payloads));
                    if (tag_union.ext) |ext| try stack.append(self.allocator, ext);
                },
                .list => |elem| try stack.append(self.allocator, elem),
                .box => |payload| try stack.append(self.allocator, payload),
                .nominal => |nominal| try stack.appendSlice(self.allocator, self.store.nominalArgs(nominal)),
                .function => |function| {
                    try stack.appendSlice(self.allocator, self.store.span(function.args));
                    try stack.append(self.allocator, function.ret);
                },
            }
        }
        return false;
    }

    /// What resolves `requirement`, seen from the module that states it: the
    /// method its dispatcher's owner registers; a structural derivation
    /// where the owner registers none; a crash where no value of the
    /// dispatcher can exist; and for a scope variable, a structural method
    /// over its descriptor for a method descriptors carry, or else its
    /// scope's dictionary.
    pub fn resolveRequirement(self: *Program, requirement: DictRequirement) Allocator.Error!RequirementResolution {
        const method_text = self.module(requirement.module).nameStore().methodNameText(requirement.method);
        switch (self.store.get(requirement.dispatcher)) {
            .variable => |index| return if (std.meta.stringToEnum(StructuralMethod, method_text)) |method|
                .{ .structural = .{ .method = method, .over = requirement.dispatcher } }
            else
                .{ .scope_variable = index },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => {},
        }
        if (self.methodOwner(requirement.dispatcher)) |owned| {
            if (self.registeredMethod(requirement.module, owned.module, owned.owner, method_text)) |found| switch (found) {
                .rejected => return .{ .crash = .checked_error },
                .target => |target| switch (target.kind) {
                    .procedure => |procedure| return .{ .target = try self.templateRefWorker(procedure.template) },
                    .structural => |kind| return structuralResolution(kind, requirement.dispatcher),
                    .local_proc => invariant("a method a local procedure implements belongs to a type declared in a function body, which no type-resolved requirement outside that body names"),
                },
            };
        }
        var structural_ty = requirement.dispatcher;
        while (self.store.get(structural_ty) == .nominal) structural_ty = try self.converter().backing(structural_ty);
        const uninhabited = switch (self.store.get(structural_ty)) {
            .tag_union => |tag_union| tag_union.tags.len == 0 and tag_union.ext == null,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => false,
        };
        if (uninhabited) return switch (static_dispatch.unpinnedDispatchResolution(requirement.structural)) {
            .structural => structuralResolution(requirement.structural.?, requirement.dispatcher),
            .unreachable_value => .{ .crash = .unreachable_value },
        };
        const kind = requirement.structural orelse invariant("a type-resolved requirement's dispatcher has no method and no structural derivation, which checking rejects");
        return structuralResolution(kind, requirement.dispatcher);
    }

    /// The structural resolution of derivation `kind` over `dispatcher`.
    fn structuralResolution(kind: static_dispatch.StructuralKind, dispatcher: TypeId) RequirementResolution {
        return switch (kind) {
            .equality, .hash => .{ .structural = .{ .method = structuralMethodOf(kind), .over = dispatcher } },
            .parser, .encoder, .map, .map_effectful => .{ .derived = kind },
        };
    }

    /// The structural method of derivation `kind`. Derived codecs and maps
    /// are selected by checking at the dispatch they serve, so a requirement
    /// resolved from a type alone compares or hashes.
    fn structuralMethodOf(kind: static_dispatch.StructuralKind) StructuralMethod {
        return switch (kind) {
            .equality => .is_eq,
            .hash => .to_hash,
            .parser, .encoder, .map, .map_effectful => invariant("a structural codec or map requirement resolved from a type alone, which checking records at its dispatch"),
        };
    }

    /// The method owner of `ty`, with the module whose names the owner is
    /// expressed in (a builtin owner names no module's).
    fn methodOwner(self: *Program, ty: TypeId) ?struct { module: ?ModuleIndex, owner: static_dispatch.MethodOwner } {
        return switch (self.store.get(ty)) {
            .primitive => |primitive| .{ .module = null, .owner = .{ .builtin = checked.builtinOwnerForPrimitive(primitive) } },
            .list => .{ .module = null, .owner = .{ .builtin = .list } },
            .box => .{ .module = null, .owner = .{ .builtin = .box } },
            .nominal => |nominal| blk: {
                const view = self.module(nominal.decl.module);
                const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
                break :blk .{ .module = nominal.decl.module, .owner = if (nominal.builtin) |builtin| (if (builtin == .try_)
                    .{ .nominal = declaration.nominal }
                else
                    .{ .builtin = static_dispatch.builtinOwnerForCheckedBuiltin(builtin) }) else .{ .nominal = declaration.nominal } };
            },
            .empty_record, .record, .tuple, .tag_union, .function, .variable => null,
        };
    }

    /// The registration of `owner.method_text`, as `scope_module` names the
    /// owner: an owner's methods live in its declaring module's registry
    /// alone, Builtin's for a builtin owner.
    fn registeredMethod(self: *Program, scope_module: ModuleIndex, owner_module: ?ModuleIndex, owner: static_dispatch.MethodOwner, method_text: []const u8) ?static_dispatch.CheckedMethodLookup {
        const owner_names = self.module(owner_module orelse scope_module).nameStore();
        const declaring = switch (owner) {
            .builtin => self.builtinModule(),
            .nominal => self.module(owner_module orelse scope_module),
        };
        return methodIn(declaring, owner_names, owner, method_text);
    }

    /// The Builtin module, which declares every builtin owner's methods.
    fn builtinModule(self: *Program) *const types.ModuleView {
        if (self.builtin_module) |index| return self.module(index);
        for (self.modules.views.items) |*view| {
            if (view.view.module_env.module_role == .builtin) {
                self.builtin_module = view.index;
                return view;
            }
        }
        invariant("a program reaches a builtin owner's method without the Builtin module");
    }

    fn methodIn(view: *const types.ModuleView, owner_names: anytype, owner: static_dispatch.MethodOwner, method_text: []const u8) ?static_dispatch.CheckedMethodLookup {
        const view_owner = static_dispatch.methodOwnerInImportedStore(owner_names, view.nameStore(), owner) orelse return null;
        const method = view.nameStore().lookupMethodName(method_text) orelse return null;
        return view.view.method_registry.lookup(.{ .owner = view_owner, .method = method });
    }

    /// The worker of an adapter function value (see `WorkerKey.adapter`).
    pub fn adapterWorker(self: *Program, outer: WorkerId, from: TypeId, to: TypeId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .adapter = .{ .outer = outer, .from = from, .to = to } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const outer_worker = self.worker(outer).*;
        const signature = switch (self.store.get(to)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("an adapted function value's type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.adapter", @backingInt(id)),
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = outer_worker.module,
            .scheme_vars = outer_worker.scheme_vars,
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .capture_types = try self.arena.allocator().dupe(TypeId, &.{from}),
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// What the function value of lambda or closure expression `expr`
    /// captures: the binders of enclosing bodies whose values it needs,
    /// which are its own captures and those of the local procedures its
    /// body uses (`NestedProcSite.runtime_captures`).
    pub fn siteCaptures(self: *const Program, module_index: ModuleIndex, expr: checked.CheckedExprId) []const checked.CheckedCapture {
        const site_index = self.sites_by_expr.get(.{ .module = module_index, .expr = expr }) orelse
            invariant("a lambda or closure has no nested procedure site");
        const table = self.module(module_index).view.nested_proc_sites;
        const span = table.sites[site_index].runtime_captures;
        return table.runtime_captures[span.start..][0..span.len];
    }

    /// The worker evaluating default expression `expr` of `module_index`.
    pub fn defaultWorker(self: *Program, module_index: ModuleIndex, expr: checked.CheckedExprId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .default = .{ .module = module_index, .expr = expr } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const view = self.module(module_index);
        const ret = try self.converter().convert(module_index, view.bodies().expr(expr).ty, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const identity = self.workerIdentity(key);
        const imported = !self.ownsModule(module_index);
        const proc = if (imported) try self.addImportedProc(identity, module_index, .roc) else try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = &.{},
            .ret = ret,
            .imported = imported,
            .lowered = imported,
            .queued = true,
        });
        try self.worker_ids.put(self.allocator, key, id);
        if (imported) {
            // A default is called directly, at its closed result's layout.
            self.result.store.getProcSpecPtr(proc).ret_layout = try self.layouts.layoutOf(ret);
            self.bindImportedInterface(id);
        } else {
            try self.worklist.append(self.allocator, id);
        }
        return id;
    }

    /// The initializer of static data request `index` (see
    /// `WorkerKey.static_data`).
    pub fn staticDataWorker(self: *Program, index: u32, request: Common.StaticDataRequest, module_index: ModuleIndex) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .static_data = index };
        if (self.worker_ids.get(key)) |existing| return existing;
        const ret = try self.converter().convert(module_index, request.checked_type, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.static_data", @backingInt(id)),
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = &.{},
            .ret = ret,
            .static_data_request = request,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The wrapper of root request `index` (see `WorkerKey.root`).
    pub fn rootWorker(self: *Program, index: u32, request: checked.RootRequest, module_index: ModuleIndex) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .root = index };
        if (self.worker_ids.get(key)) |existing| return existing;
        const fn_ty = try self.converter().convert(module_index, request.checked_type, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a procedure root's checked type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.root", @backingInt(id)),
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .root_request = request,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The workers whose scopes `worker_id` runs in and that own literal
    /// literal requirements, innermost first (the order the worker's literal
    /// arguments take): the worker itself where its own scope has
    /// literal requirements, then each enclosing lambda scope that does.
    pub fn scopeOwners(self: *Program, allocator: Allocator, worker_id: WorkerId) Allocator.Error![]WorkerId {
        var owners = std.ArrayList(WorkerId).empty;
        errdefer owners.deinit(allocator);
        var current = worker_id;
        while (true) {
            const scoped = self.worker(current).*;
            const outer: ?WorkerId = switch (scoped.key) {
                .lambda => |lambda| lambda.outer,
                .template, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => null,
            };
            const own = if (outer) |o| scoped.literal_scopes.len != self.worker(o).literal_scopes.len else scoped.literal_scopes.len != 0;
            if (own) try owners.append(allocator, current);
            current = outer orelse break;
        }
        return try owners.toOwnedSlice(allocator);
    }

    /// The literal thunks of `worker_id` whose scheme takes `bound`, each
    /// an erased worker taking no arguments, one per literal requirement of its
    /// scopes in literal argument order: what an instantiation checking
    /// recorded no edge for passes as the worker's literal arguments
    /// (design.md "Literal Arguments").
    pub fn literalThunks(self: *Program, allocator: Allocator, worker_id: WorkerId, bound: []const TypeId) Allocator.Error![]WorkerId {
        const owners = try self.scopeOwners(allocator, worker_id);
        defer allocator.free(owners);
        var thunks = std.ArrayList(WorkerId).empty;
        errdefer thunks.deinit(allocator);
        for (owners) |owner| {
            const owner_worker = self.worker(owner).*;
            const owner_bound = bound[0..owner_worker.scheme_vars.len];
            for (0..owner_worker.literal_scopes[0].len) |literal_requirement| {
                try thunks.append(allocator, try self.literalThunkWorker(owner, owner_bound, @intCast(literal_requirement)));
            }
        }
        return try thunks.toOwnedSlice(allocator);
    }

    /// The row conversion worker of `module_index` from `from` to `to` (see
    /// `WorkerKey.convert`).
    pub fn convertWorker(self: *Program, module_index: ModuleIndex, from: TypeId, to: TypeId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .convert = .{ .module = module_index, .from = from, .to = to } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.convert", @backingInt(id)),
            .emission_unit = LIR.EmissionUnit.module(@backingInt(module_index)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, &.{from}),
            .ret = to,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The dictionary entry of template worker `target`.
    pub fn methodEntryWorker(self: *Program, target: WorkerId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .method_entry = target };
        if (self.worker_ids.get(key)) |existing| return existing;
        const target_worker = self.worker(target).*;
        const identity = self.workerIdentity(key);
        if (target_worker.imported) {
            // The entry of an imported template is its object's.
            const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
            const proc = try self.addImportedProc(identity, target_worker.module, .erased_record);
            try self.workers.append(self.allocator, .{
                .key = key,
                .proc = proc,
                .module = target_worker.module,
                .scheme_vars = target_worker.scheme_vars,
                .dictionary_params = &.{},
                .params = target_worker.params,
                .ret = target_worker.ret,
                .imported = true,
                .lowered = true,
                .queued = true,
            });
            try self.worker_ids.put(self.allocator, key, id);
            return id;
        }
        return try self.addErasedWorkerWithIdentity(key, target_worker.module, target_worker.scheme_vars, target_worker.params, target_worker.ret, identity);
    }

    /// The thunk of literal requirement `literal requirement` of the own scope of `target`
    /// instantiated at `bound` (`WorkerKey.literal_thunk`).
    pub fn literalThunkWorker(self: *Program, target: WorkerId, bound: []const TypeId, literal_requirement: u32) Allocator.Error!WorkerId {
        const bound_ty = try self.store.intern(.{ .tuple = bound });
        const key: WorkerKey = .{ .literal_thunk = .{ .target = target, .bound = bound_ty, .literal_requirement = literal_requirement } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const target_worker = self.worker(target).*;
        if (bound.len != target_worker.scheme_vars.len) invariant("a literal thunk's types differ from its target's scheme");
        if (target_worker.literal_scopes.len == 0 or literal_requirement >= target_worker.literal_scopes[0].len) invariant("a literal thunk names an literal_requirement its target's scope does not have");
        const ty = target_worker.literal_scopes[0][literal_requirement].ty;
        const ret = try self.converter().convert(target_worker.module, ty, .{ .module = target_worker.module, .vars = target_worker.scheme_vars, .bound = bound });
        return try self.addErasedWorker(key, target_worker.module, &.{}, &.{}, ret, "boxy.literal_thunk");
    }

    /// The thunk reading the compile-time value slot of literal argument root
    /// `root` of `module_index`, whose value has type `ty`
    /// (`WorkerKey.root_slot`).
    pub fn rootSlotThunkWorker(self: *Program, module_index: ModuleIndex, root: checked.ComptimeRootId, ty: TypeId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .root_slot = .{ .module = module_index, .root = root } };
        if (self.worker_ids.get(key)) |existing| return existing;
        return try self.addErasedWorker(key, module_index, &.{}, &.{}, ty, "boxy.root_slot");
    }

    /// The structural method worker of `method`, generic over one type `a`
    /// whose descriptor it captures.
    pub fn structuralWorker(self: *Program, module_index: ModuleIndex, method: StructuralMethod) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .structural = method };
        if (self.worker_ids.get(key)) |existing| return existing;
        const a = try self.store.variable(0);
        const params: []const TypeId, const ret: TypeId = switch (method) {
            .is_eq => .{ try self.arena.allocator().dupe(TypeId, &.{ a, a }), try self.store.primitive(.bool) },
            .to_hash => blk: {
                const hasher = try self.store.primitive(.u64);
                break :blk .{ try self.arena.allocator().dupe(TypeId, &.{ a, hasher }), hasher };
            },
            .to_inspect => .{ try self.arena.allocator().dupe(TypeId, &.{a}), try self.store.primitive(.str) },
        };
        const id = try self.addErasedWorker(key, module_index, &.{}, params, ret, "boxy.structural");
        self.worker(id).structural_descs = 1;
        return id;
    }

    /// The worker of a derived map with the checked selection `map`.
    pub fn structuralMapWorker(self: *Program, module_index: ModuleIndex, map: StructuralMap) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .structural_map = map };
        if (self.worker_ids.get(key)) |existing| return existing;
        const input = try self.store.variable(0);
        const output = try self.store.variable(1);
        const selected = try self.store.variable(2 + map.payload_index);
        const mapped = try self.store.variable(2 + map.arity);
        const transform = try self.store.intern(.{ .function = .{ .args = &.{selected}, .ret = mapped, .effectful = map.effectful } });
        const params = try self.arena.allocator().dupe(TypeId, &.{ input, transform });
        const id = try self.addErasedWorker(key, module_index, &.{}, params, output, "boxy.structural_map");
        self.worker(id).structural_descs = 3 + map.arity;
        return id;
    }

    /// The method of a crashing dictionary slot (see
    /// `WorkerKey.crash_dictionary`). It reads no arguments and never
    /// returns.
    pub fn crashDictionaryWorker(self: *Program, module_index: ModuleIndex, kind: CrashDictionary) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .crash_dictionary = kind };
        if (self.worker_ids.get(key)) |existing| return existing;
        return try self.addErasedWorker(key, module_index, &.{}, &.{}, try self.store.emptyRecord(), "boxy.crash_dictionary");
    }

    fn addErasedWorker(self: *Program, key: WorkerKey, module_index: ModuleIndex, scheme_vars: []const checked.CheckedTypeId, params: []const TypeId, ret: TypeId, role: []const u8) Allocator.Error!WorkerId {
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        return try self.addErasedWorkerWithIdentity(key, module_index, scheme_vars, params, ret, LIR.ProcIdentity.programLocal(role, @backingInt(id)));
    }

    fn addErasedWorkerWithIdentity(self: *Program, key: WorkerKey, module_index: ModuleIndex, scheme_vars: []const checked.CheckedTypeId, params: []const TypeId, ret: TypeId, identity: LIR.ProcIdentity) Allocator.Error!WorkerId {
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = scheme_vars,
            .dictionary_params = &.{},
            .params = params,
            .ret = ret,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    pub fn worker(self: *Program, id: WorkerId) *Worker {
        return &self.workers.items[@backingInt(id)];
    }

    /// The worker of a template reference, which names its module by digest.
    /// The body of the procedure binding a use names, and the module that
    /// owns it. A hosted binding is its hosted template's direct body.
    pub fn procedureBinding(self: *const Program, binding: checked.ProcedureBindingRef) struct { module: ModuleIndex, body: checked.ProcedureBindingBody } {
        return switch (binding) {
            .top_level => |top| blk: {
                const owner = self.moduleByKey(top.artifact);
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(top.binding)].body };
            },
            .imported => |imported| blk: {
                const owner = self.moduleByKey(imported.artifact);
                const exported = self.exported_bindings.get(.{ .module = owner.index, .def = imported.def, .pattern = imported.pattern }) orelse
                    invariant("an imported procedure binding is not exported by its module");
                break :blk .{ .module = owner.index, .body = exported.body };
            },
            .platform_required => |required| blk: {
                const owner = self.moduleByKey(required.artifact);
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(required.procedure_binding)].body };
            },
            .hosted => |hosted| .{ .module = self.moduleByIdx(hosted.module_idx).index, .body = .{ .direct_template = .{ .proc_value = hosted.proc, .template = .{ .checked = hosted.template } } } },
        };
    }

    /// The checked template a binding's direct body names. Checking binds
    /// every procedure to a checked template or a synthetic one wrapping it;
    /// lifted templates exist only after post-check lifting.
    pub fn directTemplate(template: checked_names.CallableProcedureTemplateRef) checked_names.ProcedureTemplateRef {
        return switch (template) {
            .checked => |ref| ref,
            .synthetic => |synthetic| synthetic.template,
            .lifted => invariant("a checked procedure binding names a lifted template, which only post-check lifting creates"),
        };
    }

    /// A compile-time root of a module of the program.
    pub const RootSlotKey = struct { module: ModuleIndex, root: checked.ComptimeRootId };

    /// Whether this program evaluates compile-time root `root` of module
    /// `module_index`: the root is one of the program's requests. A pending
    /// root no program evaluates (a literal in a branch, whose conversion
    /// runs where it is reached) has no value to read.
    pub fn evaluatesRoot(self: *const Program, module_index: ModuleIndex, root: checked.ComptimeRootId) bool {
        return self.requested_roots.contains(.{ .module = module_index, .root = root });
    }

    /// Whether a read of root `root` of module `module_index` reads its
    /// compile-time value slot: this program evaluates the root, at one
    /// type. A polymorphic constant computes at each instance it is read
    /// at, as its entry procedure's value.
    pub fn readsRootSlot(self: *const Program, module_index: ModuleIndex, root: checked.ComptimeRootId) bool {
        if (!self.evaluatesRoot(module_index, root)) return false;
        const view = self.module(module_index);
        return !types.Converter.rootHasIdentityVariables(view, view.view.compile_time_roots.root(root).checked_type);
    }

    /// The static data slot holding the value of compile-time root `root` of
    /// module `module_index`, of type `ty`, once the evaluator completes it:
    /// a compile-time value slot with a failure record, which every read
    /// guards (`lir.ComptimeValueGuards`). The evaluator marks the slot
    /// failed before any root runs, so a read of a root not yet completed
    /// fails as the recursion evaluating the value in place would end in,
    /// and the evaluator reruns the reader once the root completes
    /// (design.md "Compile-Time Evaluation Strategy").
    pub fn rootValueSlot(self: *Program, module_index: ModuleIndex, root: checked.ComptimeRootId, ty: TypeId) Allocator.Error!LIR.StaticDataId {
        const key: RootSlotKey = .{ .module = module_index, .root = root };
        if (self.root_value_slots.get(key)) |slot| return slot;
        const module_key = self.module(module_index).key;
        const failure_layout = try self.result.layouts.putStructFields(&.{
            .{ .index = 0, .layout = .u8 },
            .{ .index = 1, .layout = .str },
        });
        const struct_idx = self.result.layouts.getLayout(failure_layout).getStruct().idx;
        const failure_slot: LIR.StaticDataId = @fromBackingInt(@intCast(self.result.static_data_values.items.len));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = failure_layout,
            .compile_time_root = .{
                .module = module_key,
                .root = .{ .checked = root },
                .const_locator = null,
                .role = .{ .failure_message = .{
                    .failed_field = 0,
                    .message_field = 1,
                    .failed_offset = self.result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
                    .message_offset = self.result.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
                } },
            },
        });
        const slot: LIR.StaticDataId = @fromBackingInt(@intCast(self.result.static_data_values.items.len));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = null,
            .layout_idx = try self.layouts.layoutOf(ty),
            .compile_time_root = .{
                .module = module_key,
                .root = .{ .checked = root },
                .const_locator = null,
                .role = .{ .value = .{ .failure_slot = failure_slot, .plan = try const_schema.constPlanOf(self, ty) } },
            },
        });
        try self.root_value_slots.put(self.allocator, key, slot);
        return slot;
    }

    pub fn templateRefWorker(self: *Program, ref: checked_names.ProcedureTemplateRef) Allocator.Error!WorkerId {
        const view = self.moduleByKey(.{ .bytes = ref.artifact.bytes });
        return try self.templateWorker(view.index, ref.template);
    }

    fn drain(self: *Program) Allocator.Error!void {
        while (self.worklist.pop()) |id| {
            if (self.worker(id).lowered) continue;
            self.worker(id).lowered = true;
            try Body.lowerWorker(self, id);
        }
    }
};

/// The items of `pool` that `span` names.
pub fn poolSlice(comptime T: type, pool: []const T, span: anytype) []const T {
    return pool[span.start..][0..span.len];
}

/// Lower the requested roots and everything they reach.
pub fn run(allocator: Allocator, input: Input) Allocator.Error!Output {
    base.dev_diagnostics.traceBoxyWork("boxy lowering");
    var result = try LirProgram.Result.init(allocator, input.target_usize);
    const program = allocator.create(Program) catch |err| {
        result.deinit();
        return err;
    };
    program.* = .{
        .allocator = allocator,
        .input = input,
        .modules = .{ .allocator = allocator, .link_names = switch (input.unit) {
            .program => null,
            .module => |unit| unit.names,
        } },
        .store = types.Store.init(allocator),
        .layouts = undefined,
        .result = result,
        .static_descs = undefined,
        .observe_expects = input.roots.test_plan_metadata.len != 0,
        .list_in_place_map = input.list_in_place_map,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    var retained = false;
    defer if (!retained) {
        program.release();
        allocator.destroy(program);
    };
    errdefer program.result.deinit();
    program.result.store.runtime_contract = .boxy;
    program.layouts = .{
        .allocator = allocator,
        .store = &program.store,
        .converter = program.converter(),
        .layouts = &program.result.layouts,
    };
    program.static_descs = StaticDescs.init(allocator, &program.store, &program.layouts, &program.result, input.target_usize);

    _ = try program.modules.add(checked.importedView(input.modules.root.module));
    for (input.modules.imports) |view| _ = try program.modules.add(view);
    for (input.modules.root.relation_modules) |view| _ = try program.modules.add(view);
    for (program.modules.views.items) |view| {
        try program.by_module_idx.put(allocator, view.view.module_identity.module_idx, view.index);
        for (view.view.exported_procedure_bindings.bindings) |*binding| {
            try program.exported_bindings.put(allocator, .{ .module = view.index, .def = binding.binding.def, .pattern = binding.binding.pattern }, binding);
        }
        for (view.view.callable_eval_templates.templates) |template| {
            try program.callable_eval_by_pattern.put(allocator, .{ .module = view.index, .pattern = template.pattern }, template.id);
        }
        for (view.view.entry_wrappers.wrappers.items) |wrapper| {
            try program.wrapper_by_root.put(allocator, .{ .module = view.index, .root = wrapper.root }, wrapper.template);
        }
    }
    try solvePlatformIdentities(program);
    try seedSourceFiles(program);
    try initHosted(program);
    for (program.modules.views.items) |*view| {
        const bodies = view.bodies();
        for (view.view.checked_procedure_templates.dispatch_scopes, 0..) |scope, index| {
            const id: checked.DispatchScopeId = @fromBackingInt(@intCast(index));
            try program.local_scopes.put(allocator, .{ .module = view.index, .expr = scope.checked_expr }, id);
            switch (bodies.expr(scope.checked_expr).data) {
                .closure => |closure| try program.local_scopes.put(allocator, .{ .module = view.index, .expr = closure.lambda }, id),
                .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => {},
            }
        }
        try program.by_identity.put(allocator, view.view.module_identity.stable_hash, view.index);
        for (view.view.nested_proc_sites.sites, 0..) |site, site_index| {
            const site_expr = site.checked_expr orelse continue;
            try program.sites_by_expr.put(allocator, .{ .module = view.index, .expr = site_expr }, @intCast(site_index));
            // A closure's site is also its lambda's, which names its worker.
            const site_data = view.bodies().expr(site_expr).data;
            if (site_data == .closure) try program.sites_by_expr.put(allocator, .{ .module = view.index, .expr = site_data.closure.lambda }, @intCast(site_index));
        }
        for (bodies.record_omitted_defaults) |omitted| {
            const name = try program.store.internName(view.nameStore().recordFieldLabelText(omitted.field_name));
            try program.omitted_defaults.put(allocator, .{ .module = view.index, .expr = omitted.expr, .name = name }, omitted.default);
        }
        // A method of a type declared in a function body is a local
        // procedure its dispatches call, each at its own instantiation.
        for (view.view.method_registry.entries) |entry| {
            const target = entry.target orelse continue;
            switch (target.kind) {
                .local_proc => |local| {
                    try program.local_procs.put(allocator, .{ .module = view.index, .expr = local.expr }, {});
                    switch (bodies.expr(local.expr).data) {
                        .closure => |closure| try program.local_procs.put(allocator, .{ .module = view.index, .expr = closure.lambda }, {}),
                        .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => {},
                    }
                },
                .procedure, .structural => {},
            }
        }
        // A local procedure's uses all reach it through its binding, so its
        // own scheme's arguments are passed per call.
        for (view.view.resolved_value_refs.records) |record| {
            const local = switch (record.ref) {
                .local_proc => |local| local,
                .local_param, .local_value, .local_mutable_version, .pattern_binder, .selected_hoisted_const, .top_level_const, .imported_const, .top_level_proc, .imported_proc, .hosted_proc, .platform_required_declaration, .platform_required_checked_error, .platform_required_const, .platform_required_proc, .promoted_top_level_proc => continue,
            };
            try program.local_procs.put(allocator, .{ .module = view.index, .expr = local.expr }, {});
            switch (bodies.expr(local.expr).data) {
                .closure => |closure| try program.local_procs.put(allocator, .{ .module = view.index, .expr = closure.lambda }, {}),
                .pending, .numeral, .str_from_quote, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => {},
            }
        }
    }

    // A module unit lowers every worker of its modules as a root: the
    // workers other objects reach by symbol (design.md "Per-Module Output").
    // A compile-time program's unit lowers only what its roots reach: nothing
    // links against it.
    if (program.moduleUnit() != null and !input.compile_time) try seedUnitWorkers(program);

    const requests = input.roots.requests;
    for (requests, 0..) |request, index| {
        const root_id = request.compile_time_root orelse continue;
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        try program.requested_roots.put(allocator, .{ .module = source_module, .root = root_id }, {});
    }
    for (requests, 0..) |request, index| {
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        const worker_id = try lowerRoot(program, request, @intCast(index), source_module);
        const proc = program.worker(worker_id).proc;
        try program.result.root_procs.append(allocator, proc);
        // A compile-time evaluator stores each root's value by the plan
        // and stored type of the root's own result type. A lowering that
        // evaluates none (a test lowering one constant's root) runs the
        // root as a procedure producing the value.
        if (request.abi == .compile_time and input.compile_time) {
            const ret = program.worker(worker_id).ret;
            // A literal argument root whose value holds a function is
            // validated and discarded: nothing stores its value (design.md
            // "Literal Arguments").
            const validate_only = if (request.compile_time_root) |root_id| blk: {
                const root = program.module(source_module).view.compile_time_roots.root(root_id);
                break :blk if (root.literal_argument) |argument| argument.validate_only else false;
            } else false;
            const stored_ty = if (validate_only) try program.store.emptyRecord() else ret;
            try program.result.const_roots.append(allocator, .{
                .root_order = @intCast(index),
                .owner = program.lowering_module_ids[@backingInt(source_module)],
                .request = request,
                .proc = proc,
                .ret_layout = try program.layouts.layoutOf(ret),
                .ret_type = try const_schema.constTypeOf(program, stored_ty),
                .plan = try const_schema.constPlanOf(program, stored_ty),
            });
        }
        var metadata = RootMetadata.fromCheckedRoot(request);
        metadata.test_plan = Common.testPlanMetadataForRoot(input.roots, request, index);
        if (input.host_boundary_digests and request.abi == .platform) {
            metadata.host_boundary_digest = try hostBoundaryDigest(program, request, source_module);
        }
        try program.result.root_metadata.append(allocator, metadata);
    }
    try program.drain();
    for (input.roots.layout_requests) |checked_ty| {
        const ty = try program.converter().convert(program.modules.views.items[0].index, checked_ty, .{ .module = program.modules.views.items[0].index, .vars = &.{}, .bound = &.{} });
        _ = try program.layouts.layoutOf(ty);
    }
    // Each static data request is served by an initializer that builds the
    // requested constant, which static data materialization evaluates.
    if (input.roots.static_data_requests.len != 0) {
        const root_module = program.modules.views.items[0].index;
        const initializers = try allocator.alloc(WorkerId, input.roots.static_data_requests.len);
        defer allocator.free(initializers);
        for (input.roots.static_data_requests, initializers, 0..) |request, *initializer, index| {
            initializer.* = try program.staticDataWorker(@intCast(index), request, root_module);
        }
        try program.drain();
        for (input.roots.static_data_requests, initializers) |request, initializer| {
            const worker = program.worker(initializer).*;
            program.result.store.getProcSpecPtr(worker.proc).is_static_initializer = true;
            const plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.result.const_plans.items.len));
            try program.result.const_plans.append(allocator, .layout_only);
            try program.result.requested_layouts.append(allocator, .{
                .ty = program.module(root_module).types().rootKey(request.checked_type),
                .checked_type = request.checked_type,
                .const_locator = request.const_locator,
                .layout_idx = try program.layouts.layoutOf(worker.ret),
                .plan = plan,
                .initializer = worker.proc,
            });
        }
    }

    if (program.moduleUnit() != null and !input.compile_time) {
        for (program.workers.items) |worker| {
            if (worker.imported or !Program.exportableKind(worker.key)) continue;
            program.result.store.getProcSpecPtr(worker.proc).exported = true;
            try program.result.exported_procs.append(allocator, worker.proc);
        }
    }

    if (input.proc_debug_names) {
        for (program.workers.items) |worker| {
            // A template's worker carries its procedure's source name.
            const name: []const u8 = switch (worker.key) {
                .codec => |piece| switch (program.codec.pieces.items[piece].kind) {
                    inline .constructor, .runtime, .encode_helper, .parse_helper, .record_body, .tuple_body, .list_body, .dict_body, .tag_body, .value_thunk, .key_thunk, .tag_spec, .field_names_step => |kind| "boxy.codec." ++ @tagName(kind),
                },
                .template => |key| blk: {
                    const view = program.module(key.module);
                    const template = view.view.checked_procedure_templates.templates.items[@backingInt(key.template)];
                    const names = view.nameStore();
                    const export_name = names.procBase(template.proc_base).export_name orelse break :blk "boxy.template";
                    break :blk names.exportNameText(export_name);
                },
                inline .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot => |_, kind| "boxy." ++ @tagName(kind),
            };
            try program.result.store.setProcDebugName(worker.proc, name);
        }
    }
    program.result.finishExpectSites();
    const static_data = try program.static_descs.takeExports();
    const kept = if (input.stored_functions) try StoredFn.retain(allocator, program) else null;
    retained = kept != null;
    return .{
        .lir_result = program.result,
        .static_data = static_data,
        .retained = kept,
    };
}

/// Queue every worker a module unit's modules define for other objects:
/// each template and its dictionary entry (a template is a function value
/// wherever another module names it, and a method wherever one dispatches
/// to it), and each `??` default expression (design.md "Per-Module
/// Output").
fn seedUnitWorkers(program: *Program) Allocator.Error!void {
    for (program.modules.views.items) |*view| {
        if (!program.ownsModule(view.index)) continue;
        for (view.view.checked_procedure_templates.templates.items, 0..) |template, index| {
            if (!unitEmitsTemplate(view, template)) continue;
            const worker = try program.templateWorker(view.index, @fromBackingInt(@intCast(index)));
            _ = try program.methodEntryWorker(worker);
        }
        for (view.view.method_registry.entries) |entry| {
            const target = entry.target orelse continue;
            const template = switch (target.kind) {
                .procedure => |procedure| procedure.template,
                .local_proc, .structural => continue,
            };
            const worker = try program.templateRefWorker(template);
            if (program.worker(worker).imported) continue;
            _ = try program.methodEntryWorker(worker);
        }
        for (view.bodies().default_exprs) |default| {
            _ = try program.defaultWorker(view.index, default.checked_expr);
        }
    }
    try program.drain();
}

/// Whether a module unit emits `template` (design.md "Per-Module Output"):
/// every template another object may call or compose, which leaves out the
/// wrappers of `expect` and REPL roots. Nothing reaches those but a test
/// plan's or a REPL's own roots, and each of those lowers as one program.
fn unitEmitsTemplate(view: *const types.ModuleView, template: checked.CheckedProcedureTemplate) bool {
    return switch (template.body) {
        .checked_body, .intrinsic_wrapper, .unimplemented => true,
        .entry_wrapper => |wrapper_id| switch (view.view.compile_time_roots.root(view.view.entry_wrappers.get(wrapper_id).root).kind) {
            .expect, .repl_expr => false,
            .callable_binding, .constant, .hoisted_constant, .hoisted_validation, .numeral_conversion, .quote_conversion, .literal_argument => true,
        },
    };
}

fn seedSourceFiles(program: *Program) Allocator.Error!void {
    const allocator = program.allocator;
    const count = program.modules.views.items.len;
    const order = try allocator.alloc(u32, count);
    defer allocator.free(order);
    for (order, 0..) |*slot, index| slot.* = @intCast(index);
    std.mem.sort(u32, order, program, struct {
        fn lessThan(p: *Program, a: u32, b: u32) bool {
            const a_view = &p.modules.views.items[a];
            const b_view = &p.modules.views.items[b];
            return std.mem.order(u8, &p.modules.linkName(a_view.index), &p.modules.linkName(b_view.index)) == .lt;
        }
    }.lessThan);
    program.source_files = try program.arena.allocator().alloc(u32, count);
    program.lowering_module_ids = try program.arena.allocator().alloc(LIR.LoweringModuleId, count);
    const files = try allocator.alloc(base.SourceFileEntry, count);
    defer allocator.free(files);
    for (order, 0..) |module_position, file_index| {
        const view = &program.modules.views.items[module_position];
        files[file_index] = .{
            .name = view.view.module_env.module_name,
            .qualified_name = view.view.module_env.qualifiedModuleName(),
            .module_identity = program.modules.linkName(view.index),
        };
        program.source_files[module_position] = @intCast(file_index);
        program.lowering_module_ids[module_position] = try program.result.appendLoweringModule(view.key);
    }
    try program.result.store.setSourceFiles(files);
}

/// Bind every hosted procedure in scope to its external symbol and dispatch
/// slot: the platform header's hosted section, in order, when a platform is
/// in scope; otherwise every declaration in a stable order.
fn initHosted(program: *Program) Allocator.Error!void {
    const allocator = program.allocator;
    const DefKey = struct { module: ModuleIndex, def: u32 };
    var by_def: std.AutoHashMapUnmanaged(DefKey, TemplateKey) = .empty;
    defer by_def.deinit(allocator);
    const Ordered = struct { key: TemplateKey, order: []const u8, def: u32, digest: [32]u8 };
    var ordered: std.ArrayList(Ordered) = .empty;
    defer ordered.deinit(allocator);
    var platform: ?*const types.ModuleView = null;
    for (program.modules.views.items) |*view| {
        if (view.view.module_identity.kind == .platform) platform = view;
        for (view.view.hosted_procs.procs) |proc| {
            const owner = program.moduleByKey(.{ .bytes = proc.template.artifact.bytes });
            const key: TemplateKey = .{ .module = owner.index, .template = proc.template.template };
            try program.hosted.put(allocator, key, .{
                .symbol = view.view.canonical_names.externalSymbolNameText(proc.external_symbol_name),
                .dispatch_index = null,
            });
            try by_def.put(allocator, .{ .module = view.index, .def = @backingInt(proc.def_idx) }, key);
            try ordered.append(allocator, .{ .key = key, .order = proc.orderKey(view.view.hosted_procs), .def = @backingInt(proc.def_idx), .digest = view.key.bytes });
        }
    }
    if (platform) |view| {
        for (view.view.hosted_bindings.bindings, 0..) |binding, index| {
            const target = switch (program.input.unit) {
                .program => program.moduleByKey(binding.target_checked_module),
                // A unit binds the hosted functions of the modules it lowers;
                // a binding of another module's function is that module's
                // unit's.
                .module => program.modules.find(binding.target_checked_module) orelse continue,
            };
            const key = by_def.get(.{ .module = target.index, .def = @backingInt(binding.target_def) }) orelse
                invariant("a hosted section names a function with no hosted declaration in scope");
            program.hosted.getPtr(key).?.* = .{
                .symbol = view.view.canonical_names.externalSymbolNameText(binding.external_symbol_name),
                .dispatch_index = @intCast(index),
            };
        }
        return;
    }
    std.mem.sort(Ordered, ordered.items, {}, struct {
        fn lessThan(_: void, a: Ordered, b: Ordered) bool {
            return switch (std.mem.order(u8, a.order, b.order)) {
                .lt => true,
                .gt => false,
                .eq => if (a.def != b.def) a.def < b.def else std.mem.order(u8, &a.digest, &b.digest) == .lt,
            };
        }
    }.lessThan);
    for (ordered.items, 0..) |item, index| program.hosted.getPtr(item.key).?.dispatch_index = @intCast(index);
}

/// Bind each platform requirement identity variable to the app type the
/// app's checking solved it to: the platform's code is lowered at the types
/// the app provides for its requirements.
fn solvePlatformIdentities(program: *Program) Allocator.Error!void {
    const platform = for (program.modules.views.items) |*view| {
        if (view.view.platform_type_inputs.requirements.len != 0) break view;
    } else return;
    for (program.modules.views.items) |*app| {
        const table = app.view.platform_requirement_solutions;
        for (table.solutions) |solution| {
            if (solution.requires_idx >= platform.view.platform_type_inputs.requirements.len) invariant("an app solves a requirement its platform does not declare");
            const requirement = platform.view.platform_type_inputs.requirements[solution.requires_idx];
            const formals = platform.view.platform_type_inputs.identities[requirement.identities.start..][0..requirement.identities.len];
            const solved = table.identity_solutions[solution.identity_start..][0..solution.identity_len];
            if (formals.len != solved.len) invariant("an app's requirement solution differs from its requirement's identity variables");
            for (formals, solved) |formal, app_ty| {
                const ty = try program.converter().convert(app.index, app_ty, .{ .module = app.index, .vars = &.{}, .bound = &.{} });
                try program.platform_identities.put(program.allocator, .{ .module = platform.index, .ty = formal }, ty);
            }
        }
    }
}

/// A root's worker: its template's, which a host-shaped wrapper would call
/// with static hidden arguments when the worker has any.
fn lowerRoot(program: *Program, request: checked.RootRequest, index: u32, source_module: ModuleIndex) Allocator.Error!WorkerId {
    // A root whose target takes no hidden arguments is the target itself.
    const direct: ?checked_names.ProcedureTemplateRef = if (request.procedure_template) |template|
        template
    else if (request.procedure_binding) |binding| switch (program.module(source_module).view.top_level_procedure_bindings.bindings.items[@backingInt(binding)].body) {
        .direct_template => |direct| Program.directTemplate(direct.template),
        // A rejected binding's root is a wrapper that crashes.
        .callable_eval_template, .checked_error => null,
    } else null;
    if (direct) |template| {
        const id = try program.templateRefWorker(template);
        const worker = program.worker(id);
        if (worker.scheme_vars.len == 0 and worker.dictionary_params.len == 0) return id;
    }
    return try program.rootWorker(index, request, source_module);
}

/// The host boundary digest of a platform entrypoint: the types and layouts
/// its root procedure takes and returns.
fn hostBoundaryDigest(program: *Program, request: checked.RootRequest, source_module: ModuleIndex) Allocator.Error!host_boundary.Digest {
    const allocator = program.allocator;
    const root_ty = try program.converter().convert(source_module, request.checked_type, .{ .module = source_module, .vars = &.{}, .bound = &.{} });
    var values: std.ArrayList(HostBoundaryGraph.Edge) = .empty;
    defer values.deinit(allocator);
    const graph: HostBoundaryGraph = .{ .program = program };
    switch (program.store.get(root_ty)) {
        .function => |function| try graph.abiValues(function.args, function.ret, &values),
        .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => try values.append(allocator, .{ .node = root_ty, .position = 0, .layout = try program.layouts.layoutOf(root_ty) }),
    }
    var walk = host_boundary.Walk(TypeId, HostBoundaryGraph, Allocator.Error).init(allocator, &program.result.layouts, graph);
    defer walk.deinit();
    return try walk.signature(values.items);
}

/// The `.boxy` types a host boundary digest walks, numbered as their
/// committed layouts number fields and variants. Every function value is an
/// erased callable whose ABI is its function type's arguments and result.
const HostBoundaryGraph = struct {
    program: *Program,

    const Edge = host_boundary.Edge(TypeId);

    pub fn shape(self: HostBoundaryGraph, ty: TypeId, edges: *std.ArrayList(Edge)) Allocator.Error!host_boundary.Shape(TypeId) {
        const program = self.program;
        const allocator = program.allocator;
        switch (program.store.get(ty)) {
            .primitive, .empty_record => return .leaf,
            .variable => invariant("a type variable reached a host boundary"),
            .nominal => |nominal| return if (nominal.has_backing) .{ .transparent = try program.layouts.converter.backing(ty) } else .leaf,
            .list => |elem| try edges.append(allocator, .{ .node = elem, .position = 0 }),
            // `Box(function)` is the function's erased callable.
            .box => |payload| {
                if (program.store.get(payload) == .function) return .{ .transparent = payload };
                try edges.append(allocator, .{ .node = payload, .position = 0 });
            },
            .tuple => |items| for (program.store.span(items), 0..) |item, index| {
                try edges.append(allocator, .{ .node = item, .position = @intCast(index) });
            },
            .record => |record| {
                if (record.ext != null) invariant("an open record reached a host boundary");
                // Storage interning can grow the store under its own spans.
                const fields = try allocator.dupe(types.Field, program.store.fieldSpan(record.fields));
                defer allocator.free(fields);
                for (fields, 0..) |field, index| {
                    try edges.append(allocator, .{ .node = try program.layouts.fieldStorage(field), .position = @intCast(index) });
                }
            },
            .tag_union => |tag_union| {
                if (tag_union.ext != null) invariant("an open tag union reached a host boundary");
                const tags = try allocator.dupe(types.Tag, program.store.tagSpan(tag_union.tags));
                defer allocator.free(tags);
                for (tags, 0..) |tag, index| {
                    const payloads = try allocator.dupe(TypeId, program.store.span(tag.payloads));
                    defer allocator.free(payloads);
                    const payload = try program.layouts.tagPayload(payloads) orelse continue;
                    try edges.append(allocator, .{ .node = payload, .position = @intCast(index) });
                }
            },
            .function => |function| {
                try self.abiValues(function.args, function.ret, edges);
                return .erased;
            },
        }
        return .container;
    }

    /// A function's ABI values: its arguments in order, then its result,
    /// each with its committed layout.
    fn abiValues(self: HostBoundaryGraph, args_span: types.Span, ret: TypeId, out: *std.ArrayList(Edge)) Allocator.Error!void {
        const program = self.program;
        const args = try program.allocator.dupe(TypeId, program.store.span(args_span));
        defer program.allocator.free(args);
        for (args, 0..) |arg, index| {
            try out.append(program.allocator, .{ .node = arg, .position = @intCast(index), .layout = try program.layouts.layoutOf(arg) });
        }
        try out.append(program.allocator, .{ .node = ret, .position = @intCast(args.len), .layout = try program.layouts.layoutOf(ret) });
    }
};
