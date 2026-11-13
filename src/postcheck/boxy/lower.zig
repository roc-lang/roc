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
const Tables = @import("tables.zig");
const ConstSchema = const_schema.ConstSchema;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const ExprId = check.CheckedBodies.ExprId;
const BinderId = check.CheckedBodies.BinderId;
const PatternId = check.CheckedBodies.PatternId;
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
    /// The program evaluates roots holding function values: each type table
    /// names itself (`StaticDescs.table_identities`), and the lowering keeps
    /// the program for reading those values (`Output.retained`).
    stored_functions: bool = false,
    /// Only a demanded compiler-only unit has persistent capture/type metadata.
    function_unit: ?[32]u8 = null,
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
    /// Whether this is a test unit (`roc test`): every inline expect it
    /// lowers gets a unit-local expect site, whether or not the unit lowers
    /// any test root (design.md "Unit-Image Test Execution").
    expect_sites: bool = false,
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
    /// For a worker that takes a type table: its encoded needs list
    /// (`StoredFn.encodeNeeds`), which an importer fills its tables from.
    table_needs: []const u8 = &.{},
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

    /// Adopt the producer's calling contracts before compiling against its
    /// existing objects. A build may already share the producer's table;
    /// tests instead own a separate table which their instrumented units fill.
    pub fn copyFrom(self: *ExportInterfaces, source: *ExportInterfaces) Allocator.Error!void {
        if (self == source) return;
        // A fixed lock order also permits independent consumers to take
        // snapshots without introducing opposing lock acquisition orders.
        const first = if (@intFromPtr(self) < @intFromPtr(source)) self else source;
        const second = if (first == self) source else self;
        first.mutex.lockUncancelable(first.io);
        defer first.mutex.unlock(first.io);
        second.mutex.lockUncancelable(second.io);
        defer second.mutex.unlock(second.io);
        var entries = source.map.iterator();
        while (entries.next()) |entry| {
            var owned = entry.value_ptr.*;
            owned.rc_ret_conditions = try self.arena.allocator().dupe(u32, owned.rc_ret_conditions);
            owned.table_needs = try self.arena.allocator().dupe(u8, owned.table_needs);
            try self.map.put(self.allocator, entry.key_ptr.*, owned);
        }
    }

    /// Record `interface` for `identity`, copying its condition rows.
    pub fn put(self: *ExportInterfaces, identity: LIR.ProcIdentity, interface: ExportInterface) Allocator.Error!void {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        var owned = interface;
        owned.rc_ret_conditions = try self.arena.allocator().dupe(u32, interface.rc_ret_conditions);
        owned.table_needs = try self.arena.allocator().dupe(u8, interface.table_needs);
        try self.map.put(self.allocator, identity, owned);
    }

    /// Record the interface ARC solved for `proc` of `store`, with the
    /// worker's encoded needs list `table_needs`, under the procedure's
    /// identity, and return it.
    pub fn putProc(self: *ExportInterfaces, store: *const lir_core.LirStore, proc: LIR.LirProcSpecId, table_needs: []const u8) Allocator.Error!ExportInterface {
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
            .table_needs = try self.arena.allocator().dupe(u8, table_needs),
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

test "export interface snapshot owns the complete calling contract" {
    const allocator = std.testing.allocator;
    var producer = ExportInterfaces.init(allocator, std.testing.io);
    defer producer.deinit();
    var consumer = ExportInterfaces.init(allocator, std.testing.io);
    defer consumer.deinit();
    const identity = LIR.ProcIdentity.forTest(1);
    const expected: ExportInterface = .{
        .rc_borrowed_params = 3,
        .rc_ret_borrowed = true,
        .rc_ret_lenders = 2,
        .rc_read_only_params = 1,
        .rc_ret_unique = true,
        .rc_ret_unique_fields = 4,
        .rc_ret_conditions = &.{ 7, 8, 9 },
        .may_leave_pending = true,
        .table_needs = "producer needs",
    };
    try producer.put(identity, expected);
    try consumer.copyFrom(&producer);
    producer.clear();
    try producer.put(identity, .{ .rc_ret_conditions = &.{ 10, 11, 12 }, .table_needs = "replaced needs" });
    try std.testing.expectEqualDeep(expected, consumer.get(identity).?);
    try consumer.copyFrom(&consumer);
    try std.testing.expectEqualDeep(expected, consumer.get(identity).?);
}

/// The lowered program and the static data it carries.
pub const Output = struct {
    lir_result: LirProgram.Result,
    static_data: []LirProgram.StaticDataExport,
    runtime_schemas: @import("../runtime_schemas.zig").RuntimeSchemaStore,
    /// With `Input.stored_functions`, the program's types and workers.
    retained: ?*StoredFn.Retained = null,
};

/// A worker's identity: what it lowers.
pub const WorkerKey = union(enum) {
    /// A procedure template, lowered at `binding`.
    template: struct { module: ModuleIndex, template: checked_names.CheckedProcedureTemplateId, binding: TemplateBinding = .generic },
    /// A function value of type `to` calling one of type `from`, which differ
    /// in the storage of record fields (design.md "Field Kinds"); both are
    /// in the variable space of the worker `outer`, whose descriptors the
    /// value captures after the called function.
    adapter: struct { outer: WorkerId, from: TypeId, to: TypeId },
    /// The `??` default expression `expr` of `module`, evaluated where a
    /// construction omits its field (design.md "Defaulted Fields").
    default: struct { module: ModuleIndex, expr: ExprId },
    /// A lambda or closure expression of the worker `outer`, whose scope
    /// (its type variables and their descriptors) it shares through its
    /// captures.
    /// `captured_scheme` is set for a local procedure's function value whose
    /// own scheme's instantiation it captures (a value compile-time
    /// evaluation stored) rather than taking per call.
    lambda: struct { module: ModuleIndex, expr: ExprId, outer: WorkerId, captured_scheme: bool = false },
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
    /// The erased entry of a method bound to a computed callable
    /// (`MethodTargetKind.callable_binding`): it takes the method's
    /// arguments as its argument record, restores the binding's function
    /// value, and calls it. A dictionary or descriptor holds this entry, so
    /// composing one never evaluates the binding's root (design.md
    /// "Procedure Aliases").
    callable_eval_entry: CallableEvalBinding,
    /// The static row conversion of `module` from the closed type `from` to
    /// the closed type `to`, whose rows widen `from`'s along the coercion's
    /// reach (design.md "Sealed Result Rows"). Each module converting a pair
    /// has its own worker for it.
    convert: struct { module: ModuleIndex, from: TypeId, to: TypeId },
};

/// What a template worker's scheme variables are bound to while its body is
/// lowered (design.md "Pre-Specialized Integer List Workers").
pub const TemplateBinding = union(enum) {
    /// Every scheme variable is a type variable: the template's one generic
    /// worker.
    generic,
    /// The list item variable of a `List` template (`Program.listItemSlot`)
    /// is the integer type; every other scheme variable is a type variable.
    list_int: ListIntItem,
};

/// The fixed-size integer types every `List` template with a checked body
/// has a pre-specialized worker for (design.md "Pre-Specialized Integer List
/// Workers"), in the declared order its module's unit lowers them in.
pub const ListIntItem = enum(u8) {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,

    /// The primitive type the list item variable is bound to.
    pub fn primitive(self: ListIntItem) types.Primitive {
        return switch (self) {
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64 => .i64,
            .u128 => .u128,
            .i128 => .i128,
        };
    }

    /// The item type whose worker a call binding the list item variable to
    /// `value` targets, or null when `value` has none.
    pub fn ofPrimitive(value: types.Primitive) ?ListIntItem {
        return switch (value) {
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64 => .i64,
            .u128 => .u128,
            .i128 => .i128,
            .bool, .str, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => null,
        };
    }

    /// The suffix of the worker's readable name (`Builtin.List.get@U32`).
    pub fn nameSuffix(self: ListIntItem) []const u8 {
        return switch (self) {
            .u8 => "@U8",
            .i8 => "@I8",
            .u16 => "@U16",
            .i16 => "@I16",
            .u32 => "@U32",
            .i32 => "@I32",
            .u64 => "@U64",
            .i64 => "@I64",
            .u128 => "@U128",
            .i128 => "@I128",
        };
    }
};

test "integer list worker identities distinguish all ten bindings and the generic worker" {
    const module_name: [32]u8 = @splat(7);
    const template: checked_names.CheckedProcedureTemplateId = @fromBackingInt(23);
    const generic = templateWorkerIdentity(module_name, template);
    const items = std.enums.values(ListIntItem);
    try std.testing.expectEqual(@as(usize, 10), items.len);
    for (items, 0..) |item, index| {
        const identity = listIntTemplateWorkerIdentity(module_name, template, item);
        try std.testing.expect(!std.meta.eql(identity, generic));
        try std.testing.expectEqual(identity, listIntTemplateWorkerIdentity(module_name, template, item));
        try std.testing.expect(!std.meta.eql(identity, listIntTemplateWorkerIdentity(@splat(8), template, item)));
        try std.testing.expect(!std.meta.eql(identity, listIntTemplateWorkerIdentity(module_name, @fromBackingInt(24), item)));
        for (items[index + 1 ..]) |other| {
            try std.testing.expect(!std.meta.eql(identity, listIntTemplateWorkerIdentity(module_name, template, other)));
        }
    }
}

/// A worker's list item variable bound to an integer type: its position
/// among the worker's scheme variables, and the type.
pub const ListIntSlot = struct { slot: u32, item: ListIntItem };

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
pub const ExportedBindingKey = struct { module: ModuleIndex, def: @FieldType(checked.ImportedProcedureBindingRef, "def"), pattern: PatternId };

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
    /// descriptor order. A template's sealed variables (design.md
    /// "Sealed Result Rows") are omitted: they seal to the empty row.
    scheme_vars: []const checked.CheckedTypeId,
    /// For a template worker: its scheme's kind variables.
    kinds: SchemeKinds = .{},
    /// For a template worker: the position of each of `scheme_vars` in the
    /// template's checked `scheme_vars`, which a use's substitution is
    /// indexed by, and that checked list's length.
    checked_slots: []const u32 = &.{},
    checked_scheme_count: u32 = 0,
    /// For a template worker lowered at `TemplateBinding.list_int`, and for
    /// every lambda worker whose scope chain such a worker roots: the
    /// position among `scheme_vars` of the list item variable and the
    /// integer type the body binds it to. Its descriptor stays an input,
    /// filled with that type's, so the worker's inputs are the generic
    /// worker's.
    list_int: ?ListIntSlot = null,
    /// Evidence parameters that are runtime dictionaries, in hidden
    /// dictionary order (indices into the template's params).
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
    /// For a worker with inputs (`Program.inputCount`), once lowered or
    /// imported: what each word of its type tables is (`tables.zig`).
    needs: ?Tables.Needs = null,

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

fn hashWord(hasher: *base.TypeDigestHasher, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

/// Mix `bytes` into `hasher` after their length, so no two splits of a key
/// into fields hash alike.
fn hashLengthPrefixed(hasher: *base.TypeDigestHasher, bytes: []const u8) void {
    var len: [8]u8 = undefined;
    std.mem.writeInt(u64, &len, bytes.len, .little);
    hasher.update(&len);
    hasher.update(bytes);
}

/// The content identity of the template worker of module `link_name`'s
/// template `template` (`Program.workerIdentity`), a function of the checked
/// module alone: what names the worker in every object of the link.
pub fn templateWorkerIdentity(link_name: [32]u8, template: checked_names.CheckedProcedureTemplateId) LIR.ProcIdentity {
    var hasher = base.TypeDigestHasher.init();
    hasher.update("roc.boxy.worker.v1");
    hasher.update("template");
    hasher.update(&link_name);
    hasher.update(std.mem.asBytes(&@as(u32, @backingInt(template))));
    return .{ .bytes = hasher.finalResult() };
}

/// The content identity of the integer list worker for `item` of module
/// `link_name`'s template `template` (`TemplateBinding.list_int`), distinct
/// from its generic worker's and its other item types' and the same in every
/// object of the link.
pub fn listIntTemplateWorkerIdentity(link_name: [32]u8, template: checked_names.CheckedProcedureTemplateId, item: ListIntItem) LIR.ProcIdentity {
    var hasher = base.TypeDigestHasher.init();
    hasher.update("roc.boxy.worker.v1");
    hasher.update("template_list_int");
    hasher.update(&link_name);
    hasher.update(std.mem.asBytes(&@as(u32, @backingInt(template))));
    hasher.update(&.{@backingInt(item)});
    return .{ .bytes = hasher.finalResult() };
}

/// A scheme variable's related type, absent when no relation fixed it.
pub const Related = collections.OptionalId(TypeId);

/// The kind variables of a scheme (design.md "Field Kinds"): the value type of
/// each variable's field, null for a variable that is no kind, and the kind
/// variables, each after those its value type mentions.
pub const SchemeKinds = struct {
    payloads: collections.OptionalIndexColumn(TypeId).View = .{},
    order: []const u32 = &.{},
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

    /// Whether the scope quantifies nothing and takes no hidden argument: no
    /// variable, dictionary, or literal argument. Its procedure is called
    /// and stored like a monomorphic one, though it stays a link of the
    /// evidence chain (`Program.lambdaWorker`).
    pub fn isEmpty(self: LocalScheme) bool {
        return self.scheme_vars.len == 0 and self.dictionaries.len == 0 and self.literal_requirements.len == 0;
    }
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

/// A callable-eval binding of module `module`: a function-valued top-level
/// binding whose compile-time root evaluates its callable.
pub const CallableEvalBinding = struct { module: ModuleIndex, template: checked.CallableEvalTemplateId };

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
    /// The erased entry of a method bound to a computed callable
    /// (`WorkerKey.callable_eval_entry`), composed with no captures.
    callable_eval: WorkerId,
    /// A derived codec or map, which checking selects with its plan at the
    /// dispatch it serves: a requirement resolved from a type alone only
    /// names it.
    derived: static_dispatch.StructuralKind,
};

/// A method target instantiated at a requirement's callable: the types its
/// scheme takes and the requirements of its dictionaries.
pub const TargetInstance = struct { bound: []const TypeId, requirements: []const DictRequirement };

/// One entry of a dictionary's requirement tree (`Program.dictionaryTree`).
pub const DictionaryNode = struct {
    /// The requirement this entry resolves; null for a tree's root target.
    requirement: ?DictRequirement,
    kind: Kind,
    /// The shallowest target on this entry's path that some back edge below
    /// it returns to, or `no_reach`.
    reaches: u32 = no_reach,

    pub const no_reach = std.math.maxInt(u32);

    pub const Kind = union(enum) {
        /// A requirement no method target resolves.
        resolved: RequirementResolution,
        target: Target,
        /// A requirement resolving to the target instance at this node, which
        /// is on the path from the root: the dictionaries capture one another.
        back_edge: u32,
    };

    pub const Target = struct {
        target: WorkerId,
        instance: TargetInstance,
        /// The node of each of `instance.requirements`.
        children: []const u32,
        /// How many targets are above this one on its path.
        depth: u32,
        /// Whether a back edge returns to this target.
        recursive: bool = false,
    };
};

/// Where a dictionary's requirement tree starts.
pub const DictionaryRoot = union(enum) {
    requirement: DictRequirement,
    target: struct { target: WorkerId, instance: TargetInstance },
};

/// The overrides of a nominal type, in `Methods` order.
pub const NominalOverrides = [3]?NominalOverride;

/// A field a record construction omits.
pub const OmittedField = struct { module: ModuleIndex, expr: ExprId, name: types.NameId };

/// A checked expression by its module in this lowering.
pub const ExprKey = struct { module: ModuleIndex, expr: ExprId };

/// A pattern binder by its module in this lowering.
pub const BinderKey = struct { module: ModuleIndex, binder: BinderId };

/// A procedure template by its module in this lowering.
pub const TemplateKey = struct { module: ModuleIndex, template: checked_names.CheckedProcedureTemplateId };

/// A hosted procedure's external symbol and its slot in the host's dispatch
/// table, or null when the platform's hosted section omits it.
pub const HostedEntry = struct { symbol: []const u8, dispatch_index: ?u32 };

/// One `.boxy` lowering: its workers, types, layouts, and output.
pub const Program = struct {
    pub const StoredValueKey = struct { module: ModuleIndex, node: checked.ConstNodeId, ty: TypeId };
    allocator: Allocator,
    input: Input,
    modules: types.Modules,
    /// Checked error proofs, owned only until this lowering returns.
    error_types: ?*types.ErrorTypes = null,
    store: types.Store,
    layouts: Layouts,
    result: LirProgram.Result,
    static_descs: StaticDescs,
    workers: std.ArrayList(Worker) = .empty,
    worker_ids: std.AutoHashMapUnmanaged(WorkerKey, WorkerId) = .empty,
    worklist: std.ArrayList(WorkerId) = .empty,
    /// Each type's content digest (`typeContent`), once computed.
    type_contents: collections.DenseMapUnmanaged(TypeId, [32]u8) = .empty,
    /// The Builtin module, found on first use (`builtinModule`).
    builtin_module: ?ModuleIndex = null,
    /// The templates the Builtin module registers as methods of `List`,
    /// collected on first use (`isListMethod`).
    list_methods: ?std.AutoHashMapUnmanaged(TemplateKey, void) = null,
    /// `listItemSlot` of each template asked about.
    list_item_slots: std.AutoHashMapUnmanaged(TemplateKey, ?u32) = .empty,
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
    stored_value_slots: std.AutoHashMapUnmanaged(StoredValueKey, LIR.StaticDataId) = .empty,
    /// The compile-time roots this program evaluates (`Input.roots`), by
    /// module and root (`evaluatesRoot`), each with its request's checked
    /// type (`rootValueType`).
    requested_roots: std.AutoHashMapUnmanaged(RootSlotKey, checked.CheckedTypeId) = .empty,
    /// The Builtin `Hasher` type, once asked for (`defaultHasher`).
    default_hasher: ?TypeId = null,
    /// Each module's callable-eval templates, by their binding pattern.
    callable_eval_by_pattern: std.AutoHashMapUnmanaged(struct { module: ModuleIndex, pattern: PatternId }, checked.CallableEvalTemplateId) = .empty,
    /// Each module's entry wrappers, by the compile-time root they wrap.
    wrapper_by_root: std.AutoHashMapUnmanaged(struct { module: ModuleIndex, root: checked.ComptimeRootId }, checked_names.ProcedureTemplateRef) = .empty,
    /// Each module's exported procedure bindings, by their definition.
    exported_bindings: std.AutoHashMapUnmanaged(ExportedBindingKey, *const checked.ImportedProcedureBindingView) = .empty,
    observe_expects: bool,
    list_in_place_map: bool,
    arena: std.heap.ArenaAllocator,
    /// The host binding of each hosted procedure template.
    hosted: std.AutoHashMapUnmanaged(TemplateKey, HostedEntry) = .empty,
    /// Body metadata is indexed only when a consumer asks about that module.
    /// Imported code called only by symbol needs no local body lookup tables.
    body_indexed: []bool = &.{},
    /// Types explicitly named by emitted compile-time descriptor notes.
    /// The generalized-local scope a lambda or closure expression owns.
    local_scopes: std.AutoHashMapUnmanaged(ExprKey, checked.DispatchScopeId) = .empty,
    /// The lambda and closure expressions local procedures bind.
    local_procs: std.AutoHashMapUnmanaged(ExprKey, void) = .empty,
    /// Each local procedure by the binder that binds it.
    local_proc_bindings: std.AutoHashMapUnmanaged(BinderKey, checked.LocalProcedureBinding) = .empty,
    /// Method declarations are immutable callables, unlike ordinary closures.
    local_method_bindings: std.AutoHashMapUnmanaged(ExprKey, checked.LocalProcedureBinding) = .empty,
    /// `nominalOverrides` of each nominal type asked about.
    nominal_overrides: collections.DenseMapUnmanaged(TypeId, NominalOverrides) = .empty,
    /// `composedInBody` of each type asked about.
    composed_in_body: collections.DenseMapUnmanaged(TypeId, bool) = .empty,
    /// The nested procedure site of each lambda and closure expression.
    sites_by_expr: std.AutoHashMapUnmanaged(ExprKey, u32) = .empty,
    /// The default a record construction that omits a field materializes.
    omitted_defaults: std.AutoHashMapUnmanaged(OmittedField, checked.CheckedFieldDefault) = .empty,
    /// The generated codec families and their workers.
    codec: Codec.State = .{},

    /// Index the checked producer's exact scope, capture, local-procedure and
    /// omitted-default records once when their module is demanded. A missing
    /// lookup means the producer has no such record, never an unbuilt index.
    fn indexBodyMetadata(self: *Program, module_index: ModuleIndex) Allocator.Error!void {
        const module_slot = @backingInt(module_index);
        if (self.body_indexed[module_slot]) return;
        const program = self;
        const allocator = self.allocator;
        const view = self.module(module_index);
        const bodies = view.bodies();
        try bodies.demandIndex();
        for (view.view.checked_procedure_templates.dispatch_scopes, 0..) |scope, index| {
            const id: checked.DispatchScopeId = @fromBackingInt(@intCast(index));
            try program.local_scopes.put(allocator, .{ .module = view.index, .expr = scope.checked_expr }, id);
            switch (bodies.exprData(scope.checked_expr)) {
                .closure => |closure| try program.local_scopes.put(allocator, .{ .module = view.index, .expr = closure.lambda }, id),
                .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => {},
            }
        }
        for (view.view.nested_proc_sites.sites, 0..) |site, site_index| {
            const site_expr = site.checked_expr.get() orelse continue;
            try program.sites_by_expr.put(allocator, .{ .module = view.index, .expr = site_expr }, @intCast(site_index));
            // A closure's site is also its lambda's, which names its worker.
            const site_data = view.bodies().exprData(site_expr);
            if (site_data == .closure) try program.sites_by_expr.put(allocator, .{ .module = view.index, .expr = site_data.closure.lambda }, @intCast(site_index));
        }
        for (bodies.recordOmittedDefaults()) |source_omitted| {
            const omitted = bodies.recordOmittedDefault(source_omitted);
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
                    try program.local_proc_bindings.put(allocator, .{ .module = view.index, .binder = local.binder }, .{ .binder = local.binder, .expr = local.expr, .dispatch_scope = local.dispatch_scope });
                    const binding: checked.LocalProcedureBinding = .{ .binder = local.binder, .expr = local.expr, .dispatch_scope = local.dispatch_scope };
                    try program.local_method_bindings.put(allocator, .{ .module = view.index, .expr = local.expr }, binding);
                    switch (bodies.exprData(local.expr)) {
                        .closure => |closure| {
                            try program.local_procs.put(allocator, .{ .module = view.index, .expr = closure.lambda }, {});
                            try program.local_method_bindings.put(allocator, .{ .module = view.index, .expr = closure.lambda }, binding);
                        },
                        .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => {},
                    }
                },
                .procedure, .structural, .callable_binding => {},
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
            try program.local_proc_bindings.put(allocator, .{ .module = view.index, .binder = local.binder }, local);
            switch (bodies.exprData(local.expr)) {
                .closure => |closure| try program.local_procs.put(allocator, .{ .module = view.index, .expr = closure.lambda }, {}),
                .numeral, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => {},
            }
        }
        self.body_indexed[module_slot] = true;
        base.dev_diagnostics.countTestWork(.boxy_body_index_prepared_modules, 1);
        base.dev_diagnostics.countTestWork(.boxy_body_index_rows, view.view.checked_procedure_templates.dispatch_scopes.len +
            view.view.nested_proc_sites.sites.len + bodies.recordOmittedDefaults().len +
            view.view.method_registry.entries.len + view.view.resolved_value_refs.records.len);
    }

    pub fn localScope(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error!?checked.DispatchScopeId {
        try self.indexBodyMetadata(module_index);
        return self.local_scopes.get(.{ .module = module_index, .expr = expr });
    }

    pub fn localProcBinding(self: *Program, module_index: ModuleIndex, binder: BinderId) Allocator.Error!?checked.LocalProcedureBinding {
        try self.indexBodyMetadata(module_index);
        return self.local_proc_bindings.get(.{ .module = module_index, .binder = binder });
    }

    pub fn localMethodBinding(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error!?checked.LocalProcedureBinding {
        try self.indexBodyMetadata(module_index);
        return self.local_method_bindings.get(.{ .module = module_index, .expr = expr });
    }

    pub fn nestedProcSite(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error!?u32 {
        try self.indexBodyMetadata(module_index);
        return self.sites_by_expr.get(.{ .module = module_index, .expr = expr });
    }

    pub fn isLocalProc(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error!bool {
        try self.indexBodyMetadata(module_index);
        return self.local_procs.contains(.{ .module = module_index, .expr = expr });
    }

    pub fn omittedDefault(self: *Program, key: OmittedField) Allocator.Error!?checked.CheckedFieldDefault {
        try self.indexBodyMetadata(key.module);
        return self.omitted_defaults.get(key);
    }

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
            .template => |template| return switch (template.binding) {
                .generic => templateWorkerIdentity(self.modules.linkName(template.module), template.template),
                .list_int => |item| listIntTemplateWorkerIdentity(self.modules.linkName(template.module), template.template, item),
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
            .adapter, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("a worker no module's object exports has no content identity"),
        }
        return .{ .bytes = hasher.finalResult() };
    }

    /// The position of default expression `expr` among its module's, which
    /// an edit elsewhere in the module leaves alone.
    fn defaultOrdinal(self: *const Program, module_index: ModuleIndex, expr: ExprId) u32 {
        for (self.module(module_index).bodies().defaultExprs(), 0..) |default, ordinal| {
            if (default == @backingInt(expr)) return @intCast(ordinal);
        }
        invariant("a default worker's expression is no default expression of its module");
    }

    /// Whether workers of `key`'s kind are the workers a module's object
    /// defines for other objects, named by content identity.
    pub fn exportableKind(key: WorkerKey) bool {
        return switch (key) {
            .template, .lambda, .method_entry, .default => true,
            .adapter, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => false,
        };
    }

    /// The identity of a procedure that exists only within this lowering,
    /// whichever kind of worker it is. `raw` numbers it within this lowering
    /// alone, and a module unit's pieces meet other units' in one program,
    /// where one identity is one procedure, so a unit's numbers are named
    /// under the unit's link name.
    pub fn programLocalIdentity(self: *const Program, role: []const u8, raw: u64) LIR.ProcIdentity {
        const local = LIR.ProcIdentity.programLocal(role, raw);
        if (self.moduleUnit() == null) return local;
        return local.derived("boxy.unit", &self.modules.linkName(self.modules.views.items[0].index));
    }

    /// The identity of `key`'s worker, of a kind the lowering that reaches it
    /// defines rather than a module's object (an adapter, a structural
    /// method or map, a crash dictionary, a literal thunk, a codec piece,
    /// a computed-callable entry, or a row conversion). In a module unit it
    /// is a digest of what the worker's code is a function of: its role, its
    /// types by content, the
    /// workers it calls by identity, and, for a kind a program has one of
    /// per module, its module by link name. Every unit lowering the same
    /// worker therefore defines one procedure, which a link keeps once. In a
    /// program it is local to the program, as `raw` names it there.
    /// `signature` is the worker's parameter and result types where its key
    /// does not determine them (a codec piece's), and empty otherwise.
    pub fn generatedWorkerIdentity(self: *Program, key: WorkerKey, module_index: ModuleIndex, raw: u64, signature: []const TypeId) Allocator.Error!LIR.ProcIdentity {
        const role: []const u8 = switch (key) {
            .adapter => "boxy.adapter",
            .structural => "boxy.structural",
            .structural_map => "boxy.structural_map",
            .crash_dictionary => "boxy.crash_dictionary",
            .literal_thunk => "boxy.literal_thunk",
            .codec => "boxy.codec",
            .convert => "boxy.convert",
            .callable_eval_entry => "boxy.callable_eval_entry",
            .template, .lambda, .default, .method_entry, .root, .static_data, .root_slot => invariant("a worker a module's object defines has no generated identity"),
        };
        if (self.moduleUnit() == null) return LIR.ProcIdentity.programLocal(role, raw);
        var hasher = base.TypeDigestHasher.init();
        hasher.update("roc.boxy.generated_worker.v1");
        hashLengthPrefixed(&hasher, role);
        switch (key) {
            .adapter => |adapter| {
                hasher.update(&self.worker(adapter.outer).identityBytes(self));
                hasher.update(&try self.typeContent(adapter.from));
                hasher.update(&try self.typeContent(adapter.to));
            },
            .structural => |method| hasher.update(&[_]u8{@backingInt(method)}),
            .structural_map => |map| {
                hashLengthPrefixed(&hasher, self.store.nameText(map.name));
                hashWord(&hasher, map.payload_index);
                hashWord(&hasher, map.arity);
                hasher.update(&[_]u8{@intFromBool(map.effectful)});
            },
            .crash_dictionary => |kind| hasher.update(&[_]u8{@backingInt(kind)}),
            .literal_thunk => |thunk| {
                hasher.update(&self.worker(thunk.target).identityBytes(self));
                hasher.update(&try self.typeContent(thunk.bound));
                hashWord(&hasher, thunk.literal_requirement);
            },
            .codec => |index| {
                // A piece belongs to its module's codec family.
                hasher.update(&self.modules.linkName(module_index));
                try Codec.hashPieceContent(self, &hasher, index);
            },
            .convert => |convert| {
                hasher.update(&self.modules.linkName(convert.module));
                hasher.update(&try self.typeContent(convert.from));
                hasher.update(&try self.typeContent(convert.to));
            },
            .callable_eval_entry => |binding| {
                // The binding determines the monomorphic signature and the
                // stored value restored by this capture-free entry.
                hasher.update(&self.modules.linkName(binding.module));
                hashWord(&hasher, @backingInt(binding.template));
            },
            .template, .lambda, .default, .method_entry, .root, .static_data, .root_slot => unreachable, // rejected above
        }
        try self.hashTypes(&hasher, signature);
        return .{ .bytes = hasher.finalResult() };
    }

    /// The content digest of `ty`, the same in every lowering: its
    /// structure, its labels by text, a nominal type by its declaration's
    /// module link name and position, and a variable by its index.
    pub fn typeContent(self: *Program, ty: TypeId) Allocator.Error![32]u8 {
        if (self.type_contents.get(ty)) |known| return known;
        const store = &self.store;
        var hasher = base.TypeDigestHasher.init();
        hasher.update("roc.boxy.type_content.v1");
        const node = store.get(ty);
        hasher.update(&[_]u8{@backingInt(std.meta.activeTag(node))});
        switch (node) {
            .primitive => |primitive| hasher.update(&[_]u8{@backingInt(primitive)}),
            .empty_record => {},
            .record => |record| {
                const fields = store.fieldSpan(record.fields);
                hashWord(&hasher, @intCast(fields.len));
                for (fields) |field| {
                    hashLengthPrefixed(&hasher, store.nameText(field.name));
                    hasher.update(&try self.typeContent(field.ty));
                    hasher.update(&[_]u8{@backingInt(field.kind)});
                    hashWord(&hasher, field.slot_variable);
                    hasher.update(&[_]u8{@intFromBool(field.default.get() != null)});
                    if (field.default.get()) |default| {
                        hasher.update(&self.modules.get(default.module).view.module_identity.stable_hash);
                        hashWord(&hasher, default.expr_node);
                    }
                }
                try self.hashOptionalType(&hasher, record.ext.get());
                if (record.declared.get()) |declared| {
                    hasher.update(&[_]u8{1});
                    try self.hashTypes(&hasher, store.span(declared.padding));
                    // The entries identify the order; the slots derived
                    // from them can coincide for different entries.
                    const entries = store.declaredEntries(declared);
                    hashWord(&hasher, @intCast(entries.len));
                    for (entries) |entry| {
                        hasher.update(&[_]u8{@backingInt(std.meta.activeTag(entry))});
                        switch (entry) {
                            .named => |name| hashLengthPrefixed(&hasher, store.nameText(name)),
                            .padding => |ordinal| hashWord(&hasher, ordinal),
                        }
                    }
                } else hasher.update(&[_]u8{0});
            },
            .tuple => |items| try self.hashTypes(&hasher, store.span(items)),
            .tag_union => |tag_union| {
                const tags = store.tagSpan(tag_union.tags);
                hashWord(&hasher, @intCast(tags.len));
                for (tags) |tag| {
                    hashLengthPrefixed(&hasher, store.nameText(tag.name));
                    try self.hashTypes(&hasher, store.span(tag.payloads));
                }
                try self.hashOptionalType(&hasher, tag_union.ext);
            },
            .list, .box => |child| hasher.update(&try self.typeContent(child)),
            .nominal => |nominal| {
                hasher.update(&self.modules.linkName(nominal.decl.module));
                hashWord(&hasher, @backingInt(nominal.decl.decl));
                try self.hashTypes(&hasher, store.nominalArgs(nominal));
                hashLengthPrefixed(&hasher, store.nameText(nominal.name));
                hasher.update(&[_]u8{ @intFromBool(nominal.is_opaque), if (nominal.builtin) |builtin| @as(u8, @backingInt(builtin)) + 1 else 0, @intFromBool(nominal.has_backing) });
            },
            .function => |function| {
                try self.hashTypes(&hasher, store.span(function.args));
                hasher.update(&try self.typeContent(function.ret));
                hasher.update(&[_]u8{@intFromBool(function.effectful)});
            },
            .variable => |index| hashWord(&hasher, index),
        }
        const digest = hasher.finalResult();
        try self.type_contents.put(self.allocator, ty, digest);
        return digest;
    }

    /// Mix the content digests of `tys` into `hasher`, after their count.
    pub fn hashTypes(self: *Program, hasher: *base.TypeDigestHasher, tys: []const TypeId) Allocator.Error!void {
        hashWord(hasher, @intCast(tys.len));
        for (tys) |ty| hasher.update(&try self.typeContent(ty));
    }

    fn hashOptionalType(self: *Program, hasher: *base.TypeDigestHasher, ty: ?TypeId) Allocator.Error!void {
        if (ty) |present| {
            hasher.update(&[_]u8{1});
            hasher.update(&try self.typeContent(present));
        } else hasher.update(&[_]u8{0});
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

    /// The immediate types whose descriptors compose `ty`'s. This is the
    /// same graph for caller preparation and the body's composition walk.
    pub fn appendDescriptorComponents(self: *Program, ty: TypeId, out: *std.ArrayList(TypeId), allocator: Allocator) Allocator.Error!void {
        const store = &self.store;
        switch (store.get(ty)) {
            .list, .box => |elem| try out.append(allocator, elem),
            .tuple => |items| try out.appendSlice(allocator, store.span(items)),
            .record => |record| {
                if (record.ext.get()) |ext| try out.append(allocator, ext);
                const storage = try self.layouts.recordStorage(record);
                defer self.allocator.free(storage);
                try out.appendSlice(allocator, storage);
            },
            .tag_union => |tag_union| {
                if (tag_union.ext) |ext| try out.append(allocator, ext);
                for (store.tagSpan(tag_union.tags)) |tag| try out.appendSlice(allocator, store.span(tag.payloads));
            },
            .nominal => try out.append(allocator, try self.converter().backing(ty)),
            .primitive, .empty_record, .function, .variable => {},
        }
    }

    /// Declare the signature of imported template worker `id`, exactly as
    /// its defining unit's body lowering declares it: explicit parameters,
    /// then its type table when it has inputs, and a dynamic result slot
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
        if (self.inputCount(id) != 0) try args.append(self.allocator, try store.addLocal(.{ .layout_idx = .opaque_ptr }));
        const proc = store.getProcSpecPtr(imported.proc);
        proc.args = try store.addLocalSpan(args.items);
        if (try self.converter().representationClosed(imported.ret)) {
            proc.ret_layout = try self.layouts.layoutOf(imported.ret);
        } else {
            proc.ret_layout = dynamic_layout;
            const desc = try store.addLocal(.{ .layout_idx = .opaque_ptr });
            proc.dyn_result = .of(try store.addLocal(.{ .layout_idx = dynamic_layout, .dyn = .{ .desc = desc, .storage = .result } }));
        }
    }

    pub fn module(self: *const Program, index: ModuleIndex) *const types.ModuleView {
        return self.modules.get(index);
    }

    /// The checked graph and its module indices remain fixed during lowering.
    /// Allocate the exact reachability memo only if a consumer asks for it.
    pub fn hasError(self: *Program, module_index: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error!bool {
        const errors = self.error_types orelse blk: {
            const created = try types.ErrorTypes.create(self.allocator, &self.modules);
            self.error_types = created;
            break :blk created;
        };
        return try errors.hasError(module_index, root);
    }

    fn releaseErrorTypes(self: *Program) void {
        if (self.error_types) |errors| errors.destroy();
        self.error_types = null;
    }

    fn finishErrorTypes(self: *Program) void {
        if (base.dev_diagnostics.isSet(.trace_boxy_work)) {
            const stats = if (self.error_types) |errors| errors.stats() else types.ErrorTypes.Stats{};
            std.debug.print("boxy-work: hasError queries={d} unique_expanded_nodes={d} retained_array_capacity_bytes={d} index_capacity={d}\n", .{
                stats.queries, stats.unique_expanded_nodes, stats.retained_array_capacity_bytes, stats.index_capacity,
            });
        }
        self.releaseErrorTypes();
    }

    /// Free everything but the program's `result`, which lowering hands on.
    pub fn release(self: *Program) void {
        self.releaseErrorTypes();
        self.static_descs.deinit();
        self.layouts.deinit();
        self.store.deinit();
        self.modules.deinit();
        self.workers.deinit(self.allocator);
        self.worker_ids.deinit(self.allocator);
        self.worklist.deinit(self.allocator);
        self.type_contents.deinit(self.allocator);
        if (self.list_methods) |*methods| methods.deinit(self.allocator);
        self.list_item_slots.deinit(self.allocator);
        self.by_module_idx.deinit(self.allocator);
        self.exported_bindings.deinit(self.allocator);
        self.callable_eval_by_pattern.deinit(self.allocator);
        self.platform_identities.deinit(self.allocator);
        self.root_value_slots.deinit(self.allocator);
        self.stored_value_slots.deinit(self.allocator);
        self.requested_roots.deinit(self.allocator);
        self.const_schema.deinit(self.allocator);
        self.wrapper_by_root.deinit(self.allocator);
        self.hosted.deinit(self.allocator);
        self.allocator.free(self.body_indexed);
        self.local_scopes.deinit(self.allocator);
        self.local_procs.deinit(self.allocator);
        self.local_proc_bindings.deinit(self.allocator);
        self.local_method_bindings.deinit(self.allocator);
        self.sites_by_expr.deinit(self.allocator);
        self.nominal_overrides.deinit(self.allocator);
        self.composed_in_body.deinit(self.allocator);
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

    /// The callable-eval binding a method bound to a computed callable names.
    pub fn callableEvalBinding(self: *const Program, binding: static_dispatch.CallableBindingTarget) CallableEvalBinding {
        return .{ .module = self.moduleByKey(binding.module).index, .template = binding.template };
    }

    /// The erased entry of the method bound to the computed callable
    /// `binding` (`WorkerKey.callable_eval_entry`), emitted with module
    /// `module_index`'s workers. The binding is monomorphic, so the entry
    /// takes exactly its function type's parameters.
    pub fn callableEvalEntryWorker(self: *Program, module_index: ModuleIndex, binding: CallableEvalBinding) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .callable_eval_entry = binding };
        if (self.worker_ids.get(key)) |existing| return existing;
        const template = self.module(binding.module).view.callable_eval_templates.templates[@backingInt(binding.template)];
        const fn_ty = try self.converter().convert(binding.module, template.checked_fn_root, .{ .module = binding.module, .vars = &.{}, .bound = &.{} });
        const function = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a method bound to a computed callable has no function type"),
        };
        const params = try self.arena.allocator().dupe(TypeId, self.store.span(function.args));
        return try self.addErasedWorker(key, module_index, &.{}, params, function.ret, "boxy.callable_eval_entry");
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
        const template = view.view.checked_procedure_templates.templates.items()[@backingInt(template_id)];
        const checked_scheme_vars = poolSlice(checked.CheckedTypeId, view.view.checked_procedure_templates.scheme_vars_pool, template.scheme_vars);
        // A sealed variable (design.md "Sealed Result Rows"), which checking
        // records (`CheckedProcedureTemplate.sealed_slots`), is no hidden
        // input: it seals to the empty row wherever the body mentions it, and
        // an evidence parameter dispatching on it is no dictionary.
        var scheme_vars: std.ArrayList(checked.CheckedTypeId) = .empty;
        var checked_slots: std.ArrayList(u32) = .empty;
        const templates = view.view.checked_procedure_templates;
        const evidence_params = templates.templateEvidenceParams(&template);
        const sealed_slots = templates.templateSealedSlots(&template);
        for (checked_scheme_vars, 0..) |variable, slot| {
            // A platform requirement's identity variable is the app type
            // this program lowers the platform at (`solvePlatformIdentities`):
            // every mention converts to that type, so it is no hidden
            // input either, and no sealed one.
            if (self.converter().platformIdentity(module_index, variable) != null) continue;
            if (std.mem.findScalar(u32, sealed_slots, @intCast(slot)) != null) continue;
            try scheme_vars.append(self.arena.allocator(), variable);
            try checked_slots.append(self.arena.allocator(), @intCast(slot));
        }
        const literal_requirements = templates.templateLiteralRequirements(&template);
        var dictionary_params: std.ArrayList(u32) = .empty;
        for (evidence_params, 0..) |param, index| {
            const sealed_dispatcher = if (param.slot.get()) |slot|
                std.mem.findScalar(u32, sealed_slots, slot) != null and self.converter().platformIdentity(module_index, checked_scheme_vars[slot]) == null
            else
                false;
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
        const kind_roots = try self.allocator.alloc(TypeId, 1 + 2 * evidence_params.len);
        defer self.allocator.free(kind_roots);
        kind_roots[0] = fn_ty;
        for (evidence_params, 0..) |param, index| {
            kind_roots[1 + 2 * index] = try self.converter().convert(module_index, param.callable_ty, env);
            kind_roots[2 + 2 * index] = try self.converter().convert(module_index, param.dispatcher_ty, env);
        }
        const kinds = try self.schemeKinds(kind_roots, scheme_vars.items.len);

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
            .kinds = kinds,
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

    /// The integer list worker for `item` of a `List` template (design.md
    /// "Pre-Specialized Integer List Workers"), queued for lowering, or bound
    /// to the interface its own object solved when another module's object
    /// defines it.
    pub fn listIntTemplateWorker(self: *Program, module_index: ModuleIndex, template_id: checked_names.CheckedProcedureTemplateId, item: ListIntItem) Allocator.Error!WorkerId {
        const id = try self.listIntTemplateScope(module_index, template_id, item);
        if (!self.worker(id).queued) {
            self.worker(id).queued = true;
            if (self.worker(id).imported) {
                self.bindImportedInterface(id);
            } else {
                try self.worklist.append(self.allocator, id);
            }
        }
        return id;
    }

    /// The integer list worker for `item` of a `List` template: the generic
    /// worker's scope, inputs, and dictionaries, with the list item variable
    /// bound to `item` in its signature and body.
    fn listIntTemplateScope(self: *Program, module_index: ModuleIndex, template_id: checked_names.CheckedProcedureTemplateId, item: ListIntItem) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .template = .{ .module = module_index, .template = template_id, .binding = .{ .list_int = item } } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const generic_id = try self.templateScope(module_index, template_id);
        const slot = (try self.listItemSlot(generic_id)) orelse invariant("an integer list worker was requested for a template with no list item variable");
        const generic = self.worker(generic_id).*;
        const view = self.module(module_index);
        const template = view.view.checked_procedure_templates.templates.items()[@backingInt(template_id)];
        const bound = try self.arena.allocator().alloc(TypeId, generic.scheme_vars.len);
        for (bound, 0..) |*entry, index| entry.* = try self.store.variable(@intCast(index));
        bound[slot] = try self.store.primitive(item.primitive());
        const env: types.Env = .{ .module = module_index, .vars = generic.scheme_vars, .bound = bound };
        const fn_ty = try self.converter().convert(module_index, template.checked_fn_root, env);
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a procedure template's checked root is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const identity = self.workerIdentity(key);
        const proc = if (generic.imported) try self.addImportedProc(identity, module_index, .roc) else try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = identity,
            .emission_unit = LIR.EmissionUnit.module(@backingInt(module_index)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        var int_worker = generic;
        int_worker.key = key;
        int_worker.proc = proc;
        int_worker.params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args));
        int_worker.ret = signature.ret;
        int_worker.list_int = .{ .slot = slot, .item = item };
        int_worker.queued = false;
        int_worker.lowered = generic.imported;
        int_worker.needs = null;
        try self.workers.append(self.allocator, int_worker);
        try self.worker_ids.put(self.allocator, key, id);
        if (generic.imported) try self.declareImportedTemplateSignature(id);
        return id;
    }

    /// The position among the scheme variables of generic template worker
    /// `id` of its list item variable, or null when the template has no
    /// integer list workers (design.md "Pre-Specialized Integer List
    /// Workers"):
    /// the template must be a method the Builtin module registers for
    /// `List`, have a checked body (a provided low-level operation is
    /// emitted at every direct call instead), and take as its first
    /// parameter `List(item)` for a scheme variable `item` that is not
    /// sealed.
    pub fn listItemSlot(self: *Program, id: WorkerId) Allocator.Error!?u32 {
        const target = self.worker(id).*;
        const key = switch (target.key) {
            .template => |key| key,
            .lambda, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => return null,
        };
        const template_key: TemplateKey = .{ .module = key.module, .template = key.template };
        if (self.list_item_slots.get(template_key)) |known| return known;
        const slot = try self.computeListItemSlot(template_key, target);
        try self.list_item_slots.put(self.allocator, template_key, slot);
        return slot;
    }

    fn computeListItemSlot(self: *Program, key: TemplateKey, target: Worker) Allocator.Error!?u32 {
        const view = self.module(key.module);
        if (view.view.module_env.module_role != .builtin) return null;
        const template = view.view.checked_procedure_templates.templates.items()[@backingInt(key.template)];
        if (template.provided_low_level != null) return null;
        switch (template.body) {
            .checked_body => {},
            .intrinsic_wrapper, .unimplemented, .entry_wrapper => return null,
        }
        if (!try self.isListMethod(key)) return null;
        if (target.params.len == 0) return null;
        const item = switch (self.store.get(target.params[0])) {
            .list => |item| item,
            .primitive, .empty_record, .record, .tuple, .tag_union, .box, .nominal, .function, .variable => return null,
        };
        return switch (self.store.get(item)) {
            .variable => |index| index,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => null,
        };
    }

    /// Whether the Builtin module's method registry names template `key`
    /// as a method of `List`.
    fn isListMethod(self: *Program, key: TemplateKey) Allocator.Error!bool {
        if (self.list_methods == null) {
            var methods: std.AutoHashMapUnmanaged(TemplateKey, void) = .empty;
            errdefer methods.deinit(self.allocator);
            const builtin = self.builtinModule();
            for (builtin.view.method_registry.entries) |entry| {
                switch (entry.key.owner) {
                    .builtin => |owner| if (owner != .list) continue,
                    .nominal => continue,
                }
                const method = entry.target orelse continue;
                const ref = switch (method.kind) {
                    .procedure => |procedure| procedure.template,
                    .local_proc, .structural, .callable_binding => continue,
                };
                const owner_view = self.moduleByKey(.{ .bytes = ref.artifact.bytes });
                try methods.put(self.allocator, .{ .module = owner_view.index, .template = ref.template }, {});
            }
            self.list_methods = methods;
        }
        return self.list_methods.?.contains(key);
    }

    /// The worker a direct call of template worker `callee` at the
    /// instantiation `bound` (one type per scheme variable) targets: the
    /// template's integer list worker for the type its list item variable
    /// takes when that is exactly one of the `ListIntItem` types, and
    /// `callee` itself otherwise (design.md "Pre-Specialized Integer List
    /// Workers").
    pub fn directCallTarget(self: *Program, callee: WorkerId, bound: []const TypeId) Allocator.Error!WorkerId {
        const target = self.worker(callee).*;
        const key = switch (target.key) {
            .template => |key| key,
            .lambda, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => return callee,
        };
        switch (key.binding) {
            .generic => {},
            .list_int => return callee,
        }
        const slot = (try self.listItemSlot(callee)) orelse return callee;
        if (bound.len != target.scheme_vars.len) invariant("a call's instantiation differs from its target's scheme");
        const item = switch (self.store.get(bound[slot])) {
            .primitive => |primitive| ListIntItem.ofPrimitive(primitive) orelse return callee,
            .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => return callee,
        };
        return try self.listIntTemplateWorker(key.module, key.template, item);
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
        lambda_expr: ExprId,
        captures: []const checked.CheckedCapture,
        capture_types: []const TypeId,
        fn_ty: TypeId,
        own: ?LocalScheme,
    ) Allocator.Error!WorkerId {
        const outer_worker = self.worker(outer).*;
        // A lambda that checking gave a dispatch scope is a link of every
        // evidence and literal chain that reaches through it, even when the
        // scope quantifies nothing: an empty scope takes an empty entry, so
        // a chain depth names the same scope here as in checking.
        const empty_scope: ?LocalScheme = if (own == null and try self.localScope(outer_worker.module, lambda_expr) != null)
            .{ .scheme_vars = &.{}, .dictionaries = &.{}, .per_call = false }
        else
            null;
        const scope = own orelse empty_scope;
        const key: WorkerKey = .{ .lambda = .{ .module = outer_worker.module, .expr = lambda_expr, .outer = outer, .captured_scheme = if (own) |scheme| scheme.per_call == false and (try self.isLocalProc(outer_worker.module, lambda_expr)) else false } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const scheme_vars = if (own) |scheme| try std.mem.concat(self.arena.allocator(), checked.CheckedTypeId, &.{ outer_worker.scheme_vars, scheme.scheme_vars }) else outer_worker.scheme_vars;
        // A lambda of an integer list worker's scope binds the item variable
        // to that worker's integer type, in its signature and captures as in
        // its body.
        var lambda_ty = fn_ty;
        const stored_capture_types = try self.arena.allocator().dupe(TypeId, capture_types);
        if (outer_worker.list_int) |list_int| {
            const bound = try self.allocator.alloc(TypeId, scheme_vars.len);
            defer self.allocator.free(bound);
            for (bound, 0..) |*entry, index| entry.* = try self.store.variable(@intCast(index));
            bound[list_int.slot] = try self.store.primitive(list_int.item.primitive());
            lambda_ty = try self.substitute(self.allocator, fn_ty, bound);
            for (stored_capture_types) |*capture_ty| capture_ty.* = try self.substitute(self.allocator, capture_ty.*, bound);
        }
        const signature = switch (self.store.get(lambda_ty)) {
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
            .scheme_vars = scheme_vars,
            .list_int = outer_worker.list_int,
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .captures = try self.arena.allocator().dupe(checked.CheckedCapture, captures),
            .capture_types = stored_capture_types,
            .dict_scopes = if (scope) |scheme| try std.mem.concat(self.arena.allocator(), []const u32, &.{ &.{scheme.dictionaries}, outer_worker.dict_scopes }) else outer_worker.dict_scopes,
            .dict_scope_params = if (scope) |scheme| try std.mem.concat(self.arena.allocator(), []const static_dispatch.EvidenceParamRecord, &.{ &.{scheme.params}, outer_worker.dict_scope_params }) else outer_worker.dict_scope_params,
            .own_descs = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.scheme_vars.len) else 0) else 0,
            .own_dicts = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.dictionaries.len) else 0) else 0,
            .literal_scopes = if (scope) |scheme| try std.mem.concat(self.arena.allocator(), []const checked.LiteralRequirement, &.{ &.{scheme.literal_requirements}, outer_worker.literal_scopes }) else outer_worker.literal_scopes,
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
    pub fn relateVariablesPartial(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize) Allocator.Error![]Related {
        return try self.relateVariablesRows(allocator, roots, count, null);
    }

    /// `relateVariablesPartial`, also marking in `row_vars` each variable
    /// related as an open tag row's remainder: the actual row's tags the open
    /// row does not name (design.md "Row Joins", `.boxy`).
    pub fn relateVariablesRows(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize, row_vars: ?[]bool) Allocator.Error![]Related {
        const store = &self.store;
        const bound = try allocator.alloc(Related, count);
        defer allocator.free(bound);
        @memset(bound, .none);
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(allocator);
        try pending.appendSlice(allocator, roots);
        while (pending.pop()) |pair| {
            const formal = store.get(pair[0]);
            if (formal == .variable) {
                bound[formal.variable] = .some(pair[1]);
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
                                // A kind variable takes the field's storage
                                // on the other side.
                                if (field.kind == .variable_slot) {
                                    bound[field.slot_variable] = .some(try self.layouts.fieldStorage(candidate));
                                    if (row_vars) |marks| {
                                        if (field.slot_variable < marks.len) marks[field.slot_variable] = true;
                                    }
                                }
                            }
                        }
                        if (!named) try residual.append(allocator, candidate);
                    }
                    if (record.ext.get()) |ext| {
                        const remainder = if (residual.items.len == 0 and other.record.ext == .none)
                            try store.emptyRecord()
                        else
                            try store.intern(.{ .record = .{ .fields = residual.items, .ext = other.record.ext.get() } });
                        try pending.append(allocator, .{ ext, remainder });
                    }
                    if (record.declared.get() != null and other.record.declared.get() != null) {
                        for (store.paddingTypes(record), store.paddingTypes(other.record)) |a, b| try pending.append(allocator, .{ a, b });
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
        return try allocator.dupe(Related, bound);
    }

    /// The kind variables among `count` scheme variables that the types
    /// `roots` over those variables mention (design.md "Field Kinds"). A
    /// kind variable's scheme relates it to one field, so to one value type.
    pub fn schemeKinds(self: *Program, roots: []const TypeId, count: usize) Allocator.Error!SchemeKinds {
        const arena = self.arena.allocator();
        var payloads = try collections.OptionalIndexColumn(TypeId).initFilled(arena, count);
        var found: std.ArrayList(u32) = .empty;
        defer found.deinit(self.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        try pending.appendSlice(self.allocator, roots);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (self.store.get(ty)) {
                .record => |record| {
                    for (self.store.fieldSpan(record.fields)) |field| {
                        if (field.kind == .variable_slot and !payloads.isPresent(field.slot_variable)) {
                            payloads.set(field.slot_variable, field.ty);
                            try found.append(self.allocator, field.slot_variable);
                        }
                        try pending.append(self.allocator, field.ty);
                    }
                    if (record.ext.get()) |ext| try pending.append(self.allocator, ext);
                    try pending.appendSlice(self.allocator, self.store.paddingTypes(record));
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
        }
        // A kind whose value type mentions another comes after it.
        const order = try arena.alloc(u32, found.items.len);
        var placed: usize = 0;
        const done = try self.allocator.alloc(bool, count);
        defer self.allocator.free(done);
        @memset(done, false);
        while (placed < order.len) {
            const before = placed;
            for (found.items) |index| {
                if (done[index]) continue;
                const nested = try self.kindsMentioned(payloads.get(index).?, payloads.len());
                defer self.allocator.free(nested);
                const ready = for (nested) |other| {
                    if (!done[other]) break false;
                } else true;
                if (!ready) continue;
                done[index] = true;
                order[placed] = index;
                placed += 1;
            }
            if (placed == before) invariant("a field kind's value type mentions the kind itself, as no finite type does");
        }
        return .{ .payloads = payloads.view(), .order = order };
    }

    /// The kind variables `root` mentions.
    fn kindsMentioned(self: *Program, root: TypeId, count: usize) Allocator.Error![]u32 {
        if (self.store.isClosed(root)) return try self.allocator.alloc(u32, 0);
        const nested = try self.schemeKinds(&.{root}, count);
        return try self.allocator.dupe(u32, nested.order);
    }

    /// Turn each kind variable's binding in `bound` into the field's storage:
    /// the value type for a required kind, or for a kind nothing committed,
    /// which every strategy commits to `required`; the presence slot over it
    /// for an optional kind. A binding that already is a storage, or a
    /// variable of the enclosing scope, stays.
    pub fn resolveKindSlots(self: *Program, kinds: SchemeKinds, bound: []TypeId) Allocator.Error!void {
        for (kinds.order) |index| {
            const payload = kinds.payloads.get(index).?;
            if (self.store.committedKindOf(bound[index])) |kind| {
                const value = try self.substitute(self.allocator, payload, bound);
                bound[index] = switch (kind) {
                    .required => value,
                    .optional => try self.layouts.presenceSlot(value),
                };
            } else if (bound[index] == try self.store.emptyTagUnion()) {
                bound[index] = try self.substitute(self.allocator, payload, bound);
            }
        }
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
                    for (self.store.fieldSpan(record.fields)) |field| {
                        if (field.kind == .variable_slot) try found.append(allocator, field.slot_variable);
                        try pending.append(allocator, field.ty);
                    }
                    if (record.ext) |ext| try pending.append(allocator, ext);
                    try pending.appendSlice(allocator, self.store.paddingTypes(record));
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

    /// Sorts `fields` by label, given that `fields[0..own]` and
    /// `fields[own..]` are each sorted already.
    fn mergeSortedFields(allocator: Allocator, store: *const types.Store, fields: []types.Field, own: usize) Allocator.Error!void {
        if (own == 0 or own == fields.len) return;
        if (store.nameLessThan(fields[own - 1].name, fields[own].name)) return;
        const first = try allocator.dupe(types.Field, fields[0..own]);
        defer allocator.free(first);
        var left: usize = 0;
        var right: usize = own;
        for (fields) |*out| {
            if (right == fields.len or (left < first.len and store.nameLessThan(first[left].name, fields[right].name))) {
                out.* = first[left];
                left += 1;
            } else {
                out.* = fields[right];
                right += 1;
            }
        }
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
                        if (record.ext.get()) |ext| try stack.append(allocator, .{ .ty = ext, .expanded = false });
                        for (self.store.paddingTypes(record)) |padding| try stack.append(allocator, .{ .ty = padding, .expanded = false });
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
                    for (self.store.fieldSpan(record.fields)) |field| {
                        const ty = done.get(field.ty).?;
                        if (field.kind == .variable_slot) {
                            if (field.slot_variable >= bound.len) invariant("a substituted field kind names a variable past its bindings");
                            try fields.append(allocator, self.store.instantiatedField(field.name, ty, bound[field.slot_variable]));
                        } else {
                            var substituted = field;
                            substituted.ty = ty;
                            try fields.append(allocator, substituted);
                        }
                    }
                    var ext: ?TypeId = if (record.ext.get()) |ext| done.get(ext).? else null;
                    // The row's own fields stay sorted by label under
                    // substitution, and so do the remainder's, so a
                    // remainder that substituted to a record merges in.
                    if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                        .empty_record => ext = null,
                        .record => |ext_record| {
                            const own = fields.items.len;
                            try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                            try mergeSortedFields(allocator, &self.store, fields.items, own);
                            ext = ext_record.ext.get();
                        },
                        .variable => {},
                        .primitive, .tuple, .tag_union, .list, .box, .nominal, .function => invariant("a record row's remainder substituted to a non-record type"),
                    };
                    // A padded record's store derives its slots from its
                    // entries, so the fields a remainder brings get slots
                    // just as they would had the row been closed already.
                    if (record.declared.get()) |declared| {
                        const padding = try allocator.alloc(TypeId, declared.padding.len);
                        defer allocator.free(padding);
                        for (self.store.span(declared.padding), padding) |ty, *slot| slot.* = done.get(ty).?;
                        break :blk try self.store.intern(.{ .record = .{
                            .fields = fields.items,
                            .ext = ext,
                            .declared = .{ .padding = padding, .entries = self.store.declaredEntries(declared) },
                        } });
                    }
                    if (fields.items.len == 0 and ext == null) break :blk try self.store.emptyRecord();
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
                            .local_proc, .structural, .callable_binding => invariant("a SIMD vector's to_inspect is a Builtin procedure"),
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
                    // The entry restores the value when called, so the
                    // descriptor holds the entry: composing a descriptor
                    // never evaluates the binding's root.
                    .callable_binding => |binding| {
                        const entry = try self.callableEvalEntryWorker(nominal.decl.module, self.callableEvalBinding(binding));
                        slot.* = .{ .procedure = .{ .entry = entry, .target = entry, .bound = &.{}, .dicts = &.{} } };
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
                    if (record.ext.get()) |ext| try pending.append(self.allocator, ext);
                    try pending.appendSlice(self.allocator, store.paddingTypes(record));
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
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("a worker with dictionary parameters is not a template's"),
        };
        const templates = self.module(key.module).view.checked_procedure_templates;
        const params = poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items()[@backingInt(key.template)].evidence_params);
        const env: types.Env = .{ .module = key.module, .vars = target_worker.scheme_vars, .bound = bound };
        for (target_worker.dictionary_params, requirements) |param_index, *requirement| {
            const param = params[param_index];
            requirement.* = .{
                .module = key.module,
                .method = param.method,
                .structural = param.structural.get(),
                .dispatcher = try self.converter().convert(key.module, param.dispatcher_ty, env),
                .callable = try self.converter().convert(key.module, param.callable_ty, env),
            };
        }
        return .{ .bound = bound, .requirements = requirements };
    }

    /// The requirements of the dictionary `root`, each method target's own
    /// requirements resolved the same way, as a tree in pre-order. A target
    /// instance appears once on a path: a requirement that resolves to one
    /// already on its path is a back edge. Checking keeps the instances a
    /// chain of requirements reaches finite, so the tree is too.
    pub fn dictionaryTree(self: *Program, root: DictionaryRoot) Allocator.Error![]const DictionaryNode {
        var walk: DictionaryWalk = .{ .program = self };
        defer walk.frames.deinit(self.allocator);
        switch (root) {
            .requirement => |requirement| try walk.visit(requirement),
            .target => |target| try walk.enter(null, target.target, target.instance),
        }
        while (walk.frames.items.len > 0) {
            const frame = &walk.frames.items[walk.frames.items.len - 1];
            const requirements = walk.nodes.items[frame.node].kind.target.instance.requirements;
            if (frame.next < requirements.len) {
                const requirement = requirements[frame.next];
                frame.next += 1;
                try walk.visit(requirement);
                continue;
            }
            const done = walk.frames.pop().?;
            const entry = &walk.nodes.items[done.node];
            entry.kind.target.children = done.children.items;
            entry.reaches = done.reaches;
            if (walk.frames.items.len > 0) walk.reach(done.reaches);
        }
        return walk.nodes.items;
    }

    /// Whether the dictionary of `target` at the closed types `bound` holds a
    /// dictionary that captures itself.
    pub fn dictionaryCapturesItself(self: *Program, target: WorkerId, bound: []const TypeId) Allocator.Error!bool {
        const instance = try self.targetRequirements(target, bound);
        for (try self.dictionaryTree(.{ .target = .{ .target = target, .instance = instance } })) |node| switch (node.kind) {
            .target => |entry| if (entry.recursive) return true,
            .resolved, .back_edge => {},
        };
        return false;
    }

    const DictionaryWalk = struct {
        program: *Program,
        nodes: std.ArrayList(DictionaryNode) = .empty,
        frames: std.ArrayList(Frame) = .empty,

        const Frame = struct {
            node: u32,
            next: usize = 0,
            reaches: u32 = DictionaryNode.no_reach,
            children: std.ArrayList(u32) = .empty,
        };

        fn arena(self: *DictionaryWalk) Allocator {
            return self.program.arena.allocator();
        }

        /// Add `node`, a child of the target on top of the path.
        fn add(self: *DictionaryWalk, node: DictionaryNode) Allocator.Error!u32 {
            const index: u32 = @intCast(self.nodes.items.len);
            try self.nodes.append(self.arena(), node);
            if (self.frames.items.len > 0) try self.frames.items[self.frames.items.len - 1].children.append(self.arena(), index);
            return index;
        }

        fn reach(self: *DictionaryWalk, depth: u32) void {
            const top = &self.frames.items[self.frames.items.len - 1];
            top.reaches = @min(top.reaches, depth);
        }

        fn visit(self: *DictionaryWalk, requirement: DictRequirement) Allocator.Error!void {
            const resolution = try self.program.resolveRequirement(requirement);
            switch (resolution) {
                .target => |target| try self.enter(requirement, target, try self.program.targetAt(target, requirement.callable)),
                .structural, .crash, .scope_variable, .derived, .callable_eval => _ = try self.add(.{ .requirement = requirement, .kind = .{ .resolved = resolution } }),
            }
        }

        fn enter(self: *DictionaryWalk, requirement: ?DictRequirement, target: WorkerId, instance: TargetInstance) Allocator.Error!void {
            for (self.frames.items) |frame| {
                const on_path = &self.nodes.items[frame.node].kind.target;
                if (on_path.target != target or !std.mem.eql(TypeId, on_path.instance.bound, instance.bound)) continue;
                on_path.recursive = true;
                const depth = on_path.depth;
                _ = try self.add(.{ .requirement = requirement, .kind = .{ .back_edge = frame.node }, .reaches = depth });
                return self.reach(depth);
            }
            const depth: u32 = @intCast(self.frames.items.len);
            const node = try self.add(.{ .requirement = requirement, .kind = .{ .target = .{
                .target = target,
                .instance = instance,
                .children = &.{},
                .depth = depth,
            } } });
            try self.frames.append(self.program.allocator, .{ .node = node });
        }
    };

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
        try self.relateThroughTargetRequirements(target, related);
        const bound = try self.arena.allocator().alloc(TypeId, count);
        for (related, bound, target_worker.scheme_vars) |ty, *slot, variable| slot.* = ty.get() orelse try self.converter().sealedScheme(target_worker.module, variable);
        try self.resolveKindSlots(target_worker.kinds, bound);
        return try self.targetRequirements(target, bound);
    }

    /// Fill the scheme variables of `target` that `related` leaves unrelated
    /// from the requirements of its own evidence parameters.
    pub fn relateThroughTargetRequirements(self: *Program, target: WorkerId, related: []Related) Allocator.Error!void {
        const target_worker = self.worker(target).*;
        if (target_worker.dictionary_params.len == 0) return;
        const key = switch (target_worker.key) {
            .template => |key| key,
            .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => invariant("a worker with dictionary parameters is not a template's"),
        };
        const templates = self.module(key.module).view.checked_procedure_templates;
        const all_params = poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items()[@backingInt(key.template)].evidence_params);
        const params = try self.allocator.alloc(static_dispatch.EvidenceParamRecord, target_worker.dictionary_params.len);
        defer self.allocator.free(params);
        for (target_worker.dictionary_params, params) |param_index, *param| param.* = all_params[param_index];
        try self.relateThroughRequirements(key.module, target_worker.scheme_vars, params, related);
    }

    /// Fill the variables of `vars` (of `module`) that `related` leaves
    /// unrelated from the requirements `params` (evidence parameters over
    /// `vars`): a requirement whose dispatcher is determined resolves to a
    /// method whose own type, related back to the requirement's callable,
    /// determines the variables that callable names, until no requirement
    /// determines more.
    pub fn relateThroughRequirements(self: *Program, module_index: ModuleIndex, vars: []const checked.CheckedTypeId, params: []const static_dispatch.EvidenceParamRecord, related: []Related) Allocator.Error!void {
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
            for (related, partial, 0..) |ty, *slot, index| slot.* = ty.get() orelse try self.store.variable(placeholder_base + @as(u32, @intCast(index)));
            const env: types.Env = .{ .module = module_index, .vars = vars, .bound = partial };
            for (params) |param| {
                const dispatcher = try self.converter().convert(module_index, param.dispatcher_ty, env);
                if (try self.mentionsPlaceholder(dispatcher)) continue;
                const callable = try self.converter().convert(module_index, param.callable_ty, env);
                const method_target = switch (try self.resolveRequirement(.{ .module = module_index, .method = param.method, .structural = param.structural.get(), .dispatcher = dispatcher, .callable = callable })) {
                    .target => |method_target| method_target,
                    .structural, .crash, .scope_variable, .derived, .callable_eval => continue,
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
                for (method_related, method_bound, 0..) |ty, *slot, index| slot.* = ty.get() orelse try self.store.variable(placeholder_base + @as(u32, @intCast(count + index)));
                const method_ty = try self.substitute(self.allocator, method_formal, method_bound);
                const back = try self.relateVariablesPartial(self.allocator, &.{.{ try self.converter().convert(module_index, param.callable_ty, own_env), method_ty }}, count);
                defer self.allocator.free(back);
                for (related, back) |*slot, determined| {
                    if (slot.* != .none) continue;
                    const ty = determined.get() orelse continue;
                    if (try self.mentionsPlaceholder(ty)) continue;
                    slot.* = .some(ty);
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
                    if (record.ext.get()) |ext| try stack.append(self.allocator, ext);
                    try stack.appendSlice(self.allocator, self.store.paddingTypes(record));
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
                    .callable_binding => |binding| return .{ .callable_eval = try self.callableEvalEntryWorker(requirement.module, self.callableEvalBinding(binding)) },
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
            .identity = try self.generatedWorkerIdentity(key, outer_worker.module, @backingInt(id), &.{}),
            .emission_unit = .isolated,
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = outer_worker.module,
            .scheme_vars = if (self.store.isVariableFree(from) and self.store.isVariableFree(to)) &.{} else outer_worker.scheme_vars,
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
    pub fn siteCaptures(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error![]const checked.CheckedCapture {
        try self.indexBodyMetadata(module_index);
        const site_index = self.sites_by_expr.get(.{ .module = module_index, .expr = expr }) orelse
            invariant("a lambda or closure has no nested procedure site");
        return try self.runtimeCaptures(self.module(module_index), site_index);
    }

    /// The runtime captures of nested procedure site `site_index` of `view`.
    /// A scheme alias owns no value: each of its uses lowers the callable it
    /// names, whose own captures the site lists, so it takes no capture. A
    /// promoted method of a body-declared type, which another local
    /// procedure refers to, is a top-level procedure and takes none either.
    pub fn runtimeCaptures(self: *Program, view: *const types.ModuleView, site_index: anytype) Allocator.Error![]const checked.CheckedCapture {
        const table = view.view.nested_proc_sites;
        const span = table.sites[site_index].runtime_captures;
        const listed = table.runtime_captures[span.start..][0..span.len];
        const bodies = view.bodies();
        try bodies.demandIndex();
        var kept: std.ArrayList(checked.CheckedCapture) = .empty;
        for (listed) |capture| {
            const binder = bodies.binder(capture.pattern);
            if (binder.is_scheme_alias or binder.is_promoted) continue;
            try kept.append(self.arena.allocator(), capture);
        }
        return kept.items;
    }

    /// The worker evaluating default expression `expr` of `module_index`.
    pub fn defaultWorker(self: *Program, module_index: ModuleIndex, expr: ExprId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .default = .{ .module = module_index, .expr = expr } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const view = self.module(module_index);
        // The default's type is the field's type at a fresh instantiation of
        // its declaration's parameters (design.md "Defaulted Fields"): each
        // construction takes the types its field gives those variables.
        try view.bodies().demandIndex();
        const default_ty = view.bodies().exprType(expr);
        const scheme_vars = try self.converter().typeVariables(self.arena.allocator(), module_index, default_ty);
        const bound = try self.arena.allocator().alloc(TypeId, scheme_vars.len);
        for (bound, 0..) |*slot, index| slot.* = try self.store.variable(@intCast(index));
        const ret = try self.converter().convert(module_index, default_ty, .{ .module = module_index, .vars = scheme_vars, .bound = bound });
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
            .scheme_vars = scheme_vars,
            .dictionary_params = &.{},
            .params = &.{},
            .ret = ret,
            .imported = imported,
            .lowered = imported,
            .queued = true,
        });
        try self.worker_ids.put(self.allocator, key, id);
        if (imported) {
            // A default is called directly, with its variables' descriptors.
            try self.declareImportedTemplateSignature(id);
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
            .identity = self.programLocalIdentity("boxy.static_data", @backingInt(id)),
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
            .identity = self.programLocalIdentity("boxy.root", @backingInt(id)),
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
                .template, .adapter, .default, .method_entry, .structural, .structural_map, .root, .codec, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => null,
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
            .identity = try self.generatedWorkerIdentity(key, module_index, @backingInt(id), &.{}),
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
        const id = try self.addErasedWorkerWithIdentity(key, target_worker.module, target_worker.scheme_vars, target_worker.params, target_worker.ret, identity);
        // The entry runs in its target's scopes, whose descriptors,
        // dictionaries, and literal arguments it captures.
        const entry = self.worker(id);
        entry.dict_scopes = target_worker.dict_scopes;
        entry.dict_scope_params = target_worker.dict_scope_params;
        entry.literal_scopes = target_worker.literal_scopes;
        return id;
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
        const identity = switch (key) {
            // Only a compile-time program reads a root's slot.
            .root_slot => self.programLocalIdentity(role, @backingInt(id)),
            .adapter, .structural, .structural_map, .crash_dictionary, .literal_thunk, .codec, .convert, .callable_eval_entry => try self.generatedWorkerIdentity(key, module_index, @backingInt(id), &.{}),
            .template, .lambda, .default, .method_entry, .root, .static_data => invariant("a worker a module's object defines is no generated erased worker"),
        };
        return try self.addErasedWorkerWithIdentity(key, module_index, scheme_vars, params, ret, identity);
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
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items()[@backingInt(top.binding)].body };
            },
            .imported => |imported| blk: {
                const owner = self.moduleByKey(imported.artifact);
                const exported = self.exported_bindings.get(.{ .module = owner.index, .def = imported.def, .pattern = imported.pattern }) orelse
                    invariant("an imported procedure binding is not exported by its module");
                break :blk .{ .module = owner.index, .body = exported.body };
            },
            .platform_required => |required| blk: {
                const owner = self.moduleByKey(required.artifact);
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items()[@backingInt(required.procedure_binding)].body };
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
    pub fn readsRootSlot(self: *const Program, module_index: ModuleIndex, root: checked.ComptimeRootId) Allocator.Error!bool {
        if (!self.evaluatesRoot(module_index, root)) return false;
        const view = self.module(module_index);
        return !try types.Converter.rootHasIdentityVariables(view, view.view.compile_time_roots.root(root).checked_type);
    }

    /// The type compile-time root `root` of module `module_index`, which
    /// this program evaluates, produces its value at: the result of its
    /// request's checked type, as its root worker returns it (`rootWorker`).
    pub fn rootValueType(self: *Program, module_index: ModuleIndex, root: checked.ComptimeRootId) Allocator.Error!TypeId {
        const checked_type = self.requested_roots.get(.{ .module = module_index, .root = root }) orelse
            invariant("a root value type was asked of a root the program does not evaluate");
        const fn_ty = try self.converter().convert(module_index, checked_type, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        return switch (self.store.get(fn_ty)) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a compile-time root's checked type is not a function type"),
        };
    }

    /// The static data slot holding the value of compile-time root `root` of
    /// module `module_index` at its own type (`rootValueType`), once the
    /// evaluator completes it: a compile-time value slot with a failure
    /// record, which every read guards (`lir.ComptimeValueGuards`). The
    /// evaluator marks the slot failed before any root runs, so a read of a
    /// root not yet completed fails as the recursion evaluating the value in
    /// place would end in, and the evaluator reruns the reader once the root
    /// completes (design.md "Compile-Time Evaluation Strategy").
    pub fn rootValueSlot(self: *Program, module_index: ModuleIndex, root: checked.ComptimeRootId) Allocator.Error!LIR.StaticDataId {
        const key: RootSlotKey = .{ .module = module_index, .root = root };
        if (self.root_value_slots.get(key)) |slot| return slot;
        const ty = try self.rootValueType(module_index, root);
        const module_key = self.module(module_index).key;
        const failure_layout = try self.result.layouts.putStructFields(&.{
            .{ .index = 0, .layout = .u8 },
            .{ .index = 1, .layout = .str },
        });
        const struct_idx = self.result.layouts.getLayout(failure_layout).getStruct().idx;
        const failure_slot: LIR.StaticDataId = @fromBackingInt(@intCast(self.result.static_data_values.items.len));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = .none,
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
            .initializer = .none,
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

    /// Where two types of one value differ: nowhere; only in the tags of
    /// rows, one widening the other along the coercion's reach (design.md
    /// "Sealed Result Rows"); in the memory order of a record, one being the
    /// record a padded nominal lifts it into (design.md "Nominal Record Field
    /// Order"); or inside the function type the value has.
    pub const StorageDifference = enum { none, rows, record_order, function };

    /// A function value inside data is outside every row widening's reach
    /// (design.md "Row Coercion Primitive"), so both types of the value store
    /// such a function's signature alike.
    const nested_function_storage = "two types of one value store the signature of a function value inside data differently";

    pub fn storageDifference(self: *Program, from_root: TypeId, to_root: TypeId) Allocator.Error!StorageDifference {
        if (from_root == to_root) return .none;
        const store = &self.store;
        // Where a pair sits: in data, in the signature of the root function
        // or of a function its signature passes (each adapted by its own
        // adapter), in data such a signature passes, or in a function type
        // inside data.
        const Place = enum { data, root_function, signature_data, nested_function };
        const Pair = struct { from: TypeId, to: TypeId, place: Place };
        var pending: std.ArrayList(Pair) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .from = from_root, .to = to_root, .place = .data });
        var result: StorageDifference = .none;
        while (pending.pop()) |pair| {
            if (pair.from == pair.to) continue;
            const from = store.get(pair.from);
            const to = store.get(pair.to);
            if ((from == .variable) != (to == .variable)) {
                // A value whose type a variable of the body leaves open, flowing
                // into a tag row or out of one (a `?` return's error into the
                // function's composed row): the variable's tags are known
                // only at run time, so the value is re-tagged by name under
                // the destination's descriptor. Elsewhere a variable has the
                // one representation every type has.
                if (pair.place == .data and try self.isTagRow(if (from == .variable) pair.to else pair.from)) {
                    if (result == .none) result = .rows;
                }
                continue;
            }
            if (from == .variable) continue;
            if (std.meta.activeTag(from) != std.meta.activeTag(to)) {
                // A nominal shares its backing's representation.
                if (from == .nominal and from.nominal.has_backing) {
                    try pending.append(self.allocator, .{ .from = try self.converter().backing(pair.from), .to = pair.to, .place = pair.place });
                } else if (to == .nominal and to.nominal.has_backing) {
                    try pending.append(self.allocator, .{ .from = pair.from, .to = try self.converter().backing(pair.to), .place = pair.place });
                } else if (paddedAgainstEmpty(from, to)) {
                    switch (pair.place) {
                        .data => result = .record_order,
                        .root_function, .signature_data => return .function,
                        .nested_function => invariant(nested_function_storage),
                    }
                }
                continue;
            }
            // The place of a component of a non-function type.
            const inner: Place = if (pair.place == .root_function) .signature_data else pair.place;
            switch (from) {
                .record => |record| {
                    // A closed record widened with omitted optional fields
                    // needs their presence slots even when field order agrees.
                    if (!store.sameRecordOrder(record, to.record) or (record.ext == .none and to.record.ext == .none and record.fields.len != to.record.fields.len)) switch (pair.place) {
                        .data => result = .record_order,
                        .root_function, .signature_data => return .function,
                        .nested_function => invariant(nested_function_storage),
                    };
                    for (store.fieldSpan(to.record.fields)) |to_field| {
                        for (store.fieldSpan(record.fields)) |from_field| {
                            if (from_field.name != to_field.name) continue;
                            if (from_field.kind != to_field.kind or from_field.slot_variable != to_field.slot_variable) {
                                invariant("two types of one value store a record field differently, though a field has one storage at an instantiation");
                            }
                            try pending.append(self.allocator, .{ .from = from_field.ty, .to = to_field.ty, .place = inner });
                        }
                    }
                    if (record.ext != .none and to.record.ext != .none) try pending.append(self.allocator, .{ .from = record.ext.get().?, .to = to.record.ext.get().?, .place = inner });
                },
                .tuple => |items| for (store.span(items), store.span(to.tuple)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner }),
                .tag_union => |tag_union| {
                    // A row the destination widens holds its tags under
                    // other discriminants: the value is converted.
                    if (tag_union.tags.len != to.tag_union.tags.len or (tag_union.ext == null) != (to.tag_union.ext == null)) switch (pair.place) {
                        .data => if (result == .none) {
                            result = .rows;
                        },
                        .root_function, .signature_data => return .function,
                        .nested_function => invariant(nested_function_storage),
                    };
                    for (store.tagSpan(to.tag_union.tags)) |to_tag| {
                        for (store.tagSpan(tag_union.tags)) |from_tag| {
                            if (from_tag.name != to_tag.name or from_tag.payloads.len != to_tag.payloads.len) continue;
                            for (store.span(from_tag.payloads), store.span(to_tag.payloads)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner });
                        }
                    }
                    if (tag_union.ext != null and to.tag_union.ext != null) try pending.append(self.allocator, .{ .from = tag_union.ext.?, .to = to.tag_union.ext.?, .place = inner });
                },
                .list => |elem| try pending.append(self.allocator, .{ .from = elem, .to = to.list, .place = inner }),
                .box => |payload| try pending.append(self.allocator, .{ .from = payload, .to = to.box, .place = inner }),
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(to.nominal)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = inner }),
                .function => |function| {
                    const place: Place = switch (pair.place) {
                        .data => if (pair.from == from_root) .root_function else .nested_function,
                        .root_function => .root_function,
                        .signature_data, .nested_function => .nested_function,
                    };
                    for (store.span(function.args), store.span(to.function.args)) |a, b| try pending.append(self.allocator, .{ .from = a, .to = b, .place = place });
                    try pending.append(self.allocator, .{ .from = function.ret, .to = to.function.ret, .place = place });
                },
                .primitive, .empty_record, .variable => {},
            }
        }
        return result;
    }

    /// Whether one of two nodes is the backing of a padded nominal of no named
    /// fields and the other the empty record, which has no storage.
    pub fn paddedAgainstEmpty(a: types.Node, b: types.Node) bool {
        return a == .empty_record and b == .record and b.record.declared.get() != null or
            b == .empty_record and a == .record and a.record.declared.get() != null;
    }

    /// Whether `ty` is a tag union, through the backing of a nominal.
    fn isTagRow(self: *Program, ty: TypeId) Allocator.Error!bool {
        return switch (self.store.get(ty)) {
            .tag_union => true,
            .nominal => |nominal| nominal.has_backing and try self.isTagRow(try self.converter().backing(ty)),
            .primitive, .empty_record, .record, .tuple, .list, .box, .function, .variable => false,
        };
    }

    /// Lower every queued worker, and fill every reserved type table, each
    /// once its worker's needs are known; filling a table reserves the
    /// tables its words name, so the two alternate until neither has work.
    fn drain(self: *Program) Allocator.Error!void {
        while (true) {
            while (self.worklist.pop()) |id| {
                if (self.worker(id).lowered) continue;
                self.worker(id).lowered = true;
                try Body.lowerWorker(self, id);
            }
            const unfilled = self.static_descs.nextUnfilledTable() orelse return;
            try self.fillTable(unfilled);
        }
    }

    /// Fill the reserved table `unfilled`: its worker's inputs, then each
    /// slot's recipe evaluated at them.
    fn fillTable(self: *Program, unfilled: StaticDescs.UnfilledTable) Allocator.Error!void {
        // Tables are filled once the worklist is empty, so every worker
        // something called is lowered or imported.
        const target = self.worker(unfilled.worker);
        if (!target.lowered) invariant("a type table names a worker nothing lowers");
        // Another object's worker is filled from the needs its object
        // exports.
        if (target.imported and target.needs == null) {
            const unit = self.moduleUnit() orelse invariant("an imported worker outside a module unit");
            const interface = unit.imports.get(self.result.store.getProcSpec(target.proc).identity) orelse
                invariant("a type table names a worker of an imported module whose object exports no such worker");
            if (interface.table_needs.len == 0) invariant("an imported worker with inputs exports no needs");
            const decoded = try StoredFn.decodeNeeds(self, interface.table_needs);
            self.worker(unfilled.worker).needs = decoded;
        }
        const needs = self.worker(unfilled.worker).needs orelse invariant("a type table names a worker without needs");
        if (unfilled.inputs.len != needs.input_count) invariant("a type table's inputs differ from its worker's");
        const words = try self.allocator.alloc(StaticDescs.CaptureWord, needs.wordCount());
        defer self.allocator.free(words);
        @memcpy(words[0..needs.input_count], unfilled.inputs);
        var evaluation = Tables.Evaluation.init(self, .{ .needs = &needs }, unfilled.inputs, needs.desc_count);
        defer evaluation.deinit();
        for (words[needs.input_count..], needs.slots) |*slot_word, recipe| slot_word.* = try evaluation.word(recipe);
        try self.static_descs.fillTable(unfilled, words);
    }

    /// How many input words lead worker `id`'s tables: its variables'
    /// descriptors, its dictionaries, then its literal arguments, the hidden
    /// words its callers supply. A worker with inputs takes the address of
    /// its type table as its one hidden argument (`tables.zig`).
    pub fn inputCount(self: *Program, id: WorkerId) u32 {
        const target = self.worker(id).*;
        return switch (target.key) {
            .template => @intCast(target.scheme_vars.len + target.dictionary_params.len + target.literal_params),
            .lambda => blk: {
                var count: u32 = @intCast(target.scheme_vars.len);
                for (target.dict_scopes) |scope| count += @intCast(scope.len);
                for (target.literal_scopes) |scope| count += @intCast(scope.len);
                break :blk count;
            },
            .adapter, .codec, .default => @intCast(target.scheme_vars.len),
            .method_entry => |entry_target| self.inputCount(entry_target),
            .structural, .structural_map => target.structural_descs,
            .literal_thunk, .root_slot, .root, .static_data, .crash_dictionary, .convert, .callable_eval_entry => 0,
        };
    }

    /// Whether worker `id` is a generalized local procedure whose calls
    /// pass its own scheme's instantiation, and with it its type table, in
    /// its argument record; its function value captures no table.
    pub fn perCall(self: *Program, id: WorkerId) bool {
        const target = self.worker(id).*;
        return target.key == .lambda and target.own_descs + target.own_dicts + target.own_literals != 0;
    }

    /// How many of worker `id`'s inputs are its variables' descriptors.
    pub fn descCount(self: *Program, id: WorkerId) u32 {
        const target = self.worker(id).*;
        return switch (target.key) {
            .template, .lambda, .adapter, .codec, .default => @intCast(target.scheme_vars.len),
            .method_entry => |entry_target| self.descCount(entry_target),
            .structural, .structural_map => target.structural_descs,
            .literal_thunk, .root_slot, .root, .static_data, .crash_dictionary, .convert, .callable_eval_entry => 0,
        };
    }
};

/// Record the readable name of every worker of `program` under its
/// procedure's symbol, for the perf map (`base.dev_diagnostics.perf_map`).
fn recordPerfNames(program: *Program) Allocator.Error!void {
    var arena = std.heap.ArenaAllocator.init(program.allocator);
    defer arena.deinit();
    const names = try arena.allocator().alloc(?[]const u8, program.workers.items.len);
    @memset(names, null);
    var symbol: [LIR.ProcIdentity.symbol_name_len]u8 = undefined;
    for (program.workers.items, 0..) |worker, index| {
        const name = try perfNameOf(program, arena.allocator(), names, @fromBackingInt(@intCast(index)));
        const identity = program.result.store.getProcSpec(worker.proc).identity;
        base.dev_diagnostics.notePerfName(identity.writeSymbolName(&symbol), name);
    }
}

/// The readable name of worker `id`: a template's `<Module>.<def>`, and
/// every other worker named by what it is and, where it has one, the
/// worker it belongs to (`perfNameLabel`). `names` memoizes each worker's.
fn perfNameOf(program: *Program, a: Allocator, names: []?[]const u8, id: WorkerId) Allocator.Error![]const u8 {
    if (names[@backingInt(id)]) |known| return known;
    const worker = program.worker(id).*;
    const view = program.module(worker.module);
    const module_name = view.nameStore().moduleNameText(view.view.module_identity.module_name);
    const label: PerfNameLabel = switch (worker.key) {
        .template => |key| blk: {
            const template = view.view.checked_procedure_templates.templates.items()[@backingInt(key.template)];
            const name_store = view.nameStore();
            const suffix: []const u8 = switch (key.binding) {
                .generic => "",
                .list_int => |item| item.nameSuffix(),
            };
            break :blk if (name_store.procBase(template.proc_base).export_name.get()) |export_name|
                .{ .module = moduleUnlessPrefixed(module_name, name_store.exportNameText(export_name)), .text = name_store.exportNameText(export_name), .suffix = suffix }
            else
                .{ .module = module_name, .text = "template", .number = @backingInt(key.template), .suffix = suffix };
        },
        .lambda => |key| .{ .owner = try perfNameOf(program, a, names, key.outer), .text = "lambda@", .number = @backingInt(key.expr) },
        .method_entry => |target| .{ .owner = try perfNameOf(program, a, names, target), .text = "entry" },
        .default => |key| .{ .module = module_name, .text = "default@", .number = @backingInt(key.expr) },
        .adapter => |key| .{ .owner = try perfNameOf(program, a, names, key.outer), .text = "adapter" },
        .literal_thunk => |key| .{ .owner = try perfNameOf(program, a, names, key.target), .text = "literal_thunk", .number = key.literal_requirement },
        .structural => |method| .{ .text = "structural.", .suffix = @tagName(method) },
        .structural_map => |map| .{ .text = "structural_map.", .suffix = program.store.nameText(map.name) },
        .crash_dictionary => |kind| .{ .text = "crash_dictionary.", .suffix = @tagName(kind) },
        .codec => |index| .{ .module = module_name, .text = "codec.", .suffix = @tagName(program.codec.pieces.items[index].kind) },
        .convert => .{ .module = module_name, .text = "convert" },
        .callable_eval_entry => |binding| blk: {
            const owner = program.module(binding.module);
            break :blk .{ .module = owner.nameStore().moduleNameText(owner.view.module_identity.module_name), .text = "callable_eval_entry", .number = @backingInt(binding.template) };
        },
        .root => |index| .{ .text = "root", .number = index },
        .static_data => |index| .{ .text = "static_data", .number = index },
        .root_slot => |key| .{ .module = module_name, .text = "root_slot", .number = @backingInt(key.root) },
    };
    const name = try perfNameLabel(a, label);
    names[@backingInt(id)] = name;
    return name;
}

/// The parts of a worker's readable name: `<owner or module>.<text><suffix
/// or number>`, the owner or module omitted where a worker has neither.
const PerfNameLabel = struct {
    owner: ?[]const u8 = null,
    module: ?[]const u8 = null,
    text: []const u8,
    suffix: []const u8 = "",
    number: ?u32 = null,
};

/// `module`, to prefix the export name `text` with, or null when `text`
/// already begins with `<module>.`.
fn moduleUnlessPrefixed(module: []const u8, text: []const u8) ?[]const u8 {
    const prefixed = text.len > module.len and std.mem.startsWith(u8, text, module) and text[module.len] == '.';
    return if (prefixed) null else module;
}

fn perfNameLabel(a: Allocator, label: PerfNameLabel) Allocator.Error![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    if (label.owner orelse label.module) |prefix| {
        try out.appendSlice(a, prefix);
        try out.append(a, '.');
    }
    try out.appendSlice(a, label.text);
    try out.appendSlice(a, label.suffix);
    if (label.number) |number| try out.print(a, "{d}", .{number});
    return out.items;
}

test "a worker's readable name is its owner or module, then what it is" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    try std.testing.expectEqualStrings("Pdf.render", try perfNameLabel(a, .{ .module = "Pdf", .text = "render" }));
    try std.testing.expectEqualStrings("Pdf.render.lambda@42", try perfNameLabel(a, .{ .owner = "Pdf.render", .text = "lambda@", .number = 42 }));
    try std.testing.expectEqualStrings("Pdf.render.entry", try perfNameLabel(a, .{ .owner = "Pdf.render", .text = "entry" }));
    try std.testing.expectEqualStrings("structural.is_eq", try perfNameLabel(a, .{ .text = "structural.", .suffix = "is_eq" }));
    try std.testing.expectEqualStrings("Pdf.codec.record_body", try perfNameLabel(a, .{ .module = "Pdf", .text = "codec.", .suffix = "record_body" }));
}

test "an export name that already begins with its module is not prefixed again" {
    try std.testing.expect(moduleUnlessPrefixed("Builtin", "Builtin.Num.U8.plus") == null);
    try std.testing.expectEqualStrings("Pdf", moduleUnlessPrefixed("Pdf", "render").?);
    try std.testing.expectEqualStrings("Pdf", moduleUnlessPrefixed("Pdf", "Pdfs.render").?);
}

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
        .observe_expects = input.roots.test_plan_metadata.len != 0 or switch (input.unit) {
            .program => false,
            .module => |unit| unit.expect_sites,
        },
        .list_in_place_map = input.list_in_place_map,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    var retained = false;
    defer if (!retained) {
        program.release();
        allocator.destroy(program);
    };
    // Scratch must not follow a Program retained for function metadata.
    defer program.finishErrorTypes();
    errdefer program.result.deinit();
    program.result.store.runtime_contract = .boxy;
    program.layouts = .{
        .allocator = allocator,
        .store = &program.store,
        .converter = program.converter(),
        .layouts = &program.result.layouts,
    };
    program.static_descs = StaticDescs.init(allocator, &program.store, &program.layouts, &program.result, input.target_usize);
    program.static_descs.table_identities = input.stored_functions;
    program.static_descs.table_owner = input.function_unit orelse @splat(0);

    _ = try program.modules.add(checked.importedView(input.modules.root.module));
    for (input.modules.imports) |view| _ = try program.modules.add(view);
    for (input.modules.root.relation_modules) |view| _ = try program.modules.add(view);
    program.body_indexed = try allocator.alloc(bool, program.modules.views.items.len);
    @memset(program.body_indexed, false);
    base.dev_diagnostics.countTestWork(.boxy_body_index_available_modules, program.modules.views.items.len);
    for (program.modules.views.items) |view| {
        try program.by_module_idx.put(allocator, view.view.module_identity.module_idx, view.index);
        for (view.view.exported_procedure_bindings.bindings) |*binding| {
            try program.exported_bindings.put(allocator, .{ .module = view.index, .def = binding.binding.def, .pattern = binding.binding.pattern }, binding);
        }
        for (view.view.callable_eval_templates.templates) |template| {
            try program.callable_eval_by_pattern.put(allocator, .{ .module = view.index, .pattern = template.pattern }, template.id);
        }
        for (view.view.entry_wrappers.wrappers.items()) |wrapper| {
            try program.wrapper_by_root.put(allocator, .{ .module = view.index, .root = wrapper.root }, wrapper.template);
        }
    }
    try solvePlatformIdentities(program);
    try seedSourceFiles(program);
    try initHosted(program);

    // A module unit lowers every worker of its modules as a root: the
    // workers other objects reach by symbol (design.md "Per-Module Output").
    // A compile-time program's unit lowers only what its roots reach: nothing
    // links against it.
    if (program.moduleUnit() != null and !input.compile_time) try seedUnitWorkers(program);

    var schema_roots: std.ArrayList(TypeId) = .empty;
    defer schema_roots.deinit(allocator);
    const requests = input.roots.requests;
    for (requests, 0..) |request, index| {
        const root_id = request.compile_time_root.get() orelse continue;
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        try program.requested_roots.put(allocator, .{ .module = source_module, .root = root_id }, request.checked_type);
    }
    for (requests, 0..) |request, index| {
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        const worker_id = try lowerRoot(program, request, @intCast(index), source_module);
        const proc = program.worker(worker_id).proc;
        try program.result.root_procs.append(allocator, proc);
        if (request.abi == .platform) {
            try schema_roots.appendSlice(allocator, program.worker(worker_id).params);
            try schema_roots.append(allocator, program.worker(worker_id).ret);
        }
        // A compile-time evaluator stores each root's value by the plan
        // and stored type of the root's own result type. A lowering that
        // evaluates none (a test lowering one constant's root) runs the
        // root as a procedure producing the value.
        if (request.abi == .compile_time and input.compile_time) {
            const ret = program.worker(worker_id).ret;
            // A literal argument root whose value holds a function is
            // validated and discarded: nothing stores its value (design.md
            // "Literal Arguments").
            const validate_only = if (request.compile_time_root.get()) |root_id| blk: {
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
                .ty = try program.module(root_module).types().rootKey(request.checked_type),
                .checked_type = request.checked_type,
                .const_locator = request.const_locator,
                .layout_idx = try program.layouts.layoutOf(worker.ret),
                .plan = plan,
                .initializer = .some(worker.proc),
            });
        }
    }

    if (program.moduleUnit() != null and !input.compile_time) {
        for (program.workers.items) |worker| {
            if (worker.imported or !program.result.store.getProcSpec(worker.proc).exported) continue;
            // An importer fills the worker's tables from its needs.
            if (worker.needs) |needs| {
                const result_allocator = program.result.store.allocator;
                const bytes = try StoredFn.encodeNeeds(result_allocator, program, needs);
                errdefer result_allocator.free(bytes);
                try program.result.boxy_table_needs.put(result_allocator, program.result.store.getProcSpec(worker.proc).identity, bytes);
            }
        }
    }

    if (base.dev_diagnostics.isSet(.perf_map)) try recordPerfNames(program);

    if (input.proc_debug_names) {
        for (program.workers.items) |worker| {
            // A template's worker carries its procedure's source name.
            const name: []const u8 = switch (worker.key) {
                .codec => |piece| switch (program.codec.pieces.items[piece].kind) {
                    inline .constructor, .runtime, .encode_helper, .parse_helper, .record_body, .tuple_body, .list_body, .dict_body, .tag_body, .value_thunk, .key_thunk, .tag_spec, .field_names_step => |kind| "boxy.codec." ++ @tagName(kind),
                },
                .template => |key| blk: {
                    const view = program.module(key.module);
                    const template = view.view.checked_procedure_templates.templates.items()[@backingInt(key.template)];
                    const names = view.nameStore();
                    const export_name = names.procBase(template.proc_base).export_name.get() orelse break :blk switch (key.binding) {
                        .generic => "boxy.template",
                        .list_int => |item| try std.fmt.allocPrint(program.arena.allocator(), "boxy.template{s}", .{item.nameSuffix()}),
                    };
                    break :blk switch (key.binding) {
                        .generic => names.exportNameText(export_name),
                        .list_int => |item| try std.fmt.allocPrint(program.arena.allocator(), "{s}{s}", .{ names.exportNameText(export_name), item.nameSuffix() }),
                    };
                },
                inline .adapter, .default, .lambda, .method_entry, .structural, .structural_map, .root, .static_data, .crash_dictionary, .convert, .literal_thunk, .root_slot, .callable_eval_entry => |_, kind| "boxy." ++ @tagName(kind),
            };
            try program.result.store.setProcDebugName(worker.proc, name);
        }
    }
    program.result.finishExpectSites();
    var runtime_schemas = try @import("runtime_schemas.zig").collect(program, schema_roots.items);
    errdefer runtime_schemas.deinit();
    const static_data = try program.static_descs.takeExports();
    const kept = if (input.stored_functions) try StoredFn.retain(allocator, program) else null;
    retained = kept != null;
    return .{
        .lir_result = program.result,
        .static_data = static_data,
        .runtime_schemas = runtime_schemas,
        .retained = kept,
    };
}

/// Queue every worker a module unit's modules define for other objects:
/// each template and its dictionary entry (a template is a function value
/// wherever another module names it, and a method wherever one dispatches
/// to it), and each `??` default expression (design.md "Per-Module
/// Output"). Finish this closure and publish its exports before lowering
/// command roots, whose private helpers are not an interface for importers.
fn seedUnitWorkers(program: *Program) Allocator.Error!void {
    for (program.modules.views.items) |*view| {
        if (!program.ownsModule(view.index)) continue;
        try view.bodies().demandIndex();
        // Checked procedure aliases forward directly to their targets. They
        // have no evaluated value, so their evaluator wrappers are not part
        // of the module's runtime interface.
        const forwarded_roots = try program.allocator.alloc(bool, view.view.compile_time_roots.roots.len);
        defer program.allocator.free(forwarded_roots);
        @memset(forwarded_roots, false);
        for (view.view.callable_eval_templates.templates) |callable| {
            if (callable.forwarded_lookup.get() != null) forwarded_roots[@backingInt(callable.root)] = true;
        }
        for (view.view.checked_procedure_templates.templates.items(), 0..) |template, index| {
            if (!unitEmitsTemplate(view, template, forwarded_roots)) continue;
            if (try templateRejected(program, view, template)) continue;
            const worker = try program.templateWorker(view.index, @fromBackingInt(@intCast(index)));
            _ = try program.methodEntryWorker(worker);
            // A `List` template's integer list workers are lowered with it,
            // in `ListIntItem` order: every importer's direct calls at those
            // types name them by symbol (design.md "Pre-Specialized Integer
            // List Workers").
            if (try program.listItemSlot(worker) != null) {
                for (std.enums.values(ListIntItem)) |item| _ = try program.listIntTemplateWorker(view.index, @fromBackingInt(@intCast(index)), item);
            }
        }
        for (view.view.method_registry.entries) |entry| {
            const target = entry.target orelse continue;
            const template = switch (target.kind) {
                .procedure => |procedure| procedure.template,
                .local_proc, .structural, .callable_binding => continue,
            };
            const owner = program.moduleByKey(.{ .bytes = template.artifact.bytes });
            if (try templateRejected(program, owner, owner.view.checked_procedure_templates.get(template.template))) continue;
            const worker = try program.templateRefWorker(template);
            if (program.worker(worker).imported) continue;
            _ = try program.methodEntryWorker(worker);
        }
        for (view.bodies().defaultExprs()) |default| {
            _ = try program.defaultWorker(view.index, @fromBackingInt(default));
        }
    }
    try program.drain();
    for (program.workers.items) |worker| {
        if (worker.imported or !Program.exportableKind(worker.key)) continue;
        program.result.store.getProcSpecPtr(worker.proc).exported = true;
        try program.result.exported_procs.append(program.allocator, worker.proc);
    }
}

/// Whether checking rejected `template`'s signature: it has no type to lower
/// at, and every use of it is a runtime error of its own.
fn templateRejected(program: *Program, view: *const types.ModuleView, template: checked.CheckedProcedureTemplate) Allocator.Error!bool {
    return try program.hasError(view.index, template.checked_fn_root);
}

/// Whether a module unit emits `template` (design.md "Per-Module Output"):
/// every template another object may call or compose, which leaves out the
/// wrappers of `expect` and REPL roots. Nothing reaches those but a test
/// plan's or a REPL's own roots, and each of those lowers as one program.
/// Exact procedure aliases forward to their targets without evaluator wrappers.
fn unitEmitsTemplate(view: *const types.ModuleView, template: checked.CheckedProcedureTemplate, forwarded_roots: []const bool) bool {
    return switch (template.body) {
        .checked_body, .intrinsic_wrapper, .unimplemented => true,
        .entry_wrapper => |wrapper_id| blk: {
            const root_id = view.view.entry_wrappers.get(wrapper_id).root;
            if (forwarded_roots[@backingInt(root_id)]) break :blk false;
            break :blk switch (view.view.compile_time_roots.root(root_id).kind) {
                .expect, .repl_expr => false,
                .callable_binding, .constant, .hoisted_constant, .hoisted_validation, .numeral_conversion, .quote_conversion, .literal_argument => true,
            };
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
            // Persisted unit metadata must be identical across checkouts.
            // In-process programs keep current workspace names for reporting;
            // cached unit locations rebind through module_identity instead.
            .qualified_name = switch (program.input.unit) {
                .module => if (program.input.compile_time) view.view.module_env.qualifiedModuleName() else view.view.module_env.module_name,
                .program => view.view.module_env.qualifiedModuleName(),
            },
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
    else if (request.procedure_binding) |binding| switch (program.module(source_module).view.top_level_procedure_bindings.bindings.items()[@backingInt(binding)].body) {
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
                if (record.ext != .none) invariant("an open record reached a host boundary");
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

test "a generated worker's identity is its content, whatever lowering reaches it" {
    const allocator = std.testing.allocator;
    const unit: Unit = .{ .module = .{ .imports = undefined, .root = false, .names = undefined } };
    var identities: [2][4]LIR.ProcIdentity = undefined;
    var record_ids: [2]TypeId = undefined;
    for (0..2) |round| {
        // Initialize the owners used by module registration, worker
        // production and identity generation; unrelated state stays unread.
        var program: Program = undefined;
        program.allocator = allocator;
        program.input = .{ .modules = undefined, .roots = undefined, .target_usize = .u64, .unit = unit };
        program.store = types.Store.init(allocator);
        defer program.store.deinit();
        program.result = try LirProgram.Result.init(allocator, .u64);
        defer program.result.deinit();
        program.modules = .{ .allocator = allocator };
        defer program.modules.deinit();
        var module_view: checked.ImportedModuleView = undefined;
        module_view.key = .{ .bytes = @splat(7) };
        module_view.module_identity.stable_hash = @splat(7);
        const module_index = try program.modules.add(module_view);
        program.arena = std.heap.ArenaAllocator.init(allocator);
        defer program.arena.deinit();
        program.worker_ids = .empty;
        defer program.worker_ids.deinit(allocator);
        program.worklist = .empty;
        defer program.worklist.deinit(allocator);
        program.workers = .empty;
        defer program.workers.deinit(allocator);
        program.type_contents = .empty;
        defer program.type_contents.deinit(allocator);
        const store = &program.store;
        const number = try store.primitive(.u64);
        // The second lowering interns another type first, so the same
        // types get other ids there.
        if (round == 1) _ = try store.intern(.{ .tuple = &.{ number, number } });
        const name_x = try store.internName("x");
        const record = try store.intern(.{ .record = .{ .fields = &.{.{ .name = name_x, .ty = number, .kind = .inline_slot }}, .ext = null } });
        record_ids[round] = record;
        const function = try store.intern(.{ .function = .{ .args = &.{record}, .ret = number, .effectful = false } });
        // Both lowerings obtain the same target identity from the worker
        // producer; its WorkerId belongs to this lowering's inventory.
        const target_id = try program.structuralWorker(module_index, .to_hash);
        identities[round] = .{
            try program.generatedWorkerIdentity(.{ .structural = .is_eq }, module_index, round, &.{}),
            try program.generatedWorkerIdentity(.{ .structural = .to_hash }, module_index, round + 1, &.{}),
            try program.generatedWorkerIdentity(.{ .adapter = .{ .outer = target_id, .from = function, .to = function } }, module_index, round + 2, &.{}),
            try program.generatedWorkerIdentity(.{ .literal_thunk = .{ .target = target_id, .bound = record, .literal_requirement = 1 } }, module_index, round + 3, &.{}),
        };
        // A program's own worker is local to the program.
        program.input.unit = .program;
        const local = try program.generatedWorkerIdentity(.{ .structural = .is_eq }, module_index, round, &.{});
        try std.testing.expect(!std.meta.eql(identities[round][0], local));
    }
    try std.testing.expect(record_ids[0] != record_ids[1]);
    for (identities[0], identities[1]) |first, second| try std.testing.expectEqual(first, second);
    // Workers of different content have different identities.
    for (identities[0], 0..) |identity, index| {
        for (identities[0][index + 1 ..]) |other| try std.testing.expect(!std.meta.eql(identity, other));
    }
}

test "Boxy field default identity survives substitution and stable type digests" {
    const allocator = std.testing.allocator;
    var digests: [2][3][32]u8 = undefined;
    for (0..2) |round| {
        var program: Program = undefined;
        program.allocator = allocator;
        program.store = types.Store.init(allocator);
        defer program.store.deinit();
        program.modules = .{ .allocator = allocator };
        defer program.modules.deinit();
        program.type_contents = .empty;
        defer program.type_contents.deinit(allocator);
        for (0..2) |index| {
            var view: checked.ImportedModuleView = undefined;
            view.key = .{ .bytes = @splat(@intCast(if (round == 0) index + 1 else 2 - index)) };
            view.module_identity.stable_hash = view.key.bytes;
            _ = try program.modules.add(view);
        }
        const store = &program.store;
        const name = try store.internName("count");
        const variable = try store.variable(0);
        const number = try store.primitive(.u8);
        var fields = [_]types.Field{.{ .name = name, .ty = variable, .kind = .inline_slot }};
        for (0..3) |variant| {
            fields[0].default = .of(if (variant == 0) null else .{ .module = @fromBackingInt(@intCast(round)), .expr_node = @intCast(variant * 10) });
            const generic = try store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
            const concrete = try program.substitute(allocator, generic, &.{number});
            const field = store.fieldSpan(store.get(concrete).record.fields)[0];
            try std.testing.expectEqual(number, field.ty);
            try std.testing.expectEqual(fields[0].default, field.default);
            digests[round][variant] = try program.typeContent(concrete);
        }
    }
    for (digests[0], digests[1]) |first, second| try std.testing.expectEqual(first, second);
    for (digests[0], 0..) |first, index| {
        for (digests[0][index + 1 ..]) |second| try std.testing.expect(!std.mem.eql(u8, &first, &second));
    }
}

test "Boxy error reachability Program owns one lazy memo and releases it before retention" {
    const allocator = std.testing.allocator;
    var checked_types = checked.CheckedTypeStore{};
    defer checked_types.deinit(allocator);
    checked_types.attachSynthesizedKeys(try checked.SynthesizedTypeKeys.create(allocator, std.testing.io, 0));
    const bad = try checked_types.appendSyntheticPayloadRoot(allocator, .err);
    const root = try checked_types.appendSyntheticPayloadRoot(allocator, .{ .tuple = try allocator.dupe(checked.CheckedTypeId, &.{bad}) });
    var view: checked.ImportedModuleView = undefined;
    view.key = .{ .bytes = @splat(1) };
    view.module_identity.stable_hash = @splat(1);
    view.checked_types = checked_types.view();
    // Exercise the actual consumer API and lowering's completion cleanup;
    // unrelated worker/LIR state is never accessed by either operation.
    var program: Program = undefined;
    program.allocator = allocator;
    program.modules = .{ .allocator = allocator };
    defer program.modules.deinit();
    const module = try program.modules.add(view);
    program.error_types = null;
    defer program.releaseErrorTypes();
    try std.testing.expect(program.error_types == null);
    // Even a lowering that never queried errors can finish without a memo.
    program.finishErrorTypes();
    try std.testing.expect(program.error_types == null);
    try std.testing.expect(try program.hasError(module, root));
    const errors = program.error_types.?;
    try std.testing.expect(try program.hasError(module, root));
    try std.testing.expect(try program.hasError(module, bad));
    try std.testing.expectEqual(errors, program.error_types.?);
    try std.testing.expectEqual(@as(usize, 3), errors.stats().queries);
    try std.testing.expectEqual(@as(usize, 2), errors.stats().unique_expanded_nodes);
    // `run` executes this defer even when StoredFn retains the Program.
    program.finishErrorTypes();
    try std.testing.expect(program.error_types == null);
    program.releaseErrorTypes(); // later destruction of a retained Program
}
