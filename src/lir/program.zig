//! LIR program result shared by post-check lowering, ARC, LirImage, glue, and
//! interpreter consumers.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const root = @import("root_metadata.zig");

const Allocator = std.mem.Allocator;
const names = check.CheckedNames;
const checked = check.CheckedModule;
const const_store = check.ConstStore;
const dispatch = check.StaticDispatchRegistry;

/// Dense index in the export slice returned by one static-data materialization.
pub const StaticDataSymbolId = enum(u32) { _ };

/// Immutable data symbol materialized in the target's readonly representation.
pub const StaticDataExport = struct {
    /// Linker-visible symbol name, for example `roc__answer`.
    symbol_name: []const u8,
    /// LIR static root represented by this export, when it is an internal value.
    value_id: ?LIR.StaticDataId = null,
    /// Fully materialized Roc ABI bytes for the constant.
    bytes: []const u8,
    /// Offset inside `bytes` where `symbol_name` points.
    symbol_offset: u32 = 0,
    /// Required target alignment of the symbol.
    alignment: u32,
    /// Whether an object-file symbol has global linker binding.
    is_global: bool = true,
    /// Whether this symbol is part of the host-visible ABI.
    is_exported: bool = true,
    /// Pointer relocations from this symbol's bytes to other symbols.
    relocations: []const StaticDataRelocation = &.{},
    /// The capacities the empty lists inside this value were evaluated
    /// with, by byte offset of each list's descriptor in `bytes`. A frozen
    /// list's capacity word is its length, so the requests would otherwise
    /// be lost; a runtime consumer rebuilding the value uses them.
    empty_list_capacities: []const EmptyListCapacity = &.{},
};

/// The evaluated capacity of one empty list inside a frozen value.
pub const EmptyListCapacity = struct {
    offset: u64,
    capacity: u64,
};

/// One explicit pointer relocation inside a readonly static-data symbol.
pub const StaticDataRelocation = struct {
    /// Runtime meaning of a relocation target.
    pub const Kind = enum {
        address,
        function_pointer,
    };

    /// Byte offset inside `StaticDataExport.bytes` where the pointer is stored.
    offset: u64,
    /// Symbol whose address should be written at `offset`.
    target_symbol_name: []const u8,
    /// Address identity: an explicit linker declaration or a row in the owning export slice.
    target: union(enum) { named, data_symbol: StaticDataSymbolId } = .named,
    /// Addend applied to the target symbol address.
    addend: i64 = 0,
    /// Runtime meaning of the stored pointer.
    kind: Kind = .address,
    /// For an erased-callable function pointer, the byte distance from this
    /// pointer field to the callable's capture bytes.
    callable_capture_offset: ?u32 = null,
    /// Exact LIR procedure named by an erased-callable function relocation.
    ///
    /// In-process consumers use this identity directly; object backends use
    /// `target_symbol_name` as its linker representation.
    procedure: ?LIR.LirProcSpecId = null,
    /// Producer recipe within the erased-function set, retained for transcoding.
    boxy_recipe: ?u32 = null,
    /// Exact generated RC helper required by this function-pointer relocation.
    ///
    /// Static erased-callable `on_drop` slots are always atomic: their
    /// construction site makes no thread-confinement claim. Backends consume
    /// this identity directly instead of recovering it from a symbol or layout.
    rc_helper: ?layout.RcHelperKey = null,
    /// Whether `target_symbol_name` is owned by this relocation.
    owns_target_symbol_name: bool = false,
};

/// Owned frozen bytes retained with a lowered compilation.
pub const FrozenStaticData = struct {
    allocator: Allocator,
    exports: []StaticDataExport,

    pub fn deinit(self: *FrozenStaticData) void {
        for (self.exports) |item| {
            self.allocator.free(item.symbol_name);
            self.allocator.free(item.bytes);
            for (item.relocations) |relocation| {
                if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
            }
            self.allocator.free(item.relocations);
            self.allocator.free(item.empty_list_capacities);
        }
        self.allocator.free(self.exports);
        self.* = undefined;
    }
};

/// Layout requested for a checked value type digest.
pub const RequestedLayout = struct {
    ty: names.TypeDigest,
    checked_type: checked.CheckedTypeId,
    const_locator: ?checked.ConstLocator = null,
    layout_idx: layout.Idx,
    plan: ConstPlanId,
    /// Closed LIR procedure that constructs the exact target representation for
    /// a provided static data export. Plain layout-only requests leave this null.
    initializer: ?LIR.LirProcSpecId = null,
};

/// Identifier for a finite callable set in the LIR program.
pub const FnSetId = enum(u32) { _ };
/// Identifier for an erased callable entry set in the LIR program.
pub const ErasedFnsId = enum(u32) { _ };
/// Identifier for one finite callable variant.
pub const FnVariantId = enum(u32) { _ };

/// Callable lowering result used by const plans.
pub const FnResult = union(enum) {
    finite: FnSetId,
    erased: ErasedFnsId,
};

/// Exact member context in the common target-independent Lambda Solved graph.
/// Own captures belong to `source`; solved captures name their producer span.
pub const FrozenCallableContext = struct {
    abi: enum { finite, erased },
    source: u32,
    fn_type: u32,
    captures: union(enum) {
        own: u32,
        solved: struct { start: u32, len: u32 },
    },
};

/// Checked function template and source type used to emit callable code.
pub const FnTemplate = struct {
    /// Original function slot in the shared frozen Monotype owner.
    frozen_fn: ?u32 = null,
    /// Exact callable worker specialization key, emitted by SpecConstr:
    /// SHA-256 template, callable-ABI, and capture-ABI digests in that order.
    frozen_worker: ?[96]u8 = null,
    /// Exact member specialization within the shared frozen Solved owner.
    frozen_context: ?FrozenCallableContext = null,
    fn_def: const_store.FnDef,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    evidence: []const const_store.ConstFnEvidence = &.{},
    evidence_frames: []const const_store.ConstFnEvidenceFrame = &.{},
    evidence_frame_head: ?u32 = null,
};

/// Capture field copied from a checked binding into a callable payload. `id`
/// is checked-stage provenance for storing a compile-time result in
/// `ConstStore`; runtime capture joining was completed before LIR.
pub const CaptureSlot = struct {
    id: const_store.CaptureId,
    slot: u32,
    ty: const_store.ConstTypeId,
    plan: ConstPlanId,
    storage: CaptureSlotStorage,
};

/// Physical storage used by a callable capture slot while storing its value.
pub const CaptureSlotStorage = enum(u8) {
    value,
    recursive_box,
};

/// One runtime tag variant for a finite callable value.
pub const FnVariant = struct {
    id: FnVariantId,
    discriminant: u32,
    variant_index: u32,
    payload_layout: layout.Idx,
    template: FnTemplate,
    captures: []const CaptureSlot = &.{},
};

/// Runtime tag-union encoding for a finite callable set.
pub const FnSet = struct {
    layout: layout.Idx,
    variants: []const FnVariant = &.{},
};

/// One erased callable entry and its capture layout plan.
pub const ErasedFn = struct {
    on_drop: LIR.ErasedCallableOnDrop = .none,
    entry: LIR.LirProcSpecId,
    capture_layout: layout.Idx = .zst,
    template: ?FnTemplate = null,
    captures: []const CaptureSlot = &.{},
    /// Boxy-owned producer identity and typed runtime environment. These
    /// captures do not claim ConstStore provenance.
    boxy: ?BoxyFrozenCallable = null,
};

/// Typed field of a Boxy callable frozen during literal evaluation.
pub const BoxyFrozenCapture = struct {
    slot: u32,
    value: union(enum) {
        value: ConstPlanId,
        descriptor: BoxyTypeDescId,
        contents_descriptor: BoxyTypeDescId,
        dictionary: BoxyDictId,
    },
};

/// Closed producer recipe shared by host and target literal lowering.
pub const BoxyFrozenCallable = struct {
    key: [32]u8,
    result_desc: ?BoxyTypeDescId = null,
    captures: []const BoxyFrozenCapture,
};

/// Checked contract and selected implementation of a dictionary boundary.
/// The implementation identity survives inlining and procedure pruning.
pub const BoxyFrozenMethodOrigin = struct {
    worker: LIR.ProcIdentity,
    requirement_module: checked.ModuleId,
    requirement_type: checked.CheckedTypeId,
    callable_module: checked.ModuleId,
    callable_type: checked.CheckedTypeId,
};

/// Runtime encoding for an erased callable value type.
pub const ErasedFns = struct {
    layout: layout.Idx,
    entries: []const ErasedFn = &.{},
};

/// Identifier for a constant storage plan emitted with LIR.
pub const ConstPlanId = enum(u32) { _ };

const ExpectSiteKey = struct {
    file: u32,
    line: u32,
    column: u32,
    region_start: u32,
    region_end: u32,

    fn init(loc: base.SourceLoc, region: base.Region) ExpectSiteKey {
        return .{
            .file = loc.file,
            .line = loc.line,
            .column = loc.column,
            .region_start = region.start.offset,
            .region_end = region.end.offset,
        };
    }
};

/// Stable index of a Boxy type descriptor in the program side tables.
pub const BoxyTypeDescId = LIR.BoxyTypeDescId;
/// Stable index of a Boxy method dictionary in the program side tables.
pub const BoxyDictId = LIR.BoxyDictId;
/// Stable index of an explicit Boxy representation adapter.
pub const BoxyAdapterId = LIR.BoxyAdapterId;
/// Stable index of a method slot within a Boxy dictionary.
pub const BoxyMethodSlotId = enum(u32) { _ };
/// Explicit source for resolving a Boxy type descriptor.
pub const BoxyDescRef = LIR.BoxyDescRef;
/// Explicit source for resolving a Boxy dictionary.
pub const BoxyDictRef = LIR.BoxyDictRef;
/// Compact start and length pair into a Boxy side table.
pub const BoxySpan = LIR.BoxySpan;
/// Ownership transfer applied by one Boxy adaptation step.
pub const BoxyTransferMode = LIR.BoxyTransferMode;
/// One descriptor-guided representation adaptation step.
pub const BoxyAdaptStep = LIR.BoxyAdaptStep;
/// Operation performed by one descriptor payload traversal step.
pub const BoxyPayloadOp = LIR.BoxyPayloadOp;
/// One explicit descriptor payload traversal step.
pub const BoxyPayloadStep = LIR.BoxyPayloadStep;

/// Runtime metadata for one tag in a boxy tag-union descriptor.
pub const BoxyTagVariant = struct {
    name: LIR.BoxyNameId,
    discriminant: u32,
    /// Number of source-language payloads carried by this tag. A single
    /// aggregate payload is distinct from a multi-payload tag whose runtime
    /// payload is also a struct.
    payload_count: u32 = 0,
    payload_layout: layout.Idx,
    payload_descs: BoxySpan = .{},
};

/// Descriptor metadata for one dynamic payload in a boxy tag variant.
pub const BoxyTagPayloadDesc = struct {
    payload_index: u32,
    desc: BoxyDescRef,
};

/// Purpose of one explicit boxy representation adapter.
pub const BoxyAdapterKind = enum {
    host_to_boxy,
    boxy_to_host,
    boxy_to_boxy,
    hosted_arg,
    hosted_ret,
    container_element,
    method_arg,
    method_ret,
};

/// Runtime operation selected when an adapter is built after layout planning.
pub const BoxyAdapterOperation = enum {
    relabel,
    materialize,
};

/// Explicit representation adaptation plan used by boxy LIR statements.
pub const BoxyAdapter = struct {
    kind: BoxyAdapterKind,
    operation: BoxyAdapterOperation = .materialize,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    steps: BoxySpan = .{},
    consumes_source: bool,
    produces_owned_result: bool,
};

/// Source-language shape of the value a boxy descriptor describes. Inspection
/// dispatches on this instead of on the payload layout, which erases
/// structure: a zero-sized record, tuple, and single-tag union all share the
/// `zst` layout.
pub const BoxyDescShape = enum {
    /// Built-in scalar or `Str`, whose payload layout decodes the value.
    primitive,
    /// Record; `field_names` and `nested_descs` have one entry per field.
    record,
    /// Tuple; `nested_descs` has one entry per element.
    tuple,
    /// Tag union; `tag_variants` describes every variant.
    tag_union,
    /// List; `nested_descs` holds exactly the item descriptor.
    list,
    /// Box; `nested_descs` holds exactly the payload descriptor.
    box,
    /// Erased storage whose value is described by its boxed allocation.
    erased,
    /// Callable value.
    function,
    /// Compiler-internal storage with no source-language shape, such as an
    /// erased callable's capture struct.
    internal,

    /// Shape of a descriptor that describes only a value's storage: a scalar
    /// is a primitive, and any other storage is internal, which inspection
    /// never renders.
    pub fn forStorage(storage: layout.Layout) BoxyDescShape {
        return if (storage.tag == .scalar) .primitive else .internal;
    }
};

/// Which runtime context a static boxy descriptor's reachable references read.
/// Ordered from least to most context-dependent.
pub const BoxyDescClosure = enum(u8) {
    /// Every reachable reference is static, so the runtime uses the
    /// descriptor in place without instantiating it.
    closed,
    /// Reachable references read only the materialization's captured
    /// descriptor locals, so one instantiation serves every materialization
    /// with the same captured descriptors.
    captures,
    /// Some reachable reference reads other runtime context.
    context,
};

/// Runtime data for representation and structural operations on a boxy value.
pub const BoxyTypeDesc = struct {
    payload_layout: layout.Idx,
    contains_refcounted: bool,
    shape: BoxyDescShape,
    /// One descriptor per child position, including zero-sized and scalar
    /// children: struct field `i` (by original field index) is entry `i`, and
    /// a list's item or a box's payload is entry 0. Consumers index this
    /// directly by position.
    nested_descs: BoxySpan = .{},
    tag_variants: BoxySpan = .{},
    tag_ext_desc: ?BoxyDescRef = null,
    /// Record field names in payload field order, one per field. Empty for
    /// non-record payloads (including tuples, which print positionally).
    field_names: BoxySpan = .{},
    /// Present-variant discriminant when these bytes use the canonical
    /// optional-field slot convention. This is compiler-produced semantic
    /// data, not a runtime inference from tags or layouts.
    presence_slot_present_discriminant: ?u32 = null,
    /// The described value is an opaque nominal type: inspect must not
    /// reveal its backing structure.
    inspect_opaque: bool = false,
    copy_plan: BoxySpan = .{},
    drop_plan: BoxySpan = .{},
    inspect_method: ?BoxyMethodSlotId = null,
    /// The hidden descriptors `inspect_method`'s worker receives, in worker
    /// parameter order. They describe this descriptor's own type arguments,
    /// so a runtime-instantiated descriptor carries its own copies.
    inspect_hidden_descs: BoxySpan = .{},
    /// One descriptor: this value in the storage of `inspect_method`'s
    /// worker parameter, instantiated at this descriptor's type arguments.
    inspect_arg_descs: BoxySpan = .{},
    /// The type's own `is_eq`, which descriptor-guided equality calls in
    /// place of comparing the value's structure.
    eq_method: ?BoxyMethodSlotId = null,
    /// The hidden descriptors `eq_method`'s worker receives, in worker
    /// parameter order, like `inspect_hidden_descs`.
    eq_hidden_descs: BoxySpan = .{},
    /// Two descriptors: this value in the storage of each of `eq_method`'s
    /// worker parameters.
    eq_arg_descs: BoxySpan = .{},
    /// The static dictionaries `eq_method`'s worker receives at this
    /// descriptor's type, in worker parameter order.
    eq_nested_dicts: BoxySpan = .{},
    /// The type declares `is_eq`, but checking rejected that declaration:
    /// descriptor-guided equality reaching this type crashes as code checking
    /// rejected, exactly as a specialized comparison of it does.
    eq_rejected: bool = false,
    /// The type's own `to_hash`, which descriptor-guided hashing calls in
    /// place of hashing the value's structure.
    hash_method: ?BoxyMethodSlotId = null,
    /// The hidden descriptors `hash_method`'s worker receives, in worker
    /// parameter order, like `inspect_hidden_descs`.
    hash_hidden_descs: BoxySpan = .{},
    /// Two descriptors: this value in the storage of `hash_method`'s worker
    /// value parameter, and the Hasher.
    hash_arg_descs: BoxySpan = .{},
    /// The static dictionaries `hash_method`'s worker receives at this
    /// descriptor's type, in worker parameter order.
    hash_nested_dicts: BoxySpan = .{},
    /// The type declares `to_hash`, but checking rejected that declaration,
    /// like `eq_rejected`.
    hash_rejected: bool = false,
    /// The described value is builtin Bool, which derived hashing writes as a
    /// Bool rather than as a tag.
    is_bool: bool = false,
    debug_checked_type: ?checked.CheckedTypeId = null,
    /// Set for static descriptors once lowering has produced every descriptor;
    /// a descriptor built at runtime reads runtime context.
    closure: BoxyDescClosure = .context,
};

/// Adapter metadata for one dictionary method slot.
pub const BoxyMethodAdapter = struct {
    arg_layouts: BoxySpan = .{},
    ret_layout: ?layout.Idx = null,
    arg_descs: BoxySpan = .{},
    /// Compact static descriptor references used by `call_desc_sources`.
    call_descs: BoxySpan = .{},
    /// Exact source for every descriptor-bearing position in the checked
    /// method requirement, in requirement traversal order. Empty means the
    /// legacy one-to-one static `call_descs` representation.
    call_desc_sources: BoxySpan = .{},
    ret_desc: ?BoxyDescRef = null,
    nested_dicts: BoxySpan = .{},
    hidden_desc_sources: BoxySpan = .{},
};

/// Origin of a hidden descriptor argument passed to a dictionary method.
pub const BoxyMethodHiddenDescSource = union(enum) {
    /// Index into the slot's `hidden_descs`; for a descriptor-carried inspect
    /// method, into the inspected descriptor's `inspect_hidden_descs`.
    slot: u32,
    call: u32,
    argument: u32,
};

/// One callable slot in a boxy dictionary.
pub const BoxyMethodSlot = struct {
    /// False for an unimplemented program-wide method slot in a dictionary
    /// that requires only a subset of the program's semantic methods.
    present: bool = true,
    method: names.MethodNameId,
    proc: LIR.LirProcSpecId,
    hidden_descs: BoxySpan = .{},
    nested_dicts: BoxySpan = .{},
    adapter: BoxyMethodAdapter = .{},
};

/// Runtime data for polymorphic behavior and static dispatch in boxy LIR.
pub const BoxyDict = struct {
    debug_dispatch_plan: ?dispatch.StaticDispatchPlanId = null,
    method_slots: BoxySpan = .{},
    /// The method slots name frame locals (`.local` descriptor and dictionary
    /// references); an `assign_boxy_dict_ref` with captures materializes it.
    template: bool = false,
};

/// Tag variant in a constant storage plan.
pub const ConstTagVariant = struct {
    name: []const u8,
    checked_name: names.TagNameId,
    discriminant: u32,
    payloads: []const ConstPlanId = &.{},
};

/// Shape plan used to store an interpreted compile-time result in ConstStore.
pub const ConstPlan = union(enum) {
    pending,
    /// Layout-only request. This plan has no ConstStore materialization shape;
    /// consumers must use it only for requested layout metadata.
    layout_only,
    zst,
    scalar,
    str,
    list: ConstPlanId,
    box: ConstPlanId,
    /// Boxy worker box with a producer-resolved payload layout.
    boxy_box: struct { payload: ConstPlanId, layout_idx: layout.Idx },
    tuple: []const ConstPlanId,
    record: []const ConstPlanId,
    tag_union: []const ConstTagVariant,
    named: struct {
        named_type: check.CheckedModule.ConstNamedType,
        backing: ConstPlanId,
    },
    fn_value: FnSetId,
    erased_fn: ErasedFnsId,
};

/// Constant root metadata needed after LIR interpretation finishes.
pub const ConstRootPlan = struct {
    root_order: u32,
    /// Checked module that owns this root's compile-time root id, checked
    /// types and diagnostics. One lowered program unions several modules'
    /// root requests, so position in the root plan is not an owner.
    owner: LIR.LoweringModuleId,
    request: check.CheckedModule.RootRequest,
    proc: LIR.LirProcSpecId,
    ret_layout: layout.Idx,
    /// Exact producer-owned Monotype representation of the evaluated root.
    /// ConstStore restoration consumes this instead of reconstructing
    /// representation evidence from the public checked type.
    ret_type: const_store.ConstTypeId,
    plan: ConstPlanId,
    /// Consumer-requested materialization slot, when the root manifest asks
    /// for one. Other reads may declare additional representation-specific
    /// slots; this field is not the complete publication inventory.
    value_slot: ?LIR.StaticDataId = null,

    pub fn shape(self: ConstRootPlan) RootShape {
        return .{ .ret_layout = self.ret_layout, .plan = self.plan };
    }
};

/// The representation an evaluated root's value is frozen from.
pub const RootShape = struct {
    ret_layout: layout.Idx,
    plan: ConstPlanId,
};

/// One literal root: a custom literal's conversion, at the concrete type one
/// specialization gives it, evaluated at compile time. Its procedure returns
/// the converted value and crashes at the literal's rejection when the
/// conversion returns `Err`.
pub const LiteralRootPlan = struct {
    /// Checked module that owns the literal.
    module: checked.ModuleId,
    id: LIR.LiteralRootId,
    site: LIR.LiteralRejectionSite,
    proc: LIR.LirProcSpecId,
    ret_layout: layout.Idx,
    plan: ConstPlanId,
    /// Every consumer that reads a literal root reads it from this slot.
    value_slot: LIR.StaticDataId,

    pub fn shape(self: LiteralRootPlan) RootShape {
        return .{ .ret_layout = self.ret_layout, .plan = self.plan };
    }
};

/// One exact LIR value construction that is frozen as readonly target data.
pub const StaticDataValue = struct {
    /// Post-ARC guards reading this slot; threaded through ComptimeValueGuard.
    first_comptime_guard: ?u32 = null,
    /// Null when completed frozen data supplies this slot directly.
    initializer: ?LIR.LirProcSpecId,
    layout_idx: layout.Idx,
    /// The procedure every read of a compile-time root goes through in a
    /// program lowered before its roots were evaluated. Its body is the slot
    /// read until the root completes; a root that completed as a
    /// construction then rebuilds it there, without touching the callers.
    accessor: ?LIR.LirProcSpecId = null,
    /// Successful construction replacement is performed only once per accessor.
    accessor_rebuilt: bool = false,
    /// An evaluated root owns this slot. Its initializer is representation
    /// evidence; materialization must consume the completed root value.
    compile_time_root: ?struct {
        module: checked.ModuleId,
        root: LIR.ComptimeProducer,
        const_locator: ?checked.ConstLocator,
        role: union(enum) {
            value: struct { failure_slot: LIR.StaticDataId, plan: ConstPlanId },
            failure_message: struct {
                failed_field: u32,
                message_field: u32,
                failed_offset: u32,
                message_offset: u32,
            },
        },
    } = null,
};

/// The `failed` byte of a compile-time root's failure record.
pub const ComptimeFailureKind = enum(u8) {
    none = 0,
    crash = 1,
    /// The root's evaluation reached code checking rejected; a read of its
    /// value crashes as that code does.
    checked_error = 2,
};

/// Exact post-ARC guard identity consumed by successful-root completion.
pub const ComptimeValueGuard = struct {
    next_for_slot: ?u32 = null,
    /// Shared statements have one record per owning procedure.
    owner: LIR.LirProcSpecId,
    completed: bool = false,
    crash: LIR.CFStmtId,
    /// The failure path for a value whose evaluation reached code checking
    /// rejected.
    checked_crash: LIR.CFStmtId,
    entry: LIR.CFStmtId,
    success: LIR.CFStmtId,
    value_slot: LIR.StaticDataId,
};

/// Prefix of a datum named by content, the same in every program:
/// `roc__h{hash}`, for a string literal's backing or a constant an
/// object-cache pack carries.
pub const content_data_symbol_prefix = "roc__h";

/// Symbol of the value in static-data slot `id`: `roc__d{id}`. The naming
/// scheme is in design.md, "Object Symbol Names".
pub fn staticDataSymbolName(allocator: Allocator, id: LIR.StaticDataId) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__d{d}", .{@intFromEnum(id)});
}

/// Symbol of the `index`th further node (from 1) of the value owner `owner`
/// holds: `roc__d{owner}_{index}`. An owner below the program's static-data
/// slot count is that slot; see design.md, "Object Symbol Names", for the
/// owners past it.
pub fn staticDataNodeSymbolName(allocator: Allocator, owner: u32, index: u32) Allocator.Error![]u8 {
    std.debug.assert(index != 0);
    return try std.fmt.allocPrint(allocator, "roc__d{d}_{d}", .{ owner, index });
}

/// Complete LIR program and side data consumed by ARC, backends, and eval.
/// A template specialization's content key and the procedure lowered for it.
pub const SpecProc = struct {
    key: [32]u8,
    proc: LIR.LirProcSpecId,
};

/// Everything one lowering produced: the procedure store, its layouts, the
/// root and specialization tables, and the static data the program carries.
pub const Result = struct {
    store: LirStore,
    layouts: layout.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    /// The procedure lowered for each template specialization, by the
    /// specialization's content key (`Monotype.Ast.specIdentityKey`). Only
    /// the specialization's own procedure is listed, never SpecConstr clones
    /// or capture-bearing variants of it.
    spec_procs: std.ArrayList(SpecProc),
    root_metadata: std.ArrayList(root.RootMetadata),
    requested_layouts: std.ArrayList(RequestedLayout),
    const_types: const_store.ConstTypeStore,
    const_type_names: names.NameStore,
    fn_sets: std.ArrayList(FnSet),
    erased_fns: std.ArrayList(ErasedFns),
    /// Bytes every erased callable value of this program reserves at the
    /// start of its capture: the dev shim's hot-reload header when the
    /// program runs under hot reload, and none otherwise. Workers read their
    /// captures after it, so a value frozen into static data reserves it too.
    erased_capture_prefix: u32 = 0,
    boxy_type_descs: std.ArrayList(BoxyTypeDesc),
    boxy_dicts: std.ArrayList(BoxyDict),
    /// Selected implementation behind each Boxy dictionary boundary adapter.
    /// Compiler-only evidence used when freezing captured dictionaries.
    boxy_frozen_method_origins: std.AutoHashMapUnmanaged(LIR.LirProcSpecId, BoxyFrozenMethodOrigin) = .empty,
    boxy_adapters: std.ArrayList(BoxyAdapter),
    boxy_desc_refs: std.ArrayList(BoxyDescRef),
    boxy_dict_refs: std.ArrayList(BoxyDictRef),
    boxy_tag_variants: std.ArrayList(BoxyTagVariant),
    boxy_tag_payload_descs: std.ArrayList(BoxyTagPayloadDesc),
    boxy_field_names: std.ArrayList(LIR.BoxyNameId),
    boxy_adapt_steps: std.ArrayList(BoxyAdaptStep),
    boxy_payload_steps: std.ArrayList(BoxyPayloadStep),
    boxy_method_slots: std.ArrayList(BoxyMethodSlot),
    /// Dense, deduplicated proc ids named by live non-structural Boxy method
    /// slots. Machine backends consume this exact list when emitting the
    /// uniform worker thunks used by the Boxy runtime.
    boxy_worker_procs: std.ArrayList(LIR.LirProcSpecId),
    boxy_method_arg_layouts: std.ArrayList(layout.Idx),
    boxy_method_hidden_desc_sources: std.ArrayList(BoxyMethodHiddenDescSource),
    boxy_erased_arg_layouts: std.ArrayList(layout.Idx),
    boxy_erased_arg_desc_keys: std.ArrayList(LIR.ErasedArgDescKey),
    boxy_erased_arg_desc_offsets: std.ArrayList(LIR.ErasedArgDescOffset),
    boxy_erased_arg_desc_params: std.ArrayList(LIR.ErasedArgDescParam),
    const_plans: std.ArrayList(ConstPlan),
    const_roots: std.ArrayList(ConstRootPlan),
    literal_roots: std.ArrayList(LiteralRootPlan),
    static_data_values: std.ArrayList(StaticDataValue),
    comptime_value_guards: std.ArrayList(ComptimeValueGuard),
    comptime_sites: std.ArrayList(LIR.ComptimeSite),
    /// Checked modules of the lowering this program came from, addressed by
    /// `LIR.LoweringModuleId`. Rows that retain a module-local checked id
    /// name their owner through this table; nothing resolves an owner from
    /// row order or from the procedure a row ended up in.
    lowering_modules: std.ArrayList(checked.ModuleId),
    expect_sites: std.ArrayList(LIR.ExpectSite),
    expect_site_ids: std.AutoHashMapUnmanaged(ExpectSiteKey, LIR.ExpectSiteId),

    pub fn init(allocator: Allocator, target_usize: @import("base").target.TargetUsize) Allocator.Error!Result {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout.Store.init(allocator, target_usize),
            .root_procs = .empty,
            .spec_procs = .empty,
            .root_metadata = .empty,
            .requested_layouts = .empty,
            .const_types = const_store.ConstTypeStore.init(allocator),
            .const_type_names = names.NameStore.init(allocator),
            .fn_sets = .empty,
            .erased_fns = .empty,
            .boxy_type_descs = .empty,
            .boxy_dicts = .empty,
            .boxy_adapters = .empty,
            .boxy_desc_refs = .empty,
            .boxy_dict_refs = .empty,
            .boxy_tag_variants = .empty,
            .boxy_tag_payload_descs = .empty,
            .boxy_field_names = .empty,
            .boxy_adapt_steps = .empty,
            .boxy_payload_steps = .empty,
            .boxy_method_slots = .empty,
            .boxy_worker_procs = .empty,
            .boxy_method_arg_layouts = .empty,
            .boxy_method_hidden_desc_sources = .empty,
            .boxy_erased_arg_layouts = .empty,
            .boxy_erased_arg_desc_keys = .empty,
            .boxy_erased_arg_desc_offsets = .empty,
            .boxy_erased_arg_desc_params = .empty,
            .const_plans = .empty,
            .const_roots = .empty,
            .literal_roots = .empty,
            .static_data_values = .empty,
            .comptime_value_guards = .empty,
            .comptime_sites = .empty,
            .lowering_modules = .empty,
            .expect_sites = .empty,
            .expect_site_ids = .empty,
        };
    }

    pub fn deinit(self: *Result) void {
        const allocator = self.store.allocator;
        for (self.comptime_sites.items) |site| {
            allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(allocator);
        self.lowering_modules.deinit(allocator);
        self.expect_site_ids.deinit(allocator);
        self.expect_sites.deinit(allocator);
        self.static_data_values.deinit(allocator);
        self.comptime_value_guards.deinit(allocator);
        deinitConstPlans(allocator, self.const_plans.items);
        self.const_roots.deinit(allocator);
        self.literal_roots.deinit(allocator);
        self.const_plans.deinit(allocator);
        deinitFnSets(allocator, self.fn_sets.items);
        deinitErasedFns(allocator, self.erased_fns.items);
        self.boxy_erased_arg_desc_params.deinit(allocator);
        self.boxy_erased_arg_desc_offsets.deinit(allocator);
        self.boxy_erased_arg_desc_keys.deinit(allocator);
        self.boxy_erased_arg_layouts.deinit(allocator);
        self.boxy_method_hidden_desc_sources.deinit(allocator);
        self.boxy_method_arg_layouts.deinit(allocator);
        self.boxy_worker_procs.deinit(allocator);
        self.boxy_method_slots.deinit(allocator);
        self.boxy_payload_steps.deinit(allocator);
        self.boxy_adapt_steps.deinit(allocator);
        self.boxy_field_names.deinit(allocator);
        self.boxy_tag_payload_descs.deinit(allocator);
        self.boxy_tag_variants.deinit(allocator);
        self.boxy_dict_refs.deinit(allocator);
        self.boxy_desc_refs.deinit(allocator);
        self.boxy_adapters.deinit(allocator);
        self.boxy_dicts.deinit(allocator);
        self.boxy_frozen_method_origins.deinit(allocator);
        self.boxy_type_descs.deinit(allocator);
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
        self.const_type_names.deinit();
        self.const_types.deinit();
        self.requested_layouts.deinit(allocator);
        self.root_metadata.deinit(allocator);
        self.root_procs.deinit(allocator);
        self.spec_procs.deinit(allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    pub fn requestedLayoutForType(self: *const Result, ty: names.TypeDigest) ?layout.Idx {
        for (self.requested_layouts.items) |entry| {
            if (std.mem.eql(u8, entry.ty.bytes[0..], ty.bytes[0..])) return entry.layout_idx;
        }
        return null;
    }

    pub fn addComptimeSite(
        self: *Result,
        kind: LIR.ComptimeSiteKind,
        owner: LIR.LoweringModuleId,
        region: base.Region,
        checked_site: ?LIR.CheckedExhaustivenessSiteId,
        proc: LIR.LirProcSpecId,
        branch_regions: []const base.Region,
    ) Allocator.Error!LIR.ComptimeSiteId {
        const owned_branch_regions = try self.store.allocator.dupe(base.Region, branch_regions);
        errdefer self.store.allocator.free(owned_branch_regions);
        const id: LIR.ComptimeSiteId = @enumFromInt(@as(u32, @intCast(self.comptime_sites.items.len)));
        try self.comptime_sites.append(self.store.allocator, .{
            .kind = kind,
            .owner = owner,
            .region = region,
            .checked_site = checked_site,
            .proc = proc,
            .branch_regions = owned_branch_regions,
        });
        return id;
    }

    /// Publish the lowering's checked module table. The producer writes it
    /// once, before any consumer resolves an owner out of it.
    pub fn setLoweringModules(self: *Result, modules: []const checked.ModuleId) Allocator.Error!void {
        self.lowering_modules.clearRetainingCapacity();
        try self.lowering_modules.appendSlice(self.store.allocator, modules);
    }

    /// The checked module a `LIR.LoweringModuleId` names.
    pub fn loweringModuleKey(self: *const Result, id: LIR.LoweringModuleId) checked.ModuleId {
        const raw = @intFromEnum(id);
        if (raw >= self.lowering_modules.items.len) {
            base.invariant("{s}", .{"LIR program invariant violated: lowering module id has no published checked module"});
        }
        return self.lowering_modules.items[raw];
    }

    /// The dense id this program gave a checked module, when the module was
    /// part of its lowering input.
    pub fn loweringModuleId(self: *const Result, key: checked.ModuleId) ?LIR.LoweringModuleId {
        for (self.lowering_modules.items, 0..) |candidate, index| {
            if (std.mem.eql(u8, &candidate.bytes, &key.bytes)) return @enumFromInt(@as(u32, @intCast(index)));
        }
        return null;
    }

    /// Intern one source `expect` so generated code can use a dense counter.
    pub fn addExpectSite(self: *Result, loc: base.SourceLoc, region: base.Region) Allocator.Error!LIR.ExpectSiteId {
        const key = ExpectSiteKey.init(loc, region);
        const entry = try self.expect_site_ids.getOrPut(self.store.allocator, key);
        if (entry.found_existing) return entry.value_ptr.*;
        errdefer _ = self.expect_site_ids.remove(key);
        const id: LIR.ExpectSiteId = @enumFromInt(@as(u32, @intCast(self.expect_sites.items.len)));
        try self.expect_sites.append(self.store.allocator, .{ .loc = loc, .region = region });
        entry.value_ptr.* = id;
        return id;
    }

    pub fn findExpectSite(self: *const Result, loc: base.SourceLoc, region: base.Region) ?LIR.ExpectSiteId {
        return self.expect_site_ids.get(ExpectSiteKey.init(loc, region));
    }

    /// Classify every static Boxy descriptor by the runtime context its
    /// reachable references read. Runs once, after lowering has produced
    /// every descriptor.
    pub fn classifyBoxyDescClosures(self: *Result, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
        const descs = self.boxy_type_descs.items;
        if (descs.len == 0) return;

        // Reverse edges (child -> parents) in compressed rows, so a class
        // propagates from each descriptor to everything that reaches it.
        const edge_starts = try allocator.alloc(u32, descs.len + 1);
        defer allocator.free(edge_starts);
        @memset(edge_starts, 0);
        for (descs) |desc| {
            var refs = self.boxyDescRefIterator(desc);
            while (refs.next()) |ref| switch (ref) {
                .static => |child| edge_starts[@intFromEnum(child) + 1] += 1,
                .local, .runtime, .dict_method_arg, .dict_method_hidden => {},
            };
        }
        for (1..edge_starts.len) |index| edge_starts[index] += edge_starts[index - 1];
        const parents = try allocator.alloc(u32, edge_starts[descs.len]);
        defer allocator.free(parents);
        const fill = try allocator.dupe(u32, edge_starts[0..descs.len]);
        defer allocator.free(fill);

        var worklist = std.ArrayList(u32).empty;
        defer worklist.deinit(allocator);
        for (descs, 0..) |*desc, desc_index| {
            var class: BoxyDescClosure = .closed;
            var refs = self.boxyDescRefIterator(desc.*);
            while (refs.next()) |ref| switch (ref) {
                .static => |child| {
                    parents[fill[@intFromEnum(child)]] = @intCast(desc_index);
                    fill[@intFromEnum(child)] += 1;
                },
                .local => class = @enumFromInt(@max(@intFromEnum(class), @intFromEnum(BoxyDescClosure.captures))),
                .runtime, .dict_method_arg, .dict_method_hidden => class = .context,
            };
            desc.closure = class;
            if (class != .closed) try worklist.append(allocator, @intCast(desc_index));
        }

        while (worklist.pop()) |child| {
            const class = descs[child].closure;
            for (parents[edge_starts[child]..edge_starts[child + 1]]) |parent| {
                if (@intFromEnum(descs[parent].closure) >= @intFromEnum(class)) continue;
                descs[parent].closure = class;
                try worklist.append(allocator, parent);
            }
        }
    }

    /// Every descriptor reference `desc` holds directly.
    fn boxyDescRefIterator(self: *const Result, desc: BoxyTypeDesc) BoxyDescRefIterator {
        return .{ .result = self, .desc = desc };
    }

    const BoxyDescRefIterator = struct {
        result: *const Result,
        desc: BoxyTypeDesc,
        section: u8 = 0,
        index: u32 = 0,
        variant: u32 = 0,

        fn next(self: *BoxyDescRefIterator) ?BoxyDescRef {
            const r = self.result;
            while (true) {
                switch (self.section) {
                    0 => if (spanRef(r.boxy_desc_refs.items, self.desc.nested_descs, &self.index)) |ref| return ref,
                    1 => if (spanRef(r.boxy_desc_refs.items, self.desc.inspect_hidden_descs, &self.index)) |ref| return ref,
                    2 => if (spanRef(r.boxy_desc_refs.items, self.desc.inspect_arg_descs, &self.index)) |ref| return ref,
                    3 => if (self.index == 0) {
                        self.index = 1;
                        if (self.desc.tag_ext_desc) |ref| return ref;
                    },
                    4 => while (self.variant < self.desc.tag_variants.len) {
                        const variant = r.boxy_tag_variants.items[self.desc.tag_variants.start + self.variant];
                        if (self.index < variant.payload_descs.len) {
                            self.index += 1;
                            return r.boxy_tag_payload_descs.items[variant.payload_descs.start + self.index - 1].desc;
                        }
                        self.variant += 1;
                        self.index = 0;
                    },
                    5, 6 => {
                        const plan = if (self.section == 5) self.desc.copy_plan else self.desc.drop_plan;
                        while (self.index < plan.len) {
                            self.index += 1;
                            switch (r.boxy_payload_steps.items[plan.start + self.index - 1]) {
                                .dynamic => |step| return step.desc,
                                .concrete => {},
                            }
                        }
                    },
                    7 => if (spanRef(r.boxy_desc_refs.items, self.desc.eq_hidden_descs, &self.index)) |ref| return ref,
                    8 => if (spanRef(r.boxy_desc_refs.items, self.desc.eq_arg_descs, &self.index)) |ref| return ref,
                    9 => if (spanRef(r.boxy_desc_refs.items, self.desc.hash_hidden_descs, &self.index)) |ref| return ref,
                    10 => if (spanRef(r.boxy_desc_refs.items, self.desc.hash_arg_descs, &self.index)) |ref| return ref,
                    else => return null,
                }
                self.section += 1;
                self.index = 0;
            }
        }

        fn spanRef(refs: []const BoxyDescRef, span: BoxySpan, index: *u32) ?BoxyDescRef {
            if (index.* >= span.len) return null;
            index.* += 1;
            return refs[span.start + index.* - 1];
        }
    };

    /// Discard the lowering-only source lookup once every statement has its
    /// dense id. Runtime consumers need only `expect_sites`.
    pub fn finishExpectSites(self: *Result) void {
        self.expect_site_ids.deinit(self.store.allocator);
        self.expect_site_ids = .empty;
    }
};

/// Free slices owned by constant storage plans.
pub fn deinitConstPlans(allocator: Allocator, plans: []const ConstPlan) void {
    for (plans) |plan| {
        switch (plan) {
            .tuple => |items| allocator.free(items),
            .record => |fields| allocator.free(fields),
            .tag_union => |variants| {
                for (variants) |variant| {
                    allocator.free(variant.name);
                    allocator.free(variant.payloads);
                }
                allocator.free(variants);
            },
            .zst,
            .boxy_box,
            .layout_only,
            .pending,
            .scalar,
            .str,
            .list,
            .box,
            .named,
            => {},
            .fn_value,
            .erased_fn,
            => {},
        }
    }
}

/// Free slices owned by finite callable sets.
pub fn deinitFnSets(allocator: Allocator, fn_sets: []const FnSet) void {
    for (fn_sets) |fn_set| {
        for (fn_set.variants) |variant| {
            if (variant.captures.len > 0) allocator.free(variant.captures);
            if (variant.template.evidence.len > 0) allocator.free(variant.template.evidence);
            if (variant.template.evidence_frames.len > 0) allocator.free(variant.template.evidence_frames);
        }
        if (fn_set.variants.len > 0) allocator.free(fn_set.variants);
    }
}

/// Free slices owned by erased callable entry sets.
pub fn deinitErasedFns(allocator: Allocator, erased_fns: []const ErasedFns) void {
    for (erased_fns) |set| {
        for (set.entries) |entry| {
            if (entry.captures.len > 0) allocator.free(entry.captures);
            if (entry.template) |template| {
                if (template.evidence.len > 0) allocator.free(template.evidence);
                if (template.evidence_frames.len > 0) allocator.free(template.evidence_frames);
            }
            if (entry.boxy) |boxy| allocator.free(boxy.captures);
        }
        if (set.entries.len > 0) allocator.free(set.entries);
    }
}

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

test "lowering module table resolves checked module provenance both ways" {
    const allocator = std.testing.allocator;
    var result = try Result.init(allocator, .u64);
    defer result.deinit();

    var keys: [3]checked.ModuleId = .{ .{}, .{}, .{} };
    keys[0].bytes[0] = 7;
    keys[1].bytes[0] = 8;
    keys[2].bytes[0] = 9;
    try result.setLoweringModules(&keys);

    for (keys, 0..) |key, index| {
        const id: LIR.LoweringModuleId = @enumFromInt(@as(u32, @intCast(index)));
        try std.testing.expectEqualSlices(u8, &key.bytes, &result.loweringModuleKey(id).bytes);
        try std.testing.expectEqual(id, result.loweringModuleId(key).?);
    }

    var absent: checked.ModuleId = .{};
    absent.bytes[0] = 10;
    try std.testing.expectEqual(@as(?LIR.LoweringModuleId, null), result.loweringModuleId(absent));

    // A site keeps the owner its producer recorded, not the procedure's owner.
    const owner: LIR.LoweringModuleId = @enumFromInt(2);
    const site = try result.addComptimeSite(.destructure, owner, base.Region.zero(), @enumFromInt(41), .first, &.{});
    const stored = result.comptime_sites.items[@intFromEnum(site)];
    try std.testing.expectEqual(owner, stored.owner);
    try std.testing.expectEqual(@as(?LIR.CheckedExhaustivenessSiteId, @enumFromInt(41)), stored.checked_site);
    try std.testing.expectEqualSlices(u8, &keys[2].bytes, &result.loweringModuleKey(stored.owner).bytes);
}

test "boxy side tables initialize empty and use flat pools" {
    const allocator = std.testing.allocator;
    var result = try Result.init(allocator, .u64);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 0), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_dicts.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_adapters.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_desc_refs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_dict_refs.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_adapt_steps.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_payload_steps.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_slots.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_arg_layouts.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.boxy_method_hidden_desc_sources.items.len);

    const desc_refs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const desc_refs = BoxySpan{ .start = @intCast(desc_refs_start), .len = 1 };

    const copy_plan_start = result.boxy_payload_steps.items.len;
    try result.boxy_payload_steps.append(allocator, .{ .dynamic = .{
        .op = .copy,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    } });
    const copy_plan = BoxySpan{ .start = @intCast(copy_plan_start), .len = 1 };

    const drop_plan_start = result.boxy_payload_steps.items.len;
    try result.boxy_payload_steps.append(allocator, .{ .concrete = .{
        .op = .drop,
        .layout_idx = .zst,
    } });
    const drop_plan = BoxySpan{ .start = @intCast(drop_plan_start), .len = 1 };

    try result.boxy_type_descs.append(allocator, .{
        .payload_layout = .zst,
        .contains_refcounted = true,
        .shape = .internal,
        .nested_descs = desc_refs,
        .copy_plan = copy_plan,
        .drop_plan = drop_plan,
    });

    const arg_layouts_start = result.boxy_method_arg_layouts.items.len;
    try result.boxy_method_arg_layouts.append(allocator, .zst);
    const arg_layouts = BoxySpan{ .start = @intCast(arg_layouts_start), .len = 1 };

    const arg_descs_start = result.boxy_desc_refs.items.len;
    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const arg_descs = BoxySpan{ .start = @intCast(arg_descs_start), .len = 1 };

    const nested_dicts_start = result.boxy_dict_refs.items.len;
    try result.boxy_dict_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });
    const nested_dicts = BoxySpan{ .start = @intCast(nested_dicts_start), .len = 1 };

    const hidden_desc_sources_start = result.boxy_method_hidden_desc_sources.items.len;
    try result.boxy_method_hidden_desc_sources.append(allocator, .{ .slot = 0 });
    const hidden_desc_sources = BoxySpan{ .start = @intCast(hidden_desc_sources_start), .len = 1 };

    const method_slots_start = result.boxy_method_slots.items.len;
    try result.boxy_method_slots.append(allocator, .{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(fixtureTableIndex(0)),
        .adapter = .{
            .arg_layouts = arg_layouts,
            .arg_descs = arg_descs,
            .nested_dicts = nested_dicts,
            .hidden_desc_sources = hidden_desc_sources,
        },
    });
    const method_slots = BoxySpan{ .start = @intCast(method_slots_start), .len = 1 };

    try result.boxy_desc_refs.append(allocator, .{ .static = @enumFromInt(fixtureTableIndex(0)) });

    try result.boxy_dicts.append(allocator, .{
        .method_slots = method_slots,
    });

    const adapt_steps_start = result.boxy_adapt_steps.items.len;
    try result.boxy_adapt_steps.append(allocator, .{ .dynamic_payload = .{
        .source_offset = 0,
        .target_offset = 8,
        .source_desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .target_desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .mode = .copy,
    } });
    const adapt_steps = BoxySpan{ .start = @intCast(adapt_steps_start), .len = 1 };

    try result.boxy_adapters.append(allocator, .{
        .kind = .boxy_to_host,
        .source_layout = .str,
        .target_layout = .str,
        .steps = adapt_steps,
        .consumes_source = false,
        .produces_owned_result = true,
    });

    try std.testing.expectEqual(@as(usize, 1), result.boxy_type_descs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_dicts.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_adapters.items.len);
    try std.testing.expectEqual(@as(usize, 3), result.boxy_desc_refs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_dict_refs.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_adapt_steps.items.len);
    try std.testing.expectEqual(@as(usize, 2), result.boxy_payload_steps.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_slots.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_arg_layouts.items.len);
    try std.testing.expectEqual(@as(usize, 1), result.boxy_method_hidden_desc_sources.items.len);
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
