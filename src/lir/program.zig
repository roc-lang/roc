//! LIR program result shared by post-check lowering, ARC, LirImage, glue, and
//! interpreter consumers.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");

const LIR = @import("LIR.zig");
const collections = @import("collections");
const LirStore = @import("LirStore.zig");
const root = @import("root_metadata.zig");

const Allocator = std.mem.Allocator;
const names = check.CheckedNames;
const checked = check.CheckedModule;
const const_store = check.ConstStore;

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
    /// Named by the digest of its content and defined weak: every unit of a
    /// program that carries this constant defines the same symbol, and a
    /// link keeps one definition for all of them. Never set with
    /// `is_exported`.
    content_named: bool = false,
    /// A stored constant's image, named by its digest
    /// (`poolImageSymbolName`) rather than by the program: every program
    /// holding the image names it so, and a unit image leaves it to the
    /// constant pool (design.md "Constant Pool"). Never set with
    /// `is_exported`.
    pool_image: bool = false,
    /// Pointer relocations from this symbol's bytes to other symbols.
    relocations: []const StaticDataRelocation = &.{},
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
    callable_capture_offset: collections.OptionalU32 = .none,
    /// Exact LIR procedure named by an erased-callable function relocation.
    ///
    /// In-process consumers use this identity directly; object backends use
    /// `target_symbol_name` as its linker representation.
    procedure: collections.OptionalId(LIR.LirProcSpecId) = .none,
    /// Exact generated RC helper required by this function-pointer relocation.
    ///
    /// Static erased-callable `on_drop` slots are always atomic: their
    /// construction site makes no thread-confinement claim. Backends consume
    /// this identity directly instead of recovering it from a symbol or layout.
    rc_helper: ?layout.RcHelperKey = null,
    /// Whether `target_symbol_name` is owned by this relocation.
    owns_target_symbol_name: bool = false,
};

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(StaticDataRelocation) == 64);
}

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
    initializer: collections.OptionalId(LIR.LirProcSpecId) = .none,
};

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(RequestedLayout) == 104);
}

/// Identifier for a finite callable set in the LIR program.
pub const FnSetId = enum(u32) { _ };
/// Identifier for an erased callable entry set in the LIR program.
pub const ErasedFnsId = enum(u32) { _ };
/// Identifier for one finite callable variant.
pub const FnVariantId = enum(u32) { _ };

/// Exact member context in the common target-independent Lambda Solved graph.
/// Own captures belong to `source`; solved captures name their producer span.
pub const FrozenCallableContext = struct {
    abi: enum { finite, erased },
    /// The function at its instance (`Instances.SpecFnId`).
    source: u32,
    fn_type: u32,
    /// The instance `fn_type` reads under.
    fn_type_env: u32 = 0,
    captures: union(enum) {
        own: u32,
        solved: struct { start: u32, len: u32, env: u32 = 0 },
    },
};

/// Checked function template and source type used to emit callable code.
pub const FnTemplate = struct {
    /// Original function slot in the shared frozen Monotype owner.
    frozen_fn: collections.OptionalU32 = .none,
    /// Exact callable worker specialization key, emitted by SpecConstr:
    /// SHA-256 template, callable-ABI, and capture-ABI digests in that order.
    frozen_worker: ?[96]u8 = null,
    /// Exact member specialization within the shared frozen Solved owner.
    frozen_context: ?FrozenCallableContext = null,
    fn_def: const_store.FnDef,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    owner_fn_ty: collections.OptionalId(const_store.ConstTypeId) = .none,
    evidence: const_store.ConstFnEvidenceVector = .{},
    evidence_frames: []const const_store.ConstFnEvidenceFrame = &.{},
    evidence_frame_head: collections.OptionalU32 = .none,
};

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(FnTemplate) == 400);
}

/// Capture field copied from a checked binding into a callable payload. `id`
/// is checked-stage provenance for storing a compile-time result in
/// `ConstStore`; runtime capture joining was completed before LIR.
pub const CaptureSlot = struct {
    id: const_store.CaptureId,
    /// Whether this slot holds a lexical capture or a top-level root's
    /// recursive binding, carried from the captured Monotype local.
    kind: const_store.ConstCaptureKind,
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

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(FnVariant) == 432);
}

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
};

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(ErasedFn) == 448);
}

/// Runtime encoding for an erased callable value type.
pub const ErasedFns = struct {
    layout: layout.Idx,
    entries: []const ErasedFn = &.{},
};

/// Identifier for a constant storage plan emitted with LIR.
pub const ConstPlanId = enum(u32) { _ };

const ExpectSiteKey = struct {
    file: collections.OptionalU32,
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
    tuple: []const ConstPlanId,
    record: []const ConstPlanId,
    tag_union: []const ConstTagVariant,
    named: struct {
        named_type: check.CheckedModule.ConstNamedType,
        backing: ConstPlanId,
    },
    fn_value: FnSetId,
    erased_fn: ErasedFnsId,
    /// A function value of a `.boxy` compile-time program, stored by what
    /// it captured (`postcheck.Boxy.StoredFn`).
    boxy_fn,
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
    /// The shape of `ret_type` when it is not the evaluated value's: a
    /// Solved program's root is stored at its declaring module's rows, which
    /// rows the program joined can widen past. An image of the root is
    /// written at this shape, each tag at its variant of the same name.
    stored: ?RootShape = null,
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
    /// The Monotype representation of the converted value, in
    /// `Result.const_types`: the concrete type this root converts the
    /// literal at, which a `.boxy` runtime program keys the value by.
    ret_type: const_store.ConstTypeId,
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
    first_comptime_guard: collections.OptionalU32 = .none,
    /// Null when completed frozen data, a stored node, or a capture-free
    /// callable supplies this slot directly.
    initializer: collections.OptionalId(LIR.LirProcSpecId),
    layout_idx: layout.Idx,
    /// The completed ConstStore node frozen directly at its explicit layout
    /// and storage plan, without an initializer procedure.
    stored: ?StoredStaticValue = null,
    /// The capture-free erased callable of this procedure, which needs no
    /// initializer: its value is the callable's payload (the function
    /// pointer, no `on_drop`, no capture bytes) with the static refcount.
    callable: collections.OptionalId(LIR.LirProcSpecId) = .none,
    /// Final constant data assembled by the compiler, never runtime LIR.
    parts: ?StaticDataParts = null,
    /// Another physical representation of this canonical completed root.
    /// It shares the producer and failure status, and is filled by the data
    /// writer, never by runtime conversion or allocation instructions.
    representation_of: ?struct { slot: LIR.StaticDataId, storage: CaptureSlotStorage } = null,
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

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(StaticDataValue) == 240);
}

/// Explicit edges and representation of a completed constant. Children name
/// immutable data slots; the target-data writer connects their bytes and
/// relocations without generating a procedure that constructs the value.
pub const StaticDataParts = struct {
    kind: union(enum) {
        word: u64,
        product,
        /// A list of the children, with room for this many items: the
        /// capacity the list was evaluated with, never fewer than the
        /// children.
        list: u64,
        box,
        tag: u32,
        callable: struct {
            proc: LIR.LirProcSpecId,
            on_drop: LIR.ErasedCallableOnDrop = .none,
        },
    },
    children: []LIR.StaticDataId,
};

/// A list stored in a checked module's `ConstStore`, which static data
/// freezes at its value's layout through `plan`, the const plan of the
/// value's type. Nothing it reaches is a function value, so the plan names no
/// procedure.
pub const StoredStaticValue = struct {
    module: checked.ModuleId,
    node: checked.ConstNodeId,
    plan: ConstPlanId,
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
    next_for_slot: collections.OptionalU32 = .none,
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

comptime {
    if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(ComptimeValueGuard) == 32);
}

/// The namespace of every symbol the compiler invents for generated code or
/// data. Platforms and hosts share the `roc_` interface (one underscore) with
/// Roc; nothing but the compiler defines a name in this one.
pub const compiler_symbol_namespace = "roc__";

/// Prefix of a datum named by content, the same in every program:
/// `roc__h{hash}`, for a string literal's backing or a constant an
/// object-cache pack carries.
pub const content_data_symbol_prefix = compiler_symbol_namespace ++ "h";

/// Prefix of a stored constant's image named by its digest:
/// `roc__i{digest}`, the digest in lowercase hex (design.md "Constant
/// Pool"). A unit image references such an image as a pool import, which a
/// link resolves to the one copy it places.
pub const pool_image_symbol_prefix = compiler_symbol_namespace ++ "i";

/// The symbol of the stored image whose digest is `digest`.
pub fn poolImageSymbolName(allocator: Allocator, digest: [32]u8) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, pool_image_symbol_prefix ++ "{s}", .{&std.fmt.bytesToHex(digest, .lower)});
}

/// Symbol of the value in static-data slot `id`: `roc__d{id}`. The naming
/// scheme is in design.md, "Object Symbol Names".
pub fn staticDataSymbolName(allocator: Allocator, id: LIR.StaticDataId) Allocator.Error![]u8 {
    return try std.fmt.allocPrint(allocator, "roc__d{d}", .{@backingInt(id)});
}

/// Symbol of the `index`th further node (from 1) of the value owner `owner`
/// holds: `roc__d{owner}_{index}`. An owner below the program's static-data
/// slot count is that slot; see design.md, "Object Symbol Names", for the
/// owners past it.
pub fn staticDataNodeSymbolName(allocator: Allocator, owner: u32, index: u32) Allocator.Error![]u8 {
    std.debug.assert(index != 0);
    return try std.fmt.allocPrint(allocator, "roc__d{d}_{d}", .{ owner, index });
}

/// What a `.boxy` unit's export record says of a worker beyond the
/// interface ARC solved for it (design.md "Caching Under `.boxy`").
pub const BoxyExport = struct {
    /// For a template's worker, generic or integer list: the template's
    /// position in its module's checked table, which a needs list naming
    /// the worker by identity resolves it to in the module's current
    /// checked module. Absent for a worker that is no template's.
    template: collections.OptionalU32 = .none,
    /// Whether another unit can read the record: only these records are in
    /// the module's export digest, which its importers' keys fold in.
    visible: bool = false,
};

/// Everything one lowering produced: the procedure store, its layouts, the
/// root and specialization tables, and the static data the program carries.
pub const Result = struct {
    store: LirStore,
    layouts: layout.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId) = .empty,
    /// The procedures this program defines for other objects of its link
    /// (`LIR.LirProcSpec.exported`), roots of reachability and ARC beside
    /// `root_procs` (design.md "Per-Module Output").
    exported_procs: std.ArrayList(LIR.LirProcSpecId) = .empty,
    /// The encoded needs list of each exported `.boxy` worker that takes a
    /// type table, by its procedure's identity: what an importer fills the
    /// worker's tables from (`postcheck.Boxy.Tables`).
    boxy_table_needs: std.AutoHashMapUnmanaged(LIR.ProcIdentity, []const u8) = .empty,
    /// What an importer resolves each exported `.boxy` worker to and whether
    /// it is importer-visible, by its procedure's identity
    /// (design.md "Caching Under `.boxy`").
    boxy_exports: std.AutoHashMapUnmanaged(LIR.ProcIdentity, BoxyExport) = .empty,
    root_metadata: std.ArrayList(root.RootMetadata) = .empty,
    requested_layouts: std.ArrayList(RequestedLayout) = .empty,
    const_types: const_store.ConstTypeStore,
    const_type_names: names.NameStore,
    fn_sets: std.ArrayList(FnSet) = .empty,
    erased_fns: std.ArrayList(ErasedFns) = .empty,
    /// Bytes every erased callable value of this program reserves at the
    /// start of its capture: the dev shim's hot-reload header when the
    /// program runs under hot reload, and none otherwise. Workers read their
    /// captures after it, so a value frozen into static data reserves it too.
    erased_capture_prefix: u32 = 0,
    const_plans: std.ArrayList(ConstPlan) = .empty,
    const_roots: std.ArrayList(ConstRootPlan) = .empty,
    literal_roots: std.ArrayList(LiteralRootPlan) = .empty,
    static_data_values: std.ArrayList(StaticDataValue) = .empty,
    comptime_value_guards: std.ArrayList(ComptimeValueGuard) = .empty,
    comptime_sites: std.ArrayList(LIR.ComptimeSite) = .empty,
    /// Checked modules of the lowering this program came from, addressed by
    /// `LIR.LoweringModuleId`. Rows that retain a module-local checked id
    /// name their owner through this table; nothing resolves an owner from
    /// row order or from the procedure a row ended up in.
    lowering_modules: std.ArrayList(checked.ModuleId) = .empty,
    /// Each lowering module by the identity this program's failure meanings
    /// name it by (`LirStore.lowering_module_identities`), so a consumer
    /// reads a meaning in checked keys by one lookup per module it names.
    modules_by_failure_identity: std.AutoHashMapUnmanaged([32]u8, LIR.LoweringModuleId) = .empty,
    expect_sites: std.ArrayList(LIR.ExpectSite) = .empty,
    expect_site_ids: std.AutoHashMapUnmanaged(ExpectSiteKey, LIR.ExpectSiteId) = .empty,

    pub fn init(allocator: Allocator, target_usize: @import("base").target.TargetUsize) Allocator.Error!Result {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout.Store.init(allocator, target_usize),
            .const_types = const_store.ConstTypeStore.init(allocator),
            .const_type_names = names.NameStore.init(allocator),
        };
    }

    pub fn deinit(self: *Result) void {
        const allocator = self.store.allocator;
        self.comptime_sites.deinit(allocator);
        self.lowering_modules.deinit(allocator);
        self.modules_by_failure_identity.deinit(allocator);
        self.expect_site_ids.deinit(allocator);
        self.expect_sites.deinit(allocator);
        for (self.static_data_values.items) |value| if (value.parts) |parts| allocator.free(parts.children);
        self.static_data_values.deinit(allocator);
        self.comptime_value_guards.deinit(allocator);
        deinitConstPlans(allocator, self.const_plans.items);
        self.const_roots.deinit(allocator);
        self.literal_roots.deinit(allocator);
        self.const_plans.deinit(allocator);
        deinitFnSets(allocator, self.fn_sets.items);
        deinitErasedFns(allocator, self.erased_fns.items);
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
        self.const_type_names.deinit();
        self.const_types.deinit();
        self.requested_layouts.deinit(allocator);
        self.root_metadata.deinit(allocator);
        self.root_procs.deinit(allocator);
        self.exported_procs.deinit(allocator);
        var needs = self.boxy_table_needs.valueIterator();
        while (needs.next()) |bytes| allocator.free(bytes.*);
        self.boxy_table_needs.deinit(allocator);
        self.boxy_exports.deinit(allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    pub fn addComptimeSite(
        self: *Result,
        kind: LIR.ComptimeSiteKind,
        owner: LIR.LoweringModuleId,
        region: base.Region,
        checked_site: ?LIR.CheckedExhaustivenessSiteId,
        proc: LIR.LirProcSpecId,
    ) Allocator.Error!LIR.ComptimeSiteId {
        const id: LIR.ComptimeSiteId = @fromBackingInt(@intCast(@as(u32, @intCast(self.comptime_sites.items.len))));
        try self.comptime_sites.append(self.store.allocator, .{
            .kind = kind,
            .owner = owner,
            .region = region,
            .checked_site = .of(checked_site),
            .proc = proc,
        });
        return id;
    }

    /// Publish a program's checked module table. The producer writes it
    /// once, before any consumer resolves an owner out of it. A program's
    /// failure meanings name each module by its checked key.
    pub fn setLoweringModules(self: *Result, modules: []const checked.ModuleId) Allocator.Error!void {
        self.lowering_modules.clearRetainingCapacity();
        self.modules_by_failure_identity.clearRetainingCapacity();
        self.store.lowering_module_identities.clearRetainingCapacity();
        for (modules) |module| _ = try self.appendLoweringModule(module, module.bytes);
    }

    /// Give a checked module the next dense lowering id. `identity` is what
    /// the program's failure meanings name the module by
    /// (`LirStore.lowering_module_identities`): its checked key in a
    /// program, its link name in a module unit.
    pub fn appendLoweringModule(self: *Result, module: checked.ModuleId, identity: [32]u8) Allocator.Error!LIR.LoweringModuleId {
        const id: LIR.LoweringModuleId = @fromBackingInt(@intCast(self.lowering_modules.items.len));
        try self.lowering_modules.ensureUnusedCapacity(self.store.allocator, 1);
        const named = try self.modules_by_failure_identity.getOrPut(self.store.allocator, identity);
        if (named.found_existing) base.invariant("two lowering modules share the identity failure meanings name them by", .{});
        named.value_ptr.* = id;
        try self.store.lowering_module_identities.append(self.store.allocator, identity);
        self.lowering_modules.appendAssumeCapacity(module);
        return id;
    }

    /// `meaning`, read from this program's code or from an object it loaded,
    /// with every module it names by checked key: a module unit's code and
    /// its objects name modules by link name, which this program's table
    /// maps to the modules it lowers (design.md "Compile-Time Failure
    /// Sites").
    pub fn meaningByCheckedKey(self: *const Result, meaning: LIR.FailureMeaning) LIR.FailureMeaning {
        return switch (meaning) {
            .checked_error => .checked_error,
            .literal_rejection => |literal| .{ .literal_rejection = .{ .module = self.checkedKeyOfFailureIdentity(literal.module), .node = literal.node, .kind = literal.kind } },
            .exhaustiveness => |failure| .{ .exhaustiveness = .{ .module = self.checkedKeyOfFailureIdentity(failure.module), .checked_site = failure.checked_site } },
            .failed_value => |value| .{ .failed_value = .{ .module = self.checkedKeyOfFailureIdentity(value.module), .producer = value.producer } },
        };
    }

    fn checkedKeyOfFailureIdentity(self: *const Result, identity: [32]u8) [32]u8 {
        const id = self.modules_by_failure_identity.get(identity) orelse base.invariant("a failure meaning names a module its program does not lower", .{});
        return self.lowering_modules.items[@backingInt(id)].bytes;
    }

    /// The dense id this program gave a checked module, when the module was
    /// part of its lowering input.
    pub fn loweringModuleId(self: *const Result, key: checked.ModuleId) ?LIR.LoweringModuleId {
        for (self.lowering_modules.items, 0..) |candidate, index| {
            if (std.mem.eql(u8, &candidate.bytes, &key.bytes)) return @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        }
        return null;
    }

    /// Intern one source `expect` so generated code can use a dense counter.
    pub fn addExpectSite(self: *Result, loc: base.SourceLoc, region: base.Region) Allocator.Error!LIR.ExpectSiteId {
        const key = ExpectSiteKey.init(loc, region);
        const entry = try self.expect_site_ids.getOrPut(self.store.allocator, key);
        if (entry.found_existing) return entry.value_ptr.*;
        errdefer _ = self.expect_site_ids.remove(key);
        const id: LIR.ExpectSiteId = @fromBackingInt(@intCast(@as(u32, @intCast(self.expect_sites.items.len))));
        try self.expect_sites.append(self.store.allocator, .{ .loc = loc, .region = region });
        entry.value_ptr.* = id;
        return id;
    }

    pub fn findExpectSite(self: *const Result, loc: base.SourceLoc, region: base.Region) ?LIR.ExpectSiteId {
        return self.expect_site_ids.get(ExpectSiteKey.init(loc, region));
    }

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
            .layout_only,
            .pending,
            .scalar,
            .str,
            .list,
            .box,
            .named,
            .boxy_fn,
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
            const_store.freeVector(allocator, variant.template.evidence);
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
                const_store.freeVector(allocator, template.evidence);
                if (template.evidence_frames.len > 0) allocator.free(template.evidence_frames);
            }
        }
        if (set.entries.len > 0) allocator.free(set.entries);
    }
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
        const id: LIR.LoweringModuleId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        try std.testing.expectEqualSlices(u8, &key.bytes, &result.lowering_modules.items[@backingInt(id)].bytes);
        try std.testing.expectEqual(id, result.loweringModuleId(key).?);
    }

    var absent: checked.ModuleId = .{};
    absent.bytes[0] = 10;
    try std.testing.expectEqual(@as(?LIR.LoweringModuleId, null), result.loweringModuleId(absent));

    // A site keeps the owner its producer recorded, not the procedure's owner.
    const owner: LIR.LoweringModuleId = @fromBackingInt(@intCast(2));
    const site = try result.addComptimeSite(.destructure, owner, base.Region.zero(), @fromBackingInt(@intCast(41)), .first);
    const stored = result.comptime_sites.items[@backingInt(site)];
    try std.testing.expectEqual(owner, stored.owner);
    try std.testing.expectEqual(@as(?LIR.CheckedExhaustivenessSiteId, @fromBackingInt(@intCast(41))), stored.checked_site.get());
    try std.testing.expectEqualSlices(u8, &keys[2].bytes, &result.lowering_modules.items[@backingInt(stored.owner)].bytes);
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
