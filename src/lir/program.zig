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
    /// Zero-initialized program-owned state, never pooled or named by content.
    /// Its producer supplies a stable site identity in `symbol_name`.
    is_mutable: bool = false,
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
    evidence: const_store.ConstFnEvidenceVector = .{},
    evidence_frames: []const const_store.ConstFnEvidenceFrame = &.{},
    evidence_frame_head: ?u32 = null,
};

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
    first_comptime_guard: ?u32 = null,
    /// Null when completed frozen data or a stored node supplies this slot
    /// directly.
    initializer: ?LIR.LirProcSpecId,
    layout_idx: layout.Idx,
    /// The stored list this value is frozen from, when it has no initializer
    /// (`Mono.StoredStaticList`).
    stored: ?StoredStaticValue = null,
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

/// The namespace of every symbol the compiler invents for generated code or
/// data. Platforms and hosts share the `roc_` interface (one underscore) with
/// Roc; nothing but the compiler defines a name in this one.
pub const compiler_symbol_namespace = "roc__";

/// Prefix of a datum named by content, the same in every program:
/// `roc__h{hash}`, for a string literal's backing or a constant an
/// object-cache pack carries.
pub const content_data_symbol_prefix = compiler_symbol_namespace ++ "h";

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

/// Complete LIR program and side data consumed by ARC, backends, and eval.
/// A template specialization's content key and the procedure lowered for it.
pub const SpecProc = struct {
    key: [32]u8,
    proc: LIR.LirProcSpecId,
};

/// A procedure an object-cache entry's served code calls by identity.
pub const ServedCallee = struct {
    entry: LIR.LirProcSpecId,
    callee: LIR.LirProcSpecId,
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
    /// The procedures each object-cache entry this program takes calls by
    /// identity, which the program supplies its own code for. The entry's
    /// procedure is body-less, so these are its only outgoing edges.
    served_callees: std.ArrayList(ServedCallee) = .empty,
    /// The procedure lowered for each template specialization, by the
    /// specialization's content key (`Monotype.Ast.specIdentityKey`). Only
    /// the specialization's own procedure is listed, never SpecConstr clones
    /// or capture-bearing variants of it.
    spec_procs: std.ArrayList(SpecProc) = .empty,
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
        self.erased_fns.deinit(allocator);
        self.fn_sets.deinit(allocator);
        self.const_type_names.deinit();
        self.const_types.deinit();
        self.requested_layouts.deinit(allocator);
        self.root_metadata.deinit(allocator);
        self.root_procs.deinit(allocator);
        self.exported_procs.deinit(allocator);
        self.served_callees.deinit(allocator);
        self.spec_procs.deinit(allocator);
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
            .checked_site = checked_site,
            .proc = proc,
        });
        return id;
    }

    /// Publish the lowering's checked module table. The producer writes it
    /// once, before any consumer resolves an owner out of it.
    pub fn setLoweringModules(self: *Result, modules: []const checked.ModuleId) Allocator.Error!void {
        self.lowering_modules.clearRetainingCapacity();
        self.store.lowering_module_keys.clearRetainingCapacity();
        for (modules) |module| _ = try self.appendLoweringModule(module);
    }

    /// Give a checked module the next dense lowering id.
    pub fn appendLoweringModule(self: *Result, module: checked.ModuleId) Allocator.Error!LIR.LoweringModuleId {
        const id: LIR.LoweringModuleId = @fromBackingInt(@intCast(self.lowering_modules.items.len));
        try self.lowering_modules.ensureUnusedCapacity(self.store.allocator, 1);
        try self.store.lowering_module_keys.append(self.store.allocator, module.bytes);
        self.lowering_modules.appendAssumeCapacity(module);
        return id;
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
    try std.testing.expectEqual(@as(?LIR.CheckedExhaustivenessSiteId, @fromBackingInt(@intCast(41))), stored.checked_site);
    try std.testing.expectEqualSlices(u8, &keys[2].bytes, &result.lowering_modules.items[@backingInt(stored.owner)].bytes);
}

test "program declarations are referenced" {
    std.testing.refAllDecls(@This());
}
