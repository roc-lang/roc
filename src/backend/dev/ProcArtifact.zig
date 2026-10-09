//! Per-region artifacts of dev-backend machine code.
//!
//! The code generator emits one buffer for a whole program, but it keeps every
//! reference from that buffer into itself symbolic until a final patch pass
//! and logs each range it emits with its producer (`LirCodeGen.CodeRegion`,
//! `LirCodeGen.CodeRef`). An artifact is one such range lifted out of the
//! buffer: its bytes, set-local references (region index plus delta), stable
//! references to not-yet-emitted producers, named relocations, source lines,
//! and frame metadata. Procedures and refcount helpers are named by content
//! (`ProcIdentity`, `roc__rc_*`), so an artifact set from one program can be
//! placed into another program's buffer with `append`, then resolved after
//! all required producers have landed. `assemble` performs both steps.
//! Source-file indices remain in the originating LIR domain; session caches
//! must not reuse line tables across unrelated source-file tables.
//!
//! `verifyRoundTrip` is the gate for the format: a program assembled from its
//! own artifacts must produce the same bytes, relocations, and unwind records
//! as compiling it directly.

const std = @import("std");
const Sha256 = @import("base").Sha256;
const lir = @import("lir");
const layout = @import("layout");
const RelocationMod = @import("Relocation.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const SymbolTable = @import("SymbolTable.zig");
const CtfeContext = @import("CtfeContext.zig");
const ContextImmediate = @import("ContextImmediate.zig");

/// Whether a symbol's name means the same thing in every program.
pub const SymbolScope = SymbolTable.Scope;

const Allocator = std.mem.Allocator;
const IndexedRelocation = RelocationMod.IndexedRelocation;

/// Producer-selected encoding: direct branch, inline address-plus-call, or address.
pub const Form = enum {
    call,
    inline_call,
    addr,

    /// Exact bytes rewritten by placement, not the enclosing instruction size.
    /// ARM inline calls and addresses share the four-word PC-relative sequence.
    pub fn patchSpan(self: Form, site: u32, arm: bool) ContextImmediate.Error!PatchSpan {
        if (arm) return .{ .offset = site, .width = if (self == .call) 4 else 16 };
        return switch (self) {
            .call => .{ .offset = @as(u64, site) + 1, .width = 4 },
            .addr => .{ .offset = @as(u64, site) + 3, .width = 4 },
            .inline_call => error.InvalidContextRelocation,
        };
    }
};

pub const PatchSpan = struct { offset: u64, width: u32 };

/// A producer-identified target whose code need not have been emitted yet.
pub const SymbolicReference = struct {
    site: u32,
    form: Form,
    /// A producer-reserved in-body veneer, still repatched by final placement.
    veneer: ?u32 = null,
    target: union(enum) {
        proc: lir.ProcIdentity,
        rc_helper: []const u8,
        boxy_thunk: lir.ProcIdentity,
    },
};

/// Region-relative offsets, preserving producer order even at equal offsets.
/// SourceLoc.file belongs to the originating LIR source-file table.
pub const LineEntry = LirCodeGenMod.LineEntry;

/// A reference from an artifact's bytes to a location inside another artifact.
pub const Reference = struct {
    /// Offset of the CALL/BL or ADR/LEA instruction within the artifact.
    site: u32,
    form: Form,
    /// Index of the target artifact within the set.
    target: u32,
    /// Offset within the target artifact the reference resolves to.
    delta: u32,
    veneer: ?u32 = null,
};

/// A relocation against a named symbol, with its offset relative to the artifact.
pub const NamedRelocation = struct {
    offset: u32,
    name: []const u8,
    /// Whether `name` means the same thing in another program; declared by
    /// the code that made the reference.
    scope: SymbolScope,
    kind: union(enum) {
        function,
        data: RelocationMod.DataRelocationKind,
    },

    pub fn patchSpan(self: NamedRelocation) PatchSpan {
        return .{ .offset = self.offset, .width = switch (self.kind) {
            .function => 4,
            .data => |kind| kind.patchWidth(),
        } };
    }
};

/// Frame metadata of a procedure-shaped artifact, re-recorded as an unwind
/// entry wherever the artifact lands.
pub const Frame = struct {
    prologue_size: u32,
    stack_alloc: u32,
    frame_size: u32,
    callee_saved_mask: u32,
    epilogue_offset: u32,
    uses_frame_pointer: bool,
};

/// What an artifact holds. Procedures, helpers, and thunks are named by
/// content; the rest are program-local ranges that only ever travel with
/// their own program.
pub const Kind = union(enum) {
    proc: lir.ProcIdentity,
    /// Symbol name of the helper (`roc__rc_*`).
    rc_helper: []const u8,
    boxy_thunk: lir.ProcIdentity,
    entrypoint,
    message_pool_run,
    /// Never lifted as a standalone artifact: islands belong to placement.
    /// Reserved in-body island bytes travel with their owning procedure and
    /// are repatched there. The tag preserves pack numbering.
    branch_island,
};

/// A readonly datum the artifact's relocations name and its program
/// defined: carried so a program that lacks it can define it.
pub const DataItem = struct {
    name: []const u8,
    bytes: []const u8,
    alignment: u32,
    symbol_offset: u32,
    /// Pointers inside `bytes` to other symbols: constants, literal
    /// backings, procedures, or refcount helpers.
    relocations: []const DataRelocation = &.{},
    /// `name` is the producing program's own name for an internal constant
    /// (`roc__d{N}`, `roc__d{N}_{k}`), which another program
    /// can give to a different constant. A pack names such a datum by
    /// content instead; see `ContentNames`.
    program_local_name: bool = false,
};

/// One pointer-sized relocation inside a data item.
pub const CodeTarget = union(enum) {
    /// Exact definition in this artifact set.
    local: u32,
    /// Producer-authored dependency, resolved in this set's namespace.
    symbolic: @FieldType(SymbolicReference, "target"),
    /// Exact receiving-image address, converted to `local` during extraction.
    /// This session-only handle must never be serialized.
    image_offset: usize,
};

pub const DataRelocation = struct {
    offset: u32,
    name: []const u8,
    addend: i64,
    /// The target is code (a procedure or refcount helper) rather than data.
    function: bool,
    /// Exact external binding supplied by the linking image, not carried data.
    external: bool = false,
    code_target: ?CodeTarget = null,
};

/// Prefix of a datum named by content, the same wherever it lands.
pub const content_data_prefix = lir.Program.content_data_symbol_prefix;

/// One lifted region of machine code.
pub const Artifact = struct {
    kind: Kind,
    code: []const u8,
    /// Offset within `code` that references to this artifact resolve to.
    entry: u32,
    frame: ?Frame,
    refs: []const Reference,
    symbolic_refs: []const SymbolicReference = &.{},
    lines: []const LineEntry = &.{},
    relocations: []const NamedRelocation,
    data: []const DataItem,
    domain: CtfeContext.Domain = .runtime,
    context_contract: ?LirCodeGenMod.FragmentContract = null,
    context_dependencies: ?LirCodeGenMod.FragmentContextDependencies = null,
    context_complete: bool = true,
    context_bindings: []const CtfeContext.Binding = &.{},
    context_relocations: []const ContextImmediate.Relocation = &.{},
    /// Producer-stamped callable ABI and final ARC contract. Null explicitly
    /// forbids substituting another definition, even with the same identity.
    callable_contract: ?[32]u8 = null,
    /// Exact declaration emitted in the current producer session. Never stored
    /// in a pack: cached dependencies do not become this session's offers.
    producer_proc: ?lir.LIR.LirProcSpecId = null,
    producer_helper: ?u64 = null,
    /// Pre-omit/pre-inline source expect facts, distinct from machine hooks.
    source_observations: lir.Program.ProducerObservations = .unknown,
    /// Emission encountered a diagnostic/debug/site obligation, recorded before
    /// choosing instrumented versus runtime emission. Null is missing metadata.
    requires_ctfe_observations: ?bool = null,
};

fn contractWord(hasher: *Sha256, value: u64) void {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, value, .little);
    hasher.update(&bytes);
}

/// Stamped while the actual producer still owns layouts and ARC facts. Dense
/// local/layout IDs never enter a persistent compatibility proof.
fn callableContract(
    comptime CG: type,
    codegen: *CG,
    region: CG.CodeRegion,
    proc_specs: []const lir.LIR.LirProcSpec,
    layouts: *const layout.Store,
) Allocator.Error!?[32]u8 {
    if (region.artifact_domain != null) return region.artifact_callable_contract;
    if (region.context_dependencies == null) return null;
    var hasher = Sha256.init(.{});
    hasher.update("roc.native.callable-contract.v1");
    switch (region.kind) {
        .proc, .boxy_thunk => |id| {
            const proc = proc_specs[@intFromEnum(id)];
            // Descriptor/erased ABIs need their own complete producer contract.
            // Recording a partial signature would falsely certify substitution.
            if (proc.erased_reuse_arg != null or proc.erased_call_args != null or
                proc.ret_desc != null or proc.runtime_ret_desc != null or
                proc.erased_capture_arg != null or proc.erased_arg_desc_offsets.len != 0 or
                proc.erased_arg_layouts.len != 0 or proc.erased_arg_desc_params.len != 0 or
                proc.hosted != null or proc.is_static_initializer) return null;
            contractWord(&hasher, @intFromEnum(std.meta.activeTag(region.kind)));
            contractWord(&hasher, @intFromEnum(proc.abi));
            contractWord(&hasher, @intFromBool(proc.boxy_runtime_entry));
            contractWord(&hasher, @intFromBool(proc.rc_variant_demandable));
            const params = codegen.store.getLocalSpan(proc.args);
            contractWord(&hasher, params.len);
            for (0..params.len) |index| {
                const local = codegen.store.getLocal(lir.LirStore.GuardedList.at(params, index));
                if (local.boxy_desc != null) return null;
                const digest = try layouts.contentDigest(local.layout_idx);
                hasher.update(&digest);
            }
            const ret = try layouts.contentDigest(proc.ret_layout);
            hasher.update(&ret);
            contractWord(&hasher, proc.rc_borrowed_params);
            contractWord(&hasher, @intFromBool(proc.rc_ret_borrowed));
            contractWord(&hasher, proc.rc_ret_lenders);
            contractWord(&hasher, proc.rc_read_only_params);
            contractWord(&hasher, @intFromBool(proc.rc_ret_unique));
            contractWord(&hasher, proc.rc_ret_unique_fields);
            const conditions = codegen.store.getU32Span(proc.rc_ret_conditions);
            contractWord(&hasher, conditions.len);
            for (0..conditions.len) |index| contractWord(&hasher, lir.LirStore.GuardedList.at(conditions, index));
        },
        .rc_helper => |key| {
            // The helper producer's name encodes operation, atomicity and
            // structural layout; its fixed calling convention is versioned.
            const name = try LirCodeGenMod.compiledRcHelperSymbolName(codegen.allocator, layouts, key);
            defer codegen.allocator.free(name);
            hasher.update("rc-helper-abi-v1");
            hasher.update(name);
        },
        .spliced_proc, .spliced_boxy_thunk, .spliced_helper, .entrypoint, .message_pool_run, .branch_island, .hosted_stub => return null,
    }
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    return digest;
}

/// Context sites exclusively own their complete fixed-width templates.
/// Ordinary code patches must not touch them; data-item patches live in a
/// different section. Sorted canonical context records make each ownership
/// check logarithmic without imposing order on ordinary producer records.
pub fn validateContextPatches(artifact: Artifact) ContextImmediate.Error!void {
    try ContextImmediate.validateRecords(artifact.code, artifact.context_relocations, artifact.context_bindings.len);
    if (artifact.context_relocations.len == 0) return;
    const contract = artifact.context_contract orelse return error.InvalidContextRelocation;
    const arm = switch (contract.target.toCpuArch()) {
        .aarch64 => true,
        .x86_64 => false,
        else => return error.InvalidContextRelocation,
    };
    for (artifact.context_relocations) |relocation| {
        if ((relocation.encoding == .arm_movwide) != arm) return error.InvalidContextRelocation;
    }
    for (artifact.relocations) |relocation| try validateOrdinaryPatch(artifact, relocation.patchSpan());
    for (artifact.refs) |reference| {
        try validateOrdinaryPatch(artifact, try reference.form.patchSpan(reference.site, arm));
        if (reference.veneer) |site| {
            if (!arm or reference.form != .call) return error.InvalidContextRelocation;
            try validateOrdinaryPatch(artifact, .{ .offset = site, .width = 16 });
        }
    }
    for (artifact.symbolic_refs) |reference| {
        try validateOrdinaryPatch(artifact, try reference.form.patchSpan(reference.site, arm));
        if (reference.veneer) |site| {
            if (!arm or reference.form != .call) return error.InvalidContextRelocation;
            try validateOrdinaryPatch(artifact, .{ .offset = site, .width = 16 });
        }
    }
}

fn validateOrdinaryPatch(artifact: Artifact, span: PatchSpan) ContextImmediate.Error!void {
    if (span.offset > artifact.code.len or span.width > artifact.code.len - span.offset)
        return error.InvalidContextRelocation;
    const end = span.offset + span.width;
    var low: usize = 0;
    var high = artifact.context_relocations.len;
    // Find the first context template ending after this patch starts.
    while (low < high) {
        const mid = low + (high - low) / 2;
        const context = artifact.context_relocations[mid];
        if (@as(u64, context.offset) + ContextImmediate.width(context.encoding) <= span.offset) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    if (low < artifact.context_relocations.len and artifact.context_relocations[low].offset < end)
        return error.InvalidContextRelocation;
}

/// An ordered set of artifacts covering one code buffer. Order is emission
/// order, which `assemble` reproduces.
pub const Set = struct {
    arena: std.heap.ArenaAllocator,
    artifacts: []const Artifact,

    pub fn deinit(self: *Set) void {
        self.arena.deinit();
    }
};

pub const ContextResolver = struct {
    context: *anyopaque,
    resolve: *const fn (*anyopaque, CtfeContext.Binding) ?u64,
};

pub const BindContextError = Allocator.Error || ContextImmediate.Error || error{ IncompleteContext, InvalidContextDomain };

/// Bind an owned clone. Published artifacts remain immutable even when two
/// consumers give the same descriptor different dense IDs.
pub fn bindContextClone(allocator: Allocator, source: *const Set, resolver: ContextResolver) BindContextError!Set {
    var clone = try combine(allocator, &.{source});
    errdefer clone.deinit();
    try bindContextOwned(&clone, resolver);
    return clone;
}

/// Bind a privately owned selection only after the entire binding plan validates.
/// No code bytes change on any validation, resolver, or allocation failure.
pub fn bindContextOwned(owned: *Set, resolver: ContextResolver) BindContextError!void {
    var temporary = std.heap.ArenaAllocator.init(owned.arena.child_allocator);
    defer temporary.deinit();
    const a = temporary.allocator();
    const all_values = try a.alloc([]u64, owned.artifacts.len);
    for (owned.artifacts, all_values) |artifact, *resolved| {
        try validateContextPatches(artifact);
        if (!artifact.context_complete) return error.IncompleteContext;
        if (artifact.domain != .ctfe and (artifact.context_bindings.len != 0 or artifact.context_relocations.len != 0)) return error.InvalidContextDomain;
        if (artifact.domain == .ctfe and (artifact.context_contract == null or artifact.context_dependencies == null or !artifact.context_contract.?.hooks_enabled or !artifact.context_dependencies.?.comptime_hooks)) return error.InvalidContextDomain;
        const values = try a.alloc(u64, artifact.context_bindings.len);
        for (artifact.context_bindings, values) |binding, *value| {
            value.* = resolver.resolve(resolver.context, binding) orelse return error.MissingContextBinding;
        }
        resolved.* = values;
    }
    for (owned.artifacts, all_values) |artifact, values| {
        try ContextImmediate.bind(@constCast(artifact.code), artifact.context_relocations, values);
    }
}

test "owned context binding validates every artifact before mutation" {
    const artifact = Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "\x48\xb8" ++ "\x00" ** 8,
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .domain = .ctfe,
        .context_contract = .{
            .target = .x64musl,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
            .dict_seed_mode = .comptime_zero,
            .hooks_enabled = true,
            .initialize_boxy_runtime = false,
            .static_data_readonly = false,
        },
        .context_dependencies = .{ .comptime_hooks = true },
        .context_bindings = &.{.{ .site = .{
            .checked_module = @splat(7),
            .checked_site = null,
            .procedure_identity = lir.ProcIdentity.forTest(1).bytes,
            .kind = .if_,
            .region = .from_raw_offsets(1, 4),
            .branch_regions = &.{},
        } }},
        .context_relocations = &.{.{ .offset = 0, .binding = 0, .encoding = .x86_movabs }},
    };
    var artifacts = [_]Artifact{ artifact, artifact };
    artifacts[1].context_complete = false;
    const source = Set{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator), .artifacts = &artifacts };
    var owned = try combine(std.testing.allocator, &.{&source});
    defer owned.deinit();
    const Resolver = struct {
        fn resolve(_: *anyopaque, _: CtfeContext.Binding) ?u64 {
            return 42;
        }
    };
    var context: u8 = 0;
    try std.testing.expectError(error.IncompleteContext, bindContextOwned(&owned, .{ .context = &context, .resolve = Resolver.resolve }));
    try std.testing.expectEqualSlices(u8, artifact.code, owned.artifacts[0].code);
    @constCast(owned.artifacts)[1].context_complete = true;
    try bindContextOwned(&owned, .{ .context = &context, .resolve = Resolver.resolve });
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, owned.artifacts[0].code[2..10], .little));
    try std.testing.expectEqualSlices(u8, "\x48\xb8" ++ "\x00" ** 8, source.artifacts[0].code);
}

/// A fragment owns its requirement list in the same arena as its artifacts.
pub const Fragment = struct {
    set: Set,
    required_helpers: []const u64,
    context_dependencies: LirCodeGenMod.FragmentContextDependencies,

    pub fn clone(self: *const Fragment, allocator: Allocator) Allocator.Error!Fragment {
        var set = try combine(allocator, &.{&self.set});
        errdefer set.deinit();
        const required_helpers = try set.arena.allocator().dupe(u64, self.required_helpers);
        return .{
            .set = set,
            .required_helpers = required_helpers,
            .context_dependencies = self.context_dependencies,
        };
    }

    pub fn deinit(self: *Fragment) void {
        self.set.deinit();
    }
};

/// Emit one body against the producer's LIR metadata, then sever all workspace
/// ownership. No other procedure or helper body is needed to extract this set.
pub fn compileProcFragment(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_id: lir.LIR.LirProcSpecId,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
) ExtractError!Fragment {
    try codegen.emitProcFragment(proc_id);
    return captureFragment(CG, allocator, codegen, proc_specs, layout_store, string_exports, constant_exports);
}

/// Emit one helper without recursively emitting its transitive requirements.
/// Its key is interpreted only in the producer's layout and compilation domain.
pub fn compileRcHelperFragment(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    key: u64,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
) ExtractError!Fragment {
    try codegen.emitRcHelperFragment(key);
    return captureFragment(CG, allocator, codegen, proc_specs, layout_store, string_exports, constant_exports);
}

fn captureFragment(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
) ExtractError!Fragment {
    var prepared = try PreparedData.init(allocator, proc_specs, string_exports, constant_exports, &.{});
    defer prepared.deinit();
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, &prepared);
}

/// Emit a procedure using the coordinator's shared immutable data catalog.
pub fn compileProcFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_id: lir.LIR.LirProcSpecId,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    try codegen.emitProcFragment(proc_id);
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
}

/// Emit a helper using the coordinator's shared immutable data catalog.
pub fn compileRcHelperFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    key: u64,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    try codegen.emitRcHelperFragment(key);
    return captureFragmentPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
}

fn captureFragmentPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Fragment {
    var set = try extractPrepared(CG, allocator, codegen, proc_specs, layout_store, prepared);
    errdefer set.deinit();
    const required_helpers = try codegen.getRequiredRcHelpers(set.arena.allocator());
    return .{
        .set = set,
        .required_helpers = required_helpers,
        .context_dependencies = codegen.getFragmentContextDependencies(),
    };
}

fn cloneData(a: Allocator, item: DataItem) Allocator.Error!DataItem {
    const relocations = try a.dupe(DataRelocation, item.relocations);
    for (relocations) |*relocation| {
        relocation.name = try a.dupe(u8, relocation.name);
        if (relocation.code_target) |target| {
            if (target == .symbolic and target.symbolic == .rc_helper) {
                relocation.code_target = .{ .symbolic = .{ .rc_helper = try a.dupe(u8, target.symbolic.rc_helper) } };
            }
        }
    }
    return .{
        .name = try a.dupe(u8, item.name),
        .bytes = try a.dupe(u8, item.bytes),
        .alignment = item.alignment,
        .symbol_offset = item.symbol_offset,
        .relocations = relocations,
        .program_local_name = item.program_local_name,
    };
}

/// Deep-copy sets into one ownership domain, rebasing only set-local references.
/// Callers select cache entries using their full compilation provenance.
pub fn combine(allocator: Allocator, sets: []const *const Set) Allocator.Error!Set {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    var artifacts = std.ArrayList(Artifact).empty;
    for (sets) |set| {
        const base: u32 = @intCast(artifacts.items.len);
        for (set.artifacts) |source| {
            var artifact = source;
            artifact.kind = switch (source.kind) {
                .rc_helper => |name| .{ .rc_helper = try a.dupe(u8, name) },
                .proc, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => source.kind,
            };
            artifact.code = try a.dupe(u8, source.code);
            const bindings = try a.alloc(CtfeContext.Binding, source.context_bindings.len);
            for (source.context_bindings, bindings) |binding, *owned| owned.* = try binding.clone(a);
            artifact.context_bindings = bindings;
            artifact.context_relocations = try a.dupe(ContextImmediate.Relocation, source.context_relocations);
            const refs = try a.dupe(Reference, source.refs);
            for (refs) |*ref| ref.target += base;
            artifact.refs = refs;
            const symbolic_refs = try a.dupe(SymbolicReference, source.symbolic_refs);
            for (symbolic_refs) |*ref| switch (ref.target) {
                .rc_helper => |name| ref.target = .{ .rc_helper = try a.dupe(u8, name) },
                .proc, .boxy_thunk => {},
            };
            artifact.symbolic_refs = symbolic_refs;
            artifact.lines = try a.dupe(LineEntry, source.lines);
            const relocations = try a.dupe(NamedRelocation, source.relocations);
            for (relocations) |*relocation| relocation.name = try a.dupe(u8, relocation.name);
            artifact.relocations = relocations;
            const data = try a.alloc(DataItem, source.data.len);
            for (source.data, data) |item, *owned| {
                owned.* = try cloneData(a, item);
                for (@constCast(owned.relocations)) |*relocation| {
                    if (relocation.code_target) |*target| {
                        if (target.* == .local) target.local += base;
                    }
                }
            }
            artifact.data = data;
            try artifacts.append(a, artifact);
        }
    }
    const owned = try artifacts.toOwnedSlice(a);
    return .{ .arena = arena, .artifacts = owned };
}

/// Resolve only freshly emitted references against the current producer's
/// explicitly selected declarations. Cached dependency graphs retain their own
/// indexed edges; logical siblings beside them never participate in this bind.
pub fn bindProducedReferences(set: *Set, fresh_count: u32, helper_roots: []const u32) Allocator.Error!void {
    const a = set.arena.allocator();
    var procs = std.AutoHashMap(lir.ProcIdentity, u32).init(a);
    var thunks = std.AutoHashMap(lir.ProcIdentity, u32).init(a);
    var helpers = std.StringHashMap(u32).init(a);
    for (set.artifacts, 0..) |artifact, index| {
        if (artifact.producer_proc == null) continue;
        switch (artifact.kind) {
            .proc => |identity| try procs.putNoClobber(identity, @intCast(index)),
            .boxy_thunk => |identity| try thunks.putNoClobber(identity, @intCast(index)),
            else => unreachable,
        }
    }
    for (helper_roots) |root| try helpers.putNoClobber(set.artifacts[root].kind.rc_helper, root);
    const Bindings = struct {
        fn resolve(target: @FieldType(SymbolicReference, "target"), p: *const @TypeOf(procs), t: *const @TypeOf(thunks), h: *const @TypeOf(helpers)) ?u32 {
            return switch (target) {
                .proc => |identity| p.get(identity),
                .boxy_thunk => |identity| t.get(identity),
                .rc_helper => |name| h.get(name),
            };
        }
    };
    for (@constCast(set.artifacts[0..fresh_count])) |*artifact| {
        var refs = std.ArrayList(Reference).empty;
        try refs.appendSlice(a, artifact.refs);
        var unresolved = std.ArrayList(SymbolicReference).empty;
        for (artifact.symbolic_refs) |ref| {
            if (Bindings.resolve(ref.target, &procs, &thunks, &helpers)) |target| {
                try refs.append(a, .{
                    .site = ref.site,
                    .form = ref.form,
                    .target = target,
                    .delta = set.artifacts[target].entry,
                    .veneer = ref.veneer,
                });
            } else {
                artifact.context_complete = false;
                try unresolved.append(a, ref);
            }
        }
        artifact.refs = try refs.toOwnedSlice(a);
        artifact.symbolic_refs = try unresolved.toOwnedSlice(a);
        for (artifact.data) |item| for (@constCast(item.relocations)) |*relocation| {
            const target = relocation.code_target orelse continue;
            if (target != .symbolic) continue;
            if (Bindings.resolve(target.symbolic, &procs, &thunks, &helpers)) |node| {
                relocation.code_target = .{ .local = node };
            } else {
                artifact.context_complete = false;
            }
        };
    }
}

/// Why a code buffer could not be lifted into artifacts.
pub const ExtractError = Allocator.Error || error{
    /// Two producer regions overlap. Embedded branch islands are not
    /// independent producers and are retained with their owning region.
    NestedCodeRegion,
    /// Bytes of the code buffer belong to no logged region.
    UncoveredCode,
    /// A reference resolves to an offset inside no region.
    DanglingReference,
    /// A relocation kind an artifact cannot carry.
    UnsupportedRelocation,
};

/// Content names for the internal constants an artifact set carries. A
/// constant is a node in a graph of readonly data: its bytes plus pointer
/// relocations to other constants, literal backings, procedures, or refcount
/// helpers. Two programs that hold the same constant must name it the same
/// way in their packs, and two programs that give one program-local name to
/// different constants must not, so every carried item with a
/// `program_local_name` is named by the digest of its own rendering: bytes,
/// alignment, symbol offset, and relocations, with program-local targets by
/// digest and every other target by its name. A constant can point back at
/// itself through a cycle; the walk writes a back-reference by relative
/// stack depth, as the procedure identity renderer does, and remembers every
/// node whose rendering refers to nothing above its own frame.
pub const ContentNames = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    /// Every carried item, by the name its program gave it.
    items: std.StringHashMapUnmanaged(DataItem) = .empty,
    /// Content name of each program-local item.
    names: std.StringHashMapUnmanaged([]const u8) = .empty,
    digests: std.StringHashMapUnmanaged([32]u8) = .empty,
    active: std.StringHashMapUnmanaged(u32) = .empty,
    depth: u32 = 0,
    /// Physical source graph identity, not a callable compatibility proof.
    /// A local ordinal alone could collide between incompatible source packs.
    code_namespace: [32]u8 = [_]u8{0} ** 32,

    const domain = "roc.static.data.v2";
    const no_reference: u32 = std.math.maxInt(u32);

    pub fn init(allocator: Allocator, set: *const Set) Allocator.Error!ContentNames {
        var self = ContentNames{ .allocator = allocator, .arena = std.heap.ArenaAllocator.init(allocator) };
        errdefer self.deinit();
        var has_code = false;
        for (set.artifacts) |artifact| for (artifact.data) |item| for (item.relocations) |relocation| {
            if (relocation.code_target) |target| {
                has_code = true;
                std.debug.assert(target != .image_offset);
            }
        };
        if (has_code) {
            var hasher = Sha256.init(.{});
            hasher.update("roc.physical.artifact.graph.v1");
            for (set.artifacts) |original| {
                var artifact = original;
                artifact.producer_proc = null;
                artifact.producer_helper = null;
                std.hash.autoHashStrat(&hasher, artifact, .Deep);
            }
            self.code_namespace = hasher.finalResult();
        }
        for (set.artifacts) |artifact| {
            for (artifact.data) |item| try self.items.put(allocator, item.name, item);
        }
        for (set.artifacts) |artifact| {
            for (artifact.data) |item| {
                if (!item.program_local_name or self.names.contains(item.name)) continue;
                var hasher = Sha256.init(.{});
                _ = try self.write(&hasher, item);
                const digest = self.digests.get(item.name) orelse unreachable;
                const name = try std.fmt.allocPrint(self.arena.allocator(), content_data_prefix ++ "{s}", .{&std.fmt.bytesToHex(digest[0..16].*, .lower)});
                try self.names.put(allocator, item.name, name);
            }
        }
        return self;
    }

    pub fn deinit(self: *ContentNames) void {
        self.active.deinit(self.allocator);
        self.digests.deinit(self.allocator);
        self.names.deinit(self.allocator);
        self.items.deinit(self.allocator);
        self.arena.deinit();
    }

    /// The name a pack gives the symbol its program called `name`.
    pub fn of(self: *const ContentNames, name: []const u8) []const u8 {
        return self.names.get(name) orelse name;
    }

    /// Whether the pack names `name` by content instead.
    pub fn renames(self: *const ContentNames, name: []const u8) bool {
        return self.names.contains(name);
    }

    /// Writes the digest of `item`'s rendering and returns the shallowest
    /// stack depth that rendering referred back to.
    fn write(self: *ContentNames, hasher: *Sha256, item: DataItem) Allocator.Error!u32 {
        if (self.active.get(item.name)) |depth| {
            hasher.update("cycle");
            writeWord(hasher, self.depth - depth);
            return depth;
        }
        if (self.digests.get(item.name)) |digest| {
            hasher.update("data");
            hasher.update(&digest);
            return no_reference;
        }
        const depth = self.depth;
        try self.active.putNoClobber(self.allocator, item.name, depth);
        self.depth += 1;
        defer {
            _ = self.active.remove(item.name);
            self.depth -= 1;
        }

        var sub = Sha256.init(.{});
        sub.update(domain);
        writeWord(&sub, @intCast(item.bytes.len));
        sub.update(item.bytes);
        writeWord(&sub, item.alignment);
        writeWord(&sub, item.symbol_offset);
        var low: u32 = no_reference;
        const relocations = try self.allocator.dupe(DataRelocation, item.relocations);
        defer self.allocator.free(relocations);
        std.mem.sort(DataRelocation, relocations, {}, relocationOffsetBefore);
        writeWord(&sub, @intCast(relocations.len));
        for (relocations) |relocation| {
            writeWord(&sub, relocation.offset);
            writeWide(&sub, @bitCast(relocation.addend));
            sub.update(&.{ @intFromBool(relocation.function), @intFromBool(relocation.external) });
            if (relocation.code_target) |target| {
                switch (target) {
                    .local => |node| {
                        sub.update("physical-code");
                        sub.update(&self.code_namespace);
                        writeWord(&sub, node);
                    },
                    .symbolic => |symbol| {
                        sub.update("symbolic-code");
                        sub.update(&self.code_namespace);
                        std.hash.autoHashStrat(&sub, symbol, .Deep);
                    },
                    .image_offset => unreachable,
                }
                continue;
            }
            const target = if (relocation.function or relocation.external) null else self.items.get(relocation.name);
            if (target != null and target.?.program_local_name) {
                low = @min(low, try self.write(&sub, target.?));
            } else {
                sub.update("named");
                writeWord(&sub, @intCast(relocation.name.len));
                sub.update(relocation.name);
            }
        }
        const digest = sub.finalResult();
        hasher.update("data");
        hasher.update(&digest);
        if (low >= depth) {
            try self.digests.put(self.allocator, item.name, digest);
            return no_reference;
        }
        return low;
    }

    fn relocationOffsetBefore(_: void, a: DataRelocation, b: DataRelocation) bool {
        return a.offset < b.offset;
    }

    fn writeWord(hasher: *Sha256, value: u32) void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        hasher.update(&buffer);
    }

    fn writeWide(hasher: *Sha256, value: u64) void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        hasher.update(&buffer);
    }
};

/// Immutable data catalog prepared once by the coordinator. Worker lookups
/// borrow it read-only; retained artifacts copy only their reachable closure.
pub const PreparedData = struct {
    arena: std.heap.ArenaAllocator,
    items: std.StringHashMapUnmanaged(DataItem),

    pub fn init(
        allocator: Allocator,
        proc_specs: []const lir.LIR.LirProcSpec,
        string_exports: []const lir.Program.StaticDataExport,
        constant_exports: []const lir.Program.StaticDataExport,
        spliced_data: []const DataItem,
    ) Allocator.Error!PreparedData {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();
        const a = arena.allocator();
        var items = std.StringHashMapUnmanaged(DataItem){};
        for (string_exports) |string_export| {
            const item = try cloneData(a, .{
                .name = string_export.symbol_name,
                .bytes = string_export.bytes,
                .alignment = string_export.alignment,
                .symbol_offset = string_export.symbol_offset,
            });
            try items.put(a, item.name, item);
        }
        for (constant_exports) |constant| {
            const relocations = try a.alloc(DataRelocation, constant.relocations.len);
            for (constant.relocations, relocations) |relocation, *out| {
                const name = switch (relocation.target) {
                    .data_symbol => |target| constant_exports[@intFromEnum(target)].symbol_name,
                    .named => relocation.target_symbol_name,
                };
                out.* = .{
                    .offset = @intCast(relocation.offset),
                    .name = try a.dupe(u8, name),
                    .addend = relocation.addend,
                    .function = switch (relocation.kind) {
                        .address => false,
                        .function_pointer => true,
                    },
                    .code_target = if (relocation.procedure) |proc|
                        .{ .symbolic = .{ .proc = proc_specs[@intFromEnum(proc)].identity } }
                    else if (relocation.rc_helper != null)
                        .{ .symbolic = .{ .rc_helper = try a.dupe(u8, name) } }
                    else
                        null,
                };
            }
            const item = DataItem{
                .name = try a.dupe(u8, constant.symbol_name),
                .bytes = try a.dupe(u8, constant.bytes),
                .alignment = constant.alignment,
                .symbol_offset = constant.symbol_offset,
                .relocations = relocations,
                .program_local_name = !constant.is_exported,
            };
            try items.put(a, item.name, item);
        }
        for (spliced_data) |item| {
            const owned = try cloneData(a, item);
            try items.put(a, owned.name, owned);
        }
        return .{ .arena = arena, .items = items };
    }

    pub fn deinit(self: *PreparedData) void {
        self.arena.deinit();
    }
};

/// Lift every region of a finished code generator's buffer into an artifact set.
pub fn extract(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
    constant_exports: []const lir.Program.StaticDataExport,
    spliced_data: []const DataItem,
) ExtractError!Set {
    var prepared = try PreparedData.init(allocator, proc_specs, string_exports, constant_exports, spliced_data);
    defer prepared.deinit();
    return extractPrepared(CG, allocator, codegen, proc_specs, layout_store, &prepared);
}

/// Capture against a shared immutable catalog without scanning module exports.
pub fn extractPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    prepared: *const PreparedData,
) ExtractError!Set {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_allocator = arena.allocator();

    // Only producer-local binding cells enter this overlay. Immutable data
    // remains in the shared catalog until a relocation actually reaches it.
    var data_by_name = std.StringHashMap(DataItem).init(allocator);
    defer data_by_name.deinit();
    for (codegen.bindingDataCells()) |cell| {
        const name = try arena_allocator.dupe(u8, cell.name);
        const cell_relocations = try arena_allocator.alloc(DataRelocation, 1);
        cell_relocations[0] = .{
            .offset = 0,
            .name = try arena_allocator.dupe(u8, cell.target_name),
            .addend = 0,
            .function = false,
            .external = true,
        };
        try data_by_name.put(name, .{
            .name = name,
            .bytes = try arena_allocator.dupe(u8, &([_]u8{0} ** 8)),
            .alignment = 8,
            .symbol_offset = 0,
            .relocations = cell_relocations,
        });
    }

    const code = codegen.getGeneratedCode();
    const all_regions = try allocator.dupe(CG.CodeRegion, codegen.codeRegions());
    defer allocator.free(all_regions);
    std.mem.sort(CG.CodeRegion, all_regions, {}, regionStartsBefore(CG.CodeRegion));
    // An island inside a procedure is already carried by that procedure's
    // bytes. Top-level islands remain placement-owned, as before.
    var region_count: usize = 0;
    var containing_end: usize = 0;
    for (all_regions) |region| {
        if (region.kind == .branch_island and region.start < containing_end and region.end <= containing_end) continue;
        all_regions[region_count] = region;
        region_count += 1;
        containing_end = region.end;
    }
    const regions = all_regions[0..region_count];

    var covered: usize = 0;
    for (regions) |region| {
        if (region.start < covered) return error.NestedCodeRegion;
        if (region.start != covered) return error.UncoveredCode;
        covered = region.end;
    }
    if (covered != code.len) return error.UncoveredCode;

    const unwind = codegen.getUnwindFunctions();
    const relocations = try codegen.artifactRelocations(allocator);
    defer allocator.free(relocations);
    const refs = codegen.codeRefs();
    var producer_procs = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer producer_procs.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (!proc.is_static_initializer) try producer_procs.put(proc.identity, @enumFromInt(@as(u32, @intCast(index))));
    }

    // Standalone branch islands belong to this placement, not the artifact
    // set. Embedded reservations stay inside their owning bytes but every
    // reference still records its logical target for final placement.
    const artifact_of_region = try allocator.alloc(?u32, regions.len);
    defer allocator.free(artifact_of_region);
    var artifact_count: u32 = 0;
    for (regions, 0..) |region, index| {
        artifact_of_region[index] = if (regionHasArtifact(region.kind)) artifact_count else null;
        if (regionHasArtifact(region.kind)) artifact_count += 1;
    }

    const artifacts = try arena_allocator.alloc(Artifact, artifact_count);
    for (regions, 0..) |region, region_index| {
        const index = artifact_of_region[region_index] orelse continue;
        const kind: Kind = switch (region.kind) {
            .proc => |proc_id| .{ .proc = proc_specs[@intFromEnum(proc_id)].identity },
            .rc_helper => |key| .{ .rc_helper = try LirCodeGenMod.compiledRcHelperSymbolName(arena_allocator, layout_store, key) },
            .boxy_thunk => |proc_id| .{ .boxy_thunk = proc_specs[@intFromEnum(proc_id)].identity },
            .entrypoint => .entrypoint,
            .message_pool_run => .message_pool_run,
            .branch_island, .hosted_stub => unreachable,
            .spliced_proc => |identity| .{ .proc = identity },
            .spliced_boxy_thunk => |identity| .{ .boxy_thunk = identity },
            .spliced_helper => .{ .rc_helper = try arena_allocator.dupe(u8, region.artifact_helper_name orelse codegen.splicedHelperName(region.start + region.entry) orelse return error.DanglingReference) },
        };

        var frame: ?Frame = null;
        for (unwind) |function| {
            if (function.start_offset != region.start + region.entry) continue;
            frame = .{
                .prologue_size = function.prologue_size,
                .stack_alloc = function.stack_alloc,
                .frame_size = function.frame_size,
                .callee_saved_mask = function.callee_saved_mask,
                .epilogue_offset = function.epilogue_offset,
                .uses_frame_pointer = function.uses_frame_pointer,
            };
            break;
        }

        var region_refs = std.ArrayList(Reference).empty;
        var symbolic_refs = std.ArrayList(SymbolicReference).empty;
        for (refs) |ref| {
            if (ref.site < region.start or ref.site >= region.end) continue;
            const symbolic_target: ?@FieldType(SymbolicReference, "target") = switch (ref.target) {
                .proc => |proc_id| if (codegen.compiledProcSymbol(proc_id) == null) .{ .proc = proc_specs[@intFromEnum(proc_id)].identity } else null,
                .rc_helper => |key| if (codegen.compiledRcHelperOffset(key) == null) .{ .rc_helper = try LirCodeGenMod.compiledRcHelperSymbolName(arena_allocator, layout_store, key) } else null,
                .boxy_thunk => |proc_id| if (codegen.boxyThunkOffset(proc_id) == null) .{ .boxy_thunk = proc_specs[@intFromEnum(proc_id)].identity } else null,
                .message, .offset => null,
            };
            if (symbolic_target) |target| {
                try symbolic_refs.append(arena_allocator, .{
                    .site = @intCast(ref.site - region.start),
                    .form = switch (ref.form) {
                        .call => .call,
                        .inline_call => .inline_call,
                        .addr => .addr,
                    },
                    .veneer = if (codegen.codeRefVeneer(ref.site)) |offset| if (offset >= region.start and offset < region.end) @intCast(offset - region.start) else null else null,
                    .target = target,
                });
                continue;
            }
            const target_offset: usize = switch (ref.target) {
                .proc => |proc_id| (codegen.compiledProcSymbol(proc_id) orelse return error.DanglingReference).code_start,
                .rc_helper => |key| codegen.compiledRcHelperOffset(key) orelse return error.DanglingReference,
                .message => |message_offset| messageOffsetInCode(CG, regions, message_offset) orelse return error.DanglingReference,
                .boxy_thunk => |proc_id| codegen.boxyThunkOffset(proc_id) orelse return error.DanglingReference,
                .offset => |offset| offset,
            };
            const target_region = regionContaining(CG, regions, target_offset) orelse return error.DanglingReference;
            const target_index = artifact_of_region[target_region] orelse return error.DanglingReference;
            try region_refs.append(arena_allocator, .{
                .site = @intCast(ref.site - region.start),
                .form = switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                .target = target_index,
                .delta = @intCast(target_offset - regions[target_region].start),
                .veneer = if (codegen.codeRefVeneer(ref.site)) |offset| if (offset >= region.start and offset < region.end) @intCast(offset - region.start) else null else null,
            });
        }

        var region_relocations = std.ArrayList(NamedRelocation).empty;
        for (relocations) |relocation| {
            switch (relocation) {
                .retired => continue,
                .local_data, .jmp_to_return => return error.UnsupportedRelocation,
                .linked_function, .linked_data => {},
            }
            const offset = relocation.getOffset();
            if (offset < region.start or offset >= region.end) continue;
            try region_relocations.append(arena_allocator, switch (relocation) {
                .linked_function => |function| .{
                    .offset = @intCast(offset - region.start),
                    .name = try arena_allocator.dupe(u8, codegen.symbolName(function.symbol)),
                    .scope = codegen.symbolScope(function.symbol),
                    .kind = .function,
                },
                .linked_data => |data| .{
                    .offset = @intCast(offset - region.start),
                    .name = try arena_allocator.dupe(u8, codegen.symbolName(data.symbol)),
                    .scope = codegen.symbolScope(data.symbol),
                    .kind = .{ .data = data.kind },
                },
                .retired, .local_data, .jmp_to_return => unreachable,
            });
        }

        // Every data item the region names, then every item those name, so
        // an artifact carries the whole constant graph it points into.
        const region_data = try captureData(allocator, arena_allocator, region_relocations.items, &data_by_name, prepared);
        for (region_data) |*item| for (@constCast(item.relocations)) |*relocation| {
            const target = relocation.code_target orelse continue;
            const offset: usize = switch (target) {
                .local => return error.DanglingReference, // A borrowed image must carry image offsets, not another set's indices.
                .image_offset => |offset| offset,
                .symbolic => |symbol| switch (symbol) {
                    .proc => |identity| blk: {
                        const proc = producer_procs.get(identity) orelse continue;
                        const compiled = codegen.compiledProcSymbol(proc) orelse continue;
                        break :blk compiled.code_start;
                    },
                    .boxy_thunk => |identity| blk: {
                        const proc = producer_procs.get(identity) orelse continue;
                        break :blk codegen.boxyThunkOffset(proc) orelse continue;
                    },
                    .rc_helper => |name| codegen.splicedHelperEntry(name) orelse continue,
                },
            };
            const target_region = regionContaining(CG, regions, offset) orelse return error.DanglingReference;
            const target_index = artifact_of_region[target_region] orelse return error.DanglingReference;
            relocation.addend += @as(i64, @intCast(offset - regions[target_region].start)) - @as(i64, @intCast(regions[target_region].entry));
            relocation.code_target = .{ .local = target_index };
        };

        var lines = std.ArrayList(LineEntry).empty;
        for (codegen.getLineEntries()) |line| {
            if (line.offset < region.start or line.offset >= region.end) continue;
            try lines.append(arena_allocator, .{ .offset = @intCast(line.offset - region.start), .loc = line.loc });
        }
        const artifact_code = try arena_allocator.dupe(u8, code[region.start..region.end]);
        codegen.normalizeArtifactCode(region.start, artifact_code);
        var context_bindings = std.ArrayList(CtfeContext.Binding).empty;
        var context_relocations = std.ArrayList(ContextImmediate.Relocation).empty;
        if (@hasField(CG, "context_relocations")) {
            for (codegen.context_relocations.items) |relocation| {
                if (relocation.offset < region.start or relocation.offset >= region.end) continue;
                var local = relocation;
                local.offset -= @intCast(region.start);
                local.binding = @intCast(context_bindings.items.len);
                try context_bindings.append(arena_allocator, try codegen.context_bindings.items[relocation.binding].clone(arena_allocator));
                try context_relocations.append(arena_allocator, local);
            }
        }
        artifacts[index] = .{
            .kind = kind,
            .producer_proc = switch (region.kind) {
                .proc, .boxy_thunk => |proc| proc,
                else => null,
            },
            .producer_helper = switch (region.kind) {
                .rc_helper => |key| key,
                else => null,
            },
            .source_observations = region.artifact_source_observations orelse switch (region.kind) {
                .proc => |proc| proc_specs[@intFromEnum(proc)].source_observations,
                .rc_helper, .boxy_thunk, .message_pool_run => .absent,
                else => .unknown,
            },
            .requires_ctfe_observations = region.requires_ctfe_observations orelse
                (if (region.kind == .message_pool_run) @as(?bool, false) else null),
            .code = artifact_code,
            .callable_contract = try callableContract(CG, codegen, region, proc_specs, layout_store),
            .entry = @intCast(region.entry),
            .frame = frame,
            .refs = try region_refs.toOwnedSlice(arena_allocator),
            .symbolic_refs = try symbolic_refs.toOwnedSlice(arena_allocator),
            .lines = try lines.toOwnedSlice(arena_allocator),
            .relocations = try region_relocations.toOwnedSlice(arena_allocator),
            .data = region_data,
            .domain = region.artifact_domain orelse if (@hasDecl(CG, "getFragmentContextDependencies")) (if ((region.context_dependencies orelse codegen.getFragmentContextDependencies()).comptime_hooks) .ctfe else .runtime) else .runtime,
            .context_contract = region.artifact_context_contract orelse if (@hasDecl(CG, "getFragmentContract")) codegen.getFragmentContract() else null,
            .context_dependencies = if (@hasDecl(CG, "getFragmentContextDependencies")) region.context_dependencies orelse codegen.getFragmentContextDependencies() else null,
            .context_complete = if (@hasField(CG, "context_complete")) region.context_complete orelse codegen.context_complete else true,
            .context_bindings = try context_bindings.toOwnedSlice(arena_allocator),
            .context_relocations = try context_relocations.toOwnedSlice(arena_allocator),
        };
    }

    return .{ .arena = arena, .artifacts = artifacts };
}

fn captureData(
    allocator: Allocator,
    arena_allocator: Allocator,
    relocations: []const NamedRelocation,
    local: *const std.StringHashMap(DataItem),
    prepared: *const PreparedData,
) ExtractError![]const DataItem {
    var data = std.ArrayList(DataItem).empty;
    var pending = std.ArrayList([]const u8).empty;
    defer pending.deinit(allocator);
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();
    for (relocations) |relocation| {
        const item = local.get(relocation.name) orelse prepared.items.get(relocation.name) orelse continue;
        try pending.append(allocator, item.name);
    }
    while (pending.pop()) |name| {
        const visited = try seen.getOrPut(name);
        if (visited.found_existing) continue;
        const item = local.get(name) orelse prepared.items.get(name) orelse return error.DanglingReference;
        try data.append(arena_allocator, try cloneData(arena_allocator, item));
        for (item.relocations) |relocation| {
            if (relocation.function or relocation.external) continue;
            try pending.append(allocator, relocation.name);
        }
    }
    return try data.toOwnedSlice(arena_allocator);
}

/// Whether a region lifts into an artifact. Branch islands belong to one
/// placement of the code, and a hosted stub exists only in the evaluator's
/// image.
fn regionHasArtifact(kind: anytype) bool {
    return switch (kind) {
        .branch_island, .hosted_stub => false,
        .proc, .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => true,
    };
}

fn regionStartsBefore(comptime Region: type) fn (void, Region, Region) bool {
    return struct {
        fn lessThan(_: void, lhs: Region, rhs: Region) bool {
            return lhs.start < rhs.start;
        }
    }.lessThan;
}

fn regionContaining(comptime CG: type, regions: []const CG.CodeRegion, offset: usize) ?usize {
    var low: usize = 0;
    var high: usize = regions.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        const region = regions[mid];
        if (offset < region.start) {
            high = mid;
        } else if (offset >= region.end) {
            low = mid + 1;
        } else {
            return mid;
        }
    }
    return null;
}

/// Code offset of a message pool byte: the last pool run that starts at or
/// before the message, plus the message's distance into that run.
fn messageOffsetInCode(comptime CG: type, regions: []const CG.CodeRegion, message_offset: u32) ?usize {
    var found: ?usize = null;
    for (regions) |region| {
        switch (region.kind) {
            .message_pool_run => |pool_from| {
                if (pool_from <= message_offset) found = region.start + (message_offset - pool_from);
            },
            .proc, .rc_helper, .boxy_thunk, .entrypoint, .branch_island, .hosted_stub, .spliced_proc, .spliced_boxy_thunk, .spliced_helper => {},
        }
    }
    return found;
}

/// Why an artifact set could not be placed into a program.
pub const AssembleError = Allocator.Error || error{
    /// A procedure or thunk artifact names an identity the program has no proc for.
    UnknownProcIdentity,
    /// A helper artifact names a helper the program has no key for.
    UnknownRcHelper,
};

/// Keys of the refcount helpers a program can name, by symbol name. A program
/// assembling artifacts from its own compilation fills this from the original
/// code generator's helpers.
pub const HelperKeys = std.StringHashMap(u64);

/// Append every artifact of `set` to an open code generator in set order and
/// re-resolve every reference to where its target landed.
pub fn assemble(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    proc_specs: []const lir.LIR.LirProcSpec,
    helper_keys: *const HelperKeys,
) AssembleError!void {
    try append(CG, allocator, codegen, set, proc_specs, helper_keys);
    try codegen.resolveAssembledSymbolicRefs();
}

/// Append a set without requiring externally referenced code to exist yet.
/// Resolve queued references only after all module fragments and helpers land.
pub fn append(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    proc_specs: []const lir.LIR.LirProcSpec,
    helper_keys: *const HelperKeys,
) AssembleError!void {
    var procs_by_identity = std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId).init(allocator);
    defer procs_by_identity.deinit();
    for (proc_specs, 0..) |proc, index| {
        if (proc.is_static_initializer) continue;
        try procs_by_identity.put(proc.identity, @enumFromInt(@as(u32, @intCast(index))));
    }

    try appendPrepared(CG, allocator, codegen, set, &procs_by_identity, helper_keys);
}

/// Append using the coordinator's shared identity index. Reusing this index
/// avoids rescanning the module's procedures for every independent fragment.
pub fn appendPrepared(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    procs_by_identity: *const std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId),
    helper_keys: *const HelperKeys,
) AssembleError!void {
    const starts = try allocator.alloc(usize, set.artifacts.len);
    defer allocator.free(starts);

    for (set.artifacts, 0..) |artifact, index| {
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc => |identity| .{ .proc = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
            .rc_helper => |name| .{ .rc_helper = helper_keys.get(name) orelse return error.UnknownRcHelper },
            .boxy_thunk => |identity| .{ .boxy_thunk = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        starts[index] = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        try appendMetadata(CG, codegen, artifact, starts[index], true, true);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name, relocation.scope);
            const offset: u64 = starts[index] + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => .{ .linked_function = .{ .offset = offset, .symbol = symbol } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }

    for (set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| {
            const target_artifact = set.artifacts[ref.target];
            const target: CG.CodeRefTarget = switch (target_artifact.kind) {
                .proc => |identity| .{ .proc = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
                .rc_helper => |name| .{ .rc_helper = helper_keys.get(name) orelse return error.UnknownRcHelper },
                .boxy_thunk => |identity| .{ .boxy_thunk = procs_by_identity.get(identity) orelse return error.UnknownProcIdentity },
                // Imported pools do not share the destination's message-ID
                // domain. Preserve this exact placed target for later extraction.
                .message_pool_run, .entrypoint, .branch_island => .{ .offset = starts[ref.target] + ref.delta },
            };
            try codegen.patchAssembledRef(
                starts[index] + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                target,
                starts[ref.target] + ref.delta,
            );
        }
    }
}

fn appendMetadata(comptime CG: type, codegen: *CG, artifact: Artifact, start: usize, include_lines: bool, bind_symbols: bool) Allocator.Error!void {
    if (@hasDecl(CG, "recordAssembledArtifact")) try codegen.recordAssembledArtifact(start, artifact);
    if (@hasField(CG, "context_bindings")) {
        codegen.context_complete = codegen.context_complete and artifact.context_complete;
        if (artifact.context_dependencies) |dependencies| {
            codegen.fragment_context.merge(dependencies);
        }
        if (artifact.context_bindings.len != 0) {
            if (codegen.context_import_arena == null) codegen.context_import_arena = std.heap.ArenaAllocator.init(codegen.allocator);
            const a = codegen.context_import_arena.?.allocator();
            const base: u32 = @intCast(codegen.context_bindings.items.len);
            for (artifact.context_bindings) |binding| try codegen.context_bindings.append(codegen.allocator, try binding.clone(a));
            for (artifact.context_relocations) |relocation| {
                var placed = relocation;
                placed.offset += @intCast(start);
                placed.binding += base;
                try codegen.context_relocations.append(codegen.allocator, placed);
            }
        }
    }
    if (bind_symbols) switch (artifact.kind) {
        .proc => |identity| try codegen.registerAssembledProc(identity, start + artifact.entry),
        .rc_helper => |name| try codegen.registerSplicedHelper(name, start + artifact.entry),
        .boxy_thunk => |identity| try codegen.registerAssembledThunk(identity, start + artifact.entry),
        .entrypoint, .message_pool_run, .branch_island => {},
    };
    if (bind_symbols) for (artifact.symbolic_refs) |ref| {
        var rebased = ref;
        rebased.site = @intCast(start + ref.site);
        rebased.veneer = if (ref.veneer) |offset| @intCast(start + offset) else null;
        try codegen.queueAssembledSymbolicRef(rebased);
    };
    for (artifact.refs) |ref| {
        if (ref.form == .call) {
            try codegen.registerAssembledRefVeneer(start + ref.site, if (ref.veneer) |veneer| start + veneer else null);
        }
    }
    if (include_lines) {
        for (artifact.lines) |line| {
            try codegen.appendAssembledLineEntry(.{ .offset = @intCast(start + line.offset), .loc = line.loc });
        }
    }
}

/// Why a program and its reassembled artifacts disagree.
pub const RoundTripError = ExtractError || AssembleError || error{
    /// The assembled program differs from the compiled one.
    RoundTripMismatch,
};

/// Assemble `original`'s artifacts into `fresh` (an open code generator built
/// with the same inputs) and check that the two agree on code bytes,
/// relocations, unwind records, and region layout.
pub fn verifyRoundTrip(
    comptime CG: type,
    allocator: Allocator,
    original: *CG,
    fresh: *CG,
    proc_specs: []const lir.LIR.LirProcSpec,
    layout_store: *const layout.Store,
    string_exports: []const lir.Program.StaticDataExport,
) RoundTripError!void {
    var set = try extract(CG, allocator, original, proc_specs, layout_store, string_exports, &.{}, &.{});
    defer set.deinit();

    var helper_keys = HelperKeys.init(allocator);
    defer {
        var names = helper_keys.keyIterator();
        while (names.next()) |name| allocator.free(name.*);
        helper_keys.deinit();
    }
    const helpers = try original.compiledRcHelpers(allocator);
    defer allocator.free(helpers);
    for (helpers) |helper| {
        const name = try LirCodeGenMod.compiledRcHelperSymbolName(allocator, layout_store, helper.key);
        errdefer allocator.free(name);
        try helper_keys.putNoClobber(name, helper.key);
    }

    try assemble(CG, allocator, fresh, &set, proc_specs, &helper_keys);
    try fresh.finishImage();

    const original_code = original.getGeneratedCode();
    const fresh_code = fresh.getGeneratedCode();
    if (!std.mem.eql(u8, original_code, fresh_code)) {
        const common = @min(original_code.len, fresh_code.len);
        var first_diff: usize = common;
        for (0..common) |i| {
            if (original_code[i] != fresh_code[i]) {
                first_diff = i;
                break;
            }
        }
        std.debug.print("ROUNDTRIP code mismatch: original len {d}, fresh len {d}, first difference at {d}\n", .{ original_code.len, fresh_code.len, first_diff });
        for (original.codeRegions()) |region| {
            if (first_diff >= region.start and first_diff < region.end) std.debug.print("  original region {s} [{d}, {d}) entry {d}\n", .{ @tagName(region.kind), region.start, region.end, region.entry });
        }
        for (fresh.codeRegions()) |region| {
            if (first_diff >= region.start and first_diff < region.end) std.debug.print("  fresh region {s} [{d}, {d}) entry {d}\n", .{ @tagName(region.kind), region.start, region.end, region.entry });
        }
        if (first_diff < common) {
            const lo = if (first_diff >= 8) first_diff - 8 else 0;
            const hi = @min(common, first_diff + 8);
            std.debug.print("  original bytes {x}\n  fresh bytes    {x}\n", .{ original_code[lo..hi], fresh_code[lo..hi] });
        }
        return error.RoundTripMismatch;
    }
    if (!relocationsMatch(CG, original, fresh)) {
        std.debug.print("ROUNDTRIP relocation mismatch: original {d}, fresh {d}\n", .{ original.getRelocations().len, fresh.getRelocations().len });
        return error.RoundTripMismatch;
    }
    if (!unwindMatches(original.getUnwindFunctions(), fresh.getUnwindFunctions())) {
        std.debug.print("ROUNDTRIP unwind mismatch: original {d}, fresh {d}\n", .{ original.getUnwindFunctions().len, fresh.getUnwindFunctions().len });
        return error.RoundTripMismatch;
    }
    if (!unwindMatches(original.getLineEntries(), fresh.getLineEntries())) {
        std.debug.print("ROUNDTRIP source line mismatch\n", .{});
        return error.RoundTripMismatch;
    }
    if (artifactRegionCount(CG, original.codeRegions()) != artifactRegionCount(CG, fresh.codeRegions())) {
        std.debug.print("ROUNDTRIP region count mismatch: original {d}, fresh {d}\n", .{ original.codeRegions().len, fresh.codeRegions().len });
        return error.RoundTripMismatch;
    }
}

/// Regions that become artifacts: everything but branch islands, which each
/// placement lays out for itself.
fn artifactRegionCount(comptime CG: type, regions: []const CG.CodeRegion) usize {
    var count: usize = 0;
    for (regions) |region| count += @intFromBool(regionHasArtifact(region.kind));
    return count;
}

fn relocationsMatch(comptime CG: type, original: *CG, fresh: *CG) bool {
    var original_index: usize = 0;
    var fresh_index: usize = 0;
    const original_relocations = original.getRelocations();
    const fresh_relocations = fresh.getRelocations();
    while (true) {
        while (original_index < original_relocations.len and original_relocations[original_index] == .retired) original_index += 1;
        while (fresh_index < fresh_relocations.len and fresh_relocations[fresh_index] == .retired) fresh_index += 1;
        const original_done = original_index == original_relocations.len;
        const fresh_done = fresh_index == fresh_relocations.len;
        if (original_done or fresh_done) return original_done and fresh_done;
        const lhs = original_relocations[original_index];
        const rhs = fresh_relocations[fresh_index];
        if (!relocationEql(CG, original, fresh, lhs, rhs)) return false;
        original_index += 1;
        fresh_index += 1;
    }
}

fn relocationEql(comptime CG: type, original: *CG, fresh: *CG, lhs: IndexedRelocation, rhs: IndexedRelocation) bool {
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;
    return switch (lhs) {
        .linked_function => |function| function.offset == rhs.linked_function.offset and
            std.mem.eql(u8, original.symbolName(function.symbol), fresh.symbolName(rhs.linked_function.symbol)),
        .linked_data => |data| data.offset == rhs.linked_data.offset and data.kind == rhs.linked_data.kind and
            std.mem.eql(u8, original.symbolName(data.symbol), fresh.symbolName(rhs.linked_data.symbol)),
        .local_data, .jmp_to_return, .retired => false,
    };
}

fn unwindMatches(original: anytype, fresh: @TypeOf(original)) bool {
    if (original.len != fresh.len) return false;
    for (original, fresh) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}

/// Where an artifact from another program's pack lands in this program: on
/// a procedure this program declares (external or its own), or as code that
/// only other spliced artifacts reach.
pub const SpliceError = Allocator.Error;

/// Place the closure of `roots` (every artifact they reach through
/// references, transitively) from `set` into an open code generator, before
/// any of the program's own procedures compile. A procedure artifact whose
/// identity this program declares registers as that procedure, so the
/// program's own compile skips it and every call to it is an ordinary
/// direct call; refcount helpers register by name so a later request for the
/// same helper reuses the spliced code. `placed` remembers which artifacts of
/// `set` are already in the buffer across calls.
/// Worker identities also name Boxy thunks, but the two ABIs have separate
/// registries and region kinds so neither can satisfy demand for the other.
pub fn splice(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    set: *const Set,
    roots: []const u32,
    procs_by_identity: *const std.AutoHashMap(lir.ProcIdentity, lir.LIR.LirProcSpecId),
    placed: *std.AutoHashMap(u32, usize),
    data_out: *std.ArrayList(DataItem),
) SpliceError!void {
    var graph = try @import("ArtifactClosure.zig").init(allocator, set);
    defer graph.deinit();
    var bindings = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
    defer bindings.deinit();
    for (roots) |root| {
        const identity = switch (set.artifacts[root].kind) {
            .proc, .boxy_thunk => |identity| identity,
            else => continue,
        };
        if (procs_by_identity.get(identity)) |proc| try bindings.put(root, proc);
    }
    return spliceIndexed(CG, allocator, codegen, &graph, roots, &bindings, placed, data_out);
}

/// The caller retains one graph per source set across all root placements.
pub fn spliceIndexed(
    comptime CG: type,
    allocator: Allocator,
    codegen: *CG,
    graph: *const @import("ArtifactClosure.zig"),
    roots: []const u32,
    /// Exact serving definitions selected for the consumer's LIR procedures.
    /// Dependency variants with the same logical identity do not acquire this
    /// binding merely because another root is selected for that procedure.
    bindings: *const std.AutoHashMap(u32, lir.LIR.LirProcSpecId),
    placed: *std.AutoHashMap(u32, usize),
    data_out: *std.ArrayList(DataItem),
) SpliceError!void {
    const set = graph.set;
    // Closure in first-discovery order: deterministic for a deterministic
    // root order, and every target is placed before its references resolve.
    var order = std.ArrayList(u32).empty;
    defer order.deinit(allocator);
    var stack = std.ArrayList(u32).empty;
    defer stack.deinit(allocator);
    var seen = std.AutoHashMap(u32, void).init(allocator);
    defer seen.deinit();
    for (roots) |root| try stack.append(allocator, root);
    while (stack.pop()) |index| {
        if (placed.contains(index)) continue;
        const gop = try seen.getOrPut(index);
        if (gop.found_existing) continue;
        // Only this physical artifact's placement proves reuse. A logical
        // symbol can name multiple compiled variants with different closures.
        try order.append(allocator, index);
        const refs = set.artifacts[index].refs;
        var i = refs.len;
        while (i > 0) {
            i -= 1;
            try stack.append(allocator, refs[i].target);
        }
        // A pack may combine independent fragments. Stable references to
        // definitions in this pack participate in the same transitive closure.
        for (graph.symbolicTargets(index)) |target| {
            if (target) |candidate_index| try stack.append(allocator, candidate_index);
        }
        for (graph.dataTargets(index)) |target| {
            if (target) |candidate_index| try stack.append(allocator, candidate_index);
        }
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const kind: CG.CodeRegionKind = switch (artifact.kind) {
            .proc => |identity| if (bindings.get(index)) |proc_id| .{ .proc = proc_id } else .{ .spliced_proc = identity },
            .boxy_thunk => |identity| if (bindings.get(index)) |proc_id| .{ .boxy_thunk = proc_id } else .{ .spliced_boxy_thunk = identity },
            .rc_helper => .spliced_helper,
            .entrypoint => .entrypoint,
            .message_pool_run => .{ .message_pool_run = 0 },
            .branch_island => .branch_island,
        };
        const start = try codegen.appendAssembledRegion(artifact.code, kind, artifact.entry, artifact.frame);
        // Persistent packs do not carry a source-file domain binding. Keep
        // their exact stored lines, but do not misinterpret their file indices
        // as belonging to this program. Session append does preserve lines.
        try appendMetadata(CG, codegen, artifact, start, false, false);
        if (bindings.contains(index) or std.mem.indexOfScalar(u32, roots, index) != null) switch (artifact.kind) {
            .proc => |identity| try codegen.bindAssembledProc(identity, start + artifact.entry),
            .boxy_thunk => |identity| try codegen.registerAssembledThunk(identity, start + artifact.entry),
            .rc_helper => |name| try codegen.registerSplicedHelper(name, start + artifact.entry),
            .entrypoint, .message_pool_run, .branch_island => {},
        };
        try placed.putNoClobber(index, start);
        for (artifact.relocations) |relocation| {
            const symbol = try codegen.internSymbolName(relocation.name, relocation.scope);
            const offset: u64 = start + relocation.offset;
            try codegen.appendAssembledRelocation(switch (relocation.kind) {
                .function => .{ .linked_function = .{ .offset = offset, .symbol = symbol } },
                .data => |data_kind| .{ .linked_data = .{ .offset = offset, .symbol = symbol, .kind = data_kind } },
            });
        }
    }

    // All physical code entries exist before any frozen function pointer is
    // bound. Keep its exact image offset for subsequent artifact extraction.
    for (order.items) |index| {
        var cursor: usize = 0;
        for (set.artifacts[index].data) |item| {
            var rebound = item;
            var copied = false;
            for (item.relocations, 0..) |relocation, ref_index| {
                if (relocation.code_target == null) continue;
                if (!copied) {
                    if (codegen.context_import_arena == null) codegen.context_import_arena = std.heap.ArenaAllocator.init(codegen.allocator);
                    rebound.relocations = try codegen.context_import_arena.?.allocator().dupe(DataRelocation, item.relocations);
                    copied = true;
                }
                const target_index = graph.dataTargets(index)[cursor] orelse unreachable;
                cursor += 1;
                const target_start = placed.get(target_index) orelse unreachable;
                const target_entry = set.artifacts[target_index].entry;
                const symbol = try codegen.internRegionCodeSymbol(target_start, target_entry);
                const target = &@constCast(rebound.relocations)[ref_index];
                target.name = codegen.symbolName(symbol);
                target.code_target = .{ .image_offset = target_start + target_entry };
            }
            try data_out.append(allocator, rebound);
        }
    }

    for (order.items) |index| {
        const artifact = set.artifacts[index];
        const start = placed.get(index) orelse unreachable;
        for (artifact.refs) |ref| {
            const target_start = placed.get(ref.target) orelse unreachable;
            // Indexed edges already name the compiled definition. A logical
            // procedure ID is not permission to rebind that edge when another
            // variant of the same function is assembled later.
            const target: CG.CodeRefTarget = .{ .offset = target_start + ref.delta };
            try codegen.patchAssembledRef(
                start + ref.site,
                switch (ref.form) {
                    .call => .call,
                    .inline_call => .inline_call,
                    .addr => .addr,
                },
                target,
                target_start + ref.delta,
            );
        }
        for (artifact.symbolic_refs, graph.symbolicTargets(index)) |ref, resolved| {
            const target_index = resolved orelse unreachable; // Serving admission proved this edge.
            const target_start = placed.get(target_index) orelse unreachable;
            const target = target_start + set.artifacts[target_index].entry;
            try codegen.patchAssembledRef(start + ref.site, switch (ref.form) {
                .call => .call,
                .inline_call => .inline_call,
                .addr => .addr,
            }, .{ .offset = target }, target);
        }
    }
}

test "indexed artifact calls retain exact placement despite logical procedure mapping" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.arm64linux);
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, .u64);
    defer layouts.deinit();
    const identity = lir.ProcIdentity.forTest(17);
    const message = try store.insertString("unused source body");
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = identity,
        .args = .empty(),
        .body = try store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = message } } }, .test_fixture),
        .ret_layout = .zst,
    }, .none);
    var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer image.deinit();
    const set = Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{
            .{
                .kind = .entrypoint,
                .code = "\x00\x00\x00\x94",
                .entry = 0,
                .frame = null,
                .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }},
                .relocations = &.{},
                .data = &.{},
            },
            .{
                .kind = .{ .proc = identity },
                .code = "\xc0\x03\x5f\xd6",
                .entry = 0,
                .frame = .{
                    .prologue_size = 0,
                    .stack_alloc = 0,
                    .frame_size = 0,
                    .callee_saved_mask = 0,
                    .epilogue_offset = 0,
                    .uses_frame_pointer = false,
                },
                .refs = &.{},
                .relocations = &.{},
                .data = &.{},
            },
        },
    };
    var graph = try @import("ArtifactClosure.zig").init(allocator, &set);
    defer graph.deinit();
    var procs = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
    defer procs.deinit();
    try procs.put(1, proc);
    var placed = std.AutoHashMap(u32, usize).init(allocator);
    defer placed.deinit();
    var data = std.ArrayList(DataItem).empty;
    defer data.deinit(allocator);
    try spliceIndexed(CG, allocator, &image, &graph, &.{0}, &procs, &placed, &data);
    try std.testing.expectEqual(@as(usize, 1), image.code_refs.items.len);
    try std.testing.expectEqualDeep(CG.CodeRefTarget{ .offset = placed.get(1).? }, image.code_refs.items[0].target);
}

test "indexed artifact placement preserves duplicate logical definitions in either root order" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.arm64linux);
    const identity = lir.ProcIdentity.forTest(23);
    const leaf = Artifact{
        .kind = .{ .proc = identity },
        .code = "\xc0\x03\x5f\xd6",
        .entry = 0,
        .frame = .{
            .prologue_size = 0,
            .stack_alloc = 0,
            .frame_size = 0,
            .callee_saved_mask = 0,
            .epilogue_offset = 0,
            .uses_frame_pointer = false,
        },
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    const caller = Artifact{
        .kind = .entrypoint,
        .code = "\x00\x00\x00\x94\xc0\x03\x5f\xd6",
        .entry = 0,
        .frame = null,
        .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }},
        .relocations = &.{},
        .data = &.{},
    };
    var artifacts = [_]Artifact{ caller, leaf, leaf, caller };
    artifacts[3].refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
    const set = Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    inline for (.{ false, true }) |reverse| {
        var store = lir.LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        const message = try store.insertString("unused source body");
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .identity = identity,
            .args = .empty(),
            .body = try store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = message } } }, .test_fixture),
            .ret_layout = .zst,
        }, .none);
        var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer image.deinit();
        var graph = try @import("ArtifactClosure.zig").init(allocator, &set);
        defer graph.deinit();
        var bindings = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
        defer bindings.deinit();
        try bindings.put(2, proc);
        var placed = std.AutoHashMap(u32, usize).init(allocator);
        defer placed.deinit();
        var data = std.ArrayList(DataItem).empty;
        defer data.deinit(allocator);
        const roots = if (reverse) [_]u32{ 3, 0 } else [_]u32{ 0, 3 };
        for (roots) |root| try spliceIndexed(CG, allocator, &image, &graph, &.{root}, &bindings, &placed, &data);
        try std.testing.expectEqual(@as(usize, 4), placed.count());
        try std.testing.expect(placed.get(1).? != placed.get(2).?);
        try std.testing.expectEqual(placed.get(2).?, image.compiledProcSymbol(proc).?.code_start);
        for (image.code_refs.items) |ref| {
            const expected = if (ref.site == placed.get(0).?) placed.get(1).? else placed.get(2).?;
            try std.testing.expectEqualDeep(CG.CodeRefTarget{ .offset = expected }, ref.target);
        }
        try image.finishImage();
        var recaptured = try extract(CG, allocator, &image, store.getProcSpecs(), &layouts, &.{}, &.{}, &.{});
        defer recaptured.deinit();
        var definitions: usize = 0;
        for (recaptured.artifacts) |artifact| {
            if (artifact.kind == .proc and std.meta.eql(artifact.kind.proc, identity)) definitions += 1;
        }
        try std.testing.expectEqual(@as(usize, 2), definitions);
    }
}

test "indexed symbolic calls keep their source namespace after another variant was placed" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.arm64linux);
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, .u64);
    defer layouts.deinit();
    var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer image.deinit();
    const identity = lir.ProcIdentity.forTest(47);
    const leaf = Artifact{
        .kind = .{ .proc = identity },
        .code = "\xc0\x03\x5f\xd6",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    const previous = Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &.{leaf} };
    const source = Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{
            .{
                .kind = .entrypoint,
                .code = "\x00\x00\x00\x94\xc0\x03\x5f\xd6",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = identity } }},
                .relocations = &.{},
                .data = &.{},
            },
            leaf,
        },
    };
    var first_graph = try @import("ArtifactClosure.zig").init(allocator, &previous);
    defer first_graph.deinit();
    var graph = try @import("ArtifactClosure.zig").init(allocator, &source);
    defer graph.deinit();
    var bindings = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
    defer bindings.deinit();
    var first_placed = std.AutoHashMap(u32, usize).init(allocator);
    defer first_placed.deinit();
    var placed = std.AutoHashMap(u32, usize).init(allocator);
    defer placed.deinit();
    var data = std.ArrayList(DataItem).empty;
    defer data.deinit(allocator);
    try spliceIndexed(CG, allocator, &image, &first_graph, &.{0}, &bindings, &first_placed, &data);
    try spliceIndexed(CG, allocator, &image, &graph, &.{0}, &bindings, &placed, &data);
    try std.testing.expect(first_placed.get(0).? != placed.get(1).?);
    try std.testing.expectEqual(first_placed.get(0).?, image.splicedProcStart(identity).?);
    try std.testing.expectEqual(@as(usize, 0), image.assembled_symbolic_refs.items.len);
    try std.testing.expectEqual(@as(usize, 1), image.code_refs.items.len);
    try std.testing.expectEqualDeep(CG.CodeRefTarget{ .offset = placed.get(1).? }, image.code_refs.items[0].target);
    try image.finishImage();
}

test "frozen callable names distinguish physical graphs with equal logical identities and ordinals" {
    const allocator = std.testing.allocator;
    var artifacts = [_]Artifact{
        .{
            .kind = .entrypoint,
            .code = "",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{.{
                .name = "roc__d0",
                .bytes = "\x00" ** 8,
                .alignment = 8,
                .symbol_offset = 0,
                .program_local_name = true,
                .relocations = &.{.{
                    .offset = 0,
                    .name = "same-logical-function",
                    .addend = 0,
                    .function = true,
                    .code_target = .{ .local = 1 },
                }},
            }},
        },
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(67) },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
            .context_dependencies = .{},
        },
    };
    const set = Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    var first = try ContentNames.init(allocator, &set);
    defer first.deinit();
    artifacts[1].context_dependencies.?.dict_seed = true;
    var second = try ContentNames.init(allocator, &set);
    defer second.deinit();
    try std.testing.expect(!std.mem.eql(u8, first.of("roc__d0"), second.of("roc__d0")));
    var again = try ContentNames.init(allocator, &set);
    defer again.deinit();
    try std.testing.expectEqualStrings(second.of("roc__d0"), again.of("roc__d0"));
}

test "frozen callable placement and recapture retain exact duplicate variants" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.x64linux);
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, .u64);
    defer layouts.deinit();
    var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer image.deinit();
    const caller = Artifact{
        .kind = .entrypoint,
        .code = "\x48\x8b\x05\x00\x00\x00\x00\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{.{ .offset = 3, .name = "first", .scope = .shared, .kind = .{ .data = .rel32 } }},
        .data = &.{.{
            .name = "first",
            .bytes = "\x00" ** 8,
            .alignment = 8,
            .symbol_offset = 0,
            .relocations = &.{.{ .offset = 0, .name = "logical-function", .addend = 0, .function = true, .code_target = .{ .local = 1 } }},
        }},
    };
    const leaf = Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(71) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    var artifacts = [_]Artifact{ caller, leaf, leaf, caller };
    artifacts[3].relocations = &.{.{ .offset = 3, .name = "second", .scope = .shared, .kind = .{ .data = .rel32 } }};
    artifacts[3].data = &.{.{
        .name = "second",
        .bytes = "\x00" ** 8,
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 0, .name = "logical-function", .addend = 0, .function = true, .code_target = .{ .local = 2 } }},
    }};
    const set = Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    var graph = try @import("ArtifactClosure.zig").init(allocator, &set);
    defer graph.deinit();
    var bindings = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
    defer bindings.deinit();
    var placed = std.AutoHashMap(u32, usize).init(allocator);
    defer placed.deinit();
    var data = std.ArrayList(DataItem).empty;
    defer data.deinit(allocator);
    try spliceIndexed(CG, allocator, &image, &graph, &.{ 0, 3 }, &bindings, &placed, &data);
    try std.testing.expectEqual(@as(usize, 4), placed.count());
    try std.testing.expectEqual(@as(usize, 2), data.items.len);
    const first_index: usize = if (std.mem.eql(u8, data.items[0].name, "first")) 0 else 1;
    const first = data.items[first_index].relocations[0];
    const second = data.items[1 - first_index].relocations[0];
    try std.testing.expectEqual(placed.get(1).?, first.code_target.?.image_offset);
    try std.testing.expectEqual(placed.get(2).?, second.code_target.?.image_offset);
    try std.testing.expect(!std.mem.eql(u8, first.name, second.name));
    try image.finishImage();
    var recaptured = try extract(CG, allocator, &image, &.{}, &layouts, &.{}, &.{}, data.items);
    defer recaptured.deinit();
    var targets: [2]?u32 = .{ null, null };
    for (recaptured.artifacts) |artifact| {
        for (artifact.data) |item| {
            const index: usize = if (std.mem.eql(u8, item.name, "first")) 0 else 1;
            targets[index] = item.relocations[0].code_target.?.local;
        }
    }
    try std.testing.expect(targets[0] != null and targets[1] != null);
    try std.testing.expect(targets[0].? != targets[1].?);
    for (targets) |target| {
        try std.testing.expectEqualDeep(leaf.kind, recaptured.artifacts[target.?].kind);
    }
}

test "produced references bind selected declarations without rebinding cached siblings" {
    const allocator = std.testing.allocator;
    const identity = lir.ProcIdentity.forTest(91);
    const leaf = Artifact{
        .kind = .{ .proc = identity },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    var artifacts = [_]Artifact{ leaf, leaf, leaf, leaf };
    artifacts[0].kind = .{ .proc = lir.ProcIdentity.forTest(90) };
    artifacts[0].producer_proc = @enumFromInt(0);
    artifacts[0].symbolic_refs = &.{
        .{ .site = 0, .form = .call, .target = .{ .proc = identity } },
        .{ .site = 1, .form = .call, .target = .{ .rc_helper = "helper" } },
    };
    artifacts[0].data = &.{.{
        .name = "callable",
        .bytes = "\x00" ** 8,
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 0, .name = "logical-function", .addend = 0, .function = true, .code_target = .{ .symbolic = .{ .proc = identity } } }},
    }};
    artifacts[1].producer_proc = @enumFromInt(1);
    artifacts[2].refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
    artifacts[3].kind = .{ .rc_helper = "helper" };
    const source = Set{ .arena = std.heap.ArenaAllocator.init(allocator), .artifacts = &artifacts };
    var set = try combine(allocator, &.{&source});
    defer set.deinit();
    try bindProducedReferences(&set, 1, &.{3});
    try std.testing.expectEqual(@as(usize, 0), set.artifacts[0].symbolic_refs.len);
    try std.testing.expectEqual(@as(u32, 1), set.artifacts[0].refs[0].target);
    try std.testing.expectEqual(@as(u32, 3), set.artifacts[0].refs[1].target);
    try std.testing.expectEqual(@as(u32, 1), set.artifacts[0].data[0].relocations[0].code_target.?.local);
    try std.testing.expectEqualDeep(artifacts[2], set.artifacts[2]);
    var missing = try combine(allocator, &.{&source});
    defer missing.deinit();
    try bindProducedReferences(&missing, 1, &.{});
    try std.testing.expect(!missing.artifacts[0].context_complete);
    try std.testing.expectEqual(@as(usize, 1), missing.artifacts[0].symbolic_refs.len);
    try std.testing.expectEqualDeep(artifacts[2], missing.artifacts[2]);
}

test "independent message pools preserve their targets across repeated artifact assembly" {
    const allocator = std.testing.allocator;
    inline for (.{ @import("roc_target").RocTarget.x64linux, @import("roc_target").RocTarget.arm64linux }) |target| {
        const CG = LirCodeGenMod.LirCodeGen(target);
        var store = lir.LirStore.init(allocator);
        defer store.deinit();
        var layouts = try layout.Store.init(allocator, .u64);
        defer layouts.deinit();
        var procs: [2]lir.LIR.LirProcSpecId = undefined; // Filled by addProcSpec before emission.
        for (&procs, [_][]const u8{ "first independent message", "second independent message" }, 0..) |*proc, message, index| {
            const text = try store.insertString(message);
            const body = try store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = text } } }, .test_fixture);
            proc.* = try store.addProcSpec(.{
                .name = store.freshSyntheticSymbol(),
                .identity = lir.ProcIdentity.forTest(@intCast(index)),
                .args = .empty(),
                .body = body,
                .ret_layout = .zst,
                .source_observations = .absent,
            }, .none);
        }
        var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer image.deinit();
        var helpers = HelperKeys.init(allocator);
        defer helpers.deinit();
        var contracts: [2]?[32]u8 = .{ null, null };
        for (procs, 0..) |proc, index| {
            var producer = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
            defer producer.deinit();
            var fragment = try compileProcFragment(CG, allocator, &producer, proc, store.getProcSpecs(), &layouts, &.{}, &.{});
            defer fragment.deinit();
            for (fragment.set.artifacts) |artifact| {
                if (artifact.kind == .proc) {
                    contracts[index] = artifact.callable_contract;
                    try std.testing.expectEqual(@as(?bool, true), artifact.requires_ctfe_observations);
                    try std.testing.expect(!artifact.context_dependencies.?.comptime_hooks);
                }
            }
            try std.testing.expect(contracts[index] != null);
            try append(CG, allocator, &image, &fragment.set, store.getProcSpecs(), &helpers);
        }
        try image.finishImage();
        var recaptured = try extract(CG, allocator, &image, store.getProcSpecs(), &layouts, &.{}, &.{}, &.{});
        defer recaptured.deinit();
        var recaptured_procs: usize = 0;
        for (recaptured.artifacts) |artifact| {
            if (artifact.kind != .proc) continue;
            const index: usize = if (std.meta.eql(artifact.kind.proc, lir.ProcIdentity.forTest(0))) 0 else 1;
            try std.testing.expectEqual(contracts[index], artifact.callable_contract);
            try std.testing.expectEqual(@as(?bool, true), artifact.requires_ctfe_observations);
            try std.testing.expectEqual(LirCodeGenMod.FragmentContextDependencies{}, artifact.context_dependencies.?);
            recaptured_procs += 1;
        }
        try std.testing.expectEqual(@as(usize, 2), recaptured_procs);
        var pools: usize = 0;
        for (image.codeRegions()) |region| {
            if (region.kind == .message_pool_run) pools += 1;
        }
        try std.testing.expectEqual(@as(usize, 2), pools);
        var fresh = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
        defer fresh.deinit();
        try verifyRoundTrip(CG, allocator, &image, &fresh, store.getProcSpecs(), &layouts, &.{});
    }
}

test "artifact local calls reserve veneers before later regions exceed reach" {
    const allocator = std.testing.allocator;
    const CG = LirCodeGenMod.LirCodeGen(.arm64linux);
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, .u64);
    defer layouts.deinit();
    var image = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
    defer image.deinit();
    image.codegen.branch_reach_limit = 4096;
    const gap = [_]u8{0} ** 8192;
    var set = Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{
            .{
                .kind = .entrypoint,
                .code = "\x00\x00\x00\x94",
                .entry = 0,
                .frame = null,
                .refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }},
                .relocations = &.{},
                .data = &.{},
            },
            .{ .kind = .entrypoint, .code = &gap, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
            .{ .kind = .entrypoint, .code = "\xc0\x03\x5f\xd6", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        },
    };
    defer set.deinit();
    var helpers = HelperKeys.init(allocator);
    defer helpers.deinit();
    try assemble(CG, allocator, &image, &set, &.{}, &helpers);
    try image.finishImage();
    const veneer = image.codeRefVeneer(0) orelse return error.TestUnexpectedResult;
    try std.testing.expect(veneer < image.codegen.branch_reach_limit);
    const bytes = image.getGeneratedCode();
    const branch = std.mem.readInt(u32, bytes[0..4], .little);
    try std.testing.expectEqual(@as(u32, 0x94000000) | @as(u32, @intCast(veneer / 4)), branch);
    try std.testing.expectEqual(@as(?usize, bytes.len - 4), image.codegen.branch_sites.items[0].target);
    // The target is beyond the direct-call reach, not merely a nearby veneer.
    try std.testing.expect(bytes.len > gap.len);
}

test "artifact combination owns all fields and rebases only local references" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCombineOwnership, .{});
}

test "artifact prepared data captures only reached closure and preserves external bindings" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testPreparedData, .{});
}

fn testPreparedData(allocator: Allocator) (ExtractError || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var prepared = try PreparedData.init(allocator, &.{}, &.{
        testExport("literal", "owned string", &.{}, false),
        testExport("unreferenced", "must not be carried", &.{}, false),
    }, &.{
        testExport("roc__d0", "placeholder", &.{}, false),
    }, &.{});
    var prepared_live = true;
    defer if (prepared_live) prepared.deinit();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var local = std.StringHashMap(DataItem).init(allocator);
    defer local.deinit();
    try local.put("cell", .{
        .name = "cell",
        .bytes = &([_]u8{0} ** 8),
        .alignment = 8,
        .symbol_offset = 0,
        .relocations = &.{.{ .offset = 0, .name = "roc__d0", .addend = 0, .function = false, .external = true }},
    });
    var refs = [_]NamedRelocation{
        .{ .offset = 0, .name = "literal", .scope = .shared, .kind = .{ .data = .rel32 } },
        .{ .offset = 4, .name = "cell", .scope = .program, .kind = .{ .data = .rel32 } },
    };
    const items = try captureData(allocator, a, &refs, &local, &prepared);
    prepared.deinit();
    prepared_live = false;
    try std.testing.expectEqual(@as(usize, 2), items.len);
    try std.testing.expectEqualStrings("cell", items[0].name);
    try std.testing.expectEqualSlices(u8, &([_]u8{0} ** 8), items[0].bytes);
    try std.testing.expect(items[0].relocations[0].external);
    try std.testing.expectEqualStrings("roc__d0", items[0].relocations[0].name);
    try std.testing.expectEqualStrings("literal", items[1].name);
    try std.testing.expectEqualStrings("owned string", items[1].bytes);
    try std.testing.expectEqualStrings("literal", refs[0].name);
}

test "artifact metadata queues unresolved targets and preserves same-offset line order" {
    const Recorder = struct {
        const Self = @This();
        refs: [2]SymbolicReference = undefined,
        lines: [2]LineEntry = undefined,
        refs_len: usize = 0,
        lines_len: usize = 0,
        proc_entry: usize = 0,
        veneer_site: usize = 0,
        veneer_offset: usize = 0,

        pub fn registerAssembledProc(self: *Self, _: lir.ProcIdentity, entry: usize) Allocator.Error!void {
            self.proc_entry = entry;
        }
        pub fn registerAssembledThunk(_: *Self, _: lir.ProcIdentity, _: usize) Allocator.Error!void {}
        pub fn registerSplicedHelper(_: *Self, _: []const u8, _: usize) Allocator.Error!void {}
        pub fn queueAssembledSymbolicRef(self: *Self, ref: SymbolicReference) Allocator.Error!void {
            self.refs[self.refs_len] = ref;
            self.refs_len += 1;
        }
        pub fn registerAssembledRefVeneer(self: *Self, site: usize, veneer: ?usize) Allocator.Error!void {
            self.veneer_site = site;
            self.veneer_offset = veneer orelse 0;
        }
        pub fn appendAssembledLineEntry(self: *Self, line: LineEntry) Allocator.Error!void {
            self.lines[self.lines_len] = line;
            self.lines_len += 1;
        }
    };
    var recorder = Recorder{};
    const artifact = Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
        .code = "code",
        .entry = 1,
        .frame = null,
        .refs = &.{.{ .site = 2, .form = .call, .target = 0, .delta = 1, .veneer = 3 }},
        .symbolic_refs = &.{
            .{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) }, .veneer = 3 },
            .{ .site = 1, .form = .addr, .target = .{ .rc_helper = "unemitted" } },
        },
        .lines = &.{
            .{ .offset = 0, .loc = .{ .file = 3, .line = 10, .column = 1 } },
            .{ .offset = 0, .loc = .{ .file = 3, .line = 11, .column = 2 } },
        },
        .relocations = &.{},
        .data = &.{},
    };
    try appendMetadata(Recorder, &recorder, artifact, 100, true, true);
    try std.testing.expectEqual(@as(usize, 101), recorder.proc_entry);
    try std.testing.expectEqual(@as(u32, 100), recorder.refs[0].site);
    try std.testing.expectEqual(@as(?u32, 103), recorder.refs[0].veneer);
    try std.testing.expectEqualDeep(artifact.symbolic_refs[0].target, recorder.refs[0].target);
    try std.testing.expectEqualStrings("unemitted", recorder.refs[1].target.rc_helper);
    try std.testing.expectEqual(@as(usize, 102), recorder.veneer_site);
    try std.testing.expectEqual(@as(usize, 103), recorder.veneer_offset);
    for (artifact.lines, recorder.lines[0..recorder.lines_len]) |original, rebased| {
        try std.testing.expectEqual(@as(u32, 100), rebased.offset);
        try std.testing.expectEqualDeep(original.loc, rebased.loc);
    }
    var persistent = Recorder{};
    try appendMetadata(Recorder, &persistent, artifact, 100, false, true);
    try std.testing.expectEqual(@as(usize, 0), persistent.lines_len);
    try std.testing.expectEqual(@as(usize, 2), persistent.refs_len);
}

fn testCombineOwnership(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    const source = Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .rc_helper = "helper" },
            .code = "code",
            .entry = 1,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .addr, .target = 0, .delta = 1 }},
            .symbolic_refs = &.{
                .{ .site = 1, .form = .call, .target = .{ .rc_helper = "other" } },
                .{ .site = 2, .form = .addr, .target = .{ .proc = lir.ProcIdentity.forTest(4) } },
            },
            .lines = &.{
                .{ .offset = 0, .loc = .{ .file = 7, .line = 3, .column = 2 } },
                .{ .offset = 0, .loc = .{ .file = 7, .line = 4, .column = 5 } },
            },
            .relocations = &.{.{ .offset = 0, .name = "external", .scope = .shared, .kind = .function }},
            .data = &.{.{
                .name = "data",
                .bytes = "contents",
                .alignment = 8,
                .symbol_offset = 0,
                .relocations = &.{.{ .offset = 0, .name = "pointer", .addend = 2, .function = true }},
            }},
        }},
    };
    var copy = try combine(allocator, &.{&source});
    var copy_live = true;
    defer if (copy_live) copy.deinit();
    var combined = try combine(allocator, &.{ &copy, &copy });
    defer combined.deinit();
    copy.deinit();
    copy_live = false;
    try std.testing.expectEqualDeep(source.artifacts[0], combined.artifacts[0]);
    try std.testing.expectEqual(@as(u32, 1), combined.artifacts[1].refs[0].target);
    try std.testing.expectEqualDeep(source.artifacts[0].symbolic_refs, combined.artifacts[1].symbolic_refs);
    try std.testing.expectEqualDeep(source.artifacts[0].lines, combined.artifacts[1].lines);
    try std.testing.expectEqualDeep(source.artifacts[0].data, combined.artifacts[1].data);
}

fn testExport(name: []const u8, bytes: []const u8, relocations: []const lir.Program.StaticDataRelocation, is_exported: bool) lir.Program.StaticDataExport {
    return .{
        .symbol_name = name,
        .bytes = bytes,
        .alignment = 8,
        .is_global = false,
        .is_exported = is_exported,
        .relocations = relocations,
    };
}

fn testItem(name: []const u8, bytes: []const u8, relocations: []const DataRelocation, program_local_name: bool) DataItem {
    return .{ .name = name, .bytes = bytes, .alignment = 8, .symbol_offset = 0, .relocations = relocations, .program_local_name = program_local_name };
}

fn testReloc(offset: u32, name: []const u8, addend: i64) DataRelocation {
    return .{ .offset = offset, .name = name, .addend = addend, .function = false };
}

/// A set whose one artifact carries `data`; the set owns nothing.
fn testDataSet(storage: *[1]Artifact, data: []const DataItem) Set {
    storage[0] = .{ .kind = .entrypoint, .code = &.{}, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = data };
    return .{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator), .artifacts = storage };
}

test "constants are named by content across programs, through cycles, and never when host-visible" {
    const testing = std.testing;

    // Program one: a cycle between the first two constants, a leaf, and a
    // host-visible export that points at the leaf.
    const one = [_]DataItem{
        testItem("roc__d0", "\x00" ** 16, &.{testReloc(8, "roc__d0_1", 0)}, true),
        testItem("roc__d0_1", "\x00" ** 16, &.{ testReloc(0, "roc__d0", 0), testReloc(8, "roc__d0_2", 4) }, true),
        testItem("roc__d0_2", "leaf", &.{}, true),
        testItem("roc__answer", "\x00" ** 8, &.{testReloc(0, "roc__d0_2", 0)}, false),
    };
    // Program two: the same graph under other names and another order.
    const two = [_]DataItem{
        testItem("roc__d7_2", "leaf", &.{}, true),
        testItem("roc__d7", "\x00" ** 16, &.{testReloc(8, "roc__d7_1", 0)}, true),
        testItem("roc__d7_1", "\x00" ** 16, &.{ testReloc(0, "roc__d7", 0), testReloc(8, "roc__d7_2", 4) }, true),
    };

    var storage_one: [1]Artifact = undefined;
    const set_one = testDataSet(&storage_one, &one);
    var names_one = try ContentNames.init(testing.allocator, &set_one);
    defer names_one.deinit();
    const one_root = names_one.of("roc__d0");
    const one_node = names_one.of("roc__d0_1");
    const one_leaf = names_one.of("roc__d0_2");
    var storage_two: [1]Artifact = undefined;
    const set_two = testDataSet(&storage_two, &two);
    var names_two = try ContentNames.init(testing.allocator, &set_two);
    defer names_two.deinit();

    try testing.expectEqualStrings("roc__answer", names_one.of("roc__answer"));
    try testing.expect(std.mem.startsWith(u8, one_root, content_data_prefix));
    try testing.expectEqualStrings(one_root, names_two.of("roc__d7"));
    try testing.expectEqualStrings(one_node, names_two.of("roc__d7_1"));
    try testing.expectEqualStrings(one_leaf, names_two.of("roc__d7_2"));
    try testing.expect(!std.mem.eql(u8, one_root, one_node));
    // Names outside the carried data keep their own spelling.
    try testing.expectEqualStrings("roc__pelsewhere", names_one.of("roc__pelsewhere"));

    // A different leaf changes every name that reaches it.
    const three = [_]DataItem{
        testItem("roc__d0", "\x00" ** 16, &.{testReloc(8, "roc__d0_1", 0)}, true),
        testItem("roc__d0_1", "\x00" ** 16, &.{ testReloc(0, "roc__d0", 0), testReloc(8, "roc__d0_2", 4) }, true),
        testItem("roc__d0_2", "LEAF", &.{}, true),
    };
    var storage_three: [1]Artifact = undefined;
    const set_three = testDataSet(&storage_three, &three);
    var names_three = try ContentNames.init(testing.allocator, &set_three);
    defer names_three.deinit();
    try testing.expect(!std.mem.eql(u8, one_root, names_three.of("roc__d0")));
    // The leaf's program-local name is the same in both programs, and its
    // content name is not.
    try testing.expect(!std.mem.eql(u8, one_leaf, names_three.of("roc__d0_2")));
}
