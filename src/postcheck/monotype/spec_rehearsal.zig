//! Per-specialization rehearsal of the emission the flip will produce
//! (reunify.md sections 9, 10.2/10.6, 11.1/11.2), Slice 7 flip-prep step (b).
//!
//! For every specialization the instantiation graph lowers, this module builds
//! that specialization's binder environment from checked module data alone — the
//! callee scheme named by the requesting edge's `CheckedInstantiationSite`, whose
//! dense actuals are translated under the CALLER's environment through a stack of
//! active environments — and then produces, in isolation, the type at every
//! position the graph sealed. It never reads a graph answer to decide anything:
//! the graph is consulted only to say WHICH positions to compare and what it
//! sealed there, and the comparison is a stored-digest equality.
//!
//! Representation is decided here, not mirrored. Every representation-bearing
//! position the rehearsal emits receives a `representation_closure` slot carrying
//! the descriptor the checked data authorizes; positions the environment makes
//! identical share one slot, and two independently emitted occurrences of one
//! structure stay distinct slots (reunify.md section 9.3's occurrence-safety
//! law). At the specialization's end the slots seal (reunify.md section 10.6):
//! sealing checks that each slot's logical identity survived and that the sealed
//! descriptor still equals the one emitted at that position, so the day a rule
//! moves a descriptor the emitted type must be re-materialized from the sealed
//! slot instead — and the counter says so.
//!
//! Every difference is measured, never repaired: `rehearsal_type_mismatch_logical`
//! is the required-zero counter (a representation-free position the directed
//! emission got wrong), and `rehearsal_type_mismatch_representation` plus the
//! engine-input skip class bound exactly the representation content the flip's
//! body discovery must supply.
//!
//! State isolation: this module owns its own Monotype store and its own closure
//! engine, writes no lowering state, allocates no id in the output type pool, and
//! resolves names through the same interning the graph itself uses so a
//! rehearsal type is name-identical to a graph-sealed one. It is compiled out
//! unless `census.enabled` and turned on only by `ROC_REUNIFY_SHADOW`; every
//! internal failure disables it instead of affecting lowering.

const std = @import("std");
const Allocator = std.mem.Allocator;

const check = @import("check");
const collections = @import("collections");

const Type = @import("type.zig");
const census = @import("census.zig");
const direct_translate = @import("direct_translate.zig");
const solve = @import("solve.zig");
const closure = @import("../representation_closure.zig");
const policy = @import("../representation_policy.zig");
const reunify_shadow = @import("../reunify_shadow/shadow.zig");

const names = check.CheckedNames;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const GuardedList = collections.GuardedList;

/// The maximum distinct sealed ids one checked position may carry within a
/// single specialization before further occurrences are only counted. A position
/// reached through several instantiation contexts of one body has a handful at
/// most; this only bounds pathological input.
const max_occurrences_per_position: usize = 4;

/// The maximum depth the slot builder descends before treating a position as an
/// opaque leaf. Representation-bearing spines are shallow.
const max_slot_depth: u32 = 64;

/// The maximum number of mismatching positions described in the census dump.
const max_mismatch_details: usize = 24;

/// One checked type's address: the content identity of the module whose store
/// holds it, plus its id within that store.
pub const CheckedAddress = struct {
    module_bytes: [32]u8,
    type_id: u32,
};

/// The requesting edge of one specialization: the module whose body made the
/// request, and the instantiated function type recorded at that use. It names
/// the `CheckedInstantiationSite` whose dense actuals bind the callee scheme.
pub const RequestEdge = struct {
    module_bytes: [32]u8,
    instantiated_root: checked.CheckedTypeId,
};

/// The per-specialization record the instantiation graph fills while lowering:
/// which checked type each node it instantiated came from, and which immutable
/// id each node finally sealed to. The join of the two is the position list the
/// rehearsal compares against. Nodes are carried as raw indices so the graph
/// owns its own id type.
pub const SealTrace = struct {
    allocator: Allocator,
    provenance: std.AutoHashMapUnmanaged(u32, CheckedAddress),
    sealed: std.AutoHashMapUnmanaged(u32, Type.TypeId),
    disabled: bool,

    /// An empty trace owning no storage yet.
    pub fn init(allocator: Allocator) SealTrace {
        return .{
            .allocator = allocator,
            .provenance = .empty,
            .sealed = .empty,
            .disabled = false,
        };
    }

    /// Release the trace's tables.
    pub fn deinit(self: *SealTrace) void {
        self.provenance.deinit(self.allocator);
        self.sealed.deinit(self.allocator);
    }

    /// Record that `node` was instantiated from `address`. Repeats keep the
    /// first address: one node stands for one checked position.
    pub fn noteProvenance(self: *SealTrace, node: u32, address: CheckedAddress) void {
        if (self.disabled) return;
        const gop = self.provenance.getOrPut(self.allocator, node) catch {
            self.disabled = true;
            return;
        };
        if (!gop.found_existing) gop.value_ptr.* = address;
    }

    /// Record that `node` sealed to `ty`. A node sealed more than once keeps its
    /// latest committed id, which is the one lowering carries forward.
    pub fn noteSealed(self: *SealTrace, node: u32, ty: Type.TypeId) void {
        if (self.disabled) return;
        self.sealed.put(self.allocator, node, ty) catch {
            self.disabled = true;
        };
    }
};

/// Resolves a module's content identity to the cursor a translation reads it by.
/// The lowering Builder owns the module list; this hands the rehearsal exactly
/// the read it needs without duplicating that list.
pub const ModuleLookup = struct {
    context: *anyopaque,
    cursor_for_module: *const fn (context: *anyopaque, module_bytes: [32]u8) ?direct_translate.ModuleCursor,

    fn cursor(self: ModuleLookup, module_bytes: [32]u8) ?direct_translate.ModuleCursor {
        return self.cursor_for_module(self.context, module_bytes);
    }
};

/// The inputs one specialization's rehearsal starts from.
pub const SpecializationStart = struct {
    /// The graph lowering this specialization; the rehearsal attaches its trace.
    graph: *solve.InstGraph,
    /// The module the specialized template's body reads its checked types from.
    cursor: direct_translate.ModuleCursor,
    /// The reserved function id when this specialization was requested earlier
    /// and lowered from the deferred queue, so its edge is looked up by id.
    reserved_fn_id: ?u32,
};

/// One module's instantiation sites indexed by the instantiated root each edge
/// recorded, which is the checked function type a call site requests with.
const SiteIndex = struct {
    view: checked.CheckedTypeStoreView,
    by_root: std.AutoHashMapUnmanaged(u32, u32),
    ambiguous: std.AutoHashMapUnmanaged(u32, void),
};

/// One active specialization's environment plus the graph trace it compares
/// against. `bound` is dense and ordered exactly like `binders` (reunify.md
/// section 9.1); it is owned by the rehearsal and freed when the frame pops.
/// The trace is heap-allocated so the graph's pointer to it survives the frame
/// stack growing under a nested specialization.
const Frame = struct {
    trace: *SealTrace,
    /// The module whose ids `binders` name, and whose residual dispositions
    /// `owner_node` selects. Only positions in this module translate under the
    /// environment; a position in another module has no binder in scope.
    env_module_bytes: [32]u8,
    scheme: direct_translate.SchemeIdent,
    owner_node: u32,
    binders: []const checked.CheckedTypeId,
    bound: []direct_translate.BoundType,
    /// The callee's scheme root emitted under this binding: the specialization's
    /// own interface type (reunify.md section 11.1).
    interface_root: ?Type.TypeId,
    /// The requesting edge's instantiated root emitted under the CALLER's
    /// environment: the request context's side of the same interface.
    request_root: ?Type.TypeId,
    env_ready: bool,

    fn environment(self: *const Frame) direct_translate.BindingEnvironment {
        return .{
            .scheme = self.scheme,
            .binders = self.binders,
            .bound = self.bound,
            .captured = &.{},
            .parent = null,
        };
    }
};

/// One compared position's sealed ids, bounded so a position reached through
/// many instantiation contexts cannot allocate per occurrence.
const Occurrences = struct {
    ids: [max_occurrences_per_position]Type.TypeId,
    len: usize,
    overflow: usize,

    fn empty() Occurrences {
        return .{ .ids = undefined, .len = 0, .overflow = 0 };
    }

    /// Record one distinct sealed id for this position, counting anything past
    /// the bound instead of growing.
    fn record(self: *Occurrences, sealed: Type.TypeId) void {
        for (self.ids[0..self.len]) |existing| {
            if (existing == sealed) return;
        }
        if (self.len == max_occurrences_per_position) {
            self.overflow += 1;
            return;
        }
        self.ids[self.len] = sealed;
        self.len += 1;
    }
};

/// One recorded mismatch, dumped with the census counters. The head shapes are
/// carried so a difference can be classified without re-running: two different
/// content tags say the emission took a different shape, while one tag with a
/// different child count says the row or argument list differs.
const MismatchDetail = struct {
    module_prefix: [8]u8,
    type_id: u32,
    representation: bool,
    rehearsal_digest: names.TypeDigest,
    graph_digest: names.TypeDigest,
    rehearsal_head: HeadShape,
    graph_head: HeadShape,
    difference: Difference,
};

/// A type's outermost shape: its content tag and how many children it holds.
const HeadShape = struct {
    tag: std.meta.Tag(Type.Content),
    children: u32,

    fn of(store: *const Type.Store, ty: Type.TypeId) HeadShape {
        return .{ .tag = std.meta.activeTag(store.get(ty)), .children = childCount(store, ty) };
    }
};

/// How many type children a node has, counting every position the walk can
/// descend into: a tag union's payloads across all its tags, and a named type's
/// arguments plus its backing.
fn childCount(store: *const Type.Store, ty: Type.TypeId) u32 {
    return switch (store.get(ty)) {
        .primitive, .zst, .erased => 0,
        .list, .box => 1,
        .tuple => |span| @intCast(GuardedList.borrowLen(store.span(span))),
        .record => |span| @intCast(GuardedList.borrowLen(store.fieldSpan(span))),
        .tag_union => |span| blk: {
            const tags = store.tagSpan(span);
            var total: u32 = 0;
            for (0..GuardedList.borrowLen(tags)) |i| {
                total += @intCast(GuardedList.borrowLen(store.span(GuardedList.at(tags, i).payloads)));
            }
            break :blk total;
        },
        .func => |fn_ty| @as(u32, @intCast(GuardedList.borrowLen(store.span(fn_ty.args)))) + 1,
        .named => |named| @as(u32, @intCast(GuardedList.borrowLen(store.span(named.args)))) +
            @as(u32, if (named.backing == null) 0 else 1),
    };
}

/// The deepest pair of heads that still differ when two types are walked in
/// parallel: the exact place an emission diverged, rather than the root that
/// merely inherits the difference. Bounded by `max_difference_depth`.
const Difference = struct {
    depth: u32,
    left: HeadShape,
    right: HeadShape,
    named_field: NamedFieldDifference,
    /// Whether each side reaches itself. Two recursive types whose every child
    /// digests equal differ only in where their cycle is rooted (reunify.md
    /// section 8.3), which is a different finding from a content difference.
    left_recursive: bool,
    right_recursive: bool,
};

/// Whether a type reaches itself through any child path, bounded by the same
/// depth the difference walk uses.
fn isRecursive(store: *const Type.Store, root: Type.TypeId, ty: Type.TypeId, depth: u32) bool {
    if (depth >= max_difference_depth) return false;
    const count = childCount(store, ty);
    var index: u32 = 0;
    while (index < count) : (index += 1) {
        const child = childAt(store, ty, index) orelse continue;
        if (child == root) return true;
        if (isRecursive(store, root, child, depth + 1)) return true;
    }
    return false;
}

/// How deep the parallel difference walk descends before reporting where it is.
const max_difference_depth: u32 = 32;

/// Walk two types in parallel and report where they first stop agreeing. Both
/// stores digest into the same name store, so child digests are comparable
/// across them.
fn firstDifference(
    left_store: *const Type.Store,
    left: Type.TypeId,
    right_store: *const Type.Store,
    right: Type.TypeId,
    name_store: *const names.NameStore,
    depth: u32,
) Difference {
    const here = Difference{
        .depth = depth,
        .left = HeadShape.of(left_store, left),
        .right = HeadShape.of(right_store, right),
        .named_field = NamedFieldDifference.of(left_store, left, right_store, right),
        .left_recursive = isRecursive(left_store, left, left, 0),
        .right_recursive = isRecursive(right_store, right, right, 0),
    };
    if (depth >= max_difference_depth) return here;
    if (here.left.tag != here.right.tag or here.left.children != here.right.children) return here;

    var index: u32 = 0;
    while (index < here.left.children) : (index += 1) {
        const left_child = childAt(left_store, left, index) orelse return here;
        const right_child = childAt(right_store, right, index) orelse return here;
        const left_digest = left_store.typeDigest(name_store, left_child);
        const right_digest = right_store.typeDigest(name_store, right_child);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) continue;
        return firstDifference(left_store, left_child, right_store, right_child, name_store, depth + 1);
    }
    return here;
}

/// The `index`th type child of a node, in the order `childCount` counts them:
/// a tag union's payloads run tag by tag, a function's arguments are followed by
/// its result, and a named type's arguments are followed by its backing.
fn childAt(store: *const Type.Store, ty: Type.TypeId, index: u32) ?Type.TypeId {
    return switch (store.get(ty)) {
        .primitive, .zst, .erased => null,
        .list, .box => |elem| if (index == 0) elem else null,
        .tuple => |span| spanChild(store.span(span), index),
        .record => |span| blk: {
            const fields = store.fieldSpan(span);
            if (index >= GuardedList.borrowLen(fields)) break :blk null;
            break :blk GuardedList.at(fields, index).ty;
        },
        .tag_union => |span| blk: {
            const tags = store.tagSpan(span);
            var seen: u32 = 0;
            for (0..GuardedList.borrowLen(tags)) |i| {
                const payloads = store.span(GuardedList.at(tags, i).payloads);
                const count: u32 = @intCast(GuardedList.borrowLen(payloads));
                if (index < seen + count) break :blk GuardedList.at(payloads, index - seen);
                seen += count;
            }
            break :blk null;
        },
        .func => |fn_ty| blk: {
            const args = store.span(fn_ty.args);
            const count: u32 = @intCast(GuardedList.borrowLen(args));
            if (index < count) break :blk GuardedList.at(args, index);
            if (index == count) break :blk fn_ty.ret;
            break :blk null;
        },
        .named => |named| blk: {
            const args = store.span(named.args);
            const count: u32 = @intCast(GuardedList.borrowLen(args));
            if (index < count) break :blk GuardedList.at(args, index);
            if (index == count) {
                const backing = named.backing orelse break :blk null;
                break :blk backing.ty;
            }
            break :blk null;
        },
    };
}

fn spanChild(span: anytype, index: u32) ?Type.TypeId {
    if (index >= GuardedList.borrowLen(span)) return null;
    return GuardedList.at(span, index);
}

/// Which part of two named nodes disagrees once their type children already
/// match: the identity a nominal is declared with, the runtime encoding stamps,
/// or the layout-only declared order. Reported so a difference names an exact
/// field rather than "the types differ".
const NamedFieldDifference = enum {
    not_named,
    instance_module,
    def_module,
    source_decl,
    type_name,
    generated,
    iterator,
    kind,
    builtin_owner,
    backing_presence,
    backing_use,
    declared_order_length,
    declared_order_entry,
    equal,

    /// Compare exactly the fields the stored digest hashes, in the order it
    /// hashes them, so an "equal" answer means the two named heads really do
    /// contribute identical bytes and the difference is in a child.
    fn of(
        left_store: *const Type.Store,
        left: Type.TypeId,
        right_store: *const Type.Store,
        right: Type.TypeId,
    ) NamedFieldDifference {
        const left_named = switch (left_store.get(left)) {
            .named => |named| named,
            else => return .not_named,
        };
        const right_named = switch (right_store.get(right)) {
            .named => |named| named,
            else => return .not_named,
        };
        if (!std.mem.eql(u8, &left_named.named_type.module.bytes, &right_named.named_type.module.bytes)) {
            return .instance_module;
        }
        if (left_named.def.module != right_named.def.module) return .def_module;
        if (!std.meta.eql(left_named.def.source_decl, right_named.def.source_decl)) return .source_decl;
        if (left_named.def.source_decl == null and left_named.def.type_name != right_named.def.type_name) {
            return .type_name;
        }
        if (!std.meta.eql(left_named.def.generated, right_named.def.generated)) return .generated;
        if (left_named.def.iterator_representation != right_named.def.iterator_representation or
            left_named.def.iterator_kind != right_named.def.iterator_kind or
            left_named.def.iterator_depth != right_named.def.iterator_depth)
        {
            return .iterator;
        }
        if (left_named.kind != right_named.kind) return .kind;
        if (left_named.builtin_owner != right_named.builtin_owner) return .builtin_owner;
        if ((left_named.backing == null) != (right_named.backing == null)) return .backing_presence;
        if (left_named.backing) |left_backing| {
            if (left_backing.use != right_named.backing.?.use) return .backing_use;
        }
        const left_order = left_store.declaredFieldSpan(left_named.declared_order);
        const right_order = right_store.declaredFieldSpan(right_named.declared_order);
        const left_len = GuardedList.borrowLen(left_order);
        if (left_len != GuardedList.borrowLen(right_order)) return .declared_order_length;
        for (0..left_len) |index| {
            const left_entry = GuardedList.at(left_order, index);
            const right_entry = GuardedList.at(right_order, index);
            if (std.meta.activeTag(left_entry) != std.meta.activeTag(right_entry)) return .declared_order_entry;
            switch (left_entry) {
                .named => |label| if (label != right_entry.named) return .declared_order_entry,
                .padding => {},
            }
        }
        return .equal;
    }
};

/// The rehearsal: one per lowering run, holding the active environment stack,
/// its own emission store, and its own representation closure engine.
pub const Rehearsal = struct {
    allocator: Allocator,
    /// The output store, read only to digest what the graph sealed.
    program_types: *const Type.Store,
    /// The output name store; a rehearsal type interns its names here exactly as
    /// graph instantiation does, so equal types digest equal.
    program_names: *names.NameStore,
    /// The rehearsal's own emission store. No id here reaches lowering.
    store: Type.Store,
    translator: direct_translate.Translator,
    engine: closure.Engine,
    lookup: ModuleLookup,
    frames: std.ArrayList(Frame),
    /// The edge of the request currently being made, moved to `edges_by_fn` when
    /// the request reserves and is lowered later.
    pending_edge: ?RequestEdge,
    edges_by_fn: std.AutoHashMapUnmanaged(u32, RequestEdge),
    site_index: std.AutoHashMapUnmanaged([32]u8, SiteIndex),
    /// Interned logical-identity digests to dense engine tokens. Two slots may
    /// relate only when their tokens are equal.
    logical_tokens: std.AutoHashMapUnmanaged([32]u8, u64),
    next_token: u64,
    next_producer: u32,
    /// Slots for the emitted types of the specialization being sealed, keyed by
    /// the rehearsal id at that position: one emitted occurrence, one slot.
    slots: std.AutoHashMapUnmanaged(Type.TypeId, closure.RepresentationSlotId),
    /// The descriptor each iterator slot was created with, so sealing can see
    /// whether the closure moved it.
    slot_descriptors: std.AutoHashMapUnmanaged(u32, policy.NamedDescriptor),
    details: std.ArrayList(MismatchDetail),
    disabled: bool,

    /// Build a rehearsal when it is compiled in and enabled, otherwise null.
    pub fn maybeCreate(
        allocator: Allocator,
        program_types: *const Type.Store,
        program_names: *names.NameStore,
        resolver: direct_translate.Resolver,
        lookup: ModuleLookup,
    ) ?*Rehearsal {
        if (comptime !census.enabled) return null;
        if (!reunify_shadow.shouldRun()) return null;
        return create(allocator, program_types, program_names, resolver, lookup) catch null;
    }

    /// Build a rehearsal unconditionally; `maybeCreate` gates this.
    pub fn create(
        allocator: Allocator,
        program_types: *const Type.Store,
        program_names: *names.NameStore,
        resolver: direct_translate.Resolver,
        lookup: ModuleLookup,
    ) Allocator.Error!*Rehearsal {
        const self = try allocator.create(Rehearsal);
        self.* = .{
            .allocator = allocator,
            .program_types = program_types,
            .program_names = program_names,
            .store = Type.Store.init(allocator),
            .translator = undefined,
            .engine = closure.Engine.init(allocator),
            .lookup = lookup,
            .frames = .empty,
            .pending_edge = null,
            .edges_by_fn = .empty,
            .site_index = .empty,
            .logical_tokens = .empty,
            .next_token = 1,
            .next_producer = 1,
            .slots = .empty,
            .slot_descriptors = .empty,
            .details = .empty,
            .disabled = false,
        };
        // The graph commits every seal through the store's content-deduplicating
        // constructor, so the rehearsal's own store deduplicates too: a recursive
        // group's symmetric members must collapse on both sides or two isomorphic
        // groups would be rooted differently and digest differently (reunify.md
        // section 8.3). The store is private to the rehearsal, so this changes
        // nothing lowering can observe.
        self.store.enableInterning();
        self.translator = direct_translate.Translator.init(allocator, &self.store, program_names, resolver);
        return self;
    }

    /// Dump the bounded mismatch detail and release everything the rehearsal
    /// owns. Nothing it allocated is visible to lowering.
    pub fn destroy(self: *Rehearsal) void {
        self.dumpDetails();
        for (self.frames.items) |*frame| self.releaseFrame(frame);
        self.frames.deinit(self.allocator);
        self.details.deinit(self.allocator);
        self.slot_descriptors.deinit(self.allocator);
        self.slots.deinit(self.allocator);
        self.logical_tokens.deinit(self.allocator);
        var indexes = self.site_index.valueIterator();
        while (indexes.next()) |index| {
            index.by_root.deinit(self.allocator);
            index.ambiguous.deinit(self.allocator);
        }
        self.site_index.deinit(self.allocator);
        self.edges_by_fn.deinit(self.allocator);
        self.engine.deinit();
        self.translator.deinit();
        self.store.deinit();
        self.allocator.destroy(self);
    }

    /// Record the edge a specialization request is being made from: the caller's
    /// module and the instantiated function type at the use.
    pub fn noteRequestEdge(self: *Rehearsal, module_bytes: [32]u8, instantiated_root: checked.CheckedTypeId) void {
        if (self.disabled) return;
        self.pending_edge = .{ .module_bytes = module_bytes, .instantiated_root = instantiated_root };
    }

    /// Attach the pending edge to a reserved function id, so a specialization
    /// lowered later from the deferred queue resolves the edge that requested it.
    pub fn rememberReservedEdge(self: *Rehearsal, fn_id: u32) void {
        if (self.disabled) return;
        const edge = self.pending_edge orelse return;
        self.pending_edge = null;
        self.edges_by_fn.put(self.allocator, fn_id, edge) catch {
            self.disabled = true;
        };
    }

    /// Start one specialization: resolve its binder environment from checked
    /// data and attach the trace the graph fills. Always pushes a frame, so the
    /// matching `endSpecialization` is unconditional.
    pub fn beginSpecialization(self: *Rehearsal, start: SpecializationStart) void {
        if (self.disabled) return;
        census.bump("rehearsal_spec_attempted");
        const trace = self.allocator.create(SealTrace) catch return self.fail();
        trace.* = SealTrace.init(self.allocator);
        var frame = Frame{
            .trace = trace,
            .env_module_bytes = start.cursor.module_bytes,
            .scheme = .{ .module_bytes = start.cursor.module_bytes, .scheme = 0 },
            .owner_node = checked.checked_residual_disposition_module_body_owner,
            .binders = &.{},
            .bound = &.{},
            .interface_root = null,
            .request_root = null,
            .env_ready = false,
        };
        self.resolveEnvironment(start, &frame);
        self.frames.append(self.allocator, frame) catch {
            self.releaseFrame(&frame);
            self.disabled = true;
            return;
        };
        start.graph.trace = trace;
    }

    /// Compare, position by position, what this specialization's directed
    /// emission produces against what the graph sealed. Runs while the graph is
    /// still alive so a node's equivalence class still resolves.
    pub fn compareSpecialization(self: *Rehearsal, graph: *solve.InstGraph) void {
        if (self.disabled) return;
        if (self.frames.items.len == 0) return;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) return;
        census.bump("rehearsal_spec_compared");

        var positions: std.AutoHashMapUnmanaged(CheckedAddress, Occurrences) = .empty;
        defer positions.deinit(self.allocator);

        var it = frame.trace.provenance.iterator();
        while (it.next()) |entry| {
            const root = @intFromEnum(graph.rootOf(@enumFromInt(entry.key_ptr.*)));
            const sealed = frame.trace.sealed.get(root) orelse continue;
            const gop = positions.getOrPut(self.allocator, entry.value_ptr.*) catch return self.fail();
            if (!gop.found_existing) gop.value_ptr.* = Occurrences.empty();
            gop.value_ptr.record(sealed);
        }

        self.slots.clearRetainingCapacity();
        self.slot_descriptors.clearRetainingCapacity();

        var compared = positions.iterator();
        while (compared.next()) |entry| {
            self.comparePosition(frame, entry.key_ptr.*, entry.value_ptr.*);
        }
        self.relateInterface(frame);
        self.sealSlots();
    }

    /// Finish one specialization: detach the trace and pop the environment.
    pub fn endSpecialization(self: *Rehearsal, graph: *solve.InstGraph) void {
        graph.trace = null;
        if (self.frames.items.len == 0) return;
        var frame = self.frames.pop() orelse return;
        self.releaseFrame(&frame);
    }

    fn releaseFrame(self: *Rehearsal, frame: *Frame) void {
        frame.trace.deinit();
        self.allocator.destroy(frame.trace);
        if (frame.bound.len != 0) self.allocator.free(frame.bound);
        frame.bound = &.{};
    }

    fn fail(self: *Rehearsal) void {
        self.disabled = true;
    }

    /// Resolve one specialization's dense binding from the requesting edge's
    /// site (reunify.md sections 7.2, 9.1). Every way the edge fails to resolve
    /// is a named skip class, never an assumption.
    fn resolveEnvironment(self: *Rehearsal, start: SpecializationStart, frame: *Frame) void {
        const edge = self.takeEdge(start.reserved_fn_id) orelse {
            if (self.frames.items.len == 0) {
                census.bump("rehearsal_skip_root_edge");
            } else {
                census.bump("rehearsal_skip_generated_edge");
            }
            return;
        };
        const caller = self.lookup.cursor(edge.module_bytes) orelse {
            census.bump("rehearsal_skip_module_absent");
            return;
        };
        const site = self.siteFor(caller, edge.instantiated_root) orelse return;
        const scheme_id = site.schemeId() orelse {
            census.bump("rehearsal_skip_scheme_unresolved");
            return;
        };
        const defining_bytes = site.importedDefiningModule() orelse edge.module_bytes;
        const defining = self.lookup.cursor(defining_bytes) orelse {
            census.bump("rehearsal_skip_module_absent");
            return;
        };
        const scheme = defining.view.schemeById(scheme_id) orelse {
            census.bump("rehearsal_skip_scheme_unresolved");
            return;
        };
        if (scheme.captured_len != 0) {
            census.bump("rehearsal_skip_captured_scheme");
            return;
        }
        const binders = scheme.generalizedVars(defining.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) {
            census.bump("rehearsal_skip_arity_mismatch");
            return;
        }

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch return self.fail();
        var filled: usize = 0;
        for (actuals) |actual| {
            if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) {
                census.bump("rehearsal_skip_unreached_actual");
                self.allocator.free(bound);
                return;
            }
            const translated = self.translateActual(caller, actual) orelse {
                self.allocator.free(bound);
                return;
            };
            bound[filled] = direct_translate.BoundType.of(translated);
            filled += 1;
        }

        frame.env_module_bytes = defining_bytes;
        frame.scheme = .{ .module_bytes = defining_bytes, .scheme = @intFromEnum(scheme_id) };
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.bound = bound;
        frame.env_ready = true;
        census.bump("rehearsal_env_resolved");

        // The two sides of this specialization's representation interface
        // (reunify.md section 11.1): the callee's scheme root emitted under the
        // binding, and the request context's own emission of the same edge.
        const env = frame.environment();
        frame.interface_root = self.emitQuietly(defining, &env, scheme.owner_node, scheme.root);
        if (self.callerFrameFor(caller.module_bytes)) |active| {
            const caller_env = active.environment();
            frame.request_root = self.emitQuietly(caller, &caller_env, active.owner_node, edge.instantiated_root);
        } else {
            const owner_node = checked.checked_residual_disposition_module_body_owner;
            frame.request_root = self.emitQuietly(caller, null, owner_node, edge.instantiated_root);
        }
    }

    /// Emit one checked root, counting rather than classifying a walk that left
    /// the translatable subset: the caller only needs the type when it exists.
    fn emitQuietly(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        env: ?*const direct_translate.BindingEnvironment,
        owner_node: u32,
        root: checked.CheckedTypeId,
    ) ?Type.TypeId {
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(cursor, env, owner_node, root, &reason) catch |err| switch (err) {
            error.Skip => null,
            else => {
                self.fail();
                return null;
            },
        };
    }

    /// Translate one of the requesting edge's actuals under the CALLER's own
    /// environment, so an actual that names an enclosing binder resolves to the
    /// value that binder already took (reunify.md sections 7.3, 9.1).
    fn translateActual(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        actual: checked.CheckedTypeId,
    ) ?Type.TypeId {
        var reason: direct_translate.SkipReason = undefined;
        const caller_frame = self.callerFrameFor(caller.module_bytes);
        const env = if (caller_frame) |active| active.environment() else null;
        const owner_node = if (caller_frame) |active|
            active.owner_node
        else
            checked.checked_residual_disposition_module_body_owner;
        const env_ptr: ?*const direct_translate.BindingEnvironment = if (env) |*value| value else null;
        return self.translator.translateUnderEnvironment(caller, env_ptr, owner_node, actual, &reason) catch |err| switch (err) {
            error.Skip => {
                census.bump("rehearsal_skip_actual_untranslatable");
                return null;
            },
            else => {
                self.fail();
                return null;
            },
        };
    }

    /// The innermost active environment whose binders name ids in `module_bytes`,
    /// or null when the caller is outside every active environment's module.
    fn callerFrameFor(self: *Rehearsal, module_bytes: [32]u8) ?*const Frame {
        if (self.frames.items.len == 0) return null;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) return null;
        if (!std.mem.eql(u8, &frame.env_module_bytes, &module_bytes)) return null;
        return frame;
    }

    fn siteFor(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        instantiated_root: checked.CheckedTypeId,
    ) ?checked.CheckedInstantiationSite {
        const index = self.siteIndexFor(caller) orelse return null;
        const key = @intFromEnum(instantiated_root);
        if (index.ambiguous.contains(key)) {
            census.bump("rehearsal_skip_site_ambiguous");
            return null;
        }
        const site_index = index.by_root.get(key) orelse {
            census.bump("rehearsal_skip_no_site");
            return null;
        };
        return caller.view.instantiationSites()[site_index];
    }

    fn siteIndexFor(self: *Rehearsal, caller: direct_translate.ModuleCursor) ?*SiteIndex {
        const gop = self.site_index.getOrPut(self.allocator, caller.module_bytes) catch {
            self.fail();
            return null;
        };
        if (gop.found_existing) return gop.value_ptr;
        gop.value_ptr.* = .{ .view = caller.view, .by_root = .empty, .ambiguous = .empty };
        const index = gop.value_ptr;
        const sites = caller.view.instantiationSites();
        for (sites, 0..) |site, position| {
            const key = @intFromEnum(site.instantiated_root);
            const entry = index.by_root.getOrPut(self.allocator, key) catch {
                self.fail();
                return null;
            };
            if (entry.found_existing) {
                // Several edges legitimately record one instantiated root: a
                // re-checked source edge, and a value use also reached through a
                // shared-use record. They are the same instantiation when they
                // agree on scheme and positional actuals, and only a genuine
                // disagreement makes the root unusable as an edge name.
                if (!sitesAgree(caller.view, sites[entry.value_ptr.*], site)) {
                    index.ambiguous.put(self.allocator, key, {}) catch {
                        self.fail();
                        return null;
                    };
                }
                continue;
            }
            entry.value_ptr.* = @intCast(position);
        }
        return index;
    }

    fn takeEdge(self: *Rehearsal, reserved_fn_id: ?u32) ?RequestEdge {
        if (reserved_fn_id) |fn_id| {
            const found = self.edges_by_fn.fetchRemove(fn_id) orelse return null;
            return found.value;
        }
        const edge = self.pending_edge;
        self.pending_edge = null;
        return edge;
    }

    /// Emit one checked position under this specialization's environment and
    /// compare it against every distinct id the graph sealed there.
    fn comparePosition(
        self: *Rehearsal,
        frame: *const Frame,
        address: CheckedAddress,
        occurrences: Occurrences,
    ) void {
        var index: usize = 0;
        while (index < occurrences.overflow) : (index += 1) {
            census.bump("rehearsal_type_skip_other_occurrence");
        }
        if (occurrences.len == 0) return;

        const cursor = self.lookup.cursor(address.module_bytes) orelse {
            census.bump("rehearsal_type_skip_module_absent");
            return;
        };
        const in_env = std.mem.eql(u8, &address.module_bytes, &frame.env_module_bytes);
        const env = frame.environment();
        const env_ptr: ?*const direct_translate.BindingEnvironment = if (in_env) &env else null;
        const owner_node = if (in_env) frame.owner_node else checked.checked_residual_disposition_module_body_owner;
        if (!in_env) census.bump("rehearsal_type_outside_environment");

        var reason: direct_translate.SkipReason = undefined;
        const emitted = self.translator.translateUnderEnvironment(
            cursor,
            env_ptr,
            owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch |err| switch (err) {
            error.Skip => {
                switch (reason) {
                    .engine_input_needed => census.bump("rehearsal_type_skip_engine_input_needed"),
                    .open_row => census.bump("rehearsal_type_skip_open_row"),
                    .recursive_cycle => census.bump("rehearsal_type_skip_recursive"),
                    .pending_or_err => census.bump("rehearsal_type_skip_pending_or_err"),
                    .numeric_default_unresolved => census.bump("rehearsal_type_skip_numeric_default"),
                    .malformed_builtin_arity => census.bump("rehearsal_type_skip_malformed_arity"),
                    .binder_not_found => census.bump("rehearsal_type_skip_binder_not_found"),
                    .missing_backing => census.bump("rehearsal_type_skip_missing_backing"),
                }
                return;
            },
            else => return self.fail(),
        };

        _ = self.slotForEmitted(emitted, 0);

        const emitted_digest = self.store.typeDigest(self.program_names, emitted);
        var matched = false;
        for (occurrences.ids[0..occurrences.len]) |sealed| {
            census.bump("rehearsal_type_compared");
            const sealed_digest = self.program_types.typeDigest(self.program_names, sealed);
            if (std.mem.eql(u8, &emitted_digest.bytes, &sealed_digest.bytes)) {
                census.bump("rehearsal_type_match");
                matched = true;
                continue;
            }
            if (matched) {
                census.bump("rehearsal_type_skip_other_occurrence");
                continue;
            }
            self.recordMismatch(address, emitted, sealed, emitted_digest, sealed_digest);
        }
    }

    fn recordMismatch(
        self: *Rehearsal,
        address: CheckedAddress,
        emitted: Type.TypeId,
        sealed: Type.TypeId,
        emitted_digest: names.TypeDigest,
        sealed_digest: names.TypeDigest,
    ) void {
        const representation = self.sealedCarriesRepresentation(sealed) or self.emittedCarriesRepresentation(emitted);
        if (representation) {
            census.bump("rehearsal_type_mismatch_representation");
        } else {
            census.bump("rehearsal_type_mismatch_logical");
        }
        if (self.details.items.len >= max_mismatch_details) return;
        var prefix: [8]u8 = undefined;
        @memcpy(&prefix, address.module_bytes[0..8]);
        self.details.append(self.allocator, .{
            .module_prefix = prefix,
            .type_id = address.type_id,
            .representation = representation,
            .rehearsal_digest = emitted_digest,
            .graph_digest = sealed_digest,
            .rehearsal_head = HeadShape.of(&self.store, emitted),
            .graph_head = HeadShape.of(self.program_types, sealed),
            .difference = firstDifference(&self.store, emitted, self.program_types, sealed, self.program_names, 0),
        }) catch self.fail();
    }

    fn sealedCarriesRepresentation(self: *Rehearsal, root: Type.TypeId) bool {
        return self.carriesRepresentation(self.program_types, root);
    }

    fn emittedCarriesRepresentation(self: *Rehearsal, root: Type.TypeId) bool {
        return self.carriesRepresentation(&self.store, root);
    }

    /// Whether a type carries iterator or generated representation content
    /// anywhere, which classifies a difference on it as a representation gap
    /// rather than a directed-emission bug.
    fn carriesRepresentation(self: *Rehearsal, store: *const Type.Store, root: Type.TypeId) bool {
        var visited = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(Type.TypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return false;
        while (stack.pop()) |ty| {
            const gop = visited.getOrPut(ty) catch return false;
            if (gop.found_existing) continue;
            switch (store.get(ty)) {
                .primitive, .zst, .erased => {},
                .list, .box => |elem| stack.append(self.allocator, elem) catch return false,
                .tuple => |span| {
                    const items = store.span(span);
                    for (0..GuardedList.borrowLen(items)) |i| {
                        stack.append(self.allocator, GuardedList.at(items, i)) catch return false;
                    }
                },
                .record => |span| {
                    const fields = store.fieldSpan(span);
                    for (0..GuardedList.borrowLen(fields)) |i| {
                        stack.append(self.allocator, GuardedList.at(fields, i).ty) catch return false;
                    }
                },
                .tag_union => |span| {
                    const tags = store.tagSpan(span);
                    for (0..GuardedList.borrowLen(tags)) |i| {
                        const payloads = store.span(GuardedList.at(tags, i).payloads);
                        for (0..GuardedList.borrowLen(payloads)) |j| {
                            stack.append(self.allocator, GuardedList.at(payloads, j)) catch return false;
                        }
                    }
                },
                .func => |fn_ty| {
                    const args = store.span(fn_ty.args);
                    for (0..GuardedList.borrowLen(args)) |i| {
                        stack.append(self.allocator, GuardedList.at(args, i)) catch return false;
                    }
                    stack.append(self.allocator, fn_ty.ret) catch return false;
                },
                .named => |named| {
                    if (named.def.iterator_representation != .none or named.def.generated != null) return true;
                    const args = store.span(named.args);
                    for (0..GuardedList.borrowLen(args)) |i| {
                        stack.append(self.allocator, GuardedList.at(args, i)) catch return false;
                    }
                    if (named.backing) |backing| stack.append(self.allocator, backing.ty) catch return false;
                },
            }
        }
        return false;
    }

    /// Build (memoized) the representation slot for one emitted position
    /// (reunify.md section 10.2). Two positions that emitted the same id are the
    /// same occurrence and share a slot; two independently emitted occurrences of
    /// one structure get distinct slots (reunify.md section 9.3).
    fn slotForEmitted(self: *Rehearsal, ty: Type.TypeId, depth: u32) ?closure.RepresentationSlotId {
        if (self.slots.get(ty)) |existing| return existing;
        if (depth >= max_slot_depth) return null;
        const token = self.tokenFor(ty) orelse return null;
        const shape = self.shapeFor(ty, token, depth) orelse return null;
        const slot = self.engine.createSlot(token, self.freshProducer(), shape) catch return null;
        self.slots.put(self.allocator, ty, slot) catch return null;
        if (shape == .iterator) {
            self.slot_descriptors.put(self.allocator, @intFromEnum(slot), shape.iterator.descriptor) catch return null;
        }
        census.bump("rehearsal_slots_created");
        return slot;
    }

    fn shapeFor(self: *Rehearsal, ty: Type.TypeId, token: closure.LogicalToken, depth: u32) ?closure.SlotShape {
        switch (self.store.get(ty)) {
            .list, .box => |elem| {
                const child = self.slotForEmitted(elem, depth + 1) orelse return null;
                return .{ .wrapper = child };
            },
            .named => |named| {
                const owner = named.builtin_owner;
                if (owner != null and static_dispatch.isIteratorOwner(owner.?)) {
                    const args = self.store.span(named.args);
                    if (GuardedList.borrowLen(args) >= 1) {
                        const item = self.slotForEmitted(GuardedList.at(args, 0), depth + 1) orelse return null;
                        const backing = if (named.backing) |backing_ty|
                            (self.slotForEmitted(backing_ty.ty, depth + 1) orelse return null)
                        else
                            (self.standInBacking() orelse return null);
                        return .{ .iterator = .{
                            .descriptor = descriptorOf(named, GuardedList.borrowLen(args)),
                            .item = item,
                            .backing = backing,
                        } };
                    }
                }
                if (policy.evidenceOwnerUsesScoreSelection(owner)) {
                    return .{ .evidence = .{ .score = 0 } };
                }
                if (named.backing) |backing_ty| {
                    const child = self.slotForEmitted(backing_ty.ty, depth + 1) orelse return null;
                    return .{ .wrapper = child };
                }
                return .{ .leaf = @intFromEnum(token) };
            },
            else => return .{ .leaf = @intFromEnum(token) },
        }
    }

    /// Relate the two sides of this specialization's representation interface
    /// (reunify.md sections 10.3, 11.1): the request context's emission of the
    /// requesting edge and the callee's scheme root emitted under the binding are
    /// two independently emitted occurrences of one type, so the edge between
    /// them is an explicit relation, not shared storage. The engine refuses the
    /// pair when their logical identities differ, which is recorded rather than
    /// assumed away.
    fn relateInterface(self: *Rehearsal, frame: *const Frame) void {
        const requested = frame.request_root orelse return;
        const declared = frame.interface_root orelse return;
        const request_slot = self.slotForEmitted(requested, 0) orelse return;
        const declared_slot = self.slotForEmitted(declared, 0) orelse return;
        if (self.engine.related(request_slot, declared_slot)) {
            census.bump("rehearsal_interface_already_related");
            return;
        }
        self.engine.relate(request_slot, declared_slot, .component_equality) catch |err| switch (err) {
            error.LogicallyUnequal => {
                census.bump("rehearsal_interface_relate_rejected");
                return;
            },
            else => return self.fail(),
        };
        census.bump("rehearsal_interface_relate_applied");
    }

    /// Seal this specialization's slots (reunify.md section 10.6): every slot's
    /// logical identity must survive, and the sealed descriptor must still be the
    /// one emitted at that position — otherwise the emitted type would have to be
    /// re-materialized from the sealed slot, which the counter records.
    fn sealSlots(self: *Rehearsal) void {
        var it = self.slots.iterator();
        while (it.next()) |entry| {
            const slot = entry.value_ptr.*;
            const representative = self.engine.find(slot);
            census.bump("rehearsal_seal_positions");
            if (representative != slot) census.bump("rehearsal_relations_applied");
            const emitted_descriptor = self.slot_descriptors.get(@intFromEnum(slot)) orelse continue;
            switch (self.engine.shapeOf(representative)) {
                .iterator => |sealed| {
                    if (!descriptorsAgree(emitted_descriptor, sealed.descriptor)) {
                        census.bump("rehearsal_seal_descriptor_moved");
                    }
                },
                else => census.bump("rehearsal_seal_descriptor_moved"),
            }
        }
    }

    fn standInBacking(self: *Rehearsal) ?closure.RepresentationSlotId {
        return self.engine.createSlot(.stand_in, self.freshProducer(), .{ .leaf = 0 }) catch null;
    }

    fn freshProducer(self: *Rehearsal) closure.ProducerAtom {
        const atom: closure.ProducerAtom = @enumFromInt(self.next_producer);
        self.next_producer +%= 1;
        return atom;
    }

    /// The dense engine token for an emitted type's logical identity. Equal
    /// tokens are the engine's precondition for relating two slots, so the token
    /// erases exactly the representation content a rule may move.
    fn tokenFor(self: *Rehearsal, ty: Type.TypeId) ?closure.LogicalToken {
        const digest = self.store.typeDigest(self.program_names, ty);
        const gop = self.logical_tokens.getOrPut(self.allocator, digest.bytes) catch return null;
        if (!gop.found_existing) {
            gop.value_ptr.* = self.next_token;
            self.next_token +%= 1;
        }
        return @enumFromInt(gop.value_ptr.*);
    }

    fn dumpDetails(self: *Rehearsal) void {
        if (comptime !census.enabled) return;
        if (self.details.items.len == 0) return;
        const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(self.allocator);
        for (self.details.items) |detail| {
            const module_hex = std.fmt.bytesToHex(detail.module_prefix, .lower);
            const emitted_hex = std.fmt.bytesToHex(detail.rehearsal_digest.bytes[0..8].*, .lower);
            const graph_hex = std.fmt.bytesToHex(detail.graph_digest.bytes[0..8].*, .lower);
            const line = std.fmt.allocPrint(
                self.allocator,
                "rehearsal_mismatch_detail module={s} checked_ty={d} representation={d} rehearsal={s}/{s}:{d} graph={s}/{s}:{d} differs_at_depth={d} {s}:{d}vs{s}:{d} named_field={s} recursive={d}/{d}\n",
                .{
                    &module_hex,
                    detail.type_id,
                    @intFromBool(detail.representation),
                    &emitted_hex,
                    @tagName(detail.rehearsal_head.tag),
                    detail.rehearsal_head.children,
                    &graph_hex,
                    @tagName(detail.graph_head.tag),
                    detail.graph_head.children,
                    detail.difference.depth,
                    @tagName(detail.difference.left.tag),
                    detail.difference.left.children,
                    @tagName(detail.difference.right.tag),
                    detail.difference.right.children,
                    @tagName(detail.difference.named_field),
                    @intFromBool(detail.difference.left_recursive),
                    @intFromBool(detail.difference.right_recursive),
                },
            ) catch return;
            defer self.allocator.free(line);
            text.appendSlice(self.allocator, line) catch return;
        }
        census.appendToFile(raw_path, text.items);
    }
};

/// Whether two edges recording one instantiated root describe the same
/// instantiation: same owning scheme (and defining module) and the same
/// positional actuals. Two such edges name one binding, so either may be read.
fn sitesAgree(
    view: checked.CheckedTypeStoreView,
    left: checked.CheckedInstantiationSite,
    right: checked.CheckedInstantiationSite,
) bool {
    if (left.scheme != right.scheme) return false;
    if (!std.meta.eql(left.defining_module_hash, right.defining_module_hash)) return false;
    const left_actuals = left.actuals(view);
    const right_actuals = right.actuals(view);
    if (left_actuals.len != right_actuals.len) return false;
    for (left_actuals, right_actuals) |a, b| {
        if (a != b) return false;
    }
    return true;
}

/// The immutable descriptor the shared representation policy reads, copied out
/// of an emitted named type.
fn descriptorOf(named: Type.NamedContent, arg_count: usize) policy.NamedDescriptor {
    return .{
        .kind = named.kind,
        .def = named.def,
        .builtin_owner = named.builtin_owner,
        .arg_count = arg_count,
        .backing_use = if (named.backing) |backing| backing.use else null,
    };
}

/// Whether a sealed slot still carries the representation the emission put
/// there, across every field the flip must preserve.
fn descriptorsAgree(emitted: policy.NamedDescriptor, sealed: policy.NamedDescriptor) bool {
    return emitted.def.iterator_representation == sealed.def.iterator_representation and
        emitted.def.iterator_kind == sealed.def.iterator_kind and
        emitted.def.iterator_depth == sealed.def.iterator_depth and
        emitted.builtin_owner == sealed.builtin_owner and
        emitted.kind == sealed.kind;
}

const testing = std.testing;

test "a seal trace joins one node's checked provenance to its sealed id" {
    var trace = SealTrace.init(testing.allocator);
    defer trace.deinit();

    const address = CheckedAddress{ .module_bytes = [_]u8{3} ** 32, .type_id = 17 };
    trace.noteProvenance(4, address);
    trace.noteSealed(4, @enumFromInt(9));

    // A repeated provenance keeps the first address; a repeated seal keeps the
    // latest committed id, which is what lowering carries forward.
    trace.noteProvenance(4, .{ .module_bytes = [_]u8{5} ** 32, .type_id = 18 });
    trace.noteSealed(4, @enumFromInt(11));

    const recorded = trace.provenance.get(4) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(u32, 17), recorded.type_id);
    try testing.expectEqual(@as(Type.TypeId, @enumFromInt(11)), trace.sealed.get(4).?);
}

test "occurrence recording is bounded and deduplicated" {
    var occurrences = Occurrences.empty();

    occurrences.record(@enumFromInt(1));
    occurrences.record(@enumFromInt(1));
    try testing.expectEqual(@as(usize, 1), occurrences.len);

    var next: u32 = 2;
    while (next < 2 + max_occurrences_per_position) : (next += 1) {
        occurrences.record(@enumFromInt(next));
    }
    try testing.expectEqual(max_occurrences_per_position, occurrences.len);
    try testing.expect(occurrences.overflow > 0);
}
