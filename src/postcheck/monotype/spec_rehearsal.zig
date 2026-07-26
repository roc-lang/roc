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

/// The maximum number of distinct unresolved specializations named in the census
/// dump. The population is a handful of definitions repeated many times, so this
/// bounds the list without hiding a kind.
const max_unresolved_details: usize = 512;

/// One checked type's address: the content identity of the module whose store
/// holds it, plus its id within that store.
pub const CheckedAddress = struct {
    module_bytes: [32]u8,
    type_id: u32,
};

/// reunify.md section 9.6's declared compiler-generated instantiation rules.
///
/// A compiler-generated call edge has no checked use site, so no
/// `CheckedInstantiationSite` records its dense actuals and nothing may be
/// matched structurally out of the concrete callable to make one up. Each
/// generating site therefore names the rule it emits under, and each rule states
/// exactly where its callee scheme's binder values come from. A rule whose
/// generating site cannot hand over that source stays declared and unbound: it
/// names the missing datum instead of guessing one.
///
/// The declarations live next to the compiler-generated-edge contract in
/// `design.md` (Static Dispatch In Monotype). Members are one per generating
/// family, and each family's mapping is stated on its member here.
pub const GeneratedInstantiationRule = enum {
    /// The generated-iterator dispatch: a `for` loop's `iter` and `next` calls,
    /// whose synthetic receiver-dispatch constraints the checker introduces with
    /// no introducing expression, so their `dispatch_target` instantiation sites
    /// are recorded under a use node of zero and no use expression names them.
    ///
    /// Binder mapping (exact, total): the callee's scheme binder `i` takes
    /// argument `i` of the dispatch plan's own checked dispatcher type, read
    /// alias-transparently as a named type. `Builtin.List.iter` is
    /// `List(item) -> Iter(item)` over a `List(X)` dispatcher, and
    /// `Builtin.Iter.next` is `Iter(item) -> [...]` over an `Iter(X)` dispatcher;
    /// in both the dispatcher's argument list is exactly the callee's binder
    /// list in order. A dispatcher whose argument count differs from the callee
    /// scheme's binder count is outside the rule and binds nothing.
    ///
    /// Witness: the plan's own checked callable type, emitted under the
    /// requesting body's environment, must equal the callee scheme root emitted
    /// under the binding.
    iterator_dispatch_receiver,
    /// `to_inspect` on a component of the value being inspected.
    ///
    /// Declared-but-unbound. Missing datum: the inspect walk descends a Monotype
    /// and calls the component method on a Monotype component; the checked type
    /// the walk started from is not carried down, so no checked receiver names
    /// the component. Binding it needs the inspect walk to carry the checked
    /// type alongside the Monotype it descends.
    inspect_component,
    /// `is_eq`/`to_hash` on a component the structural-derivation ladder reached.
    ///
    /// Declared-but-unbound. Missing datum: as for `inspect_component` — the
    /// derivation ladder is driven entirely by Monotype structure (its expansion
    /// stack and memoized helper defs are keyed on Monotype ids), so the checked
    /// type of the component it dispatches on does not exist at the call.
    structural_derivation_component,
    /// `is_eq` on the scrutinee type of a literal pattern.
    ///
    /// Declared-but-unbound. Missing datum: the pattern guard entry carries the
    /// Monotype the comparison happens at, not the checked type of the
    /// scrutinee.
    pattern_literal_equality,
    /// `Set.from_list`/`Set.to_list` behind a set literal.
    ///
    /// Declared-but-unbound. Missing datum: the literal's helper receiver types
    /// are Monotypes the literal lowering builds from the element type it
    /// lowered, so no checked type names them.
    set_literal_helper,
    /// `Dict.with_capacity`/`Dict.insert`/`Dict.to_list` behind a dict literal.
    ///
    /// Declared-but-unbound. Missing datum: as for `set_literal_helper`.
    dict_literal_helper,
    /// The encoding format's parse helpers: `parse_<scalar>`, `parse_record_field`,
    /// `skip_record_field`, `parse_null`, `parser_for`, and the format's own
    /// `parse_*` entry.
    ///
    /// Declared-but-unbound, and the receiver would not supply the binder even
    /// if it were checked: the receiver is the encoding format nominal, which
    /// takes no arguments, while the callee's one binder is the OPEN EXTENSION
    /// of its error tag union (`Try(..., [InvalidJson(Str), ..])`). Missing
    /// datum: the checked error row the call site's expected return type fixes,
    /// which the parse walk holds only as a Monotype.
    json_parse_helper,
    /// The encoding format's encode helpers: `encoder_for` and the format's own
    /// `encode_*` entry.
    ///
    /// Declared-but-unbound, for the same reason as `json_parse_helper`: the
    /// callee's one binder is the never-failing error type of its `Try` result,
    /// not an argument of the format receiver.
    json_encode_helper,
    /// The encoding format's `rename_field` helper.
    ///
    /// Declared-but-unbound. Missing datum: as for `json_parse_helper`.
    json_record_field_name,
    /// The encoding format's `invalid_value` helper.
    ///
    /// Declared-but-unbound. Missing datum: as for `json_parse_helper`.
    json_invalid_value,

    /// Whether this rule's generating sites hand over the checked data its
    /// binder mapping reads. The declaration is authoritative: a rule that
    /// answers false binds nothing, however much a site passes, so a generator
    /// stays declared-but-unbound until its missing datum above is supplied.
    pub fn declaresBinderSource(self: GeneratedInstantiationRule) bool {
        return switch (self) {
            .iterator_dispatch_receiver => true,
            .inspect_component,
            .structural_derivation_component,
            .pattern_literal_equality,
            .set_literal_helper,
            .dict_literal_helper,
            .json_parse_helper,
            .json_encode_helper,
            .json_record_field_name,
            .json_invalid_value,
            => false,
        };
    }
};

/// How many declared rules there are, for the per-rule outcome table.
const generated_rule_count = @typeInfo(GeneratedInstantiationRule).@"enum".fields.len;

/// One declared generated edge as its generating site names it: the rule, and
/// the checked data that rule's binder mapping and witness read, when the site
/// holds it (reunify.md section 9.6).
pub const GeneratedEdge = struct {
    rule: GeneratedInstantiationRule,
    source: ?GeneratedSource = null,
};

/// The checked data one generating site hands over: the module whose store holds
/// both ids, the receiver the rule's binder mapping reads, and the callable the
/// request names, which the exact structural witness compares against.
pub const GeneratedSource = struct {
    module_bytes: [32]u8,
    receiver: checked.CheckedTypeId,
    requested: checked.CheckedTypeId,
};

/// The requesting edge of one specialization: the module whose body made the
/// request, and the checked expression the request was made at. That expression
/// is the `use_node` half of the section 7.2 edge identity, resolved to a checked
/// id by the checker, so it names the `CheckedInstantiationSite` whose dense
/// actuals bind the callee scheme exactly.
pub const RequestEdge = struct {
    module_bytes: [32]u8,
    use_expr: checked.CheckedExprId,
    /// The requesting body's own binding at the moment the request was made. A
    /// request that reserves is lowered later, from a completely different frame
    /// stack, so the environment a symbolic actual resolves under travels with
    /// the edge instead of being read off whatever frame happens to be active
    /// when the request is finally lowered (reunify.md sections 7.3, 9.1).
    caller: ?CapturedEnvironment,
};

/// One declared generated edge together with the requesting body's own binding
/// at the moment the request was made, which the rule's receiver and witness
/// both translate under (reunify.md sections 7.3, 9.1, 9.6).
const GeneratedRequest = struct {
    edge: GeneratedEdge,
    caller: ?CapturedEnvironment,
};

/// What one open request scope names: nothing, the checked use site the request
/// was made at, or the declared generated rule that produced it.
const RequestScope = union(enum) {
    none,
    checked: RequestEdge,
    generated: GeneratedRequest,
};

/// What a reservation claimed from its request scope, held under the reserved
/// function id until that specialization's body lowers.
const ClaimedRequest = union(enum) {
    checked: RequestEdge,
    generated: GeneratedRequest,
};

/// Where the residual materialization one binding carries came from, so a
/// mismatching position attributes its empty tag union to the exact binding
/// that produced it rather than to the position it surfaced at. A binding that
/// inherits a residual value from its requesting context inherits that
/// context's origin, so every cascade reports the origin at its head.
pub const ResidualOrigin = enum {
    /// No binder of this binding carries a residual materialization.
    absent,
    /// The request carried no requesting environment, because an enclosing
    /// specialization's own environment never resolved (its class is one of the
    /// `rehearsal_edgeless_binders_*` counters), so an actual naming that
    /// context's binder resolved against nothing.
    unresolved_request_context,
    /// The actual is a checked variable that some scheme of the requesting
    /// module names as a binder, which this environment chain does not bind.
    scheme_binder,
    /// The actual is a residual variable carrying a disposition recorded under
    /// the requesting body context itself (reunify.md section 7.4), so the
    /// emission applied exactly the disposition checking recorded there.
    disposed_here,
    /// The actual is a residual variable carrying a disposition recorded under a
    /// different body context than the requesting one (reunify.md section 7.4).
    disposed_elsewhere,
    /// The actual is a residual variable carrying no disposition at all.
    undisposed,
    /// The actual reaches no checked variable: its empty tag union is checked
    /// content, not a materialization.
    closed_empty_row,
};

/// A copy of one requesting body's binding environment chain, owned by the edge
/// that captured it. The chain's innermost level is the requesting body's own
/// binding; its enclosing levels are the lexical environments that body itself
/// resolved under (reunify.md sections 7.1, 7.3).
pub const CapturedEnvironment = struct {
    /// The requesting context's own residual origin, so a binding that inherits
    /// a residual value reports where that value came from.
    residual_origin: ResidualOrigin = .absent,
    module_bytes: [32]u8,
    owner_node: u32,
    chain: EnvironmentChain,

    fn environment(self: *const CapturedEnvironment) ?*const direct_translate.BindingEnvironment {
        return self.chain.innermost();
    }
};

/// A self-contained copy of one lexical binding environment chain (reunify.md
/// section 7.3). `levels` runs outermost first and each level's `parent` points
/// at the level before it, so the last level is the innermost environment and
/// the whole chain stays usable after the frames it was copied from are gone.
/// `values` is the single allocation backing every level's bound and captured
/// slices; `binders` slices alias frozen checked data and are never copied.
const EnvironmentChain = struct {
    levels: []direct_translate.BindingEnvironment,
    values: []direct_translate.BoundType,

    /// A chain that owns nothing and names no environment.
    const none = EnvironmentChain{ .levels = &.{}, .values = &.{} };

    /// The innermost environment of the chain, or null when the chain is empty.
    fn innermost(self: *const EnvironmentChain) ?*const direct_translate.BindingEnvironment {
        if (self.levels.len == 0) return null;
        return &self.levels[self.levels.len - 1];
    }

    /// How many levels deep the chain runs, counting from the outermost.
    fn depth(self: *const EnvironmentChain) usize {
        return self.levels.len;
    }

    fn release(self: *EnvironmentChain, allocator: Allocator) void {
        if (self.levels.len != 0) allocator.free(self.levels);
        if (self.values.len != 0) allocator.free(self.values);
        self.* = EnvironmentChain.none;
    }
};

/// One level of a chain under construction: the scheme it binds, its binders in
/// checked data, and the dense values for them.
const EnvironmentLevel = struct {
    scheme: direct_translate.SchemeIdent,
    binders: []const checked.CheckedTypeId,
    bound: []const direct_translate.BoundType,
    captured: []const direct_translate.BoundType,
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
    /// The function id this specialization reserved. The requesting edge is
    /// stored under exactly this id when the request that reserved it named one,
    /// so an edge reaches the specialization that requested it and no other.
    reserved_fn_id: u32,
    /// This template's target kind, which says whether an edgeless
    /// specialization is an entry root, a hosted boundary, or ordinary Roc code.
    target_kind: std.meta.Tag(checked.ProcTarget),
    /// This template's exported name when it has one, so the census dump names
    /// which definition an unresolved specialization belongs to.
    template_name: []const u8,
    /// The specialized template's own owning scheme, named by the scheme id the
    /// checked procedure template carries and qualified by the defining checked
    /// module (reunify.md section 7.1). It is the environment a specialization
    /// has even when no requesting edge named it (section 9.6's generated edges
    /// and the root requests that have no requesting site). Null for a template
    /// whose owner is a checked type rather than a defining-module node, which is
    /// the synthesized wrapper kinds.
    template_scheme: ?checked.CheckedTypeSchemeId,
};

/// One module's instantiation sites indexed by the edge identity a consumer can
/// name them by: the checked expression the edge is used at together with the
/// CIR node its callee scheme is owned by (reunify.md section 7.2's
/// `(use_node, ..., scheme_owner_node)` identity). One expression carries an edge
/// per callee it instantiates — an operator desugaring reaches several — so the
/// use alone under-keys them, while the pair names exactly one callee's edge. A
/// pair still carrying edges that disagree on scheme or actuals is ambiguous and
/// names no edge.
const SiteIndex = struct {
    view: checked.CheckedTypeStoreView,
    by_edge: std.AutoHashMapUnmanaged(u64, u32),
    ambiguous: std.AutoHashMapUnmanaged(u64, void),
    /// Every use expression any recorded site names, so a lookup that finds no
    /// edge says whether the use carries sites owned by other definitions or
    /// carries none at all.
    used_exprs: std.AutoHashMapUnmanaged(u32, void),
};

/// The index key for one edge identity.
fn siteKey(use_expr: checked.CheckedExprId, scheme_owner_node: u32) u64 {
    return (@as(u64, @intFromEnum(use_expr)) << 32) | @as(u64, scheme_owner_node);
}

/// The CIR node this specialization's own template scheme is owned by, which is
/// the `scheme_owner_node` half of its requesting edge's identity.
fn templateSchemeOwnerNode(start: SpecializationStart) ?u32 {
    const scheme_id = start.template_scheme orelse return null;
    const scheme = start.cursor.view.schemeById(scheme_id) orelse return null;
    return scheme.owner_node;
}

/// One active specialization's environment plus the graph trace it compares
/// against. `chain` ends at this specialization's own level, whose bound values
/// are dense and ordered exactly like `binders` (reunify.md section 9.1); the
/// levels before it are the lexically enclosing environments the callee scheme's
/// checked captured binders name (reunify.md sections 7.1, 7.3). The whole
/// chain is owned by the rehearsal and freed when the frame pops. The trace is
/// heap-allocated so the graph's pointer to it survives the frame stack growing
/// under a nested specialization.
const Frame = struct {
    trace: *SealTrace,
    /// The module whose ids `binders` name, and whose residual dispositions
    /// `owner_node` selects. Only positions in this module translate under the
    /// environment; a position in another module has no binder in scope.
    env_module_bytes: [32]u8,
    scheme: direct_translate.SchemeIdent,
    owner_node: u32,
    binders: []const checked.CheckedTypeId,
    chain: EnvironmentChain,
    /// The callee's scheme root emitted under this binding: the specialization's
    /// own interface type (reunify.md section 11.1).
    interface_root: ?Type.TypeId,
    /// The requesting edge's instantiated root emitted under the CALLER's
    /// environment: the request context's side of the same interface.
    request_root: ?Type.TypeId,
    env_ready: bool,
    /// Where this binding's residual materialization came from, if any.
    residual_origin: ResidualOrigin = .absent,

    fn environment(self: *const Frame) ?*const direct_translate.BindingEnvironment {
        return self.chain.innermost();
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
    /// How many binders the specialization's environment carried, so a detail
    /// says whether an unbound position sat under an empty binding.
    binder_count: u32,
    rehearsal_digest: names.TypeDigest,
    graph_digest: names.TypeDigest,
    rehearsal_head: HeadShape,
    graph_head: HeadShape,
    difference: Difference,
};

/// A type's outermost shape: its content tag, how many children it holds, and
/// how many labelled entries its head declares. The two counts differ exactly
/// where a row's width is not its payload count: `[A, B]` holds two entries and
/// no children, and the empty tag union holds neither — the shape a residual
/// variable materializes to.
const HeadShape = struct {
    tag: std.meta.Tag(Type.Content),
    children: u32,
    entries: u32,

    fn of(store: *const Type.Store, ty: Type.TypeId) HeadShape {
        return .{
            .tag = std.meta.activeTag(store.get(ty)),
            .children = childCount(store, ty),
            .entries = entryCount(store, ty),
        };
    }

    /// Whether this head is the stored empty tag union — what an undisposed,
    /// undefaulted residual variable translates to.
    fn isEmptyTagUnionHead(self: HeadShape) bool {
        return self.tag == .tag_union and self.entries == 0;
    }
};

/// How many labelled entries a head declares: a tag union's tags, a record's
/// fields, a tuple's elements, a function's arguments, a named type's arguments.
fn entryCount(store: *const Type.Store, ty: Type.TypeId) u32 {
    return switch (store.get(ty)) {
        .primitive, .zst, .erased, .list, .box => 0,
        .tuple => |span| @intCast(GuardedList.borrowLen(store.span(span))),
        .record => |span| @intCast(GuardedList.borrowLen(store.fieldSpan(span))),
        .tag_union => |span| @intCast(GuardedList.borrowLen(store.tagSpan(span))),
        .func => |fn_ty| @intCast(GuardedList.borrowLen(store.span(fn_ty.args))),
        .named => |named| @intCast(GuardedList.borrowLen(store.span(named.args))),
    };
}

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
    if (depth >= max_recursion_probe_depth) return false;
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
/// Deep enough that a recursive pair which agrees on every head it reaches is a
/// statement about the two types rather than about the bound.
const max_difference_depth: u32 = 96;

/// How deep the self-reachability probe searches before answering "not
/// recursive". A cycle a type actually has closes within a few levels; this only
/// bounds the search on a deep acyclic spine.
const max_recursion_probe_depth: u32 = 32;

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
    // A named head's own identity fields are part of the difference, so a
    // disagreement there is where the two types part company even though every
    // head below it agrees.
    if (here.named_field != .not_named and here.named_field != .equal) return here;

    var index: u32 = 0;
    while (index < here.left.children) : (index += 1) {
        const left_child = childAt(left_store, left, index) orelse return here;
        const right_child = childAt(right_store, right, index) orelse return here;
        // Descend by the ENTRY-INDEPENDENT digest (reunify.md section 8.3): a
        // stored digest encodes a recursive back reference by visiting-stack
        // position, so two children of one rooted graph reached through
        // different entry paths compare unequal and the walk would descend a
        // rooting difference instead of the content one it is looking for.
        const left_digest = left_store.unfoldedDigest(name_store, left_child);
        const right_digest = right_store.unfoldedDigest(name_store, right_child);
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

/// How one declared generated rule's edges resolved, counted per rule so a rule
/// that binds and a rule that stays declared-but-unbound are never summed
/// together (reunify.md section 9.6).
const GeneratedOutcome = struct {
    /// Edges this rule produced that reserved a specialization.
    claimed: u64 = 0,
    /// Claimed edges whose specialization's own scheme carries no binders, so
    /// the empty binding is the whole binding and the rule supplies nothing.
    ground: u64 = 0,
    /// Claimed edges whose specialization's scheme carries binders and whose
    /// rule declares no binder source: declared-but-unbound.
    unbound: u64 = 0,
    /// Binder-carrying edges whose rule bound an environment that the exact
    /// structural witness accepted.
    witness_agrees: u64 = 0,
    /// Binder-carrying edges whose rule bound an environment the witness
    /// rejected; the binding is released and the specialization stays
    /// unresolved.
    witness_differs: u64 = 0,
    /// Binder-carrying edges whose witness could not be emitted on one side or
    /// the other, so nothing accepted the binding.
    witness_absent: u64 = 0,
    /// Binder-carrying edges whose declared receiver was absent, unresolvable,
    /// or not the nominal shape the rule names.
    receiver_unusable: u64 = 0,

    fn isEmpty(self: GeneratedOutcome) bool {
        return std.meta.eql(self, GeneratedOutcome{});
    }
};

/// One distinct unresolved specialization, dumped with the census counters so
/// the population that still needs a declared binding is named rather than
/// counted: which definition it specializes, which module owns it, how many
/// binders its scheme carries, and why its requesting edge supplied nothing.
const UnresolvedDetail = struct {
    name: [48]u8,
    template_module: [6]u8,
    binders: u32,
    skip: Rehearsal.EdgeSkip,
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
    /// The open request scopes, innermost last. The seam opens one around every
    /// request it makes and closes it when the request finishes, so the edge a
    /// use site named is only ever visible to the request that site made — a
    /// reservation claims the innermost scope's edge, and a scope that closes
    /// unclaimed drops it instead of leaving it for an unrelated later request.
    /// An entry is `.none` once claimed, and for a request whose use site named
    /// no edge at all.
    requests: std.ArrayList(RequestScope),
    /// The requesting edge of each reserved function id: the identity a request
    /// is tied to from reservation until its body lowers (reunify.md 11.3).
    edges_by_fn: std.AutoHashMapUnmanaged(u32, ClaimedRequest),
    /// Per declared generated rule, how its edges resolved (reunify.md 9.6).
    generated_outcomes: [generated_rule_count]GeneratedOutcome,
    site_index: std.AutoHashMapUnmanaged([32]u8, SiteIndex),
    /// Interned logical-identity digests to dense engine tokens. Two slots may
    /// relate only when their tokens are equal.
    logical_tokens: std.AutoHashMapUnmanaged([32]u8, u64),
    next_token: u64,
    next_producer: u32,
    /// Every representation slot the specialization being sealed created, in
    /// creation order. A slot belongs to one emitted OCCURRENCE — the compared
    /// position it was built under and the position path inside it — never to a
    /// stored type id, so two independent occurrences of one structure begin with
    /// distinct slots and only recursion or an explicit relation joins them
    /// (reunify.md section 9.3's occurrence-safety law).
    slots: std.ArrayList(closure.RepresentationSlotId),
    /// The descriptor each iterator slot was created with, so sealing can see
    /// whether the closure moved it.
    slot_descriptors: std.AutoHashMapUnmanaged(u32, policy.NamedDescriptor),
    details: std.ArrayList(MismatchDetail),
    unresolved_details: std.ArrayList(UnresolvedDetail),
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
            .requests = .empty,
            .edges_by_fn = .empty,
            .generated_outcomes = [_]GeneratedOutcome{.{}} ** generated_rule_count,
            .site_index = .empty,
            .logical_tokens = .empty,
            .next_token = 1,
            .next_producer = 1,
            .slots = .empty,
            .slot_descriptors = .empty,
            .details = .empty,
            .unresolved_details = .empty,
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
        self.unresolved_details.deinit(self.allocator);
        self.slot_descriptors.deinit(self.allocator);
        self.slots.deinit(self.allocator);
        self.logical_tokens.deinit(self.allocator);
        var indexes = self.site_index.valueIterator();
        while (indexes.next()) |index| {
            index.by_edge.deinit(self.allocator);
            index.ambiguous.deinit(self.allocator);
            index.used_exprs.deinit(self.allocator);
        }
        self.site_index.deinit(self.allocator);
        for (self.requests.items) |open| {
            self.releaseScope(open);
        }
        self.requests.deinit(self.allocator);
        var held = self.edges_by_fn.valueIterator();
        while (held.next()) |claim| {
            self.releaseClaim(claim.*);
        }
        self.edges_by_fn.deinit(self.allocator);
        self.engine.deinit();
        self.translator.deinit();
        self.store.deinit();
        self.allocator.destroy(self);
    }

    /// Open a request scope naming the edge the request is made from: the
    /// caller's module and the checked expression the use sits at. The scope
    /// must be closed by `closeRequest` when the request finishes, so an edge no
    /// reservation claimed cannot be read by a later, unrelated request.
    pub fn openRequestEdge(self: *Rehearsal, module_bytes: [32]u8, use_expr: checked.CheckedExprId) void {
        if (self.disabled) return;
        const edge = RequestEdge{
            .module_bytes = module_bytes,
            .use_expr = use_expr,
            .caller = self.captureCallerEnvironment(module_bytes),
        };
        self.requests.append(self.allocator, .{ .checked = edge }) catch {
            self.releaseEdge(edge);
            self.fail();
        };
    }

    /// Open a request scope for a use that names no instantiation edge. The
    /// scope still exists so the requests made inside it cannot reach an
    /// enclosing scope's edge.
    pub fn openRequestWithoutEdge(self: *Rehearsal) void {
        if (self.disabled) return;
        self.requests.append(self.allocator, .none) catch self.fail();
    }

    /// Open a request scope for a declared compiler-generated edge (reunify.md
    /// section 9.6). The scope names the rule the generating site emits under
    /// and the checked data that rule's binder mapping reads, so the
    /// specialization this request reserves records which rule produced it and
    /// under which requesting environment.
    pub fn openGeneratedRequest(self: *Rehearsal, edge: GeneratedEdge) void {
        if (self.disabled) return;
        const request = GeneratedRequest{
            .edge = edge,
            .caller = if (edge.source) |source|
                self.captureCallerEnvironment(source.module_bytes)
            else
                null,
        };
        self.requests.append(self.allocator, .{ .generated = request }) catch {
            self.releaseGeneratedRequest(request);
            self.fail();
        };
    }

    /// Close the innermost request scope. An edge no reservation claimed
    /// belonged to a request that bound no new specialization, so it is dropped
    /// rather than left for whichever specialization lowers next.
    pub fn closeRequest(self: *Rehearsal) void {
        // Once the rehearsal disables itself no scope is opened, so none is
        // closed either and the stack stays balanced across the transition.
        if (self.disabled) return;
        if (self.requests.items.len == 0) return;
        const open = self.requests.pop() orelse return;
        switch (open) {
            .none => {},
            .generated => |request| self.releaseGeneratedRequest(request),
            .checked => |edge| {
                census.bump("rehearsal_request_edge_unclaimed");
                self.releaseEdge(edge);
            },
        }
    }

    /// Bind the innermost open request's edge to the function id that request
    /// reserved, which is the identity the specialization is lowered under
    /// however much later that happens (reunify.md 11.3). A reservation made
    /// outside any edge-naming request scope claims nothing.
    pub fn claimRequestEdge(self: *Rehearsal, fn_id: u32) void {
        if (self.disabled) return;
        if (self.requests.items.len == 0) {
            census.bump("rehearsal_request_edge_claim_without_scope");
            return;
        }
        const slot = &self.requests.items[self.requests.items.len - 1];
        const claim: ClaimedRequest = switch (slot.*) {
            .none => {
                census.bump("rehearsal_request_edge_claim_without_edge");
                return;
            },
            .checked => |edge| .{ .checked = edge },
            .generated => |request| blk: {
                self.generated_outcomes[@intFromEnum(request.edge.rule)].claimed += 1;
                break :blk .{ .generated = request };
            },
        };
        const existing = self.edges_by_fn.fetchPut(self.allocator, fn_id, claim) catch {
            self.releaseClaim(claim);
            slot.* = .none;
            self.fail();
            return;
        };
        slot.* = .none;
        census.bump("rehearsal_request_edge_claimed");
        // One reserved id is requested once: a second claim would mean two
        // distinct use sites reserved the same specialization body, which is
        // recorded rather than silently overwritten.
        if (existing) |previous| {
            census.bump("rehearsal_request_edge_claim_repeated");
            self.releaseClaim(previous.value);
        }
    }

    /// Copy the innermost ready frame's whole environment chain when it binds ids
    /// in the requesting module, so the edge carries every lexical level a
    /// symbolic actual can resolve under, not only the innermost binding
    /// (reunify.md sections 7.3, 9.1).
    fn captureCallerEnvironment(self: *Rehearsal, module_bytes: [32]u8) ?CapturedEnvironment {
        const frame = self.callerFrameFor(module_bytes) orelse return null;
        census.bump("rehearsal_caller_env_captured");
        if (frame.chain.depth() > 1) census.bump("rehearsal_caller_env_captured_chained");
        const chain = self.copyEnvironmentChain(frame.environment(), frame.chain.depth(), null) orelse return null;
        return .{
            .module_bytes = frame.env_module_bytes,
            .owner_node = frame.owner_node,
            .chain = chain,
            .residual_origin = frame.residual_origin,
        };
    }

    fn releaseEdge(self: *Rehearsal, edge: RequestEdge) void {
        var caller = edge.caller orelse return;
        caller.chain.release(self.allocator);
    }

    fn releaseGeneratedRequest(self: *Rehearsal, request: GeneratedRequest) void {
        var caller = request.caller orelse return;
        caller.chain.release(self.allocator);
    }

    fn releaseScope(self: *Rehearsal, scope: RequestScope) void {
        switch (scope) {
            .none => {},
            .checked => |edge| self.releaseEdge(edge),
            .generated => |request| self.releaseGeneratedRequest(request),
        }
    }

    fn releaseClaim(self: *Rehearsal, claim: ClaimedRequest) void {
        switch (claim) {
            .checked => |edge| self.releaseEdge(edge),
            .generated => |request| self.releaseGeneratedRequest(request),
        }
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
            .chain = EnvironmentChain.none,
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
        frame.chain.release(self.allocator);
    }

    fn fail(self: *Rehearsal) void {
        self.disabled = true;
    }

    /// Copy the outermost `keep` levels of the chain ending at `innermost`, and
    /// append `own` as a further innermost level when one is given. The result
    /// owns its storage and its `parent` links point inside it, so it stays valid
    /// once the environments it was copied from are released.
    fn copyEnvironmentChain(
        self: *Rehearsal,
        innermost: ?*const direct_translate.BindingEnvironment,
        keep: usize,
        own: ?EnvironmentLevel,
    ) ?EnvironmentChain {
        var order: std.ArrayList(*const direct_translate.BindingEnvironment) = .empty;
        defer order.deinit(self.allocator);
        var cursor = innermost;
        while (cursor) |level| : (cursor = level.parent) {
            order.append(self.allocator, level) catch return null;
        }
        if (keep > order.items.len) return null;

        const total = keep + @as(usize, if (own == null) 0 else 1);
        if (total == 0) return EnvironmentChain.none;

        var value_count: usize = 0;
        for (0..keep) |index| {
            const level = order.items[order.items.len - 1 - index];
            value_count += level.bound.len + level.captured.len;
        }
        if (own) |level| value_count += level.bound.len + level.captured.len;

        const levels = self.allocator.alloc(direct_translate.BindingEnvironment, total) catch return null;
        const values = self.allocator.alloc(direct_translate.BoundType, value_count) catch {
            self.allocator.free(levels);
            return null;
        };
        var used: usize = 0;
        for (0..total) |index| {
            const source: EnvironmentLevel = if (index < keep) blk: {
                const level = order.items[order.items.len - 1 - index];
                break :blk .{
                    .scheme = level.scheme,
                    .binders = level.binders,
                    .bound = level.bound,
                    .captured = level.captured,
                };
            } else own.?;
            const bound_start = used;
            @memcpy(values[used..][0..source.bound.len], source.bound);
            used += source.bound.len;
            const captured_start = used;
            @memcpy(values[used..][0..source.captured.len], source.captured);
            used += source.captured.len;
            levels[index] = .{
                .scheme = source.scheme,
                .binders = source.binders,
                .bound = values[bound_start .. bound_start + source.bound.len],
                .captured = values[captured_start .. captured_start + source.captured.len],
                .parent = if (index == 0) null else &levels[index - 1],
            };
        }
        return .{ .levels = levels, .values = values };
    }

    /// Where one callee scheme's checked captured binders land in the caller's
    /// environment chain (reunify.md sections 7.1, 9.1): the dense values for
    /// `scheme.capturedBinders`, and how many levels of the caller's chain the
    /// callee's own level must link to as its lexical parents.
    const CapturedBinding = struct {
        values: []direct_translate.BoundType,
        parent_levels: usize,
    };

    /// Project the caller's environment onto `scheme`'s captured binders. Each
    /// captured `(outer scheme, binder index)` pair is looked up by identity in
    /// the caller's chain; the innermost level any pair names becomes the callee's
    /// lexical parent, so an enclosing binder the callee's body reaches resolves
    /// through the chain instead of materializing as a residual.
    fn bindCaptured(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme: checked.CheckedTypeScheme,
        caller_env: ?*const direct_translate.BindingEnvironment,
    ) ?CapturedBinding {
        const captured = scheme.capturedBinders(defining.view);
        if (captured.len == 0) return .{ .values = &.{}, .parent_levels = 0 };

        var order: std.ArrayList(*const direct_translate.BindingEnvironment) = .empty;
        defer order.deinit(self.allocator);
        var cursor = caller_env;
        while (cursor) |level| : (cursor = level.parent) {
            order.append(self.allocator, level) catch return null;
        }

        const values = self.allocator.alloc(direct_translate.BoundType, captured.len) catch return null;
        var parent_levels: usize = 0;
        for (captured, 0..) |entry, index| {
            census.bump("rehearsal_captured_binder");
            // A residual with no value is the uninhabited materialization the
            // rest of the rehearsal already measures, so an unresolved captured
            // position stays visible as a mismatch instead of as a silent hole.
            values[index] = direct_translate.BoundType.of(self.uninhabitedStandIn() orelse {
                self.allocator.free(values);
                return null;
            });
            const outer_id = entry.outerScheme() orelse {
                census.bump("rehearsal_captured_binder_outer_unattributed");
                continue;
            };
            const outer = defining.view.schemeById(outer_id) orelse {
                census.bump("rehearsal_captured_binder_outer_unresolved");
                continue;
            };
            const outer_binders = outer.generalizedVars(defining.view);
            if (entry.binder_index >= outer_binders.len) {
                census.bump("rehearsal_captured_binder_index_out_of_range");
                continue;
            }
            // The chain runs innermost first, so the first level that names the
            // outer scheme is the innermost instance of it.
            var found: ?usize = null;
            for (order.items, 0..) |level, position| {
                if (level.scheme.scheme != @intFromEnum(outer_id)) continue;
                if (!std.mem.eql(u8, &level.scheme.module_bytes, &defining.module_bytes)) continue;
                found = position;
                break;
            }
            const position = found orelse {
                census.bump("rehearsal_captured_binder_outer_not_active");
                continue;
            };
            const level = order.items[position];
            if (entry.binder_index >= level.bound.len or entry.binder_index >= level.binders.len) {
                census.bump("rehearsal_captured_binder_index_out_of_range");
                continue;
            }
            // The checked pair and the active level must name the SAME checked
            // binder at that index; a disagreement would mean the two binder
            // orderings drifted and the value read would silently mis-bind.
            if (level.binders[entry.binder_index] != outer_binders[entry.binder_index]) {
                census.bump("rehearsal_captured_binder_identity_disagrees");
                continue;
            }
            values[index] = level.bound[entry.binder_index];
            census.bump("rehearsal_captured_binder_bound");
            const levels_to_here = order.items.len - position;
            if (levels_to_here > parent_levels) parent_levels = levels_to_here;
        }
        return .{ .values = values, .parent_levels = parent_levels };
    }

    /// The stored empty tag union, which is what an unresolved checked variable
    /// materializes to everywhere else in this rehearsal.
    fn uninhabitedStandIn(self: *Rehearsal) ?Type.TypeId {
        return self.store.internTagUnion(self.program_names, &.{}) catch null;
    }

    /// Why one specialization's requesting edge supplied no binding. Carried to
    /// the edgeless path so a specialization that ends up with no environment
    /// says which half of the seam it is missing.
    const EdgeSkip = union(enum) {
        /// The request that reserved this specialization named no edge at all
        /// and no active specialization was lowering, which is a root request.
        root_request,
        /// The request came from a compiler-generated edge (reunify.md 9.6),
        /// carrying the declared rule when the generating site named one.
        generated_request: ?GeneratedInstantiationRule,
        /// The edge's use expression names no recorded instantiation site.
        no_site,
        /// The edge's use expression names several sites that disagree.
        site_ambiguous,
        /// The site's callee is defined by a different module than this
        /// specialization's template body reads from.
        defining_module_differs,
        /// Everything else the edge could not supply: an absent module, an
        /// unresolved scheme, a site arity disagreement, or an actual outside
        /// the translatable subset. Each already has its own skip counter.
        edge_unusable,
    };

    /// Resolve one specialization's environment: from the requesting edge's site
    /// when one named it (reunify.md sections 7.2, 9.1), and otherwise from what
    /// the specialization's own template says. Every way the edge fails to
    /// resolve is a named skip class, never an assumption.
    fn resolveEnvironment(self: *Rehearsal, start: SpecializationStart, frame: *Frame) void {
        const skip = self.resolveEnvironmentFromEdge(start, frame) orelse return;
        self.resolveGroundTemplateEnvironment(start, frame, skip);
    }

    /// Resolve one specialization's dense binding from the requesting edge's
    /// site, reporting null when the binding was resolved and otherwise why the
    /// edge supplied none. Every way the edge fails to resolve is a named skip
    /// class, never an assumption.
    fn resolveEnvironmentFromEdge(self: *Rehearsal, start: SpecializationStart, frame: *Frame) ?EdgeSkip {
        const claim = self.takeClaim(start.reserved_fn_id) orelse {
            if (self.frames.items.len == 0) {
                census.bump("rehearsal_skip_root_edge");
                return .root_request;
            }
            census.bump("rehearsal_skip_generated_edge");
            return .{ .generated_request = null };
        };
        const edge = switch (claim) {
            .checked => |checked_edge| checked_edge,
            .generated => |request| return self.resolveEnvironmentFromGeneratedRule(start, frame, request),
        };
        defer self.releaseEdge(edge);
        const caller = self.lookup.cursor(edge.module_bytes) orelse {
            census.bump("rehearsal_skip_caller_module_absent");
            return .edge_unusable;
        };
        // The edge this specialization was requested at is the one whose callee
        // scheme is owned by the definition this template specializes: the use
        // expression alone names an edge per callee it instantiates, and only
        // the owner node picks this one out (reunify.md section 7.2's edge
        // identity).
        const owner_node = templateSchemeOwnerNode(start) orelse {
            census.bump("rehearsal_skip_template_owner_unresolved");
            return .edge_unusable;
        };
        const site = self.siteFor(caller, edge.use_expr, owner_node) catch |err| return switch (err) {
            error.NoSite => .no_site,
            error.SiteAmbiguous => .site_ambiguous,
            error.Unavailable => .edge_unusable,
        };
        const caller_env: ?*const direct_translate.BindingEnvironment = if (edge.caller) |*captured|
            captured.environment()
        else
            null;
        const caller_owner_node = if (edge.caller) |captured|
            captured.owner_node
        else
            checked.checked_residual_disposition_module_body_owner;
        const caller_origin: ResidualOrigin = if (edge.caller) |captured|
            captured.residual_origin
        else
            .unresolved_request_context;
        const scheme_id = site.schemeId() orelse {
            census.bump("rehearsal_skip_scheme_unresolved");
            return .edge_unusable;
        };
        const defining_bytes = site.importedDefiningModule() orelse edge.module_bytes;
        // The binders the specialized body's positions name are ids in the store
        // its template reads from, so a site whose scheme is owned by a
        // different module names none of them: its scheme id belongs to the
        // other store entirely. The site's ACTUALS are still this request's,
        // recorded in the requesting module and translated there, so the
        // specialization binds from its own template's scheme instead — under an
        // exact witness, never by assuming the two binder orders line up.
        if (!std.mem.eql(u8, &start.cursor.module_bytes, &defining_bytes)) {
            census.bump("rehearsal_skip_edge_defining_module_differs");
            if (self.resolveEnvironmentFromForeignSchemeEdge(start, frame, caller, site, edge)) return null;
            return .defining_module_differs;
        }
        census.bump("rehearsal_edge_defining_module_matches_template");
        const defining = self.definingCursor(start, defining_bytes) orelse {
            census.bump("rehearsal_skip_defining_module_absent");
            return .edge_unusable;
        };
        const scheme = defining.view.schemeById(scheme_id) orelse {
            census.bump("rehearsal_skip_scheme_unresolved");
            return .edge_unusable;
        };
        const binders = scheme.generalizedVars(defining.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) {
            census.bump("rehearsal_skip_arity_mismatch");
            return .edge_unusable;
        }

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return .edge_unusable;
        };
        defer self.allocator.free(bound);
        var filled: usize = 0;
        for (actuals) |actual| {
            if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) {
                census.bump("rehearsal_skip_unreached_actual");
                return .edge_unusable;
            }
            const translated = self.translateActual(caller, caller_env, caller_owner_node, actual) orelse return .edge_unusable;
            if (self.carriesResidualMaterialization(translated)) {
                noteResidualOrigin(frame, self.classifyResidualActual(
                    caller,
                    caller_owner_node,
                    actual,
                    caller_env,
                    caller_origin,
                ));
            }
            bound[filled] = direct_translate.BoundType.of(translated);
            filled += 1;
        }

        const captured = self.bindCaptured(defining, scheme, caller_env) orelse {
            self.fail();
            return .edge_unusable;
        };
        defer if (captured.values.len != 0) self.allocator.free(captured.values);
        const scheme_ident = direct_translate.SchemeIdent{
            .module_bytes = defining_bytes,
            .scheme = @intFromEnum(scheme_id),
        };
        frame.chain = self.copyEnvironmentChain(caller_env, captured.parent_levels, .{
            .scheme = scheme_ident,
            .binders = binders,
            .bound = bound,
            .captured = captured.values,
        }) orelse {
            self.fail();
            return .edge_unusable;
        };
        if (captured.parent_levels == 0) {
            census.bump("rehearsal_env_parent_absent");
        } else {
            census.bump("rehearsal_env_parent_linked");
        }

        frame.env_module_bytes = defining_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.env_ready = true;
        census.bump("rehearsal_env_resolved");
        noteEnvironmentScheme(scheme);
        if (binders.len == 0) {
            census.bump("rehearsal_env_resolved_without_binders");
            self.classifyEmptyBinders(defining, scheme, site.importedDefiningModule() != null);
        }

        // The two sides of this specialization's representation interface
        // (reunify.md section 11.1): the callee's scheme root emitted under the
        // binding, and the request context's own emission of the same edge.
        frame.interface_root = self.emitQuietly(defining, frame.environment(), scheme.owner_node, scheme.root);
        frame.request_root = self.emitQuietly(caller, caller_env, caller_owner_node, site.instantiated_root);
        return null;
    }

    /// Resolve one specialization whose request came from a declared
    /// compiler-generated edge (reunify.md section 9.6). The rule names where
    /// its binder values come from; a rule that declares no source leaves the
    /// specialization unresolved rather than inventing a binding from the
    /// concrete callable. Reports why no binding was produced, or null once one
    /// was.
    fn resolveEnvironmentFromGeneratedRule(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        request: GeneratedRequest,
    ) ?EdgeSkip {
        defer self.releaseGeneratedRequest(request);
        const outcome = &self.generated_outcomes[@intFromEnum(request.edge.rule)];
        const named: EdgeSkip = .{ .generated_request = request.edge.rule };
        const scheme_id = start.template_scheme orelse {
            census.bump("rehearsal_skip_generated_edge");
            return named;
        };
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            census.bump("rehearsal_skip_generated_edge");
            return named;
        };
        // A generated edge into a scheme with no binders has exactly one
        // instantiation; the ground path already resolves those exactly.
        if (scheme.gv_len == 0) {
            outcome.ground += 1;
            census.bump("rehearsal_skip_generated_edge");
            return named;
        }
        const declared_source = if (request.edge.rule.declaresBinderSource()) request.edge.source else null;
        const source = declared_source orelse {
            outcome.unbound += 1;
            census.bump("rehearsal_skip_generated_edge");
            census.bump("rehearsal_generated_rule_declared_unbound");
            return named;
        };
        if (self.bindGeneratedRule(start, frame, scheme_id, scheme, source, request.caller, outcome)) {
            return null;
        }
        census.bump("rehearsal_skip_generated_edge");
        return named;
    }

    /// Apply one declared rule's binder mapping and accept the binding only
    /// under the exact structural witness the rule declares: the callee scheme
    /// root emitted under the binding must equal the checked callable the
    /// request names, emitted under the requesting body's own environment
    /// (reunify.md sections 7.5, 9.6). A binding without that witness is
    /// released and the specialization stays unresolved.
    fn bindGeneratedRule(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        scheme_id: checked.CheckedTypeSchemeId,
        scheme: checked.CheckedTypeScheme,
        source: GeneratedSource,
        captured_caller: ?CapturedEnvironment,
        outcome: *GeneratedOutcome,
    ) bool {
        const caller = self.lookup.cursor(source.module_bytes) orelse {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_caller_module_absent");
            return false;
        };
        const binders = scheme.generalizedVars(start.cursor.view);
        // The rule's mapping is over the callee scheme's own binders; a scheme
        // that also captures enclosing binders is outside every declared rule.
        if (scheme.captured_len != 0) {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_scheme_captures");
            return false;
        }
        const arguments = receiverArguments(caller.view, source.receiver) orelse {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_not_named");
            return false;
        };
        if (!receiverSuppliesBinders(arguments, binders)) {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_arity_differs");
            return false;
        }

        const caller_env: ?*const direct_translate.BindingEnvironment = if (captured_caller) |*held|
            held.environment()
        else
            null;
        const caller_owner_node = if (captured_caller) |held|
            held.owner_node
        else
            checked.checked_residual_disposition_module_body_owner;
        const caller_origin: ResidualOrigin = if (captured_caller) |held|
            held.residual_origin
        else
            .unresolved_request_context;

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return false;
        };
        defer self.allocator.free(bound);
        for (arguments, 0..) |argument, index| {
            const translated = self.translateActual(caller, caller_env, caller_owner_node, argument) orelse {
                outcome.receiver_unusable += 1;
                census.bump("rehearsal_generated_rule_argument_untranslatable");
                return false;
            };
            if (self.carriesResidualMaterialization(translated)) {
                noteResidualOrigin(frame, self.classifyResidualActual(
                    caller,
                    caller_owner_node,
                    argument,
                    caller_env,
                    caller_origin,
                ));
            }
            bound[index] = direct_translate.BoundType.of(translated);
        }

        const scheme_ident = direct_translate.SchemeIdent{
            .module_bytes = start.cursor.module_bytes,
            .scheme = @intFromEnum(scheme_id),
        };
        var chain = self.copyEnvironmentChain(null, 0, .{
            .scheme = scheme_ident,
            .binders = binders,
            .bound = bound,
            .captured = &.{},
        }) orelse {
            self.fail();
            return false;
        };
        const declared = self.emitQuietly(start.cursor, chain.innermost(), scheme.owner_node, scheme.root);
        const requested = self.emitQuietly(caller, caller_env, caller_owner_node, source.requested);
        if (!self.generatedWitnessAgrees(declared, requested, outcome)) {
            chain.release(self.allocator);
            return false;
        }

        frame.chain = chain;
        frame.env_module_bytes = start.cursor.module_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.env_ready = true;
        frame.interface_root = declared;
        frame.request_root = requested;
        census.bump("rehearsal_env_resolved");
        census.bump("rehearsal_env_resolved_generated_rule");
        noteEnvironmentScheme(scheme);
        census.bump("rehearsal_env_parent_absent");
        return true;
    }

    /// Whether a rule's binding produced the exact witness that accepts it,
    /// counted per rule. Two rooted recursive graphs entered from different
    /// paths store different digests for one type (reunify.md section 8.3), so
    /// the unfolding decides those.
    fn generatedWitnessAgrees(
        self: *Rehearsal,
        declared: ?Type.TypeId,
        requested: ?Type.TypeId,
        outcome: *GeneratedOutcome,
    ) bool {
        const left = declared orelse {
            outcome.witness_absent += 1;
            census.bump("rehearsal_generated_rule_witness_absent");
            return false;
        };
        const right = requested orelse {
            outcome.witness_absent += 1;
            census.bump("rehearsal_generated_rule_witness_absent");
            return false;
        };
        const left_digest = self.store.typeDigest(self.program_names, left);
        const right_digest = self.store.typeDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            outcome.witness_agrees += 1;
            census.bump("rehearsal_generated_rule_witness_agrees");
            return true;
        }
        const left_unfolded = self.store.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.store.unfoldedDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes)) {
            outcome.witness_agrees += 1;
            census.bump("rehearsal_generated_rule_witness_agrees_under_rerooting");
            return true;
        }
        outcome.witness_differs += 1;
        census.bump("rehearsal_generated_rule_witness_differs");
        return false;
    }

    /// Resolve a specialization the rehearsal saw no requesting edge for. A root
    /// request has no requesting site at all and a compiler-generated request
    /// records none (reunify.md section 9.6), but the specialization still has an
    /// exact environment whenever its own template's scheme is ground: a scheme
    /// with no binders whose root reaches no checked variable has exactly one
    /// instantiation, so the empty binding is the whole binding and no request
    /// could change it. The scheme is named by the scheme id the checked
    /// procedure template carries, qualified by its defining checked module
    /// (reunify.md section 7.1) — owner identity, never a content key or a root
    /// the module's schemes may share; a scheme that does carry generalized
    /// structure stays skipped, because only the edge can say what its binders
    /// took.
    fn resolveGroundTemplateEnvironment(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        skip: EdgeSkip,
    ) void {
        const scheme_id = start.template_scheme orelse {
            census.bump("rehearsal_edgeless_template_scheme_absent");
            return;
        };
        const scheme_raw = @intFromEnum(scheme_id);
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            census.bump("rehearsal_edgeless_scheme_unresolved");
            return;
        };
        if (scheme.gv_len != 0) {
            census.bump("rehearsal_edgeless_scheme_has_binders");
            noteEdgelessWithBinders(start, scheme, skip);
            self.noteUnresolvedDetail(start, scheme, skip);
            return;
        }
        if (scheme.captured_len != 0) {
            census.bump("rehearsal_edgeless_scheme_captures");
            return;
        }
        if (self.schemeRootReachesVariable(start.cursor.view, scheme.root)) {
            census.bump("rehearsal_edgeless_scheme_root_variable");
            return;
        }
        const scheme_ident = direct_translate.SchemeIdent{
            .module_bytes = start.cursor.module_bytes,
            .scheme = scheme_raw,
        };
        frame.chain = self.copyEnvironmentChain(null, 0, .{
            .scheme = scheme_ident,
            .binders = &.{},
            .bound = &.{},
            .captured = &.{},
        }) orelse return self.fail();
        frame.env_module_bytes = start.cursor.module_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = &.{};
        frame.env_ready = true;
        census.bump("rehearsal_env_resolved");
        census.bump("rehearsal_env_resolved_edgeless_ground");
        noteEnvironmentScheme(scheme);
        frame.interface_root = self.emitQuietly(start.cursor, frame.environment(), scheme.owner_node, scheme.root);
    }

    /// Bind a specialization whose requesting site names its callee scheme
    /// through a different module than the one this template's body reads from,
    /// reporting whether the binding was resolved.
    ///
    /// The two modules are two checked outputs of one definition: the site's
    /// `scheme_owner_node` is the defining CIR node the scheme is owned by, and
    /// the template's own scheme names that same owner node. That agreement is
    /// what makes the site's positional actuals a vector over THIS template
    /// scheme's binders — but agreement of owner identity alone does not prove
    /// the two checked outputs ordered their binders identically, so the binding is
    /// only accepted once it produces an exact structural witness: the callee
    /// scheme root emitted under it must equal the site's own instantiated root
    /// emitted in the requesting module (reunify.md section 7.5's substitution
    /// law). A binding without that witness is released and the specialization
    /// stays unresolved.
    fn resolveEnvironmentFromForeignSchemeEdge(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        caller: direct_translate.ModuleCursor,
        site: checked.CheckedInstantiationSite,
        edge: RequestEdge,
    ) bool {
        const scheme_id = start.template_scheme orelse return false;
        const scheme = start.cursor.view.schemeById(scheme_id) orelse return false;
        if (scheme.owner_node != site.scheme_owner_node) {
            census.bump("rehearsal_foreign_scheme_owner_node_differs");
            return false;
        }
        census.bump("rehearsal_foreign_scheme_owner_node_agrees");
        const binders = scheme.generalizedVars(start.cursor.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) {
            census.bump("rehearsal_foreign_scheme_arity_differs");
            return false;
        }

        const caller_env: ?*const direct_translate.BindingEnvironment = if (edge.caller) |*captured|
            captured.environment()
        else
            null;
        const caller_owner_node = if (edge.caller) |captured|
            captured.owner_node
        else
            checked.checked_residual_disposition_module_body_owner;
        const caller_origin: ResidualOrigin = if (edge.caller) |captured|
            captured.residual_origin
        else
            .unresolved_request_context;

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return false;
        };
        defer self.allocator.free(bound);
        for (actuals, 0..) |actual, index| {
            if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) {
                census.bump("rehearsal_skip_unreached_actual");
                return false;
            }
            const translated = self.translateActual(caller, caller_env, caller_owner_node, actual) orelse return false;
            if (self.carriesResidualMaterialization(translated)) {
                noteResidualOrigin(frame, self.classifyResidualActual(
                    caller,
                    caller_owner_node,
                    actual,
                    caller_env,
                    caller_origin,
                ));
            }
            bound[index] = direct_translate.BoundType.of(translated);
        }

        const scheme_ident = direct_translate.SchemeIdent{
            .module_bytes = start.cursor.module_bytes,
            .scheme = @intFromEnum(scheme_id),
        };
        var chain = self.copyEnvironmentChain(null, 0, .{
            .scheme = scheme_ident,
            .binders = binders,
            .bound = bound,
            .captured = &.{},
        }) orelse {
            self.fail();
            return false;
        };
        const declared = self.emitQuietly(start.cursor, chain.innermost(), scheme.owner_node, scheme.root);
        const requested = self.emitQuietly(caller, caller_env, caller_owner_node, site.instantiated_root);
        if (!self.witnessesAgree(declared, requested)) {
            chain.release(self.allocator);
            return false;
        }

        frame.chain = chain;
        frame.env_module_bytes = start.cursor.module_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.env_ready = true;
        frame.interface_root = declared;
        frame.request_root = requested;
        census.bump("rehearsal_env_resolved");
        census.bump("rehearsal_env_resolved_foreign_scheme");
        noteEnvironmentScheme(scheme);
        census.bump("rehearsal_env_parent_absent");
        return true;
    }

    /// Whether a candidate binding produced the exact witness that accepts it:
    /// the callee's scheme root emitted under the binding and the requesting
    /// site's own instantiated root are the same type. Two rooted recursive
    /// graphs entered from different paths store different digests for one type
    /// (reunify.md section 8.3), so the unfolding decides those.
    fn witnessesAgree(self: *Rehearsal, declared: ?Type.TypeId, requested: ?Type.TypeId) bool {
        const left = declared orelse {
            census.bump("rehearsal_foreign_witness_absent");
            return false;
        };
        const right = requested orelse {
            census.bump("rehearsal_foreign_witness_absent");
            return false;
        };
        const left_digest = self.store.typeDigest(self.program_names, left);
        const right_digest = self.store.typeDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            census.bump("rehearsal_foreign_witness_agrees");
            return true;
        }
        const left_unfolded = self.store.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.store.unfoldedDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes)) {
            census.bump("rehearsal_foreign_witness_agrees_under_rerooting");
            return true;
        }
        census.bump("rehearsal_foreign_witness_differs");
        return false;
    }

    fn noteUnresolvedDetail(
        self: *Rehearsal,
        start: SpecializationStart,
        scheme: checked.CheckedTypeScheme,
        skip: EdgeSkip,
    ) void {
        if (comptime !census.enabled) return;
        if (self.unresolved_details.items.len >= max_unresolved_details) return;
        var name_buf: [48]u8 = [_]u8{' '} ** 48;
        const copy = @min(start.template_name.len, name_buf.len);
        @memcpy(name_buf[0..copy], start.template_name[0..copy]);
        const entry: UnresolvedDetail = .{
            .name = name_buf,
            .template_module = start.cursor.module_bytes[0..6].*,
            .binders = scheme.gv_len,
            .skip = skip,
        };
        for (self.unresolved_details.items) |existing| {
            if (std.meta.eql(existing, entry)) return;
        }
        self.unresolved_details.append(self.allocator, entry) catch return;
    }

    /// Name the population that genuinely still needs a binding: a
    /// specialization whose requesting edge supplied none and whose own template
    /// scheme carries binders, so the empty binding would be wrong. It is split
    /// three ways — why the edge supplied nothing, which owner kind the scheme
    /// has, and which target kind the template is — because those three together
    /// say whether the missing binding is a root's requested type (reunify.md
    /// 7.2), a declared generated edge (reunify.md 9.6), or an unindexed
    /// dispatch site.
    fn noteEdgelessWithBinders(
        start: SpecializationStart,
        scheme: checked.CheckedTypeScheme,
        skip: EdgeSkip,
    ) void {
        switch (skip) {
            .root_request => census.bump("rehearsal_edgeless_binders_root_request"),
            .generated_request => census.bump("rehearsal_edgeless_binders_generated_request"),
            .no_site => census.bump("rehearsal_edgeless_binders_no_site"),
            .site_ambiguous => census.bump("rehearsal_edgeless_binders_site_ambiguous"),
            .defining_module_differs => census.bump("rehearsal_edgeless_binders_module_differs"),
            .edge_unusable => census.bump("rehearsal_edgeless_binders_edge_unusable"),
        }
        switch (scheme.owner_kind) {
            .top_level_def => census.bump("rehearsal_edgeless_binders_owner_top_level"),
            .nested_def => census.bump("rehearsal_edgeless_binders_owner_nested"),
            .required_type => census.bump("rehearsal_edgeless_binders_owner_required"),
            .synthetic => census.bump("rehearsal_edgeless_binders_owner_synthetic"),
        }
        switch (start.target_kind) {
            .roc => census.bump("rehearsal_edgeless_binders_target_roc"),
            .hosted => census.bump("rehearsal_edgeless_binders_target_hosted"),
            .intrinsic => census.bump("rehearsal_edgeless_binders_target_intrinsic"),
            .entry => census.bump("rehearsal_edgeless_binders_target_entry"),
            .comptime_only => census.bump("rehearsal_edgeless_binders_target_comptime"),
        }
    }

    /// The cursor the callee scheme's own module reads by. The lowering input
    /// indexes the root module, its imports, and its relation modules; a
    /// specialization whose template body is already being read through
    /// `start.cursor` names that same module directly, which is the same module
    /// identity and therefore the same frozen store.
    fn definingCursor(
        self: *Rehearsal,
        start: SpecializationStart,
        defining_bytes: [32]u8,
    ) ?direct_translate.ModuleCursor {
        if (self.lookup.cursor(defining_bytes)) |cursor| return cursor;
        if (std.mem.eql(u8, &start.cursor.module_bytes, &defining_bytes)) {
            census.bump("rehearsal_defining_module_from_template_cursor");
            return start.cursor;
        }
        return null;
    }

    /// Name why one checked actual translated to a value carrying a residual
    /// materialization, and report the origin that value inherits: the actual
    /// reaches a checked variable this environment does not bind (a scheme
    /// binder of the requesting module, a variable disposed in another body
    /// context, or an undisposed one), it reaches only variables the caller
    /// already bound to a residual value, or it reaches none at all and its
    /// empty row is checked content.
    fn classifyResidualActual(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        owner_node: u32,
        actual: checked.CheckedTypeId,
        caller_env: ?*const direct_translate.BindingEnvironment,
        caller_origin: ResidualOrigin,
    ) ResidualOrigin {
        if (caller_env == null) {
            census.bump("rehearsal_actual_residual_without_caller_env");
            return .unresolved_request_context;
        }
        census.bump("rehearsal_actual_residual_with_caller_env");
        const free = self.firstFreeVariable(caller.view, actual, caller_env) orelse {
            // Every variable the actual reaches is bound, so the residual came
            // in through one of those bindings — or the empty row is checked
            // content the requesting body really names.
            if (caller_origin != .absent) {
                census.bump("rehearsal_actual_residual_inherited");
                return caller_origin;
            }
            census.bump("rehearsal_actual_residual_closed_empty_row");
            return .closed_empty_row;
        };
        census.bump("rehearsal_actual_residual_unbound_here");
        switch (caller.view.payload(actual)) {
            .flex, .rigid => census.bump("rehearsal_actual_residual_bare_variable"),
            else => census.bump("rehearsal_actual_residual_structure"),
        }
        for (caller.view.schemes) |scheme| {
            for (scheme.generalizedVars(caller.view)) |binder| {
                if (binder != free) continue;
                census.bump("rehearsal_actual_residual_is_scheme_binder");
                return .scheme_binder;
            }
        }
        for (caller.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(free)) continue;
            if (disposition.scheme_owner_node == owner_node) {
                census.bump("rehearsal_actual_residual_disposed_here");
                return .disposed_here;
            }
            census.bump("rehearsal_actual_residual_disposed_elsewhere");
            return .disposed_elsewhere;
        }
        census.bump("rehearsal_actual_residual_undisposed");
        return .undisposed;
    }

    /// Whether a stored type carries the empty tag union anywhere, which is what
    /// an undisposed, undefaulted residual variable materializes to. Checking
    /// the whole value rather than its head catches a residual nested inside a
    /// structure, which is where most bound residuals sit.
    fn carriesResidualMaterialization(self: *Rehearsal, root: Type.TypeId) bool {
        var visited = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(Type.TypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return false;
        while (stack.pop()) |ty| {
            const gop = visited.getOrPut(ty) catch return false;
            if (gop.found_existing) continue;
            if (HeadShape.of(&self.store, ty).isEmptyTagUnionHead()) return true;
            var index: u32 = 0;
            while (childAt(&self.store, ty, index)) |child| : (index += 1) {
                stack.append(self.allocator, child) catch return false;
            }
        }
        return false;
    }

    /// Fold one actual's residual origin into the binding's: the first origin a
    /// binding takes on is the one it reports, so a binding names the head of
    /// its own cascade rather than the last binder that repeated it.
    fn noteResidualOrigin(frame: *Frame, origin: ResidualOrigin) void {
        if (frame.residual_origin != .absent) return;
        frame.residual_origin = origin;
    }

    /// Name the owner kind of the callee scheme one resolved environment binds,
    /// and whether that scheme carries any checked captured binder. A scheme with
    /// a top-level owner has no enclosing scheme, so no captured pair could link
    /// its environment to a lexical parent however wide the checked capture is.
    fn noteEnvironmentScheme(scheme: checked.CheckedTypeScheme) void {
        switch (scheme.owner_kind) {
            .top_level_def => census.bump("rehearsal_env_owner_top_level"),
            .nested_def => census.bump("rehearsal_env_owner_nested"),
            .required_type => census.bump("rehearsal_env_owner_required"),
            .synthetic => census.bump("rehearsal_env_owner_synthetic"),
        }
        if (scheme.captured_len == 0) {
            census.bump("rehearsal_env_scheme_captures_absent");
        } else {
            census.bump("rehearsal_env_scheme_captures_present");
        }
    }

    /// Split one empty-binder environment into the classes reunify.md 7.1
    /// distinguishes: a genuinely monomorphic scheme (its root reaches no
    /// residual variable, so an empty vector is the right answer) versus a gap
    /// (the root does reach one), reported alongside the owner kind, whether a
    /// pristine snapshot root came with it, and whether the scheme was read out
    /// of another module's checked data.
    fn classifyEmptyBinders(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme: checked.CheckedTypeScheme,
        imported: bool,
    ) void {
        if (self.schemeRootReachesVariable(defining.view, scheme.root)) {
            census.bump("rehearsal_env_no_binders_root_variable");
        } else {
            census.bump("rehearsal_env_no_binders_root_ground");
        }
        switch (scheme.owner_kind) {
            .top_level_def => census.bump("rehearsal_env_no_binders_owner_top_level"),
            .nested_def => census.bump("rehearsal_env_no_binders_owner_nested"),
            .required_type => census.bump("rehearsal_env_no_binders_owner_required"),
            .synthetic => census.bump("rehearsal_env_no_binders_owner_synthetic"),
        }
        if (scheme.snapshotRoot() == null) {
            census.bump("rehearsal_env_no_binders_snapshot_absent");
        } else {
            census.bump("rehearsal_env_no_binders_snapshot_present");
        }
        if (imported) census.bump("rehearsal_env_no_binders_imported");
    }

    /// Whether a checked scheme root reaches any checked variable payload —
    /// the structure a binder would name. A scheme whose root reaches none is
    /// monomorphic, so an empty binder vector describes it exactly.
    fn schemeRootReachesVariable(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        var visited = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(checked.CheckedTypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return false;
        while (stack.pop()) |ty| {
            const gop = visited.getOrPut(ty) catch return false;
            if (gop.found_existing) continue;
            switch (view.payload(ty)) {
                .flex, .rigid => return true,
                .pending, .err, .empty_record, .empty_tag_union => {},
                .alias => |alias_ty| {
                    stack.append(self.allocator, alias_ty.backing) catch return false;
                    for (alias_ty.args) |arg| stack.append(self.allocator, arg) catch return false;
                },
                .record => |record_ty| {
                    for (record_ty.fields) |field| stack.append(self.allocator, field.ty) catch return false;
                    stack.append(self.allocator, record_ty.ext) catch return false;
                },
                .record_unbound => |fields| {
                    for (fields) |field| stack.append(self.allocator, field.ty) catch return false;
                },
                .tuple => |elems| {
                    for (elems) |elem| stack.append(self.allocator, elem) catch return false;
                },
                .nominal => |nominal_ty| {
                    for (nominal_ty.args) |arg| stack.append(self.allocator, arg) catch return false;
                    for (nominal_ty.padding_field_types) |field| stack.append(self.allocator, field) catch return false;
                },
                .function => |fn_ty| {
                    for (fn_ty.args) |arg| stack.append(self.allocator, arg) catch return false;
                    stack.append(self.allocator, fn_ty.ret) catch return false;
                },
                .tag_union => |tag_ty| {
                    for (tag_ty.tags) |tag| {
                        for (tag.argsSlice(view)) |arg| stack.append(self.allocator, arg) catch return false;
                    }
                    stack.append(self.allocator, tag_ty.ext) catch return false;
                },
            }
        }
        return false;
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
        env: ?*const direct_translate.BindingEnvironment,
        owner_node: u32,
        actual: checked.CheckedTypeId,
    ) ?Type.TypeId {
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(caller, env, owner_node, actual, &reason) catch |err| switch (err) {
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
        if (self.frames.items.len == 0) {
            census.bump("rehearsal_caller_env_no_frame");
            return null;
        }
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) {
            census.bump("rehearsal_caller_env_frame_not_ready");
            return null;
        }
        if (!std.mem.eql(u8, &frame.env_module_bytes, &module_bytes)) {
            census.bump("rehearsal_caller_env_other_module");
            return null;
        }
        return frame;
    }

    /// Why one use expression named no usable instantiation site.
    const SiteError = error{
        /// No recorded site names this use expression.
        NoSite,
        /// Several recorded sites name it and they disagree.
        SiteAmbiguous,
        /// The module's site index could not be built.
        Unavailable,
    };

    fn siteFor(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        use_expr: checked.CheckedExprId,
        scheme_owner_node: u32,
    ) SiteError!checked.CheckedInstantiationSite {
        const index = self.siteIndexFor(caller) orelse return error.Unavailable;
        const key = siteKey(use_expr, scheme_owner_node);
        if (index.ambiguous.contains(key)) {
            census.bump("rehearsal_skip_site_ambiguous");
            return error.SiteAmbiguous;
        }
        const site_index = index.by_edge.get(key) orelse {
            census.bump("rehearsal_skip_no_site");
            if (index.used_exprs.contains(@intFromEnum(use_expr))) {
                census.bump("rehearsal_no_site_use_owned_elsewhere");
            } else {
                census.bump("rehearsal_no_site_use_unrecorded");
            }
            return error.NoSite;
        };
        return caller.view.instantiationSites()[site_index];
    }

    fn siteIndexFor(self: *Rehearsal, caller: direct_translate.ModuleCursor) ?*SiteIndex {
        const gop = self.site_index.getOrPut(self.allocator, caller.module_bytes) catch {
            self.fail();
            return null;
        };
        if (gop.found_existing) return gop.value_ptr;
        gop.value_ptr.* = .{
            .view = caller.view,
            .by_edge = .empty,
            .ambiguous = .empty,
            .used_exprs = .empty,
        };
        const index = gop.value_ptr;
        const sites = caller.view.instantiationSites();
        for (sites, 0..) |site, position| {
            const use_expr = site.useExpr() orelse continue;
            const key = siteKey(use_expr, site.scheme_owner_node);
            index.used_exprs.put(self.allocator, @intFromEnum(use_expr), {}) catch {
                self.fail();
                return null;
            };
            const entry = index.by_edge.getOrPut(self.allocator, key) catch {
                self.fail();
                return null;
            };
            if (entry.found_existing) {
                // Several edges legitimately name one edge identity: a re-checked
                // source edge, and a value use also reached through a shared-use
                // record. They are the same instantiation when they agree on
                // scheme and positional actuals, and only a genuine disagreement
                // makes the identity unusable as an edge name.
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

    /// What the request that reserved `fn_id` claimed, or null when the request
    /// claimed nothing.
    fn takeClaim(self: *Rehearsal, reserved_fn_id: u32) ?ClaimedRequest {
        const found = self.edges_by_fn.fetchRemove(reserved_fn_id) orelse return null;
        return found.value;
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
        const env_ptr: ?*const direct_translate.BindingEnvironment = if (in_env) frame.environment() else null;
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
            // A stored digest encodes a recursive back reference by the position
            // on the visiting stack the walk entered the cycle at, so one rooted
            // graph reached through two entry paths digests two ways (reunify.md
            // section 8.3). The graph roots such a knot wherever unification
            // happened to join two nodes, which differs between call sites of one
            // nominal; the directed emission roots it at the nominal every time.
            // Equal unfoldings say the two are the same type under a different
            // rooting, which is a deliberate difference in the emitted stored
            // form and not a content difference.
            const emitted_unfolded = self.store.unfoldedDigest(self.program_names, emitted);
            const sealed_unfolded = self.program_types.unfoldedDigest(self.program_names, sealed);
            if (std.mem.eql(u8, &emitted_unfolded.bytes, &sealed_unfolded.bytes)) {
                census.bump("rehearsal_type_equal_under_rerooting");
                matched = true;
                continue;
            }
            if (matched) {
                census.bump("rehearsal_type_skip_other_occurrence");
                continue;
            }
            self.recordMismatch(frame, address, emitted, sealed, emitted_digest, sealed_digest);
        }
    }

    fn recordMismatch(
        self: *Rehearsal,
        frame: *const Frame,
        address: CheckedAddress,
        emitted: Type.TypeId,
        sealed: Type.TypeId,
        emitted_digest: names.TypeDigest,
        sealed_digest: names.TypeDigest,
    ) void {
        const representation = self.sealedCarriesRepresentation(sealed) or self.emittedCarriesRepresentation(emitted);
        const difference = firstDifference(&self.store, emitted, self.program_types, sealed, self.program_names, 0);
        // A difference outside the residual-materialization class is a finding of
        // its own, so its detail is always dumped: the bounded budget exists to
        // stop the residual class from filling the file, not to hide the rest.
        var beyond_residual_class = true;
        if (representation) {
            census.bump("rehearsal_type_mismatch_representation");
        } else {
            census.bump("rehearsal_type_mismatch_logical");
            if (difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead()) {
                beyond_residual_class = false;
                census.bump("rehearsal_type_mismatch_unbound_residual");
                if (frame.binders.len == 0) {
                    census.bump("rehearsal_unbound_residual_env_without_binders");
                } else {
                    census.bump("rehearsal_unbound_residual_env_with_binders");
                }
                switch (frame.residual_origin) {
                    .absent => census.bump("rehearsal_unbound_origin_absent"),
                    .unresolved_request_context => census.bump("rehearsal_unbound_origin_unresolved_context"),
                    .scheme_binder => census.bump("rehearsal_unbound_origin_scheme_binder"),
                    .disposed_here => census.bump("rehearsal_unbound_origin_disposed_here"),
                    .disposed_elsewhere => census.bump("rehearsal_unbound_origin_disposed_elsewhere"),
                    .undisposed => census.bump("rehearsal_unbound_origin_undisposed"),
                    .closed_empty_row => census.bump("rehearsal_unbound_origin_closed_empty_row"),
                }
                self.classifyUnboundPosition(frame, address);
            } else if (difference.left.tag != difference.right.tag) {
                census.bump("rehearsal_type_mismatch_head_tag");
            } else if (difference.left.entries != difference.right.entries) {
                census.bump("rehearsal_type_mismatch_row_width");
            } else if (difference.named_field != .not_named and difference.named_field != .equal) {
                census.bump("rehearsal_type_mismatch_named_identity");
            } else if (difference.depth >= max_difference_depth and
                difference.left_recursive and difference.right_recursive)
            {
                census.bump("rehearsal_type_mismatch_recursive_beyond_depth");
            } else {
                census.bump("rehearsal_type_mismatch_unclassified");
            }
        }
        if (!beyond_residual_class and self.details.items.len >= max_mismatch_details) return;
        var prefix: [8]u8 = undefined;
        @memcpy(&prefix, address.module_bytes[0..8]);
        self.details.append(self.allocator, .{
            .module_prefix = prefix,
            .type_id = address.type_id,
            .representation = representation,
            .binder_count = @intCast(frame.binders.len),
            .rehearsal_digest = emitted_digest,
            .graph_digest = sealed_digest,
            .rehearsal_head = HeadShape.of(&self.store, emitted),
            .graph_head = HeadShape.of(self.program_types, sealed),
            .difference = difference,
        }) catch self.fail();
    }

    /// Name the reason one mismatching position emitted a residual
    /// materialization: find the first checked variable it reaches that this
    /// environment does not bind, and report whether that variable is another
    /// checked scheme's binder, carries a residual disposition (and under
    /// which body context), carries none, or is absent entirely.
    fn classifyUnboundPosition(self: *Rehearsal, frame: *const Frame, address: CheckedAddress) void {
        const cursor = self.lookup.cursor(address.module_bytes) orelse return;
        const in_env = std.mem.eql(u8, &address.module_bytes, &frame.env_module_bytes);
        const free = self.firstFreeVariable(
            cursor.view,
            @enumFromInt(address.type_id),
            if (in_env) frame.environment() else null,
        ) orelse {
            census.bump("rehearsal_unbound_no_free_variable");
            return;
        };
        for (cursor.view.schemes) |scheme| {
            for (scheme.generalizedVars(cursor.view)) |binder| {
                if (binder != free) continue;
                census.bump("rehearsal_unbound_other_scheme_binder");
                // Whether that scheme is on this frame's lexical chain says which
                // half is missing: a chain level that does not name the binder
                // means the level's own binding is short, while a scheme that is
                // nowhere on the chain means no checked relation links the two
                // (reunify.md section 7.1's captured set covers a scheme's root).
                var on_chain = false;
                var level = frame.environment();
                while (level) |env| : (level = env.parent) {
                    if (env.scheme.scheme != @intFromEnum(scheme.id)) continue;
                    if (!std.mem.eql(u8, &env.scheme.module_bytes, &address.module_bytes)) continue;
                    on_chain = true;
                }
                if (on_chain) {
                    census.bump("rehearsal_unbound_other_scheme_binder_on_chain");
                } else {
                    census.bump("rehearsal_unbound_other_scheme_binder_off_chain");
                    // Which direction the two schemes sit in: a scheme whose own
                    // captured pairs name this frame's scheme is nested INSIDE the
                    // specialized body, so its binders are bound by its own use
                    // sites through its own binder list (reunify.md section 7.3),
                    // not by any captured pair this frame could carry. The rest
                    // name no checked relation to this frame at all.
                    var inner_of_frame = false;
                    for (scheme.capturedBinders(cursor.view)) |captured| {
                        const outer_id = captured.outerScheme() orelse continue;
                        if (@intFromEnum(outer_id) != frame.scheme.scheme) continue;
                        if (!std.mem.eql(u8, &frame.scheme.module_bytes, &address.module_bytes)) continue;
                        inner_of_frame = true;
                    }
                    if (inner_of_frame) {
                        census.bump("rehearsal_unbound_binder_scheme_inside_frame");
                    } else {
                        census.bump("rehearsal_unbound_binder_scheme_unrelated");
                    }
                }
                return;
            }
        }
        for (cursor.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(free)) continue;
            if (disposition.scheme_owner_node == frame.owner_node) {
                switch (disposition.kind) {
                    .contextual => census.bump("rehearsal_unbound_disposed_contextual"),
                    .uninhabited => census.bump("rehearsal_unbound_disposed_uninhabited"),
                }
                return;
            }
            if (disposition.scheme_owner_node == checked.checked_residual_disposition_module_body_owner) {
                census.bump("rehearsal_unbound_disposed_module_body");
                return;
            }
            census.bump("rehearsal_unbound_disposed_other_owner");
            return;
        }
        census.bump("rehearsal_unbound_undisposed");
    }

    /// The first checked variable reachable from `root` that no level of `env`
    /// binds, in the walk order the translation itself descends.
    fn firstFreeVariable(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
        env: ?*const direct_translate.BindingEnvironment,
    ) ?checked.CheckedTypeId {
        var visited = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(checked.CheckedTypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return null;
        while (stack.pop()) |ty| {
            const gop = visited.getOrPut(ty) catch return null;
            if (gop.found_existing) continue;
            var bound = false;
            var level = env;
            while (level) |scope| : (level = scope.parent) {
                for (scope.binders) |binder| {
                    if (binder == ty) bound = true;
                }
            }
            if (bound) continue;
            switch (view.payload(ty)) {
                .flex, .rigid => return ty,
                .pending, .err, .empty_record, .empty_tag_union => {},
                .alias => |alias_ty| {
                    stack.append(self.allocator, alias_ty.backing) catch return null;
                    for (alias_ty.args) |arg| stack.append(self.allocator, arg) catch return null;
                },
                .record => |record_ty| {
                    for (record_ty.fields) |field| stack.append(self.allocator, field.ty) catch return null;
                    stack.append(self.allocator, record_ty.ext) catch return null;
                },
                .record_unbound => |fields| {
                    for (fields) |field| stack.append(self.allocator, field.ty) catch return null;
                },
                .tuple => |elems| {
                    for (elems) |elem| stack.append(self.allocator, elem) catch return null;
                },
                .nominal => |nominal_ty| {
                    for (nominal_ty.args) |arg| stack.append(self.allocator, arg) catch return null;
                    for (nominal_ty.padding_field_types) |field| stack.append(self.allocator, field) catch return null;
                },
                .function => |fn_ty| {
                    for (fn_ty.args) |arg| stack.append(self.allocator, arg) catch return null;
                    stack.append(self.allocator, fn_ty.ret) catch return null;
                },
                .tag_union => |tag_ty| {
                    for (tag_ty.tags) |tag| {
                        for (tag.argsSlice(view)) |arg| stack.append(self.allocator, arg) catch return null;
                    }
                    stack.append(self.allocator, tag_ty.ext) catch return null;
                },
            }
        }
        return null;
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

    /// Build the representation slot for one emitted occurrence (reunify.md
    /// section 10.2). The slot is created fresh at every position the walk
    /// reaches: a stored type id names a type, not an occurrence (reunify.md
    /// section 8.5), so keying slots by it would pre-join independent occurrences
    /// that interning collapsed to one id and let one occurrence's representation
    /// flow reach another with no value-flow relation between them. Two
    /// occurrences are joined only by an explicit relation; a back reference
    /// inside one occurrence stops at `max_slot_depth`.
    fn slotForEmitted(self: *Rehearsal, ty: Type.TypeId, depth: u32) ?closure.RepresentationSlotId {
        if (depth >= max_slot_depth) return null;
        const token = self.tokenFor(ty) orelse return null;
        const shape = self.shapeFor(ty, token, depth) orelse return null;
        const slot = self.engine.createSlot(token, self.freshProducer(), shape) catch return null;
        self.slots.append(self.allocator, slot) catch return null;
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
        for (self.slots.items) |slot| {
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

    fn dumpUnresolved(self: *Rehearsal) void {
        if (comptime !census.enabled) return;
        if (self.unresolved_details.items.len == 0) return;
        const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(self.allocator);
        for (self.unresolved_details.items) |detail| {
            const line = std.fmt.allocPrint(
                self.allocator,
                "rehearsal_unresolved_detail name={s} module={s} binders={d} skip={s} rule={s}\n",
                .{
                    &detail.name,
                    &std.fmt.bytesToHex(detail.template_module, .lower),
                    detail.binders,
                    @tagName(detail.skip),
                    skipRuleName(detail.skip),
                },
            ) catch return;
            defer self.allocator.free(line);
            text.appendSlice(self.allocator, line) catch return;
        }
        census.appendToFile(raw_path, text.items);
    }

    /// One line per declared generated rule that produced any edge, so a rule
    /// that binds and a rule that stays declared-but-unbound are read apart
    /// (reunify.md section 9.6).
    fn dumpGeneratedRules(self: *Rehearsal) void {
        if (comptime !census.enabled) return;
        const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(self.allocator);
        for (self.generated_outcomes, 0..) |outcome, index| {
            if (outcome.isEmpty()) continue;
            const rule: GeneratedInstantiationRule = @enumFromInt(index);
            const line = std.fmt.allocPrint(
                self.allocator,
                "rehearsal_generated_rule rule={s} claimed={d} ground={d} unbound={d} witness_agrees={d} witness_differs={d} witness_absent={d} receiver_unusable={d}\n",
                .{
                    @tagName(rule),
                    outcome.claimed,
                    outcome.ground,
                    outcome.unbound,
                    outcome.witness_agrees,
                    outcome.witness_differs,
                    outcome.witness_absent,
                    outcome.receiver_unusable,
                },
            ) catch return;
            defer self.allocator.free(line);
            text.appendSlice(self.allocator, line) catch return;
        }
        if (text.items.len == 0) return;
        census.appendToFile(raw_path, text.items);
    }

    fn dumpDetails(self: *Rehearsal) void {
        if (comptime !census.enabled) return;
        self.dumpGeneratedRules();
        self.dumpUnresolved();
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
                "rehearsal_mismatch_detail module={s} checked_ty={d} representation={d} binders={d} rehearsal={s}/{s}:{d}/{d} graph={s}/{s}:{d}/{d} differs_at_depth={d} {s}:{d}/{d}vs{s}:{d}/{d} named_field={s} recursive={d}/{d}\n",
                .{
                    &module_hex,
                    detail.type_id,
                    @intFromBool(detail.representation),
                    detail.binder_count,
                    &emitted_hex,
                    @tagName(detail.rehearsal_head.tag),
                    detail.rehearsal_head.children,
                    detail.rehearsal_head.entries,
                    &graph_hex,
                    @tagName(detail.graph_head.tag),
                    detail.graph_head.children,
                    detail.graph_head.entries,
                    detail.difference.depth,
                    @tagName(detail.difference.left.tag),
                    detail.difference.left.children,
                    detail.difference.left.entries,
                    @tagName(detail.difference.right.tag),
                    detail.difference.right.children,
                    detail.difference.right.entries,
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

/// How many alias layers a declared receiver is read through before it must be
/// a named type. Aliases nest a handful deep at most; this only bounds input.
const max_receiver_alias_depth: u32 = 8;

/// The declared receiver's positional arguments, read alias-transparently, or
/// null when the receiver is not a named type at all. This is the only shape a
/// declared generated rule's binder mapping reads: positions of the nominal the
/// generating site dispatched on, never a match against the concrete callable
/// (reunify.md sections 9.5, 9.6).
fn receiverArguments(
    view: checked.CheckedTypeStoreView,
    receiver: checked.CheckedTypeId,
) ?[]const checked.CheckedTypeId {
    var ty = receiver;
    var depth: u32 = 0;
    while (depth < max_receiver_alias_depth) : (depth += 1) {
        switch (view.payload(ty)) {
            .nominal => |nominal| return nominal.args,
            .alias => |alias| ty = alias.backing,
            else => return null,
        }
    }
    return null;
}

/// Whether a declared receiver's positional arguments can supply a callee
/// scheme's binders. Every declared mapping is positional and total, so the two
/// lists must have the same length; a receiver that is shorter or longer names a
/// different generator than the rule declares and binds nothing.
fn receiverSuppliesBinders(
    arguments: []const checked.CheckedTypeId,
    binders: []const checked.CheckedTypeId,
) bool {
    return arguments.len == binders.len;
}

/// The declared rule a skip names, or `none` for a skip that names no rule.
fn skipRuleName(skip: Rehearsal.EdgeSkip) []const u8 {
    return switch (skip) {
        .generated_request => |rule| if (rule) |named| @tagName(named) else "none",
        else => "none",
    };
}

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

/// A minimal hand-built checked type store view, enough to read the receiver
/// shapes a declared generated rule's binder mapping accepts and rejects.
const ReceiverFixture = struct {
    allocator: Allocator,
    source_names: names.NameStore,
    payloads: std.ArrayList(checked.StoredCheckedTypePayload),
    type_id_pool: std.ArrayList(checked.CheckedTypeId),
    module_hash: [32]u8,

    fn init(allocator: Allocator) ReceiverFixture {
        return .{
            .allocator = allocator,
            .source_names = names.NameStore.init(allocator),
            .payloads = .empty,
            .type_id_pool = .empty,
            .module_hash = [_]u8{9} ** 32,
        };
    }

    fn deinit(self: *ReceiverFixture) void {
        self.type_id_pool.deinit(self.allocator);
        self.payloads.deinit(self.allocator);
        self.source_names.deinit();
    }

    fn add(self: *ReceiverFixture, payload: checked.StoredCheckedTypePayload) Allocator.Error!checked.CheckedTypeId {
        const id: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.payloads.items.len)));
        try self.payloads.append(self.allocator, payload);
        return id;
    }

    fn addFlex(self: *ReceiverFixture) Allocator.Error!checked.CheckedTypeId {
        return try self.add(.{ .flex = .{} });
    }

    /// A named type with `args` positional arguments, which is the only receiver
    /// shape any declared rule's mapping reads.
    fn addNamed(
        self: *ReceiverFixture,
        name_text: []const u8,
        args: []const checked.CheckedTypeId,
    ) Allocator.Error!checked.CheckedTypeId {
        const name = try self.source_names.internTypeName(name_text);
        const module = try self.source_names.internModuleIdentity(&self.module_hash);
        const start: u32 = @intCast(self.type_id_pool.items.len);
        try self.type_id_pool.appendSlice(self.allocator, args);
        return try self.add(.{ .nominal = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = self.module_hash },
            .is_opaque = false,
            .representation = .opaque_without_backing,
            .args = .{ .start = start, .len = @intCast(args.len) },
        } });
    }

    fn addAlias(
        self: *ReceiverFixture,
        name_text: []const u8,
        backing: checked.CheckedTypeId,
    ) Allocator.Error!checked.CheckedTypeId {
        const name = try self.source_names.internTypeName(name_text);
        const module = try self.source_names.internModuleIdentity(&self.module_hash);
        return try self.add(.{ .alias = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = self.module_hash },
            .backing = backing,
        } });
    }

    fn addFunction(
        self: *ReceiverFixture,
        args: []const checked.CheckedTypeId,
        ret: checked.CheckedTypeId,
    ) Allocator.Error!checked.CheckedTypeId {
        const start: u32 = @intCast(self.type_id_pool.items.len);
        try self.type_id_pool.appendSlice(self.allocator, args);
        return try self.add(.{ .function = .{
            .kind = .pure,
            .args = .{ .start = start, .len = @intCast(args.len) },
            .ret = ret,
        } });
    }

    fn view(self: *ReceiverFixture) checked.CheckedTypeStoreView {
        return .{
            .stored_payloads = self.payloads.items,
            .type_id_pool = self.type_id_pool.items,
        };
    }
};

test "the iterator dispatch rule accepts a named receiver and reads its arguments in order" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addFlex();
    const other = try fixture.addFlex();
    // `Builtin.List.iter` is `List(item) -> Iter(item)`: one binder, and the
    // dispatcher's single argument is exactly that binder's value.
    const list = try fixture.addNamed("List", &.{item});
    const arguments = receiverArguments(fixture.view(), list) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(usize, 1), arguments.len);
    try testing.expectEqual(item, arguments[0]);
    try testing.expect(receiverSuppliesBinders(arguments, &.{item}));

    // A two-argument receiver reads both, in declaration order.
    const dict = try fixture.addNamed("Dict", &.{ item, other });
    const pair = receiverArguments(fixture.view(), dict) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(usize, 2), pair.len);
    try testing.expectEqual(item, pair[0]);
    try testing.expectEqual(other, pair[1]);
}

test "the iterator dispatch rule reads its receiver alias-transparently" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addFlex();
    const iterator = try fixture.addNamed("Iter", &.{item});
    const aliased = try fixture.addAlias("Stepper", iterator);
    const twice = try fixture.addAlias("Steps", aliased);

    const arguments = receiverArguments(fixture.view(), twice) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(usize, 1), arguments.len);
    try testing.expectEqual(item, arguments[0]);
}

test "a receiver that is not a named type binds nothing" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addFlex();
    const bare = try fixture.addFlex();
    const function = try fixture.addFunction(&.{item}, item);

    try testing.expect(receiverArguments(fixture.view(), bare) == null);
    try testing.expect(receiverArguments(fixture.view(), function) == null);
}

test "a receiver whose argument count differs from the callee's binders binds nothing" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const key = try fixture.addFlex();
    const value = try fixture.addFlex();
    const dict = try fixture.addNamed("Dict", &.{ key, value });
    const arguments = receiverArguments(fixture.view(), dict) orelse return error.TestUnexpectedResult;

    // A one-binder callee cannot take a two-argument receiver's positions, and a
    // zero-argument receiver cannot supply a one-binder callee — which is
    // exactly why the encoding-format rules stay declared-but-unbound.
    try testing.expect(!receiverSuppliesBinders(arguments, &.{key}));
    const format = try fixture.addNamed("JsonEncoding", &.{});
    const none = receiverArguments(fixture.view(), format) orelse return error.TestUnexpectedResult;
    try testing.expect(!receiverSuppliesBinders(none, &.{key}));
    try testing.expect(receiverSuppliesBinders(none, &.{}));
}

test "only the declared rules that carry a checked receiver bind from one" {
    // The declared-but-unbound inventory (reunify.md section 9.6): each of these
    // names its missing datum on its enum member and in design.md, and binds
    // nothing until that datum reaches its generating site.
    const unbound = [_]GeneratedInstantiationRule{
        .inspect_component,
        .structural_derivation_component,
        .pattern_literal_equality,
        .set_literal_helper,
        .dict_literal_helper,
        .json_parse_helper,
        .json_encode_helper,
        .json_record_field_name,
        .json_invalid_value,
    };
    for (unbound) |rule| try testing.expect(!rule.declaresBinderSource());
    try testing.expect(GeneratedInstantiationRule.iterator_dispatch_receiver.declaresBinderSource());

    // Every declared rule is in exactly one of the two lists, so a rule added
    // later cannot slip in without declaring which it is.
    try testing.expectEqual(generated_rule_count, unbound.len + 1);
}

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

test "an edge identity key separates one use expression's callees" {
    const use: checked.CheckedExprId = @enumFromInt(7);
    const other_use: checked.CheckedExprId = @enumFromInt(8);

    // One use expression instantiating two callees is two edge identities, and
    // the same callee at two uses is two more: no pair collides.
    try testing.expect(siteKey(use, 3) != siteKey(use, 4));
    try testing.expect(siteKey(use, 3) != siteKey(other_use, 3));
    try testing.expectEqual(siteKey(use, 3), siteKey(use, 3));

    // The owner node occupies the low half, so an owner node large enough to
    // look like another expression's key still cannot alias one.
    try testing.expect(siteKey(use, std.math.maxInt(u32)) != siteKey(other_use, 0));
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
