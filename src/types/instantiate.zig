//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
const types_store = @import("store.zig");
const types_mod = @import("types.zig");

const TypesStore = types_store.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const InterpolationPartMetadata = types_mod.InterpolationPartMetadata;
const WhereMethodMarkerContract = types_mod.WhereMethodMarkerContract;
const WhereMethodMarkerBasis = types_mod.WhereMethodMarkerBasis;
const WhereMethodMarkerPathStep = types_mod.WhereMethodMarkerPathStep;
const WhereMethodMarkerMetadata = types_mod.WhereMethodMarkerMetadata;
const Rigid = types_mod.Rigid;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const NominalType = types_mod.NominalType;
const Tuple = types_mod.Tuple;
const Rank = types_mod.Rank;
const Polarity = types_mod.Polarity;
const Ident = base.Ident;

/// The explicit declaration-backed opening operation (issue #9983): make a
/// fresh copy of `decl`'s backing template with the application's actual
/// `args` substituted for the declaration's formals, positionally.
///
/// `var_map` is caller-provided scratch; it is cleared, seeded with
/// (resolved formal root -> actual arg), and afterwards holds every mapping
/// the instantiation created. Callers own follow-up bookkeeping for the
/// freshly minted vars (regions, rank pools), exactly as with any other
/// instantiation; freshly minted vars are those `var_map` values not equal to
/// a seeded arg.
///
/// The declaration must be valid and its arity must match `args`—callers
/// check `NominalDecl.isValid` (and poison to err) before opening.
pub fn instantiateNominalBacking(
    store: *TypesStore,
    idents: *const base.Ident.Store,
    var_map: *std.AutoHashMap(Var, Var),
    decl: types_mod.NominalDecl,
    args: []const Var,
    current_rank: Rank,
) std.mem.Allocator.Error!Var {
    const formals = store.sliceVars(decl.formals);
    std.debug.assert(formals.len == args.len);

    // Formals substitute both by variable root AND by rigid name. The name
    // route matters for associated type references inside the template: an
    // associated alias/nominal instance embedded there can carry rigids that
    // resolve to different roots than the declaration's formal vars while
    // still NAMING the same formals (the annotation-application path has
    // always rebound such rigids by name, and the opening operation must
    // agree with it).
    var_map.clearRetainingCapacity();
    var rigid_subs = std.AutoHashMapUnmanaged(Ident.Idx, Var){};
    defer rigid_subs.deinit(store.gpa);
    for (formals, args) |formal, arg| {
        const formal_resolved = store.resolveVar(formal);
        try var_map.put(formal_resolved.var_, arg);
        // A malformed header arg (underscore/malformed anno) is err, not
        // rigid; the template cannot reference it by name.
        switch (formal_resolved.desc.content) {
            .rigid => |rigid| try rigid_subs.put(store.gpa, rigid.name, arg),
            .flex, .alias, .field_presence, .structure, .err => {},
        }
    }

    var instantiator = Instantiator{
        .store = store,
        .idents = idents,
        .var_map = var_map,
        .current_rank = current_rank,
        // Rigids naming a formal take that formal's arg; any other rigid
        // (impossible in a well-formed template) stays rigid rather than
        // silently flexing.
        .rigid_behavior = .{ .substitute_rigids_fresh = &rigid_subs },
    };
    return try instantiator.instantiateVar(decl.backing);
}

/// Reusable heap buffers backing `Instantiator`'s explicit worklist. Owned by
/// the `TypesStore` so every instantiation against a store reuses the same
/// capacity. Between top-level instantiation calls every list is back at its
/// entry length; the buffers carry no meaning across calls.
pub const Scratch = struct {
    frames: std.ArrayListUnmanaged(Frame) = .empty,
    value_stack: std.ArrayListUnmanaged(Var) = .empty,
    pending_tags: std.ArrayListUnmanaged(Tag) = .empty,
    pending_fields: std.ArrayListUnmanaged(RecordField) = .empty,
    pending_constraints: std.ArrayListUnmanaged(StaticDispatchConstraint) = .empty,
    pending_parts: std.ArrayListUnmanaged(InterpolationPartMetadata) = .empty,
    pending_marker_contracts: std.ArrayListUnmanaged(WhereMethodMarkerContract) = .empty,
    pending_marker_bases: std.ArrayListUnmanaged(WhereMethodMarkerBasis) = .empty,
    pending_marker_paths: std.ArrayListUnmanaged(WhereMethodMarkerPathStep) = .empty,

    pub fn deinit(self: *Scratch, gpa: std.mem.Allocator) void {
        self.frames.deinit(gpa);
        self.value_stack.deinit(gpa);
        self.pending_tags.deinit(gpa);
        self.pending_fields.deinit(gpa);
        self.pending_constraints.deinit(gpa);
        self.pending_parts.deinit(gpa);
        self.pending_marker_contracts.deinit(gpa);
        self.pending_marker_bases.deinit(gpa);
        self.pending_marker_paths.deinit(gpa);
    }
};

/// One exact copy-time occurrence and its canonical resolved pair in an
/// instantiation traversal. This sink is separate from `var_map`: the semantic
/// map intentionally omits shared vars, while a lineage certificate must cover
/// every requested source occurrence, including redirects and identity edges.
pub const ProofPair = extern struct {
    raw_source_var: u32,
    raw_destination_var: u32,
    canonical_source_var: u32,
    canonical_destination_var: u32,
};

/// Exact source/destination pool indexes for a constraint copied as part of
/// one Instantiator traversal. This is a producer sink, not a structural
/// reconstruction: `stepFlexLike` records the pair at the same append that
/// creates the destination range. A shared-var cut terminates before the
/// source descriptor's constraint edges, so it emits no constraint pair.
pub const ProofConstraintPair = extern struct {
    source_constraint_index: u32,
    destination_constraint_index: u32,
};

/// Exact caller-owned destinations selected by rigid-substitution cuts during
/// one Instantiator traversal. The substitution branch is the producer of
/// this transient relation; checker bookkeeping consumes it to distinguish a
/// reused argument from a variable minted by the traversal without replaying
/// the branch decision from the completed type graph.
pub const PreexistingRigidSubstitutions = std.AutoHashMapUnmanaged(Var, Var);

/// Finite source-descriptor edge kinds emitted by the instantiation walk.
/// These tags deliberately mirror `ModuleEnv.WhereMarkerCopyWitness.EdgeKind`
/// without importing canonicalization into the type package; the checked
/// module producer translates them exhaustively by tag name.
pub const ProofWitnessEdgeKind = enum(u32) {
    root_copy_action,
    static_dispatch_function,
    interpolation_part,
    interpolation_item,
    alias_backing,
    alias_argument,
    tuple_element,
    nominal_argument,
    function_argument,
    function_return,
    function_effect_dependency,
    record_field_type,
    record_field_presence,
    record_extension,
    record_unbound_field_type,
    record_unbound_field_presence,
    tag_payload,
    tag_extension,
    scheme_requirement_receiver,
    scheme_requirement_function,
};

/// Exact action selected for one requested child at copy time. The raw-id
/// cuts are intentionally separate: final unification may otherwise erase
/// the distinction between sharing, freshening, substitution, and polarity.
pub const ProofWitnessAction = enum(u32) {
    traverse,
    local_raw_identity_share_cut,
    rigid_fresh_flex_cut,
    rigid_fresh_rigid_cut,
    exact_annotation_substitution_cut,
    polarity_open_cut,
    polarity_close_cut,
    polarity_preserve_cut,
    binding_codec_reuse_cut,
    platform_preseed_cut,
    requirement_component_ingress,
    flex_fresh_flex_copy,
    requirement_component_fresh_flex_copy,
};

pub const ProofWitnessAuxiliaryOriginKind = enum(u32) {
    none,
    scheme_use,
    scheme_use_pair,
    binding_scheme_codec_requirement,
    platform_substitution,
    binding_copy_pair,
    predeclared_annotation_event,
    annotation_substitution,
};

/// One producer-time witness. Endpoints name the exact raw copy occurrences;
/// the checked module producer binds them to immutable occurrence rows. A
/// constraint auxiliary initially carries the exact source constraint index
/// and is likewise rewritten to the owning step's constraint-pair offset.
pub const ProofWitness = struct {
    parent_raw_source_var: u32,
    parent_raw_destination_var: u32,
    child_raw_source_var: u32,
    child_raw_destination_var: u32,
    edge_kind: ProofWitnessEdgeKind,
    edge_index: u32 = 0,
    edge_name: u32 = 0,
    edge_origin_module: u32 = 0,
    edge_source_decl: u32 = 0,
    /// Exact source constraint index until checked-module output rewrites it
    /// to the owning step's constraint-pair offset.
    constraint_source_index: u32 = std.math.maxInt(u32),
    action: ProofWitnessAction = .traverse,
    auxiliary_origin_kind: ProofWitnessAuxiliaryOriginKind = .none,
    auxiliary_origin_index: u32 = 0,
    raw_source_var: u32 = std.math.maxInt(u32),
    raw_destination_var: u32 = std.math.maxInt(u32),
};

/// Parent occurrence plus the finite source-side edge being requested. This
/// is worklist state only; `recordProofWitness` turns it into a `ProofWitness`
/// as soon as `requestVar` selects the exact destination and copy action.
const ProofEdge = struct {
    parent_raw_source_var: Var,
    parent_raw_destination_var: Var,
    edge_kind: ProofWitnessEdgeKind,
    edge_index: u32 = 0,
    edge_name: u32 = 0,
    edge_origin_module: u32 = 0,
    edge_source_decl: u32 = 0,
    constraint_source_index: u32 = std.math.maxInt(u32),
    action_override: ?ProofWitnessAction = null,
};

pub const ProofRootPair = struct {
    /// Exact raw parent occurrence for detached requirement ingress.
    source_var: Var,
    /// Exact raw destination occurrence for detached requirement ingress.
    destination_var: Var,
};

/// Exact raw occurrence and action selected by one outer instantiation root
/// request. Unlike `ProofRootPair`, this is a one-shot producer result: it
/// distinguishes the caller's requested var from the resolved occurrence at
/// which a rank/leaf share actually terminates.
pub const ProofRootSelection = struct {
    source_var: Var,
    destination_var: Var,
    action: ProofWitnessAction,
};

/// One suspended copy step on the explicit instantiation worklist. Every
/// frame owns a freshly minted placeholder var (already registered in
/// `var_map`, so cycles in the source graph resolve to it) and fills that
/// placeholder's descriptor once all of its child copies are on the value
/// stack.
const Frame = union(enum) {
    flex_like: FlexLikeFrame,
    alias: AliasFrame,
    tuple: TupleFrame,
    nominal: NominalFrame,
    func: FuncFrame,
    record: RecordFrame,
    record_unbound: RecordUnboundFrame,
    tag_union: TagUnionFrame,
};

/// State shared by every frame: the immutable raw parent occurrence, the raw
/// placeholder occurrence to fill, and the descriptor flag carried over from
/// the resolved source var.
const FillCommon = struct {
    raw_source_var: Var,
    raw_destination_var: Var,
    empty_tag_union_is_default: bool,
};

/// Copies a flex var, or a rigid var that keeps a fresh identity, by copying
/// its static-dispatch constraint list one constraint at a time.
const FlexLikeFrame = struct {
    common: FillCommon,
    result: enum { flex, rigid },
    name: ?Ident.Idx,
    /// Raw base index of the source constraint run in the store.
    cons_start: u32,
    cons_len: u32,
    cons_idx: u32 = 0,
    /// Base of this frame's collected constraints in `Scratch.pending_constraints`.
    cons_base: u32,
    fresh_fn_var: Var = undefined,
    /// Base of the current constraint's collected interpolation parts in
    /// `Scratch.pending_parts`.
    parts_base: u32 = 0,
    part_idx: u32 = 0,
    stage: Stage = .dispatch_fn,

    const Stage = enum {
        dispatch_fn,
        await_fn,
        dispatch_part_or_item,
        await_part,
        await_item,
    };
};

const AliasFrame = struct {
    common: FillCommon,
    alias: Alias,
    /// Raw base index of the alias's arg run (backing var excluded).
    args_start: u32,
    args_count: u32,
    /// Base of this frame's child results in `Scratch.value_stack`.
    vars_base: u32,
};

const TupleFrame = struct {
    common: FillCommon,
    elems_start: u32,
    elems_count: u32,
    vars_base: u32,
};

const NominalFrame = struct {
    common: FillCommon,
    nominal: NominalType,
    args_start: u32,
    args_count: u32,
    vars_base: u32,
};

const FuncFrame = struct {
    common: FillCommon,
    func: Func,
    kind: enum { pure, effectful, unbound },
    vars_base: u32,
    /// The polarity surrounding this function. Argument positions negate it;
    /// the return (and effect-dep) positions restore it. Re-asserted before
    /// every child request so suspension cannot leave a stale value.
    saved_polarity: Polarity,
};

/// Source runs are held as whole ranges, never as an unpacked start index:
/// `SafeRange.empty()` leaves `start` undefined, so `start` may only be read
/// under a `count` guard, which the step functions below apply.
const RecordFrame = struct {
    common: FillCommon,
    source_fields: RecordField.SafeMultiList.Range,
    ext: Var,
    vars_base: u32,
    field_idx: u32 = 0,
    field_axis: enum { type_var, presence_var } = .type_var,
    fields_range: RecordField.SafeMultiList.Range = undefined,
    stage: enum { fields, await_ext } = .fields,
};

const RecordUnboundFrame = struct {
    common: FillCommon,
    source_fields: RecordField.SafeMultiList.Range,
    vars_base: u32,
    field_idx: u32 = 0,
    field_axis: enum { type_var, presence_var } = .type_var,
};

const TagUnionFrame = struct {
    common: FillCommon,
    source_tags: Tag.SafeMultiList.Range,
    ext: Var,
    tag_idx: u32 = 0,
    /// Base of the current tag's copied payload vars in `Scratch.value_stack`.
    vars_base: u32,
    /// Base of this frame's collected tags in `Scratch.pending_tags`.
    tags_base: u32,
    tags_range: Tag.SafeMultiList.Range = undefined,
    stage: enum { tags, await_ext } = .tags,
};

/// Type to manage instantiation.
///
/// Entry point is `instantiateVar`
///
/// The graph copy runs on an explicit heap worklist (`TypesStore`'s
/// `Scratch`), so copy depth is bounded only by available heap memory, never
/// by the native stack. Cycles in the source graph terminate through
/// `var_map`: every frame registers its placeholder before any child copy
/// starts, so a child that re-reaches the frame's source var resolves to the
/// placeholder instead of recursing.
///
/// This type does not own any of it's fields – it's a convenience wrapper to
/// making threading it's field through all the copy steps easier
pub const Instantiator = struct {
    // not owned
    store: *TypesStore,
    idents: *const base.Ident.Store,
    var_map: *std.AutoHashMap(Var, Var),

    /// Optional complete traversal relation. Callers that may instantiate a
    /// marker-bearing graph provide this and publish it under a finite durable
    /// use anchor; ordinary callers leave it null and must be marker-free.
    proof_pairs: ?*std.ArrayListUnmanaged(ProofPair) = null,
    proof_constraint_pairs: ?*std.ArrayListUnmanaged(ProofConstraintPair) = null,
    proof_witnesses: ?*std.ArrayListUnmanaged(ProofWitness) = null,
    /// Optional one-shot result for the initial edge-null proof request. The
    /// caller supplies an empty slot and consumes it before this Instantiator
    /// or its proof scratch is reused. Edge-bearing child/detached requests do
    /// not touch it.
    proof_root_selection: ?*?ProofRootSelection = null,
    /// Optional transient producer sink for exact rigid-substitution cuts.
    /// The key is the canonical source used by `var_map`; the value is the
    /// caller-owned destination selected at that cut.
    preexisting_rigid_substitutions: ?*PreexistingRigidSubstitutions = null,
    /// Exact annotation receiver/argument ordinal for each rigid name accepted
    /// by `.substitute_rigids`. Required whenever a proof-carrying child takes
    /// that cut; the substitution map alone is not durable authority.
    rigid_substitution_origins: ?*const std.AutoHashMapUnmanaged(Ident.Idx, u32) = null,
    /// Predicted ModuleEnv copy-step index owned by the caller. Marker-bearing
    /// constraints may be copied only when this is present; every destination
    /// marker then receives one immediate-source basis naming this step.
    where_marker_copy_step: ?u32 = null,
    where_marker_carried: ?*bool = null,

    current_rank: Rank,
    rigid_behavior: RigidBehavior,
    rank_behavior: RankBehavior = .respect_rank,
    /// A rank-1 scheme can contain quantified leaves below monomorphic
    /// structural nodes. While instantiating such a scheme, copy that complete
    /// structural spine so the walk reaches every generalized descendant;
    /// monomorphic flex/rigid leaves remain shared.
    copy_scheme_structure: bool = false,
    /// Share every leaf (flex, rigid, field presence, error) whatever its
    /// rank, copying only structure and resolving polarity markers. This is
    /// the shape of a where-method signature's per-use instantiation: the
    /// signature's other variables belong to the enclosing scheme (live
    /// during the body check, generalized at a later requirement re-check)
    /// and must stay the same variables; only the deferred rows are fresh.
    share_leaves: bool = false,

    /// The `Ident.Idx` of `types.polarity_var_text` in `idents`, when the
    /// caller wants polarity-deferred tag union extensions recognized. Rigids
    /// with this name never reach `rigid_behavior`; they are resolved per
    /// `polarity_var_behavior`. When null, polarity vars are treated as
    /// ordinary rigids (only sound for stores that cannot contain them).
    polarity_var_ident: ?Ident.Idx = null,
    /// The `Ident.Idx` of the anonymous open extension rigid (`#others`, what
    /// a written `..` produces in an input position). Exempt from
    /// `.substitute_rigids`' unknown-rigid assertion: an anonymous extension
    /// is never a declaration parameter, so it is never in a substitution map
    /// and copies as a fresh rigid there.
    anonymous_ext_ident: ?Ident.Idx = null,
    /// When set, every polarity var this instantiation resolves OPEN (a fresh
    /// flex ext) is appended here, so the caller can record it for the
    /// post-body audit of implicitly opened rows (`Check.auditImplicitOpenExts`).
    /// An entry learns the tags of the union it extends when that union's
    /// copy is finished (`OpenedMarkerExt.listed_tags`).
    opened_marker_exts: ?*std.ArrayListUnmanaged(OpenedMarkerExt) = null,
    /// How to resolve polarity vars (see `PolarityVarBehavior`). `.close`
    /// reproduces the written (closed) row and is the safe default.
    polarity_var_behavior: PolarityVarBehavior = .close,
    /// The polarity of the position currently being instantiated. Starts at
    /// the polarity of the instantiation root (callers using
    /// `.resolve_by_polarity` set it) and is negated for function argument
    /// positions as the walk descends — each func frame saves the
    /// surrounding polarity and re-asserts the stage-appropriate value
    /// before every child it requests.
    current_polarity: Polarity = .pos,

    /// Controls whether to respect rank when deciding what to instantiate
    pub const RankBehavior = enum {
        /// Only instantiate generalized types (type checker semantics)
        respect_rank,
        /// Instantiate all types regardless of rank (runtime semantics)
        ignore_rank,
    };

    /// The mode to use when instantiating rigids
    pub const RigidBehavior = union(enum) {
        /// In this mode, all rigids are instantiated as new flex vars
        /// Note that the the rigid var structure will be preserved.
        /// E.g. `a -> a`, `a` will reference the same new rigid var
        fresh_flex,

        /// In this mode, all rigids are instantiated as new rigid variables
        /// Note that the the rigid var structure will be preserved.
        /// E.g. `a -> a`, `a` will reference the same new flex var
        fresh_rigid,

        /// In this mode, all rigids  we be substituted with values in the provided map.
        /// If a rigid var is not in the map, then that variable will be set to
        /// `.err` & in debug mode it will error
        substitute_rigids: *std.AutoHashMapUnmanaged(Ident.Idx, Var),

        /// In this mode, rigids present in the provided map are substituted,
        /// and any other rigids are instantiated as fresh rigid variables.
        substitute_rigids_fresh: *std.AutoHashMapUnmanaged(Ident.Idx, Var),
    };

    /// How to instantiate polarity vars: the marker rigids (named
    /// `types.polarity_var_text`) that alias declarations store as the ext of
    /// extensionless tag unions to defer the open-vs-closed decision to the
    /// use site (see the doc comment on `types.polarity_var_text`).
    pub const PolarityVarBehavior = enum {
        /// Resolve every polarity var to a closed (`[]`) extension: the
        /// written meaning of an extensionless tag union, and the behavior
        /// for positions with no meaningful polarity (eg nominal declaration
        /// bodies, numeric suffix targets).
        close,
        /// Copy each polarity var as a fresh rigid with the same name, keeping
        /// the decision deferred. Used when the copy is itself a declaration
        /// template (eg an orphan scheme copy), where the use-site polarity is
        /// still unknown.
        preserve,
        /// Resolve each polarity var by the polarity of the position it
        /// occupies: open (a fresh unnamed flex, exactly what an implicitly
        /// opened output-position union gets) in positive/output positions,
        /// closed (`[]`) in negative/input positions. The walk starts at
        /// `current_polarity` and negates through function argument
        /// positions, so polarity composes correctly through functions
        /// embedded in alias bodies.
        resolve_by_polarity,
        /// Like `resolve_by_polarity` for negative positions (closed), but a
        /// marker in a positive position is copied as a fresh marker: the copy
        /// is itself a deferred signature whose own uses decide (a where-alias
        /// declaration's method signature copied into a referencing
        /// annotation, or an alias body embedded in one).
        defer_open,
    };

    /// A polarity marker this instantiation resolved open: the fresh flex
    /// ext it became, and the tags the copied union lists next to it. The
    /// tags are attached when the union's copy is finished, the one point
    /// where the ext and the union's tags meet; a marker is always a union's
    /// ext, so `listed_tags` is null only for a copy that never reached one.
    pub const OpenedMarkerExt = struct {
        source: Var,
        ext: Var,
        listed_tags: ?Tag.SafeMultiList.Range = null,
    };

    const Self = @This();

    fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.idents.getText(idx);
    }

    fn scratch(self: *Self) *Scratch {
        return &self.store.instantiate_scratch;
    }

    fn recordProofPair(self: *Self, raw_source: Var, raw_destination: Var) std.mem.Allocator.Error!void {
        const sink = self.proof_pairs orelse return;
        const source_root = self.store.resolveVar(raw_source).var_;
        const destination_root = self.store.resolveVar(raw_destination).var_;
        try sink.append(self.store.gpa, .{
            .raw_source_var = @intFromEnum(raw_source),
            .raw_destination_var = @intFromEnum(raw_destination),
            .canonical_source_var = @intFromEnum(source_root),
            .canonical_destination_var = @intFromEnum(destination_root),
        });
    }

    /// Ensure that a checker decision made while this traversal is active can
    /// name one exact immutable occurrence in the proof. The instantiator is
    /// the producer of both the fresh destination and this occurrence; callers
    /// retain the returned raw token only until the same copy publication
    /// binds it to its canonical pair offset.
    pub fn ensureProofOccurrence(
        self: *Self,
        raw_source: Var,
        raw_destination: Var,
    ) std.mem.Allocator.Error!void {
        const sink = self.proof_pairs orelse
            std.debug.panic("instantiation decision requested proof from a traversal without a proof sink", .{});
        for (sink.items) |pair| {
            if (pair.raw_source_var == @intFromEnum(raw_source) and
                pair.raw_destination_var == @intFromEnum(raw_destination))
            {
                return;
            }
        }
        try self.recordProofPair(raw_source, raw_destination);
    }

    fn recordMapping(
        self: *Self,
        canonical_source: Var,
        raw_source: Var,
        raw_destination: Var,
    ) std.mem.Allocator.Error!void {
        try self.var_map.put(canonical_source, raw_destination);
        try self.recordProofPair(raw_source, raw_destination);
    }

    fn recordPreexistingRigidSubstitution(
        self: *Self,
        canonical_source: Var,
        destination: Var,
    ) std.mem.Allocator.Error!void {
        const sink = self.preexisting_rigid_substitutions orelse return;
        const entry = try sink.getOrPut(self.store.gpa, canonical_source);
        if (entry.found_existing) {
            std.debug.assert(entry.value_ptr.* == destination);
        } else {
            entry.value_ptr.* = destination;
        }
    }

    fn recordProofWitness(
        self: *Self,
        mb_edge: ?ProofEdge,
        source: Var,
        destination: Var,
        action: ProofWitnessAction,
        action_auxiliary_origin_kind: ProofWitnessAuxiliaryOriginKind,
        action_auxiliary_origin_index: u32,
    ) std.mem.Allocator.Error!void {
        const final_action = if (mb_edge) |edge|
            if (edge.action_override == .requirement_component_ingress and
                (action == .flex_fresh_flex_copy or action == .rigid_fresh_flex_cut))
                ProofWitnessAction.requirement_component_fresh_flex_copy
            else
                edge.action_override orelse action
        else
            action;
        if (mb_edge == null) {
            if (self.proof_root_selection) |selection| {
                if (selection.* != null) {
                    std.debug.panic("instantiation root selection was published more than once", .{});
                }
                selection.* = .{
                    .source_var = source,
                    .destination_var = destination,
                    .action = final_action,
                };
            }
        }
        const sink = self.proof_witnesses orelse return;
        const edge = mb_edge orelse blk: {
            // An ordinary traversed root is established by the root pair and
            // its structural witnesses. A root cut or fresh-flex creation
            // additionally needs the typed action and raw ids that later
            // unification can erase.
            if (action == .traverse) return;
            break :blk ProofEdge{
                .parent_raw_source_var = source,
                .parent_raw_destination_var = destination,
                .edge_kind = .root_copy_action,
            };
        };
        if (edge.action_override != null and action_auxiliary_origin_kind != .none) {
            std.debug.panic("virtual requirement ingress hid a child cut authority", .{});
        }
        const carries_raw_cut = final_action != .traverse and final_action != .requirement_component_ingress;
        try sink.append(self.store.gpa, .{
            .parent_raw_source_var = @intFromEnum(edge.parent_raw_source_var),
            .parent_raw_destination_var = @intFromEnum(edge.parent_raw_destination_var),
            .child_raw_source_var = @intFromEnum(source),
            .child_raw_destination_var = @intFromEnum(destination),
            .edge_kind = edge.edge_kind,
            .edge_index = edge.edge_index,
            .edge_name = edge.edge_name,
            .edge_origin_module = edge.edge_origin_module,
            .edge_source_decl = edge.edge_source_decl,
            .constraint_source_index = edge.constraint_source_index,
            .action = final_action,
            .auxiliary_origin_kind = if (edge.action_override == null) action_auxiliary_origin_kind else .none,
            .auxiliary_origin_index = if (edge.action_override == null) action_auxiliary_origin_index else 0,
            .raw_source_var = if (carries_raw_cut) @intFromEnum(source) else std.math.maxInt(u32),
            .raw_destination_var = if (carries_raw_cut) @intFromEnum(destination) else std.math.maxInt(u32),
        });
    }

    pub fn recordProofConstraintPair(
        self: *Self,
        source_constraint_index: u32,
        destination_constraint_index: u32,
    ) std.mem.Allocator.Error!void {
        const sink = self.proof_constraint_pairs orelse return;
        try sink.append(self.store.gpa, .{
            .source_constraint_index = source_constraint_index,
            .destination_constraint_index = destination_constraint_index,
        });
    }

    // instantiation //

    /// Instantiate a variable
    pub fn instantiateVar(
        self: *Self,
        initial_var: Var,
    ) std.mem.Allocator.Error!Var {
        return self.instantiateVarHelp(initial_var, false);
    }

    /// Instantiate a binding that the checker explicitly classified as a
    /// rank-1 type scheme. A scheme may be partially generalized: its
    /// structural root can be monomorphic while descendants are quantified.
    /// Force-copying the root enters that structure so the ordinary rank-aware
    /// walk can freshen exactly those generalized descendants.
    pub fn instantiateTypeScheme(
        self: *Self,
        initial_var: Var,
    ) std.mem.Allocator.Error!Var {
        const previous = self.copy_scheme_structure;
        self.copy_scheme_structure = true;
        defer self.copy_scheme_structure = previous;
        return self.instantiateVarHelp(initial_var, true);
    }

    fn instantiateVarHelp(
        self: *Self,
        initial_var: Var,
        force_root_copy: bool,
    ) std.mem.Allocator.Error!Var {
        return self.instantiateVarHelpFromEdge(initial_var, force_root_copy, null);
    }

    fn instantiateVarHelpFromEdge(
        self: *Self,
        initial_var: Var,
        force_root_copy: bool,
        edge: ?ProofEdge,
    ) std.mem.Allocator.Error!Var {
        const root_selection = if (edge == null) self.proof_root_selection else null;
        if (root_selection) |selection| {
            if (self.proof_pairs == null or self.proof_witnesses == null) {
                std.debug.panic("instantiation root selection omitted its proof sinks", .{});
            }
            if (selection.* != null) {
                std.debug.panic("instantiation root selection sink was not empty", .{});
            }
        }
        errdefer {
            if (root_selection) |selection| selection.* = null;
        }

        const machine = self.scratch();
        const frames_base = machine.frames.items.len;
        const values_base = machine.value_stack.items.len;
        // A completed walk drains every buffer back to its entry length:
        // frames as each one finishes, the value stack as each frame consumes
        // its children, and every pending run as the step that collected it
        // appends the run to the store. An allocation failure mid-copy can
        // leave entries behind on buffers the `TypesStore` keeps for the next
        // instantiation, so unwind them here and preserve `Scratch`'s
        // entry-length invariant on both exit paths.
        const tags_base = machine.pending_tags.items.len;
        const fields_base = machine.pending_fields.items.len;
        const constraints_base = machine.pending_constraints.items.len;
        const parts_base = machine.pending_parts.items.len;
        const marker_contracts_base = machine.pending_marker_contracts.items.len;
        const marker_bases_base = machine.pending_marker_bases.items.len;
        const marker_paths_base = machine.pending_marker_paths.items.len;
        const proof_pairs_base = if (self.proof_pairs) |pairs| pairs.items.len else 0;
        const proof_constraint_pairs_base = if (self.proof_constraint_pairs) |pairs| pairs.items.len else 0;
        const proof_witnesses_base = if (self.proof_witnesses) |witnesses| witnesses.items.len else 0;
        errdefer {
            machine.frames.items.len = frames_base;
            machine.value_stack.items.len = values_base;
            machine.pending_tags.items.len = tags_base;
            machine.pending_fields.items.len = fields_base;
            machine.pending_constraints.items.len = constraints_base;
            machine.pending_parts.items.len = parts_base;
            machine.pending_marker_contracts.items.len = marker_contracts_base;
            machine.pending_marker_bases.items.len = marker_bases_base;
            machine.pending_marker_paths.items.len = marker_paths_base;
            if (self.proof_pairs) |pairs| pairs.items.len = proof_pairs_base;
            if (self.proof_constraint_pairs) |pairs| pairs.items.len = proof_constraint_pairs_base;
            if (self.proof_witnesses) |witnesses| witnesses.items.len = proof_witnesses_base;
        }

        if (!try self.requestVar(initial_var, force_root_copy, edge)) {
            while (machine.frames.items.len > frames_base) {
                const top = &machine.frames.items[machine.frames.items.len - 1];
                // A step either suspends after pushing exactly one child
                // frame (having already written its own resume state), or
                // finishes without pushing anything—so popping on finish
                // always removes the frame the step ran for.
                const finished = switch (top.*) {
                    .flex_like => |*frame| try self.stepFlexLike(frame),
                    .alias => |*frame| try self.stepAlias(frame),
                    .tuple => |*frame| try self.stepTuple(frame),
                    .nominal => |*frame| try self.stepNominal(frame),
                    .func => |*frame| try self.stepFunc(frame),
                    .record => |*frame| try self.stepRecord(frame),
                    .record_unbound => |*frame| try self.stepRecordUnbound(frame),
                    .tag_union => |*frame| try self.stepTagUnion(frame),
                };
                if (finished) {
                    machine.frames.items.len -= 1;
                }
            }
        }

        std.debug.assert(machine.value_stack.items.len == values_base + 1);
        const result = machine.value_stack.pop().?;
        if (edge == null) {
            if (self.proof_witnesses) |witnesses| {
                if (witnesses.items.len == proof_witnesses_base) {
                    try self.recordProofWitness(.{
                        .parent_raw_source_var = initial_var,
                        .parent_raw_destination_var = result,
                        .edge_kind = .root_copy_action,
                    }, initial_var, result, .traverse, .none, 0);
                }
            }
        }
        if (root_selection) |selection| {
            const selected = selection.* orelse
                std.debug.panic("instantiation root request omitted its exact selection", .{});
            if (selected.destination_var != result) {
                std.debug.panic("instantiation root selection disagreed with its returned destination", .{});
            }
        }
        return result;
    }

    /// Copy the head of one var: resolve it, share it when rank says so,
    /// reuse an existing mapping, and otherwise mint + register the
    /// placeholder and either fill it immediately (contents with no children)
    /// or push the frame that will fill it. Returns true when the result var
    /// is already on the value stack; false when a frame was pushed.
    fn requestVar(
        self: *Self,
        initial_var: Var,
        force_root_copy: bool,
        edge: ?ProofEdge,
    ) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        const resolved = self.store.resolveVar(initial_var);
        const resolved_var = resolved.var_;

        // Ordinary instantiation shares every non-generalized var. A binding
        // explicitly classified as a scheme instead copies non-generalized
        // structural nodes so generalized leaves at arbitrary depth remain
        // reachable, while preserving the identity of monomorphic leaves. A
        // polarity marker is never shared: it stands for "decided by this
        // instantiation" whatever its rank (a where-method signature is
        // instantiated per body use before the enclosing scheme generalizes).
        const is_polarity_marker = self.polarity_var_ident != null and
            resolved.desc.content == .rigid and
            resolved.desc.content.rigid.name.eql(self.polarity_var_ident.?);
        const is_leaf = switch (resolved.desc.content) {
            .alias, .structure => false,
            .flex, .rigid, .field_presence, .err => true,
        };
        if (!force_root_copy and self.share_leaves and is_leaf and !is_polarity_marker) {
            // This is the same Store occurrence on both sides of the cut. If
            // the request entered through a redirect, retain the exact shared
            // occurrence rather than encoding final canonical equality as if
            // it were copy-time identity.
            try self.recordProofPair(resolved_var, resolved_var);
            try self.recordProofWitness(edge, resolved_var, resolved_var, .local_raw_identity_share_cut, .none, 0);
            try machine.value_stack.append(self.store.gpa, resolved_var);
            return true;
        }
        if (!force_root_copy and self.rank_behavior == .respect_rank and resolved.desc.rank != .generalized) {
            const copy_structure = self.copy_scheme_structure and !is_leaf;
            if (!copy_structure and !is_polarity_marker) {
                try self.recordProofPair(resolved_var, resolved_var);
                try self.recordProofWitness(edge, resolved_var, resolved_var, .local_raw_identity_share_cut, .none, 0);
                try machine.value_stack.append(self.store.gpa, resolved_var);
                return true;
            }
        }

        // Check if we've already instantiated this variable
        if (self.var_map.count() > 0) {
            if (self.var_map.get(resolved_var)) |fresh_var| {
                try self.recordProofPair(initial_var, fresh_var);
                try self.recordProofWitness(edge, initial_var, fresh_var, .traverse, .none, 0);
                try machine.value_stack.append(self.store.gpa, fresh_var);
                return true;
            }
        }

        const empty_tag_union_is_default = resolved.desc.flags.empty_tag_union_is_default;
        switch (resolved.desc.content) {
            .rigid => |rigid| {
                // Polarity vars (the deferred open-vs-closed tag union
                // extensions of alias declaration bodies) are resolved before
                // any rigid behavior applies: they are compiler-internal and
                // must never be substituted, flexed, or kept rigid by the
                // caller's rigid policy.
                if (self.polarity_var_ident) |polarity_ident| {
                    if (rigid.name.eql(polarity_ident)) {
                        const opened = self.polarity_var_behavior == .resolve_by_polarity and self.current_polarity == .pos;
                        const marker_content: Content = switch (self.polarity_var_behavior) {
                            .close => .{ .structure = .empty_tag_union },
                            .preserve => .{ .rigid = Rigid.init(rigid.name) },
                            .resolve_by_polarity => switch (self.current_polarity) {
                                .pos => .{ .flex = Flex.init() },
                                .neg => .{ .structure = .empty_tag_union },
                            },
                            .defer_open => switch (self.current_polarity) {
                                .pos => .{ .rigid = Rigid.init(rigid.name) },
                                .neg => .{ .structure = .empty_tag_union },
                            },
                        };
                        const marker_var = try self.store.freshFromContentWithRank(marker_content, self.current_rank);
                        if (opened) {
                            if (self.opened_marker_exts) |sink| try sink.append(self.store.gpa, .{
                                .source = resolved_var,
                                .ext = marker_var,
                            });
                        }
                        try self.recordMapping(resolved_var, initial_var, marker_var);
                        const marker_action: ProofWitnessAction = switch (self.polarity_var_behavior) {
                            .close => .polarity_close_cut,
                            .preserve => .polarity_preserve_cut,
                            .resolve_by_polarity => switch (self.current_polarity) {
                                .pos => .polarity_open_cut,
                                .neg => .polarity_close_cut,
                            },
                            .defer_open => switch (self.current_polarity) {
                                .pos => .polarity_preserve_cut,
                                .neg => .polarity_close_cut,
                            },
                        };
                        try self.recordProofWitness(edge, initial_var, marker_var, marker_action, .none, 0);
                        try machine.value_stack.append(self.store.gpa, marker_var);
                        return true;
                    }
                }

                // If this var is rigid, then create a new var depending on the
                // provided behavior
                const fresh_type: enum { flex, rigid } = blk: {
                    switch (self.rigid_behavior) {
                        .fresh_rigid => {
                            break :blk .rigid;
                        },
                        .fresh_flex => {
                            break :blk .flex;
                        },
                        .substitute_rigids => |rigid_subs| {
                            // An anonymous open extension (`..`, written or
                            // implicit) is never a declaration parameter, so
                            // it is never in the substitution map: copy it as
                            // a fresh rigid, like `.substitute_rigids_fresh`.
                            if (self.anonymous_ext_ident) |ext_ident| {
                                if (rigid.name.eql(ext_ident)) break :blk .rigid;
                            }

                            // If this is a var that we're substituting, then we
                            // we just return it.

                            const existing_var = inner_blk: {
                                if (rigid_subs.get(rigid.name)) |existing_flex| {
                                    break :inner_blk existing_flex;
                                } else {
                                    std.debug.assert(false);
                                    break :inner_blk try self.store.freshFromContentWithRank(
                                        .err,
                                        self.current_rank,
                                    );
                                }
                            };

                            // This exact branch selected a caller-owned
                            // destination. Record that producer decision before
                            // the general semantic map is updated; downstream
                            // freshness bookkeeping must not reconstruct it.
                            try self.recordPreexistingRigidSubstitution(resolved_var, existing_var);

                            // Remember this substitution for recursive references
                            try self.recordMapping(resolved_var, initial_var, existing_var);

                            const substitution_ordinal = if (self.proof_witnesses != null) origin_blk: {
                                const origins = self.rigid_substitution_origins orelse
                                    std.debug.panic("proof-carrying rigid substitution omitted its annotation origin map", .{});
                                break :origin_blk origins.get(rigid.name) orelse
                                    std.debug.panic("proof-carrying rigid substitution omitted its exact annotation ordinal", .{});
                            } else 0;
                            try self.recordProofWitness(
                                edge,
                                initial_var,
                                existing_var,
                                .exact_annotation_substitution_cut,
                                if (self.proof_witnesses != null) .annotation_substitution else .none,
                                substitution_ordinal,
                            );

                            try machine.value_stack.append(self.store.gpa, existing_var);
                            return true;
                        },
                        .substitute_rigids_fresh => |rigid_subs| {
                            if (rigid_subs.get(rigid.name)) |existing_var| {
                                try self.recordPreexistingRigidSubstitution(resolved_var, existing_var);
                                try self.recordMapping(resolved_var, initial_var, existing_var);
                                const substitution_ordinal = if (self.proof_witnesses != null) origin_blk: {
                                    const origins = self.rigid_substitution_origins orelse
                                        std.debug.panic("proof-carrying rigid substitution omitted its annotation origin map", .{});
                                    break :origin_blk origins.get(rigid.name) orelse
                                        std.debug.panic("proof-carrying rigid substitution omitted its exact annotation ordinal", .{});
                                } else 0;
                                try self.recordProofWitness(
                                    edge,
                                    initial_var,
                                    existing_var,
                                    .exact_annotation_substitution_cut,
                                    if (self.proof_witnesses != null) .annotation_substitution else .none,
                                    substitution_ordinal,
                                );
                                try machine.value_stack.append(self.store.gpa, existing_var);
                                return true;
                            }
                            break :blk .rigid;
                        },
                    }
                };

                // Remember this substitution for recursive references
                // IMPORTANT: This has to be registered _before_ any child copy runs
                const fresh_var = try self.store.freshFromContentWithRank(.{ .flex = Flex.init() }, self.current_rank);
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                try self.recordProofWitness(
                    edge,
                    initial_var,
                    fresh_var,
                    switch (fresh_type) {
                        .flex => .rigid_fresh_flex_cut,
                        .rigid => .rigid_fresh_rigid_cut,
                    },
                    .none,
                    0,
                );

                if (rigid.constraints.len() == 0) {
                    const fresh_content = switch (fresh_type) {
                        .flex => Content{ .flex = Flex{ .name = rigid.name, .constraints = StaticDispatchConstraint.SafeList.Range.empty() } },
                        .rigid => Content{ .rigid = Rigid{ .name = rigid.name, .constraints = StaticDispatchConstraint.SafeList.Range.empty() } },
                    };
                    try self.fillPlaceholder(fresh_var, fresh_content, empty_tag_union_is_default);
                    try machine.value_stack.append(self.store.gpa, fresh_var);
                    return true;
                }

                try machine.frames.append(self.store.gpa, .{ .flex_like = .{
                    .common = .{
                        .raw_source_var = initial_var,
                        .raw_destination_var = fresh_var,
                        .empty_tag_union_is_default = empty_tag_union_is_default,
                    },
                    .result = switch (fresh_type) {
                        .flex => .flex,
                        .rigid => .rigid,
                    },
                    .name = rigid.name,
                    .cons_start = @intFromEnum(rigid.constraints.start),
                    .cons_len = @intCast(rigid.constraints.len()),
                    .cons_base = @intCast(machine.pending_constraints.items.len),
                } });
                return false;
            },
            .flex => |flex| {
                // Remember this substitution for recursive references
                // IMPORTANT: This has to be registered _before_ any child copy runs
                const fresh_var = try self.store.fresh();
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                // Unlike a memoized revisit above, this branch genuinely
                // allocates the destination for a source flex. Retain that
                // copy-time fact even when the flex has constraints and the
                // same root subsequently publishes structural witnesses.
                try self.recordProofWitness(edge, initial_var, fresh_var, .flex_fresh_flex_copy, .none, 0);

                if (flex.constraints.len() == 0) {
                    const fresh_content = Content{ .flex = Flex{ .name = flex.name, .constraints = StaticDispatchConstraint.SafeList.Range.empty() } };
                    try self.fillPlaceholder(fresh_var, fresh_content, empty_tag_union_is_default);
                    try machine.value_stack.append(self.store.gpa, fresh_var);
                    return true;
                }

                try machine.frames.append(self.store.gpa, .{ .flex_like = .{
                    .common = .{
                        .raw_source_var = initial_var,
                        .raw_destination_var = fresh_var,
                        .empty_tag_union_is_default = empty_tag_union_is_default,
                    },
                    .result = .flex,
                    .name = flex.name,
                    .cons_start = @intFromEnum(flex.constraints.start),
                    .cons_len = @intCast(flex.constraints.len()),
                    .cons_base = @intCast(machine.pending_constraints.items.len),
                } });
                return false;
            },
            .alias => |alias| {
                const fresh_var = try self.store.fresh();
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                try self.recordProofWitness(edge, initial_var, fresh_var, .traverse, .none, 0);

                var arg_span = alias.vars.nonempty;
                arg_span.dropFirstElem();
                try machine.frames.append(self.store.gpa, .{ .alias = .{
                    .common = .{
                        .raw_source_var = initial_var,
                        .raw_destination_var = fresh_var,
                        .empty_tag_union_is_default = empty_tag_union_is_default,
                    },
                    .alias = alias,
                    .args_start = @intFromEnum(arg_span.start),
                    .args_count = arg_span.count,
                    .vars_base = @intCast(machine.value_stack.items.len),
                } });
                return false;
            },
            .field_presence => |field_presence| {
                // A resolved presence fact carries no inner variables. It is
                // still copied to a fresh var so an instantiated field-kind
                // axis has the same identity semantics as every other axis.
                const fresh_var = try self.store.fresh();
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                try self.recordProofWitness(edge, initial_var, fresh_var, .traverse, .none, 0);
                try self.fillPlaceholder(fresh_var, .{ .field_presence = field_presence }, empty_tag_union_is_default);
                try machine.value_stack.append(self.store.gpa, fresh_var);
                return true;
            },
            .structure => |flat_type| {
                const fresh_var = try self.store.fresh();
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                try self.recordProofWitness(edge, initial_var, fresh_var, .traverse, .none, 0);

                switch (flat_type) {
                    .empty_record => {
                        try self.fillPlaceholder(fresh_var, Content{ .structure = FlatType.empty_record }, empty_tag_union_is_default);
                        try machine.value_stack.append(self.store.gpa, fresh_var);
                        return true;
                    },
                    .empty_tag_union => {
                        try self.fillPlaceholder(fresh_var, Content{ .structure = FlatType.empty_tag_union }, empty_tag_union_is_default);
                        try machine.value_stack.append(self.store.gpa, fresh_var);
                        return true;
                    },
                    .tuple => |tuple| {
                        try machine.frames.append(self.store.gpa, .{ .tuple = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .elems_start = @intFromEnum(tuple.elems.start),
                            .elems_count = tuple.elems.count,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .nominal_type => |nominal| {
                        // A nominal application instantiates its actual args
                        // only. The declaration's backing template is never
                        // touched here; it is instantiated exclusively by
                        // `instantiateNominalBacking` at the explicit opening
                        // operations.
                        const arg_span = TypesStore.getNominalArgsRange(nominal);
                        try machine.frames.append(self.store.gpa, .{ .nominal = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .nominal = nominal,
                            .args_start = @intFromEnum(arg_span.start),
                            .args_count = arg_span.count,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .fn_pure => |func| {
                        try machine.frames.append(self.store.gpa, .{ .func = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .pure,
                            .vars_base = @intCast(machine.value_stack.items.len),
                            .saved_polarity = self.current_polarity,
                        } });
                        return false;
                    },
                    .fn_effectful => |func| {
                        try machine.frames.append(self.store.gpa, .{ .func = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .effectful,
                            .vars_base = @intCast(machine.value_stack.items.len),
                            .saved_polarity = self.current_polarity,
                        } });
                        return false;
                    },
                    .fn_unbound => |func| {
                        try machine.frames.append(self.store.gpa, .{ .func = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .unbound,
                            .vars_base = @intCast(machine.value_stack.items.len),
                            .saved_polarity = self.current_polarity,
                        } });
                        return false;
                    },
                    .record => |record| {
                        try machine.frames.append(self.store.gpa, .{ .record = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .source_fields = record.fields,
                            .ext = record.ext,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .record_unbound => |fields| {
                        try machine.frames.append(self.store.gpa, .{ .record_unbound = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .source_fields = fields,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .tag_union => |tag_union| {
                        try machine.frames.append(self.store.gpa, .{ .tag_union = .{
                            .common = .{
                                .raw_source_var = initial_var,
                                .raw_destination_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .source_tags = tag_union.tags,
                            .ext = tag_union.ext,
                            .vars_base = @intCast(machine.value_stack.items.len),
                            .tags_base = @intCast(machine.pending_tags.items.len),
                        } });
                        return false;
                    },
                }
            },
            .err => {
                const fresh_var = try self.store.fresh();
                try self.recordMapping(resolved_var, initial_var, fresh_var);
                try self.recordProofWitness(edge, initial_var, fresh_var, .traverse, .none, 0);
                try self.fillPlaceholder(fresh_var, Content.err, empty_tag_union_is_default);
                try machine.value_stack.append(self.store.gpa, fresh_var);
                return true;
            },
        }
    }

    /// Update the placeholder fresh var with its real content.
    fn fillPlaceholder(
        self: *Self,
        fresh_var: Var,
        content: Content,
        empty_tag_union_is_default: bool,
    ) std.mem.Allocator.Error!void {
        try self.store.dangerousSetVarDesc(
            fresh_var,
            .{
                .content = content,
                .rank = self.current_rank,
                .flags = .{ .empty_tag_union_is_default = empty_tag_union_is_default },
            },
        );
    }

    /// Fill `common`'s placeholder and publish it as this frame's result.
    fn finishFrame(
        self: *Self,
        common: FillCommon,
        content: Content,
    ) std.mem.Allocator.Error!void {
        try self.fillPlaceholder(common.raw_destination_var, content, common.empty_tag_union_is_default);
        try self.scratch().value_stack.append(self.store.gpa, common.raw_destination_var);
    }

    // IMPORTANT for every step function below: source runs (vars, record
    // fields, tags, constraints, interpolation parts) must be re-fetched by
    // raw index on each visit, never held as slices. Child copies append to
    // the same backing arrays, which may reallocate and invalidate any held
    // slice. Source entries are append-only, so index-based re-fetching
    // always sees the original values.

    fn copyWhereMethodMarkerMetadata(
        self: *Self,
        source_constraint_index: u32,
        constraint: StaticDispatchConstraint,
    ) std.mem.Allocator.Error!WhereMethodMarkerMetadata {
        const marker_count = constraint.where_method_markers.len();
        if (marker_count == 0) {
            if (constraint.where_method_marker_bases.len() != 0) {
                std.debug.panic("marker-free source constraint carried where-marker bases", .{});
            }
            return .{ .markers = .empty(), .bases = .empty() };
        }
        const copy_step = self.where_marker_copy_step orelse
            std.debug.panic("marker-bearing instantiation had no finite local copy step", .{});
        if (self.where_marker_carried) |carried| carried.* = true;
        if (source_constraint_index >= self.store.static_dispatch_constraints.items.items.len or
            !std.meta.eql(
                self.store.static_dispatch_constraints.items.items[source_constraint_index],
                constraint,
            ))
        {
            std.debug.panic("marker-bearing instantiation did not name its exact source constraint", .{});
        }

        const machine = self.scratch();
        const contracts_base = machine.pending_marker_contracts.items.len;
        const bases_base = machine.pending_marker_bases.items.len;
        const paths_base = machine.pending_marker_paths.items.len;
        defer {
            machine.pending_marker_contracts.items.len = contracts_base;
            machine.pending_marker_bases.items.len = bases_base;
            machine.pending_marker_paths.items.len = paths_base;
        }

        const source_start: u32 = @intFromEnum(constraint.where_method_markers.start);
        for (0..marker_count) |raw_offset| {
            const marker_offset: u32 = @intCast(raw_offset);
            const source_index = std.math.add(u32, source_start, marker_offset) catch
                std.debug.panic("source marker index overflowed u32", .{});
            if (source_index >= self.store.where_method_marker_contracts.items.items.len) {
                std.debug.panic("source marker range escaped its pool", .{});
            }
            var copied = self.store.where_method_marker_contracts.items.items[source_index];
            if (copied.path_start > self.store.where_method_marker_path_steps.items.items.len or
                copied.path_len > self.store.where_method_marker_path_steps.items.items.len - copied.path_start)
            {
                std.debug.panic("source marker path escaped its pool", .{});
            }
            const local_path_start: u32 = @intCast(machine.pending_marker_paths.items.len - paths_base);
            try machine.pending_marker_paths.appendSlice(
                self.store.gpa,
                self.store.where_method_marker_path_steps.items.items[copied.path_start..][0..copied.path_len],
            );
            copied.path_start = local_path_start;
            try machine.pending_marker_contracts.append(self.store.gpa, copied);
            try machine.pending_marker_bases.append(self.store.gpa, .{
                .marker_offset = marker_offset,
                .copy_step = copy_step,
                .source_constraint_index = source_constraint_index,
                .source_contract_offset = marker_offset,
            });
        }

        const pending_contracts = machine.pending_marker_contracts.items[contracts_base..];
        const pending_bases = machine.pending_marker_bases.items[bases_base..];
        const pending_paths = machine.pending_marker_paths.items[paths_base..];
        try self.store.where_method_marker_contracts.items.ensureUnusedCapacity(self.store.gpa, pending_contracts.len);
        try self.store.where_method_marker_bases.items.ensureUnusedCapacity(self.store.gpa, pending_bases.len);
        try self.store.where_method_marker_path_steps.items.ensureUnusedCapacity(self.store.gpa, pending_paths.len);

        const contract_start: u32 = @intCast(self.store.where_method_marker_contracts.items.items.len);
        const basis_start: u32 = @intCast(self.store.where_method_marker_bases.items.items.len);
        const path_start: u32 = @intCast(self.store.where_method_marker_path_steps.items.items.len);
        self.store.where_method_marker_path_steps.items.appendSliceAssumeCapacity(pending_paths);
        for (pending_contracts) |pending| {
            var copied = pending;
            copied.path_start = std.math.add(u32, path_start, pending.path_start) catch
                std.debug.panic("destination marker path index overflowed u32", .{});
            self.store.where_method_marker_contracts.items.appendAssumeCapacity(copied);
        }
        self.store.where_method_marker_bases.items.appendSliceAssumeCapacity(pending_bases);
        return .{
            .markers = .{ .start = @enumFromInt(contract_start), .count = @intCast(pending_contracts.len) },
            .bases = .{ .start = @enumFromInt(basis_start), .count = @intCast(pending_bases.len) },
        };
    }

    fn stepFlexLike(self: *Self, frame: *FlexLikeFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            switch (frame.stage) {
                .dispatch_fn => {
                    if (frame.cons_idx == frame.cons_len) {
                        const fresh_range = try self.store.appendStaticDispatchConstraints(
                            machine.pending_constraints.items[frame.cons_base..],
                        );
                        if (self.proof_constraint_pairs) |sink| {
                            if (fresh_range.len() != frame.cons_len) {
                                std.debug.panic("instantiated constraint range changed cardinality", .{});
                            }
                            try sink.ensureUnusedCapacity(self.store.gpa, frame.cons_len);
                            const fresh_start: u32 = @intFromEnum(fresh_range.start);
                            for (0..frame.cons_len) |raw_offset| {
                                const offset: u32 = @intCast(raw_offset);
                                sink.appendAssumeCapacity(.{
                                    .source_constraint_index = std.math.add(u32, frame.cons_start, offset) catch
                                        std.debug.panic("source constraint proof index overflowed u32", .{}),
                                    .destination_constraint_index = std.math.add(u32, fresh_start, offset) catch
                                        std.debug.panic("destination constraint proof index overflowed u32", .{}),
                                });
                            }
                        }
                        machine.pending_constraints.items.len = frame.cons_base;
                        const fresh_content = switch (frame.result) {
                            .flex => Content{ .flex = Flex{ .name = frame.name, .constraints = fresh_range } },
                            .rigid => Content{ .rigid = Rigid{ .name = frame.name.?, .constraints = fresh_range } },
                        };
                        try self.finishFrame(frame.common, fresh_content);
                        return true;
                    }
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    frame.stage = .await_fn;
                    if (!try self.requestVar(constraint.fn_var, false, .{
                        .parent_raw_source_var = frame.common.raw_source_var,
                        .parent_raw_destination_var = frame.common.raw_destination_var,
                        .edge_kind = .static_dispatch_function,
                        .constraint_source_index = frame.cons_start + frame.cons_idx,
                    })) return false;
                },
                .await_fn => {
                    frame.fresh_fn_var = machine.value_stack.pop().?;
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    if (constraint.interpolation.isPresent()) {
                        frame.parts_base = @intCast(machine.pending_parts.items.len);
                        frame.part_idx = 0;
                        frame.stage = .dispatch_part_or_item;
                        continue;
                    }
                    var fresh_constraint = constraint;
                    fresh_constraint.fn_var = frame.fresh_fn_var;
                    fresh_constraint.constraint_evidence = .none;
                    try machine.pending_constraints.ensureUnusedCapacity(self.store.gpa, 1);
                    const marker_metadata = try self.copyWhereMethodMarkerMetadata(
                        frame.cons_start + frame.cons_idx,
                        constraint,
                    );
                    fresh_constraint.where_method_markers = marker_metadata.markers;
                    fresh_constraint.where_method_marker_bases = marker_metadata.bases;
                    machine.pending_constraints.appendAssumeCapacity(fresh_constraint);
                    frame.cons_idx += 1;
                    frame.stage = .dispatch_fn;
                },
                .dispatch_part_or_item => {
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    const metadata = constraint.interpolation;
                    if (frame.part_idx == metadata.interpolated_parts.len()) {
                        frame.stage = .await_item;
                        if (!try self.requestVar(metadata.item_var, false, .{
                            .parent_raw_source_var = frame.common.raw_source_var,
                            .parent_raw_destination_var = frame.common.raw_destination_var,
                            .edge_kind = .interpolation_item,
                            .constraint_source_index = frame.cons_start + frame.cons_idx,
                        })) return false;
                    } else {
                        const part = self.store.getInterpolationPartAt(metadata.interpolated_parts, frame.part_idx);
                        frame.stage = .await_part;
                        if (!try self.requestVar(part.var_, false, .{
                            .parent_raw_source_var = frame.common.raw_source_var,
                            .parent_raw_destination_var = frame.common.raw_destination_var,
                            .edge_kind = .interpolation_part,
                            .edge_index = frame.part_idx,
                            .constraint_source_index = frame.cons_start + frame.cons_idx,
                        })) return false;
                    }
                },
                .await_part => {
                    const fresh_part_var = machine.value_stack.pop().?;
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    const part = self.store.getInterpolationPartAt(constraint.interpolation.interpolated_parts, frame.part_idx);
                    try machine.pending_parts.append(self.store.gpa, .{
                        .var_ = fresh_part_var,
                        .region = part.region,
                    });
                    frame.part_idx += 1;
                    frame.stage = .dispatch_part_or_item;
                },
                .await_item => {
                    const fresh_item_var = machine.value_stack.pop().?;
                    const fresh_parts_range = try self.store.appendInterpolationParts(
                        machine.pending_parts.items[frame.parts_base..],
                    );
                    machine.pending_parts.items.len = frame.parts_base;
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    var fresh_constraint = constraint;
                    fresh_constraint.fn_var = frame.fresh_fn_var;
                    fresh_constraint.constraint_evidence = .none;
                    fresh_constraint.interpolation = .{
                        .expr_region = constraint.interpolation.expr_region,
                        .item_var = fresh_item_var,
                        .interpolated_parts = fresh_parts_range,
                    };
                    try machine.pending_constraints.ensureUnusedCapacity(self.store.gpa, 1);
                    const marker_metadata = try self.copyWhereMethodMarkerMetadata(
                        frame.cons_start + frame.cons_idx,
                        constraint,
                    );
                    fresh_constraint.where_method_markers = marker_metadata.markers;
                    fresh_constraint.where_method_marker_bases = marker_metadata.bases;
                    machine.pending_constraints.appendAssumeCapacity(fresh_constraint);
                    frame.cons_idx += 1;
                    frame.stage = .dispatch_fn;
                },
            }
        }
    }

    fn stepAlias(self: *Self, frame: *AliasFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            const arrived: u32 = @intCast(machine.value_stack.items.len - frame.vars_base);
            if (arrived < frame.args_count) {
                const arg_var = self.store.vars.items.items[frame.args_start + arrived];
                if (!try self.requestVar(arg_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .alias_argument,
                    .edge_index = arrived,
                    .edge_name = @bitCast(frame.alias.ident.ident_idx),
                    .edge_origin_module = @intFromEnum(frame.alias.origin_module),
                    .edge_source_decl = @bitCast(frame.alias.source_decl),
                })) return false;
                continue;
            }
            if (arrived == frame.args_count) {
                const backing_var = self.store.getAliasBackingVar(frame.alias);
                if (!try self.requestVar(backing_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .alias_backing,
                })) return false;
                continue;
            }
            const values = machine.value_stack.items;
            const fresh_backing_var = values[frame.vars_base + frame.args_count];
            const fresh_args = values[frame.vars_base..][0..frame.args_count];
            const fresh_content = try self.store.mkAliasWithSourceDeclAndBuiltinOrigin(
                frame.alias.ident,
                fresh_backing_var,
                fresh_args,
                frame.alias.origin_module,
                frame.alias.source_decl.toOptional(),
                frame.alias.source_decl.originIsBuiltin(),
            );
            machine.value_stack.items.len = frame.vars_base;
            try self.finishFrame(frame.common, fresh_content);
            return true;
        }
    }

    fn stepTuple(self: *Self, frame: *TupleFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            const arrived: u32 = @intCast(machine.value_stack.items.len - frame.vars_base);
            if (arrived < frame.elems_count) {
                const elem_var = self.store.vars.items.items[frame.elems_start + arrived];
                if (!try self.requestVar(elem_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .tuple_element,
                    .edge_index = arrived,
                })) return false;
                continue;
            }
            const fresh_elems_range = try self.store.appendVars(
                machine.value_stack.items[frame.vars_base..][0..frame.elems_count],
            );
            machine.value_stack.items.len = frame.vars_base;
            try self.finishFrame(frame.common, Content{ .structure = FlatType{ .tuple = Tuple{ .elems = fresh_elems_range } } });
            return true;
        }
    }

    fn stepNominal(self: *Self, frame: *NominalFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            const arrived: u32 = @intCast(machine.value_stack.items.len - frame.vars_base);
            if (arrived < frame.args_count) {
                const arg_var = self.store.vars.items.items[frame.args_start + arrived];
                if (!try self.requestVar(arg_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .nominal_argument,
                    .edge_index = arrived,
                    .edge_name = @bitCast(frame.nominal.ident.ident_idx),
                    .edge_origin_module = @intFromEnum(frame.nominal.origin_module),
                    .edge_source_decl = @bitCast(frame.nominal.sourceDecl()),
                })) return false;
                continue;
            }
            const fresh_content = try self.store.mkNominalWithSourceDeclAndBuiltinOrigin(
                frame.nominal.ident,
                machine.value_stack.items[frame.vars_base..][0..frame.args_count],
                frame.nominal.origin_module,
                frame.nominal.sourceDeclOptional(),
                frame.nominal.isOpaque(),
                frame.nominal.originIsBuiltin(),
            );
            machine.value_stack.items.len = frame.vars_base;
            try self.finishFrame(frame.common, fresh_content);
            return true;
        }
    }

    fn stepFunc(self: *Self, frame: *FuncFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        const args_count = frame.func.args.count;
        const deps_count = frame.func.effect_deps.count;
        while (true) {
            const arrived: u32 = @intCast(machine.value_stack.items.len - frame.vars_base);
            if (arrived < args_count) {
                const arg_var = self.store.vars.items.items[@intFromEnum(frame.func.args.start) + arrived];
                // Argument positions negate the surrounding polarity.
                self.current_polarity = frame.saved_polarity.flip();
                if (!try self.requestVar(arg_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .function_argument,
                    .edge_index = arrived,
                })) return false;
                continue;
            }
            if (arrived == args_count) {
                // The return position preserves the surrounding polarity.
                self.current_polarity = frame.saved_polarity;
                if (!try self.requestVar(frame.func.ret, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .function_return,
                })) return false;
                continue;
            }
            if (arrived < args_count + 1 + deps_count) {
                const dep_var = self.store.vars.items.items[@intFromEnum(frame.func.effect_deps.start) + (arrived - args_count - 1)];
                self.current_polarity = frame.saved_polarity;
                if (!try self.requestVar(dep_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = .function_effect_dependency,
                    .edge_index = arrived - args_count - 1,
                })) return false;
                continue;
            }
            const values = machine.value_stack.items;
            const fresh_ret = values[frame.vars_base + args_count];
            const fresh_args_range = try self.store.appendVars(values[frame.vars_base..][0..args_count]);
            const fresh_effect_deps_range = try self.store.appendVars(
                values[frame.vars_base + args_count + 1 ..][0..deps_count],
            );
            machine.value_stack.items.len = frame.vars_base;
            const fresh_func = Func{
                .args = fresh_args_range,
                .ret = fresh_ret,
                .effect_deps = fresh_effect_deps_range,
            };
            const fresh_content = Content{ .structure = switch (frame.kind) {
                .pure => FlatType{ .fn_pure = fresh_func },
                .effectful => FlatType{ .fn_effectful = fresh_func },
                .unbound => FlatType{ .fn_unbound = fresh_func },
            } };
            try self.finishFrame(frame.common, fresh_content);
            return true;
        }
    }

    fn stepRecord(self: *Self, frame: *RecordFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            switch (frame.stage) {
                .fields => {
                    if (frame.field_idx < frame.source_fields.count) {
                        // Indexing through the run's start only happens when
                        // the record has fields; start may be undefined when
                        // count is 0.
                        const field = self.store.record_fields.get(@enumFromInt(@intFromEnum(frame.source_fields.start) + frame.field_idx));
                        const requested_axis = frame.field_axis;
                        const child_var = switch (requested_axis) {
                            .type_var => blk: {
                                if (field.presence.presenceVar() != null) {
                                    frame.field_axis = .presence_var;
                                } else {
                                    frame.field_idx += 1;
                                }
                                break :blk field.presence.typeVar();
                            },
                            .presence_var => blk: {
                                frame.field_idx += 1;
                                frame.field_axis = .type_var;
                                break :blk field.presence.presenceVar().?;
                            },
                        };
                        if (!try self.requestVar(child_var, false, .{
                            .parent_raw_source_var = frame.common.raw_source_var,
                            .parent_raw_destination_var = frame.common.raw_destination_var,
                            .edge_kind = switch (requested_axis) {
                                .type_var => .record_field_type,
                                .presence_var => .record_field_presence,
                            },
                            .edge_name = @bitCast(field.name),
                        })) return false;
                        continue;
                    }
                    frame.fields_range = try self.appendFreshRecordFields(frame.source_fields, frame.vars_base);
                    machine.value_stack.items.len = frame.vars_base;
                    frame.stage = .await_ext;
                    if (!try self.requestVar(frame.ext, false, .{
                        .parent_raw_source_var = frame.common.raw_source_var,
                        .parent_raw_destination_var = frame.common.raw_destination_var,
                        .edge_kind = .record_extension,
                    })) return false;
                },
                .await_ext => {
                    const fresh_ext = machine.value_stack.pop().?;
                    try self.finishFrame(frame.common, Content{ .structure = FlatType{ .record = Record{
                        .fields = frame.fields_range,
                        .ext = fresh_ext,
                    } } });
                    return true;
                },
            }
        }
    }

    fn stepRecordUnbound(self: *Self, frame: *RecordUnboundFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            if (frame.field_idx < frame.source_fields.count) {
                // Indexing through the run's start only happens when the
                // record has fields; start may be undefined when count is 0.
                const field = self.store.record_fields.get(@enumFromInt(@intFromEnum(frame.source_fields.start) + frame.field_idx));
                const requested_axis = frame.field_axis;
                const child_var = switch (requested_axis) {
                    .type_var => blk: {
                        if (field.presence.presenceVar() != null) {
                            frame.field_axis = .presence_var;
                        } else {
                            frame.field_idx += 1;
                        }
                        break :blk field.presence.typeVar();
                    },
                    .presence_var => blk: {
                        frame.field_idx += 1;
                        frame.field_axis = .type_var;
                        break :blk field.presence.presenceVar().?;
                    },
                };
                if (!try self.requestVar(child_var, false, .{
                    .parent_raw_source_var = frame.common.raw_source_var,
                    .parent_raw_destination_var = frame.common.raw_destination_var,
                    .edge_kind = switch (requested_axis) {
                        .type_var => .record_unbound_field_type,
                        .presence_var => .record_unbound_field_presence,
                    },
                    .edge_name = @bitCast(field.name),
                })) return false;
                continue;
            }
            const fresh_fields_range = try self.appendFreshRecordFields(frame.source_fields, frame.vars_base);
            machine.value_stack.items.len = frame.vars_base;
            try self.finishFrame(frame.common, Content{ .structure = FlatType{ .record_unbound = fresh_fields_range } });
            return true;
        }
    }

    /// Pair each copied field axis on the value stack with its re-fetched
    /// source field and append the run to the store.
    fn appendFreshRecordFields(
        self: *Self,
        source_fields: RecordField.SafeMultiList.Range,
        vars_base: u32,
    ) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
        const machine = self.scratch();
        const pending_base = machine.pending_fields.items.len;
        var vars_idx: usize = vars_base;
        for (0..source_fields.count) |i| {
            // The loop body runs only for a non-empty run, so reading the
            // run's start here never reads an empty range's undefined start.
            const field = self.store.record_fields.get(@enumFromInt(@intFromEnum(source_fields.start) + i));
            const fresh_type_var = machine.value_stack.items[vars_idx];
            vars_idx += 1;
            const fresh_presence = if (field.presence.presenceVar()) |_| blk: {
                const fresh_presence_var = machine.value_stack.items[vars_idx];
                vars_idx += 1;
                break :blk RecordField.Presence.unknown(fresh_presence_var, fresh_type_var);
            } else RecordField.Presence.required(fresh_type_var);
            try machine.pending_fields.append(self.store.gpa, RecordField{
                .name = field.name,
                .presence = fresh_presence,
            });
        }
        const fresh_fields_range = try self.store.appendRecordFields(machine.pending_fields.items[pending_base..]);
        machine.pending_fields.items.len = pending_base;
        return fresh_fields_range;
    }

    fn stepTagUnion(self: *Self, frame: *TagUnionFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            switch (frame.stage) {
                .tags => {
                    if (frame.tag_idx == frame.source_tags.count) {
                        // Sort the fresh tags alphabetically by name before appending.
                        // This ensures tag discriminants are consistent after instantiation.
                        std.mem.sort(Tag, machine.pending_tags.items[frame.tags_base..], @as(*const Self, self), struct {
                            fn less(instantiator: *const Self, a: Tag, b: Tag) bool {
                                return std.mem.order(u8, instantiator.getIdentText(a.name), instantiator.getIdentText(b.name)) == .lt;
                            }
                        }.less);
                        frame.tags_range = try self.store.appendTags(machine.pending_tags.items[frame.tags_base..]);
                        machine.pending_tags.items.len = frame.tags_base;
                        frame.stage = .await_ext;
                        if (!try self.requestVar(frame.ext, false, .{
                            .parent_raw_source_var = frame.common.raw_source_var,
                            .parent_raw_destination_var = frame.common.raw_destination_var,
                            .edge_kind = .tag_extension,
                        })) return false;
                        continue;
                    }
                    // Indexing through the run's start only happens when the
                    // union has tags; start may be undefined when count is 0.
                    const tag = self.store.tags.get(@enumFromInt(@intFromEnum(frame.source_tags.start) + frame.tag_idx));
                    const arrived: u32 = @intCast(machine.value_stack.items.len - frame.vars_base);
                    if (arrived < tag.args.count) {
                        // Indexing through tag.args.start only happens when the
                        // tag has payloads; start may be undefined when count is 0.
                        const arg_var = self.store.vars.items.items[@intFromEnum(tag.args.start) + arrived];
                        if (!try self.requestVar(arg_var, false, .{
                            .parent_raw_source_var = frame.common.raw_source_var,
                            .parent_raw_destination_var = frame.common.raw_destination_var,
                            .edge_kind = .tag_payload,
                            .edge_index = arrived,
                            .edge_name = @bitCast(tag.name),
                        })) return false;
                        continue;
                    }
                    const fresh_args_range = try self.store.appendVars(
                        machine.value_stack.items[frame.vars_base..][0..tag.args.count],
                    );
                    machine.value_stack.items.len = frame.vars_base;
                    try machine.pending_tags.append(self.store.gpa, Tag{
                        .name = tag.name,
                        .args = fresh_args_range,
                    });
                    frame.tag_idx += 1;
                },
                .await_ext => {
                    const fresh_ext = machine.value_stack.pop().?;
                    // A marker resolved open is this union's ext: hand the
                    // post-body audit the tags it sits next to, so its report
                    // can suggest a listed tag for an unlisted near-miss.
                    if (self.opened_marker_exts) |sink| {
                        for (sink.items) |*opened| {
                            if (opened.ext == fresh_ext and opened.listed_tags == null) {
                                opened.listed_tags = frame.tags_range;
                                break;
                            }
                        }
                    }
                    try self.finishFrame(frame.common, Content{ .structure = FlatType{ .tag_union = TagUnion{
                        .tags = frame.tags_range,
                        .ext = fresh_ext,
                    } } });
                    return true;
                },
            }
        }
    }

    pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.getIdentText(idx);
    }

    /// Instantiate every variable-bearing field of a static-dispatch
    /// constraint. `force_root_copy` is used by explicit scheme requirements:
    /// their callable root is anchored below generalized rank by a shared outer
    /// receiver, but the generalized variables below it still need a fresh copy.
    pub fn instantiateStaticDispatchConstraint(
        self: *Self,
        constraint: StaticDispatchConstraint,
        force_root_copy: bool,
    ) std.mem.Allocator.Error!StaticDispatchConstraint {
        if (constraint.where_method_markers.len() != 0 or
            constraint.where_method_marker_bases.len() != 0)
        {
            std.debug.panic("detached marker-bearing constraint instantiation omitted its exact source index", .{});
        }
        var result = constraint;
        result.constraint_evidence = .none;
        result.fn_var = try self.instantiateVarHelp(constraint.fn_var, force_root_copy);
        result.interpolation = try self.instantiateInterpolationMetadata(constraint.interpolation, null);
        return result;
    }

    pub fn instantiateStaticDispatchConstraintAt(
        self: *Self,
        source_constraint_index: u32,
        constraint: StaticDispatchConstraint,
        force_root_copy: bool,
    ) std.mem.Allocator.Error!StaticDispatchConstraint {
        var result = constraint;
        result.constraint_evidence = .none;
        result.fn_var = try self.instantiateVarHelp(constraint.fn_var, force_root_copy);
        result.interpolation = try self.instantiateInterpolationMetadata(constraint.interpolation, null);
        const marker_metadata = try self.copyWhereMethodMarkerMetadata(
            source_constraint_index,
            constraint,
        );
        result.where_method_markers = marker_metadata.markers;
        result.where_method_marker_bases = marker_metadata.bases;
        return result;
    }

    /// Copy one detached scheme-requirement receiver as an explicit virtual
    /// edge from the scheme root. Requirement ordinals are assigned by the
    /// scheme producer and remain stable when the copied component is not
    /// structurally reachable from that root.
    pub fn instantiateSchemeRequirementReceiver(
        self: *Self,
        root: ProofRootPair,
        requirement_ordinal: u32,
        receiver_var: Var,
        force_root_copy: bool,
    ) std.mem.Allocator.Error!Var {
        return self.instantiateVarHelpFromEdge(receiver_var, force_root_copy, .{
            .parent_raw_source_var = root.source_var,
            .parent_raw_destination_var = root.destination_var,
            .edge_kind = .scheme_requirement_receiver,
            .edge_index = requirement_ordinal,
            .action_override = .requirement_component_ingress,
        });
    }

    /// Copy all variable-bearing components of one detached requirement. The
    /// function, interpolation parts, and interpolation item are independent
    /// virtual root edges authenticated by the same exact constraint-pair row.
    pub fn instantiateSchemeRequirementConstraintAt(
        self: *Self,
        root: ProofRootPair,
        requirement_ordinal: u32,
        source_constraint_index: u32,
        constraint: StaticDispatchConstraint,
        force_root_copy: bool,
    ) std.mem.Allocator.Error!StaticDispatchConstraint {
        var result = constraint;
        result.constraint_evidence = .none;
        result.fn_var = try self.instantiateVarHelpFromEdge(constraint.fn_var, force_root_copy, .{
            .parent_raw_source_var = root.source_var,
            .parent_raw_destination_var = root.destination_var,
            .edge_kind = .scheme_requirement_function,
            .edge_index = requirement_ordinal,
            .constraint_source_index = source_constraint_index,
            .action_override = .requirement_component_ingress,
        });
        result.interpolation = try self.instantiateInterpolationMetadata(constraint.interpolation, .{
            .root = root,
            .source_constraint_index = source_constraint_index,
        });
        const marker_metadata = try self.copyWhereMethodMarkerMetadata(
            source_constraint_index,
            constraint,
        );
        result.where_method_markers = marker_metadata.markers;
        result.where_method_marker_bases = marker_metadata.bases;
        return result;
    }

    const DetachedConstraintProof = struct {
        root: ProofRootPair,
        source_constraint_index: u32,
    };

    fn instantiateInterpolationMetadata(
        self: *Self,
        metadata: StaticDispatchConstraint.InterpolationMetadata,
        detached: ?DetachedConstraintProof,
    ) std.mem.Allocator.Error!StaticDispatchConstraint.InterpolationMetadata {
        if (!metadata.isPresent()) return metadata;

        const machine = self.scratch();
        const parts_len = metadata.interpolated_parts.len();
        const parts_base = machine.pending_parts.items.len;
        // The store-owned buffer outlives this call, so a failure part-way
        // through the run must not leave the collected parts on it.
        errdefer machine.pending_parts.items.len = parts_base;
        for (0..parts_len) |i| {
            const part = self.store.getInterpolationPartAt(metadata.interpolated_parts, @intCast(i));
            const fresh_part_var = if (detached) |proof|
                try self.instantiateVarHelpFromEdge(part.var_, false, .{
                    .parent_raw_source_var = proof.root.source_var,
                    .parent_raw_destination_var = proof.root.destination_var,
                    .edge_kind = .interpolation_part,
                    .edge_index = @intCast(i),
                    .constraint_source_index = proof.source_constraint_index,
                    .action_override = .requirement_component_ingress,
                })
            else
                try self.instantiateVarHelp(part.var_, false);
            try machine.pending_parts.append(self.store.gpa, .{
                .var_ = fresh_part_var,
                .region = part.region,
            });
        }

        const fresh_item_var = if (detached) |proof|
            try self.instantiateVarHelpFromEdge(metadata.item_var, false, .{
                .parent_raw_source_var = proof.root.source_var,
                .parent_raw_destination_var = proof.root.destination_var,
                .edge_kind = .interpolation_item,
                .constraint_source_index = proof.source_constraint_index,
                .action_override = .requirement_component_ingress,
            })
        else
            try self.instantiateVarHelp(metadata.item_var, false);
        const fresh_parts_range = try self.store.appendInterpolationParts(machine.pending_parts.items[parts_base..]);
        machine.pending_parts.items.len = parts_base;
        return .{
            .expr_region = metadata.expr_region,
            .item_var = fresh_item_var,
            .interpolated_parts = fresh_parts_range,
        };
    }
};

const ProofTestEnv = struct {
    const Self = @This();

    gpa: std.mem.Allocator,
    store: TypesStore,
    idents: Ident.Store,
    var_map: std.AutoHashMap(Var, Var),
    proof_pairs: std.ArrayListUnmanaged(ProofPair) = .empty,
    proof_constraint_pairs: std.ArrayListUnmanaged(ProofConstraintPair) = .empty,
    proof_witnesses: std.ArrayListUnmanaged(ProofWitness) = .empty,
    preexisting_rigid_substitutions: PreexistingRigidSubstitutions = .empty,

    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        var store = try TypesStore.initCapacity(gpa, 16, 8);
        errdefer store.deinit();
        var idents = try Ident.Store.initCapacity(gpa, 16);
        errdefer idents.deinit(gpa);
        return .{
            .gpa = gpa,
            .store = store,
            .idents = idents,
            .var_map = std.AutoHashMap(Var, Var).init(gpa),
        };
    }

    fn deinit(self: *Self) void {
        self.preexisting_rigid_substitutions.deinit(self.gpa);
        self.proof_witnesses.deinit(self.gpa);
        self.proof_constraint_pairs.deinit(self.gpa);
        self.proof_pairs.deinit(self.gpa);
        self.var_map.deinit();
        self.idents.deinit(self.gpa);
        self.store.deinit();
    }

    fn instantiator(self: *Self, rigid_behavior: Instantiator.RigidBehavior) Instantiator {
        return .{
            .store = &self.store,
            .idents = &self.idents,
            .var_map = &self.var_map,
            .proof_pairs = &self.proof_pairs,
            .proof_constraint_pairs = &self.proof_constraint_pairs,
            .proof_witnesses = &self.proof_witnesses,
            .preexisting_rigid_substitutions = &self.preexisting_rigid_substitutions,
            .current_rank = .outermost,
            .rigid_behavior = rigid_behavior,
        };
    }
};

fn expectSingleRootProofSince(
    env: *ProofTestEnv,
    pair_base: usize,
    constraint_pair_base: usize,
    witness_base: usize,
    source: Var,
    destination: Var,
    action: ProofWitnessAction,
    auxiliary_origin_kind: ProofWitnessAuxiliaryOriginKind,
    auxiliary_origin_index: u32,
    carries_raw_cut: bool,
) !void {
    try std.testing.expectEqual(pair_base + 1, env.proof_pairs.items.len);
    try std.testing.expectEqual(constraint_pair_base, env.proof_constraint_pairs.items.len);
    try std.testing.expectEqual(witness_base + 1, env.proof_witnesses.items.len);

    const source_raw: u32 = @intFromEnum(source);
    const destination_raw: u32 = @intFromEnum(destination);
    const canonical_source: u32 = @intFromEnum(env.store.resolveVar(source).var_);
    const canonical_destination: u32 = @intFromEnum(env.store.resolveVar(destination).var_);
    const pair = env.proof_pairs.items[pair_base];
    try std.testing.expectEqual(source_raw, pair.raw_source_var);
    try std.testing.expectEqual(destination_raw, pair.raw_destination_var);
    try std.testing.expectEqual(canonical_source, pair.canonical_source_var);
    try std.testing.expectEqual(canonical_destination, pair.canonical_destination_var);

    const witness = env.proof_witnesses.items[witness_base];
    try std.testing.expectEqual(ProofWitnessEdgeKind.root_copy_action, witness.edge_kind);
    try std.testing.expectEqual(@as(u32, 0), witness.edge_index);
    try std.testing.expectEqual(@as(u32, 0), witness.edge_name);
    try std.testing.expectEqual(@as(u32, 0), witness.edge_origin_module);
    try std.testing.expectEqual(@as(u32, 0), witness.edge_source_decl);
    try std.testing.expectEqual(std.math.maxInt(u32), witness.constraint_source_index);
    try std.testing.expectEqual(source_raw, witness.parent_raw_source_var);
    try std.testing.expectEqual(destination_raw, witness.parent_raw_destination_var);
    try std.testing.expectEqual(source_raw, witness.child_raw_source_var);
    try std.testing.expectEqual(destination_raw, witness.child_raw_destination_var);
    try std.testing.expectEqual(action, witness.action);
    try std.testing.expectEqual(auxiliary_origin_kind, witness.auxiliary_origin_kind);
    try std.testing.expectEqual(auxiliary_origin_index, witness.auxiliary_origin_index);
    try std.testing.expectEqual(
        if (carries_raw_cut) source_raw else std.math.maxInt(u32),
        witness.raw_source_var,
    );
    try std.testing.expectEqual(
        if (carries_raw_cut) destination_raw else std.math.maxInt(u32),
        witness.raw_destination_var,
    );
}

test "instantiator root proof: zero-outgoing structural leaf emits one traverse root action" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const source = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    var inst = env.instantiator(.fresh_flex);
    const destination = try inst.instantiateVar(source);

    try expectSingleRootProofSince(
        &env,
        0,
        0,
        0,
        source,
        destination,
        .traverse,
        .none,
        0,
        false,
    );
}

test "instantiator root proof: raw cuts emit one typed root action without traverse" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    var inst = env.instantiator(.fresh_flex);

    const shared_source = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    var pair_base = env.proof_pairs.items.len;
    var constraint_pair_base = env.proof_constraint_pairs.items.len;
    var witness_base = env.proof_witnesses.items.len;
    const shared_destination = try inst.instantiateVar(shared_source);
    try expectSingleRootProofSince(
        &env,
        pair_base,
        constraint_pair_base,
        witness_base,
        shared_source,
        shared_destination,
        .local_raw_identity_share_cut,
        .none,
        0,
        true,
    );

    const fresh_name = try env.idents.insert(gpa, Ident.for_text("fresh"));
    const fresh_flex_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(fresh_name) },
        .generalized,
    );
    pair_base = env.proof_pairs.items.len;
    constraint_pair_base = env.proof_constraint_pairs.items.len;
    witness_base = env.proof_witnesses.items.len;
    const fresh_flex_destination = try inst.instantiateVar(fresh_flex_source);
    try std.testing.expect(env.preexisting_rigid_substitutions.get(fresh_flex_source) == null);
    try expectSingleRootProofSince(
        &env,
        pair_base,
        constraint_pair_base,
        witness_base,
        fresh_flex_source,
        fresh_flex_destination,
        .rigid_fresh_flex_cut,
        .none,
        0,
        true,
    );

    inst.rigid_behavior = .fresh_rigid;
    const fresh_rigid_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(fresh_name) },
        .generalized,
    );
    pair_base = env.proof_pairs.items.len;
    constraint_pair_base = env.proof_constraint_pairs.items.len;
    witness_base = env.proof_witnesses.items.len;
    const fresh_rigid_destination = try inst.instantiateVar(fresh_rigid_source);
    try expectSingleRootProofSince(
        &env,
        pair_base,
        constraint_pair_base,
        witness_base,
        fresh_rigid_source,
        fresh_rigid_destination,
        .rigid_fresh_rigid_cut,
        .none,
        0,
        true,
    );

    const substitution_name = try env.idents.insert(gpa, Ident.for_text("substitution"));
    const substitution_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(substitution_name) },
        .generalized,
    );
    const substitution_destination = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    var substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){};
    defer substitutions.deinit(gpa);
    try substitutions.put(gpa, substitution_name, substitution_destination);
    var substitution_origins = std.AutoHashMapUnmanaged(Ident.Idx, u32){};
    defer substitution_origins.deinit(gpa);
    try substitution_origins.put(gpa, substitution_name, 17);
    inst.rigid_behavior = .{ .substitute_rigids = &substitutions };
    inst.rigid_substitution_origins = &substitution_origins;

    pair_base = env.proof_pairs.items.len;
    constraint_pair_base = env.proof_constraint_pairs.items.len;
    witness_base = env.proof_witnesses.items.len;
    const substituted_destination = try inst.instantiateVar(substitution_source);
    try std.testing.expectEqual(substitution_destination, substituted_destination);
    try std.testing.expectEqual(
        substituted_destination,
        env.preexisting_rigid_substitutions.get(substitution_source).?,
    );
    try expectSingleRootProofSince(
        &env,
        pair_base,
        constraint_pair_base,
        witness_base,
        substitution_source,
        substituted_destination,
        .exact_annotation_substitution_cut,
        .annotation_substitution,
        17,
        true,
    );
}

test "instantiator root proof: structural root with an outgoing witness emits no root action" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const source_child = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const source_elems = try env.store.appendVars(&.{source_child});
    const source = try env.store.freshFromContentWithRank(
        .{ .structure = .{ .tuple = .{ .elems = source_elems } } },
        .generalized,
    );
    var inst = env.instantiator(.fresh_flex);
    const destination = try inst.instantiateVar(source);

    try std.testing.expectEqual(@as(usize, 2), env.proof_pairs.items.len);
    try std.testing.expectEqual(@as(usize, 0), env.proof_constraint_pairs.items.len);
    try std.testing.expectEqual(@as(usize, 1), env.proof_witnesses.items.len);

    const destination_tuple = env.store.resolveVar(destination).desc.content.structure.tuple;
    const destination_child = env.store.sliceVars(destination_tuple.elems)[0];
    const witness = env.proof_witnesses.items[0];
    try std.testing.expectEqual(ProofWitnessEdgeKind.tuple_element, witness.edge_kind);
    try std.testing.expectEqual(ProofWitnessAction.traverse, witness.action);
    try std.testing.expectEqual(@as(u32, 0), witness.edge_index);
    try std.testing.expectEqual(@intFromEnum(source), witness.parent_raw_source_var);
    try std.testing.expectEqual(@intFromEnum(destination), witness.parent_raw_destination_var);
    try std.testing.expectEqual(@intFromEnum(source_child), witness.child_raw_source_var);
    try std.testing.expectEqual(@intFromEnum(destination_child), witness.child_raw_destination_var);
    try std.testing.expectEqual(ProofWitnessAuxiliaryOriginKind.none, witness.auxiliary_origin_kind);
    try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_source_var);
    try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_destination_var);
}

test "instantiator proof: root selection distinguishes direct copies and redirected identity shares" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    var selection: ?ProofRootSelection = null;
    var inst = env.instantiator(.fresh_flex);
    inst.proof_root_selection = &selection;

    const leaf_source = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const leaf_destination = try inst.instantiateVar(leaf_source);
    const leaf_selection = selection orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(leaf_source, leaf_selection.source_var);
    try std.testing.expectEqual(leaf_destination, leaf_selection.destination_var);
    try std.testing.expectEqual(ProofWitnessAction.traverse, leaf_selection.action);

    const detached_source = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .generalized,
    );
    _ = try inst.instantiateSchemeRequirementReceiver(
        .{ .source_var = leaf_source, .destination_var = leaf_destination },
        3,
        detached_source,
        false,
    );
    try std.testing.expect(std.meta.eql(leaf_selection, selection.?));

    selection = null;
    const structural_child = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const structural_elems = try env.store.appendVars(&.{structural_child});
    const structural_source = try env.store.freshFromContentWithRank(
        .{ .structure = .{ .tuple = .{ .elems = structural_elems } } },
        .generalized,
    );
    const structural_destination = try inst.instantiateVar(structural_source);
    const structural_selection = selection orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(structural_source, structural_selection.source_var);
    try std.testing.expectEqual(structural_destination, structural_selection.destination_var);
    try std.testing.expectEqual(ProofWitnessAction.traverse, structural_selection.action);

    selection = null;
    const direct_shared = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    try std.testing.expectEqual(direct_shared, try inst.instantiateVar(direct_shared));
    const direct_selection = selection orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(direct_shared, direct_selection.source_var);
    try std.testing.expectEqual(direct_shared, direct_selection.destination_var);
    try std.testing.expectEqual(
        ProofWitnessAction.local_raw_identity_share_cut,
        direct_selection.action,
    );

    selection = null;
    const redirected_request = try env.store.freshRedirect(direct_shared);
    try std.testing.expectEqual(direct_shared, try inst.instantiateVar(redirected_request));
    const redirected_selection = selection orelse return error.TestUnexpectedResult;
    try std.testing.expect(redirected_request != redirected_selection.source_var);
    try std.testing.expectEqual(direct_shared, redirected_selection.source_var);
    try std.testing.expectEqual(direct_shared, redirected_selection.destination_var);
    try std.testing.expectEqual(
        ProofWitnessAction.local_raw_identity_share_cut,
        redirected_selection.action,
    );
}

test "instantiator proof: root selection clears on allocation failure" {
    const gpa = std.testing.allocator;
    var observed_failure = false;
    var observed_success = false;
    for (0..64) |fail_index| {
        var env = try ProofTestEnv.init(gpa);
        defer env.deinit();

        const source_child = try env.store.freshFromContentWithRank(
            .{ .structure = .empty_record },
            .generalized,
        );
        const source_elems = try env.store.appendVars(&.{source_child});
        const source = try env.store.freshFromContentWithRank(
            .{ .structure = .{ .tuple = .{ .elems = source_elems } } },
            .generalized,
        );
        var selection: ?ProofRootSelection = null;
        var inst = env.instantiator(.fresh_flex);
        inst.proof_root_selection = &selection;

        var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = fail_index });
        const original_store_gpa = env.store.gpa;
        env.store.gpa = failing.allocator();
        const result = inst.instantiateVar(source);
        env.store.gpa = original_store_gpa;
        if (result) |destination| {
            try std.testing.expect(!failing.has_induced_failure);
            const selected = selection orelse return error.TestUnexpectedResult;
            try std.testing.expectEqual(destination, selected.destination_var);
            observed_success = true;
            break;
        } else |err| switch (err) {
            error.OutOfMemory => {
                try std.testing.expect(failing.has_induced_failure);
                try std.testing.expectEqual(@as(?ProofRootSelection, null), selection);
                observed_failure = true;
            },
        }
    }
    try std.testing.expect(observed_failure);
    try std.testing.expect(observed_success);
}

fn expectConstrainedSharedChildTerminatesProof(
    source_rank: Rank,
    share_leaves: bool,
) !void {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const method_name = try env.idents.insert(gpa, Ident.for_text("shared_method"));
    const source_callable = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const source_constraints = try env.store.appendStaticDispatchConstraints(&.{.{
        .fn_name = method_name,
        .fn_var = source_callable,
        .origin = .method_call,
    }});
    const source_constraint_index: u32 = @intFromEnum(source_constraints.start);
    const shared_child = try env.store.freshFromContentWithRank(
        .{ .flex = Flex{ .name = null, .constraints = source_constraints } },
        source_rank,
    );
    const source_elems = try env.store.appendVars(&.{shared_child});
    const source_root = try env.store.freshFromContentWithRank(
        .{ .structure = .{ .tuple = .{ .elems = source_elems } } },
        .generalized,
    );

    var inst = env.instantiator(.fresh_flex);
    inst.share_leaves = share_leaves;
    const destination_root = try inst.instantiateVar(source_root);
    const destination_tuple = env.store.resolveVar(destination_root).desc.content.structure.tuple;
    const destination_child = env.store.sliceVars(destination_tuple.elems)[0];

    // The terminating cut owns the exact raw child occurrence, but did not
    // traverse or append the constraint still attached to that shared var.
    try std.testing.expectEqual(shared_child, destination_child);
    try std.testing.expectEqual(@as(usize, 0), env.proof_constraint_pairs.items.len);
    try std.testing.expectEqual(
        source_constraint_index,
        @intFromEnum(env.store.resolveVar(shared_child).desc.content.flex.constraints.start),
    );
    var shared_cut_count: usize = 0;
    var constraint_function_count: usize = 0;
    for (env.proof_witnesses.items) |witness| {
        if (witness.edge_kind == .static_dispatch_function) constraint_function_count += 1;
        if (witness.edge_kind != .tuple_element or
            witness.action != .local_raw_identity_share_cut)
        {
            continue;
        }
        shared_cut_count += 1;
        try std.testing.expectEqual(@intFromEnum(shared_child), witness.child_raw_source_var);
        try std.testing.expectEqual(@intFromEnum(shared_child), witness.child_raw_destination_var);
        try std.testing.expectEqual(@intFromEnum(shared_child), witness.raw_source_var);
        try std.testing.expectEqual(@intFromEnum(shared_child), witness.raw_destination_var);
    }
    try std.testing.expectEqual(@as(usize, 1), shared_cut_count);
    try std.testing.expectEqual(@as(usize, 0), constraint_function_count);
}

test "instantiator proof: explicit leaf sharing terminates before attached constraints" {
    try expectConstrainedSharedChildTerminatesProof(.generalized, true);
}

test "instantiator proof: rank sharing terminates before attached constraints" {
    try expectConstrainedSharedChildTerminatesProof(.outermost, false);
}

test "instantiator proof: fresh constrained copy and memoized revisit retain one functional pair" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const method_name = try env.idents.insert(gpa, Ident.for_text("copied_method"));
    const source_callable = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const source_constraints = try env.store.appendStaticDispatchConstraints(&.{.{
        .fn_name = method_name,
        .fn_var = source_callable,
        .origin = .method_call,
    }});
    const source_constraint_index: u32 = @intFromEnum(source_constraints.start);
    const source_child = try env.store.freshFromContentWithRank(
        .{ .flex = Flex{ .name = null, .constraints = source_constraints } },
        .generalized,
    );
    const source_elems = try env.store.appendVars(&.{ source_child, source_child });
    const source_root = try env.store.freshFromContentWithRank(
        .{ .structure = .{ .tuple = .{ .elems = source_elems } } },
        .generalized,
    );

    var inst = env.instantiator(.fresh_flex);
    const destination_root = try inst.instantiateVar(source_root);
    const destination_tuple = env.store.resolveVar(destination_root).desc.content.structure.tuple;
    const destination_elems = env.store.sliceVars(destination_tuple.elems);
    try std.testing.expectEqual(destination_elems[0], destination_elems[1]);
    try std.testing.expect(destination_elems[0] != source_child);

    try std.testing.expectEqual(@as(usize, 1), env.proof_constraint_pairs.items.len);
    const constraint_pair = env.proof_constraint_pairs.items[0];
    try std.testing.expectEqual(source_constraint_index, constraint_pair.source_constraint_index);
    try std.testing.expect(constraint_pair.destination_constraint_index != source_constraint_index);

    var primary_function_count: usize = 0;
    var first_flex_copy_count: usize = 0;
    var memoized_revisit_count: usize = 0;
    for (env.proof_witnesses.items) |witness| {
        if (witness.edge_kind == .tuple_element and witness.edge_index == 0) {
            try std.testing.expectEqual(ProofWitnessAction.flex_fresh_flex_copy, witness.action);
            try std.testing.expectEqual(ProofWitnessAuxiliaryOriginKind.none, witness.auxiliary_origin_kind);
            try std.testing.expectEqual(@as(u32, 0), witness.auxiliary_origin_index);
            try std.testing.expectEqual(@intFromEnum(source_child), witness.raw_source_var);
            try std.testing.expectEqual(@intFromEnum(destination_elems[0]), witness.raw_destination_var);
            try std.testing.expect(witness.raw_source_var != witness.raw_destination_var);
            try std.testing.expectEqual(witness.child_raw_source_var, witness.raw_source_var);
            try std.testing.expectEqual(witness.child_raw_destination_var, witness.raw_destination_var);
            first_flex_copy_count += 1;
        }
        if (witness.edge_kind == .tuple_element and witness.edge_index == 1) {
            try std.testing.expectEqual(ProofWitnessAction.traverse, witness.action);
            try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_source_var);
            try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_destination_var);
            try std.testing.expectEqual(@intFromEnum(source_child), witness.child_raw_source_var);
            try std.testing.expectEqual(@intFromEnum(destination_elems[1]), witness.child_raw_destination_var);
            memoized_revisit_count += 1;
        }
        if (witness.edge_kind != .static_dispatch_function or
            witness.constraint_source_index != source_constraint_index)
        {
            continue;
        }
        primary_function_count += 1;
        const destination_constraint = env.store.static_dispatch_constraints.items.items[
            constraint_pair.destination_constraint_index
        ];
        try std.testing.expectEqual(
            @intFromEnum(destination_constraint.fn_var),
            witness.child_raw_destination_var,
        );
    }
    try std.testing.expectEqual(@as(usize, 1), first_flex_copy_count);
    try std.testing.expectEqual(@as(usize, 1), memoized_revisit_count);
    try std.testing.expectEqual(@as(usize, 1), primary_function_count);
}

test "instantiator proof: virtual fresh flex ingresses retain creation and memoized roles" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const attached_name = try env.idents.insert(gpa, Ident.for_text("attached"));
    const detached_name = try env.idents.insert(gpa, Ident.for_text("detached"));
    const rigid_name = try env.idents.insert(gpa, Ident.for_text("callable"));
    const attached_callable = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const attached_constraints = try env.store.appendStaticDispatchConstraints(&.{.{
        .fn_name = attached_name,
        .fn_var = attached_callable,
        .origin = .method_call,
    }});
    const source_receiver = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init().withConstraints(attached_constraints) },
        .generalized,
    );
    const source_function = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(rigid_name) },
        .generalized,
    );
    const source_part = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .generalized,
    );
    const source_item = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .generalized,
    );
    const source_parts = try env.store.appendInterpolationParts(&.{.{
        .var_ = source_part,
        .region = base.Region.zero(),
    }});
    const detached_constraints = try env.store.appendStaticDispatchConstraints(&.{.{
        .fn_name = detached_name,
        .fn_var = source_function,
        .origin = .method_call,
        .interpolation = .{
            .expr_region = StaticDispatchConstraint.OptRegion.some(base.Region.zero()),
            .item_var = source_item,
            .interpolated_parts = source_parts,
        },
    }});
    const detached_constraint_index: u32 = @intFromEnum(detached_constraints.start);
    const detached_constraint = env.store.getStaticDispatchConstraintAt(detached_constraint_index);
    const source_root = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );

    var inst = env.instantiator(.fresh_flex);
    const destination_root = try inst.instantiateVar(source_root);
    const proof_root = ProofRootPair{
        .source_var = source_root,
        .destination_var = destination_root,
    };
    const destination_receiver = try inst.instantiateSchemeRequirementReceiver(
        proof_root,
        7,
        source_receiver,
        false,
    );
    const destination_constraint = try inst.instantiateSchemeRequirementConstraintAt(
        proof_root,
        7,
        detached_constraint_index,
        detached_constraint,
        true,
    );
    const memoized_receiver = try inst.instantiateSchemeRequirementReceiver(
        proof_root,
        8,
        source_receiver,
        false,
    );
    try std.testing.expectEqual(destination_receiver, memoized_receiver);

    const expected_edges = [_]struct {
        kind: ProofWitnessEdgeKind,
        index: u32,
        source: Var,
        destination: Var,
    }{
        .{ .kind = .scheme_requirement_receiver, .index = 7, .source = source_receiver, .destination = destination_receiver },
        .{ .kind = .scheme_requirement_function, .index = 7, .source = source_function, .destination = destination_constraint.fn_var },
        .{ .kind = .interpolation_part, .index = 0, .source = source_part, .destination = env.store.getInterpolationPartAt(destination_constraint.interpolation.interpolated_parts, 0).var_ },
        .{ .kind = .interpolation_item, .index = 0, .source = source_item, .destination = destination_constraint.interpolation.item_var },
    };
    for (expected_edges) |expected| {
        var matches: usize = 0;
        for (env.proof_witnesses.items) |witness| {
            if (witness.edge_kind != expected.kind or
                witness.edge_index != expected.index or
                witness.action != .requirement_component_fresh_flex_copy)
            {
                continue;
            }
            matches += 1;
            try std.testing.expectEqual(@intFromEnum(source_root), witness.parent_raw_source_var);
            try std.testing.expectEqual(@intFromEnum(destination_root), witness.parent_raw_destination_var);
            try std.testing.expectEqual(@intFromEnum(expected.source), witness.child_raw_source_var);
            try std.testing.expectEqual(@intFromEnum(expected.destination), witness.child_raw_destination_var);
            try std.testing.expectEqual(witness.child_raw_source_var, witness.raw_source_var);
            try std.testing.expectEqual(witness.child_raw_destination_var, witness.raw_destination_var);
            try std.testing.expect(witness.raw_source_var != witness.raw_destination_var);
            try std.testing.expectEqual(ProofWitnessAuxiliaryOriginKind.none, witness.auxiliary_origin_kind);
            try std.testing.expectEqual(@as(u32, 0), witness.auxiliary_origin_index);
        }
        try std.testing.expectEqual(@as(usize, 1), matches);
    }

    var memoized_matches: usize = 0;
    for (env.proof_witnesses.items) |witness| {
        if (witness.edge_kind != .scheme_requirement_receiver or witness.edge_index != 8) continue;
        memoized_matches += 1;
        try std.testing.expectEqual(ProofWitnessAction.requirement_component_ingress, witness.action);
        try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_source_var);
        try std.testing.expectEqual(std.math.maxInt(u32), witness.raw_destination_var);
    }
    try std.testing.expectEqual(@as(usize, 1), memoized_matches);
}

test "instantiator proof: forced shared flex root records creation beside constraint witnesses" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const action_values = std.enums.values(ProofWitnessAction);
    try std.testing.expectEqual(
        @as(u32, @intCast(action_values.len - 1)),
        @intFromEnum(ProofWitnessAction.requirement_component_fresh_flex_copy),
    );

    const method_name = try env.idents.insert(gpa, Ident.for_text("forced_root_method"));
    const source_callable = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const source_constraints = try env.store.appendStaticDispatchConstraints(&.{.{
        .fn_name = method_name,
        .fn_var = source_callable,
        .origin = .method_call,
    }});
    const source = try env.store.freshFromContentWithRank(
        .{ .flex = Flex{ .name = null, .constraints = source_constraints } },
        .generalized,
    );

    var inst = env.instantiator(.fresh_flex);
    inst.share_leaves = true;
    const shared = try inst.instantiateVar(source);
    try std.testing.expectEqual(source, shared);
    try expectSingleRootProofSince(
        &env,
        0,
        0,
        0,
        source,
        shared,
        .local_raw_identity_share_cut,
        .none,
        0,
        true,
    );

    const pair_base = env.proof_pairs.items.len;
    const constraint_pair_base = env.proof_constraint_pairs.items.len;
    const witness_base = env.proof_witnesses.items.len;
    const copied = try inst.instantiateTypeScheme(source);
    try std.testing.expect(copied != source);
    try std.testing.expectEqual(constraint_pair_base + 1, env.proof_constraint_pairs.items.len);

    var root_creation_count: usize = 0;
    var constraint_edge_count: usize = 0;
    for (env.proof_witnesses.items[witness_base..]) |witness| {
        switch (witness.edge_kind) {
            .root_copy_action => {
                try std.testing.expectEqual(ProofWitnessAction.flex_fresh_flex_copy, witness.action);
                try std.testing.expectEqual(@intFromEnum(source), witness.parent_raw_source_var);
                try std.testing.expectEqual(@intFromEnum(copied), witness.parent_raw_destination_var);
                try std.testing.expectEqual(witness.parent_raw_source_var, witness.child_raw_source_var);
                try std.testing.expectEqual(witness.parent_raw_destination_var, witness.child_raw_destination_var);
                try std.testing.expectEqual(witness.child_raw_source_var, witness.raw_source_var);
                try std.testing.expectEqual(witness.child_raw_destination_var, witness.raw_destination_var);
                try std.testing.expect(witness.raw_source_var != witness.raw_destination_var);
                try std.testing.expectEqual(ProofWitnessAuxiliaryOriginKind.none, witness.auxiliary_origin_kind);
                try std.testing.expectEqual(@as(u32, 0), witness.auxiliary_origin_index);
                root_creation_count += 1;
            },
            .static_dispatch_function => {
                try std.testing.expectEqual(ProofWitnessAction.traverse, witness.action);
                try std.testing.expectEqual(
                    @intFromEnum(source_constraints.start),
                    witness.constraint_source_index,
                );
                constraint_edge_count += 1;
            },
            else => {},
        }
    }
    try std.testing.expect(env.proof_pairs.items.len > pair_base);
    try std.testing.expectEqual(@as(usize, 1), root_creation_count);
    try std.testing.expectEqual(@as(usize, 1), constraint_edge_count);
}

test "instantiator caller-owned substitution ledger is authored only by exact cuts" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const exact_name = try env.idents.insert(gpa, Ident.for_text("exact"));
    const fresh_mode_name = try env.idents.insert(gpa, Ident.for_text("fresh_mode"));
    const marker_name = try env.idents.insert(gpa, Ident.for_text("#polarity"));
    const anonymous_ext_name = try env.idents.insert(gpa, Ident.for_text("#others"));

    const exact_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(exact_name) },
        .generalized,
    );
    const exact_destination = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    const fresh_mode_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(fresh_mode_name) },
        .generalized,
    );
    const fresh_mode_destination = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    const marker_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(marker_name) },
        .generalized,
    );
    const marker_seed = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );
    const anonymous_ext_source = try env.store.freshFromContentWithRank(
        .{ .rigid = Rigid.init(anonymous_ext_name) },
        .generalized,
    );
    const anonymous_ext_seed = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .outermost,
    );

    var exact_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){};
    defer exact_substitutions.deinit(gpa);
    try exact_substitutions.put(gpa, exact_name, exact_destination);
    try exact_substitutions.put(gpa, marker_name, marker_seed);
    try exact_substitutions.put(gpa, anonymous_ext_name, anonymous_ext_seed);
    var exact_origins = std.AutoHashMapUnmanaged(Ident.Idx, u32){};
    defer exact_origins.deinit(gpa);
    try exact_origins.put(gpa, exact_name, 1);

    var inst = env.instantiator(.{ .substitute_rigids = &exact_substitutions });
    inst.rigid_substitution_origins = &exact_origins;
    inst.polarity_var_ident = marker_name;
    inst.anonymous_ext_ident = anonymous_ext_name;

    try std.testing.expectEqual(exact_destination, try inst.instantiateVar(exact_source));
    try std.testing.expectEqual(
        exact_destination,
        env.preexisting_rigid_substitutions.get(exact_source).?,
    );

    const marker_destination = try inst.instantiateVar(marker_source);
    try std.testing.expect(marker_destination != marker_seed);
    try std.testing.expect(env.preexisting_rigid_substitutions.get(marker_source) == null);

    const anonymous_ext_destination = try inst.instantiateVar(anonymous_ext_source);
    try std.testing.expect(anonymous_ext_destination != anonymous_ext_seed);
    try std.testing.expect(env.preexisting_rigid_substitutions.get(anonymous_ext_source) == null);

    var fresh_substitutions = std.AutoHashMapUnmanaged(Ident.Idx, Var){};
    defer fresh_substitutions.deinit(gpa);
    try fresh_substitutions.put(gpa, fresh_mode_name, fresh_mode_destination);
    var fresh_origins = std.AutoHashMapUnmanaged(Ident.Idx, u32){};
    defer fresh_origins.deinit(gpa);
    try fresh_origins.put(gpa, fresh_mode_name, 2);
    inst.rigid_behavior = .{ .substitute_rigids_fresh = &fresh_substitutions };
    inst.rigid_substitution_origins = &fresh_origins;

    try std.testing.expectEqual(fresh_mode_destination, try inst.instantiateVar(fresh_mode_source));
    try std.testing.expectEqual(
        fresh_mode_destination,
        env.preexisting_rigid_substitutions.get(fresh_mode_source).?,
    );
    try std.testing.expectEqual(@as(u32, 2), env.preexisting_rigid_substitutions.count());
}

test "instantiator proof: redirected record field identity cut keeps raw identity and semantic edge" {
    const gpa = std.testing.allocator;
    var env = try ProofTestEnv.init(gpa);
    defer env.deinit();

    const shared_field_var = try env.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        .generalized,
    );
    const redirected_field_var = try env.store.freshRedirect(shared_field_var);
    const field_name = try env.idents.insert(gpa, Ident.for_text("redirected_field"));
    const fields = try env.store.appendRecordFields(&.{.{
        .name = field_name,
        .presence = .required(redirected_field_var),
    }});
    const ext = try env.store.freshFromContentWithRank(
        .{ .structure = .empty_record },
        .generalized,
    );
    const source = try env.store.freshFromContentWithRank(
        .{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } },
        .generalized,
    );

    var inst = env.instantiator(.fresh_flex);
    inst.share_leaves = true;
    const destination = try inst.instantiateVar(source);

    var identity_cut_count: usize = 0;
    for (env.proof_witnesses.items) |witness| {
        if (witness.action != .local_raw_identity_share_cut) continue;
        identity_cut_count += 1;

        // A redirected request still cuts at the resolved shared occurrence;
        // final canonical equality is not recorded as raw copy-time identity.
        try std.testing.expectEqual(witness.raw_source_var, witness.raw_destination_var);
        try std.testing.expectEqual(@intFromEnum(shared_field_var), witness.raw_source_var);
        try std.testing.expectEqual(witness.raw_source_var, witness.child_raw_source_var);
        try std.testing.expectEqual(witness.raw_destination_var, witness.child_raw_destination_var);
        try std.testing.expect(witness.raw_source_var != @intFromEnum(redirected_field_var));

        // The cut endpoints are raw identity, while the enclosing edge still
        // says exactly which source/destination record field requested it.
        try std.testing.expectEqual(ProofWitnessEdgeKind.record_field_type, witness.edge_kind);
        try std.testing.expectEqual(@as(u32, @bitCast(field_name)), witness.edge_name);
        try std.testing.expectEqual(@intFromEnum(source), witness.parent_raw_source_var);
        try std.testing.expectEqual(@intFromEnum(destination), witness.parent_raw_destination_var);
    }
    try std.testing.expectEqual(@as(usize, 1), identity_cut_count);
}
