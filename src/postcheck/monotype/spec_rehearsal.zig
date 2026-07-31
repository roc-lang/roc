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

/// One side of a body-lowering constraint-replay site, described in the terms
/// directed translation reads (reunify.md sections 9, 13 Slice 7): a checked
/// position the site instantiates into the graph, or an immutable type the site
/// imports into it. A side the site builds as a bare graph node is `undescribed`
/// — the directed pipeline has no expression for it at that call, and saying so
/// is part of the measurement rather than an omission from it.
pub const UnifyOperand = union(enum) {
    checked: CheckedAddress,
    /// A checked position of the CALLEE scheme the innermost open callee
    /// binding names. It translates under the binding the checker recorded for
    /// that edge rather than under the requesting body's own, which is what
    /// reunify.md section 9.1 instantiates and section 9.5 refuses to derive
    /// from the call's argument types. With no binding open it reads exactly as
    /// `checked`.
    callee_checked: CheckedAddress,
    /// The record field a site reads off a receiver: the checked position the
    /// receiver sits at, and the interned label. The directed side translates
    /// the receiver and reads the field off it, so a field read names a type
    /// even though the graph builds it as a derived node.
    field_of: FieldOperand,
    sealed: Type.TypeId,
    undescribed,
};

/// One record-field read described in the terms directed translation reads.
pub const FieldOperand = struct {
    receiver: CheckedAddress,
    /// The label interned in the program's name table, which is the table the
    /// directed emission's record fields are named by.
    label: names.RecordFieldNameId,
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
    /// argument `i` of the dispatch plan's own checked dispatcher type, emitted
    /// under the requesting body's environment. `Builtin.List.iter` is
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
    /// A `where`-constrained method call: the checker resolved the dispatch to
    /// `constraint(depth, index)`, so the callee is chosen per specialization
    /// from the evidence chain and checking recorded no instantiation site for
    /// the edge — the callee scheme is not known where the site would be
    /// written.
    ///
    /// Binder mapping (exact, total): the callee's scheme binder `i` takes
    /// argument `i` of the plan's own checked dispatcher type, emitted under the
    /// requesting body's environment. The dispatcher is the constrained variable
    /// itself, so only the environment holds its value: `dict_find_from`'s
    /// `found_key == key` dispatches `k.is_eq` on `k`, which the requesting
    /// binding holds as `Dict(A, B)`, and `Builtin.Dict.is_eq`'s binder list is
    /// exactly that argument list in order. A dispatcher whose argument count
    /// differs from the callee scheme's binder count is outside the rule and
    /// binds nothing.
    ///
    /// Witness: the plan's own checked callable type, emitted under the
    /// requesting body's environment, must equal the callee scheme root emitted
    /// under the binding.
    constraint_dispatch_receiver,
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
    /// Binder mapping (exact, total): the callee's scheme binder `i` takes
    /// argument `i` of the component the ladder is at, reached by the rule's
    /// declared emitted path (`GeneratedReceiver`) from the checked type the
    /// ladder entered at, emitted under the requesting body's environment. The
    /// ladder descends a Monotype and appends one declared step per layer —
    /// record field by interned name, tuple element and tag payload by
    /// position, and a nominal's backing — so a position the checked side names
    /// no id for (a nominal's backing, or a field of a receiver whose checked
    /// type is the constrained variable itself) is still named exactly. A
    /// position deeper than a declared path reaches hands over no receiver and
    /// stays unbound. A component whose argument count differs from the callee
    /// scheme's binder count is outside the rule and binds nothing.
    ///
    /// Witness: the callee scheme root emitted under the binding must carry the
    /// emitted receiver at the argument position the derivation dispatches on,
    /// which is argument zero for both `is_eq` and `to_hash`.
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
            .iterator_dispatch_receiver,
            .constraint_dispatch_receiver,
            .structural_derivation_component,
            => true,
            .inspect_component,
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
/// its ids, the receiver the rule's binder mapping reads, and the exact
/// structural witness the rule accepts its binding under.
pub const GeneratedSource = struct {
    module_bytes: [32]u8,
    receiver: GeneratedReceiver,
    witness: GeneratedWitness,
};

/// How many declared steps one rule's emitted path carries. A generated
/// component call sits a handful of aggregate layers below the checked type its
/// walk entered at; a position deeper than this cannot be declared exactly, so
/// its request hands over no receiver (reunify.md section 9.6).
pub const max_emitted_path_steps: usize = 8;

/// One declared step from an emitted type to one of its components (reunify.md
/// section 9.6). The step names the component the way the emitted node names it,
/// so a position no checked id stands for — a nominal's backing, or a field of a
/// receiver whose checked type is the constrained variable itself — is still
/// declared exactly rather than searched for.
pub const EmittedPathStep = union(enum) {
    /// The backing of a named type. A checked nominal names no instantiated
    /// backing type of its own, so this step exists only against the emission.
    nominal_backing,
    /// The record field carrying this interned label.
    record_field: names.RecordFieldNameId,
    /// The tuple element at this position.
    tuple_element: u32,
    /// One payload of the tag carrying this interned name.
    tag_payload: TagPayloadStep,
};

/// The tag and payload position one `tag_payload` step names.
pub const TagPayloadStep = struct {
    name: names.TagNameId,
    index: u32,
};

/// A declared path of at most `max_emitted_path_steps` steps, applied in order to
/// the emission of a rule's checked receiver type (reunify.md section 9.6).
pub const EmittedPath = struct {
    steps: [max_emitted_path_steps]EmittedPathStep = undefined,
    count: usize = 0,

    /// This path with one more declared step, or null when the position is
    /// deeper than a declared path reaches.
    pub fn appending(self: EmittedPath, step: EmittedPathStep) ?EmittedPath {
        if (self.count == max_emitted_path_steps) return null;
        var extended = self;
        extended.steps[self.count] = step;
        extended.count = self.count + 1;
        return extended;
    }

    /// The declared steps, in the order they apply.
    pub fn declaredSteps(self: *const EmittedPath) []const EmittedPathStep {
        return self.steps[0..self.count];
    }
};

/// Where one declared rule's receiver is: a checked type the generating site
/// names, plus the declared path from that type's emission to the position the
/// site dispatched at (reunify.md section 9.6). An empty path means the site
/// dispatched on the named type itself.
pub const GeneratedReceiver = struct {
    checked_ty: checked.CheckedTypeId,
    path: EmittedPath = .{},
};

/// The exact structural witness one declared rule accepts a binding under
/// (reunify.md sections 7.5, 9.6). Both forms compare an emission of the callee
/// scheme root under the binding against an emission the requesting body already
/// names, so neither accepts a binding the checked data does not prove.
pub const GeneratedWitness = union(enum) {
    /// The checked callable the request names: the callee scheme root, emitted
    /// under the binding, must equal this type emitted under the requesting
    /// body's environment.
    callable: checked.CheckedTypeId,
    /// The argument position of the callee scheme root that the rule's receiver
    /// occupies: that argument, emitted under the binding, must equal the
    /// receiver emitted under the requesting body's environment. This is the
    /// witness available where the requesting body names a receiver but no
    /// checked callable — a compiler-generated component call builds its
    /// callable from the component type it reached.
    receiver_at_argument: u32,
};

/// The requesting edge of one specialization: the module whose body made the
/// request, and the checked expression the request was made at. That expression
/// is the `use_node` half of the section 7.2 edge identity, resolved to a checked
/// id by the checker, so it names the `CheckedInstantiationSite` whose dense
/// actuals bind the callee scheme exactly.
pub const RequestEdge = struct {
    module_bytes: [32]u8,
    use_expr: checked.CheckedExprId,
    /// The declared rule that covers this edge where checking recorded no site
    /// for it (reunify.md sections 7.2, 9.6). A `where`-constrained dispatch
    /// records an ordinary site wherever checking could name the callee scheme,
    /// and none where the callee is only chosen per specialization edge; the
    /// rule states where the binder values come from in the second case and is
    /// never consulted in the first.
    covering_rule: ?GeneratedEdge = null,
    /// The requesting body's own binding at the moment the request was made. A
    /// request that reserves is lowered later, from a completely different frame
    /// stack, so the environment a symbolic actual resolves under travels with
    /// the edge instead of being read off whatever frame happens to be active
    /// when the request is finally lowered (reunify.md sections 7.3, 9.1).
    caller: ?CapturedEnvironment,
};

/// One requesting edge named by the module the request was made in and the
/// checked expression it was made at — the `use_node` half of reunify.md
/// section 7.2's edge identity.
pub const RequestUse = struct {
    module_bytes: [32]u8,
    use_expr: checked.CheckedExprId,
};

/// The checked data that binds one callee scheme a requesting body is
/// instantiating at a call site (reunify.md sections 7.2, 9.1, 9.6). The
/// requesting body states where the binding comes from; the binding itself is
/// read from the checker's records and never derived from the call's argument
/// types, which is the derivation section 9.5 forbids.
pub const CalleeBinding = struct {
    /// The module whose frozen store defines the callee scheme.
    defining_module_bytes: [32]u8,
    /// The scheme the callee's checked positions are binders of.
    scheme: checked.CheckedTypeSchemeId,
    /// The reservation whose claimed requesting edge binds this callee, for a
    /// call that already reserved a specialization (reunify.md section 11.3).
    reserved_fn_id: ?u32 = null,
    /// The requesting edge, where the requesting body names it directly.
    request: ?RequestUse = null,
    /// The declared rule covering an edge checking recorded no site for
    /// (reunify.md section 9.6).
    rule: ?GeneratedEdge = null,
};

/// One callee scheme's binding, resolved from checked data and held for as
/// long as the requesting body instantiates that callee's checked positions.
/// An unresolved level stays on the stack so opens and closes stay paired; its
/// positions then read under the requesting body's own binding, exactly as they
/// did before the binding was named.
const CalleeLevel = struct {
    module_bytes: [32]u8,
    owner_node: u32,
    chain: EnvironmentChain,
    ready: bool,
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
/// The identity half of a request scope's checked edge.
pub const RequestEdgeName = struct {
    module_bytes: [32]u8,
    use_expr: checked.CheckedExprId,
};

const RequestScope = union(enum) {
    none,
    checked: RequestEdge,
    generated: GeneratedRequest,
};

/// The token a body-lowering request carries on the specialization record it
/// will reserve later, naming the request scope the rehearsal kept for it.
pub const HeldRequest = struct {
    /// The key the kept scope is stored under, or `none.token` for a record the
    /// rehearsal kept no scope for.
    token: u32,

    /// The token of a record made under no request scope at all.
    pub const none: HeldRequest = .{ .token = 0 };
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
/// One node's checked position together with the binding context it was
/// instantiated under.
pub const ContextedProvenance = struct {
    address: CheckedAddress,
    callee_context: bool,
    scope_depth: u32,
    /// Whether an edge-naming request scope was open when the node was made,
    /// which is the binding a read could resolve from if none other names it.
    inside_request_edge: bool = false,
    /// The request edge open when this node was instantiated. The site that
    /// names the position's owning definition, if one does, is keyed at THIS
    /// use expression rather than at the read's (reunify.md 13.2 2a).
    request_edge: ?RequestEdgeName = null,
};

/// Debug/probe-only record of what each graph node stands for and how it
/// sealed, so a read at a graph exit can name the checked position behind it.
pub const SealTrace = struct {
    allocator: Allocator,
    provenance: std.AutoHashMapUnmanaged(u32, CheckedAddress),
    /// Every node instantiated from a checked position, regardless of the
    /// binding context it was created under. `provenance` records only the
    /// contexts whose binding the rehearsal can describe; this records the
    /// question "does this node stand for a checked position at all", which is
    /// what says whether a read of it could ever name one (reunify.md 13.2
    /// step 2a).
    from_checked: std.AutoHashMapUnmanaged(u32, void),
    /// Every node instantiated from a checked position, with the binding
    /// context it was created under. `provenance` covers only the root context,
    /// because a nested scope binds the same checked id under a different
    /// binding; recording the context lets a read decide whether the binding it
    /// holds is the one the node was built under (reunify.md 13.2 step 2a).
    contexted: std.AutoHashMapUnmanaged(u32, ContextedProvenance),
    sealed: std.AutoHashMapUnmanaged(u32, Type.TypeId),
    disabled: bool,

    /// An empty trace owning no storage yet.
    pub fn init(allocator: Allocator) SealTrace {
        return .{
            .allocator = allocator,
            .provenance = .empty,
            .from_checked = .empty,
            .contexted = .empty,
            .sealed = .empty,
            .disabled = false,
        };
    }

    /// Release the trace's tables.
    pub fn deinit(self: *SealTrace) void {
        self.provenance.deinit(self.allocator);
        self.from_checked.deinit(self.allocator);
        self.contexted.deinit(self.allocator);
        self.sealed.deinit(self.allocator);
    }

    /// Record that `node` was instantiated from `address`. Repeats keep the
    /// first address: one node stands for one checked position.
    /// Record `node`'s checked position and the context it was built under.
    pub fn noteContexted(self: *SealTrace, node: u32, record: ContextedProvenance) void {
        if (self.disabled) return;
        const gop = self.contexted.getOrPut(self.allocator, node) catch {
            self.disabled = true;
            return;
        };
        if (!gop.found_existing) gop.value_ptr.* = record;
    }

    /// The recorded position and context for `node`, if any.
    pub fn contextedFor(self: *const SealTrace, node: u32) ?ContextedProvenance {
        return self.contexted.get(node);
    }

    /// Record that `node` was instantiated from some checked position.
    pub fn noteFromChecked(self: *SealTrace, node: u32) void {
        if (self.disabled) return;
        _ = self.from_checked.getOrPut(self.allocator, node) catch {
            self.disabled = true;
        };
    }

    /// Whether `node` stands for a checked position.
    pub fn isFromChecked(self: *const SealTrace, node: u32) bool {
        return self.from_checked.contains(node);
    }

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
    /// The graph-seal trace of the transitional comparison, present only while
    /// that Debug measurement runs.
    trace: ?*SealTrace,
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

/// One worked example of a constraint-replay site whose two sides directed
/// translation did not already make equal: what the pair disagreed about, both
/// outermost shapes, and where the parallel walk first localized the difference.
const UnifyDetail = struct {
    information: census.UnifySiteInformation,
    left: HeadShape,
    right: HeadShape,
    difference: Difference,
    residual: ResidualTrace,
};

/// Where an informative execution's empty-tag-union head sits, and what names
/// the position it stands for. The empty tag union is the stored shape both
/// sides use for a position no value reached, so saying WHICH side carries it
/// and what that side is separates two different findings: a checked position
/// whose variable no recorded value names, and a type the graph sealed while
/// one of its own nodes was still unresolved.
const ResidualTrace = struct {
    side: Side,
    origin: OperandOrigin,
    /// The checked position the residual side stands for, when it has one.
    module_prefix: [8]u8,
    checked_ty: u32,
    /// The checked position the difference's own path reaches inside that root,
    /// or `no_variable` when the path names none.
    position: u32,
    state: ResidualState,
    /// Whether that variable carries a checked default of its own, which is the
    /// other way its value could be named without a disposition.
    defaults: VariableDefaults,

    const no_variable: u32 = std.math.maxInt(u32);

    const empty: ResidualTrace = .{
        .side = .neither,
        .origin = .graph_sealed,
        .module_prefix = [_]u8{0} ** 8,
        .checked_ty = 0,
        .position = no_variable,
        .state = .not_a_checked_position,
        .defaults = .{},
    };
};

/// The checked defaults one residual variable carries, so a finding says
/// whether anything at all names its value.
const VariableDefaults = struct {
    rigid: bool = false,
    numeric_phase: bool = false,
    row: bool = false,
    constraints: u32 = 0,
};

/// Which side of one constraint-replay site a finding sits on.
const Side = enum { left, right, neither };

const OperandOrigin = census.UnifySiteOperandOrigin;
const ResidualState = census.UnifySiteResidualState;

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

/// How many descent steps a difference records so the position can be followed
/// back through the checked type the emission came from. A difference deeper
/// than this records that its path is incomplete rather than a short prefix.
const max_difference_path: u32 = 8;

/// The child indices one difference walk descended, outermost first.
const DifferencePath = struct {
    steps: [max_difference_path]u16 = @splat(0),
    len: u32 = 0,
    /// False once the walk descended past `max_difference_path`, so the steps
    /// no longer name the difference's own position.
    complete: bool = true,
};

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
    var path: DifferencePath = .{};
    return firstDifferenceOnPath(left_store, left, right_store, right, name_store, depth, &path);
}

/// `firstDifference`, recording the child indices it descended so the position
/// can be followed back through the checked type the emission came from.
fn firstDifferenceOnPath(
    left_store: *const Type.Store,
    left: Type.TypeId,
    right_store: *const Type.Store,
    right: Type.TypeId,
    name_store: *const names.NameStore,
    depth: u32,
    path: *DifferencePath,
) Difference {
    const here = Difference{
        .depth = depth,
        .left = HeadShape.of(left_store, left),
        .right = HeadShape.of(right_store, right),
        .named_field = NamedFieldDifference.of(left_store, left, right_store, right),
        .left_recursive = isRecursive(left_store, left, left, 0),
        .right_recursive = isRecursive(right_store, right, right, 0),
    };
    path.len = depth;
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
        if (depth < max_difference_path) {
            path.steps[depth] = @intCast(index);
        } else {
            path.complete = false;
        }
        return firstDifferenceOnPath(left_store, left_child, right_store, right_child, name_store, depth + 1, path);
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
        return compare(left_store, left, right_store, right, false);
    }

    /// The first field that differs with the two representation fields
    /// reunify.md section 10.3's rules move — the generated-owner identity and
    /// the iterator tier, kind, and depth — skipped. A pair answering `equal`
    /// here and something else through `of` is one logical type carrying two
    /// representations.
    fn ofIgnoringRepresentation(
        left_store: *const Type.Store,
        left: Type.TypeId,
        right_store: *const Type.Store,
        right: Type.TypeId,
    ) NamedFieldDifference {
        return compare(left_store, left, right_store, right, true);
    }

    fn compare(
        left_store: *const Type.Store,
        left: Type.TypeId,
        right_store: *const Type.Store,
        right: Type.TypeId,
        skip_representation: bool,
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
        if (!skip_representation) {
            if (!std.meta.eql(left_named.def.generated, right_named.def.generated)) return .generated;
            if (left_named.def.iterator_representation != right_named.def.iterator_representation or
                left_named.def.iterator_kind != right_named.def.iterator_kind or
                left_named.def.iterator_depth != right_named.def.iterator_depth)
            {
                return .iterator;
            }
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
    /// The output type store. Directed translation emits into it, so an id this
    /// module produces is an ordinary production Monotype id.
    types: *Type.Store,
    /// The output name store; a translated type interns its names here exactly
    /// as graph instantiation does, so equal types digest equal.
    program_names: *names.NameStore,
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
    /// Request scopes taken out of the stack at the moment a body-lowering
    /// request records a specialization it will not reserve until later, keyed
    /// by the token handed back to that record.
    held_requests: std.AutoHashMapUnmanaged(u32, RequestScope),
    /// The next token `holdRequest` hands out. Zero names no held scope.
    next_held_request: u32,
    /// The open callee bindings, innermost last. A requesting body opens one
    /// around the region where it instantiates a callee's checked positions, so
    /// those positions read under the binding the checker recorded for the
    /// edge rather than under the requesting body's own (reunify.md 9.1).
    callees: std.ArrayList(CalleeLevel),
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
    /// One worked example per constraint-replay site that came out informative,
    /// so its classification is read against a concrete disagreeing pair.
    unify_details: [census.unify_site_count]?UnifyDetail,
    disabled: bool,
    /// Whether the transitional graph comparison runs. Debug measurement only:
    /// it selects nothing, and the emission below is the same either way.
    comparing: bool,

    /// Build the instantiation state for one lowering run.
    pub fn create(
        allocator: Allocator,
        types: *Type.Store,
        program_names: *names.NameStore,
        resolver: direct_translate.Resolver,
        lookup: ModuleLookup,
    ) Allocator.Error!*Rehearsal {
        const self = try allocator.create(Rehearsal);
        self.* = .{
            .allocator = allocator,
            .types = types,
            .program_names = program_names,
            .translator = undefined,
            .engine = closure.Engine.init(allocator),
            .lookup = lookup,
            .frames = .empty,
            .requests = .empty,
            .edges_by_fn = .empty,
            .held_requests = .empty,
            .next_held_request = 1,
            .callees = .empty,
            .generated_outcomes = [_]GeneratedOutcome{.{}} ** generated_rule_count,
            .site_index = .empty,
            .logical_tokens = .empty,
            .next_token = 1,
            .next_producer = 1,
            .slots = .empty,
            .slot_descriptors = .empty,
            .details = .empty,
            .unresolved_details = .empty,
            .unify_details = @splat(null),
            .disabled = false,
            .comparing = census.enabled and reunify_shadow.shouldRun(),
        };
        self.translator = direct_translate.Translator.init(allocator, self.types, program_names, resolver);
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
        var pending = self.held_requests.valueIterator();
        while (pending.next()) |scope| {
            self.releaseScope(scope.*);
        }
        self.held_requests.deinit(self.allocator);
        for (self.callees.items) |*level| level.chain.release(self.allocator);
        self.callees.deinit(self.allocator);
        self.engine.deinit();
        self.translator.deinit();
        self.allocator.destroy(self);
    }

    /// Open a request scope naming the edge the request is made from: the
    /// caller's module and the checked expression the use sits at. The scope
    /// must be closed by `closeRequest` when the request finishes, so an edge no
    /// reservation claimed cannot be read by a later, unrelated request.
    /// Debug/probe-only: the innermost open request scope's checked edge, if it
    /// names one (reunify.md 13.2 2a).
    pub fn innermostRequestEdge(self: *const Rehearsal) ?RequestEdgeName {
        if (self.disabled) return null;
        if (self.requests.items.len == 0) return null;
        return switch (self.requests.items[self.requests.items.len - 1]) {
            .checked => |edge| .{ .module_bytes = edge.module_bytes, .use_expr = edge.use_expr },
            .none, .generated => null,
        };
    }

    /// Debug/probe-only: for a position that diverged, whether the checked data
    /// names its unbound variable's owning definition AT THE REQUEST EDGE the
    /// position entered through. The read's own use expression never carries
    /// that site; the edge's might, and that is the difference between building
    /// a level for the definition and having nothing to build it from.
    /// Debug/probe-only: whether `needle` is reachable from `root` in a checked
    /// type, with an explicit visited set so a recursive type terminates
    /// (reunify.md 15.10).
    fn checkedReaches(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
        needle: checked.CheckedTypeId,
    ) bool {
        var visited = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(checked.CheckedTypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return false;
        while (stack.pop()) |current| {
            if (current == needle) return true;
            const seen = visited.getOrPut(current) catch return false;
            if (seen.found_existing) continue;
            switch (cursor.view.payload(current)) {
                .alias => |alias| {
                    for (alias.args) |arg| stack.append(self.allocator, arg) catch return false;
                    stack.append(self.allocator, alias.backing) catch return false;
                },
                .record => |record| {
                    for (record.fields) |field| stack.append(self.allocator, field.ty) catch return false;
                    stack.append(self.allocator, record.ext) catch return false;
                },
                .record_unbound => |fields| for (fields) |field| {
                    stack.append(self.allocator, field.ty) catch return false;
                },
                .tuple => |items| for (items) |item| {
                    stack.append(self.allocator, item) catch return false;
                },
                .nominal => |nominal| {
                    for (nominal.args) |arg| stack.append(self.allocator, arg) catch return false;
                },
                .function => |function| {
                    for (function.args) |arg| stack.append(self.allocator, arg) catch return false;
                    stack.append(self.allocator, function.ret) catch return false;
                },
                .tag_union => |union_type| {
                    for (union_type.tags) |tag| {
                        for (tag.argsSlice(cursor.view)) |arg| stack.append(self.allocator, arg) catch return false;
                    }
                    stack.append(self.allocator, union_type.ext) catch return false;
                },
                else => {},
            }
        }
        return false;
    }

    /// Debug/probe-only: for a divergence that entered through no request edge,
    /// whether any recorded site in the position's own module names the
    /// definition its unbound variable belongs to (reunify.md 13.2 2a).
    fn noteEdgelessDivergenceOwner(self: *Rehearsal, address: CheckedAddress, under_callee: bool) void {
        if (comptime !census.enabled) return;
        const cursor = self.lookup.cursor(address.module_bytes) orelse return;
        var env: ?*const direct_translate.BindingEnvironment = null;
        const callee = if (under_callee) self.innermostCallee(address.module_bytes) else null;
        if (callee) |level| {
            env = level.chain.innermost();
        } else if (self.frameForModule(address.module_bytes)) |frame| {
            env = frame.environment();
        }
        const free = self.firstFreeVariable(cursor.view, @enumFromInt(address.type_id), env) orelse {
            census.bump("edgeless_no_free_variable");
            return;
        };
        var owner_node: ?u32 = null;
        for (cursor.view.schemes) |scheme| {
            for (scheme.generalizedVars(cursor.view)) |binder| {
                if (binder == free) {
                    owner_node = scheme.owner_node;
                    break;
                }
            }
            if (owner_node != null) break;
        }
        const owner = owner_node orelse {
            census.bump("edgeless_free_var_unowned");
            // No scheme generalizes this variable, so no instantiation edge can
            // state its value. Ask what checking DOES hold for it: if it is a
            // variable checking left unresolved and unclassified, there is
            // nothing for checking to record and the value only exists once a
            // specialization is chosen (reunify.md 15.2).
            // A rigid is a declared parameter of some signature. If no scheme's
            // binder list holds it, ask whether it is REACHABLE from the root
            // of the definition being specialized: if it is, that definition's
            // binder list omits a parameter its own signature contains, which
            // is a capture gap; if it is not, the parameter belongs to some
            // definition this specialization never names (reunify.md 7.1).
            // The decisive question: is this rigid in ANY definition's signature
            // in the module? If it is, that definition's binder list omits a
            // parameter of its own signature and checking can record it. If it
            // is in none, it is a parameter no signature carries, and there is
            // nothing for checking to attach it to (reunify.md 7.1, 15.2).
            {
                var found_in_some_scheme = false;
                for (cursor.view.schemes) |scheme| {
                    if (self.checkedReaches(cursor, scheme.root, free)) {
                        found_in_some_scheme = true;
                        break;
                    }
                }
                if (found_in_some_scheme) {
                    census.bump("unowned_rigid_in_some_scheme_root");
                } else {
                    census.bump("unowned_rigid_in_no_scheme_root");
                    // Local schemes are only half the definitions. A parameter
                    // of an IMPORTED definition lives in the projected
                    // imported-scheme table, whose binder list is separate
                    // (reunify.md 7.1).
                    var in_imported_binders = false;
                    for (cursor.view.importedSchemeBinders()) |binder| {
                        if (binder == free) {
                            in_imported_binders = true;
                            break;
                        }
                    }
                    if (in_imported_binders) {
                        census.bump("unowned_rigid_is_imported_binder");
                    } else {
                        var in_imported_root = false;
                        for (cursor.view.importedSchemes()) |imported| {
                            if (self.checkedReaches(cursor, imported.localRoot(), free)) {
                                in_imported_root = true;
                                break;
                            }
                        }
                        if (in_imported_root) {
                            census.bump("unowned_rigid_in_imported_root_not_binders");
                        } else {
                            census.bump("unowned_rigid_in_no_root_at_all");
                            // A scheme's recorded `root` is the FINAL root,
                            // whose free variables may have been unified after
                            // generalization; `snapshot_root` is the pristine
                            // scheme at the boundary. A parameter substituted
                            // away in the former still stands in the latter
                            // (reunify.md 7.1).
                            var in_pristine = false;
                            for (cursor.view.schemes) |scheme| {
                                if (scheme.snapshot_root == checked.scheme_snapshot_root_none) continue;
                                const pristine: checked.CheckedTypeId = @enumFromInt(scheme.snapshot_root);
                                if (self.checkedReaches(cursor, pristine, free)) {
                                    in_pristine = true;
                                    break;
                                }
                            }
                            if (in_pristine) {
                                census.bump("unowned_rigid_in_pristine_root_only");
                            } else {
                                census.bump("unowned_rigid_in_no_pristine_root_either");
                                // A scheme is a VALUE definition's signature. A
                                // nominal TYPE declaration also has formal
                                // parameters, and those are bound by the
                                // nominal's arguments at each use rather than by
                                // any scheme's binder list. Ask whether the
                                // parameter is one of those.
                                var in_declaration = false;
                                for (cursor.view.nominal_declarations) |declaration| {
                                    if (self.checkedReaches(cursor, declaration.declaration_root, free) or
                                        self.checkedReaches(cursor, declaration.backing, free))
                                    {
                                        in_declaration = true;
                                        break;
                                    }
                                }
                                if (in_declaration) {
                                    census.bump("unowned_rigid_is_nominal_declaration_parameter");
                                } else {
                                    census.bump("unowned_rigid_in_nothing_at_all");
                                }
                            }
                        }
                    }
                }
            }
            if (self.frameForModule(address.module_bytes)) |frame| {
                if (cursor.view.schemeIdForOwnerNode(frame.owner_node)) |frame_scheme_id| {
                    if (cursor.view.schemeById(frame_scheme_id)) |frame_scheme| {
                        if (self.checkedReaches(cursor, frame_scheme.root, free)) {
                            census.bump("unowned_rigid_reachable_from_frame_scheme");
                        } else {
                            census.bump("unowned_rigid_outside_frame_scheme");
                        }
                    }
                } else {
                    census.bump("unowned_frame_owner_has_no_scheme");
                }
            } else {
                census.bump("unowned_no_frame_for_module");
            }
            switch (cursor.view.payload(free)) {
                .flex => |variable| {
                    census.bump("unowned_var_is_flex");
                    if (variable.constraints.len != 0) census.bump("unowned_var_has_constraints");
                    if (variable.numeric_default_phase != null) census.bump("unowned_var_has_numeric_default");
                    if (variable.row_default != null) census.bump("unowned_var_has_row_default");
                },
                .rigid => census.bump("unowned_var_is_rigid"),
                else => census.bump("unowned_var_is_not_a_variable"),
            }
            var disposed = false;
            for (cursor.view.residualDispositions()) |disposition| {
                if (disposition.type_id == @intFromEnum(free)) {
                    disposed = true;
                    break;
                }
            }
            if (disposed) {
                census.bump("unowned_var_has_disposition");
            } else {
                census.bump("unowned_var_has_no_disposition");
            }
            return;
        };
        for (cursor.view.instantiationSites()) |site| {
            if (site.scheme_owner_node == owner) {
                census.bump("edgeless_owner_has_site_somewhere");
                return;
            }
        }
        census.bump("edgeless_owner_has_no_site_anywhere");
    }

    pub fn noteDivergenceEdgeSite(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        edge: ?RequestEdgeName,
    ) void {
        if (comptime !census.enabled) return;
        if (self.disabled) return;
        const named = edge orelse {
            census.bump("divergence_no_request_edge");
            // No entering edge names a site for this position. Ask whether the
            // checked data records its definition's instantiation ANYWHERE in
            // the module: if it does, the value exists and only the key that
            // selects it is missing; if it does not, no recorded edge states
            // it at all and closing this needs checking to record more.
            self.noteEdgelessDivergenceOwner(address, under_callee);
            return;
        };
        const cursor = self.lookup.cursor(address.module_bytes) orelse return;
        var env: ?*const direct_translate.BindingEnvironment = null;
        const callee = if (under_callee) self.innermostCallee(address.module_bytes) else null;
        if (callee) |level| {
            env = level.chain.innermost();
        } else if (self.frameForModule(address.module_bytes)) |frame| {
            env = frame.environment();
        }
        const free = self.firstFreeVariable(cursor.view, @enumFromInt(address.type_id), env) orelse {
            census.bump("divergence_no_free_variable");
            return;
        };
        var owner: ?u32 = null;
        for (cursor.view.schemes) |scheme| {
            for (scheme.generalizedVars(cursor.view)) |binder| {
                if (binder == free) {
                    owner = scheme.owner_node;
                    break;
                }
            }
            if (owner != null) break;
        }
        const owner_node = owner orelse {
            census.bump("divergence_free_var_unowned");
            return;
        };
        const caller = self.lookup.cursor(named.module_bytes) orelse return;
        if (self.siteQuietly(caller, named.use_expr, owner_node) != null) {
            census.bump("divergence_site_at_request_edge");
        } else {
            census.bump("divergence_no_site_at_request_edge");
        }
    }

    pub fn openRequestEdge(
        self: *Rehearsal,
        module_bytes: [32]u8,
        use_expr: checked.CheckedExprId,
        covering_rule: ?GeneratedEdge,
    ) void {
        if (self.disabled) return;
        const edge = RequestEdge{
            .module_bytes = module_bytes,
            .use_expr = use_expr,
            .covering_rule = covering_rule,
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

    /// Open a callee binding for the region where the requesting body
    /// instantiates one callee scheme's checked positions (reunify.md sections
    /// 7.2, 9.1, 9.6). The binding is resolved once, here, from the recorded
    /// site or the declared rule the requesting body names; a binding that does
    /// not resolve is still pushed, so opens and closes stay paired and its
    /// positions read exactly as they did before it was named.
    pub fn openCalleeBinding(self: *Rehearsal, binding: CalleeBinding) void {
        if (self.disabled) return;
        const level = self.resolveCalleeBinding(binding);
        self.callees.append(self.allocator, level) catch {
            var owned = level;
            owned.chain.release(self.allocator);
            self.fail();
        };
    }

    /// Close the innermost callee binding.
    pub fn closeCalleeBinding(self: *Rehearsal) void {
        // Once this state disables itself no binding is opened, so none is
        // closed either and the stack stays balanced across the transition.
        if (self.disabled) return;
        var level = self.callees.pop() orelse return;
        level.chain.release(self.allocator);
    }

    /// Resolve one callee scheme's dense binding from the checked data the
    /// requesting body named. Every way the data fails to supply one leaves the
    /// level unresolved: the binding is read, never inferred.
    fn resolveCalleeBinding(self: *Rehearsal, binding: CalleeBinding) CalleeLevel {
        const unresolved = CalleeLevel{
            .module_bytes = binding.defining_module_bytes,
            .owner_node = checked.checked_residual_disposition_module_body_owner,
            .chain = EnvironmentChain.none,
            .ready = false,
        };
        const defining = self.lookup.cursor(binding.defining_module_bytes) orelse {
            census.bump("rehearsal_callee_unresolved_defining_module_absent");
            return unresolved;
        };
        const scheme = defining.view.schemeById(binding.scheme) orelse {
            census.bump("rehearsal_callee_unresolved_scheme_absent");
            return unresolved;
        };
        // A callee that captures enclosing binders needs the lexical parents its
        // own specialization frame links (reunify.md section 7.3); a call-site
        // binding states this scheme's own binders and nothing else.
        if (scheme.captured_len != 0) {
            census.bump("rehearsal_callee_unresolved_captures");
            return unresolved;
        }
        if (scheme.gv_len == 0) census.bump("rehearsal_callee_scheme_without_binders");

        var use = binding.request;
        var rule = binding.rule;
        var caller_env: ?*const direct_translate.BindingEnvironment = null;
        var caller_owner_node = checked.checked_residual_disposition_module_body_owner;
        if (binding.reserved_fn_id) |fn_id| {
            const claim = self.edges_by_fn.getPtr(fn_id) orelse return unresolved;
            switch (claim.*) {
                .checked => |*edge| {
                    use = .{ .module_bytes = edge.module_bytes, .use_expr = edge.use_expr };
                    if (edge.covering_rule) |covering| rule = covering;
                    if (edge.caller) |*held| {
                        caller_env = held.environment();
                        caller_owner_node = held.owner_node;
                    }
                },
                .generated => |*request| {
                    rule = request.edge;
                    if (request.caller) |*held| {
                        caller_env = held.environment();
                        caller_owner_node = held.owner_node;
                    }
                },
            }
        }

        // Checking records a site wherever it could name the callee scheme, and
        // the declared rule states the binding exactly where it could not
        // (reunify.md sections 7.2, 9.6).
        if (use) |named| resolved_by_site: {
            const caller = self.lookup.cursor(named.module_bytes) orelse break :resolved_by_site;
            if (caller_env == null) {
                if (self.frameForModule(named.module_bytes)) |frame| {
                    caller_env = frame.environment();
                    caller_owner_node = frame.owner_node;
                }
            }
            const site = self.siteQuietly(caller, named.use_expr, scheme.owner_node) orelse {
                census.bump("rehearsal_callee_site_absent");
                // Only a callee whose scheme actually generalizes something can
                // strand a binder; one with none needs no binding at all.
                if (scheme.gv_len == 0) {
                    census.bump("rehearsal_callee_site_absent_scheme_without_binders");
                } else {
                    census.bump("rehearsal_callee_site_absent_scheme_with_binders");
                }
                classifyAbsentCalleeSite(caller, named.use_expr, scheme.owner_node);
                break :resolved_by_site;
            };
            const chain = self.bindCalleeFromSite(
                defining,
                binding.scheme,
                scheme,
                caller,
                caller_env,
                caller_owner_node,
                site,
            ) orelse {
                census.bump("rehearsal_callee_site_bind_failed");
                break :resolved_by_site;
            };
            census.bump("rehearsal_callee_resolved_by_site");
            return .{
                .module_bytes = defining.module_bytes,
                .owner_node = scheme.owner_node,
                .chain = chain,
                .ready = true,
            };
        }

        const declared = rule orelse {
            census.bump("rehearsal_callee_unresolved_no_site_no_rule");
            if (scheme.gv_len != 0) census.bump("rehearsal_callee_unresolved_no_rule_with_binders");
            return unresolved;
        };
        const chain = self.bindCalleeFromRule(
            defining,
            binding.scheme,
            scheme,
            declared,
            caller_env,
            caller_owner_node,
        ) orelse {
            census.bump("rehearsal_callee_unresolved_rule_bind_failed");
            if (scheme.gv_len != 0) census.bump("rehearsal_callee_unresolved_rule_failed_with_binders");
            return unresolved;
        };
        census.bump("rehearsal_callee_resolved_by_rule");
        return .{
            .module_bytes = defining.module_bytes,
            .owner_node = scheme.owner_node,
            .chain = chain,
            .ready = true,
        };
    }

    /// Build one callee scheme's dense binding from a recorded instantiation
    /// site: `actuals[i]` translated under the requesting body's environment is
    /// binder `i`'s value (reunify.md sections 7.2, 9.1). A site whose own
    /// scheme identity is not this callee's is accepted only under section
    /// 7.5's witness — the callee scheme root emitted under the binding must be
    /// the site's own instantiated root.
    fn bindCalleeFromSite(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme_id: checked.CheckedTypeSchemeId,
        scheme: checked.CheckedTypeScheme,
        caller: direct_translate.ModuleCursor,
        caller_env: ?*const direct_translate.BindingEnvironment,
        caller_owner_node: u32,
        site: checked.CheckedInstantiationSite,
    ) ?EnvironmentChain {
        const binders = scheme.generalizedVars(defining.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) return null;

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return null;
        };
        defer self.allocator.free(bound);
        for (actuals, 0..) |actual, index| {
            if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) return null;
            const translated = self.emitQuietly(caller, caller_env, caller_owner_node, actual) orelse return null;
            bound[index] = direct_translate.BoundType.of(translated);
        }
        var chain = self.copyEnvironmentChain(null, 0, .{
            .scheme = .{ .module_bytes = defining.module_bytes, .scheme = @intFromEnum(scheme_id) },
            .binders = binders,
            .bound = bound,
            .captured = &.{},
        }) orelse {
            self.fail();
            return null;
        };
        if (siteNamesScheme(site, defining, scheme_id)) return chain;
        const declared = self.emitQuietly(defining, chain.innermost(), scheme.owner_node, scheme.root);
        const requested = self.emitQuietly(caller, caller_env, caller_owner_node, site.instantiated_root);
        if (self.quietWitnessAgrees(declared, requested)) return chain;
        chain.release(self.allocator);
        return null;
    }

    /// Build one callee scheme's dense binding from a declared generated rule
    /// (reunify.md section 9.6): binder `i` takes argument `i` of the rule's
    /// receiver emitted under the requesting body's environment, accepted only
    /// under the exact witness the rule declares.
    fn bindCalleeFromRule(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme_id: checked.CheckedTypeSchemeId,
        scheme: checked.CheckedTypeScheme,
        edge: GeneratedEdge,
        held_env: ?*const direct_translate.BindingEnvironment,
        held_owner_node: u32,
    ) ?EnvironmentChain {
        if (!edge.rule.declaresBinderSource()) return null;
        const source = edge.source orelse return null;
        const caller = self.lookup.cursor(source.module_bytes) orelse return null;
        var caller_env = held_env;
        var caller_owner_node = held_owner_node;
        if (caller_env == null) {
            if (self.frameForModule(source.module_bytes)) |frame| {
                caller_env = frame.environment();
                caller_owner_node = frame.owner_node;
            }
        }
        const binders = scheme.generalizedVars(defining.view);
        const receiver_root = self.emitQuietly(caller, caller_env, caller_owner_node, source.receiver.checked_ty) orelse {
            census.bump("rehearsal_rule_receiver_untranslatable");
            return null;
        };
        const receiver = followEmittedPath(self.types, receiver_root, &source.receiver.path) orelse {
            census.bump("rehearsal_rule_receiver_path_absent");
            return null;
        };
        const argument_count = receiverArgumentCount(self.types, receiver) orelse {
            census.bump("rehearsal_rule_receiver_not_applied");
            return null;
        };
        if (argument_count != binders.len) {
            if (argument_count == 0) {
                census.bump("rehearsal_rule_receiver_argument_free");
                if (binders.len == 1) census.bump("rehearsal_rule_receiver_argument_free_one_binder");
            } else {
                census.bump("rehearsal_rule_receiver_arity_differs");
            }
            return null;
        }

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return null;
        };
        defer self.allocator.free(bound);
        for (0..binders.len) |index| {
            const argument = receiverArgumentAt(self.types, receiver, index) orelse return null;
            bound[index] = direct_translate.BoundType.of(argument);
        }
        var chain = self.copyEnvironmentChain(null, 0, .{
            .scheme = .{ .module_bytes = defining.module_bytes, .scheme = @intFromEnum(scheme_id) },
            .binders = binders,
            .bound = bound,
            .captured = &.{},
        }) orelse {
            self.fail();
            return null;
        };
        const declared = self.emitQuietly(defining, chain.innermost(), scheme.owner_node, scheme.root);
        const left: ?Type.TypeId = switch (source.witness) {
            .callable => declared,
            .receiver_at_argument => |index| if (declared) |root| functionArgumentAt(self.types, root, index) else null,
        };
        const right: ?Type.TypeId = switch (source.witness) {
            .callable => |callable| self.emitQuietly(caller, caller_env, caller_owner_node, callable),
            .receiver_at_argument => receiver,
        };
        if (self.quietWitnessAgrees(left, right)) return chain;
        census.bump("rehearsal_rule_witness_disagreed");
        chain.release(self.allocator);
        return null;
    }

    /// Whether one binding produced the exact witness that accepts it, asked
    /// without recording anything: the callee-binding path must leave the
    /// specialization resolver's own per-rule counters exactly where it put
    /// them. Two rooted recursive graphs entered from different paths store
    /// different digests for one type (reunify.md section 8.3), so the
    /// unfolding decides those.
    fn quietWitnessAgrees(self: *Rehearsal, declared: ?Type.TypeId, requested: ?Type.TypeId) bool {
        const left = declared orelse return false;
        const right = requested orelse return false;
        const left_digest = self.types.typeDigest(self.program_names, left);
        const right_digest = self.types.typeDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) return true;
        const left_unfolded = self.types.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, right);
        return std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes);
    }

    /// The instantiation site one edge identity names, asked without recording
    /// anything, so the callee-binding path leaves the specialization
    /// resolver's skip counters exactly where it put them.
    fn siteQuietly(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        use_expr: checked.CheckedExprId,
        scheme_owner_node: u32,
    ) ?checked.CheckedInstantiationSite {
        const index = self.siteIndexFor(caller) orelse return null;
        const key = siteKey(use_expr, scheme_owner_node);
        if (index.ambiguous.contains(key)) return null;
        const site_index = index.by_edge.get(key) orelse return null;
        return caller.view.instantiationSites()[site_index];
    }

    /// Why one callee edge found no site: whether checking recorded sites at
    /// that use at all, and whether any site anywhere in the requesting module
    /// names this callee's scheme owner. A use with sites that all name other
    /// owners is an edge whose identity disagrees with the recorded one; a use
    /// with no sites at all is an edge outside the section 7.2 coverage table.
    fn classifyAbsentCalleeSite(
        caller: direct_translate.ModuleCursor,
        use_expr: checked.CheckedExprId,
        scheme_owner_node: u32,
    ) void {
        var use_has_sites = false;
        var owner_has_sites = false;
        for (caller.view.instantiationSites()) |site| {
            const site_use = site.useExpr() orelse continue;
            if (site_use == use_expr) use_has_sites = true;
            if (site.scheme_owner_node == scheme_owner_node) owner_has_sites = true;
        }
        if (use_has_sites and owner_has_sites) {
            census.bump("rehearsal_callee_site_absent_both_present_unpaired");
        } else if (use_has_sites) {
            census.bump("rehearsal_callee_site_absent_use_owned_elsewhere");
        } else if (owner_has_sites) {
            census.bump("rehearsal_callee_site_absent_owner_used_elsewhere");
        } else {
            census.bump("rehearsal_callee_site_absent_unrecorded");
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

    /// Take the innermost open request scope out of the stack and keep it under
    /// a fresh token, for a body-lowering request whose specialization is
    /// recorded now and reserved only after the requesting graph freezes
    /// (reunify.md sections 7.2, 11.3).
    ///
    /// A request whose specialization is reserved in the same call reaches
    /// `claimRequestEdge` with its scope still open and holds nothing. One that
    /// defers has no scope open by then, so the scope travels with the record
    /// instead: the token is stored on it and `reopenHeldRequest` puts the scope
    /// back around the deferred reservation. Taking it here keeps the same
    /// once-per-edge discipline the claim has — a second record made under one
    /// scope names no edge, exactly as a second claim would not.
    pub fn holdRequest(self: *Rehearsal) HeldRequest {
        if (self.disabled) return HeldRequest.none;
        if (self.requests.items.len == 0) {
            census.bump("rehearsal_request_hold_without_scope");
            return HeldRequest.none;
        }
        const slot = &self.requests.items[self.requests.items.len - 1];
        const taken = slot.*;
        const token = self.next_held_request;
        self.held_requests.put(self.allocator, token, taken) catch {
            self.releaseScope(taken);
            slot.* = .none;
            self.fail();
            return HeldRequest.none;
        };
        slot.* = .none;
        self.next_held_request += 1;
        switch (taken) {
            .none => census.bump("rehearsal_request_held_without_edge"),
            .checked => census.bump("rehearsal_request_held_checked"),
            .generated => census.bump("rehearsal_request_held_generated"),
        }
        return .{ .token = token };
    }

    /// Push a held request scope back onto the stack around the deferred
    /// reservation it belongs to. The token names no held scope when the
    /// recording request had none, and the scope pushed for it names no edge, so
    /// the reservation made inside cannot reach an unrelated enclosing scope's.
    /// Paired with `closeRequest` exactly like the open functions.
    pub fn reopenHeldRequest(self: *Rehearsal, held: HeldRequest) void {
        if (self.disabled) return;
        const scope: RequestScope = scope: {
            const token = held.token;
            if (token == HeldRequest.none.token) break :scope .none;
            const found = self.held_requests.fetchRemove(token) orelse break :scope .none;
            break :scope found.value;
        };
        self.requests.append(self.allocator, scope) catch {
            self.releaseScope(scope);
            self.fail();
        };
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
        const trace: ?*SealTrace = if (self.comparing) blk: {
            const owned = self.allocator.create(SealTrace) catch return self.fail();
            owned.* = SealTrace.init(self.allocator);
            break :blk owned;
        } else null;
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
        start.graph.seal_probe = self;
    }

    /// Debug/probe-only: name the checked position behind a divergence, once
    /// per occurrence, so the number of DISTINCT positions needing a new
    /// recorded entry can be counted offline. The count of reads overstates
    /// that badly, since one position is read many times (reunify.md 15.2).
    fn notePositionNeedingRecord(self: *Rehearsal, address: CheckedAddress) void {
        if (comptime !census.enabled) return;
        const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
        const hex = std.fmt.bytesToHex(address.module_bytes[0..8].*, .lower);
        // Name the parameter the position wants. A rigid carries the source
        // name it was declared under, which says what construct introduced it.
        var param_name: []const u8 = "?";
        var param_id: u32 = 0;
        var position_kind: []const u8 = "?";
        if (self.lookup.cursor(address.module_bytes)) |cursor| {
            position_kind = @tagName(cursor.view.payload(@enumFromInt(address.type_id)));
            var env: ?*const direct_translate.BindingEnvironment = null;
            if (self.frameForModule(address.module_bytes)) |frame| env = frame.environment();
            if (self.firstFreeVariable(cursor.view, @enumFromInt(address.type_id), env)) |free| {
                param_id = @intFromEnum(free);
                switch (cursor.view.payload(free)) {
                    .rigid => |variable| param_name = variable.name orelse "<unnamed>",
                    .flex => |variable| param_name = variable.name orelse "<unnamed-flex>",
                    else => param_name = "<not-a-var>",
                }
            }
        }
        const line = std.fmt.allocPrint(
            self.allocator,
            "needs_record {s} {d} pos={s} param={d} name={s}\n",
            .{ &hex, address.type_id, position_kind, param_id, param_name },
        ) catch return;
        defer self.allocator.free(line);
        census.appendToFile(raw_path, line);
    }

    /// Debug/probe-only: asked POSITION-first rather than node-first, is this
    /// position a declaration parameter that some nominal instance in the same
    /// module already supplies an argument for? The node-first form failed
    /// because most diverging nodes are created outside a backing
    /// instantiation, which describes when the graph makes nodes rather than
    /// what the checked store holds (reunify.md 7.1).
    fn notePositionCoveredByNominalArgs(self: *Rehearsal, address: CheckedAddress) void {
        if (comptime !census.enabled) return;
        const cursor = self.lookup.cursor(address.module_bytes) orelse return;
        const position: checked.CheckedTypeId = @enumFromInt(address.type_id);
        const free = self.firstFreeVariable(cursor.view, position, null) orelse {
            census.bump("nominal_args_no_free_variable");
            return;
        };
        // Which declaration, if any, declares this parameter.
        var declaring: ?checked.CheckedNominalDeclaration = null;
        var formal_index: usize = 0;
        for (cursor.view.nominal_declarations) |declaration| {
            const formals = declaration.formalArgs(cursor.view);
            for (formals, 0..) |formal, index| {
                if (formal == free) {
                    declaring = declaration;
                    formal_index = index;
                    break;
                }
            }
            if (declaring != null) break;
        }
        const declaration = declaring orelse {
            census.bump("nominal_args_not_a_declared_parameter");
            return;
        };
        // Does any recorded nominal instance of that declaration carry an
        // argument in this slot?
        var instances: usize = 0;
        for (0..cursor.view.stored_payloads.len) |raw| {
            switch (cursor.view.payload(@enumFromInt(raw))) {
                .nominal => |nominal| {
                    if (nominal.args.len <= formal_index) continue;
                    const instance_declaration = cursor.view.nominalDeclarationForPayload(nominal) orelse continue;
                    if (@intFromEnum(instance_declaration.id) != @intFromEnum(declaration.id)) continue;
                    instances += 1;
                },
                else => {},
            }
        }
        if (instances == 0) {
            census.bump("nominal_args_no_instance_supplies_slot");
        } else if (instances == 1) {
            census.bump("nominal_args_exactly_one_instance");
        } else {
            census.bump("nominal_args_many_instances");
        }
    }

    /// Debug/probe-only: what gave a diverging node its concrete value. The
    /// graph resolves by unification, so the value arrives from some other node
    /// joined into the same class. If a class member names a DIFFERENT checked
    /// position, that position is the source a directed replacement has to read
    /// (reunify.md 13.2 2a).
    fn noteWhatSuppliedTheValue(
        record: ContextedProvenance,
        graph: *solve.InstGraph,
        node: solve.NodeId,
    ) void {
        if (comptime !census.enabled) return;
        const trace = graph.trace orelse return;
        var members: usize = 0;
        var other_positions: usize = 0;
        var same_position: usize = 0;
        var it = graph.classMemberIterator(node);
        while (it.next()) |member| {
            members += 1;
            const other = trace.contextedFor(@intFromEnum(member)) orelse continue;
            if (other.address.type_id == record.address.type_id and
                std.mem.eql(u8, &other.address.module_bytes, &record.address.module_bytes))
            {
                same_position += 1;
            } else {
                other_positions += 1;
            }
        }
        if (members <= 1) {
            census.bump("supplier_class_is_alone");
        } else if (other_positions > 0) {
            census.bump("supplier_class_holds_another_position");
        } else if (same_position > 0) {
            census.bump("supplier_class_only_same_position");
        } else {
            census.bump("supplier_class_has_no_named_member");
        }
    }

    /// Debug/probe-only: compare one sealed type against what directed
    /// translation computes for the position the node stands for, using the
    /// position's own recorded address (reunify.md 13.2 step 2a).
    pub fn compareSealedAgainstDirected(
        self: *Rehearsal,
        record: ContextedProvenance,
        sealed: Type.TypeId,
        graph: *solve.InstGraph,
        node: solve.NodeId,
    ) void {
        if (comptime !census.enabled) return;
        if (self.disabled) return;
        var binding: PositionBinding = .none;
        const probed = self.typeForCheckedPositionWithEdge(
            record.address,
            record.callee_context,
            &binding,
            record.request_edge,
        ) catch null;
        const direct_ty = probed orelse {
            census.bump("seam_direct_absent");
            return;
        };
        const left = self.types.typeDigest(self.program_names, direct_ty);
        const right = self.types.typeDigest(self.program_names, sealed);
        if (std.mem.eql(u8, &left.bytes, &right.bytes)) {
            census.bump("seam_direct");
            return;
        }
        const left_unfolded = self.types.unfoldedDigest(self.program_names, direct_ty);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, sealed);
        if (std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes)) {
            census.bump("seam_direct");
            return;
        }
        census.bump("seam_direct_diverged");
        census.bump("seal_exit_diverged");
        noteWhatSuppliedTheValue(record, graph, node);
        self.notePositionCoveredByNominalArgs(record.address);
        self.notePositionNeedingRecord(record.address);
        self.noteDivergenceEdgeSite(record.address, record.callee_context, record.request_edge);
        // Classify it the way the constraint census classifies its own
        // informative executions, so the seal exit's divergences can be
        // compared against the shape already diagnosed there.
        var path: DifferencePath = .{};
        const difference = firstDifferenceOnPath(
            self.types,
            direct_ty,
            self.types,
            sealed,
            self.program_names,
            0,
            &path,
        );
        if (difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead()) {
            census.bump("seal_diverged_direct_unbound");
        } else if (difference.right.isEmptyTagUnionHead() and !difference.left.isEmptyTagUnionHead()) {
            census.bump("seal_diverged_graph_unbound");
        } else if (difference.left.tag != difference.right.tag) {
            census.bump("seal_diverged_head_tag");
        } else if (difference.left.entries != difference.right.entries) {
            census.bump("seal_diverged_row_width");
        } else if (difference.named_field != .not_named and difference.named_field != .equal) {
            census.bump("seal_diverged_named_identity");
        } else {
            census.bump("seal_diverged_unclassified");
        }
        if (!difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead()) {
            self.noteSealedAgainstChecked(record.address, sealed);
        }
    }

    /// Debug/probe-only: for a divergence where both sides carry content, ask
    /// whether the type the GRAPH sealed still agrees with the head CHECKING
    /// recorded at that position. The seam comparison is symmetric and cannot
    /// say which side is wrong; checking is the authority on logical types, so
    /// a sealed type contradicting the checked head is the graph's error
    /// (reunify.md 15.1b).
    fn noteSealedAgainstChecked(self: *Rehearsal, address: CheckedAddress, sealed: Type.TypeId) void {
        if (comptime !census.enabled) return;
        const cursor = self.lookup.cursor(address.module_bytes) orelse return;
        // Only a head no representation choice may alter counts as a
        // contradiction. Roc lowers an enum-like tag union to an integer,
        // unwraps a one-field record or tuple, and erases a zero-sized value,
        // so those shapes are conceded rather than reported (reunify.md 10).
        const agrees = switch (cursor.view.payload(@enumFromInt(address.type_id))) {
            .function => switch (self.types.get(sealed)) {
                .func, .zst, .erased => true,
                else => false,
            },
            .record => |record| switch (self.types.get(sealed)) {
                .record => true,
                else => record.fields.len <= 1,
            },
            .record_unbound => |fields| switch (self.types.get(sealed)) {
                .record => true,
                else => fields.len <= 1,
            },
            .tuple => |items| switch (self.types.get(sealed)) {
                .tuple => true,
                else => items.len <= 1,
            },
            .tag_union => |union_type| switch (self.types.get(sealed)) {
                .tag_union, .named => true,
                else => blk: {
                    // An enum-like union - every tag payload-free - lowers to an
                    // integer, which is a representation choice, not a defect.
                    for (union_type.tags) |tag| {
                        if (tag.args_len != 0) break :blk false;
                    }
                    break :blk true;
                },
            },
            // A nominal's runtime shape comes from its backing, which section 10
            // owns, so a differing head here says nothing about logical typing.
            .nominal => {
                census.bump("sealed_vs_checked_inconclusive");
                return;
            },
            else => {
                census.bump("sealed_vs_checked_inconclusive");
                return;
            },
        };
        if (agrees) {
            census.bump("sealed_agrees_with_checked_head");
        } else {
            census.bump("sealed_contradicts_checked_head");
        }
    }

    /// Compare, position by position, what this specialization's directed
    /// emission produces against what the graph sealed. Runs while the graph is
    /// still alive so a node's equivalence class still resolves.
    pub fn compareSpecialization(self: *Rehearsal, graph: *solve.InstGraph) void {
        if (comptime !census.enabled) return;
        if (!self.comparing) return;
        if (self.disabled) return;
        if (self.frames.items.len == 0) return;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) return;
        census.bump("rehearsal_spec_compared");

        var positions: std.AutoHashMapUnmanaged(CheckedAddress, Occurrences) = .empty;
        defer positions.deinit(self.allocator);

        const trace = frame.trace orelse return;
        var it = trace.provenance.iterator();
        while (it.next()) |entry| {
            const root = @intFromEnum(graph.rootNode(@enumFromInt(entry.key_ptr.*)));
            const sealed = trace.sealed.get(root) orelse continue;
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
        graph.seal_probe = null;
        if (self.frames.items.len == 0) return;
        var frame = self.frames.pop() orelse return;
        self.releaseFrame(&frame);
    }

    fn releaseFrame(self: *Rehearsal, frame: *Frame) void {
        if (frame.trace) |trace| {
            trace.deinit();
            self.allocator.destroy(trace);
        }
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
        return self.types.internTagUnion(self.program_names, &.{}) catch null;
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
            // Checking recorded no site for this edge, which is where the edge's
            // declared covering rule states the binding instead (reunify.md
            // sections 7.2, 9.6). The rule is consulted only here, so an edge
            // that does carry a site never sees it.
            error.NoSite => self.resolveEnvironmentFromCoveringRule(start, frame, edge),
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

    /// Resolve one specialization whose requesting edge carries a checked use
    /// site that recorded nothing, through the declared rule that edge cites
    /// (reunify.md sections 7.2, 9.6). Reports the skip class when the edge
    /// names no rule or the rule produced no binding.
    fn resolveEnvironmentFromCoveringRule(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        edge: RequestEdge,
    ) ?EdgeSkip {
        const covering = edge.covering_rule orelse return .no_site;
        const outcome = &self.generated_outcomes[@intFromEnum(covering.rule)];
        outcome.claimed += 1;
        const named: EdgeSkip = .{ .generated_request = covering.rule };
        const scheme_id = start.template_scheme orelse {
            census.bump("rehearsal_skip_generated_edge");
            return named;
        };
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            census.bump("rehearsal_skip_generated_edge");
            return named;
        };
        // A rule edge into a scheme with no binders has exactly one
        // instantiation; the ground path already resolves those exactly.
        if (scheme.gv_len == 0) {
            outcome.ground += 1;
            census.bump("rehearsal_skip_generated_edge");
            return named;
        }
        const declared_source = if (covering.rule.declaresBinderSource()) covering.source else null;
        const source = declared_source orelse {
            outcome.unbound += 1;
            census.bump("rehearsal_skip_generated_edge");
            census.bump("rehearsal_generated_rule_declared_unbound");
            return named;
        };
        if (self.bindGeneratedRule(start, frame, scheme_id, scheme, source, edge.caller, outcome)) {
            return null;
        }
        census.bump("rehearsal_skip_generated_edge");
        return named;
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
    /// under the exact structural witness the rule declares (reunify.md sections
    /// 7.5, 9.6). The mapping reads the receiver EMITTED under the requesting
    /// body's environment rather than the receiver's checked payload: a
    /// `where`-constrained dispatcher names only the constrained variable, whose
    /// value lives in the environment, and a receiver that is already a checked
    /// nominal reaches the same argument list either way. A binding without its
    /// witness is released and the specialization stays unresolved.
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

        const receiver_root = self.emitQuietly(caller, caller_env, caller_owner_node, source.receiver.checked_ty) orelse {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_untranslatable");
            return false;
        };
        const receiver = followEmittedPath(self.types, receiver_root, &source.receiver.path) orelse {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_path_absent");
            return false;
        };
        const argument_count = receiverArgumentCount(self.types, receiver) orelse {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_not_named");
            return false;
        };
        if (argument_count != binders.len) {
            outcome.receiver_unusable += 1;
            census.bump("rehearsal_generated_rule_receiver_arity_differs");
            return false;
        }

        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return false;
        };
        defer self.allocator.free(bound);
        for (0..binders.len) |index| {
            const argument = receiverArgumentAt(self.types, receiver, index) orelse {
                outcome.receiver_unusable += 1;
                census.bump("rehearsal_generated_rule_argument_untranslatable");
                return false;
            };
            if (self.carriesResidualMaterialization(argument)) {
                noteResidualOrigin(frame, self.classifyResidualActual(
                    caller,
                    caller_owner_node,
                    source.receiver.checked_ty,
                    caller_env,
                    caller_origin,
                ));
            }
            bound[index] = direct_translate.BoundType.of(argument);
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
        // The two witness forms compare different halves of the same emission:
        // a rule the requesting body hands a checked callable compares the whole
        // scheme root, and a rule that hands only a receiver compares the scheme
        // root's own dispatch argument against that receiver. The second form
        // names no requesting root, so its specialization contributes no
        // interface relation.
        const requested: ?Type.TypeId = switch (source.witness) {
            .callable => |callable| self.emitQuietly(caller, caller_env, caller_owner_node, callable),
            .receiver_at_argument => null,
        };
        const witness_left: ?Type.TypeId = switch (source.witness) {
            .callable => declared,
            .receiver_at_argument => |index| if (declared) |root| functionArgumentAt(self.types, root, index) else null,
        };
        const witness_right: ?Type.TypeId = switch (source.witness) {
            .callable => requested,
            .receiver_at_argument => receiver,
        };
        if (!self.generatedWitnessAgrees(witness_left, witness_right, outcome)) {
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
        const left_digest = self.types.typeDigest(self.program_names, left);
        const right_digest = self.types.typeDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            outcome.witness_agrees += 1;
            census.bump("rehearsal_generated_rule_witness_agrees");
            return true;
        }
        const left_unfolded = self.types.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, right);
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
        const left_digest = self.types.typeDigest(self.program_names, left);
        const right_digest = self.types.typeDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            census.bump("rehearsal_foreign_witness_agrees");
            return true;
        }
        const left_unfolded = self.types.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, right);
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
            if (HeadShape.of(self.types, ty).isEmptyTagUnionHead()) return true;
            var index: u32 = 0;
            while (childAt(self.types, ty, index)) |child| : (index += 1) {
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

    /// The innermost active environment whose binders name ids in `module_bytes`,
    /// asked without recording anything. The redundancy measurement below asks
    /// the same question `callerFrameFor` does, and must leave that function's
    /// counters exactly where the rehearsal put them.
    fn frameForModule(self: *Rehearsal, module_bytes: [32]u8) ?*const Frame {
        if (self.frames.items.len == 0) return null;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) return null;
        if (!std.mem.eql(u8, &frame.env_module_bytes, &module_bytes)) return null;
        return frame;
    }

    /// One resolved operand: the type the directed side computes for it, the
    /// store that type lives in, and both digests — the stored form, and the
    /// unfolded form that says two recursive types are one type under a
    /// different rooting (reunify.md section 8.3).
    const ResolvedOperand = struct {
        store: *const Type.Store,
        ty: Type.TypeId,
        stored: names.TypeDigest,
        unfolded: names.TypeDigest,
        /// What kind of operand this was, so a finding says whether it names a
        /// directed answer or a type the graph produced.
        origin: OperandOrigin,
        /// The checked position and environment the type came from, for an
        /// operand the site named as a checked type. An imported immutable type
        /// has none: nothing in the checked stores stands behind it here.
        source: ?CheckedSource,
    };

    /// The checked position one operand was translated from, kept so a residual
    /// materialization can be traced back to the variable that produced it.
    const CheckedSource = struct {
        view: checked.CheckedTypeStoreView,
        module_bytes: [32]u8,
        checked_ty: checked.CheckedTypeId,
        env: ?*const direct_translate.BindingEnvironment,
        /// The scheme owner the translation read residual dispositions under.
        owner_node: u32,
    };

    /// Measure whether one constraint-replay site's two sides are ALREADY the
    /// same type under directed translation, and record the answer in that site's
    /// row of the census table (reunify.md sections 9, 13 Slice 7). The site's own
    /// unification still runs and still decides lowering; this reads nothing from
    /// the graph and selects nothing.
    pub fn measureUnifySite(
        self: *Rehearsal,
        site: census.UnifySite,
        left: UnifyOperand,
        right: UnifyOperand,
    ) void {
        if (comptime !census.enabled) return;
        if (self.disabled) return;
        var blocker: census.UnifySiteBlocker = .operand_undescribed;
        const resolved_left = self.resolveOperand(left, &blocker) orelse {
            census.bumpUnifySite(site, .unmeasurable);
            census.bumpUnifySiteBlocker(site, blocker);
            return;
        };
        const resolved_right = self.resolveOperand(right, &blocker) orelse {
            census.bumpUnifySite(site, .unmeasurable);
            census.bumpUnifySiteBlocker(site, blocker);
            return;
        };
        if (std.mem.eql(u8, &resolved_left.stored.bytes, &resolved_right.stored.bytes) or
            std.mem.eql(u8, &resolved_left.unfolded.bytes, &resolved_right.unfolded.bytes))
        {
            census.bumpUnifySite(site, .redundant);
            return;
        }
        if (self.representationDecisionCovers(resolved_left, resolved_right)) {
            census.bumpUnifySite(site, .representation_decision);
            return;
        }
        if (self.openGraphPositionsCover(resolved_left, resolved_right)) {
            census.bumpUnifySite(site, .redundant);
            census.bumpUnifySiteOpenOnImport(site);
            return;
        }
        census.bumpUnifySite(site, .informative);
        self.classifyInformativeSite(site, resolved_left, resolved_right);
    }

    /// Whether every position this site's two sides differ at is one the
    /// graph-built side leaves open.
    ///
    /// A site names an operand as a graph-sealed Monotype when the constraint
    /// imports that Monotype into the graph, and the graph's own Monotype
    /// import reads an empty tag union there as a slot no value reached — it
    /// becomes an
    /// unresolved node again rather than a closed row, so the content the other
    /// side holds is what the slot receives. A difference at such a position is
    /// therefore the graph's own open node being filled, not information the
    /// constraint carries into the emitted program: the other side already holds
    /// it, and the flip deletes the node together with the constraint. Only a
    /// graph-sealed side may be open this way; an empty tag union on a checked
    /// position is that position's own directed answer, and a difference there
    /// stays informative.
    fn openGraphPositionsCover(
        self: *Rehearsal,
        left: ResolvedOperand,
        right: ResolvedOperand,
    ) bool {
        const left_open = left.origin == .graph_sealed;
        const right_open = right.origin == .graph_sealed;
        if (!left_open and !right_open) return false;
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        var covered = false;
        if (!walkOpenGraphPositions(.{
            .name_store = self.program_names,
            .left_store = left.store,
            .right_store = right.store,
            .left_open = left_open,
            .right_open = right_open,
        }, left.ty, right.ty, &visited, &covered, 0)) {
            return false;
        }
        return covered;
    }

    /// Whether this site's two sides are one logical type whose difference is
    /// entirely the representation content reunify.md section 10.3's rules move,
    /// and the shared representation policy covers the pair that moved. The
    /// policy is the same one section 10's closure engine applies, so a pair it
    /// covers is a decision that engine reproduces; a pair it does not stays a
    /// difference the site really carries.
    fn representationDecisionCovers(
        self: *Rehearsal,
        left: ResolvedOperand,
        right: ResolvedOperand,
    ) bool {
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        var covered = false;
        if (!walkRepresentationOnly(left.store, left.ty, right.store, right.ty, &visited, &covered, 0)) {
            return false;
        }
        return covered;
    }

    /// Name what an informative site's two sides disagree about, and keep one
    /// worked example per site so the classification is readable against a
    /// concrete pair rather than only as a count.
    fn classifyInformativeSite(
        self: *Rehearsal,
        site: census.UnifySite,
        left: ResolvedOperand,
        right: ResolvedOperand,
    ) void {
        var path: DifferencePath = .{};
        const difference = firstDifferenceOnPath(left.store, left.ty, right.store, right.ty, self.program_names, 0, &path);
        const information: census.UnifySiteInformation = information: {
            if (self.carriesRepresentation(left.store, left.ty) or
                self.carriesRepresentation(right.store, right.ty))
            {
                break :information .representation;
            }
            if (difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead()) {
                break :information self.residualClass(left);
            }
            if (difference.right.isEmptyTagUnionHead() and !difference.left.isEmptyTagUnionHead()) {
                break :information self.residualClass(right);
            }
            if (difference.left.tag != difference.right.tag) break :information .head_tag;
            if (difference.left.entries != difference.right.entries) break :information .row_width;
            if (difference.named_field != .not_named and difference.named_field != .equal) {
                break :information .named_identity;
            }
            break :information .unclassified;
        };
        if (information == .scheme_binder_unbound) {
            // Attribute the unbound binder: an unready callee level makes
            // `innermostCallee` decline, so the callee's own positions
            // translate under the requesting frame instead, which names none
            // of its binders. Anything counted without one came from a path
            // that opened no callee binding at all.
            if (self.hasUnreadyCallee()) {
                census.bump("rehearsal_binder_unbound_under_unready_callee");
            } else if (self.callees.items.len == 0) {
                census.bump("rehearsal_binder_unbound_no_callee_open");
            } else {
                census.bump("rehearsal_binder_unbound_callee_ready");
            }
        }
        census.bumpUnifySiteInformation(site, information);
        const residual = traceResidual(difference, path, left, right);
        census.bumpUnifySiteResidual(site, residual.origin, residual.state);
        // Two sides that are logically equal but stored under different rootings
        // were already accepted as redundant, so a difference reaching here is a
        // content difference and the detail is worth keeping.
        const slot = &self.unify_details[@intFromEnum(site)];
        if (slot.* != null) return;
        slot.* = .{
            .information = information,
            .left = HeadShape.of(left.store, left.ty),
            .right = HeadShape.of(right.store, right.ty),
            .difference = difference,
            .residual = residual,
        };
    }

    /// Name the side of an informative execution that carries the empty tag
    /// union at the difference, and say what stands behind that side: a checked
    /// position whose variable nothing names a value for, or a type the graph
    /// sealed with one of its own nodes still unresolved.
    ///
    /// The position is followed by the difference walk's own child path, so the
    /// finding names the checked variable the empty tag union came FROM rather
    /// than some other variable the operand's root happens to reach.
    fn traceResidual(
        difference: Difference,
        path: DifferencePath,
        left: ResolvedOperand,
        right: ResolvedOperand,
    ) ResidualTrace {
        const left_residual = difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead();
        const right_residual = difference.right.isEmptyTagUnionHead() and !difference.left.isEmptyTagUnionHead();
        if (!left_residual and !right_residual) return ResidualTrace.empty;
        const operand = if (left_residual) left else right;
        var trace = ResidualTrace.empty;
        trace.side = if (left_residual) .left else .right;
        trace.origin = operand.origin;
        const source = operand.source orelse return trace;
        trace.module_prefix = source.module_bytes[0..8].*;
        trace.checked_ty = @intFromEnum(source.checked_ty);
        trace.state = .position_not_followed;
        if (!path.complete) return trace;
        const position = checkedPositionAtPath(source.view, source.checked_ty, path) orelse return trace;
        trace.position = @intFromEnum(position);
        switch (source.view.payload(position)) {
            .flex, .rigid => {},
            else => {
                trace.state = .checked_content;
                return trace;
            },
        }
        trace.state = residualState(source, position);
        trace.defaults = variableDefaults(source.view, position);
        return trace;
    }

    /// The checked defaults one residual variable carries. A variable with no
    /// disposition, no numeric default phase and no row default has nothing at
    /// all naming its value, which is a different finding from one whose value
    /// is named and merely read under the wrong scope.
    fn variableDefaults(
        view: checked.CheckedTypeStoreView,
        free: checked.CheckedTypeId,
    ) VariableDefaults {
        return switch (view.payload(free)) {
            .flex => |v| .{
                .numeric_phase = v.numeric_default_phase != null,
                .row = v.row_default != null,
                .constraints = @intCast(v.constraints.len),
            },
            .rigid => |v| .{
                .rigid = true,
                .numeric_phase = v.numeric_default_phase != null,
                .row = v.row_default != null,
                .constraints = @intCast(v.constraints.len),
            },
            else => .{},
        };
    }

    /// Follow one difference path from a checked root to the checked position
    /// the emission's differing head came from.
    ///
    /// Aliases are transparent, and a function's arguments-then-result and a
    /// nominal's arguments carry the same child order in both the checked
    /// payload and the emission. A row does not: the emission flattens
    /// extension chains, so a step into a record, a tag union, or a nominal's
    /// backing names no checked child here and the walk stops instead of
    /// naming the wrong one.
    fn checkedPositionAtPath(
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
        path: DifferencePath,
    ) ?checked.CheckedTypeId {
        var current = transparentCheckedPosition(view, root);
        var index: u32 = 0;
        while (index < path.len) : (index += 1) {
            const step = path.steps[index];
            const next = switch (view.payload(current)) {
                .function => |fn_ty| blk: {
                    if (step < fn_ty.args.len) break :blk fn_ty.args[step];
                    if (step == fn_ty.args.len) break :blk fn_ty.ret;
                    return null;
                },
                .nominal => |nominal_ty| blk: {
                    if (step < nominal_ty.args.len) break :blk nominal_ty.args[step];
                    return null;
                },
                .tuple => |items| blk: {
                    if (step < items.len) break :blk items[step];
                    return null;
                },
                else => return null,
            };
            current = transparentCheckedPosition(view, next);
        }
        return current;
    }

    /// One checked position with its aliases walked through, bounded so a
    /// cyclic alias chain cannot spin.
    fn transparentCheckedPosition(
        view: checked.CheckedTypeStoreView,
        start: checked.CheckedTypeId,
    ) checked.CheckedTypeId {
        var current = start;
        var steps: u32 = 0;
        while (steps < max_difference_depth) : (steps += 1) {
            switch (view.payload(current)) {
                .alias => |alias_ty| current = alias_ty.backing,
                else => return current,
            }
        }
        return current;
    }

    /// What names the value of one unbound checked variable: a scheme's binder
    /// list, a residual disposition under one of the scopes the translation
    /// reads, or nothing at all.
    fn residualState(
        source: CheckedSource,
        free: checked.CheckedTypeId,
    ) ResidualState {
        for (source.view.schemes) |scheme| {
            for (scheme.generalizedVars(source.view)) |binder| {
                if (binder == free) return .scheme_binder;
            }
        }
        var module_wide = false;
        var other_owner = false;
        for (source.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(free)) continue;
            if (disposition.scheme_owner_node == source.owner_node) {
                return switch (disposition.kind) {
                    .contextual => .disposed_contextual,
                    .uninhabited => .disposed_uninhabited,
                };
            }
            if (disposition.scheme_owner_node == checked.checked_residual_disposition_module_body_owner) {
                module_wide = true;
            } else {
                other_owner = true;
            }
        }
        if (module_wide) return .disposed_module_body;
        if (other_owner) return .disposed_other_owner;
        return .undisposed;
    }

    /// Record that a site builds one graph node out of a placeholder and its
    /// content rather than relating two independently derived types, so the table
    /// keeps the node construction the flip deletes outright apart from the
    /// constraints it has to account for.
    pub fn noteUnifyConstruction(self: *Rehearsal, site: census.UnifySite) void {
        if (comptime !census.enabled) return;
        if (self.disabled) return;
        census.bumpUnifySite(site, .construction);
    }

    /// Which residual class one operand's empty-tag-union materialization falls
    /// in: `scheme_binder_unbound` when the first checked variable that operand
    /// reaches and this environment does not bind is a generalized binder of a
    /// checked scheme — the value reunify.md section 9's directed instantiation
    /// takes from the checker's recorded substitution — and `unbound_residual`
    /// when no recorded substitution names a value for it.
    fn residualClass(self: *Rehearsal, operand: ResolvedOperand) census.UnifySiteInformation {
        const source = operand.source orelse return .unbound_residual;
        const free = self.firstFreeVariable(source.view, source.checked_ty, source.env) orelse
            return .unbound_residual;
        // How many schemes in this view generalize the same variable. The walk
        // below reports the first, so more than one would make the owner it
        // names iteration order rather than a property of the variable.
        var owners: usize = 0;
        for (source.view.schemes) |scheme| {
            for (scheme.generalizedVars(source.view)) |binder| {
                if (binder == free) {
                    owners += 1;
                    break;
                }
            }
        }
        if (owners > 1) {
            census.bump("rehearsal_unbound_binder_owned_by_many_schemes");
        } else if (owners == 1) {
            census.bump("rehearsal_unbound_binder_owned_by_one_scheme");
        }
        for (source.view.schemes) |scheme| {
            for (scheme.generalizedVars(source.view)) |binder| {
                if (binder != free) continue;
                // Which scheme owns the unnamed binder. The one this operand
                // was translated under is a binding that stated its own binders
                // and still left this position open; any other scheme is a
                // binder the operand reaches from outside the bound scheme,
                // which a call-site binding never states (reunify.md 7.3).
                if (scheme.owner_node == source.owner_node) {
                    census.bump("rehearsal_unbound_binder_of_translating_scheme");
                } else if (self.frameForModule(source.module_bytes)) |frame| {
                    // Whether the unnamed binder is generalized by the scheme
                    // the REQUESTING frame specializes. A callee-attributed
                    // operand reaching one is a caller-side position described
                    // as the callee's, not a binding the callee lacks.
                    if (scheme.owner_node == frame.owner_node) {
                        census.bump("rehearsal_unbound_binder_of_caller_frame_scheme");
                    } else {
                        census.bump("rehearsal_unbound_binder_of_third_scheme");
                        // Whether the definition owning this binder is itself
                        // specializing somewhere in the active frame stack.
                        // `frameForModule` consults only the innermost frame,
                        // so a value an outer frame already holds is invisible
                        // to the position that needs it.
                        var found_outer = false;
                        for (self.frames.items) |*outer| {
                            if (!outer.env_ready) continue;
                            if (!std.mem.eql(u8, &outer.env_module_bytes, &source.module_bytes)) continue;
                            if (outer.owner_node != scheme.owner_node) continue;
                            found_outer = true;
                            break;
                        }
                        if (found_outer) {
                            census.bump("rehearsal_unbound_binder_third_in_outer_frame");
                        } else {
                            census.bump("rehearsal_unbound_binder_third_no_frame_anywhere");
                        }
                        // Whether the checked data names this definition's
                        // instantiation anywhere in the module the position
                        // lives in. If it does, the value exists and only the
                        // key that selects it is missing from the operand; if
                        // it does not, no recorded edge states it at all.
                        if (self.lookup.cursor(source.module_bytes)) |cursor| {
                            var named = false;
                            for (cursor.view.instantiationSites()) |site| {
                                if (site.scheme_owner_node == scheme.owner_node) {
                                    named = true;
                                    break;
                                }
                            }
                            if (named) {
                                census.bump("rehearsal_unbound_binder_third_has_recorded_site");
                                // Whether some use expression carries a site for
                                // BOTH this definition and the scheme the
                                // operand translates under. Where it does, one
                                // key selects both levels, and a binding built
                                // per site at that use states this binder
                                // without any new checked data.
                                var co_located = false;
                                for (cursor.view.instantiationSites()) |third| {
                                    if (third.scheme_owner_node != scheme.owner_node) continue;
                                    const third_use = third.useExpr() orelse continue;
                                    for (cursor.view.instantiationSites()) |own| {
                                        if (own.scheme_owner_node != source.owner_node) continue;
                                        const own_use = own.useExpr() orelse continue;
                                        if (own_use == third_use) {
                                            co_located = true;
                                            break;
                                        }
                                    }
                                    if (co_located) break;
                                }
                                if (co_located) {
                                    census.bump("rehearsal_unbound_binder_third_co_located_use");
                                } else {
                                    census.bump("rehearsal_unbound_binder_third_separate_use");
                                }
                            } else {
                                census.bump("rehearsal_unbound_binder_third_has_no_recorded_site");
                            }
                        }
                        // What kind of definition that third scheme is, which
                        // says whether the position names a top-level value, an
                        // inner generalization boundary, a platform requirement,
                        // or a template scheme with no source owner.
                        switch (scheme.owner_kind) {
                            .top_level_def => census.bump("rehearsal_unbound_binder_third_top_level"),
                            .nested_def => census.bump("rehearsal_unbound_binder_third_nested"),
                            .required_type => census.bump("rehearsal_unbound_binder_third_required"),
                            .synthetic => census.bump("rehearsal_unbound_binder_third_synthetic"),
                        }
                        if (scheme.gv_len > 1) census.bump("rehearsal_unbound_binder_third_multi_binder");
                    }
                } else {
                    census.bump("rehearsal_unbound_binder_no_frame");
                }
                return .scheme_binder_unbound;
            }
        }
        return .unbound_residual;
    }

    /// The directed side's answer for one operand, or null with the reason it
    /// has none. A checked operand translates under the innermost active
    /// environment when that environment's module is the operand's, and as a
    /// ground type otherwise — exactly the rule `comparePosition` uses.
    fn resolveOperand(
        self: *Rehearsal,
        operand: UnifyOperand,
        blocker: *census.UnifySiteBlocker,
    ) ?ResolvedOperand {
        switch (operand) {
            .undescribed => {
                blocker.* = .operand_undescribed;
                return null;
            },
            .sealed => |ty| return .{
                .store = self.types,
                .ty = ty,
                .stored = self.types.typeDigest(self.program_names, ty),
                .unfolded = self.types.unfoldedDigest(self.program_names, ty),
                .origin = .graph_sealed,
                .source = null,
            },
            .checked => |address| return self.resolveCheckedOperand(address, false, blocker),
            .callee_checked => |address| return self.resolveCheckedOperand(address, true, blocker),
            .field_of => |field| {
                const receiver = self.resolveCheckedOperand(field.receiver, false, blocker) orelse return null;
                const emitted = switch (fieldOfEmitted(receiver.store, receiver.ty, field.label)) {
                    .field => |ty| ty,
                    .receiver_not_a_record => {
                        blocker.* = .field_receiver_not_a_record;
                        return null;
                    },
                    .label_absent => {
                        blocker.* = .field_label_absent;
                        return null;
                    },
                };
                return .{
                    .store = receiver.store,
                    .ty = emitted,
                    .stored = receiver.store.typeDigest(self.program_names, emitted),
                    .unfolded = receiver.store.unfoldedDigest(self.program_names, emitted),
                    .origin = .field_of_checked,
                    .source = null,
                };
            },
        }
    }

    /// The Monotype at one checked position of the specialization being lowered
    /// (reunify.md sections 9.1, 9.2): directed instantiation of the checked
    /// type under the binding the checker recorded, with no logical solving.
    ///
    /// `under_callee` reads the position under the innermost open callee
    /// binding when one resolved for its module — a body lowering a CALLEE's own
    /// checked positions reads them under the binding the checker recorded for
    /// the edge, not under the requesting body's.
    ///
    /// Null means the checked data did not name a type here: the module is
    /// outside this lowering input, or the walk left the translatable subset.
    /// How the environment one checked position was read under resolved.
    pub const PositionBinding = enum {
        /// Read under the innermost open callee binding.
        callee,
        /// Read under the active specialization frame for the position's module.
        frame,
        /// No environment named the position's module.
        none,
    };

    /// Debug/probe-only: translate a position after adding a level for the
    /// definition its unbound variable belongs to, bound from the site the
    /// entering edge names (reunify.md 13.2 2a). The existing levels are kept
    /// underneath: 753 of the divergences already receive a frame binding, so
    /// what is wrong is a missing level for another definition, not the
    /// environment as a whole.
    fn typeUnderEdgeLevel(
        self: *Rehearsal,
        address: CheckedAddress,
        base_env: ?*const direct_translate.BindingEnvironment,
        base_owner_node: u32,
        edge: ?RequestEdgeName,
    ) ?Type.TypeId {
        const cursor = self.lookup.cursor(address.module_bytes) orelse return null;
        const free = self.firstFreeVariable(cursor.view, @enumFromInt(address.type_id), base_env) orelse return null;
        var owner_node: ?u32 = null;
        for (cursor.view.schemes) |scheme| {
            for (scheme.generalizedVars(cursor.view)) |binder| {
                if (binder == free) {
                    owner_node = scheme.owner_node;
                    break;
                }
            }
            if (owner_node != null) break;
        }
        const owner = owner_node orelse return null;
        const scheme_id = cursor.view.schemeIdForOwnerNode(owner) orelse return null;
        const scheme = cursor.view.schemeById(scheme_id) orelse return null;
        if (scheme.captured_len != 0) return null;
        const caller = if (edge) |named|
            self.lookup.cursor(named.module_bytes) orelse return null
        else
            cursor;
        // With an entering edge the site is keyed at its use expression.
        // Without one, a definition the module instantiates EXACTLY ONCE still
        // names its binding unambiguously; more than one and nothing says which
        // applies, so no level is built (reunify.md 13.2 2a).
        const site = if (edge) |named|
            self.siteQuietly(caller, named.use_expr, owner) orelse return null
        else site: {
            var found: ?checked.CheckedInstantiationSite = null;
            for (cursor.view.instantiationSites()) |candidate| {
                if (candidate.scheme_owner_node != owner) continue;
                if (found != null) {
                    census.bump("unique_site_ambiguous");
                    return null;
                }
                found = candidate;
            }
            if (found == null) {
                census.bump("unique_site_absent");
                return null;
            }
            census.bump("level_from_unique_site");
            break :site found.?;
        };

        const binders = scheme.generalizedVars(cursor.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len or binders.len == 0) return null;
        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch return null;
        defer self.allocator.free(bound);
        for (actuals, 0..) |actual, index| {
            if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) return null;
            const translated = self.emitQuietly(caller, base_env, base_owner_node, actual) orelse return null;
            bound[index] = direct_translate.BoundType.of(translated);
        }
        var depth: usize = 0;
        var walk = base_env;
        while (walk) |level| : (walk = level.parent) depth += 1;
        var chain = self.copyEnvironmentChain(base_env, depth, .{
            .scheme = .{ .module_bytes = cursor.module_bytes, .scheme = @intFromEnum(scheme_id) },
            .binders = binders,
            .bound = bound,
            .captured = &.{},
        }) orelse return null;
        defer chain.release(self.allocator);
        census.bump("position_bound_by_edge_level");
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            chain.innermost(),
            base_owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch null;
    }

    pub fn typeForCheckedPosition(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        binding: *PositionBinding,
    ) Allocator.Error!?Type.TypeId {
        return self.typeForCheckedPositionWithEdge(address, under_callee, binding, null);
    }

    /// The same, first trying a level for the definition the position's unbound
    /// variable belongs to, bound from the site the entering edge names.
    pub fn typeForCheckedPositionWithEdge(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        binding: *PositionBinding,
        edge: ?RequestEdgeName,
    ) Allocator.Error!?Type.TypeId {
        const cursor = self.lookup.cursor(address.module_bytes) orelse return null;
        var env: ?*const direct_translate.BindingEnvironment = null;
        var owner_node = checked.checked_residual_disposition_module_body_owner;
        binding.* = .none;
        const callee = if (under_callee) self.innermostCallee(address.module_bytes) else null;
        if (callee) |level| {
            env = level.chain.innermost();
            owner_node = level.owner_node;
            binding.* = .callee;
        } else if (self.frameForModule(address.module_bytes)) |frame| {
            env = frame.environment();
            owner_node = frame.owner_node;
            binding.* = .frame;
        }
        if (self.typeUnderEdgeLevel(address, env, owner_node, edge)) |leveled| return leveled;
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            env,
            owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch |err| switch (err) {
            error.Skip => null,
            else => |other| other,
        };
    }

    /// Translate one checked position. `under_callee` reads it under the
    /// innermost open callee binding when one resolved for its module, which is
    /// the binding reunify.md section 9.1 instantiates the callee scheme at;
    /// otherwise, and for every requesting-body position, it reads under the
    /// innermost active specialization environment exactly as `comparePosition`
    /// does.
    fn resolveCheckedOperand(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        blocker: *census.UnifySiteBlocker,
    ) ?ResolvedOperand {
        const cursor = self.lookup.cursor(address.module_bytes) orelse {
            blocker.* = .operand_module_absent;
            return null;
        };
        var env: ?*const direct_translate.BindingEnvironment = null;
        var owner_node = checked.checked_residual_disposition_module_body_owner;
        const callee = if (under_callee) self.innermostCallee(address.module_bytes) else null;
        if (callee) |level| {
            env = level.chain.innermost();
            owner_node = level.owner_node;
        } else if (self.frameForModule(address.module_bytes)) |frame| {
            env = frame.environment();
            owner_node = frame.owner_node;
        }
        const checked_ty: checked.CheckedTypeId = @enumFromInt(address.type_id);
        var reason: direct_translate.SkipReason = undefined;
        const emitted = self.translator.translateUnderEnvironment(
            cursor,
            env,
            owner_node,
            checked_ty,
            &reason,
        ) catch |err| switch (err) {
            error.Skip => {
                blocker.* = switch (reason) {
                    .binder_not_found => .no_environment,
                    .recursive_cycle => .operand_recursive,
                    .open_row => .operand_open_row,
                    .engine_input_needed => .operand_engine_input_needed,
                    .pending_or_err => .operand_pending_or_err,
                    .numeric_default_unresolved => .operand_numeric_default,
                    .malformed_builtin_arity => .operand_malformed_arity,
                    .missing_backing => .operand_missing_backing,
                };
                return null;
            },
            else => {
                self.fail();
                blocker.* = .operand_untranslatable;
                return null;
            },
        };
        return .{
            .store = self.types,
            .ty = emitted,
            .stored = self.types.typeDigest(self.program_names, emitted),
            .unfolded = self.types.unfoldedDigest(self.program_names, emitted),
            .origin = .checked_position,
            .source = .{
                .view = cursor.view,
                .module_bytes = address.module_bytes,
                .checked_ty = checked_ty,
                .env = env,
                .owner_node = owner_node,
            },
        };
    }

    /// The innermost resolved callee binding whose binders name ids in
    /// `module_bytes`, or null when the innermost binding named none.
    /// Whether the innermost open callee binding failed to resolve, which is
    /// what makes `innermostCallee` decline and sends a callee's own checked
    /// positions through the requesting frame's environment instead.
    fn hasUnreadyCallee(self: *const Rehearsal) bool {
        if (self.callees.items.len == 0) return false;
        return !self.callees.items[self.callees.items.len - 1].ready;
    }

    fn innermostCallee(self: *Rehearsal, module_bytes: [32]u8) ?*const CalleeLevel {
        if (self.callees.items.len == 0) return null;
        const level = &self.callees.items[self.callees.items.len - 1];
        if (!level.ready) return null;
        if (!std.mem.eql(u8, &level.module_bytes, &module_bytes)) return null;
        return level;
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

        const emitted_digest = self.types.typeDigest(self.program_names, emitted);
        var matched = false;
        for (occurrences.ids[0..occurrences.len]) |sealed| {
            census.bump("rehearsal_type_compared");
            const sealed_digest = self.types.typeDigest(self.program_names, sealed);
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
            const emitted_unfolded = self.types.unfoldedDigest(self.program_names, emitted);
            const sealed_unfolded = self.types.unfoldedDigest(self.program_names, sealed);
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
        const difference = firstDifference(self.types, emitted, self.types, sealed, self.program_names, 0);
        // A difference outside the residual-materialization class is a finding of
        // its own, so its detail is always dumped: the bounded budget exists to
        // stop the residual class from filling the file, not to hide the rest.
        var beyond_residual_class = true;
        if (representation) {
            census.bump("rehearsal_type_mismatch_representation");
        } else {
            census.bump("rehearsal_type_mismatch_logical");
            if (!difference.left.isEmptyTagUnionHead() and !difference.right.isEmptyTagUnionHead()) {
                // Both sides carry content, so the comparison cannot say which
                // is wrong. Checking is the authority on logical types, so ask
                // whether the SEALED type still agrees with the head checking
                // recorded at this position (reunify.md 15.1b).
                self.noteSealedAgainstChecked(address, sealed);
            }
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
            .rehearsal_head = HeadShape.of(self.types, emitted),
            .graph_head = HeadShape.of(self.types, sealed),
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
        return self.carriesRepresentation(self.types, root);
    }

    fn emittedCarriesRepresentation(self: *Rehearsal, root: Type.TypeId) bool {
        return self.carriesRepresentation(self.types, root);
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
        switch (self.types.get(ty)) {
            .list, .box => |elem| {
                const child = self.slotForEmitted(elem, depth + 1) orelse return null;
                return .{ .wrapper = child };
            },
            .named => |named| {
                const owner = named.builtin_owner;
                if (owner != null and static_dispatch.isIteratorOwner(owner.?)) {
                    const args = self.types.span(named.args);
                    if (GuardedList.borrowLen(args) >= 1) {
                        const item = self.slotForEmitted(GuardedList.at(args, 0), depth + 1) orelse return null;
                        const backing = if (named.backing) |backing_ty|
                            (self.slotForEmitted(backing_ty.ty, depth + 1) orelse return null)
                        else
                            (self.standInBacking() orelse return null);
                        return .{ .iterator = .{
                            .descriptor = descriptorOf(named),
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
        const digest = self.types.typeDigest(self.program_names, ty);
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

    /// One line per constraint-replay site that came out informative in this
    /// compilation, naming what its two sides disagreed about.
    fn dumpUnifyDetails(self: *Rehearsal) void {
        if (comptime !census.enabled) return;
        const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(self.allocator);
        for (self.unify_details, 0..) |maybe_detail, index| {
            const detail = maybe_detail orelse continue;
            const site: census.UnifySite = @enumFromInt(index);
            const line = std.fmt.allocPrint(
                self.allocator,
                "rehearsal_unify_detail site={s} information={s} left={s}:{d}/{d} right={s}:{d}/{d}" ++
                    " differs_at_depth={d} {s}:{d}/{d}vs{s}:{d}/{d} named_field={s} recursive={d}/{d}" ++
                    " residual_side={s} residual_origin={s} residual_state={s}" ++
                    " residual_module={s} residual_checked_ty={d} residual_position={d}" ++
                    " residual_rigid={d} residual_numeric_phase={d} residual_row_default={d}" ++
                    " residual_constraints={d}\n",
                .{
                    @tagName(site),
                    @tagName(detail.information),
                    @tagName(detail.left.tag),
                    detail.left.children,
                    detail.left.entries,
                    @tagName(detail.right.tag),
                    detail.right.children,
                    detail.right.entries,
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
                    @tagName(detail.residual.side),
                    @tagName(detail.residual.origin),
                    @tagName(detail.residual.state),
                    &std.fmt.bytesToHex(detail.residual.module_prefix, .lower),
                    detail.residual.checked_ty,
                    detail.residual.position,
                    @intFromBool(detail.residual.defaults.rigid),
                    @intFromBool(detail.residual.defaults.numeric_phase),
                    @intFromBool(detail.residual.defaults.row),
                    detail.residual.defaults.constraints,
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
        self.dumpUnifyDetails();
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

/// How many positional arguments an emitted receiver carries, or null when the
/// emission is not a shape a declared rule's binder mapping reads. This is the
/// only shape the mapping reads: positions of the type the generating site
/// dispatched on, never a match against the concrete callable (reunify.md
/// sections 9.5, 9.6). The count is what decides arity: every declared mapping
/// is positional and total, so a receiver carrying more or fewer arguments than
/// the callee has binders names a different generator than the rule declares.
/// `List` and `Box` emit their element as the structural shape rather than a
/// named node, so their single argument is read from that shape.
fn receiverArgumentCount(store: *const Type.Store, receiver: Type.TypeId) ?usize {
    return switch (store.get(receiver)) {
        .named => |named| GuardedList.borrowLen(store.span(named.args)),
        .list, .box => 1,
        else => null,
    };
}

/// Apply a rule's declared emitted path to the emission of its checked receiver
/// type, or null when the emission carries no such position (reunify.md section
/// 9.6). Each step reads exactly the component its declaration names — no step
/// searches for a shape that would fit, and a path that does not land is a
/// binding the rule refuses rather than one it approximates.
fn followEmittedPath(store: *const Type.Store, root: Type.TypeId, path: *const EmittedPath) ?Type.TypeId {
    var current = root;
    for (path.declaredSteps()) |step| {
        current = switch (step) {
            .nominal_backing => switch (store.get(current)) {
                .named => |named| (named.backing orelse return null).ty,
                else => return null,
            },
            .record_field => |label| switch (store.get(current)) {
                .record => |fields| blk: {
                    const entries = store.fieldSpan(fields);
                    var found: ?Type.TypeId = null;
                    for (0..GuardedList.borrowLen(entries)) |index| {
                        const field = GuardedList.at(entries, index);
                        if (field.name != label) continue;
                        found = field.ty;
                    }
                    break :blk found orelse return null;
                },
                else => return null,
            },
            .tuple_element => |index| switch (store.get(current)) {
                .tuple => |items| blk: {
                    const entries = store.span(items);
                    if (index >= GuardedList.borrowLen(entries)) return null;
                    break :blk GuardedList.at(entries, index);
                },
                else => return null,
            },
            .tag_payload => |payload| switch (store.get(current)) {
                .tag_union => |tags| blk: {
                    const entries = store.tagSpan(tags);
                    var found: ?Type.TypeId = null;
                    for (0..GuardedList.borrowLen(entries)) |index| {
                        const tag = GuardedList.at(entries, index);
                        if (tag.name != payload.name) continue;
                        const payloads = store.span(tag.payloads);
                        if (payload.index >= GuardedList.borrowLen(payloads)) return null;
                        found = GuardedList.at(payloads, payload.index);
                    }
                    break :blk found orelse return null;
                },
                else => return null,
            },
        };
    }
    return current;
}

/// Walk two types in parallel and report whether every difference between them
/// is representation content reunify.md section 10.3's rules move over a pair
/// the shared policy covers. `covered` is set once such a pair is reached, so a
/// walk that returns true without setting it found two identical types and a
/// walk that returns false found a difference the rules do not move.
///
/// Recursion closes on the visited pair set, and a pair deeper than the
/// difference budget is refused rather than assumed equal.
fn walkRepresentationOnly(
    left_store: *const Type.Store,
    left: Type.TypeId,
    right_store: *const Type.Store,
    right: Type.TypeId,
    visited: *std.AutoHashMap(u64, void),
    covered: *bool,
    depth: u32,
) bool {
    if (depth >= max_difference_depth) return false;
    const key = (@as(u64, @intFromEnum(left)) << 32) | @as(u64, @intFromEnum(right));
    const seen = visited.getOrPut(key) catch return false;
    if (seen.found_existing) return true;

    const left_content = left_store.get(left);
    if (std.meta.activeTag(left_content) != std.meta.activeTag(right_store.get(right))) return false;
    if (left_content == .primitive) {
        return left_content.primitive == right_store.get(right).primitive;
    }
    if (left_content == .named) {
        if (NamedFieldDifference.ofIgnoringRepresentation(left_store, left, right_store, right) != .equal) {
            return false;
        }
        if (NamedFieldDifference.of(left_store, left, right_store, right) != .equal) {
            if (!representationPolicyCovers(left_store, left, right_store, right)) return false;
            covered.* = true;
        }
    } else if (!rowLabelsEqual(left_store, left, right_store, right)) {
        // A row's labels are part of its logical identity and no rule moves
        // them, so two rows that disagree on labels are two types.
        return false;
    }
    const left_shape = HeadShape.of(left_store, left);
    const right_shape = HeadShape.of(right_store, right);
    if (left_shape.children != right_shape.children or left_shape.entries != right_shape.entries) return false;
    var index: u32 = 0;
    while (index < left_shape.children) : (index += 1) {
        const left_child = childAt(left_store, left, index) orelse return false;
        const right_child = childAt(right_store, right, index) orelse return false;
        if (!walkRepresentationOnly(
            left_store,
            left_child,
            right_store,
            right_child,
            visited,
            covered,
            depth + 1,
        )) return false;
    }
    return true;
}

/// The two stores one open-position walk compares, and which side the site
/// imports as a graph-sealed Monotype.
const OpenGraphWalk = struct {
    name_store: *const names.NameStore,
    left_store: *const Type.Store,
    right_store: *const Type.Store,
    left_open: bool,
    right_open: bool,
};

/// Walk two types in parallel and report whether every difference between them
/// is a position the graph-built side leaves open. `covered` is set once such a
/// position is reached, so a walk that returns true without setting it found two
/// identical types.
///
/// Recursion closes on the visited pair set, and a pair deeper than the
/// difference budget is refused rather than assumed equal.
fn walkOpenGraphPositions(
    walk: OpenGraphWalk,
    left: Type.TypeId,
    right: Type.TypeId,
    visited: *std.AutoHashMap(u64, void),
    covered: *bool,
    depth: u32,
) bool {
    if (depth >= max_difference_depth) return false;
    const key = (@as(u64, @intFromEnum(left)) << 32) | @as(u64, @intFromEnum(right));
    const seen = visited.getOrPut(key) catch return false;
    if (seen.found_existing) return true;

    const left_digest = walk.left_store.unfoldedDigest(walk.name_store, left);
    const right_digest = walk.right_store.unfoldedDigest(walk.name_store, right);
    if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) return true;

    const left_shape = HeadShape.of(walk.left_store, left);
    const right_shape = HeadShape.of(walk.right_store, right);
    if (walk.left_open and left_shape.isEmptyTagUnionHead() and !right_shape.isEmptyTagUnionHead()) {
        covered.* = true;
        return true;
    }
    if (walk.right_open and right_shape.isEmptyTagUnionHead() and !left_shape.isEmptyTagUnionHead()) {
        covered.* = true;
        return true;
    }

    const left_content = walk.left_store.get(left);
    if (std.meta.activeTag(left_content) != std.meta.activeTag(walk.right_store.get(right))) return false;
    if (left_content == .primitive) {
        return left_content.primitive == walk.right_store.get(right).primitive;
    }
    // A named head's declared identity and a row's labels are the type's own
    // content, so a disagreement there is two types rather than one open slot.
    if (left_content == .named) {
        if (NamedFieldDifference.of(walk.left_store, left, walk.right_store, right) != .equal) return false;
    } else if (!rowLabelsEqual(walk.left_store, left, walk.right_store, right)) {
        return false;
    }
    if (left_shape.children != right_shape.children or left_shape.entries != right_shape.entries) return false;
    var index: u32 = 0;
    while (index < left_shape.children) : (index += 1) {
        const left_child = childAt(walk.left_store, left, index) orelse return false;
        const right_child = childAt(walk.right_store, right, index) orelse return false;
        if (!walkOpenGraphPositions(walk, left_child, right_child, visited, covered, depth + 1)) return false;
    }
    return true;
}

/// Whether the site's own scheme identity is this callee's, in which case the
/// positional actuals are a vector over exactly these binders and no further
/// witness is needed.
fn siteNamesScheme(
    site: checked.CheckedInstantiationSite,
    defining: direct_translate.ModuleCursor,
    scheme_id: checked.CheckedTypeSchemeId,
) bool {
    const named = site.schemeId() orelse return false;
    if (named != scheme_id) return false;
    const imported = site.importedDefiningModule() orelse return true;
    return std.mem.eql(u8, &imported, &defining.module_bytes);
}

/// Whether two rows carry the same labels in the same order. Records and tag
/// unions carry them; every other head has none and answers true.
fn rowLabelsEqual(
    left_store: *const Type.Store,
    left: Type.TypeId,
    right_store: *const Type.Store,
    right: Type.TypeId,
) bool {
    switch (left_store.get(left)) {
        .record => |left_span| {
            const right_span = switch (right_store.get(right)) {
                .record => |span| span,
                else => return false,
            };
            const left_fields = left_store.fieldSpan(left_span);
            const right_fields = right_store.fieldSpan(right_span);
            const count = GuardedList.borrowLen(left_fields);
            if (count != GuardedList.borrowLen(right_fields)) return false;
            for (0..count) |index| {
                if (GuardedList.at(left_fields, index).name != GuardedList.at(right_fields, index).name) return false;
            }
            return true;
        },
        .tag_union => |left_span| {
            const right_span = switch (right_store.get(right)) {
                .tag_union => |span| span,
                else => return false,
            };
            const left_tags = left_store.tagSpan(left_span);
            const right_tags = right_store.tagSpan(right_span);
            const count = GuardedList.borrowLen(left_tags);
            if (count != GuardedList.borrowLen(right_tags)) return false;
            for (0..count) |index| {
                const left_tag = GuardedList.at(left_tags, index);
                const right_tag = GuardedList.at(right_tags, index);
                if (left_tag.name != right_tag.name) return false;
                if (GuardedList.borrowLen(left_store.span(left_tag.payloads)) !=
                    GuardedList.borrowLen(right_store.span(right_tag.payloads))) return false;
            }
            return true;
        },
        else => return true,
    }
}

/// Whether the shared representation policy declares a relation for two named
/// heads that agree on everything except the representation fields: an iterator
/// tier the policy relates, or a generated evidence owner whose backing it
/// selects by score (reunify.md section 10.3).
fn representationPolicyCovers(
    left_store: *const Type.Store,
    left: Type.TypeId,
    right_store: *const Type.Store,
    right: Type.TypeId,
) bool {
    const left_named = switch (left_store.get(left)) {
        .named => |named| named,
        else => return false,
    };
    const right_named = switch (right_store.get(right)) {
        .named => |named| named,
        else => return false,
    };
    const left_descriptor = descriptorOf(left_named);
    const right_descriptor = descriptorOf(right_named);
    // Both operands are sealed emissions carrying their recorded generated
    // identity, so neither states a minting identity and the policy's component
    // question does not arise.
    if (policy.iteratorTierRelation(left_descriptor, right_descriptor, .differ) != .ordinary) return true;
    return policy.evidenceOwnerUsesScoreSelection(left_named.builtin_owner) and
        left_named.builtin_owner == right_named.builtin_owner;
}

/// The type at one record field of an emitted receiver, following named
/// backings the way a field read reaches the row through them. Null when the
/// emission carries no record with that label.
fn fieldOfEmitted(store: *const Type.Store, receiver: Type.TypeId, label: names.RecordFieldNameId) FieldOfEmitted {
    var current = receiver;
    var steps: usize = 0;
    while (steps < max_slot_depth) : (steps += 1) {
        switch (store.get(current)) {
            .named => |named| current = (named.backing orelse return .receiver_not_a_record).ty,
            .record => |fields| {
                const entries = store.fieldSpan(fields);
                for (0..GuardedList.borrowLen(entries)) |index| {
                    const field = GuardedList.at(entries, index);
                    if (field.name == label) return .{ .field = field.ty };
                }
                return .label_absent;
            },
            else => return .receiver_not_a_record,
        }
    }
    return .receiver_not_a_record;
}

/// What reading one label off an emitted receiver produced: the field's type,
/// or exactly which of the two ways the read had no answer. The split names
/// which operand a `field_of` site could not translate and why, so an
/// unmeasurable execution is never left as an unexplained blocker.
const FieldOfEmitted = union(enum) {
    field: Type.TypeId,
    /// The receiver's translation is not a record, and unwrapping named
    /// backings did not reach one — most often because the receiver's checked
    /// position reaches a variable no binding names, so it translates to the
    /// empty tag union.
    receiver_not_a_record,
    /// The receiver's translation IS a record, and carries no such label.
    label_absent,
};

/// The emitted receiver's argument at `index`, or null when the emission carries
/// no such position.
fn receiverArgumentAt(store: *const Type.Store, receiver: Type.TypeId, index: usize) ?Type.TypeId {
    return switch (store.get(receiver)) {
        .named => |named| blk: {
            const args = store.span(named.args);
            if (index >= GuardedList.borrowLen(args)) break :blk null;
            break :blk GuardedList.at(args, index);
        },
        .list, .box => |elem| if (index == 0) elem else null,
        else => null,
    };
}

/// The argument at `index` of an emitted function type, or null when the type is
/// not a function or carries no such argument. The receiver-position witness
/// reads the callee scheme root through this.
fn functionArgumentAt(store: *const Type.Store, root: Type.TypeId, index: u32) ?Type.TypeId {
    return switch (store.get(root)) {
        .func => |fn_ty| blk: {
            const args = store.span(fn_ty.args);
            if (index >= GuardedList.borrowLen(args)) break :blk null;
            break :blk GuardedList.at(args, index);
        },
        else => null,
    };
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
fn descriptorOf(named: Type.NamedContent) policy.NamedDescriptor {
    return .{
        .kind = named.kind,
        .def = named.def,
        .builtin_owner = named.builtin_owner,
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

/// A minimal hand-built emitted type store, enough to read the receiver shapes a
/// declared generated rule's binder mapping accepts and rejects.
const ReceiverFixture = struct {
    allocator: Allocator,
    program_names: names.NameStore,
    store: Type.Store,
    module_hash: [32]u8,
    /// The checked id the next named type carries, so two named types in one
    /// fixture never share a checked identity.
    next_checked_id: u32,

    fn init(allocator: Allocator) ReceiverFixture {
        return .{
            .allocator = allocator,
            .program_names = names.NameStore.init(allocator),
            .store = Type.Store.init(allocator),
            .module_hash = [_]u8{9} ** 32,
            .next_checked_id = 1,
        };
    }

    fn deinit(self: *ReceiverFixture) void {
        self.store.deinit();
        self.program_names.deinit();
    }

    fn addPrimitive(self: *ReceiverFixture, primitive: Type.Primitive) Allocator.Error!Type.TypeId {
        return try self.store.add(.{ .primitive = primitive });
    }

    /// A named type with `args` positional arguments, which is one of the two
    /// receiver shapes any declared rule's mapping reads.
    fn addNamed(
        self: *ReceiverFixture,
        name_text: []const u8,
        args: []const Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const type_name = try self.program_names.internTypeName(name_text);
        const module = try self.program_names.internModuleIdentity(&self.module_hash);
        const span = try self.store.addSpan(args);
        const checked_id: checked.CheckedTypeId = @enumFromInt(self.next_checked_id);
        self.next_checked_id += 1;
        return try self.store.add(.{ .named = .{
            .named_type = .{ .module = .{}, .ty = checked_id },
            .def = .{ .module = module, .type_name = type_name },
            .kind = .nominal,
            .args = span,
        } });
    }

    fn addList(self: *ReceiverFixture, elem: Type.TypeId) Allocator.Error!Type.TypeId {
        return try self.store.add(.{ .list = elem });
    }

    /// A named type whose backing is `backing`, which is the shape a declared
    /// `nominal_backing` step descends into.
    fn addNamedWithBacking(
        self: *ReceiverFixture,
        name_text: []const u8,
        args: []const Type.TypeId,
        backing: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const type_name = try self.program_names.internTypeName(name_text);
        const module = try self.program_names.internModuleIdentity(&self.module_hash);
        const span = try self.store.addSpan(args);
        const checked_id: checked.CheckedTypeId = @enumFromInt(self.next_checked_id);
        self.next_checked_id += 1;
        return try self.store.add(.{ .named = .{
            .named_type = .{ .module = .{}, .ty = checked_id },
            .def = .{ .module = module, .type_name = type_name },
            .kind = .nominal,
            .args = span,
            .backing = .{ .ty = backing, .use = .inspectable },
        } });
    }

    fn fieldName(self: *ReceiverFixture, text: []const u8) Allocator.Error!names.RecordFieldNameId {
        return try self.program_names.internRecordFieldLabel(text);
    }

    fn tagName(self: *ReceiverFixture, text: []const u8) Allocator.Error!names.TagNameId {
        return try self.program_names.internTagLabel(text);
    }

    fn addRecord(self: *ReceiverFixture, fields: []const Type.Field) Allocator.Error!Type.TypeId {
        return try self.store.add(.{ .record = try self.store.addRecordFields(&self.program_names, fields) });
    }

    fn addTuple(self: *ReceiverFixture, items: []const Type.TypeId) Allocator.Error!Type.TypeId {
        return try self.store.add(.{ .tuple = try self.store.addSpan(items) });
    }

    fn addTagUnion(self: *ReceiverFixture, tags: []const Type.Tag) Allocator.Error!Type.TypeId {
        return try self.store.add(.{ .tag_union = try self.store.addTagVariants(&self.program_names, tags) });
    }

    fn addFunction(
        self: *ReceiverFixture,
        args: []const Type.TypeId,
        ret: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const span = try self.store.addSpan(args);
        return try self.store.add(.{ .func = .{ .args = span, .ret = ret } });
    }
};

test "a declared rule reads a named receiver's arguments in order" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addPrimitive(.u64);
    const other = try fixture.addPrimitive(.str);
    // `Builtin.Set.is_eq` is `Set(a), Set(a) -> Bool`: one binder, and the
    // receiver's single argument is exactly that binder's value.
    const set = try fixture.addNamed("Set", &.{item});
    try testing.expectEqual(@as(?usize, 1), receiverArgumentCount(&fixture.store, set));
    try testing.expectEqual(@as(?Type.TypeId, item), receiverArgumentAt(&fixture.store, set, 0));
    try testing.expectEqual(@as(?Type.TypeId, null), receiverArgumentAt(&fixture.store, set, 1));

    // A two-argument receiver reads both, in declaration order.
    const dict = try fixture.addNamed("Dict", &.{ item, other });
    try testing.expectEqual(@as(?usize, 2), receiverArgumentCount(&fixture.store, dict));
    try testing.expectEqual(@as(?Type.TypeId, item), receiverArgumentAt(&fixture.store, dict, 0));
    try testing.expectEqual(@as(?Type.TypeId, other), receiverArgumentAt(&fixture.store, dict, 1));
}

test "a declared rule reads a list receiver's element as its one argument" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    // `List` emits as the structural list shape rather than a named node, so
    // `Builtin.List.iter`'s one binder is read from that shape.
    const item = try fixture.addPrimitive(.u64);
    const list = try fixture.addList(item);
    try testing.expectEqual(@as(?usize, 1), receiverArgumentCount(&fixture.store, list));
    try testing.expectEqual(@as(?Type.TypeId, item), receiverArgumentAt(&fixture.store, list, 0));
    try testing.expectEqual(@as(?Type.TypeId, null), receiverArgumentAt(&fixture.store, list, 1));
}

test "a receiver whose emission carries no argument list binds nothing" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addPrimitive(.u64);
    const function = try fixture.addFunction(&.{item}, item);

    try testing.expect(receiverArgumentCount(&fixture.store, item) == null);
    try testing.expect(receiverArgumentCount(&fixture.store, function) == null);
}

test "a receiver whose argument count differs from the callee's binders binds nothing" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const key = try fixture.addPrimitive(.u64);
    const value = try fixture.addPrimitive(.str);
    const dict = try fixture.addNamed("Dict", &.{ key, value });

    // A one-binder callee cannot take a two-argument receiver's positions, and a
    // zero-argument receiver cannot supply a one-binder callee — which is
    // exactly why the encoding-format rules stay declared-but-unbound.
    try testing.expectEqual(@as(?usize, 2), receiverArgumentCount(&fixture.store, dict));
    const format = try fixture.addNamed("JsonEncoding", &.{});
    try testing.expectEqual(@as(?usize, 0), receiverArgumentCount(&fixture.store, format));
}

test "the receiver-position witness reads the callee scheme root's dispatch argument" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addPrimitive(.u64);
    const boolean = try fixture.addPrimitive(.bool);
    const set = try fixture.addNamed("Set", &.{item});
    // `Set.is_eq : Set(a), Set(a) -> Bool` under the binding: argument zero is
    // the position the derivation dispatched on.
    const root = try fixture.addFunction(&.{ set, set }, boolean);
    try testing.expectEqual(@as(?Type.TypeId, set), functionArgumentAt(&fixture.store, root, 0));
    try testing.expectEqual(@as(?Type.TypeId, set), functionArgumentAt(&fixture.store, root, 1));
    try testing.expectEqual(@as(?Type.TypeId, null), functionArgumentAt(&fixture.store, root, 2));
    try testing.expectEqual(@as(?Type.TypeId, null), functionArgumentAt(&fixture.store, set, 0));
}

test "a declared emitted path lands on the component each of its steps names" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const key = try fixture.addPrimitive(.str);
    const value = try fixture.addPrimitive(.u64);
    const dict = try fixture.addNamed("Dict", &.{ key, value });
    const other = try fixture.addPrimitive(.bool);

    // `Dict.empty().insert({ owes: d }, "found")` reaches the dict through a
    // record field of a receiver whose checked type is the constrained variable
    // `k` itself, so the field is named by the declared step, not by a checked
    // id.
    const owes = try fixture.fieldName("owes");
    const also = try fixture.fieldName("also");
    const record = try fixture.addRecord(&.{
        .{ .name = owes, .ty = dict },
        .{ .name = also, .ty = other },
    });
    const record_path = (EmittedPath{}).appending(.{ .record_field = owes }) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(?Type.TypeId, dict), followEmittedPath(&fixture.store, record, &record_path));

    // A tuple element and a tag payload are named by position.
    const tuple = try fixture.addTuple(&.{ other, dict });
    const tuple_path = (EmittedPath{}).appending(.{ .tuple_element = 1 }) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(?Type.TypeId, dict), followEmittedPath(&fixture.store, tuple, &tuple_path));

    const held = try fixture.tagName("Held");
    const tag_union = try fixture.addTagUnion(&.{.{
        .name = held,
        .checked_name = held,
        .payloads = try fixture.store.addSpan(&.{ other, dict }),
    }});
    const tag_path = (EmittedPath{}).appending(.{ .tag_payload = .{ .name = held, .index = 1 } }) orelse
        return error.TestUnexpectedResult;
    try testing.expectEqual(@as(?Type.TypeId, dict), followEmittedPath(&fixture.store, tag_union, &tag_path));

    // A named type's backing, which no checked id stands for, and a path of
    // several steps applied in order.
    const wrapper = try fixture.addNamedWithBacking("Wrapper", &.{}, record);
    const backing_path = (EmittedPath{}).appending(.nominal_backing) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(?Type.TypeId, record), followEmittedPath(&fixture.store, wrapper, &backing_path));
    const nested = backing_path.appending(.{ .record_field = owes }) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(@as(?Type.TypeId, dict), followEmittedPath(&fixture.store, wrapper, &nested));

    // An empty path is the named type itself.
    const empty = EmittedPath{};
    try testing.expectEqual(@as(?Type.TypeId, dict), followEmittedPath(&fixture.store, dict, &empty));
}

test "a declared emitted path that does not land binds nothing" {
    var fixture = ReceiverFixture.init(testing.allocator);
    defer fixture.deinit();

    const item = try fixture.addPrimitive(.u64);
    const present = try fixture.fieldName("present");
    const absent = try fixture.fieldName("absent");
    const record = try fixture.addRecord(&.{.{ .name = present, .ty = item }});

    // A field the emission does not carry, a step against the wrong shape, a
    // position past a tuple's end, and a backing a named type does not have all
    // refuse rather than approximating.
    const missing_field = (EmittedPath{}).appending(.{ .record_field = absent }) orelse return error.TestUnexpectedResult;
    try testing.expect(followEmittedPath(&fixture.store, record, &missing_field) == null);

    const tuple_step = (EmittedPath{}).appending(.{ .tuple_element = 0 }) orelse return error.TestUnexpectedResult;
    try testing.expect(followEmittedPath(&fixture.store, record, &tuple_step) == null);

    const tuple = try fixture.addTuple(&.{item});
    const past_end = (EmittedPath{}).appending(.{ .tuple_element = 1 }) orelse return error.TestUnexpectedResult;
    try testing.expect(followEmittedPath(&fixture.store, tuple, &past_end) == null);

    const backing_step = (EmittedPath{}).appending(.nominal_backing) orelse return error.TestUnexpectedResult;
    const opaque_named = try fixture.addNamed("Handle", &.{});
    try testing.expect(followEmittedPath(&fixture.store, opaque_named, &backing_step) == null);

    const held = try fixture.tagName("Held");
    const other_tag = try fixture.tagName("Other");
    const tag_union = try fixture.addTagUnion(&.{.{
        .name = held,
        .checked_name = held,
        .payloads = try fixture.store.addSpan(&.{item}),
    }});
    const missing_tag = (EmittedPath{}).appending(.{ .tag_payload = .{ .name = other_tag, .index = 0 } }) orelse
        return error.TestUnexpectedResult;
    try testing.expect(followEmittedPath(&fixture.store, tag_union, &missing_tag) == null);
    const past_payloads = (EmittedPath{}).appending(.{ .tag_payload = .{ .name = held, .index = 1 } }) orelse
        return error.TestUnexpectedResult;
    try testing.expect(followEmittedPath(&fixture.store, tag_union, &past_payloads) == null);
}

test "a position deeper than a declared path holds is not declared at all" {
    var path = EmittedPath{};
    var step: u32 = 0;
    while (step < max_emitted_path_steps) : (step += 1) {
        path = path.appending(.{ .tuple_element = step }) orelse return error.TestUnexpectedResult;
    }
    try testing.expectEqual(max_emitted_path_steps, path.declaredSteps().len);
    // One layer past the declared depth: the site hands over no receiver rather
    // than a truncated path that would land somewhere else.
    try testing.expect(path.appending(.nominal_backing) == null);
}

test "only the declared rules that carry a checked receiver bind from one" {
    // The declared-but-unbound inventory (reunify.md section 9.6): each of these
    // names its missing datum on its enum member and in design.md, and binds
    // nothing until that datum reaches its generating site.
    const unbound = [_]GeneratedInstantiationRule{
        .inspect_component,
        .pattern_literal_equality,
        .set_literal_helper,
        .dict_literal_helper,
        .json_parse_helper,
        .json_encode_helper,
        .json_record_field_name,
        .json_invalid_value,
    };
    for (unbound) |rule| try testing.expect(!rule.declaresBinderSource());
    const bound = [_]GeneratedInstantiationRule{
        .iterator_dispatch_receiver,
        .constraint_dispatch_receiver,
        .structural_derivation_component,
    };
    for (bound) |rule| try testing.expect(rule.declaresBinderSource());

    // Every declared rule is in exactly one of the two lists, so a rule added
    // later cannot slip in without declaring which it is.
    try testing.expectEqual(generated_rule_count, unbound.len + bound.len);
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
