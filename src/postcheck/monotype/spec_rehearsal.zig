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
//! This module maintains, for every lowering run in every build mode, the
//! per-specialization binding environments, request scopes, and callee
//! bindings that directed instantiation reads (reunify.md sections 9 and 11).
//! Its translator interns into the program's own type store, so an id it
//! returns is a production id. The graph comparison it can also run is Debug
//! measurement only, turned on by `ROC_REUNIFY_SHADOW`, and selects nothing;
//! a failure inside the measurement disables the measurement, never lowering.

const std = @import("std");
const Allocator = std.mem.Allocator;

const check = @import("check");
const Common = @import("../common.zig");
const collections = @import("collections");

const Type = @import("type.zig");
const census = @import("census.zig");
const direct_translate = @import("direct_translate.zig");
const solve = @import("solve.zig");
const closure = @import("../representation_closure.zig");
const policy = @import("../representation_policy.zig");

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

/// One checked type's address: the content identity of the module whose store
/// holds it, plus its id within that store.
pub const CheckedAddress = struct {
    module_bytes: [32]u8,
    type_id: u32,
};

/// A checked position named for the variable-presence memo. It is `CheckedAddress`
/// by content, kept separate so the memo's key type states what it keys.
const VariablePresenceKey = struct {
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
    /// A direct call to a compiler-owned iterator procedure — `List.iter`,
    /// ranges, `Iter.custom` and the adapters — whose checked use site the
    /// checker records ordinarily. The rule carries no binder mapping (the
    /// site binds); it carries the MINT: the procedure decides the produced
    /// kind, and the use's own formal argument types decide the components,
    /// so the specialization's directed reads state the produced
    /// representation without consulting the requesting graph
    /// (reunify.md 10.2, 13.2e).
    iterator_direct_call,
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
            .iterator_direct_call,
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
    /// The callable the generating walk built in the program's type store, for
    /// a rule whose generating site holds no checked data at all (reunify.md
    /// section 9.6): the walks that descend Monotypes name their callee's type
    /// here, and the binding reads the callee scheme's binder positions out of
    /// it directly. The section 7.5 witness still gates the binding — the
    /// scheme root emitted under it must digest-equal this callable.
    stored_callable: ?Type.TypeId = null,
};

/// The checked data one generating site hands over: the module whose store holds
/// its ids, the receiver the rule's binder mapping reads, and the exact
/// structural witness the rule accepts its binding under.
/// One requesting call site: the module and use expression a specialization
/// was requested at.
pub const RequestingSite = struct {
    module_bytes: [32]u8,
    use_expr: u32,
};

/// One producer feeding a request's receiver, named entirely in the
/// requesting module's checked store: the producing expression's own type is
/// the position its mint stands at, and `inner` is the producer feeding its
/// receiver in turn. The chain gives each request instance its own derivation,
/// so two uses of one interned checked type never share a mint.
pub const ReceiverLink = struct {
    /// The iterator procedure to derive from; null when `ready` already
    /// carries the produced representation.
    procedure: ?checked.IteratorProcedureId = null,
    /// The representation the producing call's own specialization derived,
    /// recorded under its use expression when that specialization resolved.
    ready: ?direct_translate.ProducerRepresentation = null,
    /// The producing call site, consulted when the chain is declared: the
    /// producer's specialization may resolve after this link is built and
    /// before any consumer reads it.
    use_key: ?RequestingSite = null,
    /// The producing expression's checked type: where the mint stands.
    produced: checked.CheckedTypeId,
    /// The producer's own receiver position.
    receiver: checked.CheckedTypeId,
    /// `Iter.custom`'s step-state formal.
    state: ?checked.CheckedTypeId = null,
    /// The callable evidence this link's mint is digested under.
    evidence: ?names.TypeDigest = null,
    inner: ?*const ReceiverLink = null,
};

/// Where a generated edge came from: the module that produced it plus the
/// receiver and witness positions the rule reads.
pub const GeneratedSource = struct {
    module_bytes: [32]u8,
    receiver: GeneratedReceiver,
    witness: GeneratedWitness,
    /// For the iterator rules: which iterator procedure the callee is, which
    /// decides the minted representation's kind. The generating site knows it;
    /// nothing else can derive it (reunify.md 13.2e).
    procedure: ?checked.IteratorProcedureId = null,
    /// For `Iter.custom`: the step-state formal's checked type at the use,
    /// the second component the producer mints over.
    state: ?checked.CheckedTypeId = null,
    /// The callable evidence the produced representation is minted under,
    /// digested at the requesting site from the callable operand itself.
    evidence: ?names.TypeDigest = null,
    /// The producers feeding this request's receiver, outermost first.
    receiver_link: ?*const ReceiverLink = null,
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

/// One step of the position a scheme binder occupies, stated in the vocabulary
/// both stores share: the checked walk that records it reads checked function,
/// nominal, and tuple components, and the follower reads the same components of
/// a stored callable. `named_arg` also reads a stored list or box element,
/// which is where a checked `List`/`Box` nominal argument stands after
/// translation.
pub const BinderPathStep = union(enum) {
    fn_arg: u32,
    fn_ret,
    named_arg: u32,
    tuple_element: u32,
    /// The extension position of a tag union: the binder stands for whatever
    /// variants the instance carries beyond the ones the scheme declares at
    /// that position, so its value is the stored row minus the declared tags.
    /// Always the final step of its path.
    tag_union_ext,
    /// One payload of one variant, positioned by the checked side's variant
    /// order; the stored variant is found by label text at follow time, since
    /// the two sides intern their labels in different name stores and need not
    /// share variant order.
    checked_tag_payload: CheckedTagPayloadStep,
};

/// The variant and payload position one `checked_tag_payload` step names, both
/// in the checked side's order.
pub const CheckedTagPayloadStep = struct {
    tag: u32,
    payload: u32,
};

/// How many binder-position steps a path holds, and how many binders one
/// scheme's stored binding resolves; schemes beyond either bound decline.
const max_binder_path_steps = 12;
const max_binder_paths = 8;

/// The position one scheme binder occupies in the scheme root, applied to the
/// callable a generating walk built to read that binder's value.
pub const BinderPath = struct {
    steps: [max_binder_path_steps]BinderPathStep = undefined,
    count: usize = 0,

    /// This path with one more step, or null when the position is deeper than
    /// a path reaches.
    pub fn appending(self: BinderPath, step: BinderPathStep) ?BinderPath {
        if (self.count == max_binder_path_steps) return null;
        var extended = self;
        extended.steps[self.count] = step;
        extended.count = self.count + 1;
        return extended;
    }

    pub fn recordedSteps(self: *const BinderPath) []const BinderPathStep {
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
    /// The enclosing producer's declared mint, captured while its frame was
    /// live for a constructor edge that produces its caller's return; a
    /// deferred specialization resolves after that frame pops.
    adopted_mint: ?direct_translate.ProducerRepresentation = null,
    /// Where the translator's declared representation inputs stood when a
    /// callee under this scope declared its own; closing the scope retracts
    /// back to it, exactly as a generated scope does.
    input_floor: ?usize = null,
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
    /// Where the translator's declared representation inputs stood when this
    /// level declared its own; closing the level retracts back to it.
    input_floor: ?usize = null,
};

/// One declared generated edge together with the requesting body's own binding
/// at the moment the request was made, which the rule's receiver and witness
/// both translate under (reunify.md sections 7.3, 9.1, 9.6).
const GeneratedRequest = struct {
    edge: GeneratedEdge,
    caller: ?CapturedEnvironment,
    /// The enclosing producer's declared mint, captured while its frame was
    /// live, exactly as a checked edge captures it (`RequestEdge.adopted_mint`).
    adopted_mint: ?direct_translate.ProducerRepresentation = null,
    /// Where the translator's declared representation inputs stood when a
    /// callee under this scope declared its own; closing the scope retracts
    /// back to it. The scope outlives the callee level, and the consumers of
    /// the declared representation read after that level closes.
    input_floor: ?usize = null,
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

/// Resolves a module's content identity to the cursor a translation reads it by.
/// The lowering Builder owns the module list; this hands the rehearsal exactly
/// the read it needs without duplicating that list.
pub const ModuleLookup = struct {
    context: *anyopaque,
    cursor_for_module: *const fn (context: *anyopaque, module_bytes: [32]u8) ?direct_translate.ModuleCursor,
    /// The module's unique iterator representation topology, for the callee
    /// derivation that states a minted producer representation (reunify.md
    /// 13.2e). Null when the host exposes none.
    iterator_topology: ?*const fn (context: *anyopaque, module_bytes: [32]u8) ?static_dispatch.IteratorRepresentationTopology = null,
    /// Whether this lowering has an instantiated stored type for a checked
    /// position. A position with none is a template's own, which a use edge's
    /// actuals make concrete; the production probe compares only the
    /// instantiated ones, so a probe elsewhere can ask the same question.
    position_is_instantiated: *const fn (context: *anyopaque, address: CheckedAddress) bool,

    fn cursor(self: ModuleLookup, module_bytes: [32]u8) ?direct_translate.ModuleCursor {
        return self.cursor_for_module(self.context, module_bytes);
    }
};

/// The inputs one specialization's rehearsal starts from.
pub const SpecializationStart = struct {
    /// The graph lowering this specialization; the rehearsal attaches its
    /// trace. Null when the frame is resolved before the graph exists — the
    /// caller attaches the graph once it is created.
    graph: ?*solve.InstGraph,
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
    /// The callable this specialization was requested at, in the program's
    /// type store, stamped while the requesting frame was live. A frame whose
    /// requesting edge resolves no binding binds from this instead — binder
    /// `i` takes the callable's component at the position binder `i` occupies
    /// in the scheme root, gated by the section 7.5 witness exactly as a
    /// declared rule's binding is (reunify.md section 9.6).
    stored_request_callable: ?Type.TypeId = null,
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

/// One active specialization's environment. `chain` ends at this
/// specialization's own level, whose bound values
/// are dense and ordered exactly like `binders` (reunify.md section 9.1); the
/// levels before it are the lexically enclosing environments the callee scheme's
/// checked captured binders name (reunify.md sections 7.1, 7.3). The whole
/// chain is owned by the rehearsal and freed when the frame pops.
const Frame = struct {
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
    /// The function id this specialization reserved, so an open recursive
    /// request can name the active frame it joins (reunify.md 13.2e, E2).
    reserved_fn_id: u32 = 0,
    /// The producer representation this frame's covering rule declared at its
    /// scheme's return position, kept so the template body can restate it at
    /// its own result position (the body's ids are instantiation-fresh, so the
    /// declared return's address never names them).
    ret_mint: ?direct_translate.ProducerRepresentation = null,
    /// The use-mint entries recorded while this frame was the requesting
    /// context, removed when it closes.
    recorded_uses: std.ArrayList(RequestingSite) = .empty,
    /// The requesting emission walked provisionally at resolution: joinable
    /// slots at undictated positions, drafts above them, sealed after the
    /// body relates the slots (reunify.md 9.1, 10.6).
    request_provisional: ?direct_translate.ProvisionalType = null,
    request_drafts: ?*direct_translate.MonoDraftStore = null,
    /// The defining scheme's root, kept so the post-body value read can
    /// re-emit it under every input the body declared.
    scheme_root_checked: ?u32 = null,
    /// The requesting module and the instantiated callable's return position,
    /// kept so a mint the body produces can be emitted there after lowering.
    request_ret_module: [32]u8 = [_]u8{0} ** 32,
    request_ret_checked: ?u32 = null,
    /// Why the edge supplied no binding, when it did not.
    skip: ?std.meta.Tag(Rehearsal.EdgeSkip) = null,
    /// The call site this specialization was requested at, so a mint its own
    /// body produces in tail position can be recorded for that site's readers.
    use_key: ?RequestingSite = null,
    /// The body's tail-position expressions, noted when the template body
    /// starts lowering: a producer resolving at one of these produces this
    /// specialization's own return.
    body_tails: std.ArrayList(u32) = .empty,
    env_ready: bool,
    /// Where this binding's residual materialization came from, if any.
    residual_origin: ResidualOrigin = .absent,
    /// Retraction floor for representation inputs declared for this
    /// specialization's lifetime, when no request scope was open to own them.
    input_floor: ?usize = null,

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
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return .not_named,
        };
        const right_named = switch (right_store.get(right)) {
            .named => |named| named,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return .not_named,
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

const ReachKind = enum(u8) { function, variable, row_or_function, defaultable };

const ReachMemoKey = struct {
    module_bytes: [32]u8,
    ty: checked.CheckedTypeId,
    kind: ReachKind,
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
    /// How many unbound-with-no-frame positions have been named, so the dump
    /// cannot fill the census file.
    unbound_no_frame_dumped: usize = 0,
    /// Backing storage for component slices this rehearsal declares as
    /// representation inputs. An arena keeps every slice at a stable address
    /// for as long as it stays declared — appending for a later declaration
    /// must not move an earlier one — and resets only when every declaration
    /// has been retracted.
    component_arena: std.heap.ArenaAllocator,
    /// The representation the most recent rule declaration stated for its
    /// scheme's return, consumed by the frame that ran the declaration.
    last_declared_mint: ?direct_translate.ProducerRepresentation = null,
    /// Memoized answers for the checked-reachability predicates. A checked
    /// root's answer depends only on immutable checked module data, so one
    /// lowering run never walks the same root twice for the same question.
    reach_memo: std.AutoHashMap(ReachMemoKey, bool),
    /// The produced representation each requesting use expression's
    /// specialization derived, keyed per call site so two uses of one
    /// interned checked type never share an entry. Entries are owned by the
    /// frame that was requesting when the specialization resolved and are
    /// removed when it closes.
    use_mints: std.AutoHashMap(RequestingSite, direct_translate.ProducerRepresentation),
    /// How many failed edge-to-site joins have been dumped in detail.
    nested_leaf_dumped: usize = 0,
    /// Positions the seam reported a divergence at, so whether each is a
    /// template can be settled after lowering rather than mid-seal.
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
    /// The emitted type each slot was created for, so a class's final type is
    /// readable after relations settle: the engine's classes carry
    /// descriptors, not types, and do not enumerate members.
    slot_types: std.AutoHashMapUnmanaged(u32, Type.TypeId),
    /// The minted member's type per relation class, recorded at each relate:
    /// the section 10.6 slot-finals the draft seal projects.
    class_finals: std.AutoHashMapUnmanaged(u32, Type.TypeId),
    details: std.ArrayList(MismatchDetail),
    unresolved_details: std.ArrayList(UnresolvedDetail),
    /// One worked example per constraint-replay site that came out informative,
    /// so its classification is read against a concrete disagreeing pair.
    unify_details: [census.unify_site_count]?UnifyDetail,
    /// Whether a checked position holds a variable at all, per position. The
    /// leveled read below searches for a FREE one on every read, and that
    /// search traverses the whole position; a position holding no variable can
    /// hold no free one under any environment, so the answer is settled once.
    variable_presence: std.AutoHashMapUnmanaged(VariablePresenceKey, bool),
    /// Which scheme generalizes a given variable, per variable. The search is a
    /// scan of every scheme in the module crossed with its binders, and the
    /// module's schemes do not change while a lowering runs.
    binder_owners: std.AutoHashMapUnmanaged(VariablePresenceKey, ?u32),
    disabled: bool,
    /// Set only by a measurement run; see `solve.LoweringCostClock`. Directed
    /// instantiation is what the graph is being replaced with, so its own read
    /// cost has to be weighable against the graph's on the same terms.
    cost_clock: ?*solve.LoweringCostClock,

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
            .component_arena = std.heap.ArenaAllocator.init(allocator),
            .use_mints = std.AutoHashMap(RequestingSite, direct_translate.ProducerRepresentation).init(allocator),
            .reach_memo = std.AutoHashMap(ReachMemoKey, bool).init(allocator),
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
            .slot_types = .empty,
            .class_finals = .empty,
            .details = .empty,
            .unresolved_details = .empty,
            .unify_details = @splat(null),
            .variable_presence = .empty,
            .binder_owners = .empty,
            .disabled = false,
            .cost_clock = null,
        };
        self.translator = direct_translate.Translator.init(allocator, self.types, program_names, resolver);
        return self;
    }

    /// Dump the bounded mismatch detail and release everything the rehearsal
    /// owns. Nothing it allocated is visible to lowering.
    pub fn destroy(self: *Rehearsal) void {
        for (self.frames.items) |*frame| self.releaseFrame(frame);
        self.frames.deinit(self.allocator);
        self.component_arena.deinit();
        self.use_mints.deinit();
        self.reach_memo.deinit();
        self.details.deinit(self.allocator);
        self.unresolved_details.deinit(self.allocator);
        self.slot_descriptors.deinit(self.allocator);
        self.slot_types.deinit(self.allocator);
        self.class_finals.deinit(self.allocator);
        self.variable_presence.deinit(self.allocator);
        self.binder_owners.deinit(self.allocator);
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
    /// Seal the innermost frame's provisional request emission through the
    /// class finals the body's relations recorded (reunify.md 10.6). Null
    /// where no provisional emission ran or a slot's class carries no final.
    /// Which value stage the innermost frame's read would take, for the
    /// probe's divergence trace.
    pub fn currentFrameValueStage(self: *const Rehearsal) []const u8 {
        if (self.frames.items.len == 0) return "no_frame";
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (frame.request_provisional != null) return "drafts";
        if (frame.scheme_root_checked != null) return "reemission";
        return "declined";
    }

    pub fn currentFrameRequestSealed(self: *Rehearsal) ?Type.TypeId {
        if (self.frames.items.len == 0) return null;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (frame.request_provisional) |provisional_root| seal: {
            const drafts = frame.request_drafts orelse break :seal;
            const sealed = (drafts.seal(
                self.types,
                self.program_names,
                provisional_root,
                self,
                rehearsalSlotFinal,
            ) catch null) orelse break :seal;
            return sealed;
        }
        // A frame whose start-of-frame emission skipped at an undictated
        // position re-emits after the body: every input the body declared is
        // live under the frame's floor, so the position the start emission
        // could not state may state now.
        const scheme_root = frame.scheme_root_checked orelse {
            return null;
        };
        if (!frame.env_ready) {
            return null;
        }
        const cursor = self.lookup.cursor(frame.env_module_bytes) orelse return null;
        // A scheme whose return shares its checked id with an argument holds
        // two roles at one address; a declared input for the argument would
        // answer at the return too, which 13.2c's law forbids a read to do.
        // Exactly that scheme declines instead.
        switch (cursor.view.payload(@enumFromInt(scheme_root))) {
            .function => |root_fn| {
                for (root_fn.args) |arg| {
                    if (arg == root_fn.ret) {
                        return null;
                    }
                }
            },
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {},
        }
        const sealed = self.emitQuietly(
            cursor,
            frame.environment(),
            frame.owner_node,
            @enumFromInt(scheme_root),
        ) orelse {
            return null;
        };
        return sealed;
    }

    /// Whether a callable root shares one checked id between an argument and
    /// its return: two roles at one address, which no address-keyed read may
    /// serve (reunify.md 13.2c).
    fn throughAliases(cursor: direct_translate.ModuleCursor, start_ty: checked.CheckedTypeId) checked.CheckedTypeId {
        var current = start_ty;
        var remaining: usize = 16;
        while (remaining > 0) : (remaining -= 1) {
            switch (cursor.view.payload(current)) {
                .alias => |alias_ty| current = alias_ty.backing,
                .pending, .err, .flex, .rigid, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => return current,
            }
        }
        return current;
    }

    fn rootSharesArgRet(cursor: direct_translate.ModuleCursor, root: checked.CheckedTypeId) bool {
        switch (cursor.view.payload(root)) {
            .function => |root_fn| {
                const ret = throughAliases(cursor, root_fn.ret);
                for (root_fn.args) |raw_arg| {
                    const arg = throughAliases(cursor, raw_arg);
                    if (arg != ret) continue;
                    // Only a position whose representation the checked data
                    // does not dictate can answer differently per role; a
                    // ground shared id carries one value for both.
                    switch (cursor.view.payload(ret)) {
                        .nominal => |n| {
                            if (direct_translate.nominalIsOpenRepresentation(n)) return true;
                        },
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => {},
                    }
                }
            },
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {},
        }
        return false;
    }

    fn rehearsalSlotFinal(self: *Rehearsal, slot: closure.RepresentationSlotId) ?Type.TypeId {
        return self.slotFinal(slot);
    }

    /// Relate each slot in a frame's provisional request tree to the slot of
    /// the same position in its emitted request tree, under the
    /// public-meets-minted rule: the emitted side carries whatever mint the
    /// declared inputs stated, so the relation is exactly how a joinable slot
    /// learns its final (reunify.md 10.2, 10.3). The trees share one shape by
    /// construction; a shape the walk does not model stops quietly.
    fn relateProvisionalToEmitted(
        self: *Rehearsal,
        provisional_root: direct_translate.ProvisionalType,
        drafts: *const direct_translate.MonoDraftStore,
        emitted: Type.TypeId,
        depth: u32,
    ) void {
        if (depth >= 16) return;
        switch (provisional_root) {
            .interned => {},
            .representation_slot => |slot| {
                const emitted_slot = self.slotForEmitted(emitted, depth) orelse return;
                if (self.engine.related(slot, emitted_slot)) return;
                self.engine.relate(slot, emitted_slot, .iterator_public_minted) catch return;
                self.recordClassFinal(emitted_slot);
            },
            .draft => |draft_id| {
                const draft = drafts.drafts.items[@intFromEnum(draft_id)];
                switch (draft.content) {
                    .func => |func| {
                        const emitted_fn = switch (self.types.get(emitted)) {
                            .func => |emitted_func| emitted_func,
                            .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => return,
                        };
                        const emitted_args = self.types.span(emitted_fn.args);
                        if (func.args.len != GuardedList.borrowLen(emitted_args)) return;
                        for (func.args, 0..) |arg, index| {
                            self.relateProvisionalToEmitted(arg, drafts, GuardedList.at(emitted_args, index), depth + 1);
                        }
                        self.relateProvisionalToEmitted(func.ret, drafts, emitted_fn.ret, depth + 1);
                    },
                    .list => |elem| {
                        const emitted_elem = switch (self.types.get(emitted)) {
                            .list => |list_elem| list_elem,
                            .primitive, .named, .record, .tuple, .tag_union, .box, .func, .erased, .zst => return,
                        };
                        self.relateProvisionalToEmitted(elem, drafts, emitted_elem, depth + 1);
                    },
                    .named => |named| {
                        const emitted_named = switch (self.types.get(emitted)) {
                            .named => |emitted_value| emitted_value,
                            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return,
                        };
                        const emitted_args = self.types.span(emitted_named.args);
                        if (named.args.len > GuardedList.borrowLen(emitted_args)) return;
                        for (named.args, 0..) |arg, index| {
                            self.relateProvisionalToEmitted(arg, drafts, GuardedList.at(emitted_args, index), depth + 1);
                        }
                    },
                }
            },
        }
    }

    fn openJoinableSlotInEngine(context: *anyopaque, declared: policy.NamedDescriptor, args: []const Type.TypeId) ?closure.RepresentationSlotId {
        const self: *Rehearsal = @ptrCast(@alignCast(context));
        const identity = direct_translate.slotIdentityDigest(self.types, self.program_names, declared, args);
        const token = token: {
            const gop = self.logical_tokens.getOrPut(self.allocator, identity) catch return null;
            if (!gop.found_existing) {
                gop.value_ptr.* = self.next_token;
                self.next_token +%= 1;
            }
            break :token @as(closure.LogicalToken, @enumFromInt(gop.value_ptr.*));
        };
        const item = if (args.len != 0) (self.slotForEmitted(args[0], 1) orelse return null) else (self.standInBacking() orelse return null);
        const backing = self.standInBacking() orelse return null;
        const slot = self.engine.createSlot(token, self.freshProducer(), .{ .iterator = .{
            .descriptor = declared,
            .item = item,
            .backing = backing,
        } }) catch return null;
        self.slots.append(self.allocator, slot) catch return null;
        self.slot_descriptors.put(self.allocator, @intFromEnum(slot), declared) catch return null;
        return slot;
    }

    /// The frame's request emission with everything its body produced: a mint
    /// the body handed to the frame's return after the start-of-frame emission
    /// is emitted at the requested return position, and the callable is
    /// rebuilt around it. Read after the body lowers.
    pub fn currentFrameRequestRootFinal(self: *Rehearsal) ?Type.TypeId {
        if (self.frames.items.len == 0) return null;
        const frame = &self.frames.items[self.frames.items.len - 1];
        const start_root = frame.request_root orelse {
            if (!frame.env_ready) {
                if (frame.skip) |skip| switch (skip) {
                    .root_request => {},
                    .generated_request => {},
                    .no_site => {},
                    .site_ambiguous => {},
                    .defining_module_differs => {},
                    .edge_unusable => {},
                };
            } else {}
            return null;
        };
        const mint = frame.ret_mint orelse return start_root;
        const ret_checked = frame.request_ret_checked orelse return start_root;
        const start_function = switch (self.types.get(start_root)) {
            .func => |func| func,
            .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => return start_root,
        };
        switch (self.types.get(start_function.ret)) {
            .named => |named| if (named.def.iterator_representation != .none) return start_root,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return start_root,
        }
        const caller = self.lookup.cursor(frame.request_ret_module) orelse return start_root;
        const floor = self.translator.representationInputCount();
        defer self.translator.truncateRepresentationInputs(floor);
        self.translator.declareRepresentationInput(.{
            .position = .{ .module_bytes = frame.request_ret_module, .type_id = ret_checked },
            .representation = mint,
        }) catch return start_root;
        var reason: direct_translate.SkipReason = undefined;
        const minted_ret = self.translator.translateUnderEnvironment(
            caller,
            null,
            checked.checked_residual_disposition_module_body_owner,
            @enumFromInt(ret_checked),
            &reason,
        ) catch return start_root;
        const args = self.types.span(start_function.args);
        const len = GuardedList.borrowLen(args);
        const arg_buffer = self.allocator.alloc(Type.TypeId, len) catch return start_root;
        defer self.allocator.free(arg_buffer);
        var index: usize = 0;
        while (index < len) : (index += 1) arg_buffer[index] = GuardedList.at(args, index);
        const rebuilt = self.types.internFunc(self.program_names, arg_buffer, minted_ret) catch return start_root;
        return rebuilt;
    }

    /// Restate the innermost frame's declared return mint at the template
    /// body's own result position: the body's checked ids are
    /// instantiation-fresh, so the scheme return's address never names them,
    /// while the result position produces the same representation. The frame
    /// carries the retraction.
    pub fn declareFrameRetMintAtBodyRoot(
        self: *Rehearsal,
        module_bytes: [32]u8,
        body_ret_ty: checked.CheckedTypeId,
    ) void {
        if (self.disabled) return;
        if (self.frames.items.len == 0) return;
        const frame = &self.frames.items[self.frames.items.len - 1];
        const mint = frame.ret_mint orelse return;
        const floor = self.translator.representationInputCount();
        self.translator.declareRepresentationInput(.{
            .position = .{ .module_bytes = module_bytes, .type_id = @intFromEnum(body_ret_ty) },
            .representation = mint,
        }) catch return;
        if (frame.input_floor == null) frame.input_floor = floor;
    }

    /// The innermost frame's skip-class name, for the probe's decline trace.
    pub fn currentFrameSkipName(self: *const Rehearsal) []const u8 {
        if (self.frames.items.len == 0) return "no_frame";
        const frame = &self.frames.items[self.frames.items.len - 1];
        const skip = frame.skip orelse return "none";
        return @tagName(skip);
    }

    pub fn innermostRequestEdge(self: *const Rehearsal) ?RequestEdgeName {
        if (self.disabled) return null;
        if (self.requests.items.len == 0) return null;
        return switch (self.requests.items[self.requests.items.len - 1]) {
            .checked => |edge| .{ .module_bytes = edge.module_bytes, .use_expr = edge.use_expr },
            .none, .generated => null,
        };
    }

    pub fn openRequestEdge(
        self: *Rehearsal,
        module_bytes: [32]u8,
        use_expr: checked.CheckedExprId,
        covering_rule: ?GeneratedEdge,
    ) void {
        if (self.disabled) return;
        var edge = RequestEdge{
            .module_bytes = module_bytes,
            .use_expr = use_expr,
            .covering_rule = covering_rule,
            .caller = self.captureCallerEnvironment(module_bytes),
        };
        if (covering_rule) |covering| capture: {
            const source = covering.source orelse break :capture;
            if (source.procedure != .iter_from_step) break :capture;
            var index = self.frames.items.len;
            while (index > 0) {
                index -= 1;
                if (self.frames.items[index].ret_mint) |mint| {
                    edge.adopted_mint = mint;
                    break :capture;
                }
            }
        }
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
        var request = GeneratedRequest{
            .edge = edge,
            .caller = if (edge.source) |source|
                self.captureCallerEnvironment(source.module_bytes)
            else
                null,
        };
        if (edge.source) |source| capture: {
            if (source.procedure != .iter_from_step) break :capture;
            var index = self.frames.items.len;
            while (index > 0) {
                index -= 1;
                if (self.frames.items[index].ret_mint) |mint| {
                    request.adopted_mint = mint;
                    break :capture;
                }
            }
        }
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
            .generated => |request| {
                if (request.input_floor) |floor| {
                    self.translator.truncateRepresentationInputs(floor);
                }
                self.releaseGeneratedRequest(request);
            },
            .checked => |edge| {
                if (edge.input_floor) |floor| {
                    self.translator.truncateRepresentationInputs(floor);
                }
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
        if (self.callees.items.len != 0) {
            if (self.callees.items[self.callees.items.len - 1].input_floor) |floor| {
                self.translator.truncateRepresentationInputs(floor);
            }
        }
        var level = self.callees.pop() orelse return;
        level.chain.release(self.allocator);
    }

    /// Resolve one callee scheme's dense binding from the checked data the
    /// requesting body named. Every way the data fails to supply one leaves the
    /// level unresolved: the binding is read, never inferred.
    /// State the producer representation the iterator rule mints at the
    /// callee's result position, so directed emission under this binding
    /// carries the minted tier the graph's generator would have minted
    /// (reunify.md 13.2e). Returns the translator's input floor to retract to
    /// when the level closes, or null when the rule mints nothing here.
    fn declareIteratorProducerInput(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme: checked.CheckedTypeScheme,
        declared: GeneratedEdge,
        caller_env: ?*const direct_translate.BindingEnvironment,
        caller_owner_node: u32,
        edge_adopted: ?direct_translate.ProducerRepresentation,
    ) ?usize {
        if (declared.rule != .iterator_dispatch_receiver and declared.rule != .iterator_direct_call) {
            return null;
        }
        const source = declared.source orelse {
            return null;
        };
        const procedure = source.procedure orelse {
            return null;
        };
        // A constructor that mints nothing of its own produces exactly what
        // its enclosing producer's return produces (the graph reads the same
        // through its expected return), so it adopts the requesting frame's
        // declared mint, already stamped with its recorded identity.
        const adopted: ?direct_translate.ProducerRepresentation = adopted: {
            if (kindForIteratorProcedure(procedure) != null) break :adopted null;
            if (procedure != .iter_from_step) break :adopted null;
            if (edge_adopted) |mint| break :adopted mint;
            // The innermost requesting frame that declared a mint is the
            // enclosing producer; a frame still being resolved carries none
            // yet, so the walk skips it naturally.
            var index = self.frames.items.len;
            while (index > 0) {
                index -= 1;
                if (self.frames.items[index].ret_mint) |mint| break :adopted mint;
            }
            break :adopted null;
        };
        const kind = kindForIteratorProcedure(procedure) orelse (if (adopted) |mint| mint.iterator_kind else {
            return null;
        });
        const function = switch (defining.view.payload(scheme.root)) {
            .function => |function| function,
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {
                return null;
            },
        };
        const caller = self.lookup.cursor(source.module_bytes) orelse {
            return null;
        };
        const topology_lookup = self.lookup.iterator_topology orelse {
            return null;
        };
        const topology_ids = topology_lookup(self.lookup.context, source.module_bytes) orelse {
            return null;
        };
        const topology = self.internTopology(caller, topology_ids) orelse {
            return null;
        };

        const floor = self.translator.representationInputCount();
        self.declareReceiverChainInputs(caller, caller_env, caller_owner_node, source.receiver_link, topology);
        const final_representation: direct_translate.ProducerRepresentation = if (adopted) |mint|
            mint
        else
            self.deriveProducerMint(caller, caller_env, caller_owner_node, .{
                .kind = kind,
                .receiver_ty = source.receiver.checked_ty,
                .state_ty = source.state,
                .evidence = source.evidence,
                .stamp_position = switch (source.witness) {
                    .callable => |callable_ty| switch (caller.view.payload(callable_ty)) {
                        .function => |f| f.ret,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => null,
                    },
                    .receiver_at_argument => null,
                },
            }, topology) orelse {
                self.translator.truncateRepresentationInputs(floor);
                return null;
            };

        self.translator.declareRepresentationInput(.{
            .position = .{
                .module_bytes = defining.module_bytes,
                .type_id = @intFromEnum(function.ret),
            },
            .representation = final_representation,
        }) catch return null;
        self.last_declared_mint = final_representation;
        // The requesting side reads its own return position, which the
        // callable witness names in the caller's module; the same minted
        // representation holds there.
        switch (source.witness) {
            .callable => |callable_ty| switch (caller.view.payload(callable_ty)) {
                .function => |caller_function| {
                    self.translator.declareRepresentationInput(.{
                        .position = .{
                            .module_bytes = source.module_bytes,
                            .type_id = @intFromEnum(caller_function.ret),
                        },
                        .representation = final_representation,
                    }) catch return null;
                },
                .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {},
            },
            .receiver_at_argument => {},
        }
        // The consumers of this declaration read after the callee level
        // closes, so the enclosing generated request scope carries the
        // retraction when one is open; the level carries it otherwise.
        if (self.requests.items.len != 0) {
            switch (self.requests.items[self.requests.items.len - 1]) {
                .generated => |*request| {
                    if (request.input_floor == null) request.input_floor = floor;
                    return null;
                },
                .checked => |*edge| {
                    if (edge.input_floor == null) edge.input_floor = floor;
                    return null;
                },
                .none => {},
            }
        }
        return floor;
    }

    /// Note one tail-position expression of the innermost frame's template
    /// body: a producer resolving at it produces the frame's own return.
    pub fn noteBodyTail(self: *Rehearsal, module_bytes: [32]u8, expr_id: checked.CheckedExprId) void {
        if (self.disabled) return;
        if (self.frames.items.len == 0) return;
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!std.mem.eql(u8, &frame.env_module_bytes, &module_bytes)) return;
        frame.body_tails.append(self.allocator, @intFromEnum(expr_id)) catch {};
    }

    /// Record the representation a use expression's specialization derived,
    /// owned by the innermost frame (the requesting context at resolution
    /// time). With no frame open the request came from a module's top level,
    /// which instantiates exactly once, so the entry stands for the whole
    /// rehearsal. A mint produced at the requesting frame's own body tail is
    /// that frame's return: it becomes the frame's declared mint and is
    /// recorded for the frame's own requesting site in turn, so a procedure
    /// that produces by delegating hands the mint to its callers.
    fn recordUseMint(
        self: *Rehearsal,
        module_bytes: [32]u8,
        use_expr: checked.CheckedExprId,
        mint: direct_translate.ProducerRepresentation,
    ) void {
        const key = RequestingSite{ .module_bytes = module_bytes, .use_expr = @intFromEnum(use_expr) };
        self.use_mints.put(key, mint) catch return;
        if (self.frames.items.len == 0) return;
        const owner = &self.frames.items[self.frames.items.len - 1];
        owner.recorded_uses.append(self.allocator, key) catch {
            _ = self.use_mints.remove(key);
            return;
        };
        if (!std.mem.eql(u8, &owner.env_module_bytes, &module_bytes)) return;
        for (owner.body_tails.items) |tail| {
            if (tail != @intFromEnum(use_expr)) continue;
            if (owner.ret_mint == null) owner.ret_mint = mint;
            if (owner.use_key) |own_key| {
                if (self.use_mints.get(own_key) == null) {
                    self.use_mints.put(own_key, mint) catch return;
                    // The reader sits in the frame's own requesting context,
                    // which outlives the frame: the entry belongs to the frame
                    // below it, and to the whole rehearsal when the request
                    // came from a module's top level.
                    if (self.frames.items.len >= 2) {
                        const grandparent = &self.frames.items[self.frames.items.len - 2];
                        grandparent.recorded_uses.append(self.allocator, own_key) catch {
                            _ = self.use_mints.remove(own_key);
                            return;
                        };
                    }
                }
            }
            break;
        }
    }

    /// The representation the specialization requested at this use expression
    /// derived, when it resolved under a frame still open.
    pub fn recordedUseMint(
        self: *const Rehearsal,
        module_bytes: [32]u8,
        use_expr: checked.CheckedExprId,
    ) ?direct_translate.ProducerRepresentation {
        return self.use_mints.get(.{ .module_bytes = module_bytes, .use_expr = @intFromEnum(use_expr) });
    }

    /// Copy one receiver-chain link into rehearsal-owned storage, so the edge
    /// carrying it outlives the requesting body context that built it.
    pub fn poolReceiverLink(self: *Rehearsal, link: ReceiverLink) ?*const ReceiverLink {
        const pooled = self.component_arena.allocator().create(ReceiverLink) catch return null;
        pooled.* = link;
        return pooled;
    }

    /// Declare each producer in a request's receiver chain at its own
    /// produced position, innermost first, so every outer derivation's
    /// receiver translation finds the mint the producer feeding it states.
    /// The declarations live under the caller's floor and retract with it.
    fn declareReceiverChainInputs(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        caller_env: ?*const direct_translate.BindingEnvironment,
        caller_owner_node: u32,
        link_opt: ?*const ReceiverLink,
        topology: Type.IteratorTopology,
    ) void {
        const link = link_opt orelse return;
        self.declareReceiverChainInputs(caller, caller_env, caller_owner_node, link.inner, topology);
        const recorded: ?direct_translate.ProducerRepresentation = if (link.use_key) |key|
            self.use_mints.get(key)
        else
            null;
        const mint = link.ready orelse recorded orelse derive: {
            const procedure = link.procedure orelse {
                return;
            };
            const kind = kindForIteratorProcedure(procedure) orelse return;
            break :derive self.deriveProducerMint(caller, caller_env, caller_owner_node, .{
                .kind = kind,
                .receiver_ty = link.receiver,
                .state_ty = link.state,
                .evidence = link.evidence,
                .stamp_position = link.produced,
            }, topology) orelse {
                return;
            };
        };
        self.translator.declareRepresentationInput(.{
            .position = .{ .module_bytes = caller.module_bytes, .type_id = @intFromEnum(link.produced) },
            .representation = mint,
        }) catch return;
    }

    /// What one producer mints over, named entirely in the requesting
    /// module's checked store, so one derivation serves the edge's own
    /// declaration and every producer feeding its receiver.
    const ProducerMintSpec = struct {
        kind: Type.IteratorKind,
        /// The producer's receiver position: the value a primary iterates, or
        /// the iterator an adapter minted over.
        receiver_ty: checked.CheckedTypeId,
        /// `Iter.custom`'s step-state formal.
        state_ty: ?checked.CheckedTypeId = null,
        /// The callable evidence the mint is digested under.
        evidence: ?names.TypeDigest = null,
        /// The caller-module position whose emission names the minted shape
        /// for the recorded identity digest; null leaves the mint unstamped.
        stamp_position: ?checked.CheckedTypeId = null,
    };

    /// Derive the representation one producer mints, from the requesting
    /// context's own knowledge. A primary — a list, string, single value,
    /// range, or custom step becoming an iterator — mints at depth one over
    /// its own arguments: the value it iterates (and, for `Iter.custom`, its
    /// step state), each the use's formal type emitted under the requesting
    /// binding. An adapter instead mints over its receiver's existing
    /// representation: the receiver's minted depth decides this mint's depth,
    /// and a chain past the cap runs forced-dynamic. A minted result carries
    /// the recorded identity: the produced position is emitted once with the
    /// mint unstamped, and that shape is digested under the recorded
    /// producer-identity recipe together with the callable evidence.
    fn deriveProducerMint(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        caller_env: ?*const direct_translate.BindingEnvironment,
        caller_owner_node: u32,
        spec: ProducerMintSpec,
        topology: Type.IteratorTopology,
    ) ?direct_translate.ProducerRepresentation {
        const primary = switch (spec.kind) {
            .list, .str, .single, .custom, .range_exclusive, .range_inclusive => true,
            .none, .map, .keep_if, .drop_if, .take_first, .drop_first, .concat, .append, .forced_dynamic => false,
        };
        var depth: u8 = 0;
        var receiver: ?Type.TypeId = null;
        var reason: direct_translate.SkipReason = undefined;
        if (self.translator.translateUnderEnvironment(
            caller,
            caller_env,
            caller_owner_node,
            spec.receiver_ty,
            &reason,
        )) |receiver_ty| {
            receiver = receiver_ty;
            switch (self.types.get(receiver_ty)) {
                .named => |named| depth = named.def.iterator_depth,
                .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => {},
            }
        } else |_| {}

        const representation: direct_translate.ProducerRepresentation = if (primary) primary: {
            var primary_components: [2]Type.TypeId = undefined;
            var count: usize = 0;
            switch (spec.kind) {
                .range_exclusive, .range_inclusive => {},
                .none, .custom, .list, .str, .single, .map, .keep_if, .drop_if, .take_first, .drop_first, .concat, .append, .forced_dynamic => {
                    const value = receiver orelse {
                        return null;
                    };
                    primary_components[0] = value;
                    count = 1;
                    if (spec.kind == .custom) {
                        const state_ty = spec.state_ty orelse {
                            return null;
                        };
                        const state = self.translator.translateUnderEnvironment(
                            caller,
                            caller_env,
                            caller_owner_node,
                            state_ty,
                            &reason,
                        ) catch {
                            return null;
                        };
                        primary_components[1] = state;
                        count = 2;
                    }
                },
            }
            const pooled = self.component_arena.allocator().alloc(Type.TypeId, count) catch return null;
            @memcpy(pooled, primary_components[0..count]);
            break :primary .{
                .iterator_representation = .minted,
                .iterator_kind = spec.kind,
                .iterator_depth = 1,
                .topology = topology,
                .minting = .{ .callable_evidence = spec.evidence },
                .components = pooled,
            };
        } else representation: {
            const components = self.pooledReceiverComponents(receiver);
            const over_cap = depth >= max_minted_chain_depth;
            break :representation if (over_cap) .{
                .iterator_representation = .forced_dynamic,
                .iterator_kind = .forced_dynamic,
                .iterator_depth = 0,
                .topology = topology,
                .minting = .{ .callable_evidence = null },
            } else .{
                .iterator_representation = .minted,
                .iterator_kind = spec.kind,
                .iterator_depth = depth + 1,
                .topology = topology,
                .minting = .{ .callable_evidence = spec.evidence },
                .components = components,
            };
        };

        var final_representation = representation;
        if (representation.iterator_representation == .minted) two_phase: {
            const stamp_position = spec.stamp_position orelse break :two_phase;
            const unstamped_floor = self.translator.representationInputCount();
            self.translator.declareRepresentationInput(.{
                .position = .{
                    .module_bytes = caller.module_bytes,
                    .type_id = @intFromEnum(stamp_position),
                },
                .representation = representation,
            }) catch break :two_phase;
            var unstamped_reason: direct_translate.SkipReason = undefined;
            const unstamped = self.translator.translateUnderEnvironment(
                caller,
                caller_env,
                caller_owner_node,
                stamp_position,
                &unstamped_reason,
            ) catch {
                self.translator.truncateRepresentationInputs(unstamped_floor);
                break :two_phase;
            };
            self.translator.truncateRepresentationInputs(unstamped_floor);
            switch (self.types.get(unstamped)) {
                .named => {},
                .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => {
                    break :two_phase;
                },
            }
            const shape = self.types.typeDigest(self.program_names, unstamped);
            var hasher = std.crypto.hash.sha2.Sha256.init(.{});
            hasher.update("roc.generated_iterator.final_identity");
            hasher.update(&shape.bytes);
            if (spec.evidence) |evidence| {
                hasher.update("callable_evidence");
                hasher.update(&evidence.bytes);
            }
            final_representation.generated = .{ .bytes = hasher.finalResult() };
            final_representation.minting = null;
        }
        return final_representation;
    }

    /// The minted components the receiver carries past its public item, pooled
    /// so the slice stays valid while declared. A list longer than the engine
    /// models is left undeclared rather than answered from a prefix.
    fn pooledReceiverComponents(self: *Rehearsal, receiver_ty: ?Type.TypeId) []const Type.TypeId {
        const ty = receiver_ty orelse return &.{};
        const named = switch (self.types.get(ty)) {
            .named => |named| named,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return &.{},
        };
        const args = self.types.span(named.args);
        const len = GuardedList.borrowLen(args);
        if (len <= 1 or len - 1 > 16) return &.{};
        const pooled = self.component_arena.allocator().alloc(Type.TypeId, len - 1) catch return &.{};
        var index: usize = 1;
        while (index < len) : (index += 1) {
            pooled[index - 1] = GuardedList.at(args, index);
        }
        return pooled;
    }

    /// Declare the producer representation a sealed Monotype carries at one
    /// specialization interface position, so the deferred body's directed
    /// reads state the sealed form — tier, kind, depth, generated identity,
    /// topology, and components — at the callee's own checked position.
    /// Returns the floor to retract at with `retractConsumerInputs`, or null
    /// when the sealed type carries no producer representation.
    pub fn declareSealedProducerInput(
        self: *Rehearsal,
        address: CheckedAddress,
        sealed: Type.TypeId,
    ) ?usize {
        const named = switch (self.types.get(sealed)) {
            .named => |named| named,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return null,
        };
        if (named.def.iterator_representation == .none) return null;
        const args = self.types.span(named.args);
        const len = GuardedList.borrowLen(args);
        if (len == 0 or len - 1 > 16) return null;
        const pooled = self.component_arena.allocator().alloc(Type.TypeId, len - 1) catch return null;
        var index: usize = 1;
        while (index < len) : (index += 1) {
            pooled[index - 1] = GuardedList.at(args, index);
        }
        const floor = self.translator.representationInputCount();
        self.translator.declareRepresentationInput(.{
            .position = .{ .module_bytes = address.module_bytes, .type_id = address.type_id },
            .representation = .{
                .iterator_representation = named.def.iterator_representation,
                .iterator_kind = named.def.iterator_kind,
                .iterator_depth = named.def.iterator_depth,
                .generated = named.def.generated,
                .topology = named.def.iterator_topology,
                .minting = null,
                .components = pooled,
            },
        }) catch return null;
        return floor;
    }

    /// Declare every producer representation a sealed Monotype carries under
    /// one specialization interface position, walking the checked type and the
    /// sealed type together and declaring at each nominal position the sealed
    /// side represents — a step function's iterator-typed fields as much as
    /// the position itself. The walk pairs rows by label text and stops where
    /// the two sides' structure does not correspond. Returns the first floor
    /// to retract at with `retractConsumerInputs`.
    pub fn declareSealedProducerInputsDeep(
        self: *Rehearsal,
        module_bytes: [32]u8,
        checked_ty: checked.CheckedTypeId,
        sealed: Type.TypeId,
    ) ?usize {
        const cursor = self.lookup.cursor(module_bytes) orelse return null;
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        var first: ?usize = null;
        self.declareSealedDeepInner(cursor, checked_ty, sealed, &visited, &first) catch {};
        return first;
    }

    fn declareSealedDeepInner(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        checked_ty: checked.CheckedTypeId,
        sealed: Type.TypeId,
        visited: *std.AutoHashMap(u64, void),
        first: *?usize,
    ) Allocator.Error!void {
        const pair = (@as(u64, @intFromEnum(checked_ty)) << 32) | @intFromEnum(sealed);
        const entry = try visited.getOrPut(pair);
        if (entry.found_existing) return;

        switch (cursor.view.payload(checked_ty)) {
            .nominal => |nominal| {
                const named = switch (self.types.get(sealed)) {
                    .named => |named| named,
                    .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return,
                };
                if (self.declareSealedProducerInput(
                    .{ .module_bytes = cursor.module_bytes, .type_id = @intFromEnum(checked_ty) },
                    sealed,
                )) |floor| {
                    if (first.* == null) first.* = floor;
                }
                const sealed_args = self.types.span(named.args);
                if (nominal.args.len > GuardedList.borrowLen(sealed_args)) return;
                for (nominal.args, 0..) |checked_arg, index| {
                    try self.declareSealedDeepInner(cursor, checked_arg, GuardedList.at(sealed_args, index), visited, first);
                }
            },
            .alias => |alias| try self.declareSealedDeepInner(cursor, alias.backing, sealed, visited, first),
            .function => |function| {
                const sealed_fn = switch (self.types.get(sealed)) {
                    .func => |func| func,
                    .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => return,
                };
                const sealed_args = self.types.span(sealed_fn.args);
                if (function.args.len != GuardedList.borrowLen(sealed_args)) return;
                for (function.args, 0..) |checked_arg, index| {
                    try self.declareSealedDeepInner(cursor, checked_arg, GuardedList.at(sealed_args, index), visited, first);
                }
                try self.declareSealedDeepInner(cursor, function.ret, sealed_fn.ret, visited, first);
            },
            .tuple => |items| {
                const sealed_items = switch (self.types.get(sealed)) {
                    .tuple => |span| self.types.span(span),
                    .primitive, .named, .record, .tag_union, .list, .box, .func, .erased, .zst => return,
                };
                if (items.len != GuardedList.borrowLen(sealed_items)) return;
                for (items, 0..) |checked_item, index| {
                    try self.declareSealedDeepInner(cursor, checked_item, GuardedList.at(sealed_items, index), visited, first);
                }
            },
            .record => |record| {
                const sealed_fields = switch (self.types.get(sealed)) {
                    .record => |span| self.types.fieldSpan(span),
                    .primitive, .named, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return,
                };
                for (record.fields) |checked_field| {
                    const field_text = cursor.source_names.recordFieldLabelText(checked_field.name);
                    const len = GuardedList.borrowLen(sealed_fields);
                    var index: usize = 0;
                    while (index < len) : (index += 1) {
                        const sealed_field = GuardedList.at(sealed_fields, index);
                        if (std.mem.eql(u8, field_text, self.program_names.recordFieldLabelText(sealed_field.name))) {
                            try self.declareSealedDeepInner(cursor, checked_field.ty, sealed_field.ty, visited, first);
                            break;
                        }
                    }
                }
            },
            .tag_union => |tag_union| {
                const sealed_tags = switch (self.types.get(sealed)) {
                    .tag_union => |span| self.types.tagSpan(span),
                    .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => return,
                };
                for (tag_union.tags) |checked_tag| {
                    const tag_text = cursor.source_names.tagLabelText(checked_tag.name);
                    const len = GuardedList.borrowLen(sealed_tags);
                    var index: usize = 0;
                    while (index < len) : (index += 1) {
                        const sealed_tag = GuardedList.at(sealed_tags, index);
                        if (!std.mem.eql(u8, tag_text, self.program_names.tagLabelText(sealed_tag.name))) continue;
                        const checked_args = checked_tag.argsSlice(cursor.view);
                        const sealed_payloads = self.types.span(sealed_tag.payloads);
                        if (checked_args.len == GuardedList.borrowLen(sealed_payloads)) {
                            for (checked_args, 0..) |checked_arg, payload_index| {
                                try self.declareSealedDeepInner(cursor, checked_arg, GuardedList.at(sealed_payloads, payload_index), visited, first);
                            }
                        }
                        break;
                    }
                }
            },
            .pending, .err, .flex, .rigid, .record_unbound, .empty_record, .empty_tag_union => {},
        }
    }

    /// The longest minted-iterator chain a producer builds before running the
    /// tail forced-dynamic, matching the generator's own cap.
    const max_minted_chain_depth: u8 = 16;

    /// Intern one module's iterator topology names into the program's stores.
    fn internTopology(
        self: *Rehearsal,
        caller: direct_translate.ModuleCursor,
        ids: static_dispatch.IteratorRepresentationTopology,
    ) ?Type.IteratorTopology {
        const source_names = caller.source_names;
        const interned: Type.IteratorTopology = .{
            .len_field = self.program_names.internRecordFieldLabel(source_names.recordFieldLabelText(ids.len_field)) catch return null,
            .step_field = self.program_names.internRecordFieldLabel(source_names.recordFieldLabelText(ids.step_field)) catch return null,
            .known_tag = self.program_names.internTagLabel(source_names.tagLabelText(ids.known_tag)) catch return null,
            .unknown_tag = self.program_names.internTagLabel(source_names.tagLabelText(ids.unknown_tag)) catch return null,
            .done_tag = self.program_names.internTagLabel(source_names.tagLabelText(ids.done_tag)) catch return null,
            .one_tag = self.program_names.internTagLabel(source_names.tagLabelText(ids.one_tag)) catch return null,
            .skip_tag = self.program_names.internTagLabel(source_names.tagLabelText(ids.skip_tag)) catch return null,
            .item_field = self.program_names.internRecordFieldLabel(source_names.recordFieldLabelText(ids.item_field)) catch return null,
            .rest_field = self.program_names.internRecordFieldLabel(source_names.recordFieldLabelText(ids.rest_field)) catch return null,
        };
        return interned;
    }

    /// Which minted kind one iterator procedure produces. A procedure that
    /// mints nothing - a pass-through, a consumer, or a constructor whose
    /// expected return already carries the representation - maps to null.
    fn kindForIteratorProcedure(procedure: checked.IteratorProcedureId) ?Type.IteratorKind {
        return switch (procedure) {
            .list_iter => .list,
            .str_iter_utf8 => .str,
            .iter_single => .single,
            .iter_map => .map,
            .iter_keep_if => .keep_if,
            .iter_drop_if => .drop_if,
            .iter_take_first => .take_first,
            .iter_drop_first => .drop_first,
            .iter_concat => .concat,
            .iter_append => .append,
            .iter_custom => .custom,
            .iter_exclusive_range, .numeric_range_exclusive => .range_exclusive,
            .iter_inclusive_range, .numeric_range_inclusive => .range_inclusive,
            .iter_iter, .iter_next, .iter_from_step, .range_done => null,
        };
    }

    fn resolveCalleeBinding(self: *Rehearsal, binding: CalleeBinding) CalleeLevel {
        const unresolved = CalleeLevel{
            .module_bytes = binding.defining_module_bytes,
            .owner_node = checked.checked_residual_disposition_module_body_owner,
            .chain = EnvironmentChain.none,
            .ready = false,
        };
        const defining = self.lookup.cursor(binding.defining_module_bytes) orelse {
            return unresolved;
        };
        const scheme = defining.view.schemeById(binding.scheme) orelse {
            return unresolved;
        };
        // A callee that captures enclosing binders needs the lexical parents its
        // own specialization frame links (reunify.md section 7.3); a call-site
        // binding states this scheme's own binders and nothing else.
        if (scheme.captured_len != 0) {
            return unresolved;
        }

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
                // Only a callee whose scheme actually generalizes something can
                // strand a binder; one with none needs no binding at all.
                if (scheme.gv_len == 0) {} else {}
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
                break :resolved_by_site;
            };
            return .{
                .module_bytes = defining.module_bytes,
                .owner_node = scheme.owner_node,
                .chain = chain,
                .ready = true,
                .input_floor = if (rule) |declared_rule| self.declareIteratorProducerInput(
                    defining,
                    scheme,
                    declared_rule,
                    caller_env,
                    caller_owner_node,
                    null,
                ) else null,
            };
        }

        const declared = rule orelse {
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
            return unresolved;
        };
        return .{
            .module_bytes = defining.module_bytes,
            .owner_node = scheme.owner_node,
            .chain = chain,
            .ready = true,
            .input_floor = self.declareIteratorProducerInput(
                defining,
                scheme,
                declared,
                caller_env,
                caller_owner_node,
                null,
            ),
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
        if (!edge.rule.declaresBinderSource()) {
            // The walks that descend Monotypes declare no checked source; the
            // callable they built is the binder source instead.
            const callable = edge.stored_callable orelse return null;
            return self.bindCalleeFromStoredCallable(defining, scheme_id, scheme, callable);
        }
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
            return null;
        };
        const receiver = followEmittedPath(self.types, receiver_root, &source.receiver.path) orelse {
            return null;
        };
        const argument_count = receiverArgumentCount(self.types, receiver) orelse {
            return null;
        };
        if (argument_count != binders.len) {
            if (argument_count == 0) {} else {}
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
        chain.release(self.allocator);
        return null;
    }

    /// Build one callee scheme's dense binding from the callable a generating
    /// walk built in the program's type store (reunify.md section 9.6). The
    /// walks that descend Monotypes — inspect, the json codecs, the literal
    /// helpers — hold no checked type for the call they build, but the callable
    /// they built is the instance the callee scheme is requested at. Binder `i`
    /// takes the callable's component at the position binder `i` occupies in
    /// the scheme root, and the section 7.5 witness gates the result: the
    /// scheme root emitted under the binding must digest-equal the callable.
    fn bindCalleeFromStoredCallable(
        self: *Rehearsal,
        defining: direct_translate.ModuleCursor,
        scheme_id: checked.CheckedTypeSchemeId,
        scheme: checked.CheckedTypeScheme,
        callable: Type.TypeId,
    ) ?EnvironmentChain {
        const binders = scheme.generalizedVars(defining.view);
        var paths: [max_binder_paths]?BinderPath = @splat(null);
        if (binders.len > max_binder_paths) {
            return null;
        }
        self.findBinderPaths(defining.view, scheme.root, binders, paths[0..binders.len]);
        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return null;
        };
        defer self.allocator.free(bound);
        for (paths[0..binders.len], 0..) |path, index| {
            const declared_path = path orelse {
                return null;
            };
            const component = self.followBinderPath(defining, scheme.root, callable, &declared_path) orelse {
                return null;
            };
            bound[index] = direct_translate.BoundType.of(component);
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
        if (self.quietWitnessAgrees(declared, callable)) {
            return chain;
        }
        chain.release(self.allocator);
        return null;
    }

    /// Record, for each binder, the position it first occupies in the scheme
    /// root, walking only through the positions the program's type store
    /// mirrors: function arguments and returns, nominal type arguments, tuple
    /// elements, and alias backings (which emit no step of their own). A binder
    /// whose only occurrences sit elsewhere — a record field, a tag payload, an
    /// alias argument — keeps a null path and the binding declines.
    fn findBinderPaths(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
        binders: []const checked.CheckedTypeId,
        paths: []?BinderPath,
    ) void {
        const Visit = struct {
            ty: checked.CheckedTypeId,
            path: BinderPath,
        };
        var visited = collections.DenseMap(checked.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(Visit).empty;
        defer stack.deinit(self.allocator);
        var remaining: usize = 0;
        for (paths) |path| {
            if (path == null) remaining += 1;
        }
        stack.append(self.allocator, .{ .ty = root, .path = .{} }) catch return;
        while (stack.pop()) |visit| {
            if (remaining == 0) return;
            const gop = visited.getOrPut(visit.ty) catch return;
            if (gop.found_existing) continue;
            for (binders, 0..) |binder, index| {
                if (binder != visit.ty) continue;
                if (paths[index] != null) continue;
                paths[index] = visit.path;
                remaining -= 1;
            }
            // A stack pops its most recent entry first, so each arm pushes its
            // components in reverse for the walk to reach them in declaration
            // order; the first path recorded per binder is then the leftmost
            // shallow-first occurrence the pushed order reaches.
            switch (view.payload(visit.ty)) {
                .flex, .rigid, .pending, .err, .empty_record, .empty_tag_union => {},
                .record, .record_unbound => {},
                .tag_union => |tag_ty| {
                    // The extension position carries no structural step of its
                    // own, so a binder standing there is matched here at the
                    // union rather than by visiting it as a child.
                    for (binders, 0..) |binder, index| {
                        if (binder != tag_ty.ext) continue;
                        if (paths[index] != null) continue;
                        const extended = visit.path.appending(.tag_union_ext) orelse continue;
                        paths[index] = extended;
                        remaining -= 1;
                    }
                    var tag_index = tag_ty.tags.len;
                    while (tag_index > 0) {
                        tag_index -= 1;
                        const args = tag_ty.tags[tag_index].argsSlice(view);
                        var payload_index = args.len;
                        while (payload_index > 0) {
                            payload_index -= 1;
                            const extended = visit.path.appending(.{ .checked_tag_payload = .{
                                .tag = @intCast(tag_index),
                                .payload = @intCast(payload_index),
                            } }) orelse continue;
                            stack.append(self.allocator, .{ .ty = args[payload_index], .path = extended }) catch return;
                        }
                    }
                },
                .alias => |alias_ty| {
                    stack.append(self.allocator, .{ .ty = alias_ty.backing, .path = visit.path }) catch return;
                },
                .tuple => |elems| {
                    var index = elems.len;
                    while (index > 0) {
                        index -= 1;
                        const extended = visit.path.appending(.{ .tuple_element = @intCast(index) }) orelse continue;
                        stack.append(self.allocator, .{ .ty = elems[index], .path = extended }) catch return;
                    }
                },
                .function => |fn_ty| {
                    if (visit.path.appending(.fn_ret)) |extended| {
                        stack.append(self.allocator, .{ .ty = fn_ty.ret, .path = extended }) catch return;
                    }
                    var index = fn_ty.args.len;
                    while (index > 0) {
                        index -= 1;
                        const extended = visit.path.appending(.{ .fn_arg = @intCast(index) }) orelse continue;
                        stack.append(self.allocator, .{ .ty = fn_ty.args[index], .path = extended }) catch return;
                    }
                },
                .nominal => |nominal_ty| {
                    var index = nominal_ty.args.len;
                    while (index > 0) {
                        index -= 1;
                        const extended = visit.path.appending(.{ .named_arg = @intCast(index) }) orelse continue;
                        stack.append(self.allocator, .{ .ty = nominal_ty.args[index], .path = extended }) catch return;
                    }
                },
            }
        }
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
        if (use_has_sites and owner_has_sites) {} else if (use_has_sites) {} else if (owner_has_sites) {} else {}
    }

    /// Bind the innermost open request's edge to the function id that request
    /// reserved, which is the identity the specialization is lowered under
    /// however much later that happens (reunify.md 11.3). A reservation made
    /// outside any edge-naming request scope claims nothing.
    pub fn claimRequestEdge(self: *Rehearsal, fn_id: u32) void {
        if (self.disabled) return;
        if (self.requests.items.len == 0) {
            return;
        }
        const slot = &self.requests.items[self.requests.items.len - 1];
        const claim: ClaimedRequest = switch (slot.*) {
            .none => {
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
        // One reserved id is requested once: a second claim would mean two
        // distinct use sites reserved the same specialization body, which is
        // recorded rather than silently overwritten.
        if (existing) |previous| {
            self.releaseClaim(previous.value);
        }
    }

    /// Bind the innermost open request's edge to a reserved function id only
    /// when that edge names exactly this use expression, so a reservation made
    /// while an enclosing call's edge is innermost cannot steal it. Reports
    /// whether the claim was made.
    pub fn claimRequestEdgeForUse(
        self: *Rehearsal,
        fn_id: u32,
        module_bytes: [32]u8,
        use_expr: checked.CheckedExprId,
    ) bool {
        if (self.disabled) return false;
        if (self.requests.items.len == 0) {
            return false;
        }
        const top = self.requests.items[self.requests.items.len - 1];
        const edge = switch (top) {
            .checked => |edge| edge,
            .none, .generated => {
                return false;
            },
        };
        if (edge.use_expr != use_expr or !std.mem.eql(u8, &edge.module_bytes, &module_bytes)) {
            return false;
        }
        self.claimRequestEdge(fn_id);
        return true;
    }

    /// Open a frame for a nested specialization lowered inline in its
    /// requester's graph: the frame resolves from the claimed edge exactly as
    /// a template specialization's does, but the graph's trace and seal probe
    /// stay with the enclosing specialization that owns the graph.
    pub fn beginNestedSpecialization(self: *Rehearsal, start: SpecializationStart) void {
        if (self.disabled) return;
        var frame = Frame{
            .env_module_bytes = start.cursor.module_bytes,
            .scheme = .{ .module_bytes = start.cursor.module_bytes, .scheme = 0 },
            .owner_node = checked.checked_residual_disposition_module_body_owner,
            .binders = &.{},
            .chain = EnvironmentChain.none,
            .interface_root = null,
            .request_root = null,
            .reserved_fn_id = start.reserved_fn_id,
            .ret_mint = null,
            .env_ready = false,
        };
        self.resolveEnvironment(start, &frame);
        self.frames.append(self.allocator, frame) catch {
            self.releaseFrame(&frame);
            self.disabled = true;
            return;
        };
    }

    /// Close the innermost frame a `beginNestedSpecialization` opened. The
    /// enclosing specialization's graph comparison is untouched.
    pub fn endNestedSpecialization(self: *Rehearsal) void {
        if (self.disabled) return;
        if (self.frames.items.len == 0) return;
        var frame = self.frames.pop() orelse return;
        self.releaseFrame(&frame);
    }

    /// The stored callable the innermost open request scope names, for the
    /// specialization record made under it to carry (reunify.md section 9.6):
    /// a generated scope carries the callable its walk built, and a checked
    /// scope carries the one its covering rule derived. Read at recording
    /// time, while the scope is open, because a deferred or per-context
    /// reservation resolves its frame long after the scope closes.
    pub fn innermostStoredCallable(self: *Rehearsal) ?Type.TypeId {
        if (self.disabled) return null;
        if (self.requests.items.len == 0) return null;
        return switch (self.requests.items[self.requests.items.len - 1]) {
            .none => null,
            .generated => |request| request.edge.stored_callable,
            .checked => |edge| if (edge.covering_rule) |covering| covering.stored_callable else null,
        };
    }

    /// The directed answer for a checked position with exactly one extra
    /// binder stated by the caller: the numeral call's return emitted with
    /// the pre-target variable bound to the target the site holds (reunify.md
    /// 13.2d, the literal-leaves rule executed in its own terms — the value
    /// comes from the target, so the target is the binding). The live frame's
    /// environment sits underneath for any other position the type carries.
    pub fn typeForCheckedUnderSingleBinding(
        self: *Rehearsal,
        module_bytes: [32]u8,
        ty: checked.CheckedTypeId,
        binder: checked.CheckedTypeId,
        bound_value: Type.TypeId,
    ) ?Type.TypeId {
        if (self.disabled) return null;
        const cursor = self.lookup.cursor(module_bytes) orelse return null;
        const frame_env: ?*const direct_translate.BindingEnvironment =
            if (self.frameForModule(module_bytes)) |frame| frame.environment() else null;
        const binders = [_]checked.CheckedTypeId{binder};
        const bound = [_]direct_translate.BoundType{direct_translate.BoundType.of(bound_value)};
        const env = direct_translate.BindingEnvironment{
            .scheme = .{ .module_bytes = module_bytes, .scheme = 0 },
            .binders = &binders,
            .bound = &bound,
            .captured = &.{},
            .parent = frame_env,
        };
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            &env,
            checked.checked_residual_disposition_module_body_owner,
            ty,
            &reason,
        ) catch null;
    }

    /// The reserved function id of the innermost active specialization frame
    /// whose directed request root carries this representation-erased digest:
    /// the graph-free half of the open recursive-request join (reunify.md
    /// 13.2e, E2). An open request made while lowering a specialization's own
    /// body re-enters that specialization exactly when its callable, emitted
    /// under the live binding, is the identity the active frame was requested
    /// at; the frame stack is the active-descent set, so no interface walk is
    /// needed to scope the search.
    pub fn activeRecursiveJoin(self: *Rehearsal, digest: names.TypeDigest) ?u32 {
        if (self.disabled) return null;
        var index = self.frames.items.len;
        while (index > 0) {
            index -= 1;
            const frame = &self.frames.items[index];
            const root = frame.request_root orelse continue;
            const root_digest = self.types.specializationDigest(self.program_names, root);
            if (std.mem.eql(u8, &root_digest.bytes, &digest.bytes)) return frame.reserved_fn_id;
        }
        return null;
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
            .none => {},
            .checked => {},
            .generated => {},
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
    /// data. Always pushes a frame, so the matching `endSpecialization` is
    /// unconditional.
    pub fn beginSpecialization(self: *Rehearsal, start: SpecializationStart) void {
        if (self.disabled) return;
        self.beginSpecializationFrame(start);
    }

    /// Resolve and push this specialization's frame: the binder environment
    /// from the requesting edge and everything the frame carries. Separated
    /// from graph attachment so a caller can resolve the frame before the
    /// specialization's graph exists — the directed request identity is read
    /// off the frame ahead of the cache probe.
    pub fn beginSpecializationFrame(self: *Rehearsal, start: SpecializationStart) void {
        if (self.disabled) return;
        var frame = Frame{
            .env_module_bytes = start.cursor.module_bytes,
            .scheme = .{ .module_bytes = start.cursor.module_bytes, .scheme = 0 },
            .owner_node = checked.checked_residual_disposition_module_body_owner,
            .binders = &.{},
            .chain = EnvironmentChain.none,
            .interface_root = null,
            .request_root = null,
            .reserved_fn_id = start.reserved_fn_id,
            .ret_mint = null,
            .env_ready = false,
        };
        self.resolveEnvironment(start, &frame);
        self.frames.append(self.allocator, frame) catch {
            self.releaseFrame(&frame);
            self.disabled = true;
            return;
        };
    }

    /// Whether the innermost binding for a module covers every variable a
    /// checked position reaches: the active frame's environment leaves no free
    /// variable, so directed instantiation under it answers with the same
    /// binding the specialization was entered at (reunify.md 13.2d).
    pub fn activeBindingCovers(self: *Rehearsal, address: CheckedAddress) bool {
        if (self.disabled) return false;
        const cursor = self.lookup.cursor(address.module_bytes) orelse return false;
        const frame = self.frameForModule(address.module_bytes) orelse return false;
        if (!frame.env_ready) return false;
        return self.firstFreeVariable(
            cursor.view,
            @enumFromInt(address.type_id),
            frame.environment(),
        ) == null;
    }

    /// Finish one specialization: pop the environment.
    pub fn endSpecialization(self: *Rehearsal) void {
        if (self.requests.items.len == 0 and self.callees.items.len == 0 and
            self.translator.representationInputCount() == 0)
        {
            _ = self.component_arena.reset(.retain_capacity);
        }
        if (self.frames.items.len == 0) return;
        var frame = self.frames.pop() orelse return;
        self.releaseFrame(&frame);
    }

    fn releaseFrame(self: *Rehearsal, frame: *Frame) void {
        if (frame.input_floor) |floor| {
            self.translator.truncateRepresentationInputs(floor);
        }
        for (frame.recorded_uses.items) |key| _ = self.use_mints.remove(key);
        frame.recorded_uses.deinit(self.allocator);
        frame.body_tails.deinit(self.allocator);
        if (frame.request_drafts) |drafts| {
            drafts.deinit();
            self.allocator.destroy(drafts);
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
            // A residual with no value is the uninhabited materialization the
            // rest of the rehearsal already measures, so an unresolved captured
            // position stays visible as a mismatch instead of as a silent hole.
            values[index] = direct_translate.BoundType.of(self.uninhabitedStandIn() orelse {
                self.allocator.free(values);
                return null;
            });
            const outer_id = entry.outerScheme() orelse {
                continue;
            };
            const outer = defining.view.schemeById(outer_id) orelse {
                continue;
            };
            const outer_binders = outer.generalizedVars(defining.view);
            if (entry.binder_index >= outer_binders.len) {
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
                continue;
            };
            const level = order.items[position];
            if (entry.binder_index >= level.bound.len or entry.binder_index >= level.binders.len) {
                continue;
            }
            // The checked pair and the active level must name the SAME checked
            // binder at that index; a disagreement would mean the two binder
            // orderings drifted and the value read would silently bind incorrectly.
            if (level.binders[entry.binder_index] != outer_binders[entry.binder_index]) {
                continue;
            }
            values[index] = level.bound[entry.binder_index];
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
        // An edge that resolved no binding still leaves the callable the
        // requesting site stamped on the specialization record; the stored
        // binding reads that, under the same witness discipline a declared
        // rule's binding is gated by (reunify.md section 9.6).
        if (start.stored_request_callable) |callable| stored: {
            const scheme_id = start.template_scheme orelse {
                break :stored;
            };
            const scheme = start.cursor.view.schemeById(scheme_id) orelse {
                break :stored;
            };
            if (scheme.gv_len == 0) break :stored;
            var scratch_outcome = GeneratedOutcome{};
            if (self.bindGeneratedRuleFromStoredCallable(start, frame, scheme_id, scheme, callable, &scratch_outcome)) {
                return;
            }
        }
        frame.skip = skip;
        self.resolveGroundTemplateEnvironment(start, frame, skip);
    }

    /// Resolve one specialization's dense binding from the requesting edge's
    /// site, reporting null when the binding was resolved and otherwise why the
    /// edge supplied none. Every way the edge fails to resolve is a named skip
    /// class, never an assumption.
    fn resolveEnvironmentFromEdge(self: *Rehearsal, start: SpecializationStart, frame: *Frame) ?EdgeSkip {
        const claim = self.takeClaim(start.reserved_fn_id) orelse {
            if (self.frames.items.len == 0) {
                return .root_request;
            }
            return .{ .generated_request = null };
        };
        const edge = switch (claim) {
            .checked => |checked_edge| checked_edge,
            .generated => |request| return self.resolveEnvironmentFromGeneratedRule(start, frame, request),
        };
        defer self.releaseEdge(edge);
        const caller = self.lookup.cursor(edge.module_bytes) orelse {
            return .edge_unusable;
        };
        // The edge this specialization was requested at is the one whose callee
        // scheme is owned by the definition this template specializes: the use
        // expression alone names an edge per callee it instantiates, and only
        // the owner node picks this one out (reunify.md section 7.2's edge
        // identity).
        const owner_node = templateSchemeOwnerNode(start) orelse {
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
            if (self.resolveEnvironmentFromForeignSchemeEdge(start, frame, caller, site, edge)) return null;
            return .defining_module_differs;
        }
        const defining = self.definingCursor(start, defining_bytes) orelse {
            return .edge_unusable;
        };
        const scheme = defining.view.schemeById(scheme_id) orelse {
            return .edge_unusable;
        };
        const binders = scheme.generalizedVars(defining.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) {
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
                return .edge_unusable;
            }
            const translated = self.translateActual(caller, caller_env, caller_owner_node, actual) orelse {
                // An actual the site cannot state — a generically-recorded
                // instantiation whose variable no open binding disposes —
                // is where the edge's declared covering rule states the
                // binding instead (reunify.md sections 7.2, 9.6).
                if (edge.covering_rule != null) {
                    return self.resolveEnvironmentFromCoveringRule(start, frame, edge);
                }
                return .edge_unusable;
            };
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
        if (captured.parent_levels == 0) {} else {}

        frame.env_module_bytes = defining_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.env_ready = true;
        frame.scheme_root_checked = @intFromEnum(scheme.root);
        noteEnvironmentScheme(scheme);
        if (binders.len == 0) {}

        // The mint the requesting edge's covering rule states for this
        // specialization: declared for the frame's whole lifetime, so every
        // directed read inside the body — the interface emissions below
        // included — answers with the produced representation
        // (reunify.md 10.2, 13.2e). The floor rides the frame unless an open
        // request scope claimed it.
        if (edge.covering_rule) |covering| {
            frame.input_floor = self.declareIteratorProducerInput(
                defining,
                scheme,
                covering,
                caller_env,
                caller_owner_node,
                edge.adopted_mint,
            );
            frame.ret_mint = self.last_declared_mint;
            self.last_declared_mint = null;
            if (frame.ret_mint) |mint| self.recordUseMint(edge.module_bytes, edge.use_expr, mint);
        }
        frame.use_key = .{ .module_bytes = edge.module_bytes, .use_expr = @intFromEnum(edge.use_expr) };

        // The two sides of this specialization's representation interface
        // (reunify.md section 11.1): the callee's scheme root emitted under the
        // binding, and the request context's own emission of the same edge.
        frame.interface_root = self.emitQuietly(defining, frame.environment(), scheme.owner_node, scheme.root);
        frame.request_root = self.emitQuietly(caller, caller_env, caller_owner_node, site.instantiated_root);
        switch (caller.view.payload(site.instantiated_root)) {
            .function => |request_function| {
                frame.request_ret_module = caller.module_bytes;
                frame.request_ret_checked = @intFromEnum(request_function.ret);
            },
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {},
        }

        if (comptime census.enabled) provisional: {
            if (rootSharesArgRet(caller, site.instantiated_root)) {
                census.bump("value_reemission_shared_position");
                break :provisional;
            }
            const drafts = self.allocator.create(direct_translate.MonoDraftStore) catch break :provisional;
            drafts.* = direct_translate.MonoDraftStore.init(self.allocator);
            var provisional_reason: direct_translate.SkipReason = undefined;
            const provisional_root = self.translator.translateProvisionalUnderEnvironment(
                caller,
                caller_env,
                caller_owner_node,
                site.instantiated_root,
                drafts,
                .{ .context = @ptrCast(self), .open = openJoinableSlotInEngine },
                &provisional_reason,
            ) catch {
                drafts.deinit();
                self.allocator.destroy(drafts);
                break :provisional;
            };
            frame.request_provisional = provisional_root;
            frame.request_drafts = drafts;
            census.bump("rehearsal_request_provisional_emitted");
            if (frame.request_root) |emitted_root| {
                self.relateProvisionalToEmitted(provisional_root, drafts, emitted_root, 0);
            }
        }

        // The requesting context's own emission of the instantiated callable
        // carries every producer representation the caller's directed
        // knowledge states for this specialization — arguments as much as the
        // return. Declaring them at the scheme's positions for the frame's
        // lifetime lets the deferred body's directed reads answer from the
        // requesting site alone (reunify.md sections 7.2, 10.2).
        if (frame.request_root) |requested| {
            const deep_floor = self.declareSealedProducerInputsDeep(defining_bytes, scheme.root, requested);
            if (frame.input_floor == null) frame.input_floor = deep_floor;
        }
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
            return named;
        };
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            return named;
        };
        // The mint the rule states is independent of whether it can bind
        // binders: a ground scheme and a rule without a binder source still
        // produce at their return position, and the requesting context's
        // captured environment translates the components (reunify.md 10.2).
        frame.input_floor = self.declareIteratorProducerInput(
            start.cursor,
            scheme,
            covering,
            if (edge.caller) |*captured| captured.environment() else null,
            if (edge.caller) |captured| captured.owner_node else checked.checked_residual_disposition_module_body_owner,
            edge.adopted_mint,
        );
        frame.ret_mint = self.last_declared_mint;
        self.last_declared_mint = null;
        if (frame.ret_mint) |mint| self.recordUseMint(edge.module_bytes, edge.use_expr, mint);
        frame.use_key = .{ .module_bytes = edge.module_bytes, .use_expr = @intFromEnum(edge.use_expr) };
        // A rule edge into a scheme with no binders has exactly one
        // instantiation; the ground path already resolves those exactly.
        if (scheme.gv_len == 0) {
            outcome.ground += 1;
            return named;
        }
        // A covering edge names its callable in checked terms; where the rule
        // declares no binder source, that callable's emission under the held
        // requesting context is the stored instance the binding reads.
        const stored_callable: ?Type.TypeId = covering.stored_callable orelse derived: {
            const covering_source = covering.source orelse {
                break :derived null;
            };
            const checked_callable = switch (covering_source.witness) {
                .callable => |callable| callable,
                .receiver_at_argument => {
                    break :derived null;
                },
            };
            const caller = self.lookup.cursor(covering_source.module_bytes) orelse {
                break :derived null;
            };
            const emitted = self.emitQuietly(
                caller,
                if (edge.caller) |*captured| captured.environment() else null,
                if (edge.caller) |captured| captured.owner_node else checked.checked_residual_disposition_module_body_owner,
                checked_callable,
            ) orelse {
                break :derived null;
            };
            break :derived emitted;
        };
        const declared_source = if (covering.rule.declaresBinderSource()) covering.source else null;
        const source = declared_source orelse {
            if (stored_callable) |callable| {
                if (self.bindGeneratedRuleFromStoredCallable(start, frame, scheme_id, scheme, callable, outcome)) {
                    return null;
                }
                return named;
            }
            outcome.unbound += 1;
            return named;
        };
        if (self.bindGeneratedRule(start, frame, scheme_id, scheme, source, edge.caller, outcome)) {
            return null;
        }
        if (stored_callable) |callable| {
            if (self.bindGeneratedRuleFromStoredCallable(start, frame, scheme_id, scheme, callable, outcome)) {
                return null;
            }
        }
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
            return named;
        };
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            return named;
        };
        // As on the covering-rule path: the mint the rule states is declared
        // even where it binds nothing.
        frame.input_floor = self.declareIteratorProducerInput(
            start.cursor,
            scheme,
            request.edge,
            if (request.caller) |*captured| captured.environment() else null,
            if (request.caller) |captured| captured.owner_node else checked.checked_residual_disposition_module_body_owner,
            request.adopted_mint,
        );
        frame.ret_mint = self.last_declared_mint;
        self.last_declared_mint = null;
        // A generated edge into a scheme with no binders has exactly one
        // instantiation; the ground path already resolves those exactly.
        if (scheme.gv_len == 0) {
            outcome.ground += 1;
            return named;
        }
        // The walks that descend Monotypes hand the callable they built over
        // directly; an edge that names its callable only in checked terms gets
        // it emitted under the held requesting context instead.
        const stored_callable: ?Type.TypeId = request.edge.stored_callable orelse derived: {
            const edge_source = request.edge.source orelse {
                break :derived null;
            };
            const checked_callable = switch (edge_source.witness) {
                .callable => |callable| callable,
                .receiver_at_argument => {
                    break :derived null;
                },
            };
            const caller = self.lookup.cursor(edge_source.module_bytes) orelse {
                break :derived null;
            };
            const emitted = self.emitQuietly(
                caller,
                if (request.caller) |*captured| captured.environment() else null,
                if (request.caller) |captured| captured.owner_node else checked.checked_residual_disposition_module_body_owner,
                checked_callable,
            ) orelse {
                break :derived null;
            };
            break :derived emitted;
        };
        const declared_source = if (request.edge.rule.declaresBinderSource()) request.edge.source else null;
        const source = declared_source orelse {
            if (stored_callable) |callable| {
                if (self.bindGeneratedRuleFromStoredCallable(start, frame, scheme_id, scheme, callable, outcome)) {
                    return null;
                }
                return named;
            }
            outcome.unbound += 1;
            return named;
        };
        if (self.bindGeneratedRule(start, frame, scheme_id, scheme, source, request.caller, outcome)) {
            return null;
        }
        // A declared checked source that does not emit under the requesting
        // context still leaves the callable the generating walk built; the
        // stored binding reads that instead, under the same witness discipline.
        if (stored_callable) |callable| {
            if (self.bindGeneratedRuleFromStoredCallable(start, frame, scheme_id, scheme, callable, outcome)) {
                return null;
            }
        }
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
            return false;
        };
        const binders = scheme.generalizedVars(start.cursor.view);
        // The rule's mapping is over the callee scheme's own binders; a scheme
        // that also captures enclosing binders is outside every declared rule.
        if (scheme.captured_len != 0) {
            outcome.receiver_unusable += 1;
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
            return false;
        };
        const receiver = followEmittedPath(self.types, receiver_root, &source.receiver.path) orelse {
            outcome.receiver_unusable += 1;
            return false;
        };
        const argument_count = receiverArgumentCount(self.types, receiver) orelse {
            outcome.receiver_unusable += 1;
            return false;
        };
        if (argument_count != binders.len) {
            outcome.receiver_unusable += 1;
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
        frame.scheme_root_checked = @intFromEnum(scheme.root);
        frame.interface_root = declared;
        frame.request_root = requested;
        noteEnvironmentScheme(scheme);
        return true;
    }

    /// Bind one generated specialization's environment from the callable its
    /// generating walk built in the program's type store (reunify.md section
    /// 9.6). The walks that descend Monotypes — inspect, the json codecs, the
    /// literal helpers — hold no checked type for the call they build, but the
    /// callable they built is the instance the callee scheme is requested at:
    /// binder `i` takes the callable's component at the position binder `i`
    /// occupies in the scheme root, and the section 7.5 witness gates the
    /// whole binding — the scheme root emitted under it must digest-equal the
    /// callable, which is therefore also the specialization's requesting root.
    fn bindGeneratedRuleFromStoredCallable(
        self: *Rehearsal,
        start: SpecializationStart,
        frame: *Frame,
        scheme_id: checked.CheckedTypeSchemeId,
        scheme: checked.CheckedTypeScheme,
        callable: Type.TypeId,
        outcome: *GeneratedOutcome,
    ) bool {
        // The rule's mapping is over the callee scheme's own binders; a scheme
        // that also captures enclosing binders is outside every declared rule.
        if (scheme.captured_len != 0) {
            outcome.receiver_unusable += 1;
            return false;
        }
        const binders = scheme.generalizedVars(start.cursor.view);
        if (binders.len > max_binder_paths) {
            outcome.receiver_unusable += 1;
            return false;
        }
        var paths: [max_binder_paths]?BinderPath = @splat(null);
        self.findBinderPaths(start.cursor.view, scheme.root, binders, paths[0..binders.len]);
        const bound = self.allocator.alloc(direct_translate.BoundType, binders.len) catch {
            self.fail();
            return false;
        };
        defer self.allocator.free(bound);
        for (paths[0..binders.len], 0..) |path, index| {
            const recorded = path orelse {
                outcome.receiver_unusable += 1;
                return false;
            };
            const component = self.followBinderPath(start.cursor, scheme.root, callable, &recorded) orelse {
                outcome.receiver_unusable += 1;
                return false;
            };
            if (self.carriesResidualMaterialization(component)) {
                noteResidualOrigin(frame, .unresolved_request_context);
            }
            bound[index] = direct_translate.BoundType.of(component);
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
        if (!self.generatedWitnessAgrees(declared, callable, outcome)) {
            chain.release(self.allocator);
            return false;
        }
        frame.chain = chain;
        frame.env_module_bytes = start.cursor.module_bytes;
        frame.scheme = scheme_ident;
        frame.owner_node = scheme.owner_node;
        frame.binders = binders;
        frame.env_ready = true;
        frame.scheme_root_checked = @intFromEnum(scheme.root);
        frame.interface_root = declared;
        frame.request_root = callable;
        noteEnvironmentScheme(scheme);
        return true;
    }

    /// Apply one binder's recorded scheme position to the callable a
    /// generating walk built, walking the scheme root alongside it so a
    /// row-extension step can read which variants the scheme declares there.
    /// Each step reads exactly the component it names — no step searches for a
    /// shape that would fit — and the section 7.5 witness still gates whatever
    /// the whole binding produces. Null when either side carries no such
    /// position.
    fn followBinderPath(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        checked_root: checked.CheckedTypeId,
        stored_root: Type.TypeId,
        path: *const BinderPath,
    ) ?Type.TypeId {
        const store = self.types;
        var checked_pos = checked_root;
        var current = stored_root;
        for (path.recordedSteps()) |step| {
            checked_pos = checkedThroughAliases(cursor.view, checked_pos);
            switch (step) {
                .fn_arg => |index| {
                    current = functionArgumentAt(store, current, index) orelse return null;
                    checked_pos = switch (cursor.view.payload(checked_pos)) {
                        .function => |fn_ty| if (index < fn_ty.args.len) fn_ty.args[index] else return null,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => return null,
                    };
                },
                .fn_ret => {
                    current = switch (store.get(current)) {
                        .func => |fn_ty| fn_ty.ret,
                        .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => return null,
                    };
                    checked_pos = switch (cursor.view.payload(checked_pos)) {
                        .function => |fn_ty| fn_ty.ret,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => return null,
                    };
                },
                .named_arg => |index| {
                    const nominal_args = switch (cursor.view.payload(checked_pos)) {
                        .nominal => |nominal_ty| nominal_ty.args,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => return null,
                    };
                    if (index >= nominal_args.len) return null;
                    current = switch (store.get(current)) {
                        .named => |named| blk: {
                            const args = store.span(named.args);
                            if (index >= GuardedList.borrowLen(args)) return null;
                            break :blk GuardedList.at(args, index);
                        },
                        .list => |elem| if (index == 0) elem else return null,
                        .box => |elem| if (index == 0) elem else return null,
                        // An open-representation nominal is emitted as its
                        // backing structure, so the argument's position is
                        // wherever the declaration places that formal.
                        .primitive, .record, .tuple, .tag_union, .func, .erased, .zst => self.storedThroughErasedNominal(cursor, checked_pos, current, index) orelse return null,
                    };
                    checked_pos = nominal_args[index];
                },
                .checked_tag_payload => |position| {
                    const declared = switch (cursor.view.payload(checked_pos)) {
                        .tag_union => |tag_ty| tag_ty.tags,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .empty_tag_union => return null,
                    };
                    if (position.tag >= declared.len) return null;
                    const declared_tag = declared[position.tag];
                    const declared_args = declared_tag.argsSlice(cursor.view);
                    if (position.payload >= declared_args.len) return null;
                    const declared_text = cursor.source_names.tagLabelText(declared_tag.name);
                    current = stored: {
                        switch (store.get(current)) {
                            .tag_union => |span| {
                                const entries = store.tagSpan(span);
                                for (0..GuardedList.borrowLen(entries)) |entry_index| {
                                    const entry = GuardedList.at(entries, entry_index);
                                    if (!std.mem.eql(u8, self.program_names.tagLabelText(entry.checked_name), declared_text)) continue;
                                    const payloads = store.span(entry.payloads);
                                    if (position.payload >= GuardedList.borrowLen(payloads)) return null;
                                    break :stored GuardedList.at(payloads, position.payload);
                                }
                                return null;
                            },
                            .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => return null,
                        }
                    };
                    checked_pos = declared_args[position.payload];
                },
                .tuple_element => |index| {
                    current = switch (store.get(current)) {
                        .tuple => |items| blk: {
                            const entries = store.span(items);
                            if (index >= GuardedList.borrowLen(entries)) return null;
                            break :blk GuardedList.at(entries, index);
                        },
                        .primitive, .named, .record, .tag_union, .list, .box, .func, .erased, .zst => return null,
                    };
                    checked_pos = switch (cursor.view.payload(checked_pos)) {
                        .tuple => |elems| if (index < elems.len) elems[index] else return null,
                        .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => return null,
                    };
                },
                .tag_union_ext => return self.tagRowRemainder(cursor, checked_pos, current),
            }
        }
        return current;
    }

    /// The stored position of one argument of a checked nominal whose stored
    /// instance is its backing structure rather than a named wrapper: the
    /// declaration says where the formal for that argument stands in the
    /// backing, and the stored instance mirrors the backing, so the formal's
    /// recorded position applies to it directly. Substitution changes
    /// payloads, never variant labels, so the label-text matching the inner
    /// walk uses reads the instance exactly.
    fn storedThroughErasedNominal(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        checked_nominal: checked.CheckedTypeId,
        stored: Type.TypeId,
        arg_index: u32,
    ) ?Type.TypeId {
        const nominal_ty = switch (cursor.view.payload(checked_nominal)) {
            .nominal => |nominal_ty| nominal_ty,
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => return null,
        };
        const backing = self.translator.resolver.nominalBacking(cursor, nominal_ty) orelse return null;
        if (arg_index >= backing.formal_args.len) return null;
        const formal = backing.formal_args[arg_index];
        var formal_paths: [1]?BinderPath = .{null};
        self.findBinderPaths(backing.cursor.view, backing.root, &.{formal}, formal_paths[0..1]);
        const formal_path = formal_paths[0] orelse return null;
        return self.followBinderPath(backing.cursor, backing.root, stored, &formal_path);
    }

    /// The stored row minus the variants the scheme declares at a tag union's
    /// extension position: the binder standing at that extension takes exactly
    /// the variants the instance carries beyond the declared ones. Tags are
    /// matched by label text, since the two sides intern their labels in
    /// different name stores. The result is interned in the program's store,
    /// and the section 7.5 witness gates the binding it participates in.
    fn tagRowRemainder(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        checked_ty: checked.CheckedTypeId,
        stored_ty: Type.TypeId,
    ) ?Type.TypeId {
        const declared = switch (cursor.view.payload(checked_ty)) {
            .tag_union => |tag_ty| tag_ty.tags,
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .empty_tag_union => return null,
        };
        const entries = switch (self.types.get(stored_ty)) {
            .tag_union => |span| self.types.tagSpan(span),
            .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => return null,
        };
        const entry_count = GuardedList.borrowLen(entries);
        // Copy the surviving variants out before interning: interning grows
        // the store, and the spans borrowed above read from it.
        var remainder = std.ArrayList(Type.Store.TagInput).empty;
        defer remainder.deinit(self.allocator);
        var payload_ids = std.ArrayList(Type.TypeId).empty;
        defer payload_ids.deinit(self.allocator);
        var payload_ranges = std.ArrayList([2]usize).empty;
        defer payload_ranges.deinit(self.allocator);
        for (0..entry_count) |index| {
            const entry = GuardedList.at(entries, index);
            const entry_text = self.program_names.tagLabelText(entry.checked_name);
            var is_declared = false;
            for (declared) |declared_tag| {
                if (std.mem.eql(u8, cursor.source_names.tagLabelText(declared_tag.name), entry_text)) {
                    is_declared = true;
                    break;
                }
            }
            if (is_declared) continue;
            const payloads = self.types.span(entry.payloads);
            const start = payload_ids.items.len;
            for (0..GuardedList.borrowLen(payloads)) |payload_index| {
                payload_ids.append(self.allocator, GuardedList.at(payloads, payload_index)) catch return null;
            }
            payload_ranges.append(self.allocator, .{ start, payload_ids.items.len }) catch return null;
            remainder.append(self.allocator, .{
                .name = entry.name,
                .checked_name = entry.checked_name,
                .payloads = &.{},
            }) catch return null;
        }
        for (remainder.items, payload_ranges.items) |*input, range| {
            input.payloads = payload_ids.items[range[0]..range[1]];
        }
        return self.types.internTagUnion(self.program_names, remainder.items) catch null;
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
            return false;
        };
        const right = requested orelse {
            outcome.witness_absent += 1;
            return false;
        };
        // As in `witnessesAgree`: the binding proof reads the
        // representation-erased identity.
        const left_digest = self.types.specializationDigest(self.program_names, left);
        const right_digest = self.types.specializationDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            outcome.witness_agrees += 1;
            return true;
        }
        const left_unfolded = self.types.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes)) {
            outcome.witness_agrees += 1;
            return true;
        }
        outcome.witness_differs += 1;
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
            return;
        };
        const scheme_raw = @intFromEnum(scheme_id);
        const scheme = start.cursor.view.schemeById(scheme_id) orelse {
            return;
        };
        if (scheme.gv_len != 0) {
            noteEdgelessWithBinders(start, scheme, skip);
            return;
        }
        if (scheme.captured_len != 0) {
            return;
        }
        if (self.schemeRootReachesVariable(start.cursor.view, scheme.root)) {
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
        frame.scheme_root_checked = @intFromEnum(scheme.root);
        noteEnvironmentScheme(scheme);
        frame.interface_root = self.emitQuietly(start.cursor, frame.environment(), scheme.owner_node, scheme.root);
        // A ground scheme has exactly one instantiation, so the declared root
        // emitted under the empty binding is the requested callable itself.
        frame.request_root = frame.interface_root;
        if (comptime census.enabled) provisional: {
            if (rootSharesArgRet(start.cursor, scheme.root)) {
                census.bump("value_reemission_shared_position");
                break :provisional;
            }
            const drafts = self.allocator.create(direct_translate.MonoDraftStore) catch break :provisional;
            drafts.* = direct_translate.MonoDraftStore.init(self.allocator);
            var provisional_reason: direct_translate.SkipReason = undefined;
            const provisional_root = self.translator.translateProvisionalUnderEnvironment(
                start.cursor,
                frame.environment(),
                scheme.owner_node,
                scheme.root,
                drafts,
                .{ .context = @ptrCast(self), .open = openJoinableSlotInEngine },
                &provisional_reason,
            ) catch {
                drafts.deinit();
                self.allocator.destroy(drafts);
                break :provisional;
            };
            frame.request_provisional = provisional_root;
            frame.request_drafts = drafts;
            census.bump("rehearsal_request_provisional_emitted");
            if (frame.request_root) |emitted_root| {
                self.relateProvisionalToEmitted(provisional_root, drafts, emitted_root, 0);
            }
        }
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
            return false;
        }
        const binders = scheme.generalizedVars(start.cursor.view);
        const actuals = site.actuals(caller.view);
        if (actuals.len != binders.len) {
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
                return false;
            }
            const translated = self.translateActual(caller, caller_env, caller_owner_node, actual) orelse {
                // As on the same-module path: an actual the site cannot state
                // routes to the rule the edge declares (reunify.md 7.2, 9.6).
                if (edge.covering_rule != null) {
                    return self.resolveEnvironmentFromCoveringRule(start, frame, edge) == null;
                }
                return false;
            };
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
        // The covering rule's mint is declared before either interface side
        // is emitted, so both emissions carry the produced representation
        // exactly as the same-module path's do (reunify.md 10.2, 13.2e).
        if (edge.covering_rule) |covering| {
            frame.input_floor = self.declareIteratorProducerInput(
                start.cursor,
                scheme,
                covering,
                caller_env,
                caller_owner_node,
                edge.adopted_mint,
            );
            frame.ret_mint = self.last_declared_mint;
            self.last_declared_mint = null;
            if (frame.ret_mint) |mint| self.recordUseMint(edge.module_bytes, edge.use_expr, mint);
        }
        frame.use_key = .{ .module_bytes = edge.module_bytes, .use_expr = @intFromEnum(edge.use_expr) };
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
        frame.scheme_root_checked = @intFromEnum(scheme.root);
        frame.interface_root = declared;
        frame.request_root = requested;
        switch (caller.view.payload(site.instantiated_root)) {
            .function => |request_function| {
                frame.request_ret_module = caller.module_bytes;
                frame.request_ret_checked = @intFromEnum(request_function.ret);
            },
            .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => {},
        }
        noteEnvironmentScheme(scheme);

        // Every producer representation the requesting context's emission
        // carries is declared at the scheme's positions, exactly as on the
        // same-module path.
        if (frame.request_root) |requested_root| {
            const deep_floor = self.declareSealedProducerInputsDeep(start.cursor.module_bytes, scheme.root, requested_root);
            if (frame.input_floor == null) frame.input_floor = deep_floor;
        }
        return true;
    }

    /// Whether a candidate binding produced the exact witness that accepts it:
    /// the callee's scheme root emitted under the binding and the requesting
    /// site's own instantiated root are the same type. Two rooted recursive
    /// graphs entered from different paths store different digests for one type
    /// (reunify.md section 8.3), so the unfolding decides those.
    fn witnessesAgree(self: *Rehearsal, declared: ?Type.TypeId, requested: ?Type.TypeId) bool {
        const left = declared orelse {
            return false;
        };
        const right = requested orelse {
            return false;
        };
        // The witness proves the binding produced the requested logical
        // instantiation. Representation tiers are section 10 content that a
        // producer legitimately settles on one side before the other, so the
        // comparison reads the representation-erased identity.
        const left_digest = self.types.specializationDigest(self.program_names, left);
        const right_digest = self.types.specializationDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes)) {
            return true;
        }
        const left_unfolded = self.types.unfoldedDigest(self.program_names, left);
        const right_unfolded = self.types.unfoldedDigest(self.program_names, right);
        if (std.mem.eql(u8, &left_unfolded.bytes, &right_unfolded.bytes)) {
            return true;
        }
        return false;
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
            .root_request => {},
            .generated_request => {},
            .no_site => {},
            .site_ambiguous => {},
            .defining_module_differs => {},
            .edge_unusable => {},
        }
        switch (scheme.owner_kind) {
            .top_level_def => {},
            .nested_def => {},
            .required_type => {},
            .synthetic => {},
        }
        switch (start.target_kind) {
            .roc => {},
            .hosted => {},
            .intrinsic => {},
            .entry => {},
            .comptime_only => {},
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
            return .unresolved_request_context;
        }
        const free = self.firstFreeVariable(caller.view, actual, caller_env) orelse {
            // Every variable the actual reaches is bound, so the residual came
            // in through one of those bindings — or the empty row is checked
            // content the requesting body really names.
            if (caller_origin != .absent) {
                return caller_origin;
            }
            return .closed_empty_row;
        };
        switch (caller.view.payload(actual)) {
            .flex, .rigid => {},
            .pending, .err, .alias, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => {},
        }
        for (caller.view.schemes) |scheme| {
            for (scheme.generalizedVars(caller.view)) |binder| {
                if (binder != free) continue;
                return .scheme_binder;
            }
        }
        for (caller.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(free)) continue;
            if (disposition.scheme_owner_node == owner_node) {
                return .disposed_here;
            }
            return .disposed_elsewhere;
        }
        return .undisposed;
    }

    /// Whether a stored type carries the empty tag union anywhere, which is what
    /// an undisposed, undefaulted residual variable materializes to. Checking
    /// the whole value rather than its head catches a residual nested inside a
    /// structure, which is where most bound residuals sit.
    fn carriesResidualMaterialization(self: *Rehearsal, root: Type.TypeId) bool {
        var visited = collections.DenseMap(Type.TypeId, void).init(self.allocator);
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
            .top_level_def => {},
            .nested_def => {},
            .required_type => {},
            .synthetic => {},
        }
        if (scheme.captured_len == 0) {} else {}
    }

    /// Whether a checked scheme root reaches any checked variable payload —
    /// the structure a binder would name. A scheme whose root reaches none is
    /// monomorphic, so an empty binder vector describes it exactly.
    /// Whether a checked root reaches any variable payload, for callers
    /// judging whether a callable is ground.
    pub fn checkedRootReachesVariable(
        self: *Rehearsal,
        module_bytes: [32]u8,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        const key = ReachMemoKey{ .module_bytes = module_bytes, .ty = root, .kind = .variable };
        if (self.reach_memo.get(key)) |hit| return hit;
        const answer = self.checkedRootReachesVariableWalk(view, root);
        self.reach_memo.put(key, answer) catch return answer;
        return answer;
    }

    fn checkedRootReachesVariableWalk(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        return self.schemeRootReachesVariable(view, root);
    }

    /// Whether a checked root reaches a function type anywhere in its
    /// structure — through nominal backing declarations, aliases, and
    /// containers — which is the population whose lowering may erase and box
    /// that callable. Erased-reuse ownership is decided across the
    /// instantiation graph's relations, so a request whose answer carries one
    /// must instantiate open nodes rather than a frozen constant. A checked
    /// nominal names no instantiated backing of its own, so each nominal's
    /// backing declaration is resolved and walked in its defining module.
    /// Whether a checked root reaches a tag row or a function anywhere,
    /// including through alias and nominal backings. Rows are where graph-side
    /// widening lives and functions are where capture evidence lives, so a
    /// bound request that reaches either cannot be born as a frozen constant.
    pub fn checkedRootReachesRowOrFunction(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
    ) bool {
        const key = ReachMemoKey{ .module_bytes = cursor.module_bytes, .ty = root, .kind = .row_or_function };
        if (self.reach_memo.get(key)) |hit| return hit;
        const answer = self.checkedRootReachesRowOrFunctionWalk(cursor, root);
        self.reach_memo.put(key, answer) catch return answer;
        return answer;
    }

    fn checkedRootReachesRowOrFunctionWalk(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
    ) bool {
        const Position = struct {
            module_bytes: [32]u8,
            ty: checked.CheckedTypeId,
        };
        const Entry = struct {
            cursor: direct_translate.ModuleCursor,
            ty: checked.CheckedTypeId,
        };
        var visited = std.AutoHashMap(Position, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(Entry).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, .{ .cursor = cursor, .ty = root }) catch return true;
        while (stack.pop()) |entry| {
            const gop = visited.getOrPut(.{ .module_bytes = entry.cursor.module_bytes, .ty = entry.ty }) catch return true;
            if (gop.found_existing) continue;
            const view = entry.cursor.view;
            switch (view.payload(entry.ty)) {
                .function => return true,
                .flex, .rigid, .pending, .err, .empty_record, .empty_tag_union => {},
                .alias => |alias_ty| {
                    stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = alias_ty.backing }) catch return true;
                    for (alias_ty.args) |arg| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = arg }) catch return true;
                },
                .record => |record_ty| {
                    for (record_ty.fields) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field.ty }) catch return true;
                    stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = record_ty.ext }) catch return true;
                },
                .record_unbound => |fields| {
                    for (fields) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field.ty }) catch return true;
                },
                .tuple => |elems| {
                    for (elems) |elem| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = elem }) catch return true;
                },
                .nominal => |nominal_ty| {
                    for (nominal_ty.args) |arg| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = arg }) catch return true;
                    for (nominal_ty.padding_field_types) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field }) catch return true;
                    if (self.translator.resolver.nominalBacking(entry.cursor, nominal_ty)) |backing| {
                        stack.append(self.allocator, .{ .cursor = backing.cursor, .ty = backing.root }) catch return true;
                    }
                },
                .tag_union => return true,
            }
        }
        return false;
    }
    pub fn checkedRootReachesFunction(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
    ) bool {
        const key = ReachMemoKey{ .module_bytes = cursor.module_bytes, .ty = root, .kind = .function };
        if (self.reach_memo.get(key)) |hit| return hit;
        const answer = self.checkedRootReachesFunctionWalk(cursor, root);
        self.reach_memo.put(key, answer) catch return answer;
        return answer;
    }

    fn checkedRootReachesFunctionWalk(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
    ) bool {
        const Position = struct {
            module_bytes: [32]u8,
            ty: checked.CheckedTypeId,
        };
        const Entry = struct {
            cursor: direct_translate.ModuleCursor,
            ty: checked.CheckedTypeId,
        };
        var visited = std.AutoHashMap(Position, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(Entry).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, .{ .cursor = cursor, .ty = root }) catch return true;
        while (stack.pop()) |entry| {
            const gop = visited.getOrPut(.{ .module_bytes = entry.cursor.module_bytes, .ty = entry.ty }) catch return true;
            if (gop.found_existing) continue;
            const view = entry.cursor.view;
            switch (view.payload(entry.ty)) {
                .function => return true,
                .flex, .rigid, .pending, .err, .empty_record, .empty_tag_union => {},
                .alias => |alias_ty| {
                    stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = alias_ty.backing }) catch return true;
                    for (alias_ty.args) |arg| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = arg }) catch return true;
                },
                .record => |record_ty| {
                    for (record_ty.fields) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field.ty }) catch return true;
                    stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = record_ty.ext }) catch return true;
                },
                .record_unbound => |fields| {
                    for (fields) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field.ty }) catch return true;
                },
                .tuple => |elems| {
                    for (elems) |elem| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = elem }) catch return true;
                },
                .nominal => |nominal_ty| {
                    for (nominal_ty.args) |arg| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = arg }) catch return true;
                    for (nominal_ty.padding_field_types) |field| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = field }) catch return true;
                    if (self.translator.resolver.nominalBacking(entry.cursor, nominal_ty)) |backing| {
                        stack.append(self.allocator, .{ .cursor = backing.cursor, .ty = backing.root }) catch return true;
                    }
                },
                .tag_union => |tag_ty| {
                    for (tag_ty.tags) |tag| {
                        for (tag.argsSlice(view)) |arg| stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = arg }) catch return true;
                    }
                    stack.append(self.allocator, .{ .cursor = entry.cursor, .ty = tag_ty.ext }) catch return true;
                },
            }
        }
        return false;
    }

    /// Whether a checked root reaches a variable that would DEFAULT under
    /// directed translation - a numeral or row default - which is the class
    /// whose value the graph takes from the other operand instead (the
    /// literal-leaves law). A plain binder either binds under the reading
    /// frame or declines; only a defaultable one can answer wrongly.
    pub fn checkedRootReachesDefaultableVariable(
        self: *Rehearsal,
        module_bytes: [32]u8,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        const key = ReachMemoKey{ .module_bytes = module_bytes, .ty = root, .kind = .defaultable };
        if (self.reach_memo.get(key)) |hit| return hit;
        const answer = self.checkedRootReachesDefaultableVariableWalk(view, root);
        self.reach_memo.put(key, answer) catch return answer;
        return answer;
    }

    fn checkedRootReachesDefaultableVariableWalk(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        var visited = collections.DenseMap(checked.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(checked.CheckedTypeId).empty;
        defer stack.deinit(self.allocator);
        stack.append(self.allocator, root) catch return true;
        while (stack.pop()) |ty| {
            const gop = visited.getOrPut(ty) catch return true;
            if (gop.found_existing) continue;
            switch (view.payload(ty)) {
                .flex, .rigid => |v| {
                    if (v.numeric_default_phase != null or v.row_default != null) return true;
                },
                .pending, .err, .alias, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => {},
            }
            self.pushCheckedChildren(view, ty, &stack) catch return true;
        }
        return false;
    }

    fn pushCheckedChildren(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        ty: checked.CheckedTypeId,
        stack: *std.ArrayList(checked.CheckedTypeId),
    ) Allocator.Error!void {
        switch (view.payload(ty)) {
            .flex, .rigid, .pending, .err, .empty_record, .empty_tag_union => {},
            .alias => |alias_ty| {
                try stack.append(self.allocator, alias_ty.backing);
                for (alias_ty.args) |arg| try stack.append(self.allocator, arg);
            },
            .record => |record_ty| {
                for (record_ty.fields) |field| try stack.append(self.allocator, field.ty);
                try stack.append(self.allocator, record_ty.ext);
            },
            .record_unbound => |fields| {
                for (fields) |field| try stack.append(self.allocator, field.ty);
            },
            .tuple => |elems| {
                for (elems) |elem| try stack.append(self.allocator, elem);
            },
            .function => |fn_ty| {
                for (fn_ty.args) |arg| try stack.append(self.allocator, arg);
                try stack.append(self.allocator, fn_ty.ret);
            },
            .nominal => |nominal_ty| {
                for (nominal_ty.args) |arg| try stack.append(self.allocator, arg);
                for (nominal_ty.padding_field_types) |field| try stack.append(self.allocator, field);
            },
            .tag_union => |tag_ty| {
                for (tag_ty.tags) |tag| {
                    for (tag.argsSlice(view)) |arg| try stack.append(self.allocator, arg);
                }
                try stack.append(self.allocator, tag_ty.ext);
            },
        }
    }

    fn schemeRootReachesVariable(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) bool {
        var visited = collections.DenseMap(checked.CheckedTypeId, void).init(self.allocator);
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
            return null;
        }
        const frame = &self.frames.items[self.frames.items.len - 1];
        if (!frame.env_ready) {
            return null;
        }
        if (!std.mem.eql(u8, &frame.env_module_bytes, &module_bytes)) {
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
        if (!self.positionHoldsVariable(cursor, @enumFromInt(address.type_id))) return null;
        const free = self.firstFreeVariable(cursor.view, @enumFromInt(address.type_id), base_env) orelse return null;
        const owner = self.schemeOwnerGeneralizing(cursor, free) orelse return null;
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
                    return null;
                }
                found = candidate;
            }
            if (found == null) {
                return null;
            }
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
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            chain.innermost(),
            base_owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch null;
    }

    /// Declare, at a consumer's own address, the producer representation one
    /// generated rule states, returning the floor to retract to after the
    /// consumer's read. The rule derivation is the same one a callee binding
    /// runs; only the address is the consumer's (reunify.md 13.2e).
    pub fn declareConsumerInputAt(
        self: *Rehearsal,
        address: CheckedAddress,
        declared: GeneratedEdge,
    ) ?usize {
        const source = declared.source orelse return null;
        const procedure = source.procedure orelse return null;
        const kind = kindForIteratorProcedure(procedure) orelse return null;
        const caller = self.lookup.cursor(source.module_bytes) orelse return null;
        const topology_lookup = self.lookup.iterator_topology orelse return null;
        const topology_ids = topology_lookup(self.lookup.context, source.module_bytes) orelse return null;
        const topology = self.internTopology(caller, topology_ids) orelse return null;
        var depth: u8 = 0;
        var receiver: ?Type.TypeId = null;
        var reason: direct_translate.SkipReason = undefined;
        if (self.translator.translateUnderEnvironment(
            caller,
            if (self.frameForModule(source.module_bytes)) |frame| frame.environment() else null,
            checked.checked_residual_disposition_module_body_owner,
            source.receiver.checked_ty,
            &reason,
        )) |receiver_ty| {
            receiver = receiver_ty;
            switch (self.types.get(receiver_ty)) {
                .named => |named| depth = named.def.iterator_depth,
                .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => {},
            }
        } else |_| {}
        const components = self.pooledReceiverComponents(receiver);
        const over_cap = depth >= max_minted_chain_depth;
        const representation: direct_translate.ProducerRepresentation = if (over_cap) .{
            .iterator_representation = .forced_dynamic,
            .iterator_kind = .forced_dynamic,
            .iterator_depth = 0,
            .topology = topology,
            .minting = .{ .callable_evidence = null },
        } else .{
            .iterator_representation = .minted,
            .iterator_kind = kind,
            .iterator_depth = depth + 1,
            .topology = topology,
            // The callable evidence the mint is minted under travels on the
            // edge; the stamped identity folds it in exactly as the graph's
            // finalize pass does.
            .minting = .{ .callable_evidence = source.evidence },
            .components = components,
        };
        const floor = self.translator.representationInputCount();
        self.translator.declareRepresentationInput(.{
            .position = .{ .module_bytes = address.module_bytes, .type_id = address.type_id },
            .representation = representation,
        }) catch return null;
        return floor;
    }

    /// Stamp the generated-owner digest a finished minted representation
    /// records, mirroring the graph's finalize pass on the emission side: the
    /// digest is computed over the pre-stamp form, with the callable evidence
    /// the representation was minted under folded in when one exists
    /// (reunify.md 13.2e). A head that is not a finished generated iterator
    /// returns unchanged.
    pub fn stampGeneratedIdentity(
        self: *Rehearsal,
        ty: Type.TypeId,
        callable_evidence: ?names.TypeDigest,
    ) Type.TypeId {
        const named = switch (self.types.get(ty)) {
            .named => |named| named,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return ty,
        };
        if (named.def.generated != null) return ty;
        if (named.def.iterator_topology == null) return ty;
        switch (named.def.iterator_representation) {
            .minted, .forced_dynamic => {},
            .none => return ty,
        }
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        if (named.def.iterator_representation == .forced_dynamic) {
            const args = self.types.span(named.args);
            if (GuardedList.borrowLen(args) != 1) return ty;
            const item_digest = self.types.typeDigest(self.program_names, GuardedList.at(args, 0));
            hasher.update("roc.generated_iterator.forced_dynamic_identity");
            hasher.update(&item_digest.bytes);
        } else {
            const shape = self.types.typeDigest(self.program_names, ty);
            hasher.update("roc.generated_iterator.final_identity");
            hasher.update(&shape.bytes);
            if (callable_evidence) |evidence| {
                hasher.update("callable_evidence");
                hasher.update(&evidence.bytes);
            }
        }
        var def = named.def;
        def.generated = .{ .bytes = hasher.finalResult() };

        var arg_buf: [17]Type.TypeId = undefined;
        const args = self.types.span(named.args);
        const arg_len = GuardedList.borrowLen(args);
        if (arg_len > arg_buf.len) return ty;
        var index: usize = 0;
        while (index < arg_len) : (index += 1) arg_buf[index] = GuardedList.at(args, index);

        var order_buf: [32]Type.DeclaredField = undefined;
        const order = self.types.declaredFieldSpan(named.declared_order);
        const order_len = GuardedList.borrowLen(order);
        if (order_len > order_buf.len) return ty;
        index = 0;
        while (index < order_len) : (index += 1) order_buf[index] = GuardedList.at(order, index);

        return self.types.internNamed(self.program_names, .{
            .named_type = named.named_type,
            .def = def,
            .kind = named.kind,
            .builtin_owner = named.builtin_owner,
            .args = arg_buf[0..arg_len],
            .backing = named.backing,
            .declared_order = order_buf[0..order_len],
        }) catch ty;
    }

    /// Retract consumer-declared inputs back to the floor their read opened.
    pub fn retractConsumerInputs(self: *Rehearsal, floor: usize) void {
        self.translator.truncateRepresentationInputs(floor);
    }

    /// The Monotype this lowering gives one checked position, as production
    /// authority: directed instantiation of the position under the innermost
    /// binding recorded for it, with no logical solving. Total or fatal - a
    /// position this cannot answer is a compiler defect named by its reason,
    /// never a silent decline (reunify.md 13.2d).
    pub fn typeForCheckedAuthoritative(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        edge: ?RequestEdgeName,
    ) Allocator.Error!Type.TypeId {
        const clock = self.cost_clock;
        const started = if (clock) |active| active.enterDirected() else null;
        defer if (clock) |active| active.leaveDirected(started);
        if (self.disabled) {
            Common.invariant("directed instantiation state was disabled while holding production authority");
        }
        const cursor = self.lookup.cursor(address.module_bytes) orelse
            Common.invariant("directed instantiation read a checked position of an unloaded module");
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
        if (self.typeUnderEdgeLevel(address, env, owner_node, edge)) |leveled| return leveled;
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            env,
            owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch |err| switch (err) {
            error.Skip => switch (reason) {
                .recursive_cycle => Common.invariant("directed instantiation could not close a recursive cycle"),
                .pending_or_err => Common.invariant("directed instantiation read a pending or erroneous checked position"),
                .numeric_default_unresolved => Common.invariant("directed instantiation read an unresolved numeric default"),
                .open_row => Common.invariant("directed instantiation read an open row"),
                .malformed_builtin_arity => Common.invariant("directed instantiation read a builtin with malformed arity"),
                .binder_not_found => Common.invariant("directed instantiation read a binder no environment names"),
                .missing_backing => Common.invariant("directed instantiation read a nominal with no backing source"),
                .engine_input_needed => Common.invariant("directed instantiation left a representation position unemitted"),
                .undisposed_residual => Common.invariant("directed instantiation read a residual no disposition, default, or binding answers"),
            },
            else => |other| other,
        };
    }

    /// The Monotype at one checked position as production authority, or null
    /// exactly when the position holds a residual no disposition, default, or
    /// binding answers — the one condition under which no final type can be
    /// stated, so the caller keeps that position on its graph node where the
    /// relations that bind it still apply (reunify.md 7.4, 13.2d). Every other
    /// reason a translation cannot answer stays fatal, exactly as
    /// `typeForCheckedAuthoritative` states it.
    pub fn typeForCheckedAuthoritativeOrUnstated(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
        edge: ?RequestEdgeName,
    ) Allocator.Error!?Type.TypeId {
        const clock = self.cost_clock;
        const started = if (clock) |active| active.enterDirected() else null;
        defer if (clock) |active| active.leaveDirected(started);
        if (self.disabled) {
            Common.invariant("directed instantiation state was disabled while holding production authority");
        }
        const cursor = self.lookup.cursor(address.module_bytes) orelse
            Common.invariant("directed instantiation read a checked position of an unloaded module");
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
        if (self.typeUnderEdgeLevel(address, env, owner_node, edge)) |leveled| return leveled;
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.translateUnderEnvironment(
            cursor,
            env,
            owner_node,
            @enumFromInt(address.type_id),
            &reason,
        ) catch |err| switch (err) {
            error.Skip => switch (reason) {
                .recursive_cycle => Common.invariant("directed instantiation could not close a recursive cycle"),
                .pending_or_err => Common.invariant("directed instantiation read a pending or erroneous checked position"),
                .numeric_default_unresolved => Common.invariant("directed instantiation read an unresolved numeric default"),
                .open_row => Common.invariant("directed instantiation read an open row"),
                .malformed_builtin_arity => Common.invariant("directed instantiation read a builtin with malformed arity"),
                .binder_not_found => Common.invariant("directed instantiation read a binder no environment names"),
                .missing_backing => Common.invariant("directed instantiation read a nominal with no backing source"),
                .engine_input_needed => Common.invariant("directed instantiation left a representation position unemitted"),
                .undisposed_residual => {
                    return null;
                },
            },
            else => |other| other,
        };
    }

    /// The same, first trying a level for the definition the position's unbound
    /// variable belongs to, bound from the site the entering edge names.
    /// The position's type as a SEAL states it: the same directed emission a
    /// read performs, with a residual no disposition or default answers
    /// materialized as the uninhabited row instead of declining (reunify.md
    /// 7.4). A read must not state that; a seal must, because it produces the
    /// position's final stored type and that row is what the residual denotes.
    pub fn sealingTypeForCheckedPosition(
        self: *Rehearsal,
        address: CheckedAddress,
        under_callee: bool,
    ) ?Type.TypeId {
        if (self.disabled) return null;
        const cursor = self.lookup.cursor(address.module_bytes) orelse return null;
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
        var reason: direct_translate.SkipReason = undefined;
        return self.translator.eagerWalkDisposing(
            cursor,
            env,
            owner_node,
            @enumFromInt(address.type_id),
            &reason,
            true,
        ) catch null;
    }

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
            error.Skip => {
                // Name why the position states nothing, so the declining set is
                // a per-reason list rather than a bulk (reunify.md 13.2 2a).
                if (comptime census.enabled) switch (reason) {
                    .recursive_cycle => census.bump("position_skip_recursive_cycle"),
                    .pending_or_err => census.bump("position_skip_pending_or_err"),
                    .numeric_default_unresolved => census.bump("position_skip_numeric_default"),
                    .open_row => census.bump("position_skip_open_row"),
                    .malformed_builtin_arity => census.bump("position_skip_builtin_arity"),
                    .binder_not_found => census.bump("position_skip_binder_not_found"),
                    .missing_backing => census.bump("position_skip_missing_backing"),
                    .engine_input_needed => census.bump("position_skip_engine_input"),
                    .undisposed_residual => census.bump("position_skip_undisposed_residual"),
                };
                return null;
            },
            else => |other| other,
        };
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
            return error.SiteAmbiguous;
        }
        const site_index = index.by_edge.get(key) orelse {
            if (index.used_exprs.contains(@intFromEnum(use_expr))) {} else {}
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

    /// Which scheme's generalization names this variable, if any.
    fn schemeOwnerGeneralizing(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        free: checked.CheckedTypeId,
    ) ?u32 {
        const key = VariablePresenceKey{
            .module_bytes = cursor.module_bytes,
            .type_id = @intFromEnum(free),
        };
        if (self.binder_owners.get(key)) |known| return known;
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
        self.binder_owners.put(self.allocator, key, owner_node) catch return owner_node;
        return owner_node;
    }

    /// Whether this checked position holds any variable at all. With no
    /// environment nothing is bound, so the first-free search answers exactly
    /// that question, and its answer is a property of the checked store alone.
    fn positionHoldsVariable(
        self: *Rehearsal,
        cursor: direct_translate.ModuleCursor,
        root: checked.CheckedTypeId,
    ) bool {
        const key = VariablePresenceKey{
            .module_bytes = cursor.module_bytes,
            .type_id = @intFromEnum(root),
        };
        if (self.variable_presence.get(key)) |known| return known;
        const holds = self.firstFreeVariable(cursor.view, root, null) != null;
        self.variable_presence.put(self.allocator, key, holds) catch return holds;
        return holds;
    }

    /// The first checked variable reachable from `root` that no level of `env`
    /// binds, in the walk order the translation itself descends.
    fn firstFreeVariable(
        self: *Rehearsal,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
        env: ?*const direct_translate.BindingEnvironment,
    ) ?checked.CheckedTypeId {
        var visited = collections.DenseMap(checked.CheckedTypeId, void).init(self.allocator);
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

    /// Build the representation slot for one emitted occurrence (reunify.md
    /// section 10.2). The slot is created fresh at every position the walk
    /// reaches: a stored type id names a type, not an occurrence (reunify.md
    /// section 8.5), so keying slots by it would pre-join independent occurrences
    /// that interning collapsed to one id and let one occurrence's representation
    /// flow reach another with no value-flow relation between them. Two
    /// occurrences are joined only by an explicit relation; a back reference
    /// inside one occurrence stops at `max_slot_depth`.
    /// Record the minted member's type for a slot's relation class, the
    /// section 10.6 slot-final the draft seal projects. The recorded type is
    /// the member whose sealed descriptor carries a mint; a class with no
    /// minted member records nothing and its slots seal from their own
    /// emitted types.
    fn recordClassFinal(self: *Rehearsal, slot: closure.RepresentationSlotId) void {
        const ty = self.slot_types.get(@intFromEnum(slot)) orelse return;
        const named = switch (self.types.get(ty)) {
            .named => |named| named,
            .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return,
        };
        if (named.def.iterator_representation == .none) return;
        const root = self.engine.find(slot);
        self.class_finals.put(self.allocator, @intFromEnum(root), ty) catch {};
    }

    /// The final type a slot's relation class carries: the minted member's
    /// recorded type, or the slot's own emitted type where the class carries
    /// no mint. Null for a slot the rehearsal never emitted a type for.
    pub fn slotFinal(self: *Rehearsal, slot: closure.RepresentationSlotId) ?Type.TypeId {
        const root = self.engine.find(slot);
        if (self.class_finals.get(@intFromEnum(root))) |final| return final;
        return self.slot_types.get(@intFromEnum(slot));
    }

    fn slotForEmitted(self: *Rehearsal, ty: Type.TypeId, depth: u32) ?closure.RepresentationSlotId {
        if (depth >= max_slot_depth) return null;
        const token = self.tokenFor(ty) orelse return null;
        const shape = self.shapeFor(ty, token, depth) orelse return null;
        const slot = self.engine.createSlot(token, self.freshProducer(), shape) catch return null;
        self.slots.append(self.allocator, slot) catch return null;
        if (shape == .iterator) {
            self.slot_descriptors.put(self.allocator, @intFromEnum(slot), shape.iterator.descriptor) catch return null;
        }
        self.slot_types.put(self.allocator, @intFromEnum(slot), ty) catch return null;
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
            .primitive, .record, .tuple, .tag_union, .func, .erased, .zst => return .{ .leaf = @intFromEnum(token) },
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
        .primitive, .record, .tuple, .tag_union, .func, .erased, .zst => null,
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
                .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return null,
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
                .primitive, .named, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return null,
            },
            .tuple_element => |index| switch (store.get(current)) {
                .tuple => |items| blk: {
                    const entries = store.span(items);
                    if (index >= GuardedList.borrowLen(entries)) return null;
                    break :blk GuardedList.at(entries, index);
                },
                .primitive, .named, .record, .tag_union, .list, .box, .func, .erased, .zst => return null,
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
                .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => return null,
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
                .primitive, .named, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return false,
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
                .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => return false,
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
        .primitive, .named, .tuple, .list, .box, .func, .erased, .zst => return true,
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
        .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return false,
    };
    const right_named = switch (right_store.get(right)) {
        .named => |named| named,
        .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => return false,
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
        .primitive, .record, .tuple, .tag_union, .func, .erased, .zst => null,
    };
}

/// The argument at `index` of an emitted function type, or null when the type is
/// not a function or carries no such argument. The receiver-position witness
/// reads the callee scheme root through this.
/// The checked type behind however many alias layers wrap it, which is the
/// structure a stored instance mirrors: emission expands aliases, so the
/// stored side of a lockstep walk never has an alias layer of its own.
fn checkedThroughAliases(view: checked.CheckedTypeStoreView, ty: checked.CheckedTypeId) checked.CheckedTypeId {
    var current = ty;
    var remaining: usize = max_binder_path_steps;
    while (remaining > 0) : (remaining -= 1) {
        switch (view.payload(current)) {
            .alias => |alias_ty| current = alias_ty.backing,
            .pending, .err, .flex, .rigid, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => return current,
        }
    }
    return current;
}

fn functionArgumentAt(store: *const Type.Store, root: Type.TypeId, index: u32) ?Type.TypeId {
    return switch (store.get(root)) {
        .func => |fn_ty| blk: {
            const args = store.span(fn_ty.args);
            if (index >= GuardedList.borrowLen(args)) break :blk null;
            break :blk GuardedList.at(args, index);
        },
        .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => null,
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
        // Carries the MINT for a direct iterator-procedure call, not a binder
        // mapping: the call's ordinary checked use site binds.
        .iterator_direct_call,
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
