//! Directed scheme instantiation that emits STORED-form Monotype ids
//! (reunify.md section 9), relocated into the production Monotype module as
//! verified-inert code for the Slice 7 flip staging (Stage A). Nothing in the
//! output path calls it yet: it is dead-but-compiled and exercised only by a
//! Debug, env-gated equality probe (`runDirectTranslateProbe` in lower.zig)
//! that compares its output against the graph's sealed types. Stage E repoints
//! the production lowering seam onto this module and deletes the graph.
//!
//! Where the Slice 5 shadow (`reunify_shadow/logical_identity.zig`) erases
//! representation and every backed source alias down to a logical skeleton, this
//! module produces the STORED form: the representation-bearing shape that
//! `instNode` plus sealing produce today for ground inputs. Named types keep
//! their backing, builtin dispatch owner, and declared field order; a
//! storage-transparent alias is erased by the store's `internNamed` constructor
//! exactly as production materializes it.
//!
//! Runtime-encoding content the checked data does not dictate — a generated
//! iterator chain's tier, a forced-dynamic fixed point, a generated evidence
//! owner — is not read from the checked module: it is EMITTED here through the
//! section 10 representation layer. A position carrying such content opens a
//! representation slot holding the declared identity checking owns plus the
//! representation the producer stated for that position, the enclosing compound
//! is built as a draft while a slot is open under it, the declared relation runs
//! to its fixpoint, and the draft seals bottom-up into immutable ids. See
//! `Emission` below.
//!
//! Names are interned into the PRODUCTION name store the same way `instNode`
//! resolves them today (module identity rebasing, type/field/tag name
//! interning), so a translated type is name-identical to a graph-produced one.
//! Types are built child-first through the store's `intern*` constructors, which
//! plain-add while interning is off and deduplicate while it is on — correct
//! either way. Cross-module nominal declaration lookups the Builder owns are
//! reached through a small `Resolver` the caller supplies.

const std = @import("std");
const Allocator = std.mem.Allocator;

const check = @import("check");
const collections = @import("collections");

const MonoType = @import("type.zig");
const census = @import("census.zig");
const closure = @import("../representation_closure.zig");
const policy = @import("../representation_policy.zig");

const names = check.CheckedNames;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;

/// A stored Monotype id in the target store.
pub const TypeId = MonoType.TypeId;

/// A scheme reference qualified by its owning module's content identity, so a
/// module-local scheme id never collides across modules (reunify.md section
/// 9.4 memo key).
pub const SchemeIdent = struct {
    module_bytes: [32]u8,
    scheme: u32,
};

/// One binder's stored value in a binding environment (reunify.md section 7.3,
/// 9.1). For directed instantiation the bound type is already a stored id, so
/// the representation half is the stored id itself.
pub const BoundType = struct {
    stored: TypeId,

    pub fn of(stored: TypeId) BoundType {
        return .{ .stored = stored };
    }
};

/// One checked module's frozen types, the name store that resolves its names to
/// text, and its module content identity. A walk carries the cursor of the view
/// it is reading and switches cursors when it descends into a nominal's backing
/// declaration in another module.
pub const ModuleCursor = struct {
    view: checked.CheckedTypeStoreView,
    source_names: *const names.NameStore,
    module_bytes: [32]u8,
};

/// The Builder-owned lookups a nominal translation needs but that this module
/// does not reproduce: the dispatch owner stamp, the backing declaration
/// source, and the declared field order. The caller supplies an implementation;
/// the production probe wraps the Builder, and tests supply a trivial one. The
/// name interning, module rebasing, and structural translation stay here.
pub const Resolver = struct {
    context: *anyopaque,
    vtable: *const VTable,

    /// The declaration source for a nominal's backing (reunify.md section 9.2):
    /// its own module cursor, its formal binders, and its backing root. A walk
    /// instantiates the backing by binding the formals to the instance's
    /// translated argument ids, exactly as `instNominalDeclarationBackingNode`
    /// seeds a scope with the instance's argument nodes.
    pub const NominalBacking = struct {
        cursor: ModuleCursor,
        /// The declaration's own id in its module, which with the module
        /// identity names the declaration a nominal instance is an instance of.
        declaration: u32,
        formal_args: []const checked.CheckedTypeId,
        root: checked.CheckedTypeId,
    };

    /// One declared field-order entry (layout only). A named entry re-interns
    /// its label; a padding entry carries the instance's substituted padding
    /// type in `cursor`, translated like any other checked type.
    pub const DeclaredField = union(enum) {
        named: names.RecordFieldNameId,
        padding: checked.CheckedTypeId,
    };

    pub const VTable = struct {
        builtin_owner: *const fn (
            context: *anyopaque,
            cursor: ModuleCursor,
            nominal: checked.CheckedNominalType,
        ) ?static_dispatch.BuiltinOwner,
        nominal_backing: *const fn (
            context: *anyopaque,
            cursor: ModuleCursor,
            nominal: checked.CheckedNominalType,
        ) ?NominalBacking,
        /// Fills `out` with the declared field order and returns the cursor its
        /// entries read, or null when the nominal has no declared order.
        declared_order: *const fn (
            context: *anyopaque,
            cursor: ModuleCursor,
            nominal: checked.CheckedNominalType,
            out: *std.ArrayList(DeclaredField),
        ) Allocator.Error!?ModuleCursor,
    };

    fn builtinOwner(self: Resolver, cursor: ModuleCursor, nominal: checked.CheckedNominalType) ?static_dispatch.BuiltinOwner {
        return self.vtable.builtin_owner(self.context, cursor, nominal);
    }

    fn nominalBacking(self: Resolver, cursor: ModuleCursor, nominal: checked.CheckedNominalType) ?NominalBacking {
        return self.vtable.nominal_backing(self.context, cursor, nominal);
    }

    fn declaredOrder(
        self: Resolver,
        cursor: ModuleCursor,
        nominal: checked.CheckedNominalType,
        out: *std.ArrayList(DeclaredField),
    ) Allocator.Error!?ModuleCursor {
        return self.vtable.declared_order(self.context, cursor, nominal, out);
    }
};

/// Why a checked root or an instantiation edge fell outside the translatable
/// subset. Recorded by the caller; never a panic.
///
/// `recursive_cycle` is now emitted only when the recursive-group builder cannot
/// close a cycle (a degenerate cycle through a node that reserves no slot);
/// ordinary recursive types are built through the store's recursive-group
/// builder (reunify.md section 9.2, 10.6).
///
/// `engine_input_needed` is the EAGER walk's signal that the root reaches a
/// position whose runtime encoding the checked data does not dictate — a
/// generated opaque-evidence owner or an iterator tier (reunify.md sections
/// 10.1, 10.3). The eager walk interns each child as it finishes it, which
/// leaves no draft for such a position's representation to seal into, so it
/// leaves the walk here and `translateUnderEnvironment` reruns the root as a
/// draft, where the section 10 layer opens, closes, and seals the position. It
/// therefore reaches a caller only when the draft rerun ALSO leaves the subset
/// at the same position, which is a genuinely unemittable position.
pub const SkipReason = enum {
    recursive_cycle,
    pending_or_err,
    numeric_default_unresolved,
    open_row,
    malformed_builtin_arity,
    binder_not_found,
    missing_backing,
    engine_input_needed,
    /// A variable no disposition, default, or binding answers. Emitting a type
    /// here would state knowledge the checked module does not hold — the empty
    /// tag union it once produced is indistinguishable from a genuinely
    /// uninhabited position — so the walk declines instead (reunify.md 7.4).
    undisposed_residual,
};

/// A walk left the translatable subset (with `reason` recorded on the walker),
/// or the target store ran out of memory.
pub const WalkError = error{Skip} || Allocator.Error;

/// Identity of one active checked node, qualified by its module so a cross-module
/// backing descent never confuses two modules' node ids.
const ActiveNode = struct {
    module_bytes: [32]u8,
    type_id: u32,
};

/// The 32-byte content digest keying a memoized instantiation (reunify.md
/// section 9.4).
const InstantiationDigest = [32]u8;

/// One nominal instance's identity inside a reserve-before-descend walk: the
/// declaration it instantiates plus the stored ids its arguments translated to.
/// This is the reunify.md section 9.4 instantiation key at a nominal
/// declaration — checking allocates a distinct checked id for every occurrence
/// of one nominal, so the checked address alone cannot recognize that a
/// declaration's backing reached the very instance it is the backing of, and the
/// knot would close one level deeper on the backing instead of on the nominal.
const NominalInstance = struct {
    module_bytes: [32]u8,
    declaration: u32,
    args: []const TypeId,
    slot: TypeId,

    fn sameInstance(self: NominalInstance, module_bytes: [32]u8, declaration: u32, args: []const TypeId) bool {
        if (self.declaration != declaration) return false;
        if (!std.mem.eql(u8, &self.module_bytes, &module_bytes)) return false;
        if (self.args.len != args.len) return false;
        for (self.args, args) |left, right| {
            if (left != right) return false;
        }
        return true;
    }
};

/// The nominal instances one reserve-fill walk has reserved a slot for. A walk
/// builds a handful of them, so the lookup is a scan over the exact key rather
/// than a hash of it.
const NominalInstances = struct {
    allocator: Allocator,
    items: std.ArrayList(NominalInstance),

    fn init(allocator: Allocator) NominalInstances {
        return .{ .allocator = allocator, .items = .empty };
    }

    fn deinit(self: *NominalInstances) void {
        for (self.items.items) |entry| self.allocator.free(entry.args);
        self.items.deinit(self.allocator);
    }

    fn find(self: *const NominalInstances, module_bytes: [32]u8, declaration: u32, args: []const TypeId) ?TypeId {
        for (self.items.items) |entry| {
            if (entry.sameInstance(module_bytes, declaration, args)) return entry.slot;
        }
        return null;
    }

    fn record(
        self: *NominalInstances,
        module_bytes: [32]u8,
        declaration: u32,
        args: []const TypeId,
        slot: TypeId,
    ) Allocator.Error!void {
        const owned = try self.allocator.dupe(TypeId, args);
        errdefer self.allocator.free(owned);
        try self.items.append(self.allocator, .{
            .module_bytes = module_bytes,
            .declaration = declaration,
            .args = owned,
            .slot = slot,
        });
    }
};

/// Where one checked position lives: the content identity of its module plus
/// the module-local checked type id. A declared representation input names the
/// position it applies to by this address.
pub const PositionAddress = struct {
    module_bytes: [32]u8,
    type_id: u32,

    fn eql(self: PositionAddress, other: PositionAddress) bool {
        return self.type_id == other.type_id and
            std.mem.eql(u8, &self.module_bytes, &other.module_bytes);
    }
};

/// The runtime-encoding content a producer placed at one position (reunify.md
/// section 10.1). Checking owns the declared identity there; this owns
/// everything about the runtime encoding checking does not dictate — the
/// iterator tier, the producer kind that minted it, the minted-chain depth, and
/// the generated-owner digest a finished representation records.
pub const ProducerRepresentation = struct {
    iterator_representation: MonoType.IteratorRepresentation = .none,
    iterator_kind: MonoType.IteratorKind = .none,
    iterator_depth: u8 = 0,
    generated: ?names.TypeDigest = null,
    /// The record and tag names a generated iterator's runtime encoding uses.
    topology: ?MonoType.IteratorTopology = null,
    /// The callable evidence this representation is being minted under while
    /// the producer has not recorded a generated digest for it yet (reunify.md
    /// section 10.3).
    minting: ?policy.MintingIdentity = null,
    /// The component types the producer minted this representation over, in
    /// producer order. They sit after the position's public item argument and
    /// are representation, not identity: two representations of one declaration
    /// carry different components and still relate.
    components: []const TypeId = &.{},
    /// The backing the producer generated for this position. When null, the
    /// declaration's own backing stands.
    backing: ?MonoType.NamedBacking = null,
};

/// What the section 10 layer sealed at one position: the definition the position
/// emits, plus the producer-placed content that changes the position's own
/// shape.
const SealedPosition = struct {
    def: MonoType.TypeDef,
    components: []const TypeId = &.{},
    backing: ?MonoType.NamedBacking = null,
};

/// One declared representation input: the position it applies to and the
/// representation the producer placed there. The caller states these before a
/// translation; emission never derives one from a store it reads.
pub const RepresentationInput = struct {
    position: PositionAddress,
    representation: ProducerRepresentation,
};

/// The maximum depth the emission layer builds child representation slots to
/// before modelling a position as an opaque leaf. Representation-bearing spines
/// (iterators, evidence owners, box/list wrappers) are shallow; this only bounds
/// a pathological input.
const max_emission_slot_depth: u32 = 32;

/// The maximum number of producer-minted components emission models on one
/// iterator slot: an iterator states its public item plus the components its
/// producer minted it over, and a longer list leaves the components unmodelled
/// rather than answering the component question from an incomplete list.
const max_emission_components: usize = 16;

/// The representation layer one directed translation runs (reunify.md sections
/// 9.1, 10.2, 10.4, 10.6).
///
/// A position whose runtime encoding the checked data does not dictate opens a
/// **representation slot**. The slot carries that position's fixed
/// representation-erased identity, taken from the declared identity checking
/// owns plus its already-emitted arguments, and a complete represented type: the
/// declaration's own encoding, joined with whatever representation the caller
/// declared the producer placed there. The declared section 10.3 relation then
/// runs to its fixpoint over that slot, and `seal` reads the class
/// representative's representation back out. Sealing recomputes the
/// representation-erased identity of the sealed representation and requires it
/// to equal the one the slot was opened with, so a relation can move the
/// encoding and never the identity.
///
/// Two properties make sealing safe to do at the position rather than after the
/// whole walk. First, a relation is created only from the caller's declared
/// inputs, all of which are stated before the translation starts, so nothing
/// discovered later can move a slot that already sealed. Second, distinct
/// positions of one translation are distinct occurrences (reunify.md section
/// 8.5), so no relation crosses two of them: each slot plus the child relations
/// its rules generate is its own representation dependency component, and the
/// walk's child-first order is that component order. Identity assignment for
/// the enclosing compounds is still deferred — the whole region builds as a
/// draft through reserve-before-descend and interns bottom-up at the end
/// (`translateDraftRoot`), which is section 10.6's sealing boundary.
///
/// Termination (reunify.md section 10.4) has three parts, and none of them is
/// assumed:
///
///  1. The walk terminates. Reserve-before-descend records every compound's
///     reserved stored id before its children are translated and every named
///     position's under its instance identity, so a backing that reaches the
///     position it is the backing of resolves to the reserved id instead of
///     descending again. Each open position therefore opens exactly one slot per
///     reserved id.
///  2. Slot creation terminates. One position creates its own slot, one child
///     slot per public item, backing, and modelled component, and at most one
///     peer slot for the caller's declared input. Child slots are built from
///     already-emitted immutable ids and stop at `max_emission_slot_depth`, so
///     the slot count is bounded by the emitted region.
///  3. The relation terminates. Every relation runs inside the closure engine,
///     whose contract is stated on `representation_closure`: a join's derived
///     identity is `(rule, logical token, sorted producer-atom set)`, inserted
///     into the memo BEFORE descending into backing relations; an active-pair map
///     closes cycles; and the progress measure is the finite tuple of distinct
///     union-find roots, unseen derived identities, and in-flight relation edges.
///     Emission adds no relation outside that engine, and the declared tier order
///     refuses any move back down, so each position's encoding moves upward at
///     most twice (public to minted to forced dynamic) before it seals.
const Emission = struct {
    allocator: Allocator,
    engine: closure.Engine,
    /// The representation inputs the caller declared for this translation.
    inputs: std.ArrayList(RepresentationInput),
    /// Representation-erased identity digests -> dense engine token. The engine
    /// refuses to relate two slots with unequal tokens, so a token is exactly
    /// the identity a representation relation may not move.
    tokens: std.AutoHashMapUnmanaged([32]u8, u64),
    next_token: u64,
    next_producer: u32,
    /// How many positions this translation has opened a representation slot at.
    /// A caller reads it around one root to learn whether emission produced any
    /// of that root's content.
    positions_opened: u64,
    /// The declared inputs in force for the store the current build emits into.
    /// A draft region is built in an isolated scratch store, so the ids an input
    /// names are moved there first and the moved list stands for the duration of
    /// that build. Null means the caller's own list, whose ids name the target.
    active_inputs: ?[]const RepresentationInput = null,

    fn init(allocator: Allocator) Emission {
        return .{
            .allocator = allocator,
            .engine = closure.Engine.init(allocator),
            .inputs = .empty,
            .tokens = .empty,
            .next_token = 1,
            .next_producer = 1,
            .positions_opened = 0,
        };
    }

    fn deinit(self: *Emission) void {
        self.engine.deinit();
        self.inputs.deinit(self.allocator);
        self.tokens.deinit(self.allocator);
    }

    fn inputFor(self: *const Emission, address: PositionAddress) ?ProducerRepresentation {
        // Latest declaration wins: declarations nest with the scopes that
        // state them, so the innermost scope's statement for a position
        // stands over an enclosing one's.
        const stated = self.active_inputs orelse self.inputs.items;
        var index = stated.len;
        while (index > 0) {
            index -= 1;
            if (stated[index].position.eql(address)) return stated[index].representation;
        }
        return null;
    }

    fn freshProducer(self: *Emission) closure.ProducerAtom {
        const atom: closure.ProducerAtom = @enumFromInt(self.next_producer);
        self.next_producer +%= 1;
        return atom;
    }

    fn tokenForDigest(self: *Emission, digest: [32]u8) Allocator.Error!closure.LogicalToken {
        const gop = try self.tokens.getOrPut(self.allocator, digest);
        if (!gop.found_existing) {
            gop.value_ptr.* = self.next_token;
            self.next_token +%= 1;
        }
        return @enumFromInt(gop.value_ptr.*);
    }

    /// Open a representation slot at one position, run the declared relation to
    /// its fixpoint, and return the sealed type definition the position emits.
    ///
    /// `declared` is the definition the checked declaration states, carrying no
    /// runtime-encoding content of its own. The returned definition differs from
    /// it only in the fields section 10.1 owns.
    fn sealPosition(
        self: *Emission,
        store: *const MonoType.Store,
        name_store: *const names.NameStore,
        address: PositionAddress,
        declared: policy.NamedDescriptor,
        args: []const TypeId,
        backing: ?TypeId,
    ) Allocator.Error!SealedPosition {
        census.bump("emission_positions_opened");
        self.positions_opened +%= 1;
        const identity = identityDigest(store, name_store, declared, args);
        const token = try self.tokenForDigest(identity);
        const shape = try self.shapeAt(store, name_store, declared, token, args, backing);
        const slot = try self.engine.createSlot(token, self.freshProducer(), shape);

        const input = self.inputFor(address);
        var produced_stands = false;
        if (input) |stated| {
            census.bump("emission_input_declared");
            produced_stands = try self.applyInput(store, name_store, slot, token, declared, stated, args, backing);
        }

        const sealed = self.engine.sealedDescriptor(slot) orelse declared;
        // Sealing requires the representation-erased identity to survive the
        // relation: only the section 10.1 fields may have moved. The producer's
        // minted components and generated backing are representation too, so they
        // are deliberately outside the identity the slot was opened with.
        const sealed_identity = identityDigest(store, name_store, sealed, args);
        if (!std.mem.eql(u8, &sealed_identity, &identity)) {
            census.bump("emission_seal_identity_lost");
            return .{ .def = declared.def };
        }
        census.bump("emission_slots_sealed");
        if (!encodingEql(sealed.def, declared.def)) census.bump("emission_seal_moved");
        // The producer-placed content that changes the position's own shape rides
        // out with the sealed definition, and only while the class still carries
        // the producer's own representation after the relation settled.
        const placed = if (produced_stands) input else null;
        return .{
            .def = sealed.def,
            .components = if (placed) |stated| stated.components else &.{},
            .backing = if (placed) |stated| stated.backing else null,
        };
    }

    /// Relate the representation the producer placed at this position to the one
    /// the declaration states, under the section 10.3 rule the shared policy
    /// classifies for the pair. An iterator pair takes the tier relation; every
    /// other pair takes producer adoption, whose declared tier order refuses a
    /// move back down. Returns whether the producer's own representation is the
    /// one the slot's class carries once the relation settled.
    fn applyInput(
        self: *Emission,
        store: *const MonoType.Store,
        name_store: *const names.NameStore,
        slot: closure.RepresentationSlotId,
        token: closure.LogicalToken,
        declared: policy.NamedDescriptor,
        input: ProducerRepresentation,
        args: []const TypeId,
        backing: ?TypeId,
    ) Allocator.Error!bool {
        var produced = declared;
        produced.def.iterator_representation = input.iterator_representation;
        produced.def.iterator_kind = input.iterator_kind;
        produced.def.iterator_depth = input.iterator_depth;
        produced.def.generated = input.generated;
        produced.def.iterator_topology = input.topology;
        produced.minting = input.minting;

        const relation = policy.iteratorTierRelation(
            declared,
            produced,
            componentAgreementOf(declared, produced),
        );
        const rule: closure.RepresentationRule = switch (relation) {
            .public_minted => .iterator_public_minted,
            .forced_dynamic => .iterator_forced_dynamic,
            .minted_join => .iterator_minted_join,
            // The pair states one representation, so there is nothing to
            // relate: the producer's own content is adopted at the position.
            .ordinary => {
                self.engine.adoptProducerRepresentation(slot, produced) catch |err| switch (err) {
                    error.NotAProducerRepresentation => {
                        census.bump("emission_input_not_modelled");
                        return false;
                    },
                    error.TierMovedDown => {
                        census.bump("emission_input_refused_tier");
                        return false;
                    },
                };
                return true;
            },
        };

        const peer_shape = try self.shapeAt(store, name_store, produced, token, args, backing);
        const peer = try self.engine.createSlot(token, self.freshProducer(), peer_shape);
        self.engine.relate(slot, peer, rule) catch |err| switch (err) {
            error.LogicallyUnequal => {
                census.bump("emission_input_refused_identity");
                return false;
            },
            else => |other| return other,
        };
        const settled = self.engine.sealedDescriptor(slot) orelse return false;
        return encodingEql(settled.def, produced.def);
    }

    /// The engine shape one emitted position models: an iterator states its
    /// public item, its backing, and the components its producer minted it over;
    /// a score-selected evidence owner states its descriptor; anything else with
    /// a backing is a wrapper, and the rest is an opaque leaf.
    fn shapeAt(
        self: *Emission,
        store: *const MonoType.Store,
        name_store: *const names.NameStore,
        descriptor: policy.NamedDescriptor,
        token: closure.LogicalToken,
        args: []const TypeId,
        backing: ?TypeId,
    ) Allocator.Error!closure.SlotShape {
        const owner = descriptor.builtin_owner;
        if (owner != null and static_dispatch.isIteratorOwner(owner.?) and args.len >= 1) {
            const item = try self.slotForStored(store, name_store, args[0], 0);
            const backing_slot = if (backing) |backing_ty|
                try self.slotForStored(store, name_store, backing_ty, 0)
            else
                try self.standInBacking();
            const components = try self.componentSlots(store, name_store, args[1..]);
            return .{ .iterator = .{
                .descriptor = descriptor,
                .item = item,
                .backing = backing_slot,
                .components = components,
            } };
        }
        if (policy.evidenceOwnerUsesScoreSelection(owner)) {
            return .{ .evidence = .{ .score = 0, .descriptor = descriptor } };
        }
        if (backing) |backing_ty| {
            return .{ .wrapper = try self.slotForStored(store, name_store, backing_ty, 0) };
        }
        return .{ .leaf = @intFromEnum(token) };
    }

    fn componentSlots(
        self: *Emission,
        store: *const MonoType.Store,
        name_store: *const names.NameStore,
        component_types: []const TypeId,
    ) Allocator.Error!closure.ComponentSpan {
        if (component_types.len > max_emission_components) return .{};
        var slots: [max_emission_components]closure.RepresentationSlotId = undefined;
        for (component_types, 0..) |component, index| {
            slots[index] = try self.slotForStored(store, name_store, component, 0);
        }
        return try self.engine.recordComponents(slots[0..component_types.len]);
    }

    /// Build a child slot for an already-emitted stored type. Children of an
    /// emitted position are immutable ids, so their shapes are read straight off
    /// the store; a fresh slot per occurrence keeps two occurrences of one id
    /// from being pre-joined (reunify.md section 8.5).
    fn slotForStored(
        self: *Emission,
        store: *const MonoType.Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        depth: u32,
    ) Allocator.Error!closure.RepresentationSlotId {
        const token = try self.tokenForDigest(storedIdentityDigest(store, name_store, ty));
        if (depth >= max_emission_slot_depth) {
            return try self.engine.createSlot(token, self.freshProducer(), .{ .leaf = @intFromEnum(token) });
        }
        const shape: closure.SlotShape = switch (store.get(ty)) {
            .list, .box => |elem| .{ .wrapper = try self.slotForStored(store, name_store, elem, depth + 1) },
            .named => |named| blk: {
                const descriptor: policy.NamedDescriptor = .{
                    .kind = named.kind,
                    .def = named.def,
                    .builtin_owner = named.builtin_owner,
                };
                const owner = named.builtin_owner;
                const named_args = store.span(named.args);
                const arg_count = collections.GuardedList.borrowLen(named_args);
                if (owner != null and static_dispatch.isIteratorOwner(owner.?) and arg_count >= 1) {
                    const item = try self.slotForStored(
                        store,
                        name_store,
                        collections.GuardedList.at(named_args, 0),
                        depth + 1,
                    );
                    const backing_slot = if (named.backing) |backing|
                        try self.slotForStored(store, name_store, backing.ty, depth + 1)
                    else
                        try self.standInBacking();
                    break :blk .{ .iterator = .{
                        .descriptor = descriptor,
                        .item = item,
                        .backing = backing_slot,
                    } };
                }
                if (policy.evidenceOwnerUsesScoreSelection(owner)) {
                    break :blk .{ .evidence = .{ .score = 0, .descriptor = descriptor } };
                }
                if (named.backing) |backing| {
                    break :blk .{ .wrapper = try self.slotForStored(store, name_store, backing.ty, depth + 1) };
                }
                break :blk .{ .leaf = @intFromEnum(token) };
            },
            else => .{ .leaf = @intFromEnum(token) },
        };
        return try self.engine.createSlot(token, self.freshProducer(), shape);
    }

    /// The shared stand-in backing for an iterator that states none. It carries
    /// the engine's `stand_in` token, which no emitted identity mints, so a rule
    /// that relates two stand-in backings still relates them while rules that
    /// keep distinct identities apart still do.
    fn standInBacking(self: *Emission) Allocator.Error!closure.RepresentationSlotId {
        return try self.engine.createSlot(.stand_in, self.freshProducer(), .{ .leaf = 0 });
    }
};

/// Whether two definitions state the same runtime encoding. Only the section
/// 10.1 fields are compared: the declared identity is checking's and a
/// representation relation never moves it.
fn encodingEql(left: MonoType.TypeDef, right: MonoType.TypeDef) bool {
    if (left.iterator_representation != right.iterator_representation) return false;
    if (left.iterator_kind != right.iterator_kind) return false;
    if (left.iterator_depth != right.iterator_depth) return false;
    if (left.generated == null or right.generated == null) {
        return left.generated == null and right.generated == null;
    }
    return std.mem.eql(u8, &left.generated.?.bytes, &right.generated.?.bytes);
}

/// This translation's answer to the policy's component question. Emission opens
/// one slot per position and states the producer's representation at that same
/// position, so the two operands are the one position's declared and produced
/// representations: their components are the same emitted types by construction.
fn componentAgreementOf(left: policy.NamedDescriptor, right: policy.NamedDescriptor) policy.ComponentAgreement {
    return if (left.builtin_owner == right.builtin_owner) .agree else .differ;
}

/// The representation-erased identity of a position: the declared identity
/// checking owns plus the identities of the already-emitted arguments. Every
/// field section 10.1 owns is left out, so two representations of one
/// declaration digest equal here and the engine will relate them.
fn identityDigest(
    store: *const MonoType.Store,
    name_store: *const names.NameStore,
    descriptor: policy.NamedDescriptor,
    args: []const TypeId,
) [32]u8 {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update("roc.emission.position");
    hasher.update(&.{@intFromEnum(descriptor.kind)});
    hasher.update(std.mem.asBytes(&@intFromEnum(descriptor.def.module)));
    hasher.update(std.mem.asBytes(&@intFromEnum(descriptor.def.type_name)));
    const decl: u32 = descriptor.def.source_decl orelse std.math.maxInt(u32);
    hasher.update(std.mem.asBytes(&decl));
    const owner: u8 = if (descriptor.builtin_owner) |value| @intFromEnum(value) + 1 else 0;
    hasher.update(&.{owner});
    for (args) |arg| {
        hasher.update(&storedIdentityDigest(store, name_store, arg));
    }
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

/// The identity of an already-emitted child. A child of an emitted position is
/// an immutable id, so its stored digest names it exactly.
fn storedIdentityDigest(
    store: *const MonoType.Store,
    name_store: *const names.NameStore,
    ty: TypeId,
) [32]u8 {
    return store.typeDigest(name_store, ty).bytes;
}

/// The directed translation context. It owns no type store: it emits into the
/// caller's target store (the program's types, or a mutable snapshot of them for
/// the probe) through the `intern*` constructors, and re-interns names into the
/// caller's target name store. Destroying it frees only its own memo tables.
pub const Translator = struct {
    allocator: Allocator,
    /// The store this translation emits into. `intern*` calls plain-add while
    /// interning is off (the probe snapshot) and deduplicate while it is on.
    store: *MonoType.Store,
    /// The name store translated names are interned into: the production name
    /// store, so a translated type's names match the graph's. Interning a name
    /// already present returns its existing id and adds nothing.
    target_names: *names.NameStore,
    resolver: Resolver,

    /// The represented instantiation memo (reunify.md section 9.4): keyed by the
    /// qualified scheme plus the ordered bound and captured stored-type digests,
    /// so two instantiations collide only when scheme, bindings, and captures all
    /// agree in representation. Its value is the stored root.
    represented_memo: std.AutoHashMap(InstantiationDigest, TypeId),
    /// The logical instantiation memo (reunify.md section 9.4): keyed by the
    /// qualified scheme plus the ordered bound and captured logical ids, valued
    /// by a representation-free result. Declared and keyed for the flip, where
    /// logical and represented identity split; this stage emits stored form only,
    /// so it stays empty here.
    logical_memo: std.AutoHashMap(InstantiationDigest, TypeId),

    /// The section 10 representation layer this translation emits through.
    emission: Emission,

    pub fn init(
        allocator: Allocator,
        store: *MonoType.Store,
        target_names: *names.NameStore,
        resolver: Resolver,
    ) Translator {
        return .{
            .allocator = allocator,
            .store = store,
            .target_names = target_names,
            .resolver = resolver,
            .represented_memo = std.AutoHashMap(InstantiationDigest, TypeId).init(allocator),
            .logical_memo = std.AutoHashMap(InstantiationDigest, TypeId).init(allocator),
            .emission = Emission.init(allocator),
        };
    }

    pub fn deinit(self: *Translator) void {
        self.emission.deinit();
        self.logical_memo.deinit();
        self.represented_memo.deinit();
    }

    /// State the representation a producer placed at one checked position
    /// (reunify.md sections 10.1, 11.1). Emission takes these as inputs to the
    /// positions it opens; it never derives one by reading another stage's
    /// store. Declaring an input invalidates nothing already emitted, so a
    /// caller states every input for a translation before running it.
    pub fn declareRepresentationInput(
        self: *Translator,
        input: RepresentationInput,
    ) Allocator.Error!void {
        try self.emission.inputs.append(self.allocator, input);
    }

    /// Drop the declared representation inputs, for a caller that reuses one
    /// translator across requests whose declared inputs differ.
    pub fn clearRepresentationInputs(self: *Translator) void {
        self.emission.inputs.clearRetainingCapacity();
    }

    /// How many representation inputs are declared right now. A caller opening
    /// a scoped declaration reads this before declaring and hands the value to
    /// `truncateRepresentationInputs` when its scope closes, so declarations
    /// nest with the request scopes that made them.
    pub fn representationInputCount(self: *const Translator) usize {
        return self.emission.inputs.items.len;
    }

    /// Retract every representation input declared after `count` was read: the
    /// scoped counterpart of `clearRepresentationInputs`, for declarations tied
    /// to a request scope's lifetime. A floor at or above the current count is
    /// a no-op: a deeper scope already retracted past it, so everything the
    /// floor covered is gone.
    pub fn truncateRepresentationInputs(self: *Translator, count: usize) void {
        if (count >= self.emission.inputs.items.len) return;
        self.emission.inputs.shrinkRetainingCapacity(count);
    }

    /// How many positions this translator has opened a representation slot at.
    /// A caller comparing one emitted root against another stage's type for the
    /// same position reads this before and after that root: a growth says the
    /// root's content includes representation this layer emitted rather than
    /// read out of the checked module.
    pub fn representationPositionsOpened(self: *const Translator) u64 {
        return self.emission.positions_opened;
    }

    /// Translate a concrete checked root with no active binder environment into
    /// its stored Monotype id: the stored twin of the shadow's ground
    /// logical-identity walk. `cursor` reads the root's own module.
    pub fn translateGroundRoot(
        self: *Translator,
        cursor: ModuleCursor,
        checked_ty: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        const owner_node = checked.checked_residual_disposition_module_body_owner;
        return try self.translateUnderEnvironment(cursor, null, owner_node, checked_ty, skip_reason);
    }

    /// Translate one checked root under an already-built binder environment
    /// (reunify.md section 9.2). The caller owns `binding_env` and the storage
    /// its bound values name; `scheme_owner_node` selects the residual
    /// dispositions that apply to this walk. A recursive root, and a root
    /// carrying a position whose runtime encoding the checked data does not
    /// dictate, rerun through the draft builder, exactly as the ground entry
    /// point does.
    pub fn translateUnderEnvironment(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        checked_ty: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        return self.eagerWalk(cursor, binding_env, scheme_owner_node, checked_ty, skip_reason) catch |err| switch (err) {
            error.Skip => switch (skip_reason.*) {
                .recursive_cycle, .engine_input_needed => try self.translateDraftRoot(
                    cursor,
                    binding_env,
                    scheme_owner_node,
                    checked_ty,
                    skip_reason,
                ),
                else => err,
            },
            else => return err,
        };
    }

    /// Run one acyclic (eager, child-first interning) walk. A recursive cycle
    /// leaves this walk through the cycle guard so the caller can translate the
    /// root through the recursive-group builder instead (reunify.md section 9.2).
    fn eagerWalk(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .build_store = self.store,
            .binding_env = binding_env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .recursion_slots = null,
            .slot_journal = null,
            .nominal_instances = null,
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(root);
    }

    /// Instantiate a scheme's root under a dense binding and captured binding
    /// (reunify.md section 9.1, 9.5), producing the stored root. The binding is
    /// ordered exactly like the scheme's binders and carries no inference
    /// variables. The result is memoized by the represented section 9.4 key.
    pub fn instantiateStoredScheme(
        self: *Translator,
        scheme_ident: SchemeIdent,
        cursor: ModuleCursor,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        binders: []const checked.CheckedTypeId,
        binding: []const BoundType,
        captured: []const BoundType,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        const key = self.representedDigest(scheme_ident, binding, captured);
        if (self.represented_memo.get(key)) |cached| return cached;

        const env = BindingEnvironment{
            .scheme = scheme_ident,
            .binders = binders,
            .bound = binding,
            .captured = captured,
            .parent = null,
        };

        const result = self.eagerWalk(cursor, &env, scheme_owner_node, root, skip_reason) catch |err| switch (err) {
            error.Skip => switch (skip_reason.*) {
                .recursive_cycle, .engine_input_needed => try self.translateDraftRoot(
                    cursor,
                    &env,
                    scheme_owner_node,
                    root,
                    skip_reason,
                ),
                else => return err,
            },
            else => return err,
        };
        try self.represented_memo.put(key, result);
        return result;
    }

    /// Translate a root the eager walk found recursive, or carrying a position
    /// whose runtime encoding the checked data does not dictate, as a DRAFT
    /// (reunify.md sections 9.1, 9.2, 10.6). Every compound node reserves its
    /// stored slot before its children are translated, so identity assignment for
    /// the whole region waits on its children: a back-reference resolves to the
    /// reserved slot, and a representation slot under the region seals before the
    /// region interns. Reserve-before-descend closes a cycle on the checked
    /// address it reached twice, which is one address among the several the
    /// checker may hold for one type, so the raw group can carry a member that
    /// repeats an ancestor's rooted graph. The interner is the structural equality
    /// authority (reunify.md sections 8.2, 8.3): the region is therefore built in
    /// an isolated scratch store and re-interned into the target bottom-up,
    /// children first, through the recursive-group builder, which registers each
    /// member's rooted identity and collapses the repeats. An active binder
    /// environment names ids in the target store, so its bound values move into
    /// the scratch first. A target store that does not deduplicate has no
    /// recursive-group builder to hand the region to, so it is built in place.
    fn translateDraftRoot(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        if (!self.store.internEnabled()) {
            return try self.reserveFillWalk(self.store, cursor, binding_env, scheme_owner_node, root, skip_reason);
        }
        var scratch = MonoType.Store.init(self.allocator);
        defer scratch.deinit();
        scratch.enableInterning();
        var moved = MovedEnvironment.init(self.allocator);
        defer moved.deinit();
        const scratch_env = try moved.move(self.store, self.target_names, &scratch, binding_env);
        // A declared representation input names ids in the target store, and this
        // region builds in the scratch, so its ids move there first for exactly
        // the reason the binding environment's do.
        var moved_inputs = MovedInputs.init(self.allocator);
        defer moved_inputs.deinit();
        self.emission.active_inputs = try moved_inputs.move(
            self.store,
            self.target_names,
            &scratch,
            self.emission.inputs.items,
        );
        defer self.emission.active_inputs = null;
        const scratch_root = try self.reserveFillWalk(&scratch, cursor, scratch_env, scheme_owner_node, root, skip_reason);
        return try MonoType.reintern(self.store, self.target_names, scratch.view(), scratch_root);
    }

    /// Build `root` and its draft region into `build_store` with
    /// reserve-before-descend cycle closure and the representation layer on, so a
    /// position whose runtime encoding the checked data does not dictate opens and
    /// seals a slot instead of leaving the subset. Names always intern into the
    /// target name store, so a scratch build shares row/tag/name ids with the
    /// target and re-interns cleanly.
    fn reserveFillWalk(
        self: *Translator,
        build_store: *MonoType.Store,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        var slots = std.AutoHashMap(ActiveNode, TypeId).init(self.allocator);
        defer slots.deinit();
        var journal = std.ArrayList(ActiveNode).empty;
        defer journal.deinit(self.allocator);
        var instances = NominalInstances.init(self.allocator);
        defer instances.deinit();
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .build_store = build_store,
            .binding_env = binding_env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .recursion_slots = &slots,
            .slot_journal = &journal,
            .nominal_instances = &instances,
            .skip_reason = skip_reason,
            .emitting_representation = true,
        };
        defer walk.active.deinit();
        census.bump("emission_drafts_built");
        return try walk.node(root);
    }

    /// The represented section 9.4 memo key: the qualified scheme plus the
    /// ordered bound and captured stored-type digests.
    fn representedDigest(
        self: *Translator,
        scheme: SchemeIdent,
        binding: []const BoundType,
        captured: []const BoundType,
    ) InstantiationDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(&scheme.module_bytes);
        hasher.update(std.mem.asBytes(&scheme.scheme));
        const bound_len: u32 = @intCast(binding.len);
        hasher.update(std.mem.asBytes(&bound_len));
        for (binding) |value| {
            const digest = self.store.typeDigest(self.target_names, value.stored);
            hasher.update(&digest.bytes);
        }
        const captured_len: u32 = @intCast(captured.len);
        hasher.update(std.mem.asBytes(&captured_len));
        for (captured) |value| {
            const digest = self.store.typeDigest(self.target_names, value.stored);
            hasher.update(&digest.bytes);
        }
        return hasher.finalResult();
    }

    // --- Name interning into the target (production) name store ---
    //
    // These reuse the exact paths `instNode` resolves names by today: a source
    // name id is resolved to text in the reading module's name store and
    // re-interned into the target name store, so a translated type's names are
    // identical to the graph's.

    fn internTypeName(self: *Translator, cursor: ModuleCursor, id: names.TypeNameId) WalkError!names.TypeNameId {
        return try self.target_names.internTypeName(cursor.source_names.typeNameText(id));
    }

    fn internModuleIdentity(self: *Translator, cursor: ModuleCursor, id: names.ModuleIdentityId) WalkError!names.ModuleIdentityId {
        return try self.target_names.internModuleIdentity(cursor.source_names.moduleIdentityBytes(id));
    }

    fn internRecordFieldName(self: *Translator, cursor: ModuleCursor, id: names.RecordFieldNameId) WalkError!names.RecordFieldNameId {
        return try self.target_names.internRecordFieldLabel(cursor.source_names.recordFieldLabelText(id));
    }

    fn internTagName(self: *Translator, cursor: ModuleCursor, id: names.TagNameId) WalkError!names.TagNameId {
        return try self.target_names.internTagLabel(cursor.source_names.tagLabelText(id));
    }

    fn typeDef(
        self: *Translator,
        cursor: ModuleCursor,
        origin_module: names.ModuleIdentityId,
        type_name: names.TypeNameId,
        source_decl: ?u32,
    ) WalkError!MonoType.TypeDef {
        return .{
            .module = try self.internModuleIdentity(cursor, origin_module),
            .type_name = try self.internTypeName(cursor, type_name),
            .source_decl = source_decl,
        };
    }
};

/// The bound values for one active instantiation's binders, linked lexically for
/// a nested scheme (reunify.md section 7.3). `parent` is the enclosing scheme's
/// environment; `captured` holds the values for the scheme's captured enclosing
/// binders in order.
pub const BindingEnvironment = struct {
    scheme: SchemeIdent,
    binders: []const checked.CheckedTypeId,
    bound: []const BoundType,
    captured: []const BoundType,
    parent: ?*const BindingEnvironment,

    fn binderIndex(self: BindingEnvironment, checked_ty: checked.CheckedTypeId) ?usize {
        for (self.binders, 0..) |binder, index| {
            if (binder == checked_ty) return index;
        }
        return null;
    }
};

/// One binding environment chain relocated into the scratch store a recursive
/// group is built in (reunify.md section 9.2). Every bound and captured value is
/// re-interned into that store, so a binder substitution during the scratch
/// build names a scratch id. Both buffers are sized exactly once, so the
/// relocated `parent` links and value slices stay valid for the whole build.
const MovedEnvironment = struct {
    allocator: Allocator,
    frames: std.ArrayList(BindingEnvironment),
    values: std.ArrayList(BoundType),

    fn init(allocator: Allocator) MovedEnvironment {
        return .{ .allocator = allocator, .frames = .empty, .values = .empty };
    }

    fn deinit(self: *MovedEnvironment) void {
        self.values.deinit(self.allocator);
        self.frames.deinit(self.allocator);
    }

    /// Relocate `env` and every environment it links to, returning the innermost
    /// relocated environment, or null when there is none.
    fn move(
        self: *MovedEnvironment,
        source: *const MonoType.Store,
        name_store: *const names.NameStore,
        scratch: *MonoType.Store,
        env: ?*const BindingEnvironment,
    ) Allocator.Error!?*const BindingEnvironment {
        var depth: usize = 0;
        var value_count: usize = 0;
        var cursor = env;
        while (cursor) |frame| : (cursor = frame.parent) {
            depth += 1;
            value_count += frame.bound.len + frame.captured.len;
        }
        if (depth == 0) return null;

        try self.frames.ensureTotalCapacityPrecise(self.allocator, depth);
        try self.values.ensureTotalCapacityPrecise(self.allocator, value_count);

        const chain = try self.allocator.alloc(*const BindingEnvironment, depth);
        defer self.allocator.free(chain);
        var index = depth;
        cursor = env;
        while (cursor) |frame| : (cursor = frame.parent) {
            index -= 1;
            chain[index] = frame;
        }

        for (chain) |frame| {
            const bound = try self.moveValues(source, name_store, scratch, frame.bound);
            const captured = try self.moveValues(source, name_store, scratch, frame.captured);
            const parent: ?*const BindingEnvironment = if (self.frames.items.len == 0)
                null
            else
                &self.frames.items[self.frames.items.len - 1];
            self.frames.appendAssumeCapacity(.{
                .scheme = frame.scheme,
                .binders = frame.binders,
                .bound = bound,
                .captured = captured,
                .parent = parent,
            });
        }
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn moveValues(
        self: *MovedEnvironment,
        source: *const MonoType.Store,
        name_store: *const names.NameStore,
        scratch: *MonoType.Store,
        values: []const BoundType,
    ) Allocator.Error![]const BoundType {
        const start = self.values.items.len;
        for (values) |value| {
            const moved = try MonoType.reintern(scratch, name_store, source.view(), value.stored);
            self.values.appendAssumeCapacity(BoundType.of(moved));
        }
        return self.values.items[start..];
    }
};

/// The declared representation inputs relocated into the store one draft region
/// is built in (reunify.md section 9.2). Every id an input names — its minted
/// component types and its generated backing — is re-interned into that store,
/// so the content emitted during the build names ids of the store it is built
/// in. The buffers are sized exactly once, so the relocated slices stay valid
/// for the whole build.
const MovedInputs = struct {
    allocator: Allocator,
    inputs: std.ArrayList(RepresentationInput),
    components: std.ArrayList(TypeId),

    fn init(allocator: Allocator) MovedInputs {
        return .{ .allocator = allocator, .inputs = .empty, .components = .empty };
    }

    fn deinit(self: *MovedInputs) void {
        self.components.deinit(self.allocator);
        self.inputs.deinit(self.allocator);
    }

    fn move(
        self: *MovedInputs,
        source: *const MonoType.Store,
        name_store: *const names.NameStore,
        scratch: *MonoType.Store,
        inputs: []const RepresentationInput,
    ) Allocator.Error![]const RepresentationInput {
        if (inputs.len == 0) return &.{};
        var component_count: usize = 0;
        for (inputs) |input| component_count += input.representation.components.len;
        try self.inputs.ensureTotalCapacityPrecise(self.allocator, inputs.len);
        try self.components.ensureTotalCapacityPrecise(self.allocator, component_count);

        for (inputs) |input| {
            const start = self.components.items.len;
            for (input.representation.components) |component| {
                const moved = try MonoType.reintern(scratch, name_store, source.view(), component);
                self.components.appendAssumeCapacity(moved);
            }
            var representation = input.representation;
            representation.components = self.components.items[start..];
            if (input.representation.backing) |backing| {
                representation.backing = .{
                    .ty = try MonoType.reintern(scratch, name_store, source.view(), backing.ty),
                    .use = backing.use,
                    .authority = backing.authority,
                };
            }
            self.inputs.appendAssumeCapacity(.{
                .position = input.position,
                .representation = representation,
            });
        }
        return self.inputs.items;
    }
};

/// One directed translation walk (reunify.md section 9.2). Carries the active
/// map for cycle detection, the reading cursor (which changes when descending a
/// backing declaration in another module), the optional binder environment for
/// substitution, and the scheme owner node for residual disposition lookup.
///
/// `build_store` is the store this walk emits into: the target for an eager
/// walk, and the target or an isolated scratch for a reserve-fill recursive
/// build (reunify.md section 9.2). Names always intern into `owner.target_names`.
/// When `recursion_slots` is non-null the walk is in reserve-before-descend mode:
/// every compound node reserves its stored slot and records it in the map before
/// its children are translated, so a back-reference closes the cycle onto the
/// reserved slot.
const Walk = struct {
    owner: *Translator,
    cursor: ModuleCursor,
    build_store: *MonoType.Store,
    binding_env: ?*const BindingEnvironment,
    scheme_owner_node: u32,
    active: std.AutoHashMap(ActiveNode, void),
    recursion_slots: ?*std.AutoHashMap(ActiveNode, TypeId),
    /// Reserve-order journal of every key added to `recursion_slots`, so a
    /// binder frame can retract the entries its backing descent recorded when
    /// it pops (`nominalBacking`). A declaration holds ONE checked backing
    /// root and every instance walks that root under its own binder frame, so
    /// an entry recorded under one instance's substitution must not resolve
    /// another instance's walk of the same checked address (reunify.md
    /// sections 8.3, 9.2). Entries recorded outside any frame — the reserved
    /// nominal positions themselves — stay for the whole walk, which is what
    /// closes a recursive minted backing onto its own position. Null in eager
    /// mode, like `recursion_slots`.
    slot_journal: ?*std.ArrayList(ActiveNode),
    /// The nominal instances this reserve-fill walk already reserved a slot for,
    /// keyed by declaration and translated arguments. Null in eager mode.
    nominal_instances: ?*NominalInstances,
    skip_reason: *SkipReason,
    /// Set when a reserve-fill node left the subset. The recursive-group builder
    /// (`Store.addRecursive`) cannot carry `error.Skip` out of its fill callback,
    /// so the skip is recorded here and re-raised once the reserved slot returns.
    /// `skip_reason` already holds the recorded reason.
    reserve_fill_skipped: bool = false,
    /// Whether this walk emits through the section 10 representation layer. An
    /// eager walk does not: it leaves the subset at the first position whose
    /// runtime encoding the checked data does not dictate, so the caller reruns
    /// the root as a draft with this set (`translateDraftRoot`), where such a
    /// position opens a representation slot, seals it, and emits.
    emitting_representation: bool = false,

    fn skip(self: *Walk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
    }

    /// Record one reserved slot and journal the key, so the binder frame this
    /// reservation was made under can retract it when the frame pops.
    fn recordSlot(self: *Walk, key: ActiveNode, reserved: TypeId) Allocator.Error!void {
        try self.recursion_slots.?.put(key, reserved);
        try self.slot_journal.?.append(self.owner.allocator, key);
    }

    /// Retract every slot recorded after `mark`, restoring the map to the
    /// state the enclosing frame saw. Journal and map move together, so the
    /// retraction is exact.
    fn retractSlots(self: *Walk, mark: usize) void {
        const journal = self.slot_journal orelse return;
        while (journal.items.len > mark) {
            const key = journal.pop().?;
            _ = self.recursion_slots.?.remove(key);
        }
    }

    fn activeKey(self: *Walk, checked_ty: checked.CheckedTypeId) ActiveNode {
        return .{ .module_bytes = self.cursor.module_bytes, .type_id = @intFromEnum(checked_ty) };
    }

    /// The bound stored id of a binder visible in the active environment or any
    /// lexically enclosing one, or null when the checked type is not a bound
    /// binder (reunify.md section 7.3 links environments through `parent`).
    fn envBinder(self: *Walk, checked_ty: checked.CheckedTypeId) ?TypeId {
        var env = self.binding_env;
        while (env) |e| : (env = e.parent) {
            if (e.binderIndex(checked_ty)) |index| return e.bound[index].stored;
        }
        return null;
    }

    fn node(self: *Walk, checked_ty: checked.CheckedTypeId) WalkError!TypeId {
        if (self.recursion_slots != null) return try self.nodeReserveFill(checked_ty);

        // A binder owned by the active scheme (or a lexically enclosing one)
        // substitutes its bound stored id (reunify.md section 9.2), checked
        // before the cycle guard so a binder never registers as a cyclic node.
        if (self.envBinder(checked_ty)) |bound| return bound;

        const key = self.activeKey(checked_ty);
        if (self.active.contains(key)) return self.skip(.recursive_cycle);
        try self.active.put(key, {});
        defer _ = self.active.remove(key);

        return try self.payload(checked_ty, self.cursor.view.payload(checked_ty));
    }

    /// Reserve-before-descend translation of one node (reunify.md section 9.2,
    /// 10.6). Leaf and transparent-alias nodes need no reserved slot; a compound
    /// node reserves its stored slot, records it so a back-reference resolves,
    /// then fills it with content whose children were translated in the same
    /// mode. The finished component is a valid rooted cyclic stored graph.
    fn nodeReserveFill(self: *Walk, checked_ty: checked.CheckedTypeId) WalkError!TypeId {
        if (self.envBinder(checked_ty)) |bound| return bound;

        const key = self.activeKey(checked_ty);
        if (self.recursion_slots.?.get(key)) |reserved| return reserved;

        const p = self.cursor.view.payload(checked_ty);
        switch (p) {
            .pending, .err => return self.skip(.pending_or_err),
            .flex, .rigid => |v| return try self.variable(checked_ty, v),
            .empty_record => return try self.build_store.internRecord(self.owner.target_names, &.{}),
            .empty_tag_union => return try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            // A transparent alias erases to its backing, so it holds no stored
            // slot of its own; the cycle closes on the reserved node its backing
            // reaches. The active guard turns a degenerate alias-only cycle into a
            // recorded skip instead of a nonterminating descent.
            .alias => |alias_ty| {
                if (self.active.contains(key)) return self.skip(.recursive_cycle);
                try self.active.put(key, {});
                defer _ = self.active.remove(key);
                return try self.alias(checked_ty, alias_ty);
            },
            // A declaration-backed nominal reserves its slot under its instance
            // identity rather than its checked address, so the backing closes its
            // knot on the nominal itself (`nominalReserveFill`).
            .nominal => |nominal_ty| switch (builtinDisposition(nominal_ty)) {
                // A position whose runtime encoding the checked data does not
                // dictate reserves under its instance identity too, so a minted
                // backing that refers back to the position closes on the position
                // itself (the issue-10170 recursive `rest` shape).
                .named, .open_representation => {
                    if (self.owner.resolver.nominalBacking(self.cursor, nominal_ty)) |source| {
                        return try self.nominalReserveFill(checked_ty, nominal_ty, source);
                    }
                },
                else => {},
            },
            else => {},
        }

        const Ctx = struct {
            walk: *Walk,
            checked_ty: checked.CheckedTypeId,
            key: ActiveNode,
            p: checked.CheckedTypePayload,

            fn fill(ctx: @This(), reserved: TypeId) Allocator.Error!MonoType.Content {
                try ctx.walk.recordSlot(ctx.key, reserved);
                return ctx.walk.payloadContent(ctx.checked_ty, ctx.p) catch |err| switch (err) {
                    // `skip_reason` is already recorded; signal the skip through
                    // the walk so `nodeReserveFill` re-raises it after the slot is
                    // returned (the group builder only carries allocation errors).
                    error.Skip => {
                        ctx.walk.reserve_fill_skipped = true;
                        return .zst;
                    },
                    else => |other| return other,
                };
            }
        };
        const built = try self.build_store.addRecursive(Ctx{
            .walk = self,
            .checked_ty = checked_ty,
            .key = key,
            .p = p,
        }, Ctx.fill);
        if (self.reserve_fill_skipped) return error.Skip;
        return built;
    }

    /// Reserve-before-descend translation of one declaration-backed nominal,
    /// reserving its slot under its instance identity — the declaration plus its
    /// translated arguments (`NominalInstance`) — rather than under the checked
    /// address of this occurrence. Every occurrence of one nominal instance
    /// inside the walk therefore resolves to one slot, so a recursive backing
    /// closes its knot on the nominal and the group is the rooted graph the
    /// nominal denotes (reunify.md sections 8.3, 9.4).
    ///
    /// Arguments translate before the slot is reserved, because they are part of
    /// the identity it is reserved under. A checked graph cannot reach a nominal
    /// instance from inside its own arguments — that is an infinite type in
    /// argument position, which checking never builds — and the active guard
    /// records such a walk as a cycle rather than descending forever.
    fn nominalReserveFill(
        self: *Walk,
        checked_ty: checked.CheckedTypeId,
        n: checked.CheckedNominalType,
        source: Resolver.NominalBacking,
    ) WalkError!TypeId {
        const address = self.activeKey(checked_ty);
        if (self.active.contains(address)) return self.skip(.recursive_cycle);
        try self.active.put(address, {});
        defer _ = self.active.remove(address);

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const instances = self.nominal_instances.?;
        if (instances.find(source.cursor.module_bytes, source.declaration, args.items)) |reserved| return reserved;

        const Ctx = struct {
            walk: *Walk,
            checked_ty: checked.CheckedTypeId,
            address: ActiveNode,
            n: checked.CheckedNominalType,
            source: Resolver.NominalBacking,
            args: []const TypeId,

            fn fill(ctx: @This(), reserved: TypeId) Allocator.Error!MonoType.Content {
                try ctx.walk.recordSlot(ctx.address, reserved);
                try ctx.walk.nominal_instances.?.record(
                    ctx.source.cursor.module_bytes,
                    ctx.source.declaration,
                    ctx.args,
                    reserved,
                );
                return ctx.walk.namedContent(ctx.checked_ty, ctx.n, ctx.args) catch |err| switch (err) {
                    error.Skip => {
                        ctx.walk.reserve_fill_skipped = true;
                        return .zst;
                    },
                    else => |other| return other,
                };
            }
        };
        const built = try self.build_store.addRecursive(Ctx{
            .walk = self,
            .checked_ty = checked_ty,
            .address = address,
            .n = n,
            .source = source,
            .args = args.items,
        }, Ctx.fill);
        if (self.reserve_fill_skipped) return error.Skip;
        return built;
    }

    /// Assemble the stored content of one reserved compound node (reunify.md
    /// section 9.2). The children were translated through `node` in reserve-fill
    /// mode, so a back-reference already resolved to a reserved sibling slot.
    fn payloadContent(self: *Walk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!MonoType.Content {
        return switch (p) {
            .record_unbound => |fields| .{ .record = try self.recordSpan(fields, null) },
            .record => |record| .{ .record = try self.recordSpan(record.fields, record.ext) },
            .tuple => |items| .{ .tuple = try self.tupleSpan(items) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagSpan(tag_union.tags, tag_union.ext) },
            .function => |fn_ty| try self.functionContent(fn_ty),
            .nominal => |nominal_ty| try self.nominalContent(checked_ty, nominal_ty),
            // Leaves and aliases never reach a reserved slot (nodeReserveFill
            // builds them directly), so no other payload assembles content here.
            .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union, .alias => unreachable,
        };
    }

    fn payload(self: *Walk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!TypeId {
        return switch (p) {
            .pending, .err => self.skip(.pending_or_err),
            .flex, .rigid => |v| try self.variable(checked_ty, v),
            .empty_record => try self.build_store.internRecord(self.owner.target_names, &.{}),
            .empty_tag_union => try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            .record_unbound => |fields| try self.recordFrom(fields, null),
            .record => |record| try self.recordFrom(record.fields, record.ext),
            .tuple => |items| try self.tupleFrom(items),
            .tag_union => |tag_union| try self.tagUnionFrom(tag_union.tags, tag_union.ext),
            .function => |fn_ty| try self.function(fn_ty),
            .alias => |alias_ty| try self.alias(checked_ty, alias_ty),
            .nominal => |nominal_ty| try self.nominal(checked_ty, nominal_ty),
        };
    }

    /// The disposition this walk's body context reads for `checked_ty`
    /// (reunify.md section 7.4). Dispositions are scoped by
    /// `(scheme owner, CheckedTypeId)`: the entry under this walk's own scheme
    /// owner is the more specific statement and wins, and the module-body entry
    /// — which the checked side records for a residual belonging to no scheme's
    /// type, and therefore holding in every body of the module — is read when
    /// the scheme owner records none.
    fn dispositionFor(self: *Walk, checked_ty: checked.CheckedTypeId) ?checked.CheckedResidualDisposition {
        var module_wide: ?checked.CheckedResidualDisposition = null;
        for (self.cursor.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(checked_ty)) continue;
            if (disposition.scheme_owner_node == self.scheme_owner_node) return disposition;
            if (disposition.scheme_owner_node == checked.checked_residual_disposition_module_body_owner) {
                module_wide = disposition;
            }
        }
        return module_wide;
    }

    /// A residual variable: consult its recorded disposition (reunify.md section
    /// 7.4), then apply the checked default. This matches `materializeUnresolved`
    /// exactly: a numeric default yields the defaulted primitive, a row default
    /// yields the empty record or empty tag union, and an undisposed,
    /// undefaulted residual yields the empty tag union — the same stored shape
    /// the graph materializes for an unresolved variable today.
    fn variable(self: *Walk, checked_ty: checked.CheckedTypeId, v: checked.CheckedTypeVariable) WalkError!TypeId {
        if (self.dispositionFor(checked_ty)) |disposition| {
            switch (disposition.kind) {
                .uninhabited => return try self.build_store.internTagUnion(self.owner.target_names, &.{}),
                .contextual => {
                    if (disposition.contextualTarget()) |target| return try self.node(target);
                },
            }
        }

        if (v.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                return self.skip(.numeric_default_unresolved);
            return switch (target) {
                .dec => try self.build_store.internPrimitive(self.owner.target_names, .dec),
                .str => try self.build_store.internPrimitive(self.owner.target_names, .str),
            };
        }
        if (v.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => try self.build_store.internRecord(self.owner.target_names, &.{}),
                .empty_tag_union => try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            };
        }
        return self.skip(.undisposed_residual);
    }

    fn function(self: *Walk, fn_ty: checked.CheckedFunctionType) WalkError!TypeId {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.build_store.internFunc(self.owner.target_names, args.items, ret);
    }

    fn tupleFrom(self: *Walk, items: []const checked.CheckedTypeId) WalkError!TypeId {
        var lowered = std.ArrayList(TypeId).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.build_store.internTuple(self.owner.target_names, lowered.items);
    }

    /// Collect a record's fields, flattening its extension row exactly as
    /// production record lowering does (walk aliases, an empty-record default,
    /// and nested record rows). A row-extension binder substitutes its bound
    /// stored record, whose fields splice into this row. Shared by the eager and
    /// reserve-fill record builders.
    fn collectRecordFields(
        self: *Walk,
        out: *std.ArrayList(MonoType.Field),
        head: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) WalkError!void {
        try self.appendRecordFields(out, head);

        const ext_start = ext orelse return;
        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext_start;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            if (self.envBinder(current)) |bound| {
                try self.spliceStoredRecord(out, bound);
                break;
            }
            switch (self.cursor.view.payload(current)) {
                .alias => |a| current = a.backing,
                .empty_record => break,
                .flex, .rigid => |v| {
                    if (v.row_default == .empty_record) break;
                    return self.skip(.open_row);
                },
                .record_unbound => |tail| {
                    try self.appendRecordFields(out, tail);
                    break;
                },
                .record => |record| {
                    try self.appendRecordFields(out, record.fields);
                    current = record.ext;
                },
                else => return self.skip(.open_row),
            }
        }
    }

    fn recordFrom(self: *Walk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!TypeId {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        try self.collectRecordFields(&fields, head, ext);
        return try self.build_store.internRecord(self.owner.target_names, fields.items);
    }

    /// Reserve-fill record content: the same flattened fields as `recordFrom`,
    /// added to the build store as a field span rather than interned as a root.
    fn recordSpan(self: *Walk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!MonoType.Span {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        try self.collectRecordFields(&fields, head, ext);
        return try self.build_store.addRecordFields(self.owner.target_names, fields.items);
    }

    fn appendRecordFields(self: *Walk, out: *std.ArrayList(MonoType.Field), fields: []const checked.CheckedRecordField) WalkError!void {
        for (fields) |field| {
            const label = try self.owner.internRecordFieldName(self.cursor, field.name);
            const ty = try self.node(field.ty);
            try out.append(self.owner.allocator, .{ .name = label, .ty = ty });
        }
    }

    /// Splice the fields of an already-stored record (the value bound to a
    /// record-extension binder) into `out`. A stored record node closes the row;
    /// any other head leaves the row genuinely open, outside the subset.
    fn spliceStoredRecord(self: *Walk, out: *std.ArrayList(MonoType.Field), id: TypeId) WalkError!void {
        switch (self.build_store.get(id)) {
            .record => |span| {
                const field_span = self.build_store.fieldSpan(span);
                for (0..collections.GuardedList.borrowLen(field_span)) |i| {
                    try out.append(self.owner.allocator, collections.GuardedList.at(field_span, i));
                }
            },
            else => return self.skip(.open_row),
        }
    }

    /// Collect a tag union's tags, flattening its extension row exactly as
    /// production tag-union lowering does. Shared by the eager and reserve-fill
    /// tag-union builders. The caller owns the returned inputs and frees them
    /// through `freeTagInputs`.
    fn collectTags(
        self: *Walk,
        out: *std.ArrayList(MonoType.Store.TagInput),
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) WalkError!void {
        try self.appendTags(out, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            if (self.envBinder(current)) |bound| {
                try self.spliceStoredTags(out, bound);
                break;
            }
            switch (self.cursor.view.payload(current)) {
                .alias => |a| current = a.backing,
                .empty_tag_union => break,
                .flex, .rigid => |v| {
                    if (v.row_default == .empty_tag_union) break;
                    return self.skip(.open_row);
                },
                .tag_union => |tag_union| {
                    try self.appendTags(out, tag_union.tags);
                    current = tag_union.ext;
                },
                else => return self.skip(.open_row),
            }
        }
    }

    fn tagUnionFrom(self: *Walk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!TypeId {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);
        try self.collectTags(&tags, head, ext);
        return try self.build_store.internTagUnion(self.owner.target_names, tags.items);
    }

    /// Reserve-fill tag-union content: the same flattened tags as `tagUnionFrom`,
    /// added to the build store as a tag span rather than interned as a root.
    fn tagSpan(self: *Walk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!MonoType.Span {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);
        try self.collectTags(&tags, head, ext);

        var variants = std.ArrayList(MonoType.Tag).empty;
        defer variants.deinit(self.owner.allocator);
        for (tags.items) |tag| {
            try variants.append(self.owner.allocator, .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.build_store.addSpan(tag.payloads),
            });
        }
        return try self.build_store.addTagVariants(self.owner.target_names, variants.items);
    }

    fn appendTags(self: *Walk, out: *std.ArrayList(MonoType.Store.TagInput), tags: []const checked.CheckedTag) WalkError!void {
        for (tags) |tag| {
            const label = try self.owner.internTagName(self.cursor, tag.name);
            var payloads = std.ArrayList(TypeId).empty;
            errdefer payloads.deinit(self.owner.allocator);
            for (tag.argsSlice(self.cursor.view)) |arg| {
                try payloads.append(self.owner.allocator, try self.node(arg));
            }
            try out.append(self.owner.allocator, .{
                .name = label,
                .checked_name = label,
                .payloads = try payloads.toOwnedSlice(self.owner.allocator),
            });
        }
    }

    /// Splice the tags of an already-stored tag union (the value bound to a
    /// row-extension binder) into `out`.
    fn spliceStoredTags(self: *Walk, out: *std.ArrayList(MonoType.Store.TagInput), id: TypeId) WalkError!void {
        switch (self.build_store.get(id)) {
            .tag_union => |span| {
                const tag_span = self.build_store.tagSpan(span);
                for (0..collections.GuardedList.borrowLen(tag_span)) |i| {
                    const tag = collections.GuardedList.at(tag_span, i);
                    const payload_span = self.build_store.span(tag.payloads);
                    var payloads = std.ArrayList(TypeId).empty;
                    errdefer payloads.deinit(self.owner.allocator);
                    for (0..collections.GuardedList.borrowLen(payload_span)) |j| {
                        try payloads.append(self.owner.allocator, collections.GuardedList.at(payload_span, j));
                    }
                    try out.append(self.owner.allocator, .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try payloads.toOwnedSlice(self.owner.allocator),
                    });
                }
            },
            else => return self.skip(.open_row),
        }
    }

    fn freeTagInputs(self: *Walk, tags: *std.ArrayList(MonoType.Store.TagInput)) void {
        for (tags.items) |tag| self.owner.allocator.free(tag.payloads);
        tags.deinit(self.owner.allocator);
    }

    /// A source alias. Its stored form is the backing type: the store's
    /// `internNamed` constructor erases a storage-transparent alias (backed, with
    /// no builtin dispatch owner) to its backing exactly as production
    /// materializes it (reunify.md section 8.2). Building the full named alias
    /// mirrors `instNode`, and the constructor performs the erasure.
    fn alias(self: *Walk, checked_ty: checked.CheckedTypeId, alias_ty: checked.CheckedAliasType) WalkError!TypeId {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (alias_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const backing = try self.node(alias_ty.backing);
        return try self.build_store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = alias_ty.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, alias_ty.origin_module, alias_ty.name, alias_ty.source_decl),
            .kind = .alias,
            .builtin_owner = null,
            .args = args.items,
            .backing = .{ .ty = backing, .use = .inspectable },
        });
    }

    /// How a nominal's builtin runtime encoding lowers before the general named
    /// build (reunify.md section 9.2). A primitive/list/box encoding lowers to that
    /// structural shape; a generated opaque-evidence or iterator encoding carries
    /// runtime-encoding content the checked data does not dictate, so it is
    /// `open_representation` and emits through the section 10 layer; every other
    /// encoding keeps declaration identity as a named node.
    const BuiltinDisposition = union(enum) {
        primitive: MonoType.Primitive,
        list,
        box,
        named,
        open_representation,
    };

    fn builtinDisposition(n: checked.CheckedNominalType) BuiltinDisposition {
        return switch (n.representation) {
            .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                .primitive => |value| .{ .primitive = value },
                .list => .list,
                .box => .box,
                // Generated opaque-evidence nominals: checking owns the declared
                // identity, and the producer owns the generated owner and the
                // backing it mints (reunify.md sections 10.1, 10.3). The crypto
                // digest/hasher nominals are excluded: they carry a fixed
                // declaration backing and no producer-owned encoding, so they are
                // translated like any other nominal. An iterator nominal is the
                // same shape of case as the evidence owners: its tier, producer
                // kind, and mint depth are producer decisions, not checked data.
                .parse_tag_union_spec,
                .fields,
                .field,
                .iterator,
                => .open_representation,
                .bool_tag_union,
                .try_nominal,
                .dict,
                .set,
                .crypto_sha256_digest,
                .crypto_sha256_hasher,
                .crypto_blake3_digest,
                .crypto_blake3_hasher,
                => .named,
            },
            else => .named,
        };
    }

    /// The backing a sealed position emits: the producer-placed one when the
    /// relation kept it, else the declaration's own instantiation. A position
    /// sealed at a producer-owned tier owns its runtime encoding, and the
    /// graph marks exactly that ownership on such a backing, so the emitted
    /// backing carries the same authority (reunify.md sections 10.1, 10.3).
    fn sealedBacking(sealed: SealedPosition, declaration: ?MonoType.NamedBacking) ?MonoType.NamedBacking {
        if (sealed.backing) |placed| return placed;
        var backing = declaration orelse return null;
        switch (sealed.def.iterator_representation) {
            .minted, .forced_dynamic => backing.authority = .generated_private,
            .none => {},
        }
        return backing;
    }

    /// A nominal or opaque. Builtin nominals whose runtime encoding is a
    /// primitive, list, or box lower to that structural shape, matching
    /// production; the rest keep declaration identity as a stored named node with
    /// its backing, dispatch owner, and declared field order. A position whose
    /// runtime encoding the checked data does not dictate emits its definition
    /// through the section 10 representation layer (`namedDef`).
    fn nominal(self: *Walk, checked_ty: checked.CheckedTypeId, n: checked.CheckedNominalType) WalkError!TypeId {
        switch (builtinDisposition(n)) {
            .primitive => |value| return try self.build_store.internPrimitive(self.owner.target_names, value),
            .list => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return try self.build_store.internList(self.owner.target_names, try self.node(n.args[0]));
            },
            .box => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return try self.build_store.internBox(self.owner.target_names, try self.node(n.args[0]));
            },
            .open_representation => if (!self.emitting_representation) return self.skip(.engine_input_needed),
            .named => {},
        }

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const backing = try self.nominalBacking(n, args.items);
        const declared_order = try self.declaredOrder(n);
        defer self.owner.allocator.free(declared_order);

        const sealed = try self.sealedNamed(checked_ty, n, args.items, backing);
        try args.appendSlice(self.owner.allocator, sealed.components);

        return try self.build_store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = n.owner_module.bytes }, .ty = checked_ty },
            .def = sealed.def,
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
            .args = args.items,
            .backing = sealedBacking(sealed, backing),
            .declared_order = declared_order,
        });
    }

    /// What one nominal position emits. An ordinary nominal emits exactly what
    /// its declaration states. A position whose runtime encoding the checked data
    /// does not dictate opens a representation slot over that declared
    /// definition, runs the section 10.3 relation to its fixpoint against
    /// whatever representation the caller declared the producer placed there, and
    /// emits the sealed definition together with the producer-placed components
    /// and backing that go with it (reunify.md sections 10.2, 10.6).
    fn sealedNamed(
        self: *Walk,
        checked_ty: checked.CheckedTypeId,
        n: checked.CheckedNominalType,
        args: []const TypeId,
        backing: ?MonoType.NamedBacking,
    ) WalkError!SealedPosition {
        const declared_def = try self.owner.typeDef(self.cursor, n.origin_module, n.name, n.source_decl);
        if (builtinDisposition(n) != .open_representation) return .{ .def = declared_def };

        const declared: policy.NamedDescriptor = .{
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .def = declared_def,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
        };
        return try self.owner.emission.sealPosition(
            self.build_store,
            self.owner.target_names,
            .{ .module_bytes = self.cursor.module_bytes, .type_id = @intFromEnum(checked_ty) },
            declared,
            args,
            if (backing) |present| present.ty else null,
        );
    }

    // --- Reserve-fill content assembly (reunify.md section 9.2, 10.6) ---
    //
    // These build the stored content of one reserved compound node: children were
    // translated through `node` in reserve-fill mode, so back-references already
    // resolved to reserved sibling slots. Each mirrors the eager builder of the
    // same shape but returns `Content` for the reserved slot rather than interning
    // a fresh root.

    fn functionContent(self: *Walk, fn_ty: checked.CheckedFunctionType) WalkError!MonoType.Content {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return .{ .func = .{ .args = try self.build_store.addSpan(args.items), .ret = ret } };
    }

    fn tupleSpan(self: *Walk, items: []const checked.CheckedTypeId) WalkError!MonoType.Span {
        var lowered = std.ArrayList(TypeId).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.build_store.addSpan(lowered.items);
    }

    /// Reserve-fill named/nominal content. A builtin primitive/list/box encoding
    /// still reserved its slot, so it fills that slot with the leaf shape; a
    /// position whose runtime encoding the checked data does not dictate fills its
    /// slot with the named content its sealed representation states.
    fn nominalContent(self: *Walk, checked_ty: checked.CheckedTypeId, n: checked.CheckedNominalType) WalkError!MonoType.Content {
        switch (builtinDisposition(n)) {
            .primitive => |value| return .{ .primitive = value },
            .list => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return .{ .list = try self.node(n.args[0]) };
            },
            .box => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return .{ .box = try self.node(n.args[0]) };
            },
            .open_representation => if (!self.emitting_representation) return self.skip(.engine_input_needed),
            .named => {},
        }

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        return try self.namedContent(checked_ty, n, args.items);
    }

    /// The stored named content of one nominal whose arguments are already
    /// translated, for a slot reserved before the descent (reunify.md section
    /// 9.2). Its definition comes from `namedDef`, so a position whose runtime
    /// encoding the checked data does not dictate fills the reserved slot with its
    /// sealed representation.
    fn namedContent(
        self: *Walk,
        checked_ty: checked.CheckedTypeId,
        n: checked.CheckedNominalType,
        args: []const TypeId,
    ) WalkError!MonoType.Content {
        const backing = try self.nominalBacking(n, args);
        const declared_order = try self.declaredOrder(n);
        defer self.owner.allocator.free(declared_order);

        const sealed = try self.sealedNamed(checked_ty, n, args, backing);
        var emitted_args = std.ArrayList(TypeId).empty;
        defer emitted_args.deinit(self.owner.allocator);
        try emitted_args.appendSlice(self.owner.allocator, args);
        try emitted_args.appendSlice(self.owner.allocator, sealed.components);

        return .{ .named = .{
            .named_type = .{ .module = .{ .bytes = n.owner_module.bytes }, .ty = checked_ty },
            .def = sealed.def,
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
            .args = try self.build_store.addSpan(emitted_args.items),
            .backing = sealedBacking(sealed, backing),
            .declared_order = try self.build_store.addDeclaredFields(declared_order),
        } };
    }

    /// Instantiate a nominal's backing by binding the declaration's formals to
    /// this instance's translated argument ids and translating the backing root
    /// in the declaration's module (reunify.md section 9.2). Recursive
    /// self-references leave the translatable subset through the cycle guard,
    /// matching the closed population the shadow compares.
    fn nominalBacking(self: *Walk, n: checked.CheckedNominalType, args: []const TypeId) WalkError!?MonoType.NamedBacking {
        const source = self.owner.resolver.nominalBacking(self.cursor, n) orelse {
            return switch (n.representation) {
                .opaque_without_backing => null,
                else => self.skip(.missing_backing),
            };
        };
        if (source.formal_args.len != args.len) return self.skip(.malformed_builtin_arity);

        var bound = std.ArrayList(BoundType).empty;
        defer bound.deinit(self.owner.allocator);
        for (args) |arg| try bound.append(self.owner.allocator, BoundType.of(arg));

        const frame = BindingEnvironment{
            .scheme = .{ .module_bytes = source.cursor.module_bytes, .scheme = 0 },
            .binders = source.formal_args,
            .bound = bound.items,
            .captured = &.{},
            .parent = self.binding_env,
        };

        const saved_cursor = self.cursor;
        const saved_env = self.binding_env;
        self.cursor = source.cursor;
        self.binding_env = &frame;
        // Slots reserved during this backing descent belong to this frame's
        // substitution: another instance of the same declaration walks the
        // same checked backing root under different bindings, and must not
        // resolve through these entries. The position's own slot was recorded
        // before this frame opened, so a recursive backing still closes onto
        // it (reunify.md sections 8.3, 9.2).
        const slot_mark: usize = if (self.slot_journal) |journal| journal.items.len else 0;
        defer {
            self.retractSlots(slot_mark);
            self.cursor = saved_cursor;
            self.binding_env = saved_env;
        }

        const backing_ty = try self.node(source.root);
        return .{
            .ty = backing_ty,
            .use = if (n.is_opaque) .runtime_layout_only else .inspectable,
        };
    }

    /// Build the declared field-order entries for a nominal record backing, in
    /// declared (not sorted) order (reunify.md, design.md "Nominal Record Field
    /// Order"). Named entries re-intern their label; padding entries translate
    /// the instance's substituted padding type. The caller owns the returned
    /// slice.
    fn declaredOrder(self: *Walk, n: checked.CheckedNominalType) WalkError![]const MonoType.DeclaredField {
        var sources = std.ArrayList(Resolver.DeclaredField).empty;
        defer sources.deinit(self.owner.allocator);
        const declared_cursor = try self.owner.resolver.declaredOrder(self.cursor, n, &sources) orelse
            return &.{};
        if (sources.items.len == 0) return &.{};

        const entries = try self.owner.allocator.alloc(MonoType.DeclaredField, sources.items.len);
        errdefer self.owner.allocator.free(entries);
        for (sources.items, 0..) |source, index| {
            entries[index] = switch (source) {
                .named => |label| .{ .named = try self.owner.internRecordFieldName(declared_cursor, label) },
                .padding => |checked_ty| .{ .padding = try self.declaredPadding(declared_cursor, checked_ty) },
            };
        }
        return entries;
    }

    /// Translate one padding type in the declared-order cursor without letting
    /// the row-flattening binder environment of the enclosing type leak into it:
    /// a padding type is a self-contained checked type in the declaration module.
    fn declaredPadding(self: *Walk, cursor: ModuleCursor, checked_ty: checked.CheckedTypeId) WalkError!TypeId {
        const saved_cursor = self.cursor;
        const saved_env = self.binding_env;
        self.cursor = cursor;
        self.binding_env = null;
        defer {
            self.cursor = saved_cursor;
            self.binding_env = saved_env;
        }
        return try self.node(checked_ty);
    }
};

// --- Tests ---

const testing = std.testing;

/// A minimal hand-built checked type store view plus its name store, so the
/// translation walks and instantiation can be tested without running the whole
/// pipeline.
const TestFixture = struct {
    allocator: Allocator,
    source_names: names.NameStore,
    payloads: std.ArrayList(checked.StoredCheckedTypePayload),
    type_id_pool: std.ArrayList(checked.CheckedTypeId),
    record_fields: std.ArrayList(checked.CheckedRecordField),
    tags: std.ArrayList(checked.CheckedTag),
    schemes: std.ArrayList(checked.CheckedTypeScheme),
    module_hash: [32]u8,

    fn init(allocator: Allocator) TestFixture {
        return .{
            .allocator = allocator,
            .source_names = names.NameStore.init(allocator),
            .payloads = std.ArrayList(checked.StoredCheckedTypePayload).empty,
            .type_id_pool = std.ArrayList(checked.CheckedTypeId).empty,
            .record_fields = std.ArrayList(checked.CheckedRecordField).empty,
            .tags = std.ArrayList(checked.CheckedTag).empty,
            .schemes = std.ArrayList(checked.CheckedTypeScheme).empty,
            .module_hash = [_]u8{7} ** 32,
        };
    }

    fn deinit(self: *TestFixture) void {
        self.schemes.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.record_fields.deinit(self.allocator);
        self.type_id_pool.deinit(self.allocator);
        self.payloads.deinit(self.allocator);
        self.source_names.deinit();
    }

    fn add(self: *TestFixture, payload: checked.StoredCheckedTypePayload) Allocator.Error!checked.CheckedTypeId {
        const id: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.payloads.items.len)));
        try self.payloads.append(self.allocator, payload);
        return id;
    }

    fn addPrimitiveNominal(self: *TestFixture, builtin_nominal: checked.CheckedBuiltinNominal, name_text: []const u8) Allocator.Error!checked.CheckedTypeId {
        const name = try self.source_names.internTypeName(name_text);
        const module = try self.source_names.internModuleIdentity(&self.module_hash);
        return try self.add(.{ .nominal = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = self.module_hash },
            .is_opaque = false,
            .representation = .{ .builtin = builtin_nominal },
        } });
    }

    fn addUserNominal(self: *TestFixture, name_text: []const u8, args: []const checked.CheckedTypeId) Allocator.Error!checked.CheckedTypeId {
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

    fn addBuiltinNominal(self: *TestFixture, builtin_nominal: checked.CheckedBuiltinNominal, name_text: []const u8) Allocator.Error!checked.CheckedTypeId {
        return try self.addPrimitiveNominal(builtin_nominal, name_text);
    }

    /// An opaque builtin nominal with type arguments — the shape every position
    /// whose runtime encoding the checked data does not dictate takes.
    fn addOpaqueBuiltinNominal(
        self: *TestFixture,
        builtin_nominal: checked.CheckedBuiltinNominal,
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
            .is_opaque = true,
            .representation = .{ .builtin = builtin_nominal },
            .args = .{ .start = start, .len = @intCast(args.len) },
        } });
    }

    /// A builtin `List elem` nominal, whose runtime encoding lowers to a stored
    /// list of the translated element.
    fn addUserBuiltinList(self: *TestFixture, elem: checked.CheckedTypeId) Allocator.Error!checked.CheckedTypeId {
        const name = try self.source_names.internTypeName("List");
        const module = try self.source_names.internModuleIdentity(&self.module_hash);
        const start: u32 = @intCast(self.type_id_pool.items.len);
        try self.type_id_pool.append(self.allocator, elem);
        return try self.add(.{ .nominal = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = self.module_hash },
            .is_opaque = false,
            .representation = .{ .builtin = .list },
            .args = .{ .start = start, .len = 1 },
        } });
    }

    /// One tag with its payload type ids appended into `type_id_pool`.
    const TagSpec = struct {
        name_text: []const u8,
        payloads: []const checked.CheckedTypeId,
    };

    /// Add a tag union with an empty closed extension. `tags` payloads may name
    /// any already-reserved id, including `self_id` to build a recursive knot.
    fn addTagUnion(self: *TestFixture, tags: []const TagSpec, ext: checked.CheckedTypeId) Allocator.Error!checked.CheckedTypeId {
        const tags_start: u32 = @intCast(self.tags.items.len);
        for (tags) |tag| {
            const name = try self.source_names.internTagLabel(tag.name_text);
            const args_start: u32 = @intCast(self.type_id_pool.items.len);
            try self.type_id_pool.appendSlice(self.allocator, tag.payloads);
            try self.tags.append(self.allocator, .{
                .name = name,
                .args_start = args_start,
                .args_len = @intCast(tag.payloads.len),
            });
        }
        return try self.add(.{ .tag_union = .{
            .tags = .{ .start = tags_start, .len = @intCast(tags.len) },
            .ext = ext,
        } });
    }

    /// The id the next `add` will assign, so a recursive payload can name the
    /// node it belongs to before the node itself is added.
    fn nextId(self: *TestFixture) checked.CheckedTypeId {
        return @enumFromInt(@as(u32, @intCast(self.payloads.items.len)));
    }

    fn view(self: *TestFixture) checked.CheckedTypeStoreView {
        return .{
            .stored_payloads = self.payloads.items,
            .type_id_pool = self.type_id_pool.items,
            .record_field_pool = self.record_fields.items,
            .tag_pool = self.tags.items,
            .schemes = self.schemes.items,
        };
    }

    fn cursor(self: *TestFixture) ModuleCursor {
        return .{
            .view = self.view(),
            .source_names = &self.source_names,
            .module_bytes = self.module_hash,
        };
    }
};

/// A trivial resolver for tests: every user nominal is opaque-without-backing,
/// so no backing or declared order is produced, and no dispatch owner is stamped.
const NoBackingResolver = struct {
    fn builtinOwner(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?static_dispatch.BuiltinOwner {
        return null;
    }
    fn nominalBacking(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?Resolver.NominalBacking {
        return null;
    }
    fn declaredOrder(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType, _: *std.ArrayList(Resolver.DeclaredField)) Allocator.Error!?ModuleCursor {
        return null;
    }

    const vtable = Resolver.VTable{
        .builtin_owner = builtinOwner,
        .nominal_backing = nominalBacking,
        .declared_order = declaredOrder,
    };

    fn resolver(self: *NoBackingResolver) Resolver {
        return .{ .context = self, .vtable = &vtable };
    }
};

/// A resolver that instantiates one record-backed nominal's declaration for
/// backing translation, so a nominal's stored form carries its backing record.
const RecordBackingResolver = struct {
    cursor: ModuleCursor,
    formal_args: []const checked.CheckedTypeId,
    backing_root: checked.CheckedTypeId,

    fn builtinOwner(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?static_dispatch.BuiltinOwner {
        return null;
    }
    fn nominalBacking(context: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?Resolver.NominalBacking {
        const self: *RecordBackingResolver = @ptrCast(@alignCast(context));
        return .{
            .cursor = self.cursor,
            .declaration = 0,
            .formal_args = self.formal_args,
            .root = self.backing_root,
        };
    }
    fn declaredOrder(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType, _: *std.ArrayList(Resolver.DeclaredField)) Allocator.Error!?ModuleCursor {
        return null;
    }

    const vtable = Resolver.VTable{
        .builtin_owner = builtinOwner,
        .nominal_backing = nominalBacking,
        .declared_order = declaredOrder,
    };

    fn resolver(self: *RecordBackingResolver) Resolver {
        return .{ .context = self, .vtable = &vtable };
    }
};

fn initTargetStore() MonoType.Store {
    return MonoType.Store.init(testing.allocator);
}

test "representation inputs retract to a scope's count" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();
    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();
    var resolver = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, resolver.resolver());
    defer translator.deinit();

    const outer = translator.representationInputCount();
    try translator.declareRepresentationInput(.{
        .position = .{ .module_bytes = fixture.module_hash, .type_id = 1 },
        .representation = .{ .iterator_representation = .minted },
    });
    const inner = translator.representationInputCount();
    try translator.declareRepresentationInput(.{
        .position = .{ .module_bytes = fixture.module_hash, .type_id = 2 },
        .representation = .{ .iterator_representation = .forced_dynamic },
    });
    try testing.expectEqual(@as(usize, 2), translator.representationInputCount());

    translator.truncateRepresentationInputs(inner);
    try testing.expectEqual(@as(usize, 1), translator.representationInputCount());
    translator.truncateRepresentationInputs(outer);
    try testing.expectEqual(@as(usize, 0), translator.representationInputCount());
}

test "primitive builtin nominals translate to the same stored primitive id" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const str_ty = try fixture.addPrimitiveNominal(.str, "Str");
    const u64_again = try fixture.addPrimitiveNominal(.u64, "U64");

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const a = try translator.translateGroundRoot(fixture.cursor(), u64_ty, &reason);
    const b = try translator.translateGroundRoot(fixture.cursor(), str_ty, &reason);
    const c = try translator.translateGroundRoot(fixture.cursor(), u64_again, &reason);

    try testing.expectEqual(a, c);
    try testing.expect(a != b);
}

test "records translate child-first and share a stored id by content" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const str_ty = try fixture.addPrimitiveNominal(.str, "Str");
    const x = try fixture.source_names.internRecordFieldLabel("x");
    const y = try fixture.source_names.internRecordFieldLabel("y");

    const start: u32 = @intCast(fixture.record_fields.items.len);
    try fixture.record_fields.append(testing.allocator, .{ .name = x, .ty = u64_ty });
    try fixture.record_fields.append(testing.allocator, .{ .name = y, .ty = str_ty });
    const empty = try fixture.add(.empty_record);
    const record_ty = try fixture.add(.{ .record = .{
        .fields = .{ .start = start, .len = 2 },
        .ext = empty,
    } });

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const first = try translator.translateGroundRoot(fixture.cursor(), record_ty, &reason);
    const second = try translator.translateGroundRoot(fixture.cursor(), record_ty, &reason);
    try testing.expectEqual(first, second);
    switch (store.get(first)) {
        .record => |span| try testing.expectEqual(@as(usize, 2), collections.GuardedList.borrowLen(store.fieldSpan(span))),
        else => try testing.expect(false),
    }
}

test "a self-referential record is built through the recursive-group builder" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // A record { self: <this record> }: the field type is the record's own id,
    // so the eager walk's cycle guard fires and the root is rebuilt through the
    // recursive-group builder into a closed self-recursive record.
    const empty = try fixture.add(.empty_record);
    const record_id: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(fixture.payloads.items.len)));
    const self_label = try fixture.source_names.internRecordFieldLabel("self");
    const start: u32 = @intCast(fixture.record_fields.items.len);
    try fixture.record_fields.append(testing.allocator, .{ .name = self_label, .ty = record_id });
    const record_ty = try fixture.add(.{ .record = .{
        .fields = .{ .start = start, .len = 1 },
        .ext = empty,
    } });
    try testing.expectEqual(record_id, record_ty);

    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const root = try translator.translateGroundRoot(fixture.cursor(), record_ty, &reason);
    switch (store.get(root)) {
        .record => |span| {
            const field_span = store.fieldSpan(span);
            try testing.expectEqual(@as(usize, 1), collections.GuardedList.borrowLen(field_span));
            try testing.expectEqual(root, collections.GuardedList.at(field_span, 0).ty);
        },
        else => try testing.expect(false),
    }
}

test "a residual without a disposition declines rather than emitting a type" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const flex = try fixture.add(.{ .flex = .{} });
    const empty = try fixture.add(.empty_tag_union);

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    // An undisposed residual names no type: emitting one would be
    // indistinguishable from a genuinely uninhabited position. The explicit
    // empty tag union still translates.
    var reason: SkipReason = undefined;
    try testing.expectError(error.Skip, translator.translateGroundRoot(fixture.cursor(), flex, &reason));
    try testing.expectEqual(SkipReason.undisposed_residual, reason);
    const from_empty = try translator.translateGroundRoot(fixture.cursor(), empty, &reason);
    try testing.expect(store.get(from_empty) == .tag_union);
}

test "a numeric-defaulted residual materializes as the stored default primitive" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const numeral = try fixture.add(.{ .flex = .{ .numeric_default_phase = .mono_specialization } });
    const dec_nominal = try fixture.addPrimitiveNominal(.dec, "Dec");

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const from_numeral = try translator.translateGroundRoot(fixture.cursor(), numeral, &reason);
    const from_dec = try translator.translateGroundRoot(fixture.cursor(), dec_nominal, &reason);
    try testing.expectEqual(from_dec, from_numeral);
}

test "instantiating a scheme root matches translating the instantiated root" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Scheme: Wrapper a, with binder `a` a rigid variable, root = Wrapper a.
    const binder = try fixture.add(.{ .rigid = .{} });
    const scheme_root = try fixture.addUserNominal("Wrapper", &.{binder});

    // Actual U64 and the instantiated root Wrapper U64.
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const instantiated_root = try fixture.addUserNominal("Wrapper", &.{u64_ty});

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const actual = try translator.translateGroundRoot(fixture.cursor(), u64_ty, &reason);
    const binding = [_]BoundType{BoundType.of(actual)};
    const binders = [_]checked.CheckedTypeId{binder};

    const instantiated = try translator.instantiateStoredScheme(
        .{ .module_bytes = fixture.module_hash, .scheme = 0 },
        fixture.cursor(),
        checked.checked_residual_disposition_module_body_owner,
        scheme_root,
        &binders,
        &binding,
        &.{},
        &reason,
    );
    const direct = try translator.translateGroundRoot(fixture.cursor(), instantiated_root, &reason);
    // Named ids are occurrence-held on production stores, so instantiation
    // and direct translation agree on content while each keeps its own id
    // until the holds lift (reunify.md section 8.5).
    try testing.expect(try store.typeEql(&target_names, direct, instantiated));
}

test "the represented instantiation memo returns the same id for the same binding" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const binder = try fixture.add(.{ .rigid = .{} });
    const scheme_root = try fixture.addUserNominal("Wrapper", &.{binder});
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const actual = try translator.translateGroundRoot(fixture.cursor(), u64_ty, &reason);
    const binding = [_]BoundType{BoundType.of(actual)};
    const binders = [_]checked.CheckedTypeId{binder};
    const ident = SchemeIdent{ .module_bytes = fixture.module_hash, .scheme = 0 };

    const first = try translator.instantiateStoredScheme(ident, fixture.cursor(), checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, &reason);
    const memo_count = translator.represented_memo.count();
    const second = try translator.instantiateStoredScheme(ident, fixture.cursor(), checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, &reason);

    try testing.expectEqual(first, second);
    try testing.expectEqual(@as(u32, 1), memo_count);
    try testing.expectEqual(memo_count, translator.represented_memo.count());
}

test "a nominal instance carries its declaration backing, matching the sealed record" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Declaration Wrapper a = { value: a }, formal binder `a`, backing root a
    // record { value: a }.
    const formal = try fixture.add(.{ .rigid = .{} });
    const value_label = try fixture.source_names.internRecordFieldLabel("value");
    const rf_start: u32 = @intCast(fixture.record_fields.items.len);
    try fixture.record_fields.append(testing.allocator, .{ .name = value_label, .ty = formal });
    const backing_empty = try fixture.add(.empty_record);
    const backing_root = try fixture.add(.{ .record = .{
        .fields = .{ .start = rf_start, .len = 1 },
        .ext = backing_empty,
    } });

    // Instance Wrapper U64.
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const instance = try fixture.addUserNominal("Wrapper", &.{u64_ty});

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var backing_resolver = RecordBackingResolver{
        .cursor = fixture.cursor(),
        .formal_args = try testing.allocator.dupe(checked.CheckedTypeId, &.{formal}),
        .backing_root = backing_root,
    };
    defer testing.allocator.free(backing_resolver.formal_args);
    var translator = Translator.init(testing.allocator, &store, &target_names, backing_resolver.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const instance_id = try translator.translateGroundRoot(fixture.cursor(), instance, &reason);

    // The backing record { value: U64 } is built independently and compared by
    // stored digest to the instance's backing.
    const expected_backing = expected: {
        const u64_id = try store.internPrimitive(&target_names, .u64);
        const label = try target_names.internRecordFieldLabel("value");
        break :expected try store.internRecord(&target_names, &.{.{ .name = label, .ty = u64_id }});
    };

    switch (store.get(instance_id)) {
        .named => |named| {
            const backing = named.backing orelse return testing.expect(false);
            const backing_digest = store.typeDigest(&target_names, backing.ty);
            const expected_digest = store.typeDigest(&target_names, expected_backing);
            try testing.expectEqualSlices(u8, &expected_digest.bytes, &backing_digest.bytes);
        },
        else => try testing.expect(false),
    }
}

test "a two-parameter declaration binds both formals through a nested backing" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Declaration Pair a b = { both: (a, b) }, so each formal is reached only
    // BELOW the backing's own root rather than at it. This is the shape the
    // corpus divergences take - a record over a tuple over the parameters -
    // and it is the walk, not a table, that relates a formal to its argument
    // (reunify.md 13.2c).
    const first = try fixture.add(.{ .rigid = .{} });
    const second = try fixture.add(.{ .rigid = .{} });
    const tuple_start: u32 = @intCast(fixture.type_id_pool.items.len);
    try fixture.type_id_pool.appendSlice(testing.allocator, &.{ first, second });
    const inner = try fixture.add(.{ .tuple = .{ .start = tuple_start, .len = 2 } });

    const both_label = try fixture.source_names.internRecordFieldLabel("both");
    const rf_start: u32 = @intCast(fixture.record_fields.items.len);
    try fixture.record_fields.append(testing.allocator, .{ .name = both_label, .ty = inner });
    const backing_empty = try fixture.add(.empty_record);
    const backing_root = try fixture.add(.{ .record = .{
        .fields = .{ .start = rf_start, .len = 1 },
        .ext = backing_empty,
    } });

    // Instance Pair U64 Str, whose two arguments differ so a swapped or shared
    // binding cannot pass.
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const str_ty = try fixture.addPrimitiveNominal(.str, "Str");
    const instance = try fixture.addUserNominal("Pair", &.{ u64_ty, str_ty });

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var backing_resolver = RecordBackingResolver{
        .cursor = fixture.cursor(),
        .formal_args = try testing.allocator.dupe(checked.CheckedTypeId, &.{ first, second }),
        .backing_root = backing_root,
    };
    defer testing.allocator.free(backing_resolver.formal_args);
    var translator = Translator.init(testing.allocator, &store, &target_names, backing_resolver.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const instance_id = try translator.translateGroundRoot(fixture.cursor(), instance, &reason);

    const expected_backing = expected: {
        const u64_id = try store.internPrimitive(&target_names, .u64);
        const str_id = try store.internPrimitive(&target_names, .str);
        const pair_id = try store.internTuple(&target_names, &.{ u64_id, str_id });
        const label = try target_names.internRecordFieldLabel("both");
        break :expected try store.internRecord(&target_names, &.{.{ .name = label, .ty = pair_id }});
    };

    switch (store.get(instance_id)) {
        .named => |named| {
            const backing = named.backing orelse return testing.expect(false);
            const backing_digest = store.typeDigest(&target_names, backing.ty);
            const expected_digest = store.typeDigest(&target_names, expected_backing);
            try testing.expectEqualSlices(u8, &expected_digest.bytes, &backing_digest.bytes);
        },
        else => try testing.expect(false),
    }
}

/// Assert a stored root is a self-recursive tag union: its single tag's payload
/// resolves back to the root id, so the cycle closed through a reserved slot.
/// True when `root` is a single-tag, single-payload tag union whose payload
/// resolves back to the root id: a self-recursive knot closed through a reserved
/// slot. Non-fallible so the assertion stays inside the test block.
fn isSelfRecursiveTagUnion(store: *MonoType.Store, root: TypeId) bool {
    switch (store.get(root)) {
        .tag_union => |span| {
            const tag_span = store.tagSpan(span);
            if (collections.GuardedList.borrowLen(tag_span) != 1) return false;
            const tag = collections.GuardedList.at(tag_span, 0);
            const payloads = store.span(tag.payloads);
            if (collections.GuardedList.borrowLen(payloads) != 1) return false;
            return collections.GuardedList.at(payloads, 0) == root;
        },
        else => return false,
    }
}

test "a self-recursive tag union is built through the recursive-group builder" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const self_id = fixture.nextId();
    const root = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{self_id} }}, empty);
    try testing.expectEqual(self_id, root);

    // Off: the recursive group is built reserve-fill in place. On: it is built
    // into a scratch store and re-interned. Either way the knot stays closed.
    inline for (.{ false, true }) |intern_on| {
        var store = initTargetStore();
        defer store.deinit();
        if (intern_on) store.enableInterning();
        var target_names = names.NameStore.init(testing.allocator);
        defer target_names.deinit();

        var no_backing = NoBackingResolver{};
        var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
        defer translator.deinit();

        var reason: SkipReason = undefined;
        const built = try translator.translateGroundRoot(fixture.cursor(), root, &reason);
        try testing.expect(isSelfRecursiveTagUnion(&store, built));
    }
}

test "two structurally equal recursive tag unions dedup with interning on, differ off" {
    // Build the same self-recursive tag union from two independent checked roots
    // and translate each with one translator, so the second reaches the first's
    // registered rooted group under interning.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const first_id = fixture.nextId();
    const first = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{first_id} }}, empty);
    const second_id = fixture.nextId();
    const second = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{second_id} }}, empty);

    inline for (.{ true, false }) |intern_on| {
        var store = initTargetStore();
        defer store.deinit();
        if (intern_on) store.enableInterning();
        var target_names = names.NameStore.init(testing.allocator);
        defer target_names.deinit();

        var no_backing = NoBackingResolver{};
        var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
        defer translator.deinit();

        var reason: SkipReason = undefined;
        const a = try translator.translateGroundRoot(fixture.cursor(), first, &reason);
        const b = try translator.translateGroundRoot(fixture.cursor(), second, &reason);
        try testing.expect(isSelfRecursiveTagUnion(&store, a));
        try testing.expect(isSelfRecursiveTagUnion(&store, b));
        if (intern_on) {
            try testing.expectEqual(a, b);
        } else {
            try testing.expect(a != b);
        }
    }
}

test "a mutually recursive tag-union pair builds a closed two-node group" {
    // A = [ToB B], B = [ToA A]: a two-node cycle with distinct heads.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const a_id = fixture.nextId();
    const b_id: checked.CheckedTypeId = @enumFromInt(@intFromEnum(a_id) + 1);
    const a = try fixture.addTagUnion(&.{.{ .name_text = "ToB", .payloads = &.{b_id} }}, empty);
    const b = try fixture.addTagUnion(&.{.{ .name_text = "ToA", .payloads = &.{a_id} }}, empty);
    try testing.expectEqual(a_id, a);
    try testing.expectEqual(b_id, b);

    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const a_root = try translator.translateGroundRoot(fixture.cursor(), a, &reason);

    // A's single tag payload is B; B's single tag payload is A's root — a closed
    // cycle back to the entered root.
    const b_root = payload_of: {
        switch (store.get(a_root)) {
            .tag_union => |span| {
                const tag_span = store.tagSpan(span);
                const tag = collections.GuardedList.at(tag_span, 0);
                break :payload_of collections.GuardedList.at(store.span(tag.payloads), 0);
            },
            else => return testing.expect(false),
        }
    };
    try testing.expect(a_root != b_root);
    switch (store.get(b_root)) {
        .tag_union => |span| {
            const tag_span = store.tagSpan(span);
            const tag = collections.GuardedList.at(tag_span, 0);
            try testing.expectEqual(a_root, collections.GuardedList.at(store.span(tag.payloads), 0));
        },
        else => try testing.expect(false),
    }
}

test "a recursive tag union nested under a list reproduces the graph shape" {
    // Rec = [Node (List Rec)]: the cycle passes through a builtin list nominal.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    // The list is added next, then the tag union, so the tag union's id is one
    // past the list's; the list's element names that future tag-union id.
    const rec_id: checked.CheckedTypeId = @enumFromInt(@intFromEnum(fixture.nextId()) + 1);
    const list_of_rec = try fixture.addUserBuiltinList(rec_id);
    const rec = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{list_of_rec} }}, empty);
    try testing.expectEqual(rec_id, rec);

    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const root = try translator.translateGroundRoot(fixture.cursor(), rec, &reason);
    switch (store.get(root)) {
        .tag_union => |span| {
            const tag = collections.GuardedList.at(store.tagSpan(span), 0);
            const list_id = collections.GuardedList.at(store.span(tag.payloads), 0);
            switch (store.get(list_id)) {
                .list => |elem| try testing.expectEqual(root, elem),
                else => try testing.expect(false),
            }
        },
        else => try testing.expect(false),
    }
}

test "a generated opaque-evidence position whose backing no module names is unemittable" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const field_ty = try fixture.addBuiltinNominal(.field, "FieldName");

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    // The eager walk leaves at the open position and the draft rerun reaches it
    // again, where no module names the declaration's backing at all — so the
    // position is unemittable for a reason that is not about representation.
    var reason: SkipReason = undefined;
    try testing.expectError(error.Skip, translator.translateGroundRoot(fixture.cursor(), field_ty, &reason));
    try testing.expectEqual(SkipReason.missing_backing, reason);
}

// --- Emission: representation slots, producer inputs, sealing ---

/// A resolver for an iterator-owned builtin nominal: it stamps the `.iter`
/// dispatch owner and instantiates one declaration backing, which is what an
/// open representation position needs to emit.
const IteratorResolver = struct {
    cursor: ModuleCursor,
    formal_args: []const checked.CheckedTypeId,
    backing_root: checked.CheckedTypeId,

    fn builtinOwner(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?static_dispatch.BuiltinOwner {
        return .iter;
    }
    fn nominalBacking(context: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType) ?Resolver.NominalBacking {
        const self: *IteratorResolver = @ptrCast(@alignCast(context));
        return .{
            .cursor = self.cursor,
            .declaration = 0,
            .formal_args = self.formal_args,
            .root = self.backing_root,
        };
    }
    fn declaredOrder(_: *anyopaque, _: ModuleCursor, _: checked.CheckedNominalType, _: *std.ArrayList(Resolver.DeclaredField)) Allocator.Error!?ModuleCursor {
        return null;
    }

    const vtable = Resolver.VTable{
        .builtin_owner = builtinOwner,
        .nominal_backing = nominalBacking,
        .declared_order = declaredOrder,
    };

    fn resolver(self: *IteratorResolver) Resolver {
        return .{ .context = self, .vtable = &vtable };
    }
};

/// One `Iter(U64)` position plus the pieces a test needs to name it: the
/// declaration's formal binder, its backing root, and the instance's address.
const IteratorFixture = struct {
    fixture: TestFixture,
    formal: checked.CheckedTypeId,
    formals: [1]checked.CheckedTypeId,
    backing_root: checked.CheckedTypeId,
    instance: checked.CheckedTypeId,

    /// `recursive_backing` builds the declaration backing as `[Done, One(Iter(a))]`
    /// — the issue-10170 shape, where the backing refers back to the position it
    /// is the backing of.
    fn init(recursive_backing: bool) Allocator.Error!IteratorFixture {
        var fixture = TestFixture.init(testing.allocator);
        errdefer fixture.deinit();

        const formal = try fixture.add(.{ .rigid = .{} });
        const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
        // The instance is added after its backing, so a recursive backing names
        // the id the instance will take.
        const instance_id = @intFromEnum(fixture.nextId());
        const backing_root = if (recursive_backing) blk: {
            const rest: checked.CheckedTypeId = @enumFromInt(instance_id + 2);
            const empty = try fixture.add(.empty_tag_union);
            break :blk try fixture.addTagUnion(&.{
                .{ .name_text = "Done", .payloads = &.{} },
                .{ .name_text = "One", .payloads = &.{rest} },
            }, empty);
        } else blk: {
            const empty = try fixture.add(.empty_record);
            const item_label = try fixture.source_names.internRecordFieldLabel("item");
            const start: u32 = @intCast(fixture.record_fields.items.len);
            try fixture.record_fields.append(testing.allocator, .{ .name = item_label, .ty = formal });
            break :blk try fixture.add(.{ .record = .{
                .fields = .{ .start = start, .len = 1 },
                .ext = empty,
            } });
        };
        const instance = try fixture.addOpaqueBuiltinNominal(.iter, "Iter", &.{u64_ty});
        return .{
            .fixture = fixture,
            .formal = formal,
            .formals = .{formal},
            .backing_root = backing_root,
            .instance = instance,
        };
    }

    fn deinit(self: *IteratorFixture) void {
        self.fixture.deinit();
    }

    fn address(self: *IteratorFixture) PositionAddress {
        return .{
            .module_bytes = self.fixture.module_hash,
            .type_id = @intFromEnum(self.instance),
        };
    }

    /// Fill `holder` with the declaration source this fixture's position reads
    /// its backing from, and return the resolver over it.
    fn resolver(self: *IteratorFixture, holder: *IteratorResolver) Resolver {
        holder.* = .{
            .cursor = self.fixture.cursor(),
            .formal_args = &self.formals,
            .backing_root = self.backing_root,
        };
        return holder.resolver();
    }
};

fn emittedDef(store: *const MonoType.Store, ty: TypeId) ?MonoType.TypeDef {
    return switch (store.get(ty)) {
        .named => |named| named.def,
        else => null,
    };
}

test "an open representation position emits the declared encoding with no producer input" {
    var iter = try IteratorFixture.init(false);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const emitted = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const def = emittedDef(&store, emitted) orelse return error.TestUnexpectedResult;
    // No producer stated a representation here, so the position emits the
    // encoding its own declaration states.
    try testing.expectEqual(MonoType.IteratorRepresentation.none, def.iterator_representation);
    try testing.expectEqual(MonoType.IteratorKind.none, def.iterator_kind);
    try testing.expect(def.generated == null);
}

test "a declared producer representation raises the emitted position to the minted tier" {
    var iter = try IteratorFixture.init(false);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    const owner: names.TypeDigest = .{ .bytes = [_]u8{0x5A} ** 32 };
    try translator.declareRepresentationInput(.{
        .position = iter.address(),
        .representation = .{
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
            .generated = owner,
        },
    });

    var reason: SkipReason = undefined;
    const emitted = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const def = emittedDef(&store, emitted) orelse return error.TestUnexpectedResult;
    // Public meets minted: the minted side stands, so the emitted position
    // carries the producer's whole encoding.
    try testing.expectEqual(MonoType.IteratorRepresentation.minted, def.iterator_representation);
    try testing.expectEqual(MonoType.IteratorKind.list, def.iterator_kind);
    try testing.expectEqual(@as(u8, 1), def.iterator_depth);
    try testing.expect(def.generated != null);
    try testing.expect(std.mem.eql(u8, &def.generated.?.bytes, &owner.bytes));
    // The declared identity survived the relation.
    try testing.expectEqual(
        iter.fixture.module_hash[0],
        target_names.moduleIdentityBytes(def.module)[0],
    );
}

test "a declared forced-dynamic representation stands over the declared one" {
    var iter = try IteratorFixture.init(false);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    try translator.declareRepresentationInput(.{
        .position = iter.address(),
        .representation = .{
            .iterator_representation = .forced_dynamic,
            .iterator_kind = .forced_dynamic,
        },
    });

    var reason: SkipReason = undefined;
    const emitted = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const def = emittedDef(&store, emitted) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(MonoType.IteratorRepresentation.forced_dynamic, def.iterator_representation);
}

test "a declared producer representation places its minted components and backing" {
    var iter = try IteratorFixture.init(false);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    // The producer minted this representation over one component and generated a
    // backing for it; both are runtime encoding, not identity, so they ride out
    // with the sealed definition rather than through the checked data.
    const component = try store.internPrimitive(&target_names, .str);
    const minted_backing = try store.internRecord(&target_names, &.{});
    const components = [_]TypeId{component};
    try translator.declareRepresentationInput(.{
        .position = iter.address(),
        .representation = .{
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
            .generated = .{ .bytes = [_]u8{0x11} ** 32 },
            .components = &components,
            .backing = .{ .ty = minted_backing, .use = .runtime_layout_only, .authority = .generated_private },
        },
    });

    var reason: SkipReason = undefined;
    const emitted = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const named = switch (store.get(emitted)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    const args = store.span(named.args);
    try testing.expectEqual(@as(usize, 2), collections.GuardedList.borrowLen(args));
    try testing.expectEqual(component, collections.GuardedList.at(args, 1));
    const backing = named.backing orelse return error.TestUnexpectedResult;
    try testing.expectEqual(minted_backing, backing.ty);
    try testing.expectEqual(MonoType.BackingAuthority.generated_private, backing.authority);
}

test "issue 10170: a recursive minted backing seals without minting another identity" {
    var iter = try IteratorFixture.init(true);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    try translator.declareRepresentationInput(.{
        .position = iter.address(),
        .representation = .{
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 2,
            .generated = .{ .bytes = [_]u8{0x6A} ** 32 },
        },
    });

    // The backing is `[Done, One(<this position>)]`, so opening and sealing this
    // position descends into a backing that reaches the position again. The walk
    // terminates and the sealed root is the rooted recursive graph.
    var reason: SkipReason = undefined;
    const emitted = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const named = switch (store.get(emitted)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    try testing.expectEqual(MonoType.IteratorRepresentation.minted, named.def.iterator_representation);
    try testing.expectEqual(@as(u8, 2), named.def.iterator_depth);

    const backing = named.backing orelse return error.TestUnexpectedResult;
    const tags = switch (store.get(backing.ty)) {
        .tag_union => |span| store.tagSpan(span),
        else => return error.TestUnexpectedResult,
    };
    try testing.expectEqual(@as(usize, 2), collections.GuardedList.borrowLen(tags));
    // The `One` payload closed back onto the emitted position rather than
    // opening a second one.
    var closed = false;
    for (0..collections.GuardedList.borrowLen(tags)) |index| {
        const payloads = store.span(collections.GuardedList.at(tags, index).payloads);
        for (0..collections.GuardedList.borrowLen(payloads)) |payload_index| {
            if (collections.GuardedList.at(payloads, payload_index) == emitted) closed = true;
        }
    }
    try testing.expect(closed);
}

test "emitting one position twice yields one sealed encoding" {
    var iter = try IteratorFixture.init(false);
    defer iter.deinit();

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var holder: IteratorResolver = undefined;
    const resolver = iter.resolver(&holder);

    var translator = Translator.init(testing.allocator, &store, &target_names, resolver);
    defer translator.deinit();

    try translator.declareRepresentationInput(.{
        .position = iter.address(),
        .representation = .{
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
            .generated = .{ .bytes = [_]u8{0x77} ** 32 },
        },
    });

    var reason: SkipReason = undefined;
    const first = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    const second = try translator.translateGroundRoot(iter.fixture.cursor(), iter.instance, &reason);
    // Sealing runs per position and the same declared inputs produce the same
    // sealed encoding. Named ids are occurrence-held on production stores, so
    // the two emissions agree on content while each keeps its own id until
    // the holds lift (reunify.md section 8.5).
    try testing.expect(try store.typeEql(&target_names, first, second));
}

test "declarations are referenced" {
    testing.refAllDecls(@This());
}
