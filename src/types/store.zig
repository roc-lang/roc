//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const builtin = @import("builtin");
const tracy = @import("tracy");
const base = @import("base");
const collections = @import("collections");
const types = @import("types.zig");
const debug = @import("debug.zig");

const Allocator = std.mem.Allocator;

/// Compile-time switch selecting whether the savepoint trail can be
/// cross-checked against a full copy of the store.
///
/// - `.savepoint_only` (production): rollback trusts the savepoint's undo trail
///   alone. The copy, the cross-check assert, and the savepoint's copy field are
///   all compiled away — zero code, zero state.
/// - `.clone_crosscheck` (test builds): the full-copy cross-check is compiled
///   in, and a test can opt an individual savepoint into it via
///   `createSavepointVerifying`. Savepoints created the normal way still copy
///   nothing, so the suite runs the same savepoint-only path production uses.
const SavepointVerification = enum { savepoint_only, clone_crosscheck };
const savepoint_verification: SavepointVerification =
    if (builtin.is_test) .clone_crosscheck else .savepoint_only;

/// One journaled in-place write to a pre-existing slot (for trail rollback).
const SlotUndo = struct { idx: SlotStore.Idx, old: Slot };
/// One journaled in-place write to a pre-existing descriptor.
const DescUndo = struct { idx: DescStore.Idx, old: Desc };

const Desc = types.Descriptor;
const Var = types.Var;
const Content = types.Content;
const Rank = types.Rank;
const Flex = types.Flex;
const Rigid = types.Rigid;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;
const Descriptor = types.Descriptor;
const TypeIdent = types.TypeIdent;
const Alias = types.Alias;
const FlatType = types.FlatType;
const NominalType = types.NominalType;
const NominalDecl = types.NominalDecl;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const InterpolationPartMetadata = types.InterpolationPartMetadata;
const SourceDecl = types.SourceDecl;

/// A variable & its descriptor info
pub const ResolvedVarDesc = struct {
    var_: Var,
    is_root: bool,
    desc_idx: DescStore.Idx,
    desc: Desc,
};

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

/// One entry in the store's sorted nominal-declaration lookup index: a
/// declaration key (origin module identity, source statement) mapped to the
/// declaration's stable index in `Store.nominal_decls`. The index list is kept
/// sorted by key so lookups are a binary search; declarations themselves are
/// append-only so `NominalDecl.Idx` values stay stable across registrations.
const NominalDeclIndexEntry = struct {
    origin_module: base.ModuleIdentity.Idx,
    statement: u32,
    decl: NominalDecl.Idx,

    const SafeList = collections.SafeList(@This());

    /// Total order over declaration keys: origin module identity index first,
    /// then statement.
    fn orderByKey(origin_module: base.ModuleIdentity.Idx, statement: u32, entry: @This()) std.math.Order {
        const lhs_origin = @intFromEnum(origin_module);
        const rhs_origin = @intFromEnum(entry.origin_module);
        if (lhs_origin != rhs_origin) return std.math.order(lhs_origin, rhs_origin);
        return std.math.order(statement, entry.statement);
    }
};

/// Reperents either type data *or* a symlink to another type variable
pub const Slot = union(enum) {
    root: DescStore.Idx,
    redirect: Var,

    /// Calculate the size needed to serialize this Slot
    pub fn serializedSize(_: *const Slot) usize {
        return @sizeOf(u8) + @sizeOf(u32); // tag + data
    }

    /// Deserialize a Slot from the provided buffer
    pub fn deserializeFrom(buffer: []const u8) Allocator.Error!Slot {
        if (buffer.len < @sizeOf(u8) + @sizeOf(u32)) return error.BufferTooSmall;

        const tag = buffer[0];
        const data = std.mem.readInt(u32, buffer[1..5], .little);

        switch (tag) {
            0 => return Slot{ .root = @enumFromInt(data) },
            1 => return Slot{ .redirect = @enumFromInt(data) },
            else => return error.InvalidTag,
        }
    }
};

/// The store of all type variables and their descriptors
///
/// Each type variables (`Var`) points to a Slot.
/// A Slot either redirects to a different slot or contains type `Content`
///
/// Var maps to a SlotStore.Idx internally
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    /// Type variable storage
    slots: SlotStore,
    descs: DescStore,

    /// Storage for compound type parts
    vars: VarSafeList,
    record_fields: RecordFieldSafeMultiList,
    tags: TagSafeMultiList,
    interpolation_parts: InterpolationPartMetadata.SafeList,
    static_dispatch_constraints: StaticDispatchConstraint.SafeList,

    /// The nominal declaration table: one entry per nominal declaration whose
    /// applications can appear in this store (local declarations plus every
    /// imported declaration copied in by `copy_import`). Append-only, so
    /// `NominalDecl.Idx` values are stable; keyed lookups go through the
    /// sorted `nominal_decl_index`.
    nominal_decls: NominalDecl.SafeList,
    /// Sorted (origin module identity, statement) -> declaration index. Kept
    /// sorted on insert; lookups binary-search.
    nominal_decl_index: NominalDeclIndexEntry.SafeList,

    /// Undo trail for speculative unification. While a probe is active
    /// (`savepoint_active`), every in-place write to a slot or descriptor that
    /// existed before the probe began (index < `spec_baseline_*`) is journaled
    /// as (index, old value); rollback replays the journal in reverse. Entries
    /// appended during the probe are undone by truncation, not journaled.
    /// Probes never nest (they bracket leaf-level unification), so this is a
    /// flag, not a depth.
    savepoint_active: bool = false,
    savepoint_baseline_slots: u32 = 0,
    savepoint_baseline_descs: u32 = 0,
    slot_trail: std.ArrayListUnmanaged(SlotUndo) = .empty,
    desc_trail: std.ArrayListUnmanaged(DescUndo) = .empty,

    /// Init the unification table with default capacity.
    /// For production use with source files, prefer initFromSourceLen() which
    /// computes capacity based on source file size.
    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        return try Self.initCapacity(gpa, 1024, 512);
    }

    /// Init the type store with capacity heuristics based on source file size.
    /// Larger source files typically need more type slots and variables.
    ///
    /// Heuristics based on typical Roc code patterns:
    /// - ~1 type slot per 50 bytes of source
    /// - ~1 child element (vars, tags, record fields) per 100 bytes
    pub fn initFromSourceLen(gpa: Allocator, source_len: usize) std.mem.Allocator.Error!Self {
        const root_capacity = @max(2048, @min(50_000, source_len / 50));
        const child_capacity = @max(512, @min(10_000, source_len / 100));
        return try Self.initCapacity(gpa, root_capacity, child_capacity);
    }

    /// Init the unification table
    pub fn initCapacity(gpa: Allocator, root_capacity: usize, child_capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,

            // slots & descriptors
            .descs = try DescStore.init(gpa, root_capacity),
            .slots = try SlotStore.init(gpa, root_capacity),

            // everything else
            .vars = try VarSafeList.initCapacity(gpa, child_capacity),
            .record_fields = try RecordFieldSafeMultiList.initCapacity(gpa, child_capacity),
            .tags = try TagSafeMultiList.initCapacity(gpa, child_capacity),
            .interpolation_parts = try InterpolationPartMetadata.SafeList.initCapacity(gpa, child_capacity),
            .static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, child_capacity),

            // nominal declaration table (modules typically declare few types)
            .nominal_decls = try NominalDecl.SafeList.initCapacity(gpa, 16),
            .nominal_decl_index = try NominalDeclIndexEntry.SafeList.initCapacity(gpa, 16),
        };
    }

    /// Ensure that slots & descriptor arrays have at least the provided capacity
    pub fn ensureTotalCapacity(self: *Self, capacity: usize) Allocator.Error!void {
        try self.descs.backing.ensureTotalCapacity(self.gpa, capacity);
        try self.slots.backing.items.ensureTotalCapacity(self.gpa, capacity);
    }

    pub fn extendToVar(self: *Self, var_: Var) Allocator.Error!void {
        const needed_len = @intFromEnum(var_) + 1;
        while (self.slots.backing.len() < needed_len) {
            // Create a placeholder flex variable for each new slot
            try self.fresh();
        }
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        // slots & descriptors
        self.descs.deinit(self.gpa);
        self.slots.deinit(self.gpa);

        // everything else
        self.vars.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.interpolation_parts.deinit(self.gpa);
        self.static_dispatch_constraints.deinit(self.gpa);

        // nominal declaration table
        self.nominal_decls.deinit(self.gpa);
        self.nominal_decl_index.deinit(self.gpa);

        // speculation undo trail
        self.slot_trail.deinit(self.gpa);
        self.desc_trail.deinit(self.gpa);
    }

    /// Clone this store into fresh owned memory.
    pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .slots = .{ .backing = try self.slots.backing.clone(gpa) },
            .descs = .{ .backing = try self.descs.backing.clone(gpa) },
            .vars = try self.vars.clone(gpa),
            .record_fields = try self.record_fields.clone(gpa),
            .tags = try self.tags.clone(gpa),
            .interpolation_parts = try self.interpolation_parts.clone(gpa),
            .static_dispatch_constraints = try self.static_dispatch_constraints.clone(gpa),
            .nominal_decls = try self.nominal_decls.clone(gpa),
            .nominal_decl_index = try self.nominal_decl_index.clone(gpa),
        };
    }

    /// Return the number of type variables in the store.
    pub fn len(self: *const Self) u64 {
        return self.slots.backing.len();
    }

    /// Return true when checking left any live type variable in the explicit
    /// error state. Descriptors not referenced by a current slot are rollback
    /// history and do not affect checked output.
    pub fn containsErrContent(self: *const Self) bool {
        for (self.slots.backing.items.items) |slot| {
            switch (slot) {
                .root => |desc_idx| {
                    if (self.descs.get(desc_idx).content == .err) return true;
                },
                .redirect => {},
            }
        }
        return false;
    }

    // savepoint (create/rollback) for unification //
    //
    // Probe whether two types could unify, then discard the result. The undo
    // trail records each in-place write to a pre-existing slot/descriptor;
    // rollback replays it in reverse and truncates everything appended during
    // the probe. Cost is O(entries the probe mutated), not O(store size).

    /// A handle returned by `createSavepoint`, passed back to
    /// `rollbackToSavepoint`. Captures the rollback-only state: trail position
    /// and the append-only list lengths to rewind to (and, under the
    /// clone cross-check, a full store copy to compare against). The
    /// slot/desc baselines are not here — they live on the store as
    /// `savepoint_baseline_*` because they are also the per-write journaling
    /// threshold.
    pub const Savepoint = struct {
        slot_trail_len: usize,
        desc_trail_len: usize,
        vars_len: usize,
        record_fields_len: usize,
        tags_len: usize,
        interpolation_parts_len: usize,
        static_dispatch_constraints_len: usize,
        verify_clone: SavepointVerifyClone = savepoint_verify_clone_init,
    };

    /// Full store copy kept only under the clone cross-check, to assert that the
    /// trail restored the slots/descs to exactly their pre-savepoint values.
    const VerifyClone = struct {
        slots: []Slot,
        descs: std.MultiArrayList(Desc),
        fn deinit(self: *VerifyClone, gpa: Allocator) void {
            gpa.free(self.slots);
            self.descs.deinit(gpa);
        }
    };

    /// The `Savepoint.verify_clone` field type: a real optional when the clone
    /// cross-check is compiled in, and a zero-sized `void` otherwise so
    /// production savepoints carry no extra state.
    const SavepointVerifyClone = if (savepoint_verification == .clone_crosscheck) ?VerifyClone else void;
    const savepoint_verify_clone_init: SavepointVerifyClone = if (savepoint_verification == .clone_crosscheck) null else {};

    /// Open a savepoint over the type store. Pair with `rollbackToSavepoint`.
    /// Rollback relies solely on the undo trail; no store copy is taken, so this
    /// is the path production and the bulk of the test suite run.
    pub fn createSavepoint(self: *Self) Allocator.Error!Savepoint {
        return self.createSavepointImpl(false);
    }

    /// Test-only variant of `createSavepoint` that additionally copies the whole
    /// store, so the matching `rollbackToSavepoint` asserts the trail restored
    /// every slot and descriptor byte-for-byte — i.e. that the savepoint trail is
    /// semantically identical to fully copying the store and restoring the copy.
    /// Only available when the clone cross-check is compiled in (test builds).
    fn createSavepointVerifying(self: *Self) Allocator.Error!Savepoint {
        comptime std.debug.assert(savepoint_verification == .clone_crosscheck);
        return self.createSavepointImpl(true);
    }

    fn createSavepointImpl(self: *Self, comptime take_clone: bool) Allocator.Error!Savepoint {
        const verify_clone: SavepointVerifyClone =
            if (savepoint_verification == .clone_crosscheck and take_clone) vc: {
                break :vc VerifyClone{
                    .slots = try self.gpa.dupe(Slot, self.slots.backing.items.items),
                    .descs = try self.descs.backing.items.clone(self.gpa),
                };
            } else savepoint_verify_clone_init;

        const savepoint = Savepoint{
            .slot_trail_len = self.slot_trail.items.len,
            .desc_trail_len = self.desc_trail.items.len,
            .vars_len = self.vars.items.items.len,
            .record_fields_len = self.record_fields.items.len,
            .tags_len = self.tags.items.len,
            .interpolation_parts_len = self.interpolation_parts.items.items.len,
            .static_dispatch_constraints_len = self.static_dispatch_constraints.items.items.len,
            .verify_clone = verify_clone,
        };

        // Probes never nest; catch it loudly if that invariant is ever broken.
        std.debug.assert(!self.savepoint_active);
        self.savepoint_active = true;
        self.savepoint_baseline_slots = @intCast(self.slots.backing.len());
        self.savepoint_baseline_descs = @intCast(self.descs.backing.items.len);

        return savepoint;
    }

    /// Close a savepoint KEEPING everything done since it was created — the
    /// counterpart to `rollbackToSavepoint` for a speculation that succeeded
    /// and is committed in place. The journaled undo entries are dead weight
    /// once nothing will replay them, so the trails shrink back to their
    /// savepoint lengths; the baselines deactivate so in-place writes stop
    /// journaling.
    pub fn commitSavepoint(self: *Self, savepoint: *Savepoint) void {
        std.debug.assert(self.savepoint_active);
        self.desc_trail.shrinkRetainingCapacity(savepoint.desc_trail_len);
        self.slot_trail.shrinkRetainingCapacity(savepoint.slot_trail_len);
        self.savepoint_active = false;

        if (savepoint_verification == .clone_crosscheck) {
            if (savepoint.verify_clone) |*vclone| {
                vclone.deinit(self.gpa);
                savepoint.verify_clone = null;
            }
        }
    }

    /// Undo everything done since `savepoint` was created.
    pub fn rollbackToSavepoint(self: *Self, savepoint: *Savepoint) void {
        // Replay journaled in-place writes in reverse so each pre-existing entry
        // lands back on its original value.
        var di = self.desc_trail.items.len;
        while (di > savepoint.desc_trail_len) {
            di -= 1;
            const u = self.desc_trail.items[di];
            self.descs.set(u.idx, u.old);
        }
        self.desc_trail.shrinkRetainingCapacity(savepoint.desc_trail_len);

        var si = self.slot_trail.items.len;
        while (si > savepoint.slot_trail_len) {
            si -= 1;
            const u = self.slot_trail.items[si];
            self.slots.set(u.idx, u.old);
        }
        self.slot_trail.shrinkRetainingCapacity(savepoint.slot_trail_len);

        // Drop everything appended during the probe. The slot/desc baselines are
        // the store fields (also the journaling threshold); the rest come from
        // the savepoint.
        self.slots.backing.items.shrinkRetainingCapacity(self.savepoint_baseline_slots);
        self.descs.backing.items.shrinkRetainingCapacity(self.savepoint_baseline_descs);
        self.vars.items.shrinkRetainingCapacity(savepoint.vars_len);
        self.record_fields.items.shrinkRetainingCapacity(savepoint.record_fields_len);
        self.tags.items.shrinkRetainingCapacity(savepoint.tags_len);
        self.interpolation_parts.items.shrinkRetainingCapacity(savepoint.interpolation_parts_len);
        self.static_dispatch_constraints.items.shrinkRetainingCapacity(savepoint.static_dispatch_constraints_len);

        // Back to not speculating; savepoint_baseline_* are dead until the next create.
        self.savepoint_active = false;

        if (savepoint_verification == .clone_crosscheck) {
            if (savepoint.verify_clone) |*vclone| {
                self.assertMatchesClone(vclone);
                vclone.deinit(self.gpa);
                savepoint.verify_clone = null;
            }
        }
    }

    /// Clone cross-check: assert the trail-restored store is byte-for-byte
    /// identical to the full copy taken at `createSavepointVerifying`.
    fn assertMatchesClone(self: *Self, vclone: *const VerifyClone) void {
        const live_slots = self.slots.backing.items.items;
        std.debug.assert(live_slots.len == vclone.slots.len);
        for (live_slots, vclone.slots) |a, b| std.debug.assert(std.meta.eql(a, b));

        std.debug.assert(self.descs.backing.items.len == vclone.descs.len);
        var i: usize = 0;
        while (i < vclone.descs.len) : (i += 1) {
            std.debug.assert(std.meta.eql(self.descs.backing.items.get(i), vclone.descs.get(i)));
        }
    }

    /// In-place slot write. While a probe is active, journals the slot's previous
    /// value so rollback can restore it; a failed journal append is propagated
    /// rather than risk a type store the trail can no longer faithfully undo.
    fn setSlot(self: *Self, idx: SlotStore.Idx, val: Slot) Allocator.Error!void {
        if (self.savepoint_active and @intFromEnum(idx) < self.savepoint_baseline_slots) {
            try self.slot_trail.append(self.gpa, .{ .idx = idx, .old = self.slots.get(idx) });
        }
        self.slots.set(idx, val);
    }

    /// In-place descriptor write. See setSlot.
    fn setDesc(self: *Self, idx: DescStore.Idx, val: Desc) Allocator.Error!void {
        if (self.savepoint_active and @intFromEnum(idx) < self.savepoint_baseline_descs) {
            try self.desc_trail.append(self.gpa, .{ .idx = idx, .old = self.descs.get(idx) });
        }
        self.descs.set(idx, val);
    }

    // fresh variables //

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn fresh(self: *Self) std.mem.Allocator.Error!Var {
        const trace = tracy.traceNamed(@src(), "typesStore.fresh");
        defer trace.end();
        return try self.freshFromContent(Content{ .flex = Flex.init() });
    }

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn freshWithRank(self: *Self, rank: Rank) std.mem.Allocator.Error!Var {
        return try self.freshFromContentWithRank(Content{ .flex = Flex.init() }, rank);
    }

    /// Create a new variable with the provided desc
    /// Used in tests
    /// TODO: Can we remove this function? It hardcodes rank, which is fine for
    /// test but we can never use this in actual typechecking
    pub fn freshFromContent(self: *Self, content: Content) std.mem.Allocator.Error!Var {
        const trace = tracy.traceNamed(@src(), "typesStore.freshFromContent");
        defer trace.end();
        const desc_idx = try self.descs.insert(self.gpa, .{
            .content = content,
            .rank = Rank.outermost,
        });
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given content and rank
    pub fn freshFromContentWithRank(self: *Self, content: Content, rank: Rank) std.mem.Allocator.Error!Var {
        const desc_idx = try self.descs.insert(self.gpa, .{
            .content = content,
            .rank = rank,
        });
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a variable redirecting to the provided var
    /// Used in tests
    pub fn freshRedirect(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        const slot_idx = try self.slots.insert(self.gpa, .{ .redirect = var_ });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given descriptor
    pub fn register(self: *Self, desc: Desc) std.mem.Allocator.Error!Var {
        const desc_idx = try self.descs.insert(self.gpa, desc);
        const slot_idx = try self.slots.insert(self.gpa, .{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the provided content assuming there is capacity
    pub fn appendFromContentAssumeCapacity(self: *Self, content: Content, rank: Rank) Var {
        const desc_idx = self.descs.appendAssumeCapacity(.{
            .content = content,
            .rank = rank,
        });
        const slot_idx = self.slots.appendAssumeCapacity(.{ .root = desc_idx });
        return Self.slotIdxToVar(slot_idx);
    }

    // setting variables //

    /// Reset a variable's slot to a pristine unbound flex root at the given
    /// rank — the state initial slot filling produces — severing any redirect.
    ///
    /// IMPORTANT: Only sound when nothing live references the variable's
    /// previous class through this slot. Used by the annotated-scheme
    /// pre-pass to return annotation nodes to their pre-generation state
    /// (after the declared scheme was copied out of them) so the def's body
    /// check can generate the annotation again.
    pub fn resetVarToUnbound(self: *Self, target_var: Var, rank: Rank) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        const desc_idx = try self.descs.insert(self.gpa, .{
            .content = .{ .flex = Flex.init() },
            .rank = rank,
        });
        try self.setSlot(Self.varToSlotIdx(target_var), .{ .root = desc_idx });
    }

    /// Set a type variable to the provided content
    ///
    /// IMPORTANT: When using this function during type checking, it's possible
    /// to loose `rank` information! You should prefer to use regular `unify`
    /// over this function, which correctly propagates rank, unless you already
    /// know the two vars are of  the same rank.
    pub fn dangerousSetVarDesc(self: *Self, target_var: Var, desc: Desc) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        const resolved = self.resolveVar(target_var);
        try self.setDesc(resolved.desc_idx, desc);
    }

    /// Set a type variable to the provided content
    pub fn setVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        const resolved = self.resolveVar(target_var);
        var desc = resolved.desc;
        desc.content = content;
        try self.setDesc(resolved.desc_idx, desc);
    }

    /// The declared rule a `dangerousSetVarRedirect` call site bends the solved
    /// graph under. A redirect outside ordinary unification is indistinguishable
    /// at review time from a change to the language's typing rules, so every call
    /// site must name the rule it operates under, and every member here must be
    /// one of:
    ///
    ///   (i)  diagnostic recovery on an already-reported error — the redirect
    ///        cannot change which programs typecheck or which plans are output
    ///        for error-free programs; or
    ///   (ii) a language/pipeline rule declared in design.md — the member's doc
    ///        comment names the design.md section that declares it, and the rule
    ///        has tests pinning both its accepted and its rejected side.
    ///
    /// A new call site must either cite an existing member whose rule covers it
    /// or add a member (and the design.md declaration it cites) in the same
    /// change. "It makes a test pass" is not a rule.
    pub const RedirectRule = enum {
        /// (i) Diagnostic recovery: the target var belongs to an expression
        /// whose error has already been reported, and the redirect only lets
        /// checking continue past it.
        diagnostic_recovery_reported_error,
        /// (ii) design.md "Platform/App Relation" (for-clause alias identity):
        /// a platform requirement's for-clause alias is a binder over an
        /// app-supplied type, so copied occurrences of the alias resolve to the
        /// app's own type declaration.
        for_clause_alias_identity,
        /// (ii) design.md "Hosted Try Question Widening": `?` on a direct call
        /// of a hosted function widens the condition's closed error row to the
        /// enclosing annotated return's error row when every visible error is
        /// included, keeping the hosted callee's declared closed row intact.
        hosted_try_question_widening,
    };

    /// Set a type variable to redirect to the provided variables.
    /// During type-checking, you probably don't want to use this function.
    ///
    /// This is the primitive that mutates the solved graph outside ordinary
    /// unification. `rule` names the declared rule (see `RedirectRule`) the call
    /// site operates under; a call without one does not compile.
    ///
    /// IMPORTANT: When using this function during type checking, it's possible
    /// to loose `rank` information! You should prefer to use regular `unify`
    /// over this function, which correctly propagates rank, unless you already
    /// know the two vars are of the same rank.
    pub fn dangerousSetVarRedirect(self: *Self, comptime rule: RedirectRule, target_var: Var, redirect_to: Var) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        std.debug.assert(@intFromEnum(redirect_to) < self.len());
        // Self-redirects cause infinite loops in resolveVar
        if (std.debug.runtime_safety and target_var == redirect_to) {
            std.debug.panic("self-redirect of var {d} under rule {s}", .{ @intFromEnum(target_var), @tagName(rule) });
        }
        if (std.debug.runtime_safety) {
            // Redirecting a root var into a transparent alias whose backing resolves
            // back to that same root creates a self-referential (infinite) alias.
            // Recursive transparent aliases are illegal, so this is always a bug;
            // catch it loudly rather than silently producing an INFINITE TYPE later.
            const redirect_resolved = self.resolveVar(redirect_to);
            if (redirect_resolved.desc.content == .alias) {
                const backing_root = self.resolveVar(self.getAliasBackingVar(redirect_resolved.desc.content.alias)).var_;
                std.debug.assert(backing_root != target_var);
            }
        }
        const slot_idx = Self.varToSlotIdx(target_var);
        try self.setSlot(slot_idx, .{ .redirect = redirect_to });
    }

    // make builtin types //

    /// Create a Bool type as a tag union with False and True tags.
    /// Use cached idents from CommonIdents.false_tag and CommonIdents.true_tag.
    pub fn mkBool(self: *Self, false_ident: base.Ident.Idx, true_ident: base.Ident.Idx, ext_var: Var) std.mem.Allocator.Error!Content {
        const false_tag = try self.mkTag(false_ident, &[_]Var{});
        const true_tag = try self.mkTag(true_ident, &[_]Var{});
        return try self.mkTagUnion(&[_]Tag{ false_tag, true_tag }, ext_var);
    }

    /// Create a Result type as a tag union with Ok and Err tags.
    /// Use cached idents from CommonIdents.ok and CommonIdents.err.
    pub fn mkResult(
        self: *Self,
        ok_ident: base.Ident.Idx,
        err_ident: base.Ident.Idx,
        ok_var: Var,
        err_var: Var,
        ext_var: Var,
    ) std.mem.Allocator.Error!Content {
        const ok_tag = try self.mkTag(ok_ident, &[_]Var{ok_var});
        const err_tag = try self.mkTag(err_ident, &[_]Var{err_var});
        return try self.mkTagUnion(&[_]Tag{ ok_tag, err_tag }, ext_var);
    }

    // make content types //

    /// Make a tag union data type
    /// Does not insert content into the types store
    pub fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!Content {
        const tags_range = try self.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return Content{ .structure = .{ .tag_union = tag_union } };
    }

    /// Make a tag data type
    /// Does not insert content into the types store
    pub fn mkTag(self: *Self, name: base.Ident.Idx, args: []const Var) std.mem.Allocator.Error!Tag {
        const args_range = try self.appendVars(args);
        return Tag{ .name = name, .args = args_range };
    }

    /// Make alias data type
    /// Does not insert content into the types store
    pub fn mkAlias(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
    ) std.mem.Allocator.Error!Content {
        return self.mkAliasWithSourceDecl(ident, backing_var, args, origin_module, null);
    }

    pub fn mkAliasWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
    ) std.mem.Allocator.Error!Content {
        return self.mkAliasWithSourceDeclAndBuiltinOrigin(
            ident,
            backing_var,
            args,
            origin_module,
            source_decl,
            false,
        );
    }

    pub fn mkAliasWithSourceDeclAndBuiltinOrigin(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        builtin_origin: bool,
    ) std.mem.Allocator.Error!Content {
        const packed_source_decl = try SourceDecl.fromOptionalWithBuiltinOriginChecked(source_decl, builtin_origin);
        const backing_idx = try self.appendVar(backing_var);
        var span = try self.appendVars(args);

        // Adjust args span to include backing  var
        span.start = backing_idx;
        span.count = span.count + 1;

        return Content{
            .alias = Alias{
                .ident = ident,
                .vars = .{ .nonempty = span },
                .origin_module = origin_module,
                .source_decl = packed_source_decl,
            },
        };
    }

    /// Make a nominal type application: identity plus actual type args only.
    /// The backing type lives in the declaration table, not the application.
    /// Does not insert content into the types store
    pub fn mkNominal(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDecl(ident, args, origin_module, null, is_opaque);
    }

    pub fn mkNominalWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDeclAndBuiltinOrigin(
            ident,
            args,
            origin_module,
            source_decl,
            is_opaque,
            false,
        );
    }

    pub fn mkNominalWithSourceDeclAndBuiltinOrigin(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        is_opaque: bool,
        builtin_origin: bool,
    ) std.mem.Allocator.Error!Content {
        const source = try NominalType.Source.initChecked(
            try SourceDecl.fromOptionalWithBuiltinOriginChecked(source_decl, builtin_origin),
            is_opaque,
            builtin_origin,
        );
        const args_range = try self.appendVars(args);

        return Content{ .structure = FlatType{
            .nominal_type = NominalType{
                .ident = ident,
                .args = args_range,
                .origin_module = origin_module,
                .source = source,
            },
        } };
    }

    // Make a function data type with unbound effectfulness
    // Does not insert content into the types store.
    pub fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return self.mkFuncUnboundWithEffectDeps(args, ret, &.{});
    }

    /// Make a function data type whose effect is inferred from directed
    /// dependencies on other function types.
    pub fn mkFuncUnboundWithEffectDeps(
        self: *Self,
        args: []const Var,
        ret: Var,
        effect_deps: []const Var,
    ) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        const effect_deps_range = try self.appendVars(effect_deps);
        return Content{ .structure = .{ .fn_unbound = .{
            .args = args_range,
            .ret = ret,
            .effect_deps = effect_deps_range,
        } } };
    }

    // Make a pure function data type (as opposed to an effectful or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        return Content{ .structure = .{ .fn_pure = .{
            .args = args_range,
            .ret = ret,
        } } };
    }

    // Make an effectful function data type (as opposed to a pure or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        return Content{ .structure = .{ .fn_effectful = .{
            .args = args_range,
            .ret = ret,
        } } };
    }

    // sub list setters //

    /// Append a var to the backing list, returning the idx
    pub fn appendVar(self: *Self, v: Var) std.mem.Allocator.Error!VarSafeList.Idx {
        return try self.vars.append(self.gpa, v);
    }

    /// Append a var to the backing list, returning the idx
    pub fn appendVars(self: *Self, s: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        const trace = tracy.traceNamed(@src(), "typesStore.appendVars");
        defer trace.end();
        return try self.vars.appendSlice(self.gpa, s);
    }

    /// Append a record field to the backing list, returning the idx
    pub fn appendRecordField(self: *Self, field: RecordField) std.mem.Allocator.Error!RecordFieldSafeMultiList.Idx {
        return try self.record_fields.append(self.gpa, field);
    }

    /// Append a slice of record fields to the backing list, returning the range
    pub fn appendRecordFields(self: *Self, slice: []const RecordField) std.mem.Allocator.Error!RecordFieldSafeMultiList.Range {
        return try self.record_fields.appendSlice(self.gpa, slice);
    }

    /// Append a tag to the backing list, returning the idx
    pub fn appendTag(self: *Self, tag: Tag) Allocator.Error!TagSafeMultiList.Idx {
        return try self.tags.append(self.gpa, tag);
    }

    /// Append a slice of tags to the backing list, returning the range
    pub fn appendTags(self: *Self, slice: []const Tag) std.mem.Allocator.Error!TagSafeMultiList.Range {
        return try self.tags.appendSlice(self.gpa, slice);
    }

    /// Append interpolation part metadata to the backing list, returning the range
    pub fn appendInterpolationParts(self: *Self, slice: []const InterpolationPartMetadata) std.mem.Allocator.Error!InterpolationPartMetadata.SafeList.Range {
        return try self.interpolation_parts.appendSlice(self.gpa, slice);
    }

    /// Append static dispatch constraints to the backing list, returning the range
    pub fn appendStaticDispatchConstraints(self: *Self, s: []const StaticDispatchConstraint) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        return try self.static_dispatch_constraints.appendSlice(self.gpa, s);
    }

    // sub list getters //

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceVars(self: *const Self, range: VarSafeList.Range) []Var {
        return self.vars.sliceRange(range);
    }

    /// Get an iterator over vars for the given range.
    /// Use this instead of sliceVars when the iteration may trigger
    /// reallocations (e.g., during unification).
    pub fn iterVars(self: *const Self, range: VarSafeList.Range) VarSafeList.Iterator {
        return self.vars.iterRange(range);
    }

    /// Get a var at a specific offset within a range.
    /// Use this for index-based iteration when unification may trigger reallocations.
    pub fn getVarAt(self: *const Self, range: VarSafeList.Range, offset: u32) Var {
        std.debug.assert(offset < range.count);
        const idx: VarSafeList.Idx = @enumFromInt(@intFromEnum(range.start) + offset);
        return self.vars.get(idx).*;
    }

    /// Given a range, get a slice of record fields from the backing array
    pub fn getRecordFieldsSlice(self: *const Self, range: RecordFieldSafeMultiList.Range) RecordFieldSafeMultiList.Slice {
        return self.record_fields.sliceRange(range);
    }

    /// Given a range, get a slice of tags from the backing array
    pub fn getTagsSlice(self: *const Self, range: TagSafeMultiList.Range) TagSafeMultiList.Slice {
        return self.tags.sliceRange(range);
    }

    /// Given a range, get a slice of interpolation part metadata from the backing array
    pub fn sliceInterpolationParts(self: *const Self, range: InterpolationPartMetadata.SafeList.Range) []InterpolationPartMetadata {
        return self.interpolation_parts.sliceRange(range);
    }

    /// Get an interpolation part at a specific offset within a range.
    /// Use this for index-based iteration when checking can trigger reallocations.
    pub fn getInterpolationPartAt(self: *const Self, range: InterpolationPartMetadata.SafeList.Range, offset: u32) InterpolationPartMetadata {
        std.debug.assert(offset < range.count);
        const idx: InterpolationPartMetadata.SafeList.Idx = @enumFromInt(@intFromEnum(range.start) + offset);
        return self.interpolation_parts.get(idx).*;
    }

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceStaticDispatchConstraints(self: *const Self, range: StaticDispatchConstraint.SafeList.Range) []StaticDispatchConstraint {
        return self.static_dispatch_constraints.sliceRange(range);
    }

    /// Get an iterator over static-dispatch constraints for the given range.
    /// Use this instead of sliceStaticDispatchConstraints when the iteration
    /// may append to the constraint store (e.g., instantiation/copy during a
    /// candidate probe) — a held slice would dangle on reallocation.
    pub fn iterStaticDispatchConstraints(self: *const Self, range: StaticDispatchConstraint.SafeList.Range) StaticDispatchConstraint.SafeList.Iterator {
        return self.static_dispatch_constraints.iterRange(range);
    }

    pub fn getStaticDispatchConstraintAt(self: *const Self, idx: usize) StaticDispatchConstraint {
        return self.static_dispatch_constraints.items.items[idx];
    }

    // helpers - alias types //

    // Alias types contain a span of variables. In this span, the 1st element
    // is the backing variable, and the remainder are the arguments

    /// Get the backing var for this alias type
    pub fn getAliasBackingVar(self: *const Self, alias: Alias) Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        return self.vars.get(alias.vars.nonempty.start).*;
    }

    /// Get the arg vars for this alias type
    pub fn sliceAliasArgs(self: *const Self, alias: Alias) []Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        const slice = self.vars.sliceRange(alias.vars.nonempty);
        return slice[1..];
    }

    /// Get the an iterator arg vars for this alias type
    pub fn iterAliasArgs(self: *const Self, alias: Alias) VarSafeList.Iterator {
        std.debug.assert(alias.vars.nonempty.count > 0);
        var span = alias.vars.nonempty;
        span.dropFirstElem();
        return self.vars.iterRange(span);
    }

    // helpers - nominal types //

    // A nominal application carries only its actual type arguments; backing
    // structure is resolved through the declaration table.

    /// Get the arg vars for this nominal type
    pub fn sliceNominalArgs(self: *const Self, nominal: NominalType) []Var {
        return self.vars.sliceRange(nominal.args);
    }

    /// Get the arg vars range for this nominal type.
    /// Returns a range (start index + count) which can be stored safely.
    /// Unlike sliceNominalArgs, this returns indices that remain valid even if
    /// the underlying storage is reallocated.
    pub fn getNominalArgsRange(nominal: NominalType) VarSafeList.Range {
        return nominal.args;
    }

    /// Get the an iterator arg vars for this nominal type
    pub fn iterNominalArgs(self: *const Self, nominal: NominalType) VarSafeList.Iterator {
        return self.vars.iterRange(nominal.args);
    }

    /// Whether this nominal application's declaration is known invalid
    /// (malformed backing or invalid recursion). Applications whose
    /// declaration cannot be resolved (no source declaration — possible only
    /// for hand-constructed types in tests) count as valid.
    pub fn nominalDeclIsInvalid(self: *const Self, nominal: NominalType) bool {
        const decl_idx = self.lookupNominalDecl(nominal) orelse return false;
        return !self.getNominalDecl(decl_idx).isValid();
    }

    // nominal declaration table //

    /// Register a nominal declaration, or update it if its key is already
    /// present (a declaration is re-registered when its body is generated
    /// after predeclaration). Returns the declaration's stable index.
    ///
    /// Must not run inside a unification savepoint: the declaration table is
    /// not journaled, so a rollback could not undo the registration.
    pub fn registerNominalDecl(self: *Self, decl: NominalDecl) Allocator.Error!NominalDecl.Idx {
        std.debug.assert(!self.savepoint_active);
        std.debug.assert(decl.source.sourceDecl().present);

        const statement = decl.statement();
        const entries = self.nominal_decl_index.items.items;
        var lo: usize = 0;
        var hi: usize = entries.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            switch (NominalDeclIndexEntry.orderByKey(decl.origin_module, statement, entries[mid])) {
                .lt => hi = mid,
                .gt => lo = mid + 1,
                .eq => {
                    const existing = entries[mid].decl;
                    self.nominal_decls.set(existing, decl);
                    return existing;
                },
            }
        }

        const decl_idx = try self.nominal_decls.append(self.gpa, decl);
        try self.nominal_decl_index.items.insert(self.gpa, lo, .{
            .origin_module = decl.origin_module,
            .statement = statement,
            .decl = decl_idx,
        });
        return decl_idx;
    }

    /// Look up a nominal declaration by its key: the declaring module's
    /// env-local identity index plus the declaration statement in that module.
    pub fn lookupNominalDeclByKey(
        self: *const Self,
        origin_module: base.ModuleIdentity.Idx,
        statement: u32,
    ) ?NominalDecl.Idx {
        const entries = self.nominal_decl_index.items.items;
        var lo: usize = 0;
        var hi: usize = entries.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            switch (NominalDeclIndexEntry.orderByKey(origin_module, statement, entries[mid])) {
                .lt => hi = mid,
                .gt => lo = mid + 1,
                .eq => return entries[mid].decl,
            }
        }
        return null;
    }

    /// Look up the declaration for a nominal application. Returns null only
    /// when the application carries no source declaration (possible for
    /// hand-constructed types in tests; checker-created applications always
    /// carry one).
    pub fn lookupNominalDecl(self: *const Self, nominal: NominalType) ?NominalDecl.Idx {
        const source_decl = nominal.sourceDecl();
        if (!source_decl.present) return null;
        return self.lookupNominalDeclByKey(nominal.origin_module, source_decl.statement);
    }

    /// Get a nominal declaration by index.
    pub fn getNominalDecl(self: *const Self, idx: NominalDecl.Idx) NominalDecl {
        return self.nominal_decls.get(idx).*;
    }

    /// Overwrite a nominal declaration entry in place (used by copy_import to
    /// fill a reserved entry once its formals and backing have been copied).
    pub fn setNominalDecl(self: *Self, idx: NominalDecl.Idx, decl: NominalDecl) void {
        std.debug.assert(!self.savepoint_active);
        self.nominal_decls.set(idx, decl);
    }

    /// Mark a nominal declaration invalid (malformed backing or invalid
    /// recursion). Applications of invalid declarations poison to err.
    pub fn markNominalDeclInvalid(self: *Self, idx: NominalDecl.Idx) void {
        std.debug.assert(!self.savepoint_active);
        var decl = self.nominal_decls.get(idx).*;
        decl.flags.valid = false;
        self.nominal_decls.set(idx, decl);
    }

    /// The number of registered nominal declarations.
    pub fn nominalDeclCount(self: *const Self) u64 {
        return self.nominal_decls.len();
    }

    // rank //

    /// Set the rank for a descriptor
    pub fn setDescRank(self: *Self, desc_idx: DescStore.Idx, rank: Rank) Allocator.Error!void {
        var desc = self.descs.get(desc_idx);
        desc.rank = rank;
        try self.setDesc(desc_idx, desc);
    }

    // resolvers //

    /// Given a type var, follow all redirects until finding the root descriptor
    ///
    /// Will mutate the DescStore in place to compress the path
    pub fn resolveVarAndCompressPath(self: *Self, initial_var: Var) ResolvedVarDesc {
        // Resolve the variable
        const redirected = self.resolveVar(initial_var);
        const redirected_root_var = redirected.var_;

        // Compress the chain so future resolves are O(1). Skipped during a probe:
        // compression is a pure optimization (it never changes what a var
        // resolves to), so it would only be journaled and rolled back. Skipping
        // also keeps this resolver infallible (no journaling, no allocation).
        if (!self.savepoint_active and initial_var != redirected_root_var) {
            var compressed_slot_idx = Self.varToSlotIdx(initial_var);
            var compressed_slot: Slot = self.slots.get(compressed_slot_idx);
            var guard = debug.IterationGuard.init("resolveVarAndCompressPath");
            while (true) {
                guard.tick();
                switch (compressed_slot) {
                    .redirect => |next_redirect_var| {
                        // Raw set: not speculating here, so nothing to journal.
                        self.slots.set(compressed_slot_idx, Slot{ .redirect = redirected_root_var });
                        compressed_slot_idx = Self.varToSlotIdx(next_redirect_var);
                        compressed_slot = self.slots.get(compressed_slot_idx);
                    },
                    .root => break,
                }
            }
        }

        // Compress the path
        return redirected;
    }

    /// Given a type var, follow all redirects until finding the root descriptor
    pub fn resolveVar(self: *const Self, initial_var: Var) ResolvedVarDesc {
        const trace = tracy.traceNamed(@src(), "typesStore.resolveVar");
        defer trace.end();
        var redirected_slot_idx = Self.varToSlotIdx(initial_var);
        var redirected_slot: Slot = self.slots.get(redirected_slot_idx);

        var is_root = true;
        var guard = debug.IterationGuard.init("resolveVar");

        while (true) {
            guard.tick();
            switch (redirected_slot) {
                .redirect => |next_redirect_var| {
                    redirected_slot_idx = Self.varToSlotIdx(next_redirect_var);
                    redirected_slot = self.slots.get(redirected_slot_idx);

                    is_root = false;
                },
                .root => |desc_idx| {
                    const redirected_root_var = Self.slotIdxToVar(redirected_slot_idx);
                    const desc = self.descs.get(desc_idx);
                    return .{
                        .var_ = redirected_root_var,
                        .is_root = is_root,
                        .desc_idx = desc_idx,
                        .desc = desc,
                    };
                },
            }
        }
    }

    /// Whether `var_` resolves through aliases to a function structure.
    pub fn varResolvesToFunction(self: *const Self, var_: Var) bool {
        var current = var_;
        while (true) {
            const resolved = self.resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| current = self.getAliasBackingVar(alias),
                .structure => |flat| return switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => true,
                    else => false,
                },
                .err, .flex, .rigid => return false,
            }
        }
    }

    // equivalence //

    /// The result of checking for equivalence
    pub const VarEquivResult = union(enum) { equiv, not_equiv: ResolvedVarDescs };

    /// Check if two variables are equivalent
    /// This will follow all redirects and compress the path
    ///
    /// If the vars are *not equivalent, then return the resolved vars & descs
    pub fn checkVarsEquiv(self: *Self, a_var: Var, b_var: Var) VarEquivResult {
        const a = self.resolveVarAndCompressPath(a_var);
        const b = self.resolveVarAndCompressPath(b_var);
        if (a.desc_idx == b.desc_idx) {
            return .equiv;
        } else {
            return .{ .not_equiv = .{ .a = a, .b = b } };
        }
    }

    // union //

    /// Link the variables & updated the content in the unification table
    /// * update b to to the new desc value
    /// * redirect a -> b
    ///
    /// The merge direction (a -> b) is load-bearing and must not be changed.
    /// Multiple parts of the unification algorithm depend on this specific order.
    /// Callers therefore control which variable survives by choosing operand
    /// order: a variable that must remain canonical (e.g. a shared expected-return
    /// var reused across branches and embedded in a function's annotated type)
    /// has to be passed as `b`. Passing it as `a` redirects it away and can tie a
    /// recursive type parameter off to a duplicate rigid, producing a spurious
    /// mismatch (see `Check.checkBranchBodyAgainstExpected`).
    /// Alias spelling is not preserved by choosing an alias representative; source
    /// alias views stay separate from the concrete solved backing variable.
    ///
    // NOTE: The elm & the roc compiler do this step differently
    // * The elm compiler sets b to redirect to a
    // * The roc compiler sets a to redirect to b
    pub fn union_(self: *Self, a_var: Var, b_var: Var, new_desc: Desc) Allocator.Error!void {
        const b_data = self.resolveVarAndCompressPath(b_var);

        // Update b to be the new desc
        try self.setDesc(b_data.desc_idx, new_desc);

        // Update a to point to b
        try self.setSlot(Self.varToSlotIdx(a_var), .{ .redirect = b_var });
    }

    // test helpers //

    /// Get the slot for the provided var
    /// Used in tests
    /// If you're reaching for this in non-test code, you probably want
    /// resolveVar or resolveVarAndCompressPath instead
    pub fn getSlot(self: *Self, var_: Var) Slot {
        return self.slots.get(Self.varToSlotIdx(var_));
    }

    /// Get the descriptor for the provided idx
    /// Used in tests
    pub fn getDesc(self: *Self, desc_idx: DescStore.Idx) Desc {
        return self.descs.get(desc_idx);
    }

    const Error = error{VarNotRoot};

    /// Set a root var to be the specified content
    /// Used in tests
    pub fn setRootVarContent(self: *Self, var_: Var, content: Content) (error{VarNotRoot} || Allocator.Error)!void {
        const slot = self.slots.get(Self.varToSlotIdx(var_));
        switch (slot) {
            .root => |desc_idx| {
                var desc = self.descs.get(desc_idx);
                desc.content = content;
                try self.setDesc(desc_idx, desc);
            },
            .redirect => {
                return error.VarNotRoot;
            },
        }
    }

    // helpers //

    pub fn varToSlotIdx(var_: Var) SlotStore.Idx {
        return @enumFromInt(@intFromEnum(var_));
    }

    fn slotIdxToVar(slot_idx: SlotStore.Idx) Var {
        return @enumFromInt(@intFromEnum(slot_idx));
    }

    // serialization //

    /// Serialized representation of types store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        gpa: [2]u64, // Reserve space for allocator (vtable ptr + context ptr), provided during deserialization
        slots: SlotStore.Serialized,
        descs: DescStore.Serialized,
        vars: VarSafeList.Serialized,
        record_fields: RecordFieldSafeMultiList.Serialized,
        tags: TagSafeMultiList.Serialized,
        interpolation_parts: InterpolationPartMetadata.SafeList.Serialized,
        static_dispatch_constraints: StaticDispatchConstraint.SafeList.Serialized,
        nominal_decls: NominalDecl.SafeList.Serialized,
        nominal_decl_index: NominalDeclIndexEntry.SafeList.Serialized,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            // Serialize each component
            try self.slots.serialize(&store.slots, allocator, writer);
            try self.descs.serialize(&store.descs, allocator, writer);
            try self.vars.serialize(&store.vars, allocator, writer);
            try self.record_fields.serialize(&store.record_fields, allocator, writer);
            try self.tags.serialize(&store.tags, allocator, writer);
            try self.interpolation_parts.serialize(&store.interpolation_parts, allocator, writer);
            try self.static_dispatch_constraints.serialize(&store.static_dispatch_constraints, allocator, writer);
            try self.nominal_decls.serialize(&store.nominal_decls, allocator, writer);
            try self.nominal_decl_index.serialize(&store.nominal_decl_index, allocator, writer);

            // Set gpa to all zeros; the space needs to be here,
            // but the value will be set separately during deserialization.
            self.gpa = .{ 0, 0 };
        }

        /// Deserialize into a Store value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        /// WARNING: The returned Store points into the cache buffer and CANNOT be mutated.
        /// Use deserializeWithCopy() if the store needs to be mutable.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize, gpa: Allocator) Store {
            return Store{
                .gpa = gpa,
                .slots = self.slots.deserializeInto(base_addr),
                .descs = self.descs.deserializeInto(base_addr),
                .vars = self.vars.deserializeInto(base_addr),
                .record_fields = self.record_fields.deserializeInto(base_addr),
                .tags = self.tags.deserializeInto(base_addr),
                .interpolation_parts = self.interpolation_parts.deserializeInto(base_addr),
                .static_dispatch_constraints = self.static_dispatch_constraints.deserializeInto(base_addr),
                .nominal_decls = self.nominal_decls.deserializeInto(base_addr),
                .nominal_decl_index = self.nominal_decl_index.deserializeInto(base_addr),
            };
        }

        /// Deserialize into a Store value with fresh memory allocation.
        /// The returned Store owns its memory and can be safely grown/mutated.
        pub fn deserializeWithCopy(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!Store {
            return Store{
                .gpa = gpa,
                .slots = try self.slots.deserializeWithCopy(base_addr, gpa),
                .descs = try self.descs.deserializeWithCopy(base_addr, gpa),
                .vars = try self.vars.deserializeWithCopy(base_addr, gpa),
                .record_fields = try self.record_fields.deserializeWithCopy(base_addr, gpa),
                .tags = try self.tags.deserializeWithCopy(base_addr, gpa),
                .interpolation_parts = try self.interpolation_parts.deserializeWithCopy(base_addr, gpa),
                .static_dispatch_constraints = try self.static_dispatch_constraints.deserializeWithCopy(base_addr, gpa),
                .nominal_decls = try self.nominal_decls.deserializeWithCopy(base_addr, gpa),
                .nominal_decl_index = try self.nominal_decl_index.deserializeWithCopy(base_addr, gpa),
            };
        }
    };

    /// Serialize this Store to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Self);

        // Then serialize each component and update the struct
        offset_self.* = .{
            .gpa = allocator,
            .slots = (try self.slots.serialize(allocator, writer)).*,
            .descs = (try self.descs.serialize(allocator, writer)).*,
            .vars = (try self.vars.serialize(allocator, writer)).*,
            .record_fields = (try self.record_fields.serialize(allocator, writer)).*,
            .tags = (try self.tags.serialize(allocator, writer)).*,
            .interpolation_parts = (try self.interpolation_parts.serialize(allocator, writer)).*,
            .static_dispatch_constraints = (try self.static_dispatch_constraints.serialize(allocator, writer)).*,
            .nominal_decls = (try self.nominal_decls.serialize(allocator, writer)).*,
            .nominal_decl_index = (try self.nominal_decl_index.serialize(allocator, writer)).*,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.slots.relocate(offset);
        self.descs.relocate(offset);
        self.vars.relocate(offset);
        self.record_fields.relocate(offset);
        self.tags.relocate(offset);
        self.interpolation_parts.relocate(offset);
        self.static_dispatch_constraints.relocate(offset);
        self.nominal_decls.relocate(offset);
        self.nominal_decl_index.relocate(offset);
    }
};

/// Represents a store of slots
const SlotStore = struct {
    const Self = @This();

    backing: collections.SafeList(Slot),

    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .backing = try collections.SafeList(Slot).initCapacity(gpa, capacity) };
    }

    fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Serialized representation of SlotStore
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        backing: collections.SafeList(Slot).Serialized,

        /// Serialize a SlotStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            slot_store: *const SlotStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.backing.serialize(&slot_store.backing, allocator, writer);
        }

        /// Deserialize into a SlotStore value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize) SlotStore {
            return SlotStore{
                .backing = self.backing.deserializeInto(base_addr),
            };
        }

        /// Deserialize into a SlotStore value with fresh memory allocation.
        /// The returned SlotStore owns its memory and can be safely grown/mutated.
        pub fn deserializeWithCopy(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!SlotStore {
            return SlotStore{
                .backing = try self.backing.deserializeWithCopy(base_addr, gpa),
            };
        }
    };

    /// Insert a new slot into the store
    fn insert(self: *Self, gpa: Allocator, typ: Slot) std.mem.Allocator.Error!Idx {
        const safe_idx = try self.backing.append(gpa, typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Insert a value into the store assuming there is capacity
    fn appendAssumeCapacity(self: *Self, typ: Slot) Idx {
        const safe_idx = self.backing.appendAssumeCapacity(typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Set a value in the store
    pub fn set(self: *Self, idx: Idx, val: Slot) void {
        self.backing.set(@enumFromInt(@intFromEnum(idx)), val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Slot {
        return self.backing.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    /// Serialize this SlotStore to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // Since SlotStore is just a wrapper around SafeList, serialize the backing directly
        const serialized_backing = try self.backing.serialize(allocator, writer);
        // Cast the serialized SafeList pointer to a SlotStore pointer
        return @ptrCast(serialized_backing);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.backing.relocate(offset);
    }

    /// Calculate the size needed to serialize this SlotStore
    fn serializedSize(self: *const Self) usize {
        return self.backing.serializedSize();
    }

    /// Deserialize a SlotStore from the provided buffer
    fn deserializeFrom(buffer: []align(@alignOf(Slot)) const u8, allocator: Allocator) Allocator.Error!Self {
        return .{
            .backing = try collections.SafeList(Slot).deserializeFrom(buffer, allocator),
        };
    }

    /// A type-safe index into the store
    const Idx = enum(u32) {
        first = 0,
        _,
    };
};

/// Represents a store of descriptors
///
/// Indexes into the list are typesafe
const DescStore = struct {
    const Self = @This();
    const DescSafeMultiList = collections.SafeMultiList(Desc);

    backing: DescSafeMultiList,

    /// Init & allocated memory
    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .backing = try DescSafeMultiList.initCapacity(gpa, capacity) };
    }

    /// Deinit & free allocated memory
    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Serialized representation of DescStore
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        backing: DescSafeMultiList.Serialized,

        /// Serialize a DescStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            desc_store: *const DescStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.backing.serialize(&desc_store.backing, allocator, writer);
        }

        /// Deserialize into a DescStore value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize) DescStore {
            return DescStore{
                .backing = self.backing.deserializeInto(base_addr),
            };
        }

        /// Deserialize into a DescStore value with fresh memory allocation.
        /// The returned DescStore owns its memory and can be safely grown/mutated.
        pub fn deserializeWithCopy(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!DescStore {
            return DescStore{
                .backing = try self.backing.deserializeWithCopy(base_addr, gpa),
            };
        }
    };

    /// Insert a value into the store
    fn insert(self: *Self, gpa: Allocator, typ: Desc) std.mem.Allocator.Error!Idx {
        const safe_idx = try self.backing.append(gpa, typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Appends a value to the store assuming there is capacity
    fn appendAssumeCapacity(self: *Self, typ: Desc) Idx {
        const safe_idx = self.backing.appendAssumeCapacity(typ);
        return @enumFromInt(@intFromEnum(safe_idx));
    }

    /// Set a value in the store
    fn set(self: *Self, idx: Idx, val: Desc) void {
        self.backing.set(@enumFromInt(@intFromEnum(idx)), val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Desc {
        return self.backing.get(@enumFromInt(@intFromEnum(idx)));
    }

    /// Serialize this DescStore to the given CompactWriter
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Self {
        // Since DescStore is just a wrapper around SafeMultiList, serialize the backing directly
        const serialized_backing = try self.backing.serialize(allocator, writer);
        // Cast the serialized SafeMultiList pointer to a DescStore pointer
        return @ptrCast(serialized_backing);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Self, offset: isize) void {
        self.backing.relocate(offset);
    }

    /// Calculate the size needed to serialize this DescStore
    pub fn serializedSize(self: *const Self) usize {
        return self.backing.serializedSize();
    }

    /// Deserialize a DescStore from the provided buffer
    pub fn deserializeFrom(buffer: []align(@alignOf(Desc)) const u8, allocator: Allocator) Allocator.Error!Self {
        const backing = try DescSafeMultiList.deserializeFrom(buffer, allocator);
        return Self{ .backing = backing };
    }

    /// A type-safe index into the store
    /// This type is made public below
    const Idx = enum(u32) {
        first = 0,
        _,
    };
};

/// An index into the desc store
pub const DescStoreIdx = DescStore.Idx;

// path compression

test "resolveVarAndCompressPath - flattens redirect chain to flex" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const c = try store.fresh();
    const b = try store.freshRedirect(c);
    const a = try store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}

test "dangerousSetVarRedirect requires a declared rule by signature" {
    // Zig has no negative-compile test harness, so the "an unreasoned call
    // does not build" guarantee is pinned by reflection: the signature must
    // take a `RedirectRule` before the two vars, and the enum must stay
    // exhaustive so only declared members can be passed. Removing the rule
    // parameter fails this test.
    const fn_info = @typeInfo(@TypeOf(Store.dangerousSetVarRedirect)).@"fn";
    try std.testing.expectEqual(4, fn_info.params.len);
    try std.testing.expectEqual(Store.RedirectRule, fn_info.params[1].type.?);
    comptime std.debug.assert(@typeInfo(Store.RedirectRule).@"enum".is_exhaustive);
}

test "savepoint clone cross-check is compiled in for test builds" {
    try std.testing.expect(savepoint_verification == .clone_crosscheck);
}

test "savepoint trail is byte-for-byte identical to a full store copy+rollback" {
    const gpa = std.testing.allocator;

    // A few independent runs with different mutation mixes.
    var run: usize = 0;
    while (run < 4) : (run += 1) {
        var store = try Store.init(gpa);
        defer store.deinit();

        // Pre-savepoint content: a handful of vars, some redirected/unioned.
        const a = try store.fresh();
        const b = try store.fresh();
        _ = try store.freshRedirect(b);
        try store.union_(a, b, .{ .content = .err, .rank = Rank.generalized });

        // Independent oracle: keep our own copy of the pre-savepoint slots/descs
        // to compare against after rollback, alongside the verifying savepoint's
        // internal cross-check.
        const before_slots = try gpa.dupe(Slot, store.slots.backing.items.items);
        defer gpa.free(before_slots);
        var before_descs = try store.descs.backing.items.clone(gpa);
        defer before_descs.deinit(gpa);
        const before_vars_len = store.vars.items.items.len;

        // Verifying savepoint: copies the whole store up front; rollback asserts
        // the trail restored it byte-for-byte (same semantics as restoring a copy).
        var sp = try store.createSavepointVerifying();

        // Mutations a probe might do, varied per run. These exercise: appends
        // (fresh/register), in-place writes to pre-existing entries (union_,
        // setVarContent, setDescRank), the same entry written twice (reverse
        // replay), and the compression path (a no-op while a savepoint is open).
        const fresh1 = try store.fresh();
        const fresh2 = try store.register(.{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        try store.union_(fresh1, fresh2, .{ .content = .err, .rank = Rank.outermost });
        try store.setVarContent(a, .{ .flex = Flex.init() });
        try store.setVarContent(a, .err);
        if (run % 2 == 0) try store.setDescRank(store.resolveVar(b).desc_idx, Rank.outermost);
        _ = store.resolveVarAndCompressPath(a);

        store.rollbackToSavepoint(&sp);

        // The store must be byte-identical to its pre-savepoint state.
        try std.testing.expect(!store.savepoint_active);
        try std.testing.expectEqual(before_slots.len, store.slots.backing.items.items.len);
        for (before_slots, store.slots.backing.items.items) |x, y| {
            try std.testing.expect(std.meta.eql(x, y));
        }
        try std.testing.expectEqual(before_descs.len, store.descs.backing.items.len);
        var i: usize = 0;
        while (i < before_descs.len) : (i += 1) {
            try std.testing.expect(std.meta.eql(before_descs.get(i), store.descs.backing.items.get(i)));
        }
        try std.testing.expectEqual(before_vars_len, store.vars.items.items.len);
    }
}

test "createSavepointVerifying cross-checks a probe-unify against a full copy" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    // A small typed environment a real probe would unify against.
    const a = try store.fresh();
    const b = try store.fresh();
    try store.union_(a, b, .{ .content = .err, .rank = Rank.generalized });

    // A probe brackets a trial unification it always discards. The verifying
    // savepoint copies the store up front; on rollback its internal cross-check
    // asserts the trail put the store back byte-for-byte — exactly as if we had
    // restored the full copy.
    var sp = try store.createSavepointVerifying();
    const c = try store.fresh();
    try store.union_(a, c, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try store.setVarContent(b, .err);
    _ = store.resolveVarAndCompressPath(a);
    store.rollbackToSavepoint(&sp);

    try std.testing.expect(!store.savepoint_active);
}

test "Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create an empty Store
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_empty_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try original.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.len());
}

test "Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create original Store and add some types
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create some type variables
    const flex = try original.fresh();
    const rigid = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) });

    // Create a redirect
    const redirect_var = try original.freshRedirect(flex);

    // Verify original values
    const flex_resolved = original.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const rigid_resolved = original.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, rigid_resolved.desc.content);

    const redirect_resolved = original.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_basic_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try original.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the types are accessible
    try std.testing.expectEqual(@as(usize, 3), deserialized.len());

    const deser_flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, deser_flex_resolved.desc.content);

    const deser_rigid_resolved = deserialized.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, deser_rigid_resolved.desc.content);

    const deser_redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(deser_flex_resolved.desc_idx, deser_redirect_resolved.desc_idx);
}

fn testNominalDecl(origin_module: base.ModuleIdentity.Idx, statement: u32, backing: Var) error{OutOfMemory}!NominalDecl {
    return NominalDecl{
        .ident = .{ .ident_idx = @bitCast(@as(u32, 1)) },
        .origin_module = origin_module,
        .source = try NominalType.Source.initChecked(
            try SourceDecl.fromStatementChecked(statement),
            false,
            false,
        ),
        .formals = Var.SafeList.Range.empty(),
        .backing = backing,
        .flags = .{ .valid = true },
    };
}

test "nominal declaration table: register, lookup, upsert" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const backing_a = try store.fresh();
    const backing_b = try store.fresh();
    const backing_c = try store.fresh();

    const origin_0: base.ModuleIdentity.Idx = @enumFromInt(1);
    const origin_1: base.ModuleIdentity.Idx = @enumFromInt(2);

    // Register out of key order to exercise sorted insertion.
    const idx_b = try store.registerNominalDecl(try testNominalDecl(origin_1, 5, backing_b));
    const idx_a = try store.registerNominalDecl(try testNominalDecl(origin_0, 9, backing_a));
    const idx_c = try store.registerNominalDecl(try testNominalDecl(origin_1, 2, backing_c));

    try std.testing.expectEqual(@as(u64, 3), store.nominalDeclCount());
    try std.testing.expectEqual(idx_a, store.lookupNominalDeclByKey(origin_0, 9).?);
    try std.testing.expectEqual(idx_b, store.lookupNominalDeclByKey(origin_1, 5).?);
    try std.testing.expectEqual(idx_c, store.lookupNominalDeclByKey(origin_1, 2).?);
    try std.testing.expectEqual(@as(?NominalDecl.Idx, null), store.lookupNominalDeclByKey(origin_0, 5));
    try std.testing.expectEqual(@as(?NominalDecl.Idx, null), store.lookupNominalDeclByKey(origin_1, 9));

    try std.testing.expectEqual(backing_a, store.getNominalDecl(idx_a).backing);

    // Re-registering the same key updates in place and keeps the index stable.
    var updated = try testNominalDecl(origin_0, 9, backing_c);
    const formal = try store.fresh();
    updated.formals = try store.appendVars(&.{formal});
    const idx_a_again = try store.registerNominalDecl(updated);
    try std.testing.expectEqual(idx_a, idx_a_again);
    try std.testing.expectEqual(@as(u64, 3), store.nominalDeclCount());
    try std.testing.expectEqual(backing_c, store.getNominalDecl(idx_a).backing);
    try std.testing.expectEqual(@as(u32, 1), store.getNominalDecl(idx_a).formals.count);

    // Validity flips in place.
    try std.testing.expect(store.getNominalDecl(idx_b).isValid());
    store.markNominalDeclInvalid(idx_b);
    try std.testing.expect(!store.getNominalDecl(idx_b).isValid());

    // Lookup through a nominal application resolves by (origin, statement).
    const app_content = try store.mkNominalWithSourceDecl(
        .{ .ident_idx = @bitCast(@as(u32, 1)) },
        &.{},
        origin_1,
        5,
        false,
    );
    const app = app_content.structure.nominal_type;
    try std.testing.expectEqual(idx_b, store.lookupNominalDecl(app).?);
}

test "nominal declaration table: CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    const formal = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 7))) });
    const backing = try original.freshFromContent(Content{ .structure = .empty_record });

    const origin: base.ModuleIdentity.Idx = @enumFromInt(3);
    var decl = try testNominalDecl(origin, 11, backing);
    decl.formals = try original.appendVars(&.{formal});
    _ = try original.registerNominalDecl(decl);

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_nominal_decls.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);
    try writer.writeGather(file, io);

    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    try std.testing.expectEqual(@as(u64, 1), deserialized.nominalDeclCount());
    const deser_idx = deserialized.lookupNominalDeclByKey(origin, 11).?;
    const deser_decl = deserialized.getNominalDecl(deser_idx);
    try std.testing.expectEqual(backing, deser_decl.backing);
    try std.testing.expect(deser_decl.isValid());
    const deser_formals = deserialized.sliceVars(deser_decl.formals);
    try std.testing.expectEqual(@as(usize, 1), deser_formals.len);
    try std.testing.expectEqual(formal, deser_formals[0]);
}

test "Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;
    var idents = try base.Ident.Store.initCapacity(gpa, 10);
    defer idents.deinit(gpa);

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create various types
    const flex = try original.fresh();
    const str_var = try original.freshFromContent(Content{ .structure = .empty_record });
    const list_elem = try original.fresh();
    const list_ident_idx = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 999 };
    const builtin_module_idx = base.ModuleIdentity.Idx.NONE;
    const list_content = try original.mkNominal(
        .{ .ident_idx = list_ident_idx },
        &[_]Var{list_elem},
        builtin_module_idx,
        false,
    );
    const list_var = try original.freshFromContent(list_content);

    // Create a function type
    const arg1 = try original.fresh();
    const arg2 = try original.fresh();
    const ret = try original.fresh();
    const func_content = try original.mkFuncPure(&[_]Var{ arg1, arg2 }, ret);
    const func_var = try original.freshFromContent(func_content);

    // Create a record type
    const field1_var = try original.fresh();
    const field2_var = try original.fresh();
    const record_fields = try original.appendRecordFields(&[_]RecordField{
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 100 }, .var_ = field1_var },
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 200 }, .var_ = field2_var },
    });
    const record_ext = try original.fresh();
    const record_content = Content{ .structure = .{ .record = .{ .fields = record_fields, .ext = record_ext } } };
    const record_var = try original.freshFromContent(record_content);

    // Create a tag union
    const tag1 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 300 }, &[_]Var{flex});
    const tag2 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 400 }, &[_]Var{ arg1, arg2 });
    const tag_union_ext = try original.fresh();
    const tag_union_content = try original.mkTagUnion(&[_]Tag{ tag1, tag2 }, tag_union_ext);
    const tag_union_var = try original.freshFromContent(tag_union_content);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_comprehensive_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    const serialized = try original.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate - Store is at the beginning of the buffer
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all types
    const deser_str = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, deser_str.desc.content);

    const deser_list = deserialized.resolveVar(list_var);
    // List is a nominal type
    try std.testing.expect(deser_list.desc.content.structure == .nominal_type);
    const deser_nominal = deser_list.desc.content.structure.nominal_type;
    const deser_list_args = deserialized.sliceNominalArgs(deser_nominal);
    try std.testing.expectEqual(list_elem, deser_list_args[0]);

    const deser_func = deserialized.resolveVar(func_var);
    switch (deser_func.desc.content.structure) {
        .fn_pure => |func| {
            const args = deserialized.sliceVars(func.args);
            try std.testing.expectEqual(@as(usize, 2), args.len);
            try std.testing.expectEqual(arg1, args[0]);
            try std.testing.expectEqual(arg2, args[1]);
            try std.testing.expectEqual(ret, func.ret);
        },
        else => unreachable,
    }

    const deser_record = deserialized.resolveVar(record_var);
    switch (deser_record.desc.content.structure) {
        .record => |record| {
            const fields_slice = deserialized.getRecordFieldsSlice(record.fields);
            try std.testing.expectEqual(@as(usize, 2), fields_slice.len);
            try std.testing.expectEqual(@as(u29, 100), fields_slice.items(.name)[0].idx);
            try std.testing.expectEqual(@as(u29, 200), fields_slice.items(.name)[1].idx);
            try std.testing.expectEqual(field1_var, fields_slice.items(.var_)[0]);
            try std.testing.expectEqual(field2_var, fields_slice.items(.var_)[1]);
            try std.testing.expectEqual(record_ext, record.ext);
        },
        else => unreachable,
    }

    const deser_tag_union = deserialized.resolveVar(tag_union_var);
    switch (deser_tag_union.desc.content.structure) {
        .tag_union => |tag_union| {
            const tags_slice = deserialized.getTagsSlice(tag_union.tags);
            try std.testing.expectEqual(@as(usize, 2), tags_slice.len);
            try std.testing.expectEqual(@as(u29, 300), tags_slice.items(.name)[0].idx);
            try std.testing.expectEqual(@as(u29, 400), tags_slice.items(.name)[1].idx);

            const tag1_args = deserialized.sliceVars(tags_slice.items(.args)[0]);
            try std.testing.expectEqual(@as(usize, 1), tag1_args.len);
            try std.testing.expectEqual(flex, tag1_args[0]);

            const tag2_args = deserialized.sliceVars(tags_slice.items(.args)[1]);
            try std.testing.expectEqual(@as(usize, 2), tag2_args.len);
            try std.testing.expectEqual(arg1, tag2_args[0]);
            try std.testing.expectEqual(arg2, tag2_args[1]);

            try std.testing.expectEqual(tag_union_ext, tag_union.ext);
        },
        else => unreachable,
    }
}

test "SlotStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Use a real Store to get real Var and DescStore.Idx values
    var store = try Store.init(gpa);
    defer store.deinit();

    // Create real type variables - fresh() creates a flex var with a root slot
    const var_a = try store.fresh();
    const var_b = try store.fresh();
    const var_c = try store.fresh();

    // Get the DescStore.Idx from the root slots
    const desc_idx_a = store.getSlot(var_a).root;
    const desc_idx_c = store.getSlot(var_c).root;

    // Create a separate SlotStore for serialization testing
    var slot_store = try SlotStore.init(gpa, 4);
    defer slot_store.deinit(gpa);

    // Add slots and capture returned indices
    const slot_a = try slot_store.insert(gpa, .{ .root = desc_idx_a });
    const slot_b = try slot_store.insert(gpa, .{ .redirect = var_b });
    const slot_c = try slot_store.insert(gpa, .{ .root = desc_idx_c });

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_slot_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using SlotStore.Serialized with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, SlotStore.Serialized);
    try serialized_ptr.serialize(&slot_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*SlotStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify using captured indices
    try std.testing.expectEqual(@as(u64, 3), deserialized.backing.len());
    try std.testing.expectEqual(Slot{ .root = desc_idx_a }, deserialized.get(slot_a));
    try std.testing.expectEqual(Slot{ .redirect = var_b }, deserialized.get(slot_b));
    try std.testing.expectEqual(Slot{ .root = desc_idx_c }, deserialized.get(slot_c));
}

test "DescStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var desc_store = try DescStore.init(gpa, 4);
    defer desc_store.deinit(gpa);

    // Add some descriptors and capture returned indices
    const desc1 = Descriptor{
        .content = Content{ .flex = Flex.init() },
        .rank = Rank.generalized,
    };
    const desc2 = Descriptor{
        .content = Content{ .structure = .empty_record },
        .rank = Rank.outermost,
    };

    const desc_idx_1 = try desc_store.insert(gpa, desc1);
    const desc_idx_2 = try desc_store.insert(gpa, desc2);

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_desc_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using DescStore.Serialized with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, DescStore.Serialized);
    try serialized_ptr.serialize(&desc_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*DescStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr));
    // Note: deserialize already handles relocation, don't call relocate again

    // Verify using captured indices
    try std.testing.expectEqual(@as(usize, 2), deserialized.backing.items.len);
    try std.testing.expectEqual(desc1, deserialized.get(desc_idx_1));
    try std.testing.expectEqual(desc2, deserialized.get(desc_idx_2));
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var store = try Store.init(gpa);
    defer store.deinit();

    // Create some type variables
    const flex = try store.fresh();
    const str_var = try store.freshFromContent(Content{ .structure = .empty_record });
    const redirect_var = try store.freshRedirect(flex);

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using Store.Serialized
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    const serialized_ptr = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized_ptr.serialize(&store, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - Store.Serialized is at the beginning of the buffer
    const deser_ptr = @as(*Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);

    // Verify the store was deserialized correctly
    try std.testing.expectEqual(@as(usize, 3), deserialized.len());

    const flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const str_resolved = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, str_resolved.desc.content);

    const redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);
}

test "Store multiple instances CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create multiple stores
    var store1 = try Store.init(gpa);
    defer store1.deinit();

    var store2 = try Store.init(gpa);
    defer store2.deinit();

    var store3 = try Store.init(gpa);
    defer store3.deinit();

    // Populate differently
    const var1_1 = try store1.fresh();
    const var1_2 = try store1.freshFromContent(Content{ .structure = .empty_record });
    const redirect1_1 = try store1.freshRedirect(var1_1);
    try std.testing.expectEqual(Slot{ .redirect = var1_1 }, store1.getSlot(redirect1_1));

    const var2_1 = try store2.fresh();
    const var2_2 = try store2.fresh();
    const func_content = try store2.mkFuncEffectful(&[_]Var{var2_1}, var2_2);
    const func_var = try store2.freshFromContent(func_content);
    try std.testing.expect(store2.resolveVar(func_var).desc.content.unwrapFunc() != null);

    // store3 left empty

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_multiple_stores.dat", .{ .read = true });
    defer file.close(io);

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    const offset1 = writer.total_bytes; // Store1 starts at current position
    const serialized1 = try store1.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized1) != 0);

    const offset2 = writer.total_bytes; // Store2 starts at current position
    const serialized2 = try store2.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized2) != 0);

    const offset3 = writer.total_bytes; // Store3 starts at current position
    const serialized3 = try store3.serialize(gpa, &writer);
    try std.testing.expect(@intFromPtr(serialized3) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate all three
    const deserialized1 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify store 1
    try std.testing.expectEqual(@as(usize, 3), deserialized1.len());
    const deser1_var2 = deserialized1.resolveVar(var1_2);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, deser1_var2.desc.content);

    // Verify store 2
    try std.testing.expectEqual(@as(usize, 3), deserialized2.len());

    // Verify store 3 (empty)
    try std.testing.expectEqual(@as(usize, 0), deserialized3.len());
}

test "source declaration overflow is rejected before mutating type store" {
    const gpa = std.testing.allocator;

    var store = try Store.initCapacity(gpa, 1, 1);
    defer store.deinit();

    const before_slots = store.len();
    const before_descs = store.descs.backing.len();
    const before_vars = store.vars.len();
    const unread_backing_var: Var = undefined; // source declaration validation returns before reading this value

    try std.testing.expectError(
        error.OutOfMemory,
        store.mkAliasWithSourceDecl(
            .{ .ident_idx = base.Ident.Idx.NONE },
            unread_backing_var,
            &.{},
            base.ModuleIdentity.Idx.NONE,
            SourceDecl.max_statement + 1,
        ),
    );
    try std.testing.expectEqual(before_slots, store.len());
    try std.testing.expectEqual(before_descs, store.descs.backing.len());
    try std.testing.expectEqual(before_vars, store.vars.len());

    try std.testing.expectError(
        error.OutOfMemory,
        store.mkNominalWithSourceDecl(
            .{ .ident_idx = base.Ident.Idx.NONE },
            &.{},
            base.ModuleIdentity.Idx.NONE,
            NominalType.Source.max_statement + 1,
            false,
        ),
    );
    try std.testing.expectEqual(before_slots, store.len());
    try std.testing.expectEqual(before_descs, store.descs.backing.len());
    try std.testing.expectEqual(before_vars, store.vars.len());
}
