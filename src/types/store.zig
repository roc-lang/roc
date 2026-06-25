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
const Record = types.Record;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const SourceDecl = types.SourceDecl;

const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

/// A variable & its descriptor info
pub const ResolvedVarDesc = struct {
    var_: Var,
    is_root: bool,
    desc_idx: DescStore.Idx,
    desc: Desc,
};

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

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
    static_dispatch_constraints: StaticDispatchConstraint.SafeList,

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
            .static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, child_capacity),
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
        self.static_dispatch_constraints.deinit(self.gpa);

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
            .static_dispatch_constraints = try self.static_dispatch_constraints.clone(gpa),
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

    /// Set a type variable to redirect to the provided variables.
    /// During type-checking, you probably don't want to use this function.
    ///
    /// IMPORTANT: When using this function during type checking, it's possible
    /// to loose `rank` information! You should prefer to use regular `unify`
    /// over this function, which correctly propagates rank, unless you already
    /// know the two vars are of the same rank.
    pub fn dangerousSetVarRedirect(self: *Self, target_var: Var, redirect_to: Var) Allocator.Error!void {
        std.debug.assert(@intFromEnum(target_var) < self.len());
        std.debug.assert(@intFromEnum(redirect_to) < self.len());
        // Self-redirects cause infinite loops in resolveVar
        std.debug.assert(target_var != redirect_to);
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
        origin_module: base.Ident.Idx,
    ) std.mem.Allocator.Error!Content {
        return self.mkAliasWithSourceDecl(ident, backing_var, args, origin_module, null);
    }

    pub fn mkAliasWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.Ident.Idx,
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
        origin_module: base.Ident.Idx,
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

    /// Make nominal data type
    /// Does not insert content into the types store
    pub fn mkNominal(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.Ident.Idx,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDecl(ident, backing_var, args, origin_module, null, is_opaque);
    }

    pub fn mkNominalWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.Ident.Idx,
        source_decl: ?u32,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDeclAndBuiltinOrigin(
            ident,
            backing_var,
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
        backing_var: Var,
        args: []const Var,
        origin_module: base.Ident.Idx,
        source_decl: ?u32,
        is_opaque: bool,
        builtin_origin: bool,
    ) std.mem.Allocator.Error!Content {
        const source = try NominalType.Source.initChecked(
            try SourceDecl.fromOptionalWithBuiltinOriginChecked(source_decl, builtin_origin),
            is_opaque,
            builtin_origin,
        );
        const backing_idx = try self.appendVar(backing_var);
        var span = try self.appendVars(args);

        // Adjust args span to include backing  var
        span.start = backing_idx;
        span.count = span.count + 1;

        return Content{ .structure = FlatType{
            .nominal_type = NominalType{
                .ident = ident,
                .vars = .{ .nonempty = span },
                .origin_module = origin_module,
                .source = source,
            },
        } };
    }

    // Make a function data type with unbound effectfulness
    // Does not insert content into the types store.
    pub fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_unbound = .{
            .args = args_range,
            .ret = ret,
            .needs_instantiation = needs_inst,
        } } };
    }

    // Make a pure function data type (as opposed to an effectful or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_pure = .{
            .args = args_range,
            .ret = ret,
            .needs_instantiation = needs_inst,
        } } };
    }

    // Make an effectful function data type (as opposed to a pure or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);

        // Check if any arguments need instantiation
        var needs_inst = false;
        for (args) |arg| {
            if (self.needsInstantiation(arg)) {
                needs_inst = true;
                break;
            }
        }

        // Also check the return type
        if (!needs_inst) {
            needs_inst = self.needsInstantiation(ret);
        }

        return Content{ .structure = .{ .fn_effectful = .{
            .args = args_range,
            .ret = ret,
            .needs_instantiation = needs_inst,
        } } };
    }

    // Helper to check if a type variable needs instantiation
    pub fn needsInstantiation(self: *const Self, var_: Var) bool {
        const resolved = self.resolveVar(var_);

        // Generalized variables (rank 0) always need instantiation
        if (resolved.desc.rank == Rank.generalized) {
            return true;
        }

        return self.needsInstantiationContent(resolved.desc.content);
    }

    pub fn needsInstantiationContent(self: *const Self, content: Content) bool {
        return switch (content) {
            .flex => true, // Flexible variables need instantiation
            .rigid => true, // Rigid variables need instantiation when used outside their defining scope
            .alias => true, // Aliases may contain type variables, so assume they need instantiation
            .structure => |flat_type| self.needsInstantiationFlatType(flat_type),
            .err => false,
        };
    }

    pub fn needsInstantiationFlatType(self: *const Self, flat_type: FlatType) bool {
        return switch (flat_type) {
            .tuple => |tuple| blk: {
                const elems_slice = self.sliceVars(tuple.elems);
                for (elems_slice) |elem_var| {
                    if (self.needsInstantiation(elem_var)) break :blk true;
                }
                break :blk false;
            },
            .nominal_type => |nominal| blk: {
                const args = self.sliceNominalArgs(nominal);
                for (args) |arg_var| {
                    if (self.needsInstantiation(arg_var)) break :blk true;
                }
                break :blk false;
            },
            .fn_pure => |func| func.needs_instantiation,
            .fn_effectful => |func| func.needs_instantiation,
            .fn_unbound => |func| func.needs_instantiation,
            .record => |record| self.needsInstantiationRecord(record),
            .record_unbound => |fields| self.needsInstantiationRecordFields(fields),
            .empty_record => false,
            .tag_union => |tag_union| self.needsInstantiationTagUnion(tag_union),
            .empty_tag_union => false,
        };
    }

    pub fn needsInstantiationRecord(self: *const Self, record: Record) bool {
        const fields_slice = self.getRecordFieldsSlice(record.fields);
        for (fields_slice.items(.var_)) |type_var| {
            if (self.needsInstantiation(type_var)) return true;
        }
        return self.needsInstantiation(record.ext);
    }

    pub fn needsInstantiationRecordFields(self: *const Self, fields: RecordField.SafeMultiList.Range) bool {
        const fields_slice = self.getRecordFieldsSlice(fields);
        for (fields_slice.items(.var_)) |type_var| {
            if (self.needsInstantiation(type_var)) return true;
        }
        return false;
    }

    pub fn needsInstantiationTagUnion(self: *const Self, tag_union: TagUnion) bool {
        const tags_slice = self.getTagsSlice(tag_union.tags);
        for (tags_slice.items(.args)) |tag_args| {
            const args_slice = self.sliceVars(tag_args);
            for (args_slice) |arg_var| {
                if (self.needsInstantiation(arg_var)) return true;
            }
        }
        return self.needsInstantiation(tag_union.ext);
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

    // Nominal types contain a span of variables. In this span, the 1st element
    // is the backing variable, and the remainder are the arguments

    /// Get the backing var for this nominal type
    pub fn getNominalBackingVar(self: *const Self, nominal: NominalType) Var {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        return self.vars.get(nominal.vars.nonempty.start).*;
    }

    /// Get the arg vars for this nominal type
    pub fn sliceNominalArgs(self: *const Self, nominal: NominalType) []Var {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        const slice = self.vars.sliceRange(nominal.vars.nonempty);
        return slice[1..];
    }

    /// Get the arg vars range for this nominal type.
    /// Returns a range (start index + count) which can be stored safely.
    /// Unlike sliceNominalArgs, this returns indices that remain valid even if
    /// the underlying storage is reallocated.
    pub fn getNominalArgsRange(nominal: NominalType) VarSafeList.Range {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        var span = nominal.vars.nonempty;
        span.dropFirstElem();
        return span;
    }

    /// Get the an iterator arg vars for this nominal type
    pub fn iterNominalArgs(self: *const Self, nominal: NominalType) VarSafeList.Iterator {
        std.debug.assert(nominal.vars.nonempty.count > 0);
        var span = nominal.vars.nonempty;
        span.dropFirstElem();
        return self.vars.iterRange(span);
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
        static_dispatch_constraints: StaticDispatchConstraint.SafeList.Serialized,

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
            try self.static_dispatch_constraints.serialize(&store.static_dispatch_constraints, allocator, writer);

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
                .static_dispatch_constraints = self.static_dispatch_constraints.deserializeInto(base_addr),
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
                .static_dispatch_constraints = try self.static_dispatch_constraints.deserializeWithCopy(base_addr, gpa),
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
            .static_dispatch_constraints = (try self.static_dispatch_constraints.serialize(allocator, writer)).*,
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
        self.static_dispatch_constraints.relocate(offset);
    }

    /// Calculate the size needed to serialize this Store
    pub fn serializedSize(self: *const Self) usize {
        const slots_size = self.slots.serializedSize();
        const descs_size = self.descs.serializedSize();
        const record_fields_size = self.record_fields.serializedSize();
        const tags_size = self.tags.serializedSize();
        const vars_size = self.vars.serializedSize();
        const static_dispatch_constraints_size = self.static_dispatch_constraints.serializedSize();

        // Add alignment padding for each component
        var total_size: usize = @sizeOf(u32) * 6; // size headers
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        total_size += slots_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());

        total_size += descs_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());

        total_size += record_fields_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());

        total_size += tags_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());

        total_size += vars_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());

        total_size += static_dispatch_constraints_size;
        total_size = std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT.toByteUnits());
    }

    /// Deserialize a Store from the provided buffer
    pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) Allocator.Error!Self {
        if (buffer.len < @sizeOf(u32) * 6) return error.BufferTooSmall;

        var offset: usize = 0;

        // Read sizes
        const slots_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const descs_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const record_fields_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const tags_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const vars_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        const static_dispatch_constraints_size = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);

        // Deserialize data
        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const slots_buffer = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(buffer[offset .. offset + slots_size]));
        const slots = try SlotStore.deserializeFrom(slots_buffer, allocator);
        offset += slots_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const descs_buffer = @as([]align(@alignOf(Desc)) const u8, @alignCast(buffer[offset .. offset + descs_size]));
        const descs = try DescStore.deserializeFrom(descs_buffer, allocator);
        offset += descs_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const record_fields_buffer = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(buffer[offset .. offset + record_fields_size]));
        const record_fields = try RecordFieldSafeMultiList.deserializeFrom(record_fields_buffer, allocator);
        offset += record_fields_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const tags_buffer = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(buffer[offset .. offset + tags_size]));
        const tags = try TagSafeMultiList.deserializeFrom(tags_buffer, allocator);
        offset += tags_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const vars_buffer = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(buffer[offset .. offset + vars_size]));
        const vars = try VarSafeList.deserializeFrom(vars_buffer, allocator);
        offset += vars_size;

        offset = std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT.toByteUnits());
        const static_dispatch_constraints_buffer = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(buffer[offset .. offset + static_dispatch_constraints_size]));
        const static_dispatch_constraints = try StaticDispatchConstraint.SafeList.deserializeFrom(static_dispatch_constraints_buffer, allocator);
        offset += static_dispatch_constraints_size;

        return Self{
            .gpa = allocator,
            .slots = slots,
            .descs = descs,
            .record_fields = record_fields,
            .tags = tags,
            .vars = vars,
            .static_dispatch_constraints = static_dispatch_constraints,
        };
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
    const builtin_module_idx = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 998 };
    const list_content = try original.mkNominal(
        .{ .ident_idx = list_ident_idx },
        list_elem,
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

test "SlotStore and DescStore serialization and deserialization" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create several variables to populate SlotStore with roots
    const var1 = try original.freshFromContent(Content{ .flex = Flex.init() });
    const var2 = try original.freshFromContent(Content{ .structure = .empty_record });
    const var3 = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 123))) });

    // Create redirects to populate SlotStore with redirects
    const redirect1 = try original.freshRedirect(var1);
    const redirect2 = try original.freshRedirect(var2);
    const redirect3 = try original.freshRedirect(redirect1); // Chain of redirects

    // Verify SlotStore has both root and redirect entries
    try std.testing.expectEqual(@as(usize, 6), original.slots.backing.len());

    // Verify DescStore has the descriptors
    try std.testing.expectEqual(@as(usize, 3), original.descs.backing.items.len);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_explicit_stores.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate - Store struct is at the beginning of the buffer
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify SlotStore was correctly deserialized
    try std.testing.expectEqual(@as(usize, 6), deserialized.slots.backing.len());

    // Verify DescStore was correctly deserialized
    try std.testing.expectEqual(@as(usize, 3), deserialized.descs.backing.items.len);

    // Verify we can resolve variables correctly
    const resolved1 = deserialized.resolveVar(var1);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, resolved1.desc.content);

    const resolved2 = deserialized.resolveVar(var2);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, resolved2.desc.content);

    const resolved3 = deserialized.resolveVar(var3);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 123))) }, resolved3.desc.content);

    // Verify redirects work
    const resolved_redirect1 = deserialized.resolveVar(redirect1);
    try std.testing.expectEqual(resolved1.desc_idx, resolved_redirect1.desc_idx);

    const resolved_redirect3 = deserialized.resolveVar(redirect3);
    try std.testing.expectEqual(resolved1.desc_idx, resolved_redirect3.desc_idx);

    const resolved_redirect2 = deserialized.resolveVar(redirect2);
    try std.testing.expectEqual(resolved2.desc_idx, resolved_redirect2.desc_idx);
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
            base.Ident.Idx.NONE,
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
            unread_backing_var,
            &.{},
            base.Ident.Idx.NONE,
            NominalType.Source.max_statement + 1,
            false,
        ),
    );
    try std.testing.expectEqual(before_slots, store.len());
    try std.testing.expectEqual(before_descs, store.descs.backing.len());
    try std.testing.expectEqual(before_vars, store.vars.len());
}

test "Store with path compression CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create a redirect chain
    const c = try original.fresh();
    const b = try original.freshRedirect(c);
    const a = try original.freshRedirect(b);

    // Compress the path
    const resolved = original.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(c, resolved.var_);

    // Verify path is compressed
    try std.testing.expectEqual(Slot{ .redirect = c }, original.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, original.getSlot(b));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_compressed_store.dat", .{ .read = true });
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

    // Verify compressed paths are preserved
    try std.testing.expectEqual(Slot{ .redirect = c }, deserialized.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, deserialized.getSlot(b));
}
