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

    pub fn deinit(self: *Scratch, gpa: std.mem.Allocator) void {
        self.frames.deinit(gpa);
        self.value_stack.deinit(gpa);
        self.pending_tags.deinit(gpa);
        self.pending_fields.deinit(gpa);
        self.pending_constraints.deinit(gpa);
        self.pending_parts.deinit(gpa);
    }
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

/// State shared by every frame: the placeholder to fill and the descriptor
/// flag carried over from the source var.
const FillCommon = struct {
    fresh_var: Var,
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

    current_rank: Rank,
    rigid_behavior: RigidBehavior,
    rank_behavior: RankBehavior = .respect_rank,
    /// A rank-1 scheme can contain quantified leaves below monomorphic
    /// structural nodes. While instantiating such a scheme, copy that complete
    /// structural spine so the walk reaches every generalized descendant;
    /// monomorphic flex/rigid leaves remain shared.
    copy_scheme_structure: bool = false,

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

    const Self = @This();

    fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.idents.getText(idx);
    }

    fn scratch(self: *Self) *Scratch {
        return &self.store.instantiate_scratch;
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
        errdefer {
            machine.frames.items.len = frames_base;
            machine.value_stack.items.len = values_base;
            machine.pending_tags.items.len = tags_base;
            machine.pending_fields.items.len = fields_base;
            machine.pending_constraints.items.len = constraints_base;
            machine.pending_parts.items.len = parts_base;
        }

        if (!try self.requestVar(initial_var, force_root_copy)) {
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
        return machine.value_stack.pop().?;
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
    ) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        const resolved = self.store.resolveVar(initial_var);
        const resolved_var = resolved.var_;

        // Ordinary instantiation shares every non-generalized var. A binding
        // explicitly classified as a scheme instead copies non-generalized
        // structural nodes so generalized leaves at arbitrary depth remain
        // reachable, while preserving the identity of monomorphic leaves.
        if (!force_root_copy and self.rank_behavior == .respect_rank and resolved.desc.rank != .generalized) {
            const copy_structure = self.copy_scheme_structure and switch (resolved.desc.content) {
                .alias, .structure => true,
                .flex, .rigid, .field_presence, .err => false,
            };
            if (!copy_structure) {
                try machine.value_stack.append(self.store.gpa, resolved_var);
                return true;
            }
        }

        // Check if we've already instantiated this variable
        if (self.var_map.count() > 0) {
            if (self.var_map.get(resolved_var)) |fresh_var| {
                try machine.value_stack.append(self.store.gpa, fresh_var);
                return true;
            }
        }

        const empty_tag_union_is_default = resolved.desc.flags.empty_tag_union_is_default;
        switch (resolved.desc.content) {
            .rigid => |rigid| {
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

                            // Remember this substitution for recursive references
                            try self.var_map.put(resolved_var, existing_var);

                            try machine.value_stack.append(self.store.gpa, existing_var);
                            return true;
                        },
                        .substitute_rigids_fresh => |rigid_subs| {
                            if (rigid_subs.get(rigid.name)) |existing_var| {
                                try self.var_map.put(resolved_var, existing_var);
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
                try self.var_map.put(resolved_var, fresh_var);

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
                        .fresh_var = fresh_var,
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
                try self.var_map.put(resolved_var, fresh_var);

                if (flex.constraints.len() == 0) {
                    const fresh_content = Content{ .flex = Flex{ .name = flex.name, .constraints = StaticDispatchConstraint.SafeList.Range.empty() } };
                    try self.fillPlaceholder(fresh_var, fresh_content, empty_tag_union_is_default);
                    try machine.value_stack.append(self.store.gpa, fresh_var);
                    return true;
                }

                try machine.frames.append(self.store.gpa, .{ .flex_like = .{
                    .common = .{
                        .fresh_var = fresh_var,
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
                try self.var_map.put(resolved_var, fresh_var);

                var arg_span = alias.vars.nonempty;
                arg_span.dropFirstElem();
                try machine.frames.append(self.store.gpa, .{ .alias = .{
                    .common = .{
                        .fresh_var = fresh_var,
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
                try self.var_map.put(resolved_var, fresh_var);
                try self.fillPlaceholder(fresh_var, .{ .field_presence = field_presence }, empty_tag_union_is_default);
                try machine.value_stack.append(self.store.gpa, fresh_var);
                return true;
            },
            .structure => |flat_type| {
                const fresh_var = try self.store.fresh();
                try self.var_map.put(resolved_var, fresh_var);

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
                                .fresh_var = fresh_var,
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
                                .fresh_var = fresh_var,
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
                                .fresh_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .pure,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .fn_effectful => |func| {
                        try machine.frames.append(self.store.gpa, .{ .func = .{
                            .common = .{
                                .fresh_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .effectful,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .fn_unbound => |func| {
                        try machine.frames.append(self.store.gpa, .{ .func = .{
                            .common = .{
                                .fresh_var = fresh_var,
                                .empty_tag_union_is_default = empty_tag_union_is_default,
                            },
                            .func = func,
                            .kind = .unbound,
                            .vars_base = @intCast(machine.value_stack.items.len),
                        } });
                        return false;
                    },
                    .record => |record| {
                        try machine.frames.append(self.store.gpa, .{ .record = .{
                            .common = .{
                                .fresh_var = fresh_var,
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
                                .fresh_var = fresh_var,
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
                                .fresh_var = fresh_var,
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
                try self.var_map.put(resolved_var, fresh_var);
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
        try self.fillPlaceholder(common.fresh_var, content, common.empty_tag_union_is_default);
        try self.scratch().value_stack.append(self.store.gpa, common.fresh_var);
    }

    // IMPORTANT for every step function below: source runs (vars, record
    // fields, tags, constraints, interpolation parts) must be re-fetched by
    // raw index on each visit, never held as slices. Child copies append to
    // the same backing arrays, which may reallocate and invalidate any held
    // slice. Source entries are append-only, so index-based re-fetching
    // always sees the original values.

    fn stepFlexLike(self: *Self, frame: *FlexLikeFrame) std.mem.Allocator.Error!bool {
        const machine = self.scratch();
        while (true) {
            switch (frame.stage) {
                .dispatch_fn => {
                    if (frame.cons_idx == frame.cons_len) {
                        const fresh_range = try self.store.appendStaticDispatchConstraints(
                            machine.pending_constraints.items[frame.cons_base..],
                        );
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
                    if (!try self.requestVar(constraint.fn_var, false)) return false;
                },
                .await_fn => {
                    frame.fresh_fn_var = machine.value_stack.pop().?;
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    if (!constraint.interpolation.isPresent()) {
                        var fresh_constraint = constraint;
                        fresh_constraint.fn_var = frame.fresh_fn_var;
                        try machine.pending_constraints.append(self.store.gpa, fresh_constraint);
                        frame.cons_idx += 1;
                        frame.stage = .dispatch_fn;
                    } else {
                        frame.parts_base = @intCast(machine.pending_parts.items.len);
                        frame.part_idx = 0;
                        frame.stage = .dispatch_part_or_item;
                    }
                },
                .dispatch_part_or_item => {
                    const constraint = self.store.static_dispatch_constraints.items.items[frame.cons_start + frame.cons_idx];
                    const metadata = constraint.interpolation;
                    if (frame.part_idx == metadata.interpolated_parts.len()) {
                        frame.stage = .await_item;
                        if (!try self.requestVar(metadata.item_var, false)) return false;
                    } else {
                        const part = self.store.getInterpolationPartAt(metadata.interpolated_parts, frame.part_idx);
                        frame.stage = .await_part;
                        if (!try self.requestVar(part.var_, false)) return false;
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
                    fresh_constraint.interpolation = .{
                        .expr_region = constraint.interpolation.expr_region,
                        .item_var = fresh_item_var,
                        .interpolated_parts = fresh_parts_range,
                    };
                    try machine.pending_constraints.append(self.store.gpa, fresh_constraint);
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
                if (!try self.requestVar(arg_var, false)) return false;
                continue;
            }
            if (arrived == frame.args_count) {
                const backing_var = self.store.getAliasBackingVar(frame.alias);
                if (!try self.requestVar(backing_var, false)) return false;
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
                if (!try self.requestVar(elem_var, false)) return false;
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
                if (!try self.requestVar(arg_var, false)) return false;
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
                if (!try self.requestVar(arg_var, false)) return false;
                continue;
            }
            if (arrived == args_count) {
                if (!try self.requestVar(frame.func.ret, false)) return false;
                continue;
            }
            if (arrived < args_count + 1 + deps_count) {
                const dep_var = self.store.vars.items.items[@intFromEnum(frame.func.effect_deps.start) + (arrived - args_count - 1)];
                if (!try self.requestVar(dep_var, false)) return false;
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
                        const child_var = switch (frame.field_axis) {
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
                        if (!try self.requestVar(child_var, false)) return false;
                        continue;
                    }
                    frame.fields_range = try self.appendFreshRecordFields(frame.source_fields, frame.vars_base);
                    machine.value_stack.items.len = frame.vars_base;
                    frame.stage = .await_ext;
                    if (!try self.requestVar(frame.ext, false)) return false;
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
                const child_var = switch (frame.field_axis) {
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
                if (!try self.requestVar(child_var, false)) return false;
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
                        if (!try self.requestVar(frame.ext, false)) return false;
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
                        if (!try self.requestVar(arg_var, false)) return false;
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
        var result = constraint;
        result.fn_var = try self.instantiateVarHelp(constraint.fn_var, force_root_copy);
        result.interpolation = try self.instantiateInterpolationMetadata(constraint.interpolation);
        return result;
    }

    fn instantiateInterpolationMetadata(
        self: *Self,
        metadata: StaticDispatchConstraint.InterpolationMetadata,
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
            const fresh_part_var = try self.instantiateVarHelp(part.var_, false);
            try machine.pending_parts.append(self.store.gpa, .{
                .var_ = fresh_part_var,
                .region = part.region,
            });
        }

        const fresh_item_var = try self.instantiateVarHelp(metadata.item_var, false);
        const fresh_parts_range = try self.store.appendInterpolationParts(machine.pending_parts.items[parts_base..]);
        machine.pending_parts.items.len = parts_base;
        return .{
            .expr_region = metadata.expr_region,
            .item_var = fresh_item_var,
            .interpolated_parts = fresh_parts_range,
        };
    }
};
