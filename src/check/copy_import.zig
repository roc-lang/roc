//! Cross-module type copying for imports.
//!
//! This module provides functionality to copy types from one module's type store
//! to another module's type store when importing. This ensures each module maintains
//! its own consistent type variable namespace while still being able to use types
//! from other modules.
//!
//! Copying a type across module envs is an identity REBASE boundary: nominal and
//! alias `origin_module` values are env-local indices into the source env's
//! module identity table, so the copy reads the 32-byte content identity hash
//! from the source table and getOrInserts it into the destination table. This is
//! the single cross-env identity resolution mechanism—no name matching.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");

const ModuleEnv = can.ModuleEnv;
const TypesStore = types_mod.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const InterpolationPartMetadata = types_mod.InterpolationPartMetadata;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const NominalType = types_mod.NominalType;

/// A mapping from source type variables to destination type variables.
/// Callers may preseed exact source substitutions; copying reuses those
/// destination roots and memoizes every newly copied root in the same map.
const VarMapping = std.AutoHashMap(Var, Var);

/// Explicit source declaration identity for alias substitutions performed
/// while copying a type graph between module stores.
pub const AliasSource = struct {
    origin_module: base.ModuleIdentity.Idx,
    source_decl: u32,
};

const AliasSourceMapping = std.AutoHashMap(AliasSource, Var);

/// Reusable heap buffers backing the copy's explicit worklist. A copy drains
/// every buffer back to its entry length, so nesting a nominal-declaration
/// copy inside a graph copy is safe.
const CopyScratch = struct {
    frames: std.ArrayList(Frame) = .empty,
    values: std.ArrayList(Var) = .empty,
    pending_fields: std.ArrayList(RecordField) = .empty,
    pending_tags: std.ArrayList(Tag) = .empty,
    pending_constraints: std.ArrayList(StaticDispatchConstraint) = .empty,
    pending_parts: std.ArrayList(InterpolationPartMetadata) = .empty,

    fn deinit(self: *CopyScratch, allocator: std.mem.Allocator) void {
        self.pending_parts.deinit(allocator);
        self.pending_constraints.deinit(allocator);
        self.pending_tags.deinit(allocator);
        self.pending_fields.deinit(allocator);
        self.values.deinit(allocator);
        self.frames.deinit(allocator);
    }
};

/// All state threaded through a single cross-module copy operation.
const CopyContext = struct {
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    scratch: CopyScratch = .{},

    fn deinit(self: *CopyContext) void {
        self.scratch.deinit(self.allocator);
    }

    fn sourceIdents(self: *const CopyContext) *const base.Ident.Store {
        return self.source_env.getIdentStoreConst();
    }

    fn copyIdent(self: *const CopyContext, source_ident: base.Ident.Idx) std.mem.Allocator.Error!base.Ident.Idx {
        const text = self.sourceIdents().getText(source_ident);
        const source_ident_value = base.Ident.for_text(text);
        const dest_idents = self.dest_env.getIdentStore();
        if (dest_idents.lookup(source_ident_value)) |existing| return existing;
        return try dest_idents.insert(self.allocator, source_ident_value);
    }

    /// Rebase an env-local module identity index from the source env's
    /// identity table into the destination env's table via the 32-byte
    /// content identity hash.
    fn copyOriginModule(self: *const CopyContext, source_origin: base.ModuleIdentity.Idx) std.mem.Allocator.Error!base.ModuleIdentity.Idx {
        const hash = self.source_env.moduleIdentityHash(source_origin);
        if (self.dest_env.lookupModuleIdentity(hash)) |existing| return existing;
        const source_display = self.source_env.moduleIdentityDisplayIdent(source_origin);
        const display = if (source_display.isNone())
            base.Ident.Idx.NONE
        else
            try self.copyIdent(source_display);
        return try self.dest_env.internModuleIdentity(hash, display);
    }
};

/// State shared by every frame that owns a destination placeholder: the var to
/// fill once its children are copied, and the descriptor flag carried over
/// from the source var.
const Fill = struct {
    placeholder: Var,
    empty_tag_union_is_default: bool,
};

const IdentityResult = enum { flex, rigid };

const FuncKind = enum { pure, effectful, unbound };
const FieldAxis = enum { type_var, presence_var };

/// One suspended step of the cross-module copy. Every frame that mints a
/// placeholder has already registered it in `var_mapping`, so a child that
/// re-reaches the frame's source var resolves to the placeholder instead of
/// descending again—the same cycle termination the recursion used.
///
/// Source runs are held as slices: the copy only ever writes to the
/// destination store, so a source run stays valid across the children that
/// suspend the frame holding it.
const Frame = union(enum) {
    alias_substitution: AliasSubstitutionFrame,
    identity: IdentityFrame,
    alias: AliasFrame,
    tuple: TupleFrame,
    nominal: NominalFrame,
    nominal_decl: NominalDeclFrame,
    func: FuncFrame,
    record: RecordFrame,
    record_unbound: RecordUnboundFrame,
    tag_union: TagUnionFrame,
};

/// An alias replaced wholesale by an explicit destination root still copies
/// its children, because they are independently recorded platform identity
/// slots; their copies are discarded here.
const AliasSubstitutionFrame = struct {
    dest_var: Var,
    backing: Var,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { backing, args } = .backing,
};

/// A flex or rigid var, copying its static-dispatch constraint list one
/// constraint at a time.
const IdentityFrame = struct {
    fill: Fill,
    result: IdentityResult,
    name: ?base.Ident.Idx,
    source_constraints: []const StaticDispatchConstraint,
    idx: u32 = 0,
    /// Base of this frame's collected constraints in `pending_constraints`.
    cons_base: u32,
    /// Base of the current constraint's collected parts in `pending_parts`.
    parts_base: u32 = 0,
    part_idx: u32 = 0,
    pending: StaticDispatchConstraint = undefined,
    stage: enum { head, await_fn, parts, await_part, await_item, finish_constraint, finish } = .head,
};

const AliasFrame = struct {
    fill: Fill,
    source: Alias,
    translated_ident: base.Ident.Idx,
    backing: Var,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { backing, args } = .backing,
};

const TupleFrame = struct {
    fill: Fill,
    elems: []const Var,
    idx: u32 = 0,
    values_base: u32,
};

const NominalFrame = struct {
    fill: Fill,
    source: NominalType,
    translated_ident: base.Ident.Idx,
    translated_origin: base.ModuleIdentity.Idx,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { decl, args } = .decl,
};

/// One declaration-table entry crossing the boundary. This frame fills no
/// placeholder and leaves no value behind: it exists purely to copy the
/// entry's formals and backing template onto the reserved entry.
const NominalDeclFrame = struct {
    reserved_idx: types_mod.NominalDecl.Idx,
    formals: []const Var,
    backing: Var,
    idx: u32 = 0,
    values_base: u32,
    formals_range: Var.SafeList.Range = Var.SafeList.Range.empty(),
    stage: enum { formals, backing, finish } = .formals,
};

const FuncFrame = struct {
    fill: Fill,
    kind: FuncKind,
    args: []const Var,
    ret: Var,
    effect_deps: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { args, ret, effect_deps } = .args,
};

/// A record row. Field names are translated as each field is reached, keeping
/// destination identifier interning in the recursion's order; the copied field
/// vars land on the value stack and are zipped back onto the collected names.
const RecordFrame = struct {
    fill: Fill,
    source_fields: RecordField.SafeMultiList.Range,
    ext: Var,
    idx: u32 = 0,
    axis: FieldAxis = .type_var,
    fields_base: u32,
    values_base: u32,
    fields_range: RecordField.SafeMultiList.Range = undefined,
    stage: enum { fields, await_ext } = .fields,
};

const RecordUnboundFrame = struct {
    fill: Fill,
    source_fields: RecordField.SafeMultiList.Range,
    idx: u32 = 0,
    axis: FieldAxis = .type_var,
    fields_base: u32,
    values_base: u32,
};

const TagUnionFrame = struct {
    fill: Fill,
    source_tags: Tag.SafeMultiList.Range,
    ext: Var,
    tag_idx: u32 = 0,
    arg_idx: u32 = 0,
    /// Base of the current tag's copied payload vars in the value stack.
    values_base: u32,
    /// Base of this frame's collected tags in `pending_tags`.
    tags_base: u32,
    tags_range: Tag.SafeMultiList.Range = undefined,
    stage: enum { tag_head, tag_args, tags_done, await_ext } = .tag_head,
};

/// Copy a type from one module's type store to another module's type store.
/// Unmapped source roots receive fresh destination variables. Roots already in
/// `var_mapping` are exact substitutions and are reused without copying. When
/// `alias_source_mapping` is present, every alias carrying a matching explicit
/// declaration identity resolves directly to that destination root.
///
/// Imported identifiers are interned directly into the destination module's
/// authoritative identifier store so all copied types in that module reference
/// one consistent `Ident.Store`; imported module identities are rebased into
/// the destination module's identity table the same way.
///
/// The graph copy runs on an explicit heap worklist, so copy depth is bounded
/// only by available memory, never by the native stack.
pub fn copyVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    var ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = alias_source_mapping,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
    };
    defer ctx.deinit();
    return copyVarCtx(&ctx, source_var);
}

fn copyVarCtx(ctx: *CopyContext, source_var: Var) std.mem.Allocator.Error!Var {
    const frames_base = ctx.scratch.frames.items.len;
    const values_base = ctx.scratch.values.items.len;
    if (!try request(ctx, source_var)) {
        try drive(ctx, frames_base);
    }
    std.debug.assert(ctx.scratch.values.items.len == values_base + 1);
    return ctx.scratch.values.pop().?;
}

/// Run every frame above `frames_base` to completion.
fn drive(ctx: *CopyContext, frames_base: usize) std.mem.Allocator.Error!void {
    const machine = &ctx.scratch;
    while (machine.frames.items.len > frames_base) {
        const top = &machine.frames.items[machine.frames.items.len - 1];
        // A step either suspends after requesting exactly one child (having
        // already written its own resume state), or finishes without
        // requesting anything—so popping on finish always removes the frame
        // the step ran for.
        const finished = switch (top.*) {
            .alias_substitution => |*frame| try stepAliasSubstitution(ctx, frame),
            .identity => |*frame| try stepIdentity(ctx, frame),
            .alias => |*frame| try stepAlias(ctx, frame),
            .tuple => |*frame| try stepTuple(ctx, frame),
            .nominal => |*frame| try stepNominal(ctx, frame),
            .nominal_decl => |*frame| try stepNominalDecl(ctx, frame),
            .func => |*frame| try stepFunc(ctx, frame),
            .record => |*frame| try stepRecord(ctx, frame),
            .record_unbound => |*frame| try stepRecordUnbound(ctx, frame),
            .tag_union => |*frame| try stepTagUnion(ctx, frame),
        };
        if (finished) {
            machine.frames.items.len -= 1;
        }
    }
}

/// Copy the head of one source var: reuse an existing mapping, and otherwise
/// mint + register the placeholder and either fill it immediately (contents
/// with no children) or push the frame that will fill it. Returns true when
/// the result var is already on the value stack; false when a frame was
/// pushed.
fn request(ctx: *CopyContext, source_var: Var) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    const resolved = ctx.source_store.resolveVar(source_var);

    if (ctx.var_mapping.get(resolved.var_)) |dest_var| {
        try machine.values.append(ctx.allocator, dest_var);
        return true;
    }

    if (resolved.desc.content == .alias) {
        const source_alias = resolved.desc.content.alias;
        if (source_alias.source_decl.toOptional()) |source_decl| {
            const alias_source = AliasSource{
                .origin_module = source_alias.origin_module,
                .source_decl = source_decl,
            };
            if (if (ctx.alias_source_mapping) |mapping| mapping.get(alias_source) else null) |dest_var| {
                // Memoize before visiting children so recursive source graphs
                // terminate. The replacement drops the source alias payload,
                // but its children must still be copied/memoized because they
                // are independently recorded platform identity slots.
                try ctx.var_mapping.put(resolved.var_, dest_var);
                try machine.frames.append(ctx.allocator, .{ .alias_substitution = .{
                    .dest_var = dest_var,
                    .backing = ctx.source_store.getAliasBackingVar(source_alias),
                    .args = ctx.source_store.sliceAliasArgs(source_alias),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            }
        }
    }

    const placeholder_var = try ctx.dest_store.fresh();
    try ctx.var_mapping.put(resolved.var_, placeholder_var);

    const fill = Fill{
        .placeholder = placeholder_var,
        .empty_tag_union_is_default = resolved.desc.flags.empty_tag_union_is_default,
    };

    // NOTE: a copied var whose content is a flex carrying a literal-conversion
    // constraint is an open literal in the destination module. Registering it on
    // the checker's open-literal worklist is the CALLER's job (see `Check.copyVar`,
    // which post-processes the destination store's allocation range)—this
    // module only copies type data between stores.
    return try pushContent(ctx, fill, resolved.desc.content);
}

/// Push the frame that copies `content`'s children, or fill the placeholder
/// outright when it has none. Returns true when the placeholder is already on
/// the value stack.
fn pushContent(ctx: *CopyContext, fill: Fill, content: Content) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    switch (content) {
        .err => {
            try finishFrame(ctx, fill, Content.err);
            return true;
        },
        .flex => |flex| {
            const translated_name = if (flex.name) |name_ident|
                try ctx.copyIdent(name_ident)
            else
                null;
            return try pushIdentity(ctx, fill, .flex, translated_name, flex.constraints);
        },
        .rigid => |rigid| {
            const translated_name = try ctx.copyIdent(rigid.name);
            return try pushIdentity(ctx, fill, .rigid, translated_name, rigid.constraints);
        },
        .alias => |alias| {
            const translated_ident = try ctx.copyIdent(alias.ident.ident_idx);
            try machine.frames.append(ctx.allocator, .{ .alias = .{
                .fill = fill,
                .source = alias,
                .translated_ident = translated_ident,
                .backing = ctx.source_store.getAliasBackingVar(alias),
                .args = ctx.source_store.sliceAliasArgs(alias),
                .values_base = @intCast(machine.values.items.len),
            } });
            return false;
        },
        .field_presence => |field_presence| {
            const copied_presence = switch (field_presence) {
                .required, .optional => field_presence,
                .defaulted => |id| types_mod.FieldPresence{ .defaulted = .{
                    .origin_module = try ctx.copyOriginModule(id.origin_module),
                    .expr_node = id.expr_node,
                } },
            };
            try finishFrame(ctx, fill, .{ .field_presence = copied_presence });
            return true;
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_record => {
                try finishFrame(ctx, fill, Content{ .structure = FlatType.empty_record });
                return true;
            },
            .empty_tag_union => {
                try finishFrame(ctx, fill, Content{ .structure = FlatType.empty_tag_union });
                return true;
            },
            .tuple => |tuple| {
                try machine.frames.append(ctx.allocator, .{ .tuple = .{
                    .fill = fill,
                    .elems = ctx.source_store.sliceVars(tuple.elems),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .nominal_type => |nominal| {
                const translated_ident = try ctx.copyIdent(nominal.ident.ident_idx);
                const translated_origin = try ctx.copyOriginModule(nominal.origin_module);
                try machine.frames.append(ctx.allocator, .{ .nominal = .{
                    .fill = fill,
                    .source = nominal,
                    .translated_ident = translated_ident,
                    .translated_origin = translated_origin,
                    .args = ctx.source_store.sliceNominalArgs(nominal),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .fn_pure => |func| return try pushFunc(ctx, fill, .pure, func),
            .fn_effectful => |func| return try pushFunc(ctx, fill, .effectful, func),
            .fn_unbound => |func| return try pushFunc(ctx, fill, .unbound, func),
            .record => |record| {
                try machine.frames.append(ctx.allocator, .{ .record = .{
                    .fill = fill,
                    .source_fields = record.fields,
                    .ext = record.ext,
                    .fields_base = @intCast(machine.pending_fields.items.len),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .record_unbound => |fields| {
                try machine.frames.append(ctx.allocator, .{ .record_unbound = .{
                    .fill = fill,
                    .source_fields = fields,
                    .fields_base = @intCast(machine.pending_fields.items.len),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .tag_union => |tag_union| {
                try machine.frames.append(ctx.allocator, .{ .tag_union = .{
                    .fill = fill,
                    .source_tags = tag_union.tags,
                    .ext = tag_union.ext,
                    .values_base = @intCast(machine.values.items.len),
                    .tags_base = @intCast(machine.pending_tags.items.len),
                } });
                return false;
            },
        },
    }
}

fn pushIdentity(
    ctx: *CopyContext,
    fill: Fill,
    result: IdentityResult,
    name: ?base.Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    if (constraints.len() == 0) {
        const empty = StaticDispatchConstraint.SafeList.Range.empty();
        const content: Content = switch (result) {
            .flex => Content{ .flex = Flex{ .name = name, .constraints = empty } },
            .rigid => Content{ .rigid = Rigid{ .name = name.?, .constraints = empty } },
        };
        try finishFrame(ctx, fill, content);
        return true;
    }
    try machine.frames.append(ctx.allocator, .{ .identity = .{
        .fill = fill,
        .result = result,
        .name = name,
        .source_constraints = ctx.source_store.sliceStaticDispatchConstraints(constraints),
        .cons_base = @intCast(machine.pending_constraints.items.len),
    } });
    return false;
}

fn pushFunc(
    ctx: *CopyContext,
    fill: Fill,
    kind: FuncKind,
    func: Func,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    try machine.frames.append(ctx.allocator, .{ .func = .{
        .fill = fill,
        .kind = kind,
        .args = ctx.source_store.sliceVars(func.args),
        .ret = func.ret,
        .effect_deps = ctx.source_store.sliceVars(func.effect_deps),
        .values_base = @intCast(machine.values.items.len),
    } });
    return false;
}

fn finishFrame(ctx: *CopyContext, fill: Fill, content: Content) std.mem.Allocator.Error!void {
    try ctx.dest_store.dangerousSetVarDesc(fill.placeholder, .{
        .content = content,
        .rank = types_mod.Rank.generalized,
        .flags = .{ .empty_tag_union_is_default = fill.empty_tag_union_is_default },
    });
    try ctx.scratch.values.append(ctx.allocator, fill.placeholder);
}

fn stepAliasSubstitution(ctx: *CopyContext, frame: *AliasSubstitutionFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .backing => {
                frame.stage = .args;
                if (!try request(ctx, frame.backing)) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, arg_var)) return false;
                    continue;
                }
                machine.values.items.len = frame.values_base;
                try machine.values.append(ctx.allocator, frame.dest_var);
                return true;
            },
        }
    }
}

fn stepIdentity(ctx: *CopyContext, frame: *IdentityFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .head => {
                if (frame.idx == frame.source_constraints.len) {
                    frame.stage = .finish;
                    continue;
                }
                const source_constraint = frame.source_constraints[frame.idx];
                frame.pending = source_constraint;
                frame.pending.fn_name = try ctx.copyIdent(source_constraint.fn_name);
                frame.stage = .await_fn;
                if (!try request(ctx, source_constraint.fn_var)) return false;
            },
            .await_fn => {
                frame.pending.fn_var = machine.values.pop().?;
                if (!frame.source_constraints[frame.idx].interpolation.isPresent()) {
                    frame.stage = .finish_constraint;
                    continue;
                }
                frame.parts_base = @intCast(machine.pending_parts.items.len);
                frame.part_idx = 0;
                frame.stage = .parts;
            },
            .parts => {
                const source_parts = ctx.source_store.sliceInterpolationParts(
                    frame.source_constraints[frame.idx].interpolation.interpolated_parts,
                );
                if (frame.part_idx < source_parts.len) {
                    frame.stage = .await_part;
                    if (!try request(ctx, source_parts[frame.part_idx].var_)) return false;
                    continue;
                }
                frame.stage = .await_item;
                if (!try request(ctx, frame.source_constraints[frame.idx].interpolation.item_var)) return false;
            },
            .await_part => {
                const source_parts = ctx.source_store.sliceInterpolationParts(
                    frame.source_constraints[frame.idx].interpolation.interpolated_parts,
                );
                try machine.pending_parts.append(ctx.allocator, .{
                    .var_ = machine.values.pop().?,
                    .region = source_parts[frame.part_idx].region,
                });
                frame.part_idx += 1;
                frame.stage = .parts;
            },
            .await_item => {
                const dest_item_var = machine.values.pop().?;
                const dest_parts_range = try ctx.dest_store.appendInterpolationParts(
                    machine.pending_parts.items[frame.parts_base..],
                );
                machine.pending_parts.items.len = frame.parts_base;
                frame.pending.interpolation = .{
                    .expr_region = frame.source_constraints[frame.idx].interpolation.expr_region,
                    .item_var = dest_item_var,
                    .interpolated_parts = dest_parts_range,
                };
                frame.stage = .finish_constraint;
            },
            .finish_constraint => {
                if (frame.source_constraints[frame.idx].derived_map_plan) |plan| {
                    frame.pending.derived_map_plan = .{
                        .tag_name = try ctx.copyIdent(plan.tag_name),
                        .payload_index = plan.payload_index,
                    };
                }
                // The introducing expression is module-scoped: its index refers to the
                // SOURCE module's CIR and is meaningless here. Clear it on the boundary
                // crossing so a consumer never dereferences a foreign expression index
                // against the destination module.
                frame.pending.provenance = .{};
                try machine.pending_constraints.append(ctx.allocator, frame.pending);
                frame.idx += 1;
                frame.stage = .head;
            },
            .finish => {
                const dest_range = try ctx.dest_store.appendStaticDispatchConstraints(
                    machine.pending_constraints.items[frame.cons_base..],
                );
                machine.pending_constraints.items.len = frame.cons_base;
                const content: Content = switch (frame.result) {
                    .flex => Content{ .flex = Flex{ .name = frame.name, .constraints = dest_range } },
                    .rigid => Content{ .rigid = Rigid{ .name = frame.name.?, .constraints = dest_range } },
                };
                try finishFrame(ctx, frame.fill, content);
                return true;
            },
        }
    }
}

fn stepAlias(ctx: *CopyContext, frame: *AliasFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .backing => {
                frame.stage = .args;
                if (!try request(ctx, frame.backing)) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, arg_var)) return false;
                    continue;
                }
                // The backing copy leads the run, matching the alias layout's
                // backing-then-args ordering.
                const dest_vars_span = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                const translated_origin = try ctx.copyOriginModule(frame.source.origin_module);
                try finishFrame(ctx, frame.fill, Content{ .alias = Alias{
                    .ident = types_mod.TypeIdent{ .ident_idx = frame.translated_ident },
                    .vars = .{ .nonempty = dest_vars_span },
                    .origin_module = translated_origin,
                    .source_decl = frame.source.source_decl,
                } });
                return true;
            },
        }
    }
}

fn stepTuple(ctx: *CopyContext, frame: *TupleFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        if (frame.idx < frame.elems.len) {
            const elem_var = frame.elems[frame.idx];
            frame.idx += 1;
            if (!try request(ctx, elem_var)) return false;
            continue;
        }
        const dest_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
        machine.values.items.len = frame.values_base;
        try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .tuple = types_mod.Tuple{ .elems = dest_range } } });
        return true;
    }
}

fn stepNominal(ctx: *CopyContext, frame: *NominalFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .decl => {
                frame.stage = .args;
                if (!try ensureNominalDeclCopied(ctx, frame.source, frame.translated_origin)) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, arg_var)) return false;
                    continue;
                }
                const dest_args_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .nominal_type = NominalType{
                    .ident = types_mod.TypeIdent{ .ident_idx = frame.translated_ident },
                    .args = dest_args_range,
                    .origin_module = frame.translated_origin,
                    .source = frame.source.source,
                } } });
                return true;
            },
        }
    }
}

fn stepNominalDecl(ctx: *CopyContext, frame: *NominalDeclFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .formals => {
                if (frame.idx < frame.formals.len) {
                    const formal_var = frame.formals[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, formal_var)) return false;
                    continue;
                }
                frame.formals_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                frame.stage = .backing;
            },
            .backing => {
                frame.stage = .finish;
                if (!try request(ctx, frame.backing)) return false;
            },
            .finish => {
                const dest_backing = machine.values.pop().?;
                var dest_entry = ctx.dest_store.getNominalDecl(frame.reserved_idx);
                dest_entry.formals = frame.formals_range;
                dest_entry.backing = dest_backing;
                ctx.dest_store.setNominalDecl(frame.reserved_idx, dest_entry);
                return true;
            },
        }
    }
}

fn stepFunc(ctx: *CopyContext, frame: *FuncFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, arg_var)) return false;
                    continue;
                }
                frame.idx = 0;
                frame.stage = .ret;
            },
            .ret => {
                frame.stage = .effect_deps;
                if (!try request(ctx, frame.ret)) return false;
            },
            .effect_deps => {
                if (frame.idx < frame.effect_deps.len) {
                    const effect_dep = frame.effect_deps[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, effect_dep)) return false;
                    continue;
                }
                // The value run holds the copied args, then the copied return
                // type, then the copied effect dependencies.
                const values = machine.values.items;
                const args_end = frame.values_base + frame.args.len;
                const dest_args_range = try ctx.dest_store.appendVars(values[frame.values_base..args_end]);
                const dest_ret = values[args_end];
                const dest_effect_deps_range = try ctx.dest_store.appendVars(values[args_end + 1 ..]);
                machine.values.items.len = frame.values_base;
                const dest_func = Func{
                    .args = dest_args_range,
                    .ret = dest_ret,
                    .effect_deps = dest_effect_deps_range,
                };
                const content: Content = switch (frame.kind) {
                    .pure => Content{ .structure = FlatType{ .fn_pure = dest_func } },
                    .effectful => Content{ .structure = FlatType{ .fn_effectful = dest_func } },
                    .unbound => Content{ .structure = FlatType{ .fn_unbound = dest_func } },
                };
                try finishFrame(ctx, frame.fill, content);
                return true;
            },
        }
    }
}

/// Zip the copied field types back onto the names collected for this row and
/// append the finished run to the destination store.
fn finishRecordFields(
    ctx: *CopyContext,
    source_fields: RecordField.SafeMultiList.Range,
    fields_base: u32,
    values_base: u32,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const machine = &ctx.scratch;
    const fields = machine.pending_fields.items[fields_base..];
    var value_idx: usize = values_base;
    for (fields, 0..) |*field, i| {
        const source_field = ctx.source_store.record_fields.get(@enumFromInt(@intFromEnum(source_fields.start) + i));
        const dest_type = machine.values.items[value_idx];
        value_idx += 1;
        field.presence = if (source_field.presence.presenceVar()) |_| blk: {
            const dest_presence = machine.values.items[value_idx];
            value_idx += 1;
            break :blk .unknown(dest_presence, dest_type);
        } else .required(dest_type);
    }
    const range = try ctx.dest_store.appendRecordFields(fields);
    machine.pending_fields.items.len = fields_base;
    machine.values.items.len = values_base;
    return range;
}

/// Translate one field's name, record it against its slot in this row's run,
/// and request the field's type.
fn requestRecordField(
    ctx: *CopyContext,
    source_fields: RecordField.SafeMultiList.Range,
    idx: *u32,
    axis: *FieldAxis,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    // Indexing through the run's start only happens when the record has
    // fields; start may be undefined when count is 0.
    const field = ctx.source_store.record_fields.get(@enumFromInt(@intFromEnum(source_fields.start) + idx.*));
    return switch (axis.*) {
        .type_var => blk: {
            const translated_name = try ctx.copyIdent(field.name);
            try machine.pending_fields.append(ctx.allocator, .{ .name = translated_name, .presence = undefined });
            if (field.presence.presenceVar() != null) {
                axis.* = .presence_var;
            } else {
                idx.* += 1;
            }
            break :blk try request(ctx, field.presence.typeVar());
        },
        .presence_var => blk: {
            idx.* += 1;
            axis.* = .type_var;
            break :blk try request(ctx, field.presence.presenceVar().?);
        },
    };
}

fn stepRecord(ctx: *CopyContext, frame: *RecordFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .fields => {
                if (frame.idx < frame.source_fields.count) {
                    if (!try requestRecordField(ctx, frame.source_fields, &frame.idx, &frame.axis)) return false;
                    continue;
                }
                frame.fields_range = try finishRecordFields(ctx, frame.source_fields, frame.fields_base, frame.values_base);
                frame.stage = .await_ext;
                if (!try request(ctx, frame.ext)) return false;
            },
            .await_ext => {
                const dest_ext = machine.values.pop().?;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .record = Record{
                    .fields = frame.fields_range,
                    .ext = dest_ext,
                } } });
                return true;
            },
        }
    }
}

fn stepRecordUnbound(ctx: *CopyContext, frame: *RecordUnboundFrame) std.mem.Allocator.Error!bool {
    while (true) {
        if (frame.idx < frame.source_fields.count) {
            if (!try requestRecordField(ctx, frame.source_fields, &frame.idx, &frame.axis)) return false;
            continue;
        }
        const fields_range = try finishRecordFields(ctx, frame.source_fields, frame.fields_base, frame.values_base);
        try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .record_unbound = fields_range } });
        return true;
    }
}

fn stepTagUnion(ctx: *CopyContext, frame: *TagUnionFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .tag_head => {
                if (frame.tag_idx == frame.source_tags.count) {
                    frame.stage = .tags_done;
                    continue;
                }
                frame.arg_idx = 0;
                frame.stage = .tag_args;
            },
            .tag_args => {
                // Indexing through the run's start only happens when the tag
                // union has tags; start may be undefined when count is 0.
                const tag = ctx.source_store.tags.get(@enumFromInt(@intFromEnum(frame.source_tags.start) + frame.tag_idx));
                const args_slice = ctx.source_store.sliceVars(tag.args);
                if (frame.arg_idx < args_slice.len) {
                    const arg_var = args_slice[frame.arg_idx];
                    frame.arg_idx += 1;
                    if (!try request(ctx, arg_var)) return false;
                    continue;
                }
                const dest_args_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                const translated_name = try ctx.copyIdent(tag.name);
                try machine.pending_tags.append(ctx.allocator, .{
                    .name = translated_name,
                    .args = dest_args_range,
                });
                frame.tag_idx += 1;
                frame.stage = .tag_head;
            },
            .tags_done => {
                frame.tags_range = try ctx.dest_store.appendTags(machine.pending_tags.items[frame.tags_base..]);
                machine.pending_tags.items.len = frame.tags_base;
                frame.stage = .await_ext;
                if (!try request(ctx, frame.ext)) return false;
            },
            .await_ext => {
                const dest_ext = machine.values.pop().?;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .tag_union = TagUnion{
                    .tags = frame.tags_range,
                    .ext = dest_ext,
                } } });
                return true;
            },
        }
    }
}

/// Ensure the destination store's nominal declaration table has an entry for
/// the declaration behind `source_nominal`, copying it from the source store's
/// table on first encounter. This runs once per (destination module,
/// declaration): every later application of the same declaration finds the
/// key already present and returns immediately, so declaration data crosses a
/// module boundary at most once regardless of how many applications do.
///
/// The entry is reserved (key registered) before its formals and backing are
/// copied so that self-referential backing templates terminate: copying the
/// template's own recursive application re-enters this function and finds the
/// key already present.
///
/// Returns true when nothing had to be copied; false when a frame was pushed
/// to copy the entry.
fn ensureNominalDeclCopied(
    ctx: *CopyContext,
    source_nominal: NominalType,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!bool {
    const source_decl = source_nominal.sourceDecl();
    // A nominal without a source declaration has no key and no declaration
    // table entry (only possible for hand-constructed types in tests).
    if (!source_decl.present) return true;

    if (ctx.dest_store.lookupNominalDeclByKey(translated_origin, source_decl.statement) != null) return true;

    const source_decl_idx = ctx.source_store.lookupNominalDecl(source_nominal) orelse {
        // Invariant: every nominal application in a store can resolve its
        // declaration in that store, so a keyed application without a source
        // table entry is a compiler bug.
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "copy_import invariant violated: nominal '{s}' has a source declaration but no declaration table entry in its source store",
                .{ctx.sourceIdents().getText(source_nominal.ident.ident_idx)},
            );
        }
        unreachable;
    };

    try pushNominalDeclEntry(ctx, ctx.source_store.getNominalDecl(source_decl_idx), translated_origin);
    return false;
}

/// Ensure the destination store has a declaration-table entry for the nominal
/// declaration at `statement` in the source module env, keyed under the source
/// module's own identity rebased into the destination env. No-op when the
/// source store has no entry for that statement (e.g. an alias declaration) or
/// when the destination already has one.
///
/// Newly created destination vars are recorded in `var_mapping`; the caller
/// owns follow-up bookkeeping for them (regions, worklists), exactly as with
/// `copyVar`.
pub fn ensureNominalDeclForStatement(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    statement: u32,
    var_mapping: *VarMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    var ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = null,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
    };
    defer ctx.deinit();

    const source_origin = source_env.selfModuleIdentity();
    const source_decl_idx = source_store.lookupNominalDeclByKey(source_origin, statement) orelse return;
    const translated_origin = try ctx.copyOriginModule(source_origin);
    if (dest_store.lookupNominalDeclByKey(translated_origin, statement) != null) return;

    const frames_base = ctx.scratch.frames.items.len;
    try pushNominalDeclEntry(&ctx, source_store.getNominalDecl(source_decl_idx), translated_origin);
    try drive(&ctx, frames_base);
}

/// Reserve one declaration-table entry (formals + backing template) in the
/// destination store and push the frame that copies its graph. The key is
/// reserved before the graph copy so that self-referential backing templates
/// terminate: copying the template's own recursive application re-enters
/// `ensureNominalDeclCopied` and finds the key already present. Nothing reads
/// the reserved entry's formals/backing while the copy is in flight—lookups
/// only test key presence.
fn pushNominalDeclEntry(
    ctx: *CopyContext,
    source_entry: types_mod.NominalDecl,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!void {
    const translated_ident = try ctx.copyIdent(source_entry.ident.ident_idx);
    const reserved_idx = try ctx.dest_store.registerNominalDecl(.{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .origin_module = translated_origin,
        .source = source_entry.source,
        .formals = Var.SafeList.Range.empty(),
        // Never read while the copy is in flight (see above); both fields are
        // filled in below once the graph copy completes.
        .backing = undefined,
        .flags = source_entry.flags,
    });

    try ctx.scratch.frames.append(ctx.allocator, .{ .nominal_decl = .{
        .reserved_idx = reserved_idx,
        .formals = ctx.source_store.sliceVars(source_entry.formals),
        .backing = source_entry.backing,
        .values_base = @intCast(ctx.scratch.values.items.len),
    } });
}
// Depth pin for the cross-module graph copy. Types crossing a module
// boundary are whatever the instantiator built, whose depth is bounded only by
// heap, so this copy must be too. A 40,000-node spine is past what a per-node
// native frame can hold on any ordinary 8 MiB stack: the recursive copy this
// replaced segfaulted on exactly this chain.
test "copy_import copies a spine deeper than any native-stack budget" {
    const allocator = std.testing.allocator;
    const depth: u32 = 40000;

    var source_env = try ModuleEnv.init(allocator, "");
    defer source_env.deinit();
    var dest_env = try ModuleEnv.init(allocator, "");
    defer dest_env.deinit();

    var source_store = try TypesStore.initCapacity(allocator, depth + 8, 8);
    defer source_store.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, depth + 8, 8);
    defer dest_store.deinit();

    var current = try source_store.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        const elems = try source_store.appendVars(&.{current});
        current = try source_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems } } });
    }

    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();

    const copied = try copyVar(&source_store, &dest_store, current, &mapping, null, &source_env, &dest_env, allocator);
    try std.testing.expect(dest_store.resolveVar(copied).desc.content == .structure);
}
