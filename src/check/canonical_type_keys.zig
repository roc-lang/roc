//! Deterministic checked-type digests for checked module and post-check boundaries.
//!
//! These keys are produced during checking finalization, while it is still valid
//! to inspect the checked type store and module-local identifiers. Post-check
//! stages consume the resulting keys; they must not recompute them from source
//! syntax or from environment lookup.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const canonical = @import("canonical_names.zig");

const ModuleEnv = can.ModuleEnv;

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const TypeStore = types.Store;
const Var = types.Var;
const LiteralKind = types.StaticDispatchConstraint.LiteralKind;

/// Public `TypeKeyInfo` declaration.
pub const TypeKeyInfo = struct {
    key: canonical.CanonicalTypeKey,
    contains_identity_variables: bool,
};

/// Public `fromVar` function.
pub fn fromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    return (try fromVarInfo(allocator, store, env, var_)).key;
}

/// Public `fromVarInfo` function.
pub fn fromVarInfo(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!TypeKeyInfo {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    try builder.writeVar(var_);
    return .{
        .key = .{ .bytes = builder.hasher.finalResult() },
        .contains_identity_variables = builder.contains_identity_variables,
    };
}

/// Public `identityVarsFromVar` function.
///
/// The identity variables (flex/rigid) reachable from `var_`, in the exact
/// first-encounter order the canonical key digest assigns them slots
/// (`writeIdentityVariable`). The index in the returned slice IS the identity
/// slot embedded in the key bytes, so two representations of the same type
/// (solver vars here, checked payloads in a `CheckedTypeStore`) enumerate
/// identities in the same order. Caller owns the returned slice.
pub fn identityVarsFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error![]types.Var {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    try builder.writeVar(var_);
    return try allocator.dupe(types.Var, builder.identity_variables.items);
}

/// Public `fromVarErrSensitive` function.
///
/// Like `fromVar`, except erroneous content digests as its resolved root var
/// rather than as one universal token. Dispatch-state digests use this so two
/// states that were poisoned by unrelated failures never compare equal, while
/// re-encountering the very same poisoned var still digests stably.
pub fn fromVarErrSensitive(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    builder.err_by_var = true;
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

/// Public `fromConcreteVar` function.
pub fn fromConcreteVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    builder.require_concrete = true;
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

/// Public `emptyTagUnion` function.
pub fn emptyTagUnion() canonical.CanonicalTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeByteSlice(&hasher, "[]");
    return .{ .bytes = hasher.finalResult() };
}

/// Public `defaultDec` function.
pub fn defaultDec(idents: *const Ident.Store) canonical.CanonicalTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeByteSlice(&hasher, "nominal");
    writeIdentText(&hasher, idents, builtinDecTypeIdent(idents));
    writeIdentText(&hasher, idents, builtinModuleIdent(idents));
    writeBoolValue(&hasher, true);
    writeU32Value(&hasher, 0);
    return .{ .bytes = hasher.finalResult() };
}

/// Public `schemeFromVar` function.
pub fn schemeFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeSchemeKey {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    builder.writeTag("canonical_type_scheme");
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

/// Whether the canonical-key traversal for `var_` reaches erroneous checked
/// type content. This uses the key builder itself in detection mode, so guards
/// for later key construction cannot accidentally inspect a narrower graph.
pub fn containsError(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!bool {
    var builder = Builder.init(allocator, store, env);
    defer builder.deinit();
    builder.detect_errors = true;
    try builder.writeVar(var_);
    return builder.contains_error;
}

const RecordFieldForKey = struct {
    name: Ident.Idx,
    presence: types.RecordField.Presence,
};

const TagForKey = struct {
    name: Ident.Idx,
    args: Var.SafeList.Range,
};

/// One suspended step of the digest walk. A frame is created only after its
/// node's leading bytes are already in the hasher, so byte order is the
/// recursion's: the frame then dispatches children one at a time and emits
/// whatever trailing bytes follow them. Child runs are held as slices into the
/// type store, which the digest never writes to, so a run stays valid across
/// the children that suspend the frame holding it.
const Frame = union(enum) {
    alias: AliasFrame,
    vars: VarsFrame,
    func: FuncFrame,
    record: RecordFrame,
    tag_union: TagUnionFrame,
    constraints: ConstraintsFrame,
};

/// An alias digests its backing structure first, then its argument count, then
/// the arguments—the count follows the backing subtree, so it cannot be
/// written when the frame is created.
const AliasFrame = struct {
    backing: Var,
    args: []const Var,
    idx: u32 = 0,
    stage: enum { backing, args_count, args } = .backing,
};

/// A run of child vars whose count is already written: tuple elements and
/// nominal arguments.
const VarsFrame = struct {
    vars: []const Var,
    idx: u32 = 0,
};

const FuncFrame = struct {
    args: []const Var,
    ret: Var,
    idx: u32 = 0,
    stage: enum { args, ret, done } = .args,
};

/// A normalized record row. The row's fields are collected and sorted into
/// `Builder.pending_fields` before the frame exists, so the frame carries the
/// base of its own run and re-reads entries by index: a nested row appends
/// above this run and truncates back to its own base when it finishes.
const RecordFrame = struct {
    fields_base: u32,
    fields_count: u32,
    idx: u32 = 0,
    tail: ?Var,
    stage: enum { field_head, presence_var, type_var, tail, done } = .field_head,
};

/// A normalized tag-union row, holding both the position within the row and
/// the position within the current tag's payload run.
const TagUnionFrame = struct {
    tags_base: u32,
    tags_count: u32,
    tag_idx: u32 = 0,
    args: []const Var = &.{},
    arg_idx: u32 = 0,
    tail: ?Var,
    stage: enum { tag_head, tag_args, tail, done } = .tag_head,
};

/// A static-dispatch constraint list. Each constraint writes its name, then
/// its function type as a child, then the origin bytes that follow it.
const ConstraintsFrame = struct {
    constraints: []const types.StaticDispatchConstraint,
    idx: u32 = 0,
    stage: enum { head, origin } = .head,
};

const Builder = struct {
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    idents: *const Ident.Store,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.ArrayList(Var),
    identity_variables: std.ArrayList(Var),
    /// Suspended steps of the walk, innermost last. The walk descends on this
    /// heap stack rather than the native one, so digest depth is bounded only
    /// by available memory.
    frames: std.ArrayList(Frame),
    /// Collected row fields, one contiguous run per record frame in flight.
    pending_fields: std.ArrayList(RecordFieldForKey),
    /// Collected row tags, one contiguous run per tag-union frame in flight.
    pending_tags: std.ArrayList(TagForKey),
    /// Extension vars already reached by the row collection currently running.
    /// Collection runs to completion without dispatching children, so one
    /// buffer serves every row node.
    ext_seen: std.ArrayList(Var),
    require_concrete: bool = false,
    contains_identity_variables: bool = false,
    detect_errors: bool = false,
    contains_error: bool = false,
    /// Digest erroneous content as its resolved root var instead of one
    /// universal token, so unrelated poisoned positions never key equal.
    err_by_var: bool = false,

    fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) Builder {
        return .{
            .allocator = allocator,
            .store = store,
            .env = env,
            .idents = env.getIdentStoreConst(),
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = .empty,
            .identity_variables = .empty,
            .frames = .empty,
            .pending_fields = .empty,
            .pending_tags = .empty,
            .ext_seen = .empty,
        };
    }

    fn deinit(self: *Builder) void {
        self.ext_seen.deinit(self.allocator);
        self.pending_tags.deinit(self.allocator);
        self.pending_fields.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.identity_variables.deinit(self.allocator);
        self.active.deinit(self.allocator);
    }

    /// Digest the type reachable from `var_`, driving the walk to completion on
    /// the frame stack.
    fn writeVar(self: *Builder, var_: Var) Allocator.Error!void {
        const frames_base = self.frames.items.len;
        const active_base = self.active.items.len;
        const fields_base = self.pending_fields.items.len;
        const tags_base = self.pending_tags.items.len;
        // A completed walk drains every buffer back to its entry length. An
        // allocation failure mid-walk can leave entries behind, so unwind them
        // here and keep the builder's buffers consistent on both exit paths.
        errdefer {
            self.frames.items.len = frames_base;
            self.active.items.len = active_base;
            self.pending_fields.items.len = fields_base;
            self.pending_tags.items.len = tags_base;
        }

        if (!try self.request(var_)) {
            while (self.frames.items.len > frames_base) {
                const top = &self.frames.items[self.frames.items.len - 1];
                // A step either suspends after requesting exactly one child
                // (having already written its own resume state), or finishes
                // without requesting anything—so popping on finish always
                // removes the frame the step ran for.
                const finished = switch (top.*) {
                    .alias => |*frame| try self.stepAlias(frame),
                    .vars => |*frame| try self.stepVars(frame),
                    .func => |*frame| try self.stepFunc(frame),
                    .record => |*frame| try self.stepRecord(frame),
                    .tag_union => |*frame| try self.stepTagUnion(frame),
                    .constraints => |*frame| try self.stepConstraints(frame),
                };
                if (finished) {
                    self.frames.items.len -= 1;
                }
            }
        }

        std.debug.assert(self.active.items.len == active_base);
    }

    /// Digest one var's head: write every byte that precedes its children and
    /// either finish it outright (returning true) or push the frame that will
    /// dispatch its children (returning false).
    fn request(self: *Builder, var_: Var) Allocator.Error!bool {
        const resolved = self.store.resolveVar(var_);
        const root = resolved.var_;

        if (self.err_by_var and resolved.desc.content == .err) {
            self.writeTag("err_var");
            self.writeU32(@intFromEnum(root));
            return true;
        }

        // The checker explicitly records when it closes an otherwise
        // unresolved identity to `[]`. Encode the surviving union-find root so
        // every reference to that identity shares one checked type digest.
        if (resolved.desc.flags.empty_tag_union_is_default) {
            return try self.writeIdentityVariable(
                root,
                "defaulted_empty_tag_union",
                null,
                types.StaticDispatchConstraint.SafeList.Range.empty(),
            );
        }

        const content_tag = std.meta.activeTag(resolved.desc.content);
        if (content_tag == .flex) {
            const flex = resolved.desc.content.flex;
            if (self.require_concrete) {
                if (self.flexLiteralDefaultKind(flex)) |kind| {
                    self.writeLiteralDefault(kind);
                    return true;
                }
                invariantViolation("concrete canonical type key requested for unsolved flex type variable");
            }
            return try self.writeIdentityVariable(root, "flex", flex.name, flex.constraints);
        }
        if (content_tag == .rigid) {
            const rigid = resolved.desc.content.rigid;
            if (self.require_concrete) {
                invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
            }
            return try self.writeIdentityVariable(root, "rigid", rigid.name, rigid.constraints);
        }

        if (varSlot(self.active.items, root)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return true;
        }

        try self.active.append(self.allocator, root);
        if (try self.writeContent(resolved.desc.content)) return false;
        _ = self.active.pop();
        return true;
    }

    /// Whether `writeIdentityVariable` finished the identity outright: an
    /// identity with constraints suspends on the constraint list instead.
    fn writeIdentityVariable(
        self: *Builder,
        root: Var,
        comptime tag: []const u8,
        name: ?Ident.Idx,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) Allocator.Error!bool {
        self.contains_identity_variables = true;
        if (varSlot(self.identity_variables.items, root)) |slot| {
            self.writeTag("identity_var_ref");
            self.writeU32(slot);
            return true;
        }

        const slot: u32 = @intCast(self.identity_variables.items.len);
        try self.identity_variables.append(self.allocator, root);
        self.writeTag(tag);
        self.writeU32(slot);
        try self.writeOptionalIdent(name);

        const items = self.store.sliceStaticDispatchConstraints(constraints);
        self.writeU32(@intCast(items.len));
        if (items.len == 0) return true;
        try self.frames.append(self.allocator, .{ .constraints = .{ .constraints = items } });
        return false;
    }

    fn varSlot(vars: []const Var, var_: Var) ?u32 {
        for (vars, 0..) |candidate, slot| {
            if (candidate == var_) return @intCast(slot);
        }
        return null;
    }

    /// Write `content`'s leading bytes with its root already on `active`.
    /// Returns true when a frame was pushed to dispatch children, false when
    /// the content had none and the caller must pop `active` itself.
    fn writeContent(self: *Builder, content: types.Content) Allocator.Error!bool {
        switch (content) {
            .err => {
                if (self.detect_errors) self.contains_error = true;
                self.writeTag("err");
                return false;
            },
            .flex => |flex| {
                if (self.require_concrete) {
                    if (self.flexLiteralDefaultKind(flex)) |kind| {
                        self.writeLiteralDefault(kind);
                        return false;
                    }
                    invariantViolation("concrete canonical type key requested for unsolved flex type variable");
                }
                invariantViolation("canonical type key reached an unsolved flex without its root identity");
            },
            .rigid => {
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
                }
                invariantViolation("canonical type key reached an unsolved rigid without its root identity");
            },
            .field_presence => |field_presence| {
                switch (field_presence) {
                    .required => self.writeTag("presence_required"),
                    .optional => self.writeTag("presence_optional"),
                    .defaulted => |id| {
                        self.writeTag("presence_defaulted");
                        self.writeBytes(self.env.moduleIdentityHash(id.origin_module));
                        self.writeU32(id.expr_node);
                    },
                }
                return false;
            },
            .alias => |alias| {
                self.writeTag("alias");
                self.writeNamedSourceIdentity(alias.origin_module, alias.ident.ident_idx, alias.source_decl.toOptional());
                try self.frames.append(self.allocator, .{ .alias = .{
                    .backing = self.store.getAliasBackingVar(alias),
                    .args = self.store.sliceAliasArgs(alias),
                } });
                return true;
            },
            .structure => |flat| return try self.writeFlat(flat),
        }
    }

    /// INVARIANT: a still-open flex may be keyed as the canonical literal
    /// default (Dec for numerals, Str for quotes) ONLY when every constraint on
    /// it is a literal conversion—either a literal's own `from_literal`
    /// constraint or a `where`-clause contract naming a literal-conversion hook.
    /// Such a var is exactly what the checker's defaulting commits to the kind's
    /// default.
    /// Any OTHER constraint (binop/method usage, or a `where` clause naming some
    /// other method) feeds the checker's candidate probing, which may commit a
    /// non-default candidate (e.g. an integer-only method commits I64); such a
    /// var must already be concrete when a concrete key is requested, so finding
    /// one still open here means an upstream defaulting step was skipped—
    /// keying it as the default would be a guess, so we raise an invariant
    /// violation instead.
    ///
    /// Both the kind and the "is this a literal conversion" test come from the
    /// defaulting oracle (src/types/literal_defaulting.zig), so this key builder
    /// cannot disagree with the checker's defaulting about which vars default—
    /// including the mixed-kind set (both numeral and quote literal constraints,
    /// reachable only via a flex/flex merge the checker reports as a type error,
    /// so it never survives to key generation), where the oracle's precedence
    /// deterministically picks `numeral`.
    fn flexLiteralDefaultKind(self: *Builder, flex: types.Flex) ?LiteralKind {
        const literal_idents = types.literal_defaulting.LiteralMethodIdents{
            .from_numeral = self.env.idents.from_numeral,
            .from_quote = self.env.idents.from_quote,
            .from_interpolation = self.env.idents.from_interpolation,
        };
        const constraints = self.store.sliceStaticDispatchConstraints(flex.constraints);
        const kind = types.literal_defaulting.dominantKind(literal_idents, constraints);
        var has_other = false;
        for (constraints) |constraint| {
            if (types.literal_defaulting.constraintLiteralKind(literal_idents, constraint) == null) {
                has_other = true;
            }
        }
        if (kind != null and has_other) {
            invariantViolation("concrete canonical type key requested for an open literal with non-literal constraints (defaulting was skipped)");
        }
        return kind;
    }

    fn writeLiteralDefault(self: *Builder, kind: LiteralKind) void {
        self.writeTag("nominal");
        switch (types.literal_defaulting.defaultTargetForKind(kind)) {
            .dec => self.writeIdent(builtinDecTypeIdent(self.idents)),
            .str => self.writeIdent(builtinStrTypeIdent(self.idents)),
        }
        self.writeIdent(builtinModuleIdent(self.idents));
        self.writeOptionalU32(null);
        self.writeBool(true);
        self.writeU32(0);
    }

    /// Write `flat`'s leading bytes, returning true when a frame was pushed.
    fn writeFlat(self: *Builder, flat: types.FlatType) Allocator.Error!bool {
        switch (flat) {
            .empty_record => {
                self.writeTag("empty_record");
                return false;
            },
            .empty_tag_union => {
                self.writeTag("[]");
                return false;
            },
            .record_unbound => |fields| {
                self.writeTag("record_unbound");
                return try self.writeNormalizedRecordFields(fields);
            },
            .record => |record| return try self.writeNormalizedRecordPayload(record.fields, record.ext),
            .tuple => |tuple| {
                self.writeTag("tuple");
                return try self.pushVarRange(tuple.elems);
            },
            .nominal_type => |nominal| {
                if (self.detect_errors and self.store.nominalDeclIsInvalid(nominal)) {
                    self.contains_error = true;
                }
                self.writeTag("nominal");
                self.writeNamedSourceIdentity(nominal.origin_module, nominal.ident.ident_idx, nominal.sourceDeclOptional());
                self.writeBool(nominal.isOpaque());
                const args = self.store.sliceNominalArgs(nominal);
                self.writeU32(@intCast(args.len));
                return try self.pushVars(args);
            },
            .fn_pure, .fn_unbound => |func| {
                self.writeTag("fn_pure");
                return try self.pushFunc(func);
            },
            .fn_effectful => |func| {
                self.writeTag("fn_effectful");
                return try self.pushFunc(func);
            },
            .tag_union => |tag_union| return try self.writeNormalizedTagUnionPayload(tag_union.tags, tag_union.ext),
        }
    }

    /// A function digests its argument count, then its arguments, then its
    /// return type.
    fn pushFunc(self: *Builder, func: types.Func) Allocator.Error!bool {
        const args = self.store.sliceVars(func.args);
        self.writeU32(@intCast(args.len));
        try self.frames.append(self.allocator, .{ .func = .{ .args = args, .ret = func.ret } });
        return true;
    }

    fn pushVarRange(self: *Builder, range: Var.SafeList.Range) Allocator.Error!bool {
        const vars = self.store.sliceVars(range);
        self.writeU32(@intCast(vars.len));
        return try self.pushVars(vars);
    }

    /// An empty run has no children to dispatch, so it needs no frame at all.
    fn pushVars(self: *Builder, vars: []const Var) Allocator.Error!bool {
        if (vars.len == 0) return false;
        try self.frames.append(self.allocator, .{ .vars = .{ .vars = vars } });
        return true;
    }

    fn stepAlias(self: *Builder, frame: *AliasFrame) Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .backing => {
                    frame.stage = .args_count;
                    if (!try self.request(frame.backing)) return false;
                },
                .args_count => {
                    self.writeU32(@intCast(frame.args.len));
                    frame.stage = .args;
                },
                .args => {
                    if (frame.idx < frame.args.len) {
                        const arg = frame.args[frame.idx];
                        frame.idx += 1;
                        if (!try self.request(arg)) return false;
                        continue;
                    }
                    _ = self.active.pop();
                    return true;
                },
            }
        }
    }

    fn stepVars(self: *Builder, frame: *VarsFrame) Allocator.Error!bool {
        while (true) {
            if (frame.idx < frame.vars.len) {
                const child = frame.vars[frame.idx];
                frame.idx += 1;
                if (!try self.request(child)) return false;
                continue;
            }
            _ = self.active.pop();
            return true;
        }
    }

    fn stepFunc(self: *Builder, frame: *FuncFrame) Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .args => {
                    if (frame.idx < frame.args.len) {
                        const arg = frame.args[frame.idx];
                        frame.idx += 1;
                        if (!try self.request(arg)) return false;
                        continue;
                    }
                    frame.stage = .ret;
                },
                .ret => {
                    frame.stage = .done;
                    if (!try self.request(frame.ret)) return false;
                },
                .done => {
                    _ = self.active.pop();
                    return true;
                },
            }
        }
    }

    fn appendRecordFieldsForKey(
        self: *Builder,
        range: types.RecordField.SafeMultiList.Range,
    ) Allocator.Error!void {
        const slice = self.store.getRecordFieldsSlice(range);
        const names = slice.items(.name);
        const presences = slice.items(.presence);
        for (names, presences) |name, presence| {
            try self.pending_fields.append(self.allocator, .{
                .name = name,
                .presence = presence,
            });
        }
    }

    /// Collect a record row's fields—the head run plus everything its
    /// extension chain contributes—into a fresh run on `pending_fields`, and
    /// report the extension var the row ends on, if any.
    fn collectRecordRow(
        self: *Builder,
        head: types.RecordField.SafeMultiList.Range,
        ext: ?Var,
    ) Allocator.Error!?Var {
        try self.appendRecordFieldsForKey(head);

        var tail = ext;
        self.ext_seen.clearRetainingCapacity();
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            const root = resolved.var_;
            if (varSlot(self.active.items, root) != null) break;
            if (varSlot(self.ext_seen.items, root) != null) break;
            try self.ext_seen.append(self.allocator, root);
            const content = resolved.desc.content;
            if (std.meta.activeTag(content) != .structure) break;
            const flat = content.structure;
            const flat_tag = std.meta.activeTag(flat);
            if (flat_tag == .empty_record) {
                tail = null;
                break;
            }
            if (flat_tag == .record) {
                try self.appendRecordFieldsForKey(flat.record.fields);
                tail = flat.record.ext;
                continue;
            }
            if (flat_tag == .record_unbound) {
                try self.appendRecordFieldsForKey(flat.record_unbound);
                tail = null;
            }
            break;
        }
        return tail;
    }

    fn writeNormalizedRecordFields(
        self: *Builder,
        head: types.RecordField.SafeMultiList.Range,
    ) Allocator.Error!bool {
        const fields_base: u32 = @intCast(self.pending_fields.items.len);
        const tail = try self.collectRecordRow(head, null);

        const fields = self.pending_fields.items[fields_base..];
        std.mem.sort(RecordFieldForKey, fields, self, recordFieldForKeyLessThan);
        self.writeU32(@intCast(fields.len));
        try self.frames.append(self.allocator, .{ .record = .{
            .fields_base = fields_base,
            .fields_count = @intCast(fields.len),
            .tail = tail,
        } });
        return true;
    }

    fn writeNormalizedRecordPayload(
        self: *Builder,
        head: types.RecordField.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!bool {
        const fields_base: u32 = @intCast(self.pending_fields.items.len);
        const tail = try self.collectRecordRow(head, ext);

        const fields = self.pending_fields.items[fields_base..];
        std.mem.sort(RecordFieldForKey, fields, self, recordFieldForKeyLessThan);
        if (tail == null and fields.len == 0) {
            self.pending_fields.items.len = fields_base;
            self.writeTag("empty_record");
            return false;
        }

        self.writeTag("record");
        self.writeU32(@intCast(fields.len));
        try self.frames.append(self.allocator, .{ .record = .{
            .fields_base = fields_base,
            .fields_count = @intCast(fields.len),
            .tail = tail,
        } });
        return true;
    }

    fn stepRecord(self: *Builder, frame: *RecordFrame) Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .field_head => {
                    if (frame.idx < frame.fields_count) {
                        const index = frame.fields_base + frame.idx;
                        const field = self.pending_fields.items[index];
                        if (frame.idx > 0 and self.idents.idxTextEql(self.pending_fields.items[index - 1].name, field.name)) {
                            invariantViolation("canonical type key row normalization found duplicate record fields");
                        }
                        self.writeIdent(field.name);
                        const type_var = switch (field.presence.decode()) {
                            .required => |var_| blk: {
                                self.writeBool(false);
                                break :blk var_;
                            },
                            .unknown => |unknown| blk: {
                                switch (self.store.resolveVar(unknown.presence).desc.content) {
                                    .field_presence => |presence| switch (presence) {
                                        .required => self.writeBool(false),
                                        .defaulted => |id| {
                                            self.writeTag("field_default");
                                            self.writeBytes(self.env.moduleIdentityHash(id.origin_module));
                                            self.writeU32(id.expr_node);
                                        },
                                        .optional => self.writeTag("presence_optional_field"),
                                    },
                                    .flex => {
                                        self.writeTag("presence_variable");
                                        frame.stage = .presence_var;
                                        if (!try self.request(unknown.presence)) return false;
                                    },
                                    .err => {
                                        if (self.detect_errors) self.contains_error = true;
                                        self.writeTag("err");
                                    },
                                    .rigid, .alias, .structure => invariantViolation("canonical type key reached a field presence variable holding non-presence content"),
                                }
                                break :blk unknown.var_;
                            },
                        };
                        // A flex presence first writes its identity above; all
                        // other kinds proceed directly to the value type.
                        if (frame.stage == .presence_var) continue;
                        frame.stage = .type_var;
                        if (!try self.request(type_var)) return false;
                        continue;
                    }
                    self.pending_fields.items.len = frame.fields_base;
                    frame.stage = .tail;
                },
                .presence_var => {
                    const field = self.pending_fields.items[frame.fields_base + frame.idx];
                    frame.stage = .type_var;
                    if (!try self.request(field.presence.typeVar())) return false;
                },
                .type_var => {
                    frame.idx += 1;
                    frame.stage = .field_head;
                },
                .tail => {
                    frame.stage = .done;
                    if (frame.tail) |tail_var| {
                        if (!try self.request(tail_var)) return false;
                    } else {
                        self.writeTag("empty_record");
                    }
                },
                .done => {
                    _ = self.active.pop();
                    return true;
                },
            }
        }
    }

    fn appendTagsForKey(
        self: *Builder,
        range: types.Tag.SafeMultiList.Range,
    ) Allocator.Error!void {
        const slice = self.store.getTagsSlice(range);
        const names = slice.items(.name);
        const args = slice.items(.args);
        for (names, args) |name, arg_range| {
            try self.pending_tags.append(self.allocator, .{
                .name = name,
                .args = arg_range,
            });
        }
    }

    fn writeNormalizedTagUnionPayload(
        self: *Builder,
        head: types.Tag.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!bool {
        const tags_base: u32 = @intCast(self.pending_tags.items.len);
        try self.appendTagsForKey(head);

        var tail: ?Var = ext;
        self.ext_seen.clearRetainingCapacity();
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            const root = resolved.var_;
            if (varSlot(self.active.items, root) != null) break;
            if (varSlot(self.ext_seen.items, root) != null) break;
            try self.ext_seen.append(self.allocator, root);
            const content = resolved.desc.content;
            if (std.meta.activeTag(content) != .structure) break;
            const flat = content.structure;
            const flat_tag = std.meta.activeTag(flat);
            if (flat_tag == .empty_tag_union) {
                tail = null;
                break;
            }
            if (flat_tag == .tag_union) {
                try self.appendTagsForKey(flat.tag_union.tags);
                tail = flat.tag_union.ext;
                continue;
            }
            break;
        }

        const tags = self.pending_tags.items[tags_base..];
        std.mem.sort(TagForKey, tags, self, tagForKeyLessThan);
        if (tail == null and tags.len == 0) {
            self.pending_tags.items.len = tags_base;
            self.writeTag("[]");
            return false;
        }

        self.writeTag("tag_union");
        self.writeU32(@intCast(tags.len));
        try self.frames.append(self.allocator, .{ .tag_union = .{
            .tags_base = tags_base,
            .tags_count = @intCast(tags.len),
            .tail = tail,
        } });
        return true;
    }

    fn stepTagUnion(self: *Builder, frame: *TagUnionFrame) Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .tag_head => {
                    if (frame.tag_idx >= frame.tags_count) {
                        self.pending_tags.items.len = frame.tags_base;
                        frame.stage = .tail;
                        continue;
                    }
                    const index = frame.tags_base + frame.tag_idx;
                    const tag = self.pending_tags.items[index];
                    if (frame.tag_idx > 0 and self.idents.idxTextEql(self.pending_tags.items[index - 1].name, tag.name)) {
                        invariantViolation("canonical type key row normalization found duplicate tags");
                    }
                    self.writeIdent(tag.name);
                    frame.args = self.store.sliceVars(tag.args);
                    self.writeU32(@intCast(frame.args.len));
                    frame.arg_idx = 0;
                    frame.stage = .tag_args;
                },
                .tag_args => {
                    if (frame.arg_idx < frame.args.len) {
                        const arg = frame.args[frame.arg_idx];
                        frame.arg_idx += 1;
                        if (!try self.request(arg)) return false;
                        continue;
                    }
                    frame.tag_idx += 1;
                    frame.stage = .tag_head;
                },
                .tail => {
                    frame.stage = .done;
                    if (frame.tail) |tail_var| {
                        if (!try self.request(tail_var)) return false;
                    } else {
                        self.writeTag("[]");
                    }
                },
                .done => {
                    _ = self.active.pop();
                    return true;
                },
            }
        }
    }

    fn recordFieldForKeyLessThan(self: *Builder, lhs: RecordFieldForKey, rhs: RecordFieldForKey) bool {
        return self.idents.idxTextLessThan(lhs.name, rhs.name);
    }

    fn tagForKeyLessThan(self: *Builder, lhs: TagForKey, rhs: TagForKey) bool {
        return self.idents.idxTextLessThan(lhs.name, rhs.name);
    }

    fn stepConstraints(self: *Builder, frame: *ConstraintsFrame) Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .head => {
                    if (frame.idx >= frame.constraints.len) return true;
                    const constraint = frame.constraints[frame.idx];
                    self.writeIdent(constraint.fn_name);
                    frame.stage = .origin;
                    if (!try self.request(constraint.fn_var)) return false;
                },
                .origin => {
                    const constraint = frame.constraints[frame.idx];
                    self.writeTag(@tagName(constraint.origin));
                    self.writeBool(constraint.origin.binopNegated());
                    const maybe_num_literal = constraint.origin.numeralInfo();
                    self.writeBool(maybe_num_literal != null);
                    if (maybe_num_literal) |num_literal| {
                        self.hasher.update(&num_literal.keyBytes());
                    }
                    frame.idx += 1;
                    frame.stage = .head;
                },
            }
        }
    }

    fn writeOptionalIdent(self: *Builder, maybe_ident: ?Ident.Idx) Allocator.Error!void {
        self.writeBool(maybe_ident != null);
        if (maybe_ident) |ident| {
            self.writeIdent(ident);
        }
    }

    fn writeOptionalU32(self: *Builder, maybe_value: ?u32) void {
        self.writeBool(maybe_value != null);
        if (maybe_value) |value| {
            self.writeU32(value);
        }
    }

    /// Write a named type's source identity: the declaring module's 32-byte
    /// deep CONTENT identity plus the within-module discriminator, mirroring
    /// `sameNominalIdentity` in unify.zig exactly. No name text participates
    /// in the module component, so the digest never depends on coordinator
    /// naming or build directories.
    fn writeNamedSourceIdentity(self: *Builder, origin_module: base.ModuleIdentity.Idx, ident: Ident.Idx, source_decl: ?u32) void {
        self.writeBytes(self.env.moduleIdentityHash(origin_module));
        self.writeOptionalU32(source_decl);
        if (source_decl == null) {
            self.writeIdent(ident);
        }
    }

    fn writeIdent(self: *Builder, ident: Ident.Idx) void {
        self.writeBytes(self.idents.getText(ident));
    }

    fn writeTag(self: *Builder, tag: []const u8) void {
        self.writeBytes(tag);
    }

    fn writeBytes(self: *Builder, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.hasher.update(bytes);
    }

    fn writeBool(self: *Builder, value: bool) void {
        const byte: u8 = if (value) 1 else 0;
        self.hasher.update(std.mem.asBytes(&byte));
    }

    fn writeU32(self: *Builder, value: u32) void {
        self.hasher.update(&.{
            @as(u8, @truncate(value)),
            @as(u8, @truncate(value >> 8)),
            @as(u8, @truncate(value >> 16)),
            @as(u8, @truncate(value >> 24)),
        });
    }
};

fn builtinDecTypeIdent(idents: *const Ident.Store) Ident.Idx {
    return idents.builtinDecTypeIdent();
}

fn builtinStrTypeIdent(idents: *const Ident.Store) Ident.Idx {
    return idents.builtinStrTypeIdent();
}

fn builtinModuleIdent(idents: *const Ident.Store) Ident.Idx {
    return idents.builtinModuleIdent();
}

fn writeIdentText(hasher: *std.crypto.hash.sha2.Sha256, idents: *const Ident.Store, ident: Ident.Idx) void {
    writeByteSlice(hasher, idents.getText(ident));
}

fn writeByteSlice(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32Value(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeBoolValue(hasher: *std.crypto.hash.sha2.Sha256, value: bool) void {
    const byte: u8 = if (value) 1 else 0;
    hasher.update(std.mem.asBytes(&byte));
}

fn writeU32Value(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    hasher.update(&.{
        @as(u8, @truncate(value)),
        @as(u8, @truncate(value >> 8)),
        @as(u8, @truncate(value >> 16)),
        @as(u8, @truncate(value >> 24)),
    });
}

fn invariantViolation(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "canonical type key declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "erroneous checked types have a canonical key" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, 1, 0);
    defer store.deinit();
    const err_var = try store.freshFromContent(.err);

    const first = try fromVar(allocator, &store, &env, err_var);
    const second = try fromVar(allocator, &store, &env, err_var);
    try std.testing.expectEqual(first, second);
}

test "concrete keys default open literal flex vars per kind (numeral -> Dec, quote -> Str)" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    _ = try env.insertIdent(Ident.for_text("Builtin"));
    _ = try env.insertIdent(Ident.for_text("Builtin.Num.Dec"));
    _ = try env.insertIdent(Ident.for_text("Builtin.Str"));
    const from_numeral_ident = try env.insertIdent(Ident.for_text("from_numeral"));
    const from_quote_ident = try env.insertIdent(Ident.for_text("from_quote"));

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const numeral_fn_var = try store.freshFromContent(.{ .flex = types.Flex.init() });
    const numeral_constraints = try store.appendStaticDispatchConstraints(&.{.{
        .fn_name = from_numeral_ident,
        .fn_var = numeral_fn_var,
        .origin = .{ .from_literal = .{ .numeral = types.NumeralInfo.testOnlyInt(1, false, base.Region.zero()) } },
    }});
    const numeral_var = try store.freshFromContent(.{
        .flex = types.Flex.init().withConstraints(numeral_constraints),
    });

    const quote_fn_var = try store.freshFromContent(.{ .flex = types.Flex.init() });
    const quote_constraints = try store.appendStaticDispatchConstraints(&.{.{
        .fn_name = from_quote_ident,
        .fn_var = quote_fn_var,
        .origin = .{ .from_literal = .quote },
    }});
    const quote_var = try store.freshFromContent(.{
        .flex = types.Flex.init().withConstraints(quote_constraints),
    });

    const numeral_key = try fromConcreteVar(allocator, &store, &env, numeral_var);
    const quote_key = try fromConcreteVar(allocator, &store, &env, quote_var);

    // The two defaults must key as different nominals (Dec vs Str); before
    // per-kind defaulting, a quote-only flex var keyed identically to Dec.
    try std.testing.expect(!std.meta.eql(numeral_key, quote_key));

    // Keying is deterministic per kind.
    const quote_key_again = try fromConcreteVar(allocator, &store, &env, quote_var);
    try std.testing.expect(std.meta.eql(quote_key, quote_key_again));
}

test "source type keys normalize closed empty records to empty record" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const empty = try store.freshFromContent(.{ .structure = .empty_record });
    const fields = try store.appendRecordFields(&.{});
    const closed_empty = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = fields,
        .ext = empty,
    } } });

    const empty_key = try fromVar(allocator, &store, &env, empty);
    const closed_key = try fromVar(allocator, &store, &env, closed_empty);

    try std.testing.expectEqualSlices(u8, empty_key.bytes[0..], closed_key.bytes[0..]);
}

test "record field presence participates in canonical type keys" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    const field_name = try env.insertIdent(Ident.for_text("field"));

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const field_var = try store.freshFromContent(.{ .structure = .empty_record });
    const empty_ext = try store.freshFromContent(.{ .structure = .empty_record });
    const optional_presence = try store.freshFromContent(.{ .field_presence = .optional });
    const required_fields = try store.appendRecordFields(&.{.{
        .name = field_name,
        .presence = .required(field_var),
    }});
    const optional_fields = try store.appendRecordFields(&.{.{
        .name = field_name,
        .presence = .unknown(optional_presence, field_var),
    }});
    const required_record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = required_fields,
        .ext = empty_ext,
    } } });
    const optional_record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = optional_fields,
        .ext = empty_ext,
    } } });
    const required_unbound = try store.freshFromContent(.{ .structure = .{ .record_unbound = required_fields } });
    const optional_unbound = try store.freshFromContent(.{ .structure = .{ .record_unbound = optional_fields } });

    const required_key = try fromVar(allocator, &store, &env, required_record);
    const optional_key = try fromVar(allocator, &store, &env, optional_record);
    const required_unbound_key = try fromVar(allocator, &store, &env, required_unbound);
    const optional_unbound_key = try fromVar(allocator, &store, &env, optional_unbound);
    try std.testing.expect(!std.meta.eql(required_key, optional_key));
    try std.testing.expect(!std.meta.eql(required_unbound_key, optional_unbound_key));
}

test "record field presence is stable across normalized row extensions" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    const first_name = try env.insertIdent(Ident.for_text("first"));
    const second_name = try env.insertIdent(Ident.for_text("second"));

    var store = try TypeStore.initCapacity(allocator, 32, 16);
    defer store.deinit();

    const field_var = try store.freshFromContent(.{ .structure = .empty_record });
    const empty_ext = try store.freshFromContent(.{ .structure = .empty_record });
    const optional_presence = try store.freshFromContent(.{ .field_presence = .optional });
    const flat_fields = try store.appendRecordFields(&.{
        .{ .name = first_name, .presence = .required(field_var) },
        .{ .name = second_name, .presence = .unknown(optional_presence, field_var) },
    });
    const flat_record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = flat_fields,
        .ext = empty_ext,
    } } });

    const tail_fields = try store.appendRecordFields(&.{.{
        .name = second_name,
        .presence = .unknown(optional_presence, field_var),
    }});
    const tail_record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = tail_fields,
        .ext = empty_ext,
    } } });
    const head_fields = try store.appendRecordFields(&.{.{
        .name = first_name,
        .presence = .required(field_var),
    }});
    const extended_record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = head_fields,
        .ext = tail_record,
    } } });

    const flat_key = try fromVar(allocator, &store, &env, flat_record);
    const extended_key = try fromVar(allocator, &store, &env, extended_record);
    try std.testing.expectEqualSlices(u8, flat_key.bytes[0..], extended_key.bytes[0..]);
}

test "source type keys normalize closed empty tag unions to empty tag union" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const empty = try store.freshFromContent(.{ .structure = .empty_tag_union });
    const tags = try store.appendTags(&.{});
    const closed_empty = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = tags,
        .ext = empty,
    } } });

    const empty_key = try fromVar(allocator, &store, &env, empty);
    const closed_key = try fromVar(allocator, &store, &env, closed_empty);

    try std.testing.expectEqualSlices(u8, empty_key.bytes[0..], closed_key.bytes[0..]);
}

test "err-sensitive keys distinguish unrelated erroneous vars and stay stable per var" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, 4, 0);
    defer store.deinit();
    const err_a = try store.freshFromContent(.err);
    const err_b = try store.freshFromContent(.err);

    const key_a = try fromVarErrSensitive(allocator, &store, &env, err_a);
    const key_b = try fromVarErrSensitive(allocator, &store, &env, err_b);
    const key_a_again = try fromVarErrSensitive(allocator, &store, &env, err_a);

    try std.testing.expect(!std.meta.eql(key_a, key_b));
    try std.testing.expect(std.meta.eql(key_a, key_a_again));

    // The plain digest keys every erroneous var identically; the err-sensitive
    // digest must differ from it so the two modes never collide.
    const plain_a = try fromVar(allocator, &store, &env, err_a);
    const plain_b = try fromVar(allocator, &store, &env, err_b);
    try std.testing.expect(std.meta.eql(plain_a, plain_b));
    try std.testing.expect(!std.meta.eql(key_a, plain_a));
}

test "err-sensitive keys match plain keys on error-free types" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, 8, 0);
    defer store.deinit();
    const elem = try store.freshFromContent(.{ .structure = .empty_record });
    const tuple_elems = try store.appendVars(&.{ elem, elem });
    const tuple = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = tuple_elems } } });

    const plain = try fromVar(allocator, &store, &env, tuple);
    const sensitive = try fromVarErrSensitive(allocator, &store, &env, tuple);
    try std.testing.expect(std.meta.eql(plain, sensitive));
}

test "canonical error detection traverses alias arguments" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.setContentIdentity([_]u8{0xA5} ** 32);
    const alias_ident = try env.insertIdent(Ident.for_text("Alias"));

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const backing = try store.freshFromContent(.{ .structure = .empty_record });
    const erroneous_arg = try store.freshFromContent(.err);
    const alias = try store.freshFromContent(try store.mkAlias(
        .{ .ident_idx = alias_ident },
        backing,
        &.{erroneous_arg},
        env.selfModuleIdentity(),
    ));

    try std.testing.expect(try containsError(allocator, &store, &env, alias));
}

// Depth pin for the digest walk. The type instantiator builds graphs whose
// depth is bounded only by heap, and every new dispatch edge digests its
// receiver and its callable, so the digest must survive whatever the copier
// can produce. A 40,000-node spine is past what a per-node native frame can
// hold on any ordinary 8 MiB stack: the recursive walk this replaced
// segfaulted on exactly this chain, while it survived 20,000.
test "canonical type key digests a spine deeper than any native-stack budget" {
    const allocator = std.testing.allocator;
    const depth: u32 = 40000;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();

    var store = try TypeStore.initCapacity(allocator, depth + 8, 8);
    defer store.deinit();

    var current = try store.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        const elems = try store.appendVars(&.{current});
        current = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems } } });
    }

    // Digesting the same spine twice must agree: the frame machine's cycle
    // slots and identity slots are assigned by walk order, so a walk that
    // drifted would key the same type two different ways.
    const first = try fromVar(allocator, &store, &env, current);
    const second = try fromVar(allocator, &store, &env, current);
    try std.testing.expectEqualSlices(u8, first.bytes[0..], second.bytes[0..]);
}
