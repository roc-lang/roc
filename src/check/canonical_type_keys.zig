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

const Builder = struct {
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    idents: *const Ident.Store,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.ArrayList(Var),
    identity_variables: std.ArrayList(Var),
    require_concrete: bool = false,
    contains_identity_variables: bool = false,
    detect_errors: bool = false,
    contains_error: bool = false,

    fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) Builder {
        return .{
            .allocator = allocator,
            .store = store,
            .env = env,
            .idents = env.getIdentStoreConst(),
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = .empty,
            .identity_variables = .empty,
        };
    }

    fn deinit(self: *Builder) void {
        self.identity_variables.deinit(self.allocator);
        self.active.deinit(self.allocator);
    }

    fn writeVar(self: *Builder, var_: Var) Allocator.Error!void {
        const resolved = self.store.resolveVar(var_);
        const root = resolved.var_;

        // The checker explicitly records when it closes an otherwise
        // unresolved identity to `[]`. Encode the surviving union-find root so
        // every reference to that identity shares one checked type digest.
        if (resolved.desc.flags.empty_tag_union_is_default) {
            try self.writeIdentityVariable(
                root,
                "defaulted_empty_tag_union",
                null,
                types.StaticDispatchConstraint.SafeList.Range.empty(),
            );
            return;
        }

        const content_tag = std.meta.activeTag(resolved.desc.content);
        if (content_tag == .flex) {
            const flex = resolved.desc.content.flex;
            if (self.require_concrete) {
                if (self.flexLiteralDefaultKind(flex)) |kind| {
                    self.writeLiteralDefault(kind);
                    return;
                }
                invariantViolation("concrete canonical type key requested for unsolved flex type variable");
            }
            try self.writeIdentityVariable(root, "flex", flex.name, flex.constraints);
            return;
        }
        if (content_tag == .rigid) {
            const rigid = resolved.desc.content.rigid;
            if (self.require_concrete) {
                invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
            }
            try self.writeIdentityVariable(root, "rigid", rigid.name, rigid.constraints);
            return;
        }

        if (varSlot(self.active.items, root)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return;
        }

        try self.active.append(self.allocator, root);
        errdefer _ = self.active.pop();
        try self.writeContent(resolved.desc.content);
        _ = self.active.pop();
    }

    fn writeIdentityVariable(
        self: *Builder,
        root: Var,
        comptime tag: []const u8,
        name: ?Ident.Idx,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) Allocator.Error!void {
        self.contains_identity_variables = true;
        if (varSlot(self.identity_variables.items, root)) |slot| {
            self.writeTag("identity_var_ref");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.identity_variables.items.len);
        try self.identity_variables.append(self.allocator, root);
        self.writeTag(tag);
        self.writeU32(slot);
        try self.writeOptionalIdent(name);
        try self.writeConstraints(constraints);
    }

    fn varSlot(vars: []const Var, var_: Var) ?u32 {
        for (vars, 0..) |candidate, slot| {
            if (candidate == var_) return @intCast(slot);
        }
        return null;
    }

    fn writeContent(self: *Builder, content: types.Content) Allocator.Error!void {
        switch (content) {
            .err => {
                if (self.detect_errors) self.contains_error = true;
                self.writeTag("err");
            },
            .flex => |flex| {
                if (self.require_concrete) {
                    if (self.flexLiteralDefaultKind(flex)) |kind| {
                        self.writeLiteralDefault(kind);
                        return;
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
                // A resolved presence fact keyed as the content of a field's
                // presence variable (see `writeFieldPresenceForKey`).
                switch (field_presence) {
                    .required => self.writeTag("presence_required"),
                    .optional => self.writeTag("presence_optional"),
                    .defaulted => |id| {
                        // The default identity is part of the canonical key:
                        // two rows defaulting a field differently are
                        // different types (design.md "Defaulted Fields").
                        // Written in CANONICAL form—the declaring module's
                        // content hash, never the env-local identity index,
                        // so the key is stable across environments.
                        self.writeTag("presence_defaulted");
                        self.writeBytes(self.env.moduleIdentityHash(id.origin_module));
                        self.writeU32(id.expr_node);
                    },
                }
            },
            .alias => |alias| {
                self.writeTag("alias");
                self.writeNamedSourceIdentity(alias.origin_module, alias.ident.ident_idx, alias.source_decl.toOptional());
                try self.writeVar(self.store.getAliasBackingVar(alias));
                const args = self.store.sliceAliasArgs(alias);
                self.writeU32(@intCast(args.len));
                for (args) |arg| {
                    try self.writeVar(arg);
                }
            },
            .structure => |flat| try self.writeFlat(flat),
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

    fn writeFlat(self: *Builder, flat: types.FlatType) Allocator.Error!void {
        switch (flat) {
            .empty_record => self.writeTag("empty_record"),
            .empty_tag_union => self.writeTag("[]"),
            .record_unbound => |fields| {
                self.writeTag("record_unbound");
                try self.writeNormalizedRecordFields(fields, null);
            },
            .record => |record| try self.writeNormalizedRecordPayload(record.fields, record.ext),
            .tuple => |tuple| {
                self.writeTag("tuple");
                try self.writeVarRange(tuple.elems);
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
                for (args) |arg| {
                    try self.writeVar(arg);
                }
            },
            .fn_pure, .fn_unbound => |func| {
                self.writeTag("fn_pure");
                try self.writeFunc(func);
            },
            .fn_effectful => |func| {
                self.writeTag("fn_effectful");
                try self.writeFunc(func);
            },
            .tag_union => |tag_union| try self.writeNormalizedTagUnionPayload(tag_union.tags, tag_union.ext),
        }
    }

    fn writeFunc(self: *Builder, func: types.Func) Allocator.Error!void {
        try self.writeVarRange(func.args);
        try self.writeVar(func.ret);
    }

    fn writeVarRange(self: *Builder, range: Var.SafeList.Range) Allocator.Error!void {
        const vars = self.store.sliceVars(range);
        self.writeU32(@intCast(vars.len));
        for (vars) |var_| {
            try self.writeVar(var_);
        }
    }

    const RecordFieldForKey = struct {
        name: Ident.Idx,
        presence: types.RecordField.Presence,
    };

    const TagForKey = struct {
        name: Ident.Idx,
        args: Var.SafeList.Range,
    };

    fn appendRecordFieldsForKey(
        self: *Builder,
        fields: *std.ArrayList(RecordFieldForKey),
        range: types.RecordField.SafeMultiList.Range,
    ) Allocator.Error!void {
        const slice = self.store.getRecordFieldsSlice(range);
        const names = slice.items(.name);
        const presences = slice.items(.presence);
        for (names, presences) |name, presence| {
            try fields.append(self.allocator, .{
                .name = name,
                .presence = presence,
            });
        }
    }

    fn writeNormalizedRecordFields(
        self: *Builder,
        head: types.RecordField.SafeMultiList.Range,
        ext: ?Var,
    ) Allocator.Error!void {
        var fields = std.ArrayList(RecordFieldForKey).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFieldsForKey(&fields, head);

        var tail = ext;
        var seen = std.ArrayList(Var).empty;
        defer seen.deinit(self.allocator);
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            const root = resolved.var_;
            if (varSlot(self.active.items, root) != null) break;
            if (varSlot(seen.items, root) != null) break;
            try seen.append(self.allocator, root);
            const content = resolved.desc.content;
            if (std.meta.activeTag(content) != .structure) break;
            const flat = content.structure;
            const flat_tag = std.meta.activeTag(flat);
            if (flat_tag == .empty_record) {
                tail = null;
                break;
            }
            if (flat_tag == .record) {
                try self.appendRecordFieldsForKey(&fields, flat.record.fields);
                tail = flat.record.ext;
                continue;
            }
            if (flat_tag == .record_unbound) {
                try self.appendRecordFieldsForKey(&fields, flat.record_unbound);
                tail = null;
            }
            break;
        }

        std.mem.sort(RecordFieldForKey, fields.items, self, recordFieldForKeyLessThan);
        self.writeU32(@intCast(fields.items.len));
        for (fields.items, 0..) |field, index| {
            if (index > 0 and self.idents.idxTextEql(fields.items[index - 1].name, field.name)) {
                invariantViolation("canonical type key row normalization found duplicate record fields");
            }
            self.writeIdent(field.name);
            try self.writeFieldPresenceForKey(field.presence);
        }
        if (tail) |tail_var| {
            try self.writeVar(tail_var);
        } else {
            self.writeTag("empty_record");
        }
    }

    fn writeNormalizedRecordPayload(
        self: *Builder,
        head: types.RecordField.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!void {
        var fields = std.ArrayList(RecordFieldForKey).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFieldsForKey(&fields, head);

        var tail: ?Var = ext;
        var seen = std.ArrayList(Var).empty;
        defer seen.deinit(self.allocator);
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            const root = resolved.var_;
            if (varSlot(self.active.items, root) != null) break;
            if (varSlot(seen.items, root) != null) break;
            try seen.append(self.allocator, root);
            const content = resolved.desc.content;
            if (std.meta.activeTag(content) != .structure) break;
            const flat = content.structure;
            const flat_tag = std.meta.activeTag(flat);
            if (flat_tag == .empty_record) {
                tail = null;
                break;
            }
            if (flat_tag == .record) {
                try self.appendRecordFieldsForKey(&fields, flat.record.fields);
                tail = flat.record.ext;
                continue;
            }
            if (flat_tag == .record_unbound) {
                try self.appendRecordFieldsForKey(&fields, flat.record_unbound);
                tail = null;
            }
            break;
        }

        std.mem.sort(RecordFieldForKey, fields.items, self, recordFieldForKeyLessThan);
        if (tail == null and fields.items.len == 0) {
            self.writeTag("empty_record");
            return;
        }

        self.writeTag("record");
        self.writeU32(@intCast(fields.items.len));
        for (fields.items, 0..) |field, index| {
            if (index > 0 and self.idents.idxTextEql(fields.items[index - 1].name, field.name)) {
                invariantViolation("canonical type key row normalization found duplicate record fields");
            }
            self.writeIdent(field.name);
            try self.writeFieldPresenceForKey(field.presence);
        }
        if (tail) |tail_var| {
            try self.writeVar(tail_var);
        } else {
            self.writeTag("empty_record");
        }
    }

    fn appendTagsForKey(
        self: *Builder,
        tags: *std.ArrayList(TagForKey),
        range: types.Tag.SafeMultiList.Range,
    ) Allocator.Error!void {
        const slice = self.store.getTagsSlice(range);
        const names = slice.items(.name);
        const args = slice.items(.args);
        for (names, args) |name, arg_range| {
            try tags.append(self.allocator, .{
                .name = name,
                .args = arg_range,
            });
        }
    }

    fn writeNormalizedTagUnionPayload(
        self: *Builder,
        head: types.Tag.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!void {
        var tags = std.ArrayList(TagForKey).empty;
        defer tags.deinit(self.allocator);
        try self.appendTagsForKey(&tags, head);

        var tail: ?Var = ext;
        var seen = std.ArrayList(Var).empty;
        defer seen.deinit(self.allocator);
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            const root = resolved.var_;
            if (varSlot(self.active.items, root) != null) break;
            if (varSlot(seen.items, root) != null) break;
            try seen.append(self.allocator, root);
            const content = resolved.desc.content;
            if (std.meta.activeTag(content) != .structure) break;
            const flat = content.structure;
            const flat_tag = std.meta.activeTag(flat);
            if (flat_tag == .empty_tag_union) {
                tail = null;
                break;
            }
            if (flat_tag == .tag_union) {
                try self.appendTagsForKey(&tags, flat.tag_union.tags);
                tail = flat.tag_union.ext;
                continue;
            }
            break;
        }

        std.mem.sort(TagForKey, tags.items, self, tagForKeyLessThan);
        if (tail == null and tags.items.len == 0) {
            self.writeTag("[]");
            return;
        }

        self.writeTag("tag_union");
        self.writeU32(@intCast(tags.items.len));
        for (tags.items, 0..) |tag, index| {
            if (index > 0 and self.idents.idxTextEql(tags.items[index - 1].name, tag.name)) {
                invariantViolation("canonical type key row normalization found duplicate tags");
            }
            self.writeIdent(tag.name);
            try self.writeVarRange(tag.args);
        }
        if (tail) |tail_var| {
            try self.writeVar(tail_var);
        } else {
            self.writeTag("[]");
        }
    }

    fn recordFieldForKeyLessThan(self: *Builder, lhs: RecordFieldForKey, rhs: RecordFieldForKey) bool {
        return self.idents.idxTextLessThan(lhs.name, rhs.name);
    }

    fn tagForKeyLessThan(self: *Builder, lhs: TagForKey, rhs: TagForKey) bool {
        return self.idents.idxTextLessThan(lhs.name, rhs.name);
    }

    fn writeConstraints(self: *Builder, range: types.StaticDispatchConstraint.SafeList.Range) Allocator.Error!void {
        const constraints = self.store.sliceStaticDispatchConstraints(range);
        self.writeU32(@intCast(constraints.len));
        for (constraints) |constraint| {
            self.writeIdent(constraint.fn_name);
            try self.writeVar(constraint.fn_var);
            self.writeTag(@tagName(constraint.origin));
            self.writeBool(constraint.origin.binopNegated());
            const maybe_num_literal = constraint.origin.numeralInfo();
            self.writeBool(maybe_num_literal != null);
            if (maybe_num_literal) |num_literal| {
                self.hasher.update(&num_literal.keyBytes());
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

    /// Key both axes of a field's presence: a discriminant so that fields
    /// differing only in kind hash differently, followed by the axis
    /// variable(s). A concrete required field keys its type; an `unknown`
    /// wrapper keys its resolved kind and its type. The variables route back
    /// through `writeVar`, so a kind variable's resolved `field_presence`
    /// content is keyed here rather than by `writeContent`.
    fn writeFieldPresenceForKey(self: *Builder, presence: types.RecordField.Presence) Allocator.Error!void {
        // Field kinds key by their RESOLVED state, byte-for-byte the same as
        // the checked artifact's encoding (`writeCheckedFieldKind`), so
        // solver and checked canonical keys agree: required (concrete
        // `required`, a kind var solved `required`, or a scheme interior's
        // still-flex kind—required-equivalent, see the `.flex` arm) writes
        // `writeBool(false)` + the type; a `defaulted` kind writes the
        // checked `field_default` tag + the declaring module's content hash
        // + the default's expr node + the type; an `optional` kind writes
        // the `presence_optional_field` tag + the type.
        switch (presence.decode()) {
            .required => |type_var| {
                self.writeBool(false);
                try self.writeVar(type_var);
            },
            .unknown => |unknown| switch (self.store.resolveVar(unknown.presence).desc.content) {
                .field_presence => |fp| switch (fp) {
                    .required => {
                        self.writeBool(false);
                        try self.writeVar(unknown.var_);
                    },
                    .defaulted => |id| {
                        self.writeTag("field_default");
                        self.writeBytes(self.env.moduleIdentityHash(id.origin_module));
                        self.writeU32(id.expr_node);
                        try self.writeVar(unknown.var_);
                    },
                    .optional => {
                        self.writeTag("presence_optional_field");
                        try self.writeVar(unknown.var_);
                    },
                },
                // A still-flex kind is a quantified scheme interior. Its
                // identity is part of the type: instantiation may later solve
                // it to required, optional, or defaulted, so collapsing it to
                // required here would collide distinct specialization
                // behavior at the checked boundary.
                .flex => {
                    self.writeTag("presence_variable");
                    try self.writeVar(unknown.presence);
                    try self.writeVar(unknown.var_);
                },
                // A poisoned presence var: a presence mismatch merges to err
                // like every other content (unify.zig `unifyFieldPresence`),
                // so key it as an error the same way `.err` type content is
                // keyed.
                .err => {
                    if (self.detect_errors) self.contains_error = true;
                    self.writeTag("err");
                    try self.writeVar(unknown.var_);
                },
                // A presence variable may only hold a committed `.field_presence`
                // kind, a still-undetermined `.flex`, or a poisoned `.err`. Any
                // other content (structure, alias, rigid, ...) means the
                // presence var was constructed incorrectly.
                .rigid, .alias, .structure => invariantViolation("canonical type key reached a field presence variable holding non-presence content"),
            },
        }
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
