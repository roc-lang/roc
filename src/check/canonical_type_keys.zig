//! Deterministic checked-type digests for checked module and post-check boundaries.
//!
//! These keys are produced during checking finalization, while it is still valid
//! to inspect the checked type store and module-local identifiers. Post-check
//! stages consume the resulting keys; they must not recompute them from source
//! syntax or from environment lookup.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const canonical = @import("canonical_names.zig");

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
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    return (try fromVarInfo(allocator, store, idents, var_)).key;
}

/// Public `fromVarInfo` function.
pub fn fromVarInfo(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!TypeKeyInfo {
    var builder = Builder.init(allocator, store, idents);
    defer builder.deinit();
    try builder.writeVar(var_);
    return .{
        .key = .{ .bytes = builder.hasher.finalResult() },
        .contains_identity_variables = builder.contains_identity_variables,
    };
}

/// Public `fromConcreteVar` function.
pub fn fromConcreteVar(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    var builder = Builder.init(allocator, store, idents);
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
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeSchemeKey {
    var builder = Builder.init(allocator, store, idents);
    defer builder.deinit();
    builder.writeTag("canonical_type_scheme");
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

const Builder = struct {
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.ArrayList(Var),
    identity_variables: std.ArrayList(Var),
    require_concrete: bool = false,
    contains_identity_variables: bool = false,

    fn init(allocator: Allocator, store: *const TypeStore, idents: *const Ident.Store) Builder {
        return .{
            .allocator = allocator,
            .store = store,
            .idents = idents,
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

        switch (resolved.desc.content) {
            .flex => |flex| {
                if (self.require_concrete) {
                    if (self.flexLiteralDefaultKind(flex)) |kind| {
                        self.writeLiteralDefault(kind);
                        return;
                    }
                    invariantViolation("concrete canonical type key requested for unsolved flex type variable");
                }
                try self.writeIdentityVariable(root, "flex", flex.name, flex.constraints);
                return;
            },
            .rigid => |rigid| {
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
                }
                try self.writeIdentityVariable(root, "rigid", rigid.name, rigid.constraints);
                return;
            },
            else => {},
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
            .err => invariantViolation("canonical type key requested for erroneous checked type"),
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
    /// default (Dec for numerals, Str for quotes) ONLY when every constraint
    /// on it came directly from literal conversion — a pure,
    /// otherwise-unconstrained literal is exactly what the checker's defaulting
    /// commits to the kind's default.
    /// Any OTHER constraint origin (binop/method/where-clause usage) feeds the
    /// checker's candidate probing, which may commit a non-default candidate
    /// (e.g. an integer-only method commits I64); such a var must already be
    /// concrete when a concrete key is requested, so finding one still open
    /// here means an upstream defaulting step was skipped — keying it as the
    /// default would be a guess, so we raise an invariant violation instead.
    ///
    /// A mixed-kind set (both numeral and quote literal-origin constraints,
    /// reachable only via a flex/flex merge the checker reports as a type
    /// error, so it never survives to key generation) deterministically picks
    /// `numeral`. The precedence is enforced by
    /// `StaticDispatchConstraint.dominantLiteralKind`, the single source of
    /// truth shared with `varLiteralKind` (Check.zig) and
    /// `numericDefaultPhaseForConstraints` (checked_artifact.zig).
    fn flexLiteralDefaultKind(self: *Builder, flex: types.Flex) ?LiteralKind {
        const constraints = self.store.sliceStaticDispatchConstraints(flex.constraints);
        const kind = types.StaticDispatchConstraint.dominantLiteralKind(constraints);
        var has_other = false;
        for (constraints) |constraint| {
            switch (constraint.origin) {
                .from_literal => {},
                else => has_other = true,
            }
        }
        if (kind != null and has_other) {
            invariantViolation("concrete canonical type key requested for an open literal with non-literal constraints (defaulting was skipped)");
        }
        return kind;
    }

    fn writeLiteralDefault(self: *Builder, kind: LiteralKind) void {
        self.writeTag("nominal");
        switch (kind) {
            .numeral => self.writeIdent(builtinDecTypeIdent(self.idents)),
            .quote, .interpolation => self.writeIdent(builtinStrTypeIdent(self.idents)),
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
        self.writeBool(func.needs_instantiation);
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
        var_: Var,
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
        const vars = slice.items(.var_);
        for (names, vars) |name, var_| {
            try fields.append(self.allocator, .{
                .name = name,
                .var_ = var_,
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
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .empty_record => {
                        tail = null;
                        break;
                    },
                    .record => |record| {
                        try self.appendRecordFieldsForKey(&fields, record.fields);
                        tail = record.ext;
                    },
                    .record_unbound => |record_fields| {
                        try self.appendRecordFieldsForKey(&fields, record_fields);
                        tail = null;
                    },
                    else => break,
                },
                else => break,
            }
        }

        std.mem.sort(RecordFieldForKey, fields.items, self, recordFieldForKeyLessThan);
        self.writeU32(@intCast(fields.items.len));
        for (fields.items, 0..) |field, index| {
            if (index > 0 and self.idents.idxTextEql(fields.items[index - 1].name, field.name)) {
                invariantViolation("canonical type key row normalization found duplicate record fields");
            }
            self.writeIdent(field.name);
            try self.writeVar(field.var_);
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
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .empty_record => {
                        tail = null;
                        break;
                    },
                    .record => |record| {
                        try self.appendRecordFieldsForKey(&fields, record.fields);
                        tail = record.ext;
                    },
                    .record_unbound => |record_fields| {
                        try self.appendRecordFieldsForKey(&fields, record_fields);
                        tail = null;
                    },
                    else => break,
                },
                else => break,
            }
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
            try self.writeVar(field.var_);
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
            switch (resolved.desc.content) {
                .structure => |flat| switch (flat) {
                    .empty_tag_union => {
                        tail = null;
                        break;
                    },
                    .tag_union => |tag_union| {
                        try self.appendTagsForKey(&tags, tag_union.tags);
                        tail = tag_union.ext;
                    },
                    else => break,
                },
                else => break,
            }
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
                self.hasher.update(&num_literal.bytes);
                self.writeBool(num_literal.is_u128);
                self.writeBool(num_literal.is_negative);
                self.writeBool(num_literal.is_fractional);
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

    fn writeNamedSourceIdentity(self: *Builder, origin_module: Ident.Idx, ident: Ident.Idx, source_decl: ?u32) void {
        self.writeIdent(origin_module);
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

test "concrete keys default open literal flex vars per kind (numeral -> Dec, quote -> Str)" {
    const allocator = std.testing.allocator;

    var idents = try Ident.Store.initCapacity(allocator, 8);
    defer idents.deinit(allocator);
    _ = try idents.insert(allocator, Ident.for_text("Builtin"));
    _ = try idents.insert(allocator, Ident.for_text("Builtin.Num.Dec"));
    _ = try idents.insert(allocator, Ident.for_text("Builtin.Str"));
    const from_numeral_ident = try idents.insert(allocator, Ident.for_text("from_numeral"));
    const from_quote_ident = try idents.insert(allocator, Ident.for_text("from_quote"));

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const numeral_fn_var = try store.freshFromContent(.{ .flex = types.Flex.init() });
    const numeral_constraints = try store.appendStaticDispatchConstraints(&.{.{
        .fn_name = from_numeral_ident,
        .fn_var = numeral_fn_var,
        .origin = .{ .from_literal = .{ .numeral = types.NumeralInfo.fromI128(1, false, false, base.Region.zero()) } },
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

    const numeral_key = try fromConcreteVar(allocator, &store, &idents, numeral_var);
    const quote_key = try fromConcreteVar(allocator, &store, &idents, quote_var);

    // The two defaults must key as different nominals (Dec vs Str); before
    // per-kind defaulting, a quote-only flex var keyed identically to Dec.
    try std.testing.expect(!std.meta.eql(numeral_key, quote_key));

    // Keying is deterministic per kind.
    const quote_key_again = try fromConcreteVar(allocator, &store, &idents, quote_var);
    try std.testing.expect(std.meta.eql(quote_key, quote_key_again));
}

test "source type keys normalize closed empty records to empty record" {
    const allocator = std.testing.allocator;

    var idents = try Ident.Store.initCapacity(allocator, 4);
    defer idents.deinit(allocator);

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const empty = try store.freshFromContent(.{ .structure = .empty_record });
    const fields = try store.appendRecordFields(&.{});
    const closed_empty = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = fields,
        .ext = empty,
    } } });

    const empty_key = try fromVar(allocator, &store, &idents, empty);
    const closed_key = try fromVar(allocator, &store, &idents, closed_empty);

    try std.testing.expectEqualSlices(u8, empty_key.bytes[0..], closed_key.bytes[0..]);
}

test "source type keys normalize closed empty tag unions to empty tag union" {
    const allocator = std.testing.allocator;

    var idents = try Ident.Store.initCapacity(allocator, 4);
    defer idents.deinit(allocator);

    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const empty = try store.freshFromContent(.{ .structure = .empty_tag_union });
    const tags = try store.appendTags(&.{});
    const closed_empty = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = tags,
        .ext = empty,
    } } });

    const empty_key = try fromVar(allocator, &store, &idents, empty);
    const closed_key = try fromVar(allocator, &store, &idents, closed_empty);

    try std.testing.expectEqualSlices(u8, empty_key.bytes[0..], closed_key.bytes[0..]);
}
