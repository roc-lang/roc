//! Deterministic checked-type digests for checked module and post-check boundaries.
//!
//! These keys are produced during checking finalization, while it is still valid
//! to inspect the checked type store and module-local identifiers. Post-check
//! stages consume the resulting keys; they must not recompute them from source
//! syntax or from environment lookup.

const std = @import("std");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const can = @import("can");
const types = @import("types");
const canonical = @import("canonical_names.zig");
const type_key_engine = @import("type_key_engine.zig");

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
    /// Whether the key's encoding wrote no identity or cycle token, so an
    /// enclosing type refers to this type by the key itself.
    composable: bool,
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
    var digester = Digester.init(allocator, store, env);
    defer digester.deinit();
    return try digester.info(var_);
}

/// Build a checker-local shape key while preserving the identity of variables
/// exposed by the enclosing scheme. Variables absent from `anchors` are
/// alpha-normalized, so equivalent private requirement details compare equal;
/// anchored variables include their resolved store identity. This key must
/// never cross a checked-module boundary.
pub fn fromVarWithAnchoredIdentities(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
    anchors: *const std.AutoHashMap(Var, void),
) Allocator.Error!canonical.CanonicalTypeKey {
    var digester = Digester.init(allocator, store, env);
    defer digester.deinit();
    digester.adapter.identity_anchors = anchors;
    digester.adapter.write_identity_names = false;
    return (try digester.info(var_)).key;
}

/// Public `identityVarsFromVar` function.
///
/// The identity variables (flex/rigid) reachable from `var_`, in depth-first
/// first-encounter order over exactly the children a key encodes, entering
/// each variable's constraints. A checked type in a `CheckedTypeStore`
/// enumerates its variables in the same order
/// (`type_key_engine.appendIdentityOrder`), so two representations of the
/// same type pair their identities by index. Caller owns the returned slice.
pub fn identityVarsFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error![]types.Var {
    var builder = Inspector.init(allocator, store, env);
    defer builder.deinit();
    try builder.writeVar(var_);
    return try allocator.dupe(types.Var, builder.identity_variables.items);
}

/// Whether the canonical-key walk enumerates a variable with this descriptor
/// as an identity variable: a flex, a rigid (`#polarity` markers included), or
/// an identity the checker explicitly closed to `[]`. Every producer of a
/// scheme instantiation's substitution pairs exactly these variables.
pub fn isIdentityVariable(desc: types.Descriptor) bool {
    if (desc.flags.empty_tag_union_is_default) return true;
    return switch (desc.content) {
        .flex, .rigid => true,
        .alias, .field_presence, .structure, .err => false,
    };
}

/// Enumerate a complete scheme under one identity numbering. The callable's
/// variables keep their `identityVarsFromVar` order; explicit relation roots
/// append only identities not already reachable from that callable.
pub fn identityVarsFromScheme(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    root: Var,
    relation_roots: []const Var,
) Allocator.Error![]types.Var {
    var builder = Inspector.init(allocator, store, env);
    defer builder.deinit();
    try builder.writeVar(root);
    for (relation_roots) |relation_root| try builder.writeVar(relation_root);
    return try allocator.dupe(types.Var, builder.identity_variables.items);
}

/// Append the identity variables reachable from `var_`, method requirements
/// included, to `out` in `identityVarsFromVar` order.
pub fn appendIdentityVarsFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
    out: *std.ArrayListUnmanaged(types.Var),
) Allocator.Error!void {
    var builder = Inspector.init(allocator, store, env);
    defer builder.deinit();
    try builder.writeVar(var_);
    try out.appendSlice(allocator, builder.identity_variables.items);
}

/// Return the identity variables exposed by a type's ordinary structure,
/// without following method requirements attached to those identities.
pub fn identityVarsFromVarIgnoringConstraints(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error![]types.Var {
    var builder = Inspector.init(allocator, store, env);
    defer builder.deinit();
    builder.walk_identity_constraints = false;
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
    var digester = Digester.init(allocator, store, env);
    defer digester.deinit();
    digester.adapter.err_by_var = true;
    return (try digester.info(var_)).key;
}

/// Public `fromConcreteVar` function.
pub fn fromConcreteVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    var digester = Digester.init(allocator, store, env);
    defer digester.deinit();
    digester.adapter.require_concrete = true;
    return (try digester.info(var_)).key;
}

/// Public `schemeFromVar` function.
pub fn schemeFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeSchemeKey {
    var digester = Digester.init(allocator, store, env);
    defer digester.deinit();
    return schemeKeyForType((try digester.info(var_)).key);
}

/// A source scheme's key: its root type's key in the scheme namespace.
fn schemeKeyForType(key: canonical.CanonicalTypeKey) canonical.CanonicalTypeSchemeKey {
    var hasher = TypeDigestHasher.init();
    hasher.update(&.{@backingInt(KeyTag.canonical_type_scheme)});
    hasher.update(&key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

/// Reusable scratch for complete source-scheme digests within one module.
/// Every request starts from a fresh digest unless the caller promised, with
/// `retainComposedKeys`, that the store no longer changes.
pub const SchemeWriter = struct {
    digester: Digester,
    /// Count complete digest requests in tests; no storage in compiler builds.
    test_digests: if (builtin.is_test) usize else void = if (builtin.is_test) 0 else {},

    /// Bind scratch to the source store and its module-local names.
    pub fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) SchemeWriter {
        return .{ .digester = Digester.init(allocator, store, env) };
    }

    /// Release all retained traversal storage.
    pub fn deinit(self: *SchemeWriter) void {
        self.digester.deinit();
    }

    /// Keep every class and key across requests. Only valid while the source
    /// store is no longer mutated.
    pub fn retainComposedKeys(self: *SchemeWriter) void {
        self.digester.retain = true;
    }

    /// Digest a whole source scheme, including after a failed earlier request.
    pub fn fromVar(self: *SchemeWriter, var_: Var) Allocator.Error!canonical.CanonicalTypeSchemeKey {
        if (builtin.is_test) self.test_digests += 1;
        return schemeKeyForType((try self.digester.info(var_)).key);
    }
};

/// Reusable complete type digests for one source module. Plain requests keep
/// their classes and keys across requests once `retainComposedKeys` promises
/// the store no longer changes; every other request starts fresh.
pub const TypeWriter = struct {
    digester: Digester,
    /// Error-sensitive and anchored requests: different encodings of the same
    /// nodes, so they never share the plain digester's classes and keys.
    mode_digester: Digester,
    inspector: Inspector,
    /// Count scheme digest requests in tests; no storage in compiler builds.
    test_scheme_digests: if (builtin.is_test) usize else void = if (builtin.is_test) 0 else {},

    pub fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) TypeWriter {
        return .{
            .digester = Digester.init(allocator, store, env),
            .mode_digester = Digester.init(allocator, store, env),
            .inspector = Inspector.init(allocator, store, env),
        };
    }

    pub fn deinit(self: *TypeWriter) void {
        self.digester.deinit();
        self.mode_digester.deinit();
        self.inspector.deinit();
    }

    /// Whether requests report rows that repeat a label (see
    /// `takeDuplicateRow`) instead of treating them as an invariant violation.
    pub fn setReportDuplicateRows(self: *TypeWriter, report: bool) void {
        self.digester.adapter.report_duplicate_rows = report;
        self.mode_digester.adapter.report_duplicate_rows = report;
        self.inspector.report_duplicate_rows = report;
    }

    /// The row found repeating a label during the last request, if any. Its
    /// result reflected only each label's first occurrence, so a caller that
    /// receives a row must normalize it and ask again.
    pub fn takeDuplicateRow(self: *TypeWriter) ?Var {
        const row = self.digester.adapter.duplicate_row orelse
            self.mode_digester.adapter.duplicate_row orelse
            self.inspector.duplicate_row;
        self.digester.adapter.duplicate_row = null;
        self.mode_digester.adapter.duplicate_row = null;
        self.inspector.duplicate_row = null;
        return row;
    }

    /// Keep every class and key across requests. Only valid while the source
    /// store is no longer mutated.
    pub fn retainComposedKeys(self: *TypeWriter) void {
        self.digester.retain = true;
    }

    pub fn fromVar(self: *TypeWriter, var_: Var) Allocator.Error!TypeKeyInfo {
        return try self.digester.info(var_);
    }

    /// The scheme key of `var_`, sharing this writer's classes and keys.
    pub fn schemeFromVar(self: *TypeWriter, var_: Var) Allocator.Error!canonical.CanonicalTypeSchemeKey {
        if (builtin.is_test) self.test_scheme_digests += 1;
        return schemeKeyForType((try self.digester.info(var_)).key);
    }

    /// Dispatch-state keys retain the identity of each erroneous root.
    pub fn fromVarErrSensitive(self: *TypeWriter, var_: Var) Allocator.Error!canonical.CanonicalTypeKey {
        self.mode_digester.adapter.err_by_var = true;
        defer self.mode_digester.adapter.err_by_var = false;
        return (try self.mode_digester.info(var_)).key;
    }

    /// Anchor identities only for this checker-local digest.
    /// Keep anchored-identity keys' classes across requests until
    /// `releaseAnchoredKeys`, for a caller whose anchors stay fixed. The caller
    /// calls `invalidateAnchoredKeys` whenever the store changes meanwhile.
    pub fn retainAnchoredKeys(self: *TypeWriter) void {
        self.mode_digester.retain = true;
        self.mode_digester.engine.reset();
    }

    pub fn invalidateAnchoredKeys(self: *TypeWriter) void {
        self.mode_digester.engine.reset();
    }

    pub fn releaseAnchoredKeys(self: *TypeWriter) void {
        self.mode_digester.retain = false;
        self.mode_digester.engine.reset();
    }

    pub fn fromVarWithAnchoredIdentities(self: *TypeWriter, var_: Var, anchors: *const std.AutoHashMap(Var, void)) Allocator.Error!canonical.CanonicalTypeKey {
        self.mode_digester.adapter.identity_anchors = anchors;
        self.mode_digester.adapter.write_identity_names = false;
        defer {
            self.mode_digester.adapter.identity_anchors = null;
            self.mode_digester.adapter.write_identity_names = true;
        }
        return (try self.mode_digester.info(var_)).key;
    }

    /// Inspect exactly the graph traversed by a canonical digest.
    pub fn containsError(self: *TypeWriter, var_: Var) Allocator.Error!bool {
        self.inspector.detect_errors = true;
        defer self.inspector.detect_errors = false;
        self.inspector.resetDigest();
        try self.inspector.writeVar(var_);
        return self.inspector.contains_error;
    }

    /// Return owned identities in the digest's first-encounter order.
    pub fn identityVarsFromVar(self: *TypeWriter, var_: Var) Allocator.Error![]Var {
        self.inspector.resetDigest();
        try self.inspector.writeVar(var_);
        return self.inspector.allocator.dupe(Var, self.inspector.identity_variables.items);
    }

    /// Enumerate a callable and its explicit relations under one fresh
    /// numbering, retaining only scratch capacity between schemes.
    pub fn identityVarsFromScheme(self: *TypeWriter, root: Var, relation_roots: []const Var) Allocator.Error![]Var {
        self.inspector.resetDigest();
        try self.inspector.writeVar(root);
        for (relation_roots) |relation_root| try self.inspector.writeVar(relation_root);
        return self.inspector.allocator.dupe(Var, self.inspector.identity_variables.items);
    }

    /// Enumerate ordinary structure without following identity constraints.
    pub fn identityVarsFromVarIgnoringConstraints(self: *TypeWriter, var_: Var) Allocator.Error![]Var {
        self.inspector.walk_identity_constraints = false;
        defer self.inspector.walk_identity_constraints = true;
        return self.identityVarsFromVar(var_);
    }

    /// Traverse ordinary structure without following identity constraints,
    /// only to surface a row that repeats a label (see `takeDuplicateRow`).
    pub fn visitIgnoringConstraints(self: *TypeWriter, var_: Var) Allocator.Error!void {
        self.inspector.walk_identity_constraints = false;
        defer self.inspector.walk_identity_constraints = true;
        self.inspector.resetDigest();
        try self.inspector.writeVar(var_);
    }

    /// Append one fresh traversal's identities to caller-owned storage.
    pub fn appendIdentityVarsFromVar(self: *TypeWriter, var_: Var, out: *std.ArrayListUnmanaged(Var)) Allocator.Error!void {
        self.inspector.resetDigest();
        try self.inspector.writeVar(var_);
        try out.appendSlice(self.inspector.allocator, self.inspector.identity_variables.items);
    }

    /// Like `appendIdentityVarsFromVar`, treating every var whose resolved
    /// root is in `opaque_roots` as an opaque leaf (see `opaque_roots`).
    pub fn appendIdentityVarsFromVarWithOpaqueRoots(
        self: *TypeWriter,
        var_: Var,
        opaque_roots: []const Var,
        out: *std.ArrayListUnmanaged(Var),
    ) Allocator.Error!void {
        self.inspector.opaque_roots = opaque_roots;
        defer self.inspector.opaque_roots = &.{};
        try self.appendIdentityVarsFromVar(var_, out);
    }
};

/// Whether the canonical-key traversal for `var_` reaches erroneous checked
/// type content. This uses the key builder itself in detection mode, so guards
/// for later key construction cannot accidentally inspect a narrower graph.
pub fn containsError(
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    var_: Var,
) Allocator.Error!bool {
    var builder = Inspector.init(allocator, store, env);
    defer builder.deinit();
    builder.detect_errors = true;
    try builder.writeVar(var_);
    return builder.contains_error;
}

const KeyEngine = type_key_engine.Engine(SourceAdapter);

/// The key tags the engine writes itself, shared by every checked-type key
/// encoder.
pub const key_engine_tags = type_key_engine.Tags{
    .identity_ref = @backingInt(KeyTag.identity_var_ref),
    .cycle = @backingInt(KeyTag.cycle),
    .child_key = @backingInt(KeyTag.child_key),
    .child_key_mapped = @backingInt(KeyTag.child_key_mapped),
};

/// Keys source types through the shared engine. Without `retain`, every
/// request starts from nothing, because the store may have changed since the
/// last one; with it, classes and keys persist across requests.
const Digester = struct {
    adapter: SourceAdapter,
    engine: KeyEngine,
    retain: bool = false,

    fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) Digester {
        return .{
            .adapter = SourceAdapter.init(allocator, store, env),
            .engine = KeyEngine.init(allocator, key_engine_tags),
        };
    }

    fn deinit(self: *Digester) void {
        self.engine.deinit();
        self.adapter.deinit();
    }

    fn info(self: *Digester, var_: Var) Allocator.Error!TypeKeyInfo {
        if (!self.retain) self.engine.reset();
        // A failed request can leave partial classes behind.
        errdefer self.engine.reset();
        const summary = try self.engine.summarize(&self.adapter, @backingInt(var_));
        return .{
            .key = .{ .bytes = summary.key },
            .contains_identity_variables = summary.contains_identity,
            .composable = summary.composable,
        };
    }
};

/// Describes source type-store nodes to the key engine, in the checked-type
/// key encoding: each node's own bytes and its ordered children.
const SourceAdapter = struct {
    allocator: Allocator,
    store: *const TypeStore,
    env: *const ModuleEnv,
    idents: *const Ident.Store,
    require_concrete: bool = false,
    /// Digest erroneous content as its resolved root var instead of one
    /// universal token, so unrelated poisoned positions never key equal.
    err_by_var: bool = false,
    /// Checker-local identities that keep their store identity instead of
    /// being alpha-renamed while comparing private requirement shapes.
    identity_anchors: ?*const std.AutoHashMap(Var, void) = null,
    /// Durable canonical keys preserve source-level variable names. Ephemeral
    /// requirement-shape keys ignore them because names do not affect type
    /// compatibility.
    write_identity_names: bool = true,
    /// Whether a row that repeats a label along its extension chain is
    /// reported to the caller (the checker, which normalizes the row and
    /// asks again) instead of violating the settled-row invariant.
    report_duplicate_rows: bool = false,
    /// The first row found repeating a label since the last report was taken.
    duplicate_row: ?Var = null,
    rank_scratch: base.TextRankCache,
    text_ranks: []const u32 = &.{},
    fields: std.ArrayList(RecordFieldForKey) = .empty,
    tags: std.ArrayList(TagForKey) = .empty,
    field_sort_scratch: std.ArrayList(RecordFieldForKey) = .empty,
    tag_sort_scratch: std.ArrayList(TagForKey) = .empty,
    ext_seen: collections.IndexedStack(Var),

    fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) SourceAdapter {
        return .{
            .allocator = allocator,
            .store = store,
            .env = env,
            .idents = env.getIdentStoreConst(),
            .rank_scratch = base.TextRankCache.init(allocator),
            .ext_seen = collections.IndexedStack(Var).init(allocator),
        };
    }

    fn deinit(self: *SourceAdapter) void {
        self.rank_scratch.deinit();
        self.fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.field_sort_scratch.deinit(self.allocator);
        self.tag_sort_scratch.deinit(self.allocator);
        self.ext_seen.deinit();
    }

    pub fn resolve(self: *SourceAdapter, node: u32) u32 {
        return @backingInt(self.store.resolveVar(@fromBackingInt(@intCast(node))).var_);
    }

    fn child(self: *SourceAdapter, sink: *type_key_engine.Sink, var_: Var) Allocator.Error!void {
        try sink.child(@backingInt(self.store.resolveVar(var_).var_));
    }

    fn tag(sink: *type_key_engine.Sink, comptime key_tag: KeyTag) Allocator.Error!void {
        try sink.byte(@backingInt(key_tag));
    }

    fn ident(self: *SourceAdapter, sink: *type_key_engine.Sink, idx: Ident.Idx) Allocator.Error!void {
        try sink.text(self.idents.getText(idx));
    }

    pub fn describe(self: *SourceAdapter, node: u32, sink: *type_key_engine.Sink) Allocator.Error!type_key_engine.NodeKind {
        const var_: Var = @fromBackingInt(@intCast(node));
        const resolved = self.store.resolveVar(var_);
        if (self.err_by_var and resolved.desc.content == .err) {
            try tag(sink, .err_var);
            try sink.varint(node);
            sink.contains_error = true;
            return .leaf;
        }

        // The checker explicitly records when it closes an otherwise
        // unresolved identity to `[]`; the surviving root is that identity.
        if (resolved.desc.flags.empty_tag_union_is_default) {
            return try self.describeIdentity(sink, var_, .defaulted_empty_tag_union, null, types.StaticDispatchConstraint.SafeList.Range.empty());
        }

        switch (resolved.desc.content) {
            .flex => |flex| {
                if (self.require_concrete) {
                    if (self.flexLiteralDefaultKind(flex)) |kind| {
                        try self.writeLiteralDefault(sink, kind);
                        return .leaf;
                    }
                    invariantViolation("concrete canonical type key requested for unsolved flex type variable");
                }
                return try self.describeIdentity(sink, var_, .flex, flex.name, flex.constraints);
            },
            .rigid => |rigid| {
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
                }
                return try self.describeIdentity(sink, var_, .rigid, rigid.name, rigid.constraints);
            },
            .err => {
                try tag(sink, .err);
                sink.contains_error = true;
            },
            .field_presence => |field_presence| switch (field_presence) {
                .required => try tag(sink, .presence_required),
                .optional => try tag(sink, .presence_optional),
                .defaulted => |id| {
                    try tag(sink, .presence_defaulted);
                    try sink.text(self.env.moduleIdentityHash(id.origin_module));
                    try sink.varint(id.expr_node);
                },
            },
            .alias => |alias| {
                try tag(sink, .alias);
                try self.namedSourceIdentity(sink, alias.origin_module, alias.ident.ident_idx, alias.source_decl.toOptional());
                try self.child(sink, self.store.getAliasBackingVar(alias));
                const args = self.store.sliceAliasArgs(alias);
                try sink.varint(@intCast(args.len));
                for (args) |arg| try self.child(sink, arg);
            },
            .structure => |flat| try self.describeFlat(sink, var_, flat),
        }
        return .content;
    }

    /// A type variable: its header (kind, name, constraint count), then each
    /// constraint's method name, callable, and origin. An anchored variable
    /// instead keeps its store identity as a leaf.
    fn describeIdentity(
        self: *SourceAdapter,
        sink: *type_key_engine.Sink,
        var_: Var,
        comptime key_tag: KeyTag,
        name: ?Ident.Idx,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) Allocator.Error!type_key_engine.NodeKind {
        if (self.identity_anchors) |anchors| {
            if (anchors.contains(var_)) {
                try tag(sink, .identity_var_anchor);
                try sink.varint(@backingInt(var_));
                sink.counts_identity = true;
                return .leaf;
            }
        }
        try tag(sink, key_tag);
        if (self.write_identity_names) {
            try sink.boolean(name != null);
            if (name) |text| try self.ident(sink, text);
        }
        const items = self.store.sliceStaticDispatchConstraints(constraints);
        try sink.varint(@intCast(items.len));
        sink.constraint_count = @intCast(items.len);
        for (items) |constraint| {
            try self.ident(sink, constraint.fn_name);
            try self.child(sink, constraint.fn_var);
            try sink.text(@tagName(constraint.origin));
            try sink.boolean(constraint.origin.binopNegated());
            const maybe_num_literal = constraint.origin.numeralInfo();
            try sink.boolean(maybe_num_literal != null);
            if (maybe_num_literal) |num_literal| try sink.bytes(&num_literal.keyBytes());
        }
        return .identity;
    }

    fn describeFlat(self: *SourceAdapter, sink: *type_key_engine.Sink, var_: Var, flat: types.FlatType) Allocator.Error!void {
        switch (flat) {
            .empty_record => try tag(sink, .empty_record),
            .empty_tag_union => try tag(sink, .empty_tag_union),
            .record => |record| try self.describeRecord(sink, var_, record.fields, record.ext),
            .tuple => |tuple| {
                try tag(sink, .tuple);
                const elems = self.store.sliceVars(tuple.elems);
                try sink.varint(@intCast(elems.len));
                for (elems) |elem| try self.child(sink, elem);
            },
            .nominal_type => |nominal| {
                if (self.store.nominalDeclIsInvalid(nominal)) sink.contains_error = true;
                try tag(sink, .nominal);
                try self.namedSourceIdentity(sink, nominal.origin_module, nominal.ident.ident_idx, nominal.sourceDeclOptional());
                try sink.boolean(nominal.isOpaque());
                const args = self.store.sliceNominalArgs(nominal);
                try sink.varint(@intCast(args.len));
                for (args) |arg| try self.child(sink, arg);
            },
            .fn_pure, .fn_unbound => |func| {
                try tag(sink, .fn_pure);
                try self.describeFunc(sink, func);
            },
            .fn_effectful => |func| {
                try tag(sink, .fn_effectful);
                try self.describeFunc(sink, func);
            },
            .tag_union => |tag_union| try self.describeTagUnion(sink, var_, tag_union.tags, tag_union.ext),
        }
    }

    /// A function's argument count, its arguments, then its return type.
    fn describeFunc(self: *SourceAdapter, sink: *type_key_engine.Sink, func: types.Func) Allocator.Error!void {
        const args = self.store.sliceVars(func.args);
        try sink.varint(@intCast(args.len));
        for (args) |arg| try self.child(sink, arg);
        try self.child(sink, func.ret);
    }

    /// Follow a row's extension chain while it continues the same kind of
    /// row, starting after `row` itself so a chain looping back ends there.
    fn rowTail(self: *SourceAdapter, row: Var, ext: Var, comptime kind: enum { record, tag_union }) Allocator.Error!?Var {
        self.ext_seen.clearRetainingCapacity();
        _ = try self.ext_seen.getOrPush(row);
        var tail: ?Var = ext;
        while (tail) |tail_var| {
            const resolved = self.store.resolveVar(tail_var);
            if (try self.ext_seen.getOrPush(resolved.var_) != null) break;
            if (std.meta.activeTag(resolved.desc.content) != .structure) break;
            const flat = resolved.desc.content.structure;
            switch (kind) {
                .record => switch (flat) {
                    .empty_record => return null,
                    .record => |record| {
                        try self.appendRecordFields(record.fields);
                        tail = record.ext;
                    },
                    .empty_tag_union, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .tag_union => break,
                },
                .tag_union => switch (flat) {
                    .empty_tag_union => return null,
                    .tag_union => |tag_union| {
                        try self.appendTags(tag_union.tags);
                        tail = tag_union.ext;
                    },
                    .empty_record, .record, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound => break,
                },
            }
        }
        return tail;
    }

    fn appendRecordFields(self: *SourceAdapter, range: types.RecordField.SafeMultiList.Range) Allocator.Error!void {
        const slice = self.store.getRecordFieldsSlice(range);
        for (slice.items(.name), slice.items(.presence)) |name, presence| {
            try self.fields.append(self.allocator, .{ .name = name, .presence = presence });
        }
    }

    fn appendTags(self: *SourceAdapter, range: types.Tag.SafeMultiList.Range) Allocator.Error!void {
        const slice = self.store.getTagsSlice(range);
        for (slice.items(.name), slice.items(.args)) |name, args| {
            try self.tags.append(self.allocator, .{ .name = name, .args = args });
        }
    }

    /// Keep the first of each run of equal labels in a sorted row and return
    /// the number kept. A settled row never repeats a label; the checker
    /// asks to be told about a repeated one so it can normalize the row.
    fn dropRepeatedLabels(self: *SourceAdapter, row: Var, comptime Item: type, items: []Item, comptime message: []const u8) usize {
        var kept: usize = 1;
        for (items[1..]) |item| {
            if (self.idents.idxTextEql(items[kept - 1].name, item.name)) {
                if (!self.report_duplicate_rows) invariantViolation(message);
                if (self.duplicate_row == null) self.duplicate_row = row;
                continue;
            }
            items[kept] = item;
            kept += 1;
        }
        return kept;
    }

    fn recordFieldRank(self: *SourceAdapter, field: RecordFieldForKey) u32 {
        return self.text_ranks[field.name.idx];
    }

    fn tagRank(self: *SourceAdapter, tag_for_key: TagForKey) u32 {
        return self.text_ranks[tag_for_key.name.idx];
    }

    /// A record row normalized across its extension chain: fields sorted by
    /// label, then the row's tail.
    fn describeRecord(
        self: *SourceAdapter,
        sink: *type_key_engine.Sink,
        row: Var,
        head: types.RecordField.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!void {
        self.fields.clearRetainingCapacity();
        try self.appendRecordFields(head);
        const tail = try self.rowTail(row, ext, .record);
        var fields = self.fields.items;
        if (fields.len > 1) {
            self.text_ranks = try self.idents.textRanks(&self.rank_scratch);
            try base.TextRankCache.sortByRank(RecordFieldForKey, fields, &self.field_sort_scratch, self.allocator, self, recordFieldRank);
            fields = fields[0..self.dropRepeatedLabels(row, RecordFieldForKey, fields, "canonical type key row normalization found duplicate record fields")];
        }
        if (tail == null and fields.len == 0) {
            try tag(sink, .empty_record);
            return;
        }

        try tag(sink, .record);
        try sink.varint(@intCast(fields.len));
        for (fields) |field| {
            try self.ident(sink, field.name);
            const type_var = switch (field.presence.decode()) {
                .required => |var_| blk: {
                    try sink.boolean(false);
                    break :blk var_;
                },
                .unknown => |unknown| blk: {
                    switch (self.store.resolveVar(unknown.presence).desc.content) {
                        .field_presence => |presence| switch (presence) {
                            .required => try sink.boolean(false),
                            .defaulted => |id| {
                                try tag(sink, .field_default);
                                try sink.text(self.env.moduleIdentityHash(id.origin_module));
                                try sink.varint(id.expr_node);
                            },
                            .optional => try tag(sink, .presence_optional_field),
                        },
                        .flex => {
                            try tag(sink, .presence_variable);
                            try self.child(sink, unknown.presence);
                        },
                        .err => {
                            try tag(sink, .err);
                            sink.contains_error = true;
                        },
                        .rigid, .alias, .structure => invariantViolation("canonical type key reached a field presence variable holding non-presence content"),
                    }
                    break :blk unknown.var_;
                },
            };
            try self.child(sink, type_var);
        }
        if (tail) |tail_var| {
            try self.child(sink, tail_var);
        } else {
            try tag(sink, .empty_record);
        }
    }

    /// A tag-union row normalized across its extension chain: tags sorted by
    /// name with their payloads, then the row's tail.
    fn describeTagUnion(
        self: *SourceAdapter,
        sink: *type_key_engine.Sink,
        row: Var,
        head: types.Tag.SafeMultiList.Range,
        ext: Var,
    ) Allocator.Error!void {
        self.tags.clearRetainingCapacity();
        try self.appendTags(head);
        const tail = try self.rowTail(row, ext, .tag_union);
        var tags = self.tags.items;
        if (tags.len > 1) {
            self.text_ranks = try self.idents.textRanks(&self.rank_scratch);
            try base.TextRankCache.sortByRank(TagForKey, tags, &self.tag_sort_scratch, self.allocator, self, tagRank);
            tags = tags[0..self.dropRepeatedLabels(row, TagForKey, tags, "canonical type key row normalization found duplicate tags")];
        }
        if (tail == null and tags.len == 0) {
            try tag(sink, .empty_tag_union);
            return;
        }

        try tag(sink, .tag_union);
        try sink.varint(@intCast(tags.len));
        for (tags) |tag_for_key| {
            try self.ident(sink, tag_for_key.name);
            const args = self.store.sliceVars(tag_for_key.args);
            try sink.varint(@intCast(args.len));
            for (args) |arg| try self.child(sink, arg);
        }
        if (tail) |tail_var| {
            try self.child(sink, tail_var);
        } else {
            try tag(sink, .empty_tag_union);
        }
    }

    /// Write a named type's source identity: the declaring module's 32-byte
    /// deep CONTENT identity plus the within-module discriminator, mirroring
    /// `sameNominalIdentity` in unify.zig exactly. No name text participates
    /// in the module component, so the digest never depends on coordinator
    /// naming or build directories.
    fn namedSourceIdentity(self: *SourceAdapter, sink: *type_key_engine.Sink, origin_module: base.ModuleIdentity.Idx, name: Ident.Idx, source_decl: ?u32) Allocator.Error!void {
        try sink.text(self.env.moduleIdentityHash(origin_module));
        try sink.boolean(source_decl != null);
        if (source_decl) |decl| {
            try sink.varint(decl);
        } else {
            try self.ident(sink, name);
        }
    }

    /// INVARIANT: a still-open flex may be keyed as the canonical literal
    /// default (Dec for numerals, Str for quotes) ONLY when every constraint on
    /// it is a literal conversion. Any other constraint feeds the checker's
    /// candidate probing, which may commit a non-default candidate; such a var
    /// must already be concrete when a concrete key is requested. Both the
    /// kind and the literal-conversion test come from the defaulting oracle
    /// (src/types/literal_defaulting.zig), so keys cannot disagree with the
    /// checker's defaulting about which vars default.
    fn flexLiteralDefaultKind(self: *SourceAdapter, flex: types.Flex) ?LiteralKind {
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

    fn writeLiteralDefault(self: *SourceAdapter, sink: *type_key_engine.Sink, kind: LiteralKind) Allocator.Error!void {
        try tag(sink, .nominal);
        switch (types.literal_defaulting.defaultTargetForKind(kind)) {
            .dec => try self.ident(sink, builtinDecTypeIdent(self.idents)),
            .str => try self.ident(sink, builtinStrTypeIdent(self.idents)),
        }
        try self.ident(sink, builtinModuleIdent(self.idents));
        try sink.boolean(false);
        try sink.boolean(true);
        try sink.varint(0);
    }
};

const RecordFieldForKey = struct {
    name: Ident.Idx,
    presence: types.RecordField.Presence,
};

const TagForKey = struct {
    name: Ident.Idx,
    args: Var.SafeList.Range,
};

/// One-byte node and field tags of the checked-type key encoding, shared by
/// both key encoders. Values start at 2 so a tag never equals the boolean
/// byte a required record field writes in the same position.
pub const KeyTag = enum(u8) {
    opaque_root = 2,
    err_var,
    flex,
    rigid,
    defaulted_empty_tag_union,
    identity_var_anchor,
    identity_var_ref,
    cycle,
    err,
    presence_required,
    presence_optional,
    presence_defaulted,
    alias,
    nominal,
    empty_record,
    empty_tag_union,
    tuple,
    fn_pure,
    fn_effectful,
    record,
    field_default,
    presence_optional_field,
    presence_variable,
    tag_union,
    canonical_type_scheme,
    child_key,
    named,
    padding,
    /// A reference to a child's key plus the identity variables it shares
    /// with what the enclosing encoding already defined (see
    /// `type_key_engine`).
    child_key_mapped,
};

/// Append a key node's one-byte tag.
pub fn appendKeyTag(buf: *std.ArrayList(u8), allocator: Allocator, tag: KeyTag) Allocator.Error!void {
    try buf.append(allocator, @backingInt(tag));
}

/// Unsigned LEB128. Every integer in the encoding sits at a position its
/// preceding tag fixes, so the self-delimiting form keeps the encoding
/// uniquely decodable.
pub fn appendKeyVarint(buf: *std.ArrayList(u8), allocator: Allocator, value: u32) Allocator.Error!void {
    var rest = value;
    while (rest >= 0x80) : (rest >>= 7) {
        try buf.append(allocator, @as(u8, @truncate(rest)) | 0x80);
    }
    try buf.append(allocator, @truncate(rest));
}

/// Refer to a composable subtree by its key: exactly the bytes the key engine
/// writes for a child whose encoding defines no identity variable and which
/// lies on no cycle.
pub fn writeChildKeyReference(buf: *std.ArrayList(u8), allocator: Allocator, key: canonical.CanonicalTypeKey) Allocator.Error!void {
    try appendKeyTag(buf, allocator, .child_key);
    try buf.appendSlice(allocator, &key.bytes);
}

/// The key of a function node whose arguments and return are all composed
/// subtrees: exactly the bytes a digest walk writes for such a node.
pub fn composedFunctionKey(
    allocator: Allocator,
    effectful: bool,
    arg_keys: []const canonical.CanonicalTypeKey,
    ret_key: canonical.CanonicalTypeKey,
) Allocator.Error!canonical.CanonicalTypeKey {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);
    try appendKeyTag(&buf, allocator, if (effectful) .fn_effectful else .fn_pure);
    try appendKeyVarint(&buf, allocator, @intCast(arg_keys.len));
    for (arg_keys) |key| try writeChildKeyReference(&buf, allocator, key);
    try writeChildKeyReference(&buf, allocator, ret_key);
    return .{ .bytes = TypeDigestHasher.hash(buf.items) };
}

/// Reachability over exactly the children a key encodes, in the same order:
/// identity variables in first-encounter order, erroneous content, and rows
/// that repeat a label. A shared subgraph is visited once, since a second
/// visit cannot reach an identity the first did not.
const Inspector = struct {
    allocator: Allocator,
    adapter: SourceAdapter,
    descriptions: type_key_engine.Store = .{},
    visited: collections.DenseMap(u32, void),
    frames: std.ArrayList(Frame) = .empty,
    identity_variables: std.ArrayList(Var) = .empty,
    /// Structural scheme-interface walks stop at an identity so attached
    /// requirements do not become externally visible anchors.
    walk_identity_constraints: bool = true,
    /// Resolved roots the walk treats as opaque leaves: it neither enters
    /// their content nor enumerates them as identities. A hole-sharing
    /// predeclared scheme passes its live `_` hole vars here, because a hole
    /// is monomorphic within its recursive group and shared by the scheme and
    /// the body, so whatever the group has solved it to so far is not part of
    /// either side's quantified interface.
    opaque_roots: []const Var = &.{},
    detect_errors: bool = false,
    contains_error: bool = false,
    report_duplicate_rows: bool = false,
    duplicate_row: ?Var = null,

    const Frame = struct { desc: type_key_engine.Desc, item: u32 };

    fn init(allocator: Allocator, store: *const TypeStore, env: *const ModuleEnv) Inspector {
        return .{
            .allocator = allocator,
            .adapter = SourceAdapter.init(allocator, store, env),
            .visited = collections.DenseMap(u32, void).init(allocator),
        };
    }

    fn deinit(self: *Inspector) void {
        self.adapter.deinit();
        self.descriptions.deinit(self.allocator);
        self.visited.deinit();
        self.frames.deinit(self.allocator);
        self.identity_variables.deinit(self.allocator);
    }

    /// Start a fresh traversal: the store may have changed since the last.
    fn resetDigest(self: *Inspector) void {
        self.descriptions.clearRetainingCapacity();
        self.visited.clearRetainingCapacity();
        self.frames.clearRetainingCapacity();
        self.identity_variables.clearRetainingCapacity();
        self.contains_error = false;
        self.duplicate_row = null;
    }

    /// Visit everything reachable from `var_`, continuing the current
    /// traversal's identity numbering.
    fn writeVar(self: *Inspector, var_: Var) Allocator.Error!void {
        self.adapter.report_duplicate_rows = self.report_duplicate_rows;
        const frames_base = self.frames.items.len;
        errdefer self.frames.items.len = frames_base;
        try self.visit(self.adapter.resolve(@backingInt(var_)));
        while (self.frames.items.len > frames_base) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            const items = self.descriptions.itemsOf(frame.desc);
            if (frame.item >= items.len) {
                self.frames.items.len -= 1;
                continue;
            }
            const item = items[frame.item];
            frame.item += 1;
            if (item.isChild()) try self.visit(item.child);
        }
        if (self.adapter.duplicate_row) |row| {
            if (self.duplicate_row == null) self.duplicate_row = row;
            self.adapter.duplicate_row = null;
        }
    }

    fn visit(self: *Inspector, node: u32) Allocator.Error!void {
        if ((try self.visited.getOrPut(node)).found_existing) return;
        for (self.opaque_roots) |opaque_root| {
            if (@backingInt(opaque_root) == node) return;
        }
        const desc = try type_key_engine.describe(SourceAdapter, &self.adapter, self.allocator, &self.descriptions, node, false);
        if (self.detect_errors and desc.contains_error) self.contains_error = true;
        switch (desc.kind) {
            .identity => {
                try self.identity_variables.append(self.allocator, @fromBackingInt(@intCast(node)));
                if (!self.walk_identity_constraints) return;
            },
            .content => {},
            .leaf => return,
        }
        try self.frames.append(self.allocator, .{ .desc = desc, .item = 0 });
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

fn invariantViolation(comptime message: []const u8) noreturn {
    if (builtin.mode == .debug) {
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
    const required_open = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = required_fields,
        .ext = try store.fresh(),
    } } });
    const optional_open = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = optional_fields,
        .ext = try store.fresh(),
    } } });

    const required_key = try fromVar(allocator, &store, &env, required_record);
    const optional_key = try fromVar(allocator, &store, &env, optional_record);
    const required_open_key = try fromVar(allocator, &store, &env, required_open);
    const optional_open_key = try fromVar(allocator, &store, &env, optional_open);
    try std.testing.expect(!std.meta.eql(required_key, optional_key));
    try std.testing.expect(!std.meta.eql(required_open_key, optional_open_key));
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
    try env.setContentIdentity(@as([32]u8, @splat(0xA5)));
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

    // Digesting the same spine twice must agree: cycle and variable
    // references are assigned by walk order, so a walk that drifted would key
    // the same type two different ways.
    const first = try fromVar(allocator, &store, &env, current);
    const second = try fromVar(allocator, &store, &env, current);
    try std.testing.expectEqualSlices(u8, first.bytes[0..], second.bytes[0..]);
}

test "issue 11128 scheme writer reuses scratch with fresh identity and cycle numbering" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 32, 32);
    defer store.deinit();
    const a = try store.fresh();
    const b = try store.fresh();
    const shared = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ a, a }) } } });
    const separate = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ a, b }) } } });
    const cycle = try store.fresh();
    try store.setVarContent(cycle, .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ b, cycle, a }) } } });
    const roots = [_]Var{ cycle, shared, a, separate, b, cycle, separate, shared };
    var expected: [roots.len]canonical.CanonicalTypeSchemeKey = undefined;
    for (roots, &expected) |root, *key| key.* = try schemeFromVar(gpa, &store, &env, root);
    try std.testing.expect(!std.meta.eql(expected[1], expected[3]));

    var counter = std.testing.FailingAllocator.init(gpa, .{});
    var writer = SchemeWriter.init(counter.allocator(), &store, &env);
    defer writer.deinit();
    for (roots) |root| _ = try writer.fromVar(root);
    const allocated = counter.allocated_bytes;
    for (0..128) |_| {
        for (roots, expected) |root, key| try std.testing.expectEqualDeep(key, try writer.fromVar(root));
    }
    try std.testing.expectEqual(allocated, counter.allocated_bytes);
}

test "issue 11128 scheme writer recovers from every allocation failure" {
    const gpa = std.testing.allocator;
    // Zig 0.17 SafeAllocator can grow a buffer in place depending on its
    // neighbors. Disable resize/remap so every sweep has the same allocation
    // points while retaining SafeAllocator leak and double-free checks.
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 256, 256);
    defer store.deinit();
    var args: [128]Var = undefined;
    for (&args) |*arg| arg.* = try store.fresh();
    const root = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&args) } } });
    const expected = try schemeFromVar(gpa, &store, &env, root);
    const small_expected = try schemeFromVar(gpa, &store, &env, args[0]);
    var successful = std.testing.FailingAllocator.init(gpa, .{ .resize_fail_index = 0 });
    {
        var writer = SchemeWriter.init(successful.allocator(), &store, &env);
        defer writer.deinit();
        _ = try writer.fromVar(root);
    }
    for (0..successful.allocations) |fail_at| {
        var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = fail_at, .resize_fail_index = 0 });
        var writer = SchemeWriter.init(failing.allocator(), &store, &env);
        defer writer.deinit();
        try std.testing.expectError(error.OutOfMemory, writer.fromVar(root));
        failing.fail_index = std.math.maxInt(usize);
        try std.testing.expectEqualDeep(expected, try writer.fromVar(root));
        try std.testing.expectEqualDeep(small_expected, try writer.fromVar(args[0]));
    }
}

test "row ranks preserve keys across sorting thresholds and extension runs" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 128, 128);
    defer store.deinit();
    const empty_record = try store.freshFromContent(.{ .structure = .empty_record });
    const empty_union = try store.freshFromContent(.{ .structure = .empty_tag_union });
    var fields: [512]types.RecordField = undefined;
    var tags: [512]types.Tag = undefined;
    for (0..fields.len) |i| {
        var buffer: [32]u8 = undefined;
        const name = try env.getIdentStore().insert(gpa, Ident.for_text(try std.fmt.bufPrint(&buffer, "field{d:0>2}", .{i})));
        fields[i] = .{ .name = name, .presence = .required(empty_record) };
        tags[i] = .{ .name = name, .args = try store.appendVars(&.{empty_record}) };
    }
    var writer = SchemeWriter.init(gpa, &store, &env);
    defer writer.deinit();
    for ([_]usize{ 2, 16, 17, 48, 255, 256, 257, 512 }) |len| {
        const ordered_fields = try store.appendRecordFields(fields[0..len]);
        const ordered_tags = try store.appendTags(tags[0..len]);
        const record = try store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ordered_fields, .ext = empty_record } } });
        const tag_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = ordered_tags, .ext = empty_union } } });
        const expected_record = try writer.fromVar(record);
        const expected_tags = try writer.fromVar(tag_union);
        std.mem.reverse(types.RecordField, fields[0..len]);
        std.mem.reverse(types.Tag, tags[0..len]);
        const reversed_fields = try store.appendRecordFields(fields[0..len]);
        const reversed_tags = try store.appendTags(tags[0..len]);
        const reversed_record = try store.freshFromContent(.{ .structure = .{ .record = .{ .fields = reversed_fields, .ext = empty_record } } });
        const reversed_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = reversed_tags, .ext = empty_union } } });
        try std.testing.expectEqualDeep(expected_record, try writer.fromVar(reversed_record));
        try std.testing.expectEqualDeep(expected_tags, try writer.fromVar(reversed_union));
        std.mem.reverse(types.RecordField, fields[0..len]);
        std.mem.reverse(types.Tag, tags[0..len]);
        // Two individually sorted runs whose concatenation is out of order.
        const split = len / 2;
        const record_tail = try store.freshFromContent(.{ .structure = .{ .record = .{
            .fields = try store.appendRecordFields(fields[0..split]),
            .ext = empty_record,
        } } });
        const extended_record = try store.freshFromContent(.{ .structure = .{ .record = .{
            .fields = try store.appendRecordFields(fields[split..len]),
            .ext = record_tail,
        } } });
        const tag_tail = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
            .tags = try store.appendTags(tags[0..split]),
            .ext = empty_union,
        } } });
        const extended_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
            .tags = try store.appendTags(tags[split..len]),
            .ext = tag_tail,
        } } });
        try std.testing.expectEqualDeep(expected_record, try writer.fromVar(extended_record));
        try std.testing.expectEqualDeep(expected_tags, try writer.fromVar(extended_union));
    }
}

test "type writer reuses maps and resets complete digests after allocation failure" {
    const gpa = std.testing.allocator;
    // Zig 0.17 SafeAllocator can grow a buffer in place depending on its
    // neighbors. Disable resize/remap so every sweep has the same allocation
    // points while retaining SafeAllocator leak and double-free checks.
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 256, 256);
    defer store.deinit();
    var args: [128]Var = undefined;
    for (&args) |*arg| arg.* = try store.fresh();
    const root = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&args) } } });
    const closed = try store.freshFromContent(.{ .structure = .empty_record });
    const expected = try fromVarInfo(gpa, &store, &env, root);
    const closed_expected = try fromVarInfo(gpa, &store, &env, closed);
    try std.testing.expect(expected.contains_identity_variables);
    try std.testing.expect(!closed_expected.contains_identity_variables);

    var counter = std.testing.FailingAllocator.init(gpa, .{ .resize_fail_index = 0 });
    var writer = TypeWriter.init(counter.allocator(), &store, &env);
    defer writer.deinit();
    _ = try writer.fromVar(root);
    // Every allocation a first request for `root` makes can fail.
    const allocations = counter.allocations;
    _ = try writer.fromVar(closed);
    const allocated = counter.allocated_bytes;
    for (0..128) |_| {
        try std.testing.expectEqualDeep(expected, try writer.fromVar(root));
        try std.testing.expectEqualDeep(closed_expected, try writer.fromVar(closed));
    }
    try std.testing.expectEqual(allocated, counter.allocated_bytes);

    for (0..allocations) |fail_at| {
        var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = fail_at, .resize_fail_index = 0 });
        var retry = TypeWriter.init(failing.allocator(), &store, &env);
        defer retry.deinit();
        try std.testing.expectError(error.OutOfMemory, retry.fromVar(root));
        failing.fail_index = std.math.maxInt(usize);
        try std.testing.expectEqualDeep(closed_expected, try retry.fromVar(closed));
        try std.testing.expectEqualDeep(expected, try retry.fromVar(root));
    }
}

test "type writer resets checker digest modes between requests" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 16, 8);
    defer store.deinit();
    const child = try store.fresh();
    const constraints = try store.appendStaticDispatchConstraints(&.{.{
        .fn_name = try env.insertIdent(Ident.for_text("method")),
        .fn_var = child,
        .origin = .method_call,
    }});
    const root = try store.freshFromContent(.{ .flex = types.Flex.init().withConstraints(constraints) });
    const err = try store.freshFromContent(.err);
    var anchors = std.AutoHashMap(Var, void).init(gpa);
    defer anchors.deinit();
    try anchors.put(root, {});
    var writer = TypeWriter.init(gpa, &store, &env);
    defer writer.deinit();
    const ordinary = try fromVar(gpa, &store, &env, root);
    const plain_error = try fromVar(gpa, &store, &env, err);
    const anchored = try fromVarWithAnchoredIdentities(gpa, &store, &env, root, &anchors);
    const sensitive = try fromVarErrSensitive(gpa, &store, &env, err);
    for (0..3) |_| {
        try std.testing.expectEqualDeep(anchored, try writer.fromVarWithAnchoredIdentities(root, &anchors));
        try std.testing.expectEqualDeep(ordinary, (try writer.fromVar(root)).key);
        try std.testing.expectEqualDeep(sensitive, try writer.fromVarErrSensitive(err));
        try std.testing.expectEqualDeep(plain_error, (try writer.fromVar(err)).key);
        try std.testing.expect(try writer.containsError(err));
        try std.testing.expect(!try writer.containsError(root));
        const exposed = try writer.identityVarsFromVarIgnoringConstraints(root);
        defer gpa.free(exposed);
        try std.testing.expectEqualSlices(Var, &.{root}, exposed);
        const all = try writer.identityVarsFromVar(root);
        defer gpa.free(all);
        try std.testing.expectEqualSlices(Var, &.{ root, child }, all);
        var appended = std.ArrayList(Var).empty;
        defer appended.deinit(gpa);
        try writer.appendIdentityVarsFromVar(root, &appended);
        try writer.appendIdentityVarsFromVar(child, &appended);
        try std.testing.expectEqualSlices(Var, &.{ root, child, child }, appended.items);
    }
}

test "scheme identity enumeration reuses scratch and recovers from every allocation failure" {
    const gpa = std.testing.allocator;
    // Zig 0.17 SafeAllocator can grow a buffer in place depending on its
    // neighbors. Disable resize/remap so every sweep has the same allocation
    // points while retaining SafeAllocator leak and double-free checks.
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 2048, 256);
    defer store.deinit();
    const root = try store.fresh();
    // Relation identities can occupy different sparse pages from the callable.
    for (0..1024) |_| _ = try store.fresh();
    var identities: [128]Var = undefined;
    identities[0] = root;
    for (identities[1..]) |*identity| identity.* = try store.fresh();
    const relation = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&identities) } } });
    const relations = [_]Var{ relation, root, relation };
    const expected = try identityVarsFromScheme(gpa, &store, &env, root, &relations);
    defer gpa.free(expected);
    try std.testing.expectEqualSlices(Var, &identities, expected);

    var counter = std.testing.FailingAllocator.init(gpa, .{ .resize_fail_index = 0 });
    var writer = TypeWriter.init(counter.allocator(), &store, &env);
    defer writer.deinit();
    const warm = try writer.identityVarsFromScheme(root, &relations);
    counter.allocator().free(warm);
    const allocation_count = counter.allocations;
    for (0..32) |_| {
        const allocated = counter.allocated_bytes;
        const result = try writer.identityVarsFromScheme(root, &relations);
        defer counter.allocator().free(result);
        try std.testing.expectEqualSlices(Var, expected, result);
        // Only the caller-owned result allocates after warming the traversal.
        try std.testing.expectEqual(allocated + expected.len * @sizeOf(Var), counter.allocated_bytes);
        const fresh = try writer.identityVarsFromScheme(identities[127], &.{root});
        defer counter.allocator().free(fresh);
        try std.testing.expectEqualSlices(Var, &.{ identities[127], root }, fresh);
    }
    for (0..allocation_count) |fail_at| {
        var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = fail_at, .resize_fail_index = 0 });
        var retry = TypeWriter.init(failing.allocator(), &store, &env);
        defer retry.deinit();
        try std.testing.expectError(error.OutOfMemory, retry.identityVarsFromScheme(root, &relations));
        failing.fail_index = std.math.maxInt(usize);
        const fresh = try retry.identityVarsFromScheme(identities[127], &.{root});
        defer failing.allocator().free(fresh);
        try std.testing.expectEqualSlices(Var, &.{ identities[127], root }, fresh);
        const result = try retry.identityVarsFromScheme(root, &relations);
        defer failing.allocator().free(result);
        try std.testing.expectEqualSlices(Var, expected, result);
    }
}

/// First-encounter identity order for the test graphs below (tuples,
/// functions, records whose tails are identities), traversed directly on the
/// store in the key encoding's child order.
fn referenceIdentityOrder(gpa: Allocator, store: *const TypeStore, env: *const ModuleEnv, roots: []const Var) Allocator.Error![]Var {
    var visited = std.AutoHashMap(Var, void).init(gpa);
    defer visited.deinit();
    var order = std.ArrayList(Var).empty;
    errdefer order.deinit(gpa);
    var stack = std.ArrayList(Var).empty;
    defer stack.deinit(gpa);
    var rank_cache = base.TextRankCache.init(gpa);
    defer rank_cache.deinit();
    const ranks = try env.getIdentStoreConst().textRanks(&rank_cache);
    for (roots) |root| {
        try stack.append(gpa, root);
        while (stack.pop()) |next| {
            const resolved = store.resolveVar(next);
            if ((try visited.getOrPut(resolved.var_)).found_existing) continue;
            var children = std.ArrayList(Var).empty;
            defer children.deinit(gpa);
            switch (resolved.desc.content) {
                .flex, .rigid => try order.append(gpa, resolved.var_),
                .structure => |flat| switch (flat) {
                    .tuple => |tuple| try children.appendSlice(gpa, store.sliceVars(tuple.elems)),
                    .fn_pure => |func| {
                        try children.appendSlice(gpa, store.sliceVars(func.args));
                        try children.append(gpa, func.ret);
                    },
                    .record => |record| {
                        const fields = store.getRecordFieldsSlice(record.fields);
                        var by_name: [3]struct { rank: u32, var_: Var } = undefined;
                        for (fields.items(.name), fields.items(.presence), 0..) |name, presence, i| {
                            by_name[i] = .{ .rank = ranks[name.idx], .var_ = presence.typeVar() };
                        }
                        std.mem.sort(@TypeOf(by_name[0]), by_name[0..fields.len], {}, struct {
                            fn lessThan(_: void, a: @TypeOf(by_name[0]), b: @TypeOf(by_name[0])) bool {
                                return a.rank < b.rank;
                            }
                        }.lessThan);
                        for (by_name[0..fields.len]) |field| try children.append(gpa, field.var_);
                        try children.append(gpa, record.ext);
                    },
                    .empty_record, .empty_tag_union, .nominal_type, .fn_effectful, .fn_unbound, .tag_union => unreachable,
                },
                .alias, .err, .field_presence => unreachable,
            }
            // Visit children in order: push in reverse.
            var i = children.items.len;
            while (i > 0) {
                i -= 1;
                try stack.append(gpa, children.items[i]);
            }
        }
    }
    return try order.toOwnedSlice(gpa);
}

test "inspection enumerates identities in first-encounter key order across shared cyclic graphs" {
    const gpa = std.testing.allocator;
    var rng = std.Random.DefaultPrng.init(11363);
    for (0..200) |_| {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();
        var store = try TypeStore.initCapacity(gpa, 128, 16);
        defer store.deinit();
        var vars: [10]Var = undefined;
        for (&vars) |*v| v.* = try store.fresh();
        // Compose tuples, functions, records, and identities from the same
        // small node pool, including backward edges and shared children.
        for (vars[0..6]) |v| {
            var children: [3]Var = undefined;
            for (&children) |*child| child.* = vars[rng.random().uintLessThan(usize, vars.len)];
            switch (rng.random().uintLessThan(u8, 3)) {
                0 => try store.setVarContent(v, .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&children) } } }),
                1 => try store.setVarContent(v, .{ .structure = .{ .fn_pure = .{ .args = try store.appendVars(children[0..2]), .ret = children[2] } } }),
                2 => {
                    var fields: [3]types.RecordField = undefined;
                    for (&fields, children, [_][]const u8{ "z", "a", "m" }) |*field, child, name| {
                        field.* = .{ .name = try env.insertIdent(Ident.for_text(name)), .presence = .required(child) };
                    }
                    try store.setVarContent(v, .{ .structure = .{ .record = .{
                        .fields = try store.appendRecordFields(&fields),
                        .ext = try store.fresh(),
                    } } });
                },
                else => unreachable,
            }
        }
        var writer = TypeWriter.init(gpa, &store, &env);
        defer writer.deinit();
        // Multiple roots share one identity numbering, as scheme relations do.
        const relations = vars[1..4];
        const expected = try referenceIdentityOrder(gpa, &store, &env, vars[0..4]);
        defer gpa.free(expected);
        const actual = try writer.identityVarsFromScheme(vars[0], relations);
        defer gpa.free(actual);
        try std.testing.expectEqualSlices(Var, expected, actual);
    }
}

test "inspection visits shared graphs once and observes mutations between requests" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 128, 16);
    defer store.deinit();
    const identity = try store.fresh();
    var root = identity;
    // Eighty tiny shared nodes describe an exponentially large unfolding.
    // Inspection must consume the graph, without computing that unfolding.
    for (0..80) |_| {
        root = try store.freshFromContent(.{ .structure = .{ .tuple = .{
            .elems = try store.appendVars(&.{ root, root }),
        } } });
    }
    var writer = TypeWriter.init(gpa, &store, &env);
    defer writer.deinit();
    const identities = try writer.identityVarsFromVar(root);
    defer gpa.free(identities);
    try std.testing.expectEqualSlices(Var, &.{identity}, identities);
    try std.testing.expect(!try writer.containsError(root));
    try std.testing.expect(!try containsError(gpa, &store, &env, root));

    try store.setVarContent(identity, .err);
    try std.testing.expect(try writer.containsError(root));
    try std.testing.expect(try containsError(gpa, &store, &env, root));
    const after = try writer.identityVarsFromVar(root);
    defer gpa.free(after);
    try std.testing.expectEqual(@as(usize, 0), after.len);
}

test "issue 11350 inspection preserves identity slots through constraints and cyclic row extensions" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    try env.setContentIdentity(@as([32]u8, @splat(0xA5)));
    const rigid_name = try env.insertIdent(Ident.for_text("a"));
    const method_name = try env.insertIdent(Ident.for_text("method"));
    const nominal_name = try env.insertIdent(Ident.for_text("Box"));
    const z_name = try env.insertIdent(Ident.for_text("z"));
    const m_name = try env.insertIdent(Ident.for_text("m"));
    const a_name = try env.insertIdent(Ident.for_text("a_field"));
    var store = try TypeStore.initCapacity(gpa, 32, 16);
    defer store.deinit();

    const rigid_private = try store.fresh();
    const flex_private = try store.fresh();
    const rigid = try store.freshFromContent(.{ .rigid = types.Rigid.init(rigid_name).withConstraints(
        try store.appendStaticDispatchConstraints(&.{.{
            .fn_name = method_name,
            .fn_var = rigid_private,
            .origin = .method_call,
        }}),
    ) });
    const flex = try store.freshFromContent(.{ .flex = types.Flex.init().withConstraints(
        try store.appendStaticDispatchConstraints(&.{.{
            .fn_name = method_name,
            .fn_var = flex_private,
            .origin = .method_call,
        }}),
    ) });
    const nominal_arg = try store.fresh();
    const nominal = try store.freshFromContent(try store.mkNominal(
        .{ .ident_idx = nominal_name },
        &.{ flex, nominal_arg },
        env.selfModuleIdentity(),
        false,
    ));
    const tuple = try store.freshFromContent(.{ .structure = .{ .tuple = .{
        .elems = try store.appendVars(&.{ rigid, nominal }),
    } } });
    const root = try store.fresh();
    const empty = try store.freshFromContent(.{ .structure = .empty_record });
    // Normalization must bring the extension's a_field before the head's z.
    // Its m field points back to the active record, exercising cycle handling.
    const tail = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{
            .{ .name = m_name, .presence = .required(root) },
            .{ .name = a_name, .presence = .required(tuple) },
        }),
        .ext = empty,
    } } });
    try store.setVarContent(root, .{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{.{ .name = z_name, .presence = .required(flex) }}),
        .ext = tail,
    } } });

    var writer = TypeWriter.init(gpa, &store, &env);
    defer writer.deinit();
    inline for (.{ true, false }) |walk_constraints| {
        const expected: []const Var = if (walk_constraints)
            &.{ rigid, rigid_private, flex, flex_private, nominal_arg }
        else
            &.{ rigid, flex, nominal_arg };
        const actual = if (walk_constraints)
            try identityVarsFromVar(gpa, &store, &env, root)
        else
            try identityVarsFromVarIgnoringConstraints(gpa, &store, &env, root);
        defer gpa.free(actual);
        try std.testing.expectEqualSlices(Var, expected, actual);
        const reused = if (walk_constraints)
            try writer.identityVarsFromVar(root)
        else
            try writer.identityVarsFromVarIgnoringConstraints(root);
        defer gpa.free(reused);
        try std.testing.expectEqualSlices(Var, expected, reused);
    }
}

test "composed keys are the same whether or not a retaining writer keyed the children first" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const leaf = try store.freshFromContent(.{ .structure = .empty_record });
    const inner = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ leaf, leaf }) } } });
    const middle = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ inner, leaf }) } } });
    const outer = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ middle, inner }) } } });

    const fresh = try fromVarInfo(allocator, &store, &env, outer);
    try std.testing.expect(!fresh.contains_identity_variables);

    var writer = TypeWriter.init(allocator, &store, &env);
    defer writer.deinit();
    writer.retainComposedKeys();
    const inner_key = try writer.fromVar(inner);
    _ = try writer.fromVar(middle);
    try std.testing.expectEqualDeep(fresh, try writer.fromVar(outer));
    try std.testing.expectEqualDeep(inner_key, try fromVarInfo(allocator, &store, &env, inner));

    // A subtree reached through a cycle is walked in place: its cycle
    // references name ancestors, so keying the child first must not change
    // the parent.
    const cyclic = try store.fresh();
    const cyclic_elems = try store.appendVars(&.{ cyclic, leaf });
    try store.setVarContent(cyclic, .{ .structure = .{ .tuple = .{ .elems = cyclic_elems } } });
    const holder = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ cyclic, inner }) } } });
    const holder_fresh = try fromVarInfo(allocator, &store, &env, holder);
    _ = try writer.fromVar(cyclic);
    try std.testing.expectEqualDeep(holder_fresh, try writer.fromVar(holder));
}

test "err-sensitive keys distinguish erroneous content inside closed subtrees" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(allocator, 16, 8);
    defer store.deinit();

    const err_a = try store.freshFromContent(.err);
    const err_b = try store.freshFromContent(.err);
    const leaf = try store.freshFromContent(.{ .structure = .empty_record });
    const inner_a = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ err_a, leaf }) } } });
    const inner_b = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ err_b, leaf }) } } });
    const outer_a = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{inner_a}) } } });
    const outer_b = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{inner_b}) } } });

    try std.testing.expect(std.meta.eql(try fromVar(allocator, &store, &env, outer_a), try fromVar(allocator, &store, &env, outer_b)));
    try std.testing.expect(!std.meta.eql(
        try fromVarErrSensitive(allocator, &store, &env, outer_a),
        try fromVarErrSensitive(allocator, &store, &env, outer_b),
    ));

    // A writer that keyed the erroneous subtree in the plain encoding must not
    // reuse that key for an error-sensitive request.
    var writer = TypeWriter.init(allocator, &store, &env);
    defer writer.deinit();
    writer.retainComposedKeys();
    _ = try writer.fromVar(outer_a);
    _ = try writer.fromVar(outer_b);
    try std.testing.expect(!std.meta.eql(try writer.fromVarErrSensitive(outer_a), try writer.fromVarErrSensitive(outer_b)));
}

/// Test-only reader of the key encoding: it consumes one node per call and
/// fails unless every byte is accounted for by the grammar, so an encoding
/// change that makes two shapes share a byte sequence cannot go unnoticed.
const KeyEncodingReader = struct {
    bytes: []const u8,
    pos: usize = 0,
    tags: std.ArrayList(KeyTag) = .empty,
    allocator: Allocator,

    const Error = Allocator.Error || error{TestUnexpectedResult};

    fn byte(self: *KeyEncodingReader) Error!u8 {
        if (self.pos >= self.bytes.len) return error.TestUnexpectedResult;
        self.pos += 1;
        return self.bytes[self.pos - 1];
    }

    fn tag(self: *KeyEncodingReader) Error!KeyTag {
        const raw = try self.byte();
        const value = std.enums.fromInt(KeyTag, raw) orelse return error.TestUnexpectedResult;
        try self.tags.append(self.allocator, value);
        return value;
    }

    fn varint(self: *KeyEncodingReader) Error!u32 {
        var value: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            const b = try self.byte();
            value |= @as(u32, b & 0x7f) << shift;
            if (b & 0x80 == 0) return value;
            shift += 7;
        }
    }

    fn skip(self: *KeyEncodingReader, len: usize) Error!void {
        if (self.pos + len > self.bytes.len) return error.TestUnexpectedResult;
        self.pos += len;
    }

    fn lengthPrefixed(self: *KeyEncodingReader) Error!void {
        try self.skip(try self.varint());
    }

    fn boolean(self: *KeyEncodingReader) Error!bool {
        return switch (try self.byte()) {
            0 => false,
            1 => true,
            else => error.TestUnexpectedResult,
        };
    }

    fn namedSource(self: *KeyEncodingReader) Error!void {
        try self.lengthPrefixed();
        if (try self.boolean()) {
            _ = try self.varint();
        } else {
            try self.lengthPrefixed();
        }
    }

    fn node(self: *KeyEncodingReader) Error!void {
        switch (try self.tag()) {
            .child_key => try self.skip(32),
            .child_key_mapped => {
                try self.skip(32);
                if (try self.varint() == 0) return error.TestUnexpectedResult;
                for (0..try self.varint()) |_| {
                    _ = try self.varint();
                    if (try self.varint() == 0) return error.TestUnexpectedResult;
                }
            },
            .cycle, .identity_var_ref => if (try self.varint() == 0) return error.TestUnexpectedResult,
            .identity_var_anchor, .err_var, .opaque_root => _ = try self.varint(),
            .flex, .rigid, .defaulted_empty_tag_union => {
                if (try self.boolean()) try self.lengthPrefixed();
                if (try self.varint() != 0) return error.TestUnexpectedResult;
            },
            .err, .presence_required, .presence_optional, .empty_record, .empty_tag_union => {},
            .presence_defaulted => {
                try self.lengthPrefixed();
                _ = try self.varint();
            },
            .alias => {
                try self.namedSource();
                try self.node();
                for (0..try self.varint()) |_| try self.node();
            },
            .nominal => {
                try self.namedSource();
                _ = try self.boolean();
                for (0..try self.varint()) |_| try self.node();
            },
            .tuple => for (0..try self.varint()) |_| try self.node(),
            .fn_pure, .fn_effectful => {
                for (0..try self.varint()) |_| try self.node();
                try self.node();
            },
            .record => {
                for (0..try self.varint()) |_| {
                    try self.lengthPrefixed();
                    switch (self.bytes[self.pos]) {
                        0 => self.pos += 1,
                        else => switch (try self.tag()) {
                            .field_default => {
                                try self.lengthPrefixed();
                                _ = try self.varint();
                            },
                            .presence_optional_field => {},
                            .presence_variable => try self.node(),
                            .opaque_root,
                            .err_var,
                            .flex,
                            .rigid,
                            .defaulted_empty_tag_union,
                            .identity_var_anchor,
                            .identity_var_ref,
                            .cycle,
                            .err,
                            .presence_required,
                            .presence_optional,
                            .presence_defaulted,
                            .alias,
                            .nominal,
                            .empty_record,
                            .empty_tag_union,
                            .tuple,
                            .fn_pure,
                            .fn_effectful,
                            .record,
                            .tag_union,
                            .canonical_type_scheme,
                            .child_key,
                            .child_key_mapped,
                            .named,
                            .padding,
                            => return error.TestUnexpectedResult,
                        },
                    }
                    try self.node();
                }
                try self.node();
            },
            .tag_union => {
                for (0..try self.varint()) |_| {
                    try self.lengthPrefixed();
                    for (0..try self.varint()) |_| try self.node();
                }
                try self.node();
            },
            .field_default, .presence_optional_field, .presence_variable, .canonical_type_scheme, .named, .padding => return error.TestUnexpectedResult,
        }
    }
};

test "key encodings decode back into their tag sequence" {
    const allocator = std.testing.allocator;

    var env = try ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.setContentIdentity(@as([32]u8, @splat(0x5A)));
    const alias_ident = try env.insertIdent(Ident.for_text("Alias"));
    const nominal_ident = try env.insertIdent(Ident.for_text("Wrapper"));
    const a_name = try env.insertIdent(Ident.for_text("a"));
    const b_name = try env.insertIdent(Ident.for_text("b"));
    const some_name = try env.insertIdent(Ident.for_text("Some"));
    const none_name = try env.insertIdent(Ident.for_text("None"));

    var store = try TypeStore.initCapacity(allocator, 32, 16);
    defer store.deinit();
    const empty = try store.freshFromContent(.{ .structure = .empty_record });
    const open = try store.fresh();
    const record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{
            .{ .name = a_name, .presence = .required(empty) },
            .{ .name = b_name, .presence = .required(open) },
        }),
        .ext = open,
    } } });
    const tag_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{
            .{ .name = some_name, .args = try store.appendVars(&.{empty}) },
            .{ .name = none_name, .args = try store.appendVars(&.{}) },
        }),
        .ext = open,
    } } });
    const alias = try store.freshFromContent(try store.mkAlias(.{ .ident_idx = alias_ident }, record, &.{empty}, env.selfModuleIdentity()));
    const nominal = try store.freshFromContent(try store.mkNominal(.{ .ident_idx = nominal_ident }, &.{ open, empty }, env.selfModuleIdentity(), false));
    const function = try store.freshFromContent(.{ .structure = .{ .fn_pure = .{
        .args = try store.appendVars(&.{ record, tag_union }),
        .ret = alias,
    } } });
    const root = try store.freshFromContent(.{ .structure = .{ .tuple = .{
        .elems = try store.appendVars(&.{ nominal, function }),
    } } });

    var digester = Digester.init(allocator, &store, &env);
    defer digester.deinit();
    _ = try digester.info(root);

    // Every class has its own encoding; each must decode completely.
    var reader = KeyEncodingReader{ .bytes = &.{}, .allocator = allocator };
    defer reader.tags.deinit(allocator);
    for (0..digester.engine.classCount()) |class| {
        const bytes = try digester.engine.encodingOf(@intCast(class));
        reader.bytes = bytes;
        reader.pos = 0;
        try reader.node();
        try std.testing.expectEqual(bytes.len, reader.pos);
    }
    for ([_]KeyTag{ .tuple, .nominal, .flex, .child_key, .child_key_mapped, .fn_pure, .record, .identity_var_ref, .tag_union, .alias }) |expected| {
        const found = for (reader.tags.items) |seen| {
            if (seen == expected) break true;
        } else false;
        try std.testing.expect(found);
    }
}

fn testTagUnion(store: *TypeStore, tags: []const types.Tag) Allocator.Error!types.Content {
    return .{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(tags),
        .ext = try store.freshFromContent(.{ .structure = .empty_tag_union }),
    } } };
}

test "a recursive type has one key however many times it is unrolled" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    const nil_name = try env.insertIdent(Ident.for_text("Nil"));
    const cons_name = try env.insertIdent(Ident.for_text("Cons"));
    var store = try TypeStore.initCapacity(gpa, 64, 32);
    defer store.deinit();
    const elem = try store.freshFromContent(.{ .structure = .empty_record });

    // rolled = [Nil, Cons({}, rolled)]
    const rolled = try store.fresh();
    try store.setVarContent(rolled, try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ elem, rolled }) },
    }));
    // once = [Nil, Cons({}, rolled)]: the same infinite type, unrolled once.
    const once = try store.freshFromContent(try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ elem, rolled }) },
    }));
    const twice = try store.freshFromContent(try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ elem, once }) },
    }));
    // A two-node cycle that denotes the same type.
    const left = try store.fresh();
    const right = try store.fresh();
    try store.setVarContent(left, try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ elem, right }) },
    }));
    try store.setVarContent(right, try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ elem, left }) },
    }));
    // A different recursive type: its payload order differs.
    const other = try store.fresh();
    try store.setVarContent(other, try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ other, elem }) },
    }));

    const expected = try fromVar(gpa, &store, &env, rolled);
    try std.testing.expectEqualDeep(expected, try fromVar(gpa, &store, &env, once));
    try std.testing.expectEqualDeep(expected, try fromVar(gpa, &store, &env, twice));
    try std.testing.expectEqualDeep(expected, try fromVar(gpa, &store, &env, left));
    try std.testing.expectEqualDeep(expected, try fromVar(gpa, &store, &env, right));
    try std.testing.expect(!std.meta.eql(expected, try fromVar(gpa, &store, &env, other)));

    // A retaining writer, which keeps classes across requests, agrees in any
    // request order.
    var writer = TypeWriter.init(gpa, &store, &env);
    defer writer.deinit();
    writer.retainComposedKeys();
    for ([_]Var{ twice, left, once, rolled, right }) |var_| {
        try std.testing.expectEqualDeep(expected, (try writer.fromVar(var_)).key);
    }

    // Inside another type, an unrolled layer still denotes the same type.
    const holder_rolled = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ rolled, elem }) } } });
    const holder_once = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ once, elem }) } } });
    try std.testing.expectEqualDeep(try fromVar(gpa, &store, &env, holder_rolled), try fromVar(gpa, &store, &env, holder_once));
}

test "an unrolled recursive type with type variables keeps its variable sharing" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    const nil_name = try env.insertIdent(Ident.for_text("Nil"));
    const cons_name = try env.insertIdent(Ident.for_text("Cons"));
    var store = try TypeStore.initCapacity(gpa, 64, 32);
    defer store.deinit();
    const a = try store.fresh();
    const b = try store.fresh();

    // rolled = [Nil, Cons(a, rolled)]
    const rolled = try store.fresh();
    try store.setVarContent(rolled, try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ a, rolled }) },
    }));
    const same = try store.freshFromContent(try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ a, rolled }) },
    }));
    // The outer layer holds `b`, the inner layers `a`: a different type.
    const mixed = try store.freshFromContent(try testTagUnion(&store, &.{
        .{ .name = nil_name, .args = try store.appendVars(&.{}) },
        .{ .name = cons_name, .args = try store.appendVars(&.{ b, rolled }) },
    }));
    try std.testing.expectEqualDeep(try fromVar(gpa, &store, &env, rolled), try fromVar(gpa, &store, &env, same));
    try std.testing.expect(!std.meta.eql(try fromVar(gpa, &store, &env, rolled), try fromVar(gpa, &store, &env, mixed)));
}

test "keys follow variable sharing across referenced children" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();
    var store = try TypeStore.initCapacity(gpa, 64, 32);
    defer store.deinit();
    const tuple = struct {
        fn of(s: *TypeStore, elems: []const Var) Allocator.Error!Var {
            return try s.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try s.appendVars(elems) } } });
        }
    }.of;
    const a = try store.fresh();
    const b = try store.fresh();
    const c = try store.fresh();
    const d = try store.fresh();

    // ((a, b), a) and its renaming ((c, d), c) are one type; ((a, b), b) is
    // another. The inner pair is a referenced child, so the outer `a` or `b`
    // must be recognized as one of its variables.
    const first = try tuple(&store, &.{ try tuple(&store, &.{ a, b }), a });
    const renamed = try tuple(&store, &.{ try tuple(&store, &.{ c, d }), c });
    const second = try tuple(&store, &.{ try tuple(&store, &.{ a, b }), b });
    const fresh_outer = try tuple(&store, &.{ try tuple(&store, &.{ a, b }), c });
    const first_key = try fromVar(gpa, &store, &env, first);
    try std.testing.expectEqualDeep(first_key, try fromVar(gpa, &store, &env, renamed));
    try std.testing.expect(!std.meta.eql(first_key, try fromVar(gpa, &store, &env, second)));
    try std.testing.expect(!std.meta.eql(first_key, try fromVar(gpa, &store, &env, fresh_outer)));
    try std.testing.expect(!std.meta.eql(try fromVar(gpa, &store, &env, second), try fromVar(gpa, &store, &env, fresh_outer)));

    // (x, x) and (x, y) differ; each equals its renaming.
    try std.testing.expectEqualDeep(try fromVar(gpa, &store, &env, try tuple(&store, &.{ a, a })), try fromVar(gpa, &store, &env, try tuple(&store, &.{ c, c })));
    try std.testing.expect(!std.meta.eql(try fromVar(gpa, &store, &env, try tuple(&store, &.{ a, a })), try fromVar(gpa, &store, &env, try tuple(&store, &.{ a, b }))));

    // Two referenced children sharing a variable, versus not sharing one.
    const shared = try tuple(&store, &.{ try tuple(&store, &.{ a, b }), try tuple(&store, &.{ b, c }) });
    const unshared = try tuple(&store, &.{ try tuple(&store, &.{ a, b }), try tuple(&store, &.{ c, d }) });
    const shared_renamed = try tuple(&store, &.{ try tuple(&store, &.{ c, d }), try tuple(&store, &.{ d, a }) });
    try std.testing.expect(!std.meta.eql(try fromVar(gpa, &store, &env, shared), try fromVar(gpa, &store, &env, unshared)));
    try std.testing.expectEqualDeep(try fromVar(gpa, &store, &env, shared), try fromVar(gpa, &store, &env, shared_renamed));
}

/// Whether two rooted types denote the same (possibly infinite) tree up to a
/// consistent renaming of their type variables: the definition keys encode.
/// Pairs already assumed equal are not revisited (co-induction), and each
/// variable pairs with exactly one other, checked in both directions.
const EqualityOracle = struct {
    store: *const TypeStore,
    env: *const ModuleEnv,
    gpa: Allocator,
    assumed: std.AutoHashMap([2]Var, void),
    forward: std.AutoHashMap(Var, Var),
    backward: std.AutoHashMap(Var, Var),
    pending: std.ArrayList([2]Var) = .empty,

    fn init(gpa: Allocator, store: *const TypeStore, env: *const ModuleEnv) EqualityOracle {
        return .{
            .store = store,
            .env = env,
            .gpa = gpa,
            .assumed = std.AutoHashMap([2]Var, void).init(gpa),
            .forward = std.AutoHashMap(Var, Var).init(gpa),
            .backward = std.AutoHashMap(Var, Var).init(gpa),
        };
    }

    fn deinit(self: *EqualityOracle) void {
        self.assumed.deinit();
        self.forward.deinit();
        self.backward.deinit();
        self.pending.deinit(self.gpa);
    }

    fn equal(self: *EqualityOracle, left: Var, right: Var) Allocator.Error!bool {
        try self.pending.append(self.gpa, .{ left, right });
        while (self.pending.pop()) |pair| {
            if (!try self.step(pair[0], pair[1])) return false;
        }
        return true;
    }

    fn step(self: *EqualityOracle, left_var: Var, right_var: Var) Allocator.Error!bool {
        const left = self.store.resolveVar(left_var);
        const right = self.store.resolveVar(right_var);
        if ((try self.assumed.getOrPut(.{ left.var_, right.var_ })).found_existing) return true;
        switch (left.desc.content) {
            .flex => |left_flex| {
                if (right.desc.content != .flex) return false;
                const forward = try self.forward.getOrPut(left.var_);
                const backward = try self.backward.getOrPut(right.var_);
                if (forward.found_existing or backward.found_existing) {
                    return forward.found_existing and backward.found_existing and
                        forward.value_ptr.* == right.var_ and backward.value_ptr.* == left.var_;
                }
                forward.value_ptr.* = right.var_;
                backward.value_ptr.* = left.var_;
                const left_constraints = self.store.sliceStaticDispatchConstraints(left_flex.constraints);
                const right_constraints = self.store.sliceStaticDispatchConstraints(right.desc.content.flex.constraints);
                if (left_constraints.len != right_constraints.len) return false;
                for (left_constraints, right_constraints) |l, r| {
                    if (!self.env.getIdentStoreConst().idxTextEql(l.fn_name, r.fn_name)) return false;
                    try self.pending.append(self.gpa, .{ l.fn_var, r.fn_var });
                }
                return true;
            },
            .structure => |left_flat| {
                if (right.desc.content != .structure) return false;
                const right_flat = right.desc.content.structure;
                switch (left_flat) {
                    .empty_tag_union => return right_flat == .empty_tag_union,
                    .tuple => |left_tuple| {
                        if (right_flat != .tuple) return false;
                        return try self.pairAll(self.store.sliceVars(left_tuple.elems), self.store.sliceVars(right_flat.tuple.elems));
                    },
                    .fn_pure => |left_fn| {
                        if (right_flat != .fn_pure) return false;
                        try self.pending.append(self.gpa, .{ left_fn.ret, right_flat.fn_pure.ret });
                        return try self.pairAll(self.store.sliceVars(left_fn.args), self.store.sliceVars(right_flat.fn_pure.args));
                    },
                    .tag_union => |left_union| {
                        if (right_flat != .tag_union) return false;
                        const left_tags = self.store.getTagsSlice(left_union.tags);
                        const right_tags = self.store.getTagsSlice(right_flat.tag_union.tags);
                        if (left_tags.len != right_tags.len) return false;
                        for (left_tags.items(.name), left_tags.items(.args), right_tags.items(.name), right_tags.items(.args)) |ln, la, rn, ra| {
                            if (!self.env.getIdentStoreConst().idxTextEql(ln, rn)) return false;
                            if (!try self.pairAll(self.store.sliceVars(la), self.store.sliceVars(ra))) return false;
                        }
                        try self.pending.append(self.gpa, .{ left_union.ext, right_flat.tag_union.ext });
                        return true;
                    },
                    .empty_record, .record, .nominal_type, .fn_effectful, .fn_unbound => unreachable,
                }
            },
            .rigid, .alias, .err, .field_presence => unreachable,
        }
    }

    fn pairAll(self: *EqualityOracle, left: []const Var, right: []const Var) Allocator.Error!bool {
        if (left.len != right.len) return false;
        for (left, right) |l, r| try self.pending.append(self.gpa, .{ l, r });
        return true;
    }
};

test "keys are equal exactly when types are equal up to renaming, over random shared cyclic graphs" {
    const gpa = std.testing.allocator;
    var rng = std.Random.DefaultPrng.init(11801);
    const random = rng.random();
    var equal_pairs: usize = 0;
    for (0..300) |_| {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();
        const names = [_]Ident.Idx{
            try env.insertIdent(Ident.for_text("A")),
            try env.insertIdent(Ident.for_text("B")),
        };
        const method = try env.insertIdent(Ident.for_text("step"));
        var store = try TypeStore.initCapacity(gpa, 256, 64);
        defer store.deinit();

        // A small pool of nodes: constrained and unconstrained variables,
        // tuples, functions, and single-tag unions, with random sharing and
        // cycles. Half the pool is then copied with fresh variables, so equal
        // pairs are common.
        const pool_len = 8;
        var pool: [pool_len * 2]Var = undefined;
        for (&pool) |*v| v.* = try store.fresh();
        var shapes: [pool_len]u8 = undefined;
        var children: [pool_len][2]usize = undefined;
        var tag_choice: [pool_len]usize = undefined;
        for (0..pool_len) |i| {
            shapes[i] = random.uintLessThan(u8, 5);
            children[i] = .{ random.uintLessThan(usize, pool_len), random.uintLessThan(usize, pool_len) };
            tag_choice[i] = random.uintLessThan(usize, names.len);
        }
        const empty_union = try store.freshFromContent(.{ .structure = .empty_tag_union });
        for (0..2) |copy| {
            const base_index = copy * pool_len;
            for (0..pool_len) |i| {
                const self_var = pool[base_index + i];
                const c0 = pool[base_index + children[i][0]];
                const c1 = pool[base_index + children[i][1]];
                const content: types.Content = switch (shapes[i]) {
                    0 => .{ .flex = types.Flex.init() },
                    1 => .{ .flex = types.Flex.init().withConstraints(try store.appendStaticDispatchConstraints(&.{.{
                        .fn_name = method,
                        .fn_var = c0,
                        .origin = .method_call,
                    }})) },
                    2 => .{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ c0, c1 }) } } },
                    3 => .{ .structure = .{ .fn_pure = .{ .args = try store.appendVars(&.{c0}), .ret = c1 } } },
                    4 => .{ .structure = .{ .tag_union = .{
                        .tags = try store.appendTags(&.{.{ .name = names[tag_choice[i]], .args = try store.appendVars(&.{ c0, c1 }) }}),
                        .ext = empty_union,
                    } } },
                    else => unreachable,
                };
                try store.setVarContent(self_var, content);
            }
        }

        var writer = TypeWriter.init(gpa, &store, &env);
        defer writer.deinit();
        writer.retainComposedKeys();
        for (0..24) |_| {
            const left = pool[random.uintLessThan(usize, pool.len)];
            const right = pool[random.uintLessThan(usize, pool.len)];
            var oracle = EqualityOracle.init(gpa, &store, &env);
            defer oracle.deinit();
            const same = try oracle.equal(left, right);
            const keys_equal = std.meta.eql((try writer.fromVar(left)).key, (try writer.fromVar(right)).key);
            // A fresh, non-retaining key must agree with the retained one.
            try std.testing.expectEqualDeep((try writer.fromVar(left)).key, try fromVar(gpa, &store, &env, left));
            if (same != keys_equal) {
                std.debug.print("oracle says {} but keys say {} for {} vs {}; shapes {any} children {any}\n", .{ same, keys_equal, left, right, shapes, children });
                return error.TestUnexpectedResult;
            }
            if (same) equal_pairs += 1;
        }
    }
    // The generator must exercise both outcomes substantially.
    if (equal_pairs <= 500) {
        std.debug.print("only {} equal pairs\n", .{equal_pairs});
        return error.TestUnexpectedResult;
    }
}

test "keying every link of a constrained-variable chain takes work linear in its length" {
    const gpa = std.testing.allocator;
    var work: [2]u64 = undefined;
    for ([_]usize{ 400, 800 }, &work) |len, *out| {
        var env = try ModuleEnv.init(gpa, "");
        defer env.deinit();
        const method = try env.insertIdent(Ident.for_text("map"));
        var store = try TypeStore.initCapacity(gpa, 4 * len + 8, 2 * len + 8);
        defer store.deinit();
        // link_i has a `.map` constraint (link_i, (c_i -> c_i)) -> link_{i+1},
        // the shape an unannotated chain of method calls generalizes to.
        const links = try gpa.alloc(Var, len + 1);
        defer gpa.free(links);
        links[len] = try store.fresh();
        var i = len;
        while (i > 0) {
            i -= 1;
            links[i] = try store.fresh();
            const c = try store.fresh();
            const callback = try store.freshFromContent(.{ .structure = .{ .fn_pure = .{ .args = try store.appendVars(&.{c}), .ret = c } } });
            const callable = try store.freshFromContent(.{ .structure = .{ .fn_pure = .{
                .args = try store.appendVars(&.{ links[i], callback }),
                .ret = links[i + 1],
            } } });
            try store.setVarContent(links[i], .{ .flex = types.Flex.init().withConstraints(try store.appendStaticDispatchConstraints(&.{.{
                .fn_name = method,
                .fn_var = callable,
                .origin = .method_call,
            }})) });
        }
        // Key every link, as checked-module publication does.
        var writer = TypeWriter.init(gpa, &store, &env);
        defer writer.deinit();
        writer.retainComposedKeys();
        for (links) |link| _ = try writer.fromVar(link);
        out.* = writer.digester.engine.work;
    }
    // Doubling the chain at most roughly doubles the work; the quadratic
    // walk of every link's whole remaining chain quadrupled it.
    try std.testing.expect(work[1] * 10 <= work[0] * 25);
}
