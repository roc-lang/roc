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
//! exactly as production materializes it. Representation content the graph
//! mints (iterator tiers, generated owners) is not in checked module data, so a
//! type carrying it is emitted here without that content and legitimately
//! differs from the graph until Stage B supplies the interface outputs.
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
/// subset. Recorded by the caller; never a panic. The subset matches the
/// closed population the Slice 5 shadow compares, so recursive cycles and open
/// rows are recorded rather than translated — the recursive-group builder wiring
/// lands with the later flip stages, which is where the population needs it.
pub const SkipReason = enum {
    recursive_cycle,
    pending_or_err,
    numeric_default_unresolved,
    open_row,
    malformed_builtin_arity,
    binder_not_found,
    missing_backing,
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
        };
    }

    pub fn deinit(self: *Translator) void {
        self.logical_memo.deinit();
        self.represented_memo.deinit();
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
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .binding_env = null,
            .scheme_owner_node = checked.checked_residual_disposition_module_body_owner,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(checked_ty);
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
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .binding_env = &env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();

        const result = try walk.node(root);
        try self.represented_memo.put(key, result);
        return result;
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

    /// The logical section 9.4 memo key: the qualified scheme plus the ordered
    /// bound and captured logical ids. Declared and keyed for the flip's
    /// logical/represented split; unused while this stage emits stored form.
    fn logicalDigest(
        scheme: SchemeIdent,
        bound_logical: []const TypeId,
        captured_logical: []const TypeId,
    ) InstantiationDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(&scheme.module_bytes);
        hasher.update(std.mem.asBytes(&scheme.scheme));
        const bound_len: u32 = @intCast(bound_logical.len);
        hasher.update(std.mem.asBytes(&bound_len));
        for (bound_logical) |id| {
            const raw: u32 = @intFromEnum(id);
            hasher.update(std.mem.asBytes(&raw));
        }
        const captured_len: u32 = @intCast(captured_logical.len);
        hasher.update(std.mem.asBytes(&captured_len));
        for (captured_logical) |id| {
            const raw: u32 = @intFromEnum(id);
            hasher.update(std.mem.asBytes(&raw));
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

/// One directed translation walk (reunify.md section 9.2). Carries the active
/// map for cycle detection, the reading cursor (which changes when descending a
/// backing declaration in another module), the optional binder environment for
/// substitution, and the scheme owner node for residual disposition lookup.
const Walk = struct {
    owner: *Translator,
    cursor: ModuleCursor,
    binding_env: ?*const BindingEnvironment,
    scheme_owner_node: u32,
    active: std.AutoHashMap(ActiveNode, void),
    skip_reason: *SkipReason,

    fn skip(self: *Walk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
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

    fn payload(self: *Walk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!TypeId {
        return switch (p) {
            .pending, .err => self.skip(.pending_or_err),
            .flex, .rigid => |v| try self.variable(checked_ty, v),
            .empty_record => try self.owner.store.internRecord(self.owner.target_names, &.{}),
            .empty_tag_union => try self.owner.store.internTagUnion(self.owner.target_names, &.{}),
            .record_unbound => |fields| try self.recordFrom(fields, null),
            .record => |record| try self.recordFrom(record.fields, record.ext),
            .tuple => |items| try self.tupleFrom(items),
            .tag_union => |tag_union| try self.tagUnionFrom(tag_union.tags, tag_union.ext),
            .function => |fn_ty| try self.function(fn_ty),
            .alias => |alias_ty| try self.alias(checked_ty, alias_ty),
            .nominal => |nominal_ty| try self.nominal(checked_ty, nominal_ty),
        };
    }

    /// A residual variable: consult its recorded disposition (reunify.md section
    /// 7.4), then apply the checked default. This matches `materializeUnresolved`
    /// exactly: a numeric default yields the defaulted primitive, a row default
    /// yields the empty record or empty tag union, and an undisposed,
    /// undefaulted residual yields the empty tag union — the same stored shape
    /// the graph materializes for an unresolved variable today.
    fn variable(self: *Walk, checked_ty: checked.CheckedTypeId, v: checked.CheckedTypeVariable) WalkError!TypeId {
        for (self.cursor.view.residualDispositions()) |disposition| {
            if (disposition.scheme_owner_node != self.scheme_owner_node) continue;
            if (disposition.type_id != @intFromEnum(checked_ty)) continue;
            switch (disposition.kind) {
                .uninhabited => return try self.owner.store.internTagUnion(self.owner.target_names, &.{}),
                .contextual => {
                    const target = disposition.contextualTarget() orelse break;
                    return try self.node(target);
                },
            }
        }

        if (v.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                return self.skip(.numeric_default_unresolved);
            return switch (target) {
                .dec => try self.owner.store.internPrimitive(self.owner.target_names, .dec),
                .str => try self.owner.store.internPrimitive(self.owner.target_names, .str),
            };
        }
        if (v.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => try self.owner.store.internRecord(self.owner.target_names, &.{}),
                .empty_tag_union => try self.owner.store.internTagUnion(self.owner.target_names, &.{}),
            };
        }
        return try self.owner.store.internTagUnion(self.owner.target_names, &.{});
    }

    fn function(self: *Walk, fn_ty: checked.CheckedFunctionType) WalkError!TypeId {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.owner.store.internFunc(self.owner.target_names, args.items, ret);
    }

    fn tupleFrom(self: *Walk, items: []const checked.CheckedTypeId) WalkError!TypeId {
        var lowered = std.ArrayList(TypeId).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.owner.store.internTuple(self.owner.target_names, lowered.items);
    }

    /// Collect a record's fields, flattening its extension row exactly as
    /// production record lowering does (walk aliases, an empty-record default,
    /// and nested record rows). A row-extension binder substitutes its bound
    /// stored record, whose fields splice into this row.
    fn recordFrom(self: *Walk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!TypeId {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);

        try self.appendRecordFields(&fields, head);

        if (ext) |ext_start| {
            var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
            defer seen.deinit();
            var current = ext_start;
            while (true) {
                if (seen.contains(current)) break;
                try seen.put(current, {});
                if (self.envBinder(current)) |bound| {
                    try self.spliceStoredRecord(&fields, bound);
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
                        try self.appendRecordFields(&fields, tail);
                        break;
                    },
                    .record => |record| {
                        try self.appendRecordFields(&fields, record.fields);
                        current = record.ext;
                    },
                    else => return self.skip(.open_row),
                }
            }
        }

        return try self.owner.store.internRecord(self.owner.target_names, fields.items);
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
        switch (self.owner.store.get(id)) {
            .record => |span| {
                const field_span = self.owner.store.fieldSpan(span);
                for (0..collections.GuardedList.borrowLen(field_span)) |i| {
                    try out.append(self.owner.allocator, collections.GuardedList.at(field_span, i));
                }
            },
            else => return self.skip(.open_row),
        }
    }

    /// Collect a tag union's tags, flattening its extension row exactly as
    /// production tag-union lowering does.
    fn tagUnionFrom(self: *Walk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!TypeId {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);

        try self.appendTags(&tags, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            if (self.envBinder(current)) |bound| {
                try self.spliceStoredTags(&tags, bound);
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
                    try self.appendTags(&tags, tag_union.tags);
                    current = tag_union.ext;
                },
                else => return self.skip(.open_row),
            }
        }

        return try self.owner.store.internTagUnion(self.owner.target_names, tags.items);
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
        switch (self.owner.store.get(id)) {
            .tag_union => |span| {
                const tag_span = self.owner.store.tagSpan(span);
                for (0..collections.GuardedList.borrowLen(tag_span)) |i| {
                    const tag = collections.GuardedList.at(tag_span, i);
                    const payload_span = self.owner.store.span(tag.payloads);
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
        return try self.owner.store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = alias_ty.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, alias_ty.origin_module, alias_ty.name, alias_ty.source_decl),
            .kind = .alias,
            .builtin_owner = null,
            .args = args.items,
            .backing = .{ .ty = backing, .use = .inspectable },
        });
    }

    /// A nominal or opaque. Builtin nominals whose runtime encoding is a
    /// primitive, list, or box lower to that structural shape, matching
    /// production; the rest keep declaration identity as a stored named node with
    /// its backing, dispatch owner, and declared field order. Iterator tier and
    /// generated owner are graph-minted, not in checked module data, so they stay
    /// at their defaults here.
    fn nominal(self: *Walk, checked_ty: checked.CheckedTypeId, n: checked.CheckedNominalType) WalkError!TypeId {
        switch (n.representation) {
            .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                .primitive => |value| return try self.owner.store.internPrimitive(self.owner.target_names, value),
                .list => {
                    if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                    return try self.owner.store.internList(self.owner.target_names, try self.node(n.args[0]));
                },
                .box => {
                    if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                    return try self.owner.store.internBox(self.owner.target_names, try self.node(n.args[0]));
                },
                .bool_tag_union,
                .dict,
                .set,
                .parse_tag_union_spec,
                .fields,
                .field,
                .crypto_sha256_digest,
                .crypto_sha256_hasher,
                .crypto_blake3_digest,
                .crypto_blake3_hasher,
                => {},
            },
            else => {},
        }

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const backing = try self.nominalBacking(n, args.items);
        const declared_order = try self.declaredOrder(n);
        defer self.owner.allocator.free(declared_order);

        return try self.owner.store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = n.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, n.origin_module, n.name, n.source_decl),
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
            .args = args.items,
            .backing = backing,
            .declared_order = declared_order,
        });
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
        defer {
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

test "a self-referential root leaves the subset through the cycle guard" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // A record { self: <this record> }: the field type is the record's own id,
    // so the insert-before-descend guard reaches it a second time.
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
    try testing.expectError(error.Skip, translator.translateGroundRoot(fixture.cursor(), record_ty, &reason));
    try testing.expectEqual(SkipReason.recursive_cycle, reason);
}

test "unconstrained residual variables reach the stored empty tag union" {
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

    var reason: SkipReason = undefined;
    const from_flex = try translator.translateGroundRoot(fixture.cursor(), flex, &reason);
    const from_empty = try translator.translateGroundRoot(fixture.cursor(), empty, &reason);
    try testing.expectEqual(from_empty, from_flex);
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
    try testing.expectEqual(direct, instantiated);
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

test "declarations are referenced" {
    testing.refAllDecls(@This());
}
