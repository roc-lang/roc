//! Eager logical type identity for directed scheme instantiation (reunify.md
//! section 8.2, section 9), built as a state-isolated shadow (reunify.md Slice
//! 5). Nothing here feeds production lowering: every id this module mints lives
//! in its own store, and the Slice 5 shadow only compares digests.
//!
//! A `LogicalTypeIdentity` is the source-level identity of a checked type after
//! two erasures: representation content (iterator tier/kind/depth, generated
//! owner, builtin dispatch owner) and every backed source alias are removed, so
//! only the primitive/record/tag/tuple/function shape plus nominal declaration
//! identity survives. That erased skeleton is interned in this module's own
//! bucket-enabled Monotype store, so O(1) equality is `id == id` (reunify.md
//! section 8.2).
//!
//! Both a frozen checked type (`checkedLogicalIdentity`) and an already-lowered
//! Monotype type (`monoLogicalIdentity`) erase to the same skeleton, because
//! both walks resolve every name through a text/module-content-hash interner
//! this store owns. That is the neutral ground on which the two id spaces meet:
//! a checked field label and a Monotype field label with the same text intern
//! to one shadow id, so logically-equal types on either side share one
//! `LogicalTypeIdentity`.
//!
//! Recursion (reunify.md section 8.3): a checked or Monotype cycle is detected
//! with an insert-before-descend active map and is left outside the Slice 5
//! closed subset (the caller records the skip). The recursive-group interner
//! that gives rooted cycles one id arrives with Slice 6's lifecycle.

const std = @import("std");
const Allocator = std.mem.Allocator;

const check = @import("check");
const collections = @import("collections");

const MonoType = @import("../monotype/type.zig");

const GuardedList = collections.GuardedList;

const names = check.CheckedNames;
const checked = check.CheckedModule;

/// The interned id of an erased logical skeleton. It is a `TypeId` in this
/// module's own store, never serialized, never handed to production. Equality
/// is exact logical equality within one shadow run (reunify.md section 8.2).
pub const LogicalTypeIdentity = MonoType.TypeId;

/// One arm of the `TypeHandle` union carries a live occurrence; for the Slice 5
/// closed subset only the interned arm is populated. A named type's erased
/// occurrence id is meaningless (occurrence identity is dropped), so it is set
/// to this fixed index rather than any real checked id.
const erased_occurrence_index: u32 = 0;
const erased_occurrence: checked.CheckedTypeId = @enumFromInt(erased_occurrence_index);

/// A binder's value in a `BindingEnvironment` (reunify.md section 7.3). The
/// logical half keys substitution and instantiation memos; the representation
/// half feeds drafts. For the closed subset a value carries no open
/// representation, so the representation handle is the interned logical id
/// itself (reunify.md section 9.1); the draft and slot arms arrive with Slice
/// 6's lifecycle.
pub const BoundType = struct {
    logical: LogicalTypeIdentity,
    representation: TypeHandle,

    /// Build a closed bound value whose representation half is the logical id.
    pub fn closed(logical: LogicalTypeIdentity) BoundType {
        return .{ .logical = logical, .representation = .{ .interned = logical } };
    }
};

/// A reference to a type under construction (reunify.md section 9.1). The union
/// shape is kept so Slice 6 extends it with `draft` and `representation_slot`
/// arms; for the closed subset only `interned` is populated.
pub const TypeHandle = union(enum) {
    /// An immutable interned skeleton id.
    interned: LogicalTypeIdentity,
};

/// A scheme reference qualified by its owning module's content identity, so a
/// module-local scheme id never collides across modules (reunify.md section
/// 9.4 memo key).
pub const SchemeIdent = struct {
    module_bytes: [32]u8,
    scheme: u32,
};

/// The bound values for one active specialization's binders, linked lexically
/// for a nested scheme (reunify.md section 7.3). For the Slice 5 closed subset
/// `parent` is null and `captured` is empty (no nested closures); the fields
/// are present so Slice 6 extends rather than rewrites.
pub const BindingEnvironment = struct {
    scheme: SchemeIdent,
    /// The scheme's ordered binders, translated in place within its root; a
    /// walk substitutes `bound[i]` when it reaches `binders[i]`.
    binders: []const checked.CheckedTypeId,
    /// Dense, one per binder, no null entries.
    bound: []const BoundType,
    /// Values for the scheme's captured enclosing binders, in order.
    captured: []const BoundType,
    parent: ?*const BindingEnvironment,

    fn binderIndex(self: BindingEnvironment, checked_ty: checked.CheckedTypeId) ?usize {
        for (self.binders, 0..) |binder, index| {
            if (binder == checked_ty) return index;
        }
        return null;
    }
};

/// Why a checked root or instantiation edge fell outside the Slice 5 closed
/// subset. Recorded by the caller as `shadow_skipped_<reason>`; never a panic.
pub const SkipReason = enum {
    recursive_cycle,
    representation_bearing,
    pending_or_err,
    numeric_default_unresolved,
    zero_sized_or_erased,
    alias_without_backing,
    malformed_builtin_arity,
    open_row,
    binder_not_found,
};

/// A walk left the closed subset (with `reason` recorded on the walker), or the
/// shadow store ran out of memory. Never a control-flow signal into production.
pub const WalkError = error{Skip} || Allocator.Error;

/// Digest key of a `LogicalInstantiationMemo` entry (reunify.md section 9.4):
/// the qualified scheme plus the ordered bound and captured logical ids. Two
/// instantiations collide only when scheme, bindings, and captures all match.
const InstantiationDigest = [32]u8;

/// The state-isolated skeleton store: its own name interner and its own
/// bucket-enabled Monotype store, plus the two memo classes of reunify.md
/// section 9.4. Owns everything it allocates; destroying it has no effect on
/// production.
pub const LogicalStore = struct {
    allocator: Allocator,
    /// Text/module-content-hash interner shared by both erasure walks so their
    /// two id spaces meet on neutral ground.
    shadow_names: names.NameStore,
    /// Bucket-enabled Monotype store; interning gives logically-equal skeletons
    /// one id (reunify.md section 8.2, section 8.3).
    store: MonoType.Store,
    /// LogicalInstantiationMemo (reunify.md section 9.4): keyed by qualified
    /// scheme plus ordered bound and captured logical ids, valued by the
    /// instantiated skeleton. Never holds a draft or a slot.
    instantiation_memo: std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity),
    /// SealedRepresentationMemo (reunify.md section 9.4): declared for the Slice
    /// 6 lifecycle, unused for the closed subset. Its value type is the interned
    /// id; its key adds finalized representation-input digests, which do not
    /// exist yet.
    sealed_representation_memo: std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity),

    pub fn init(allocator: Allocator) LogicalStore {
        var store = MonoType.Store.init(allocator);
        store.enableInterning();
        return .{
            .allocator = allocator,
            .shadow_names = names.NameStore.init(allocator),
            .store = store,
            .instantiation_memo = std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity).init(allocator),
            .sealed_representation_memo = std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity).init(allocator),
        };
    }

    pub fn deinit(self: *LogicalStore) void {
        self.sealed_representation_memo.deinit();
        self.instantiation_memo.deinit();
        self.store.deinit();
        self.shadow_names.deinit();
    }

    /// The 32-byte content digest of a skeleton, for bounded mismatch detail.
    pub fn digestBytes(self: *LogicalStore, id: LogicalTypeIdentity) [32]u8 {
        return self.store.typeDigest(&self.shadow_names, id).bytes;
    }

    // --- Skeleton constructors (each interns into the shadow store) ---

    fn primitive(self: *LogicalStore, value: MonoType.Primitive) WalkError!LogicalTypeIdentity {
        return try self.store.internPrimitive(&self.shadow_names, value);
    }

    fn zst(self: *LogicalStore) WalkError!LogicalTypeIdentity {
        return try self.store.internZst(&self.shadow_names);
    }

    fn list(self: *LogicalStore, elem: LogicalTypeIdentity) WalkError!LogicalTypeIdentity {
        return try self.store.internList(&self.shadow_names, elem);
    }

    fn box(self: *LogicalStore, elem: LogicalTypeIdentity) WalkError!LogicalTypeIdentity {
        return try self.store.internBox(&self.shadow_names, elem);
    }

    fn tuple(self: *LogicalStore, items: []const LogicalTypeIdentity) WalkError!LogicalTypeIdentity {
        return try self.store.internTuple(&self.shadow_names, items);
    }

    fn func(self: *LogicalStore, args: []const LogicalTypeIdentity, ret: LogicalTypeIdentity) WalkError!LogicalTypeIdentity {
        return try self.store.internFunc(&self.shadow_names, args, ret);
    }

    fn emptyRecord(self: *LogicalStore) WalkError!LogicalTypeIdentity {
        return try self.store.internRecord(&self.shadow_names, &.{});
    }

    fn emptyTagUnion(self: *LogicalStore) WalkError!LogicalTypeIdentity {
        return try self.store.internTagUnion(&self.shadow_names, &.{});
    }

    fn record(self: *LogicalStore, fields: []const MonoType.Field) WalkError!LogicalTypeIdentity {
        return try self.store.internRecord(&self.shadow_names, fields);
    }

    fn tagUnion(self: *LogicalStore, tags: []const MonoType.Store.TagInput) WalkError!LogicalTypeIdentity {
        return try self.store.internTagUnion(&self.shadow_names, tags);
    }

    /// The erased skeleton of a named type: declaration identity and arguments
    /// only. Representation content and the builtin dispatch owner are dropped;
    /// the backing is not part of source-level identity (reunify.md section
    /// 8.2). Aliases never reach here — they erase to their backing first.
    fn namedSkeleton(
        self: *LogicalStore,
        def_module_hash: [32]u8,
        owner_module_bytes: [32]u8,
        type_name_text: []const u8,
        source_decl: ?u32,
        kind: MonoType.NamedKind,
        args: []const LogicalTypeIdentity,
    ) WalkError!LogicalTypeIdentity {
        const module_id = try self.shadow_names.internModuleIdentity(&def_module_hash);
        const type_name = try self.shadow_names.internTypeName(type_name_text);
        return try self.store.internNamed(&self.shadow_names, .{
            .named_type = .{ .module = .{ .bytes = owner_module_bytes }, .ty = erased_occurrence },
            .def = .{ .module = module_id, .type_name = type_name, .source_decl = source_decl },
            .kind = kind,
            .builtin_owner = null,
            .args = args,
            .backing = null,
            .declared_order = &.{},
        });
    }

    fn internField(self: *LogicalStore, source_names: *const names.NameStore, label: names.RecordFieldNameId) WalkError!names.RecordFieldNameId {
        return try self.shadow_names.internRecordFieldLabel(source_names.recordFieldLabelText(label));
    }

    fn internTag(self: *LogicalStore, source_names: *const names.NameStore, label: names.TagNameId) WalkError!names.TagNameId {
        return try self.shadow_names.internTagLabel(source_names.tagLabelText(label));
    }

    // --- Checked-side translation (reunify.md section 8.2, section 9.2) ---

    /// The logical identity of a frozen checked root with no active binder
    /// environment: a concrete non-template root (reunify.md Slice 5 closed
    /// subset). `source_names` resolves the view's names to text.
    pub fn checkedLogicalIdentity(
        self: *LogicalStore,
        view: checked.CheckedTypeStoreView,
        source_names: *const names.NameStore,
        checked_ty: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!LogicalTypeIdentity {
        var walk = CheckedWalk{
            .owner = self,
            .view = view,
            .source_names = source_names,
            .binder_env = null,
            .scheme_owner_node = checked.checked_residual_disposition_module_body_owner,
            .active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator),
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(checked_ty);
    }

    /// Instantiate a scheme's root under a dense binding and captured
    /// binding (reunify.md section 9.1, section 9.5). The binding is ordered
    /// exactly like the scheme's binders and contains no inference variables.
    /// The result is memoized by the section 9.4 LogicalInstantiationMemo key.
    pub fn instantiateScheme(
        self: *LogicalStore,
        scheme_ident: SchemeIdent,
        view: checked.CheckedTypeStoreView,
        source_names: *const names.NameStore,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        binders: []const checked.CheckedTypeId,
        binding: []const BoundType,
        captured: []const BoundType,
        skip_reason: *SkipReason,
    ) WalkError!LogicalTypeIdentity {
        const key = instantiationDigest(scheme_ident, binding, captured);
        if (self.instantiation_memo.get(key)) |cached| return cached;

        const env = BindingEnvironment{
            .scheme = scheme_ident,
            .binders = binders,
            .bound = binding,
            .captured = captured,
            .parent = null,
        };
        var walk = CheckedWalk{
            .owner = self,
            .view = view,
            .source_names = source_names,
            .binder_env = &env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator),
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();

        const result = try walk.node(root);
        try self.instantiation_memo.put(key, result);
        return result;
    }

    // --- Closed-subset concreteness (reunify.md Slice 5) ---

    /// Whether a checked type is a fully concrete binding value: it holds no
    /// free variable (a flex/rigid with neither a numeric nor a row default)
    /// and no pending/err anywhere. A variable that defaults materializes
    /// concretely and stays concrete. This is the "fully concrete binding"
    /// predicate of the Slice 5 closed subset: an actual that is a free binder
    /// of an enclosing scheme is resolved under the caller environment in Slice
    /// 6, not here.
    pub fn isConcreteBinding(
        allocator: Allocator,
        view: checked.CheckedTypeStoreView,
        root: checked.CheckedTypeId,
    ) Allocator.Error!bool {
        var active = std.AutoHashMap(checked.CheckedTypeId, void).init(allocator);
        defer active.deinit();
        return try concreteInner(view, root, &active);
    }

    // --- Monotype-side erasure (reunify.md section 8.2) ---

    /// The logical identity of an already-lowered Monotype type: the same
    /// erasure rules as the checked side, applied to a Monotype node.
    /// `source_names` resolves the production store's names to text.
    pub fn monoLogicalIdentity(
        self: *LogicalStore,
        store: *const MonoType.Store,
        source_names: *const names.NameStore,
        mono_ty: MonoType.TypeId,
        skip_reason: *SkipReason,
    ) WalkError!LogicalTypeIdentity {
        var walk = MonoWalk{
            .owner = self,
            .store = store,
            .source_names = source_names,
            .active = std.AutoHashMap(MonoType.TypeId, void).init(self.allocator),
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(mono_ty);
    }
};

fn concreteInner(
    view: checked.CheckedTypeStoreView,
    ty: checked.CheckedTypeId,
    active: *std.AutoHashMap(checked.CheckedTypeId, void),
) Allocator.Error!bool {
    // A recursive back-reference reached again is part of a concrete graph.
    if (active.contains(ty)) return true;
    try active.put(ty, {});
    defer _ = active.remove(ty);

    switch (view.payload(ty)) {
        .pending, .err => return false,
        .flex, .rigid => |v| return v.numeric_default_phase != null or v.row_default != null,
        .empty_record, .empty_tag_union => return true,
        .record => |r| {
            for (r.fields) |field| {
                if (!try concreteInner(view, field.ty, active)) return false;
            }
            return try concreteInner(view, r.ext, active);
        },
        .record_unbound => |fields| {
            for (fields) |field| {
                if (!try concreteInner(view, field.ty, active)) return false;
            }
            return true;
        },
        .tuple => |items| {
            for (items) |item| {
                if (!try concreteInner(view, item, active)) return false;
            }
            return true;
        },
        .tag_union => |tu| {
            for (tu.tags) |tag| {
                for (tag.argsSlice(view)) |arg| {
                    if (!try concreteInner(view, arg, active)) return false;
                }
            }
            return try concreteInner(view, tu.ext, active);
        },
        .function => |fn_ty| {
            for (fn_ty.args) |arg| {
                if (!try concreteInner(view, arg, active)) return false;
            }
            return try concreteInner(view, fn_ty.ret, active);
        },
        .alias => |alias| return try concreteInner(view, alias.backing, active),
        .nominal => |n| {
            for (n.args) |arg| {
                if (!try concreteInner(view, arg, active)) return false;
            }
            return true;
        },
    }
}

/// The instantiation memo digest (reunify.md section 9.4): qualified scheme
/// plus ordered bound and captured logical ids.
fn instantiationDigest(scheme: SchemeIdent, binding: []const BoundType, captured: []const BoundType) InstantiationDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(&scheme.module_bytes);
    hasher.update(std.mem.asBytes(&scheme.scheme));
    const bound_len: u32 = @intCast(binding.len);
    hasher.update(std.mem.asBytes(&bound_len));
    for (binding) |value| {
        const raw: u32 = @intFromEnum(value.logical);
        hasher.update(std.mem.asBytes(&raw));
    }
    const captured_len: u32 = @intCast(captured.len);
    hasher.update(std.mem.asBytes(&captured_len));
    for (captured) |value| {
        const raw: u32 = @intFromEnum(value.logical);
        hasher.update(std.mem.asBytes(&raw));
    }
    return hasher.finalResult();
}

/// One checked-type erasure walk (reunify.md section 9.2). Carries the active
/// map for cycle detection, the optional binder environment for substitution,
/// and the scheme owner node for residual disposition lookup.
const CheckedWalk = struct {
    owner: *LogicalStore,
    view: checked.CheckedTypeStoreView,
    source_names: *const names.NameStore,
    binder_env: ?*const BindingEnvironment,
    scheme_owner_node: u32,
    active: std.AutoHashMap(checked.CheckedTypeId, void),
    skip_reason: *SkipReason,

    fn skip(self: *CheckedWalk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
    }

    fn node(self: *CheckedWalk, checked_ty: checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
        // A binder owned by the active scheme substitutes its bound logical id
        // (reunify.md section 9.2). Checked before the cycle guard so a binder
        // never registers as a cyclic node.
        if (self.binder_env) |env| {
            if (env.binderIndex(checked_ty)) |index| return env.bound[index].logical;
        }

        if (self.active.contains(checked_ty)) return self.skip(.recursive_cycle);
        try self.active.put(checked_ty, {});
        defer _ = self.active.remove(checked_ty);

        return try self.payload(checked_ty, self.view.payload(checked_ty));
    }

    fn payload(self: *CheckedWalk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!LogicalTypeIdentity {
        return switch (p) {
            .pending, .err => self.skip(.pending_or_err),
            .flex, .rigid => |v| try self.variable(checked_ty, v),
            .empty_record => try self.owner.emptyRecord(),
            .empty_tag_union => try self.owner.emptyTagUnion(),
            .record_unbound => |fields| try self.recordFrom(fields, null),
            .record => |r| try self.recordFrom(r.fields, r.ext),
            .tuple => |items| try self.tupleFrom(items),
            .tag_union => |tu| try self.tagUnionFrom(tu.tags, tu.ext),
            .function => |fn_ty| try self.function(fn_ty),
            .alias => |alias| try self.node(alias.backing),
            .nominal => |n| try self.nominal(n),
        };
    }

    /// A residual variable: consult its recorded disposition (reunify.md section
    /// 7.4), then apply the checked default. An `uninhabited` disposition and a
    /// bare unconstrained variable both reach the uninhabited leaf (empty tag
    /// union), matching the checked default materialization.
    fn variable(self: *CheckedWalk, checked_ty: checked.CheckedTypeId, v: checked.CheckedTypeVariable) WalkError!LogicalTypeIdentity {
        for (self.view.residualDispositions()) |disposition| {
            if (disposition.scheme_owner_node != self.scheme_owner_node) continue;
            if (disposition.type_id != @intFromEnum(checked_ty)) continue;
            switch (disposition.kind) {
                .uninhabited => return try self.owner.emptyTagUnion(),
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
                .dec => try self.owner.primitive(.dec),
                .str => try self.owner.primitive(.str),
            };
        }
        if (v.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => try self.owner.emptyRecord(),
                .empty_tag_union => try self.owner.emptyTagUnion(),
            };
        }
        return try self.owner.emptyTagUnion();
    }

    fn function(self: *CheckedWalk, fn_ty: checked.CheckedFunctionType) WalkError!LogicalTypeIdentity {
        var args = std.ArrayList(LogicalTypeIdentity).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.owner.func(args.items, ret);
    }

    fn tupleFrom(self: *CheckedWalk, items: []const checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
        var lowered = std.ArrayList(LogicalTypeIdentity).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.owner.tuple(lowered.items);
    }

    /// Collect a record's fields, flattening its extension row exactly as
    /// production record lowering does (walk aliases, an empty-record default,
    /// and nested record rows).
    fn recordFrom(self: *CheckedWalk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
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
                switch (self.view.payload(current)) {
                    .alias => |alias| current = alias.backing,
                    .empty_record => break,
                    .flex, .rigid => |v| {
                        if (v.row_default == .empty_record) break;
                        return self.skip(.open_row);
                    },
                    .record_unbound => |tail| {
                        try self.appendRecordFields(&fields, tail);
                        break;
                    },
                    .record => |r| {
                        try self.appendRecordFields(&fields, r.fields);
                        current = r.ext;
                    },
                    else => return self.skip(.open_row),
                }
            }
        }

        return try self.owner.record(fields.items);
    }

    fn appendRecordFields(self: *CheckedWalk, out: *std.ArrayList(MonoType.Field), fields: []const checked.CheckedRecordField) WalkError!void {
        for (fields) |field| {
            const label = try self.owner.internField(self.source_names, field.name);
            const ty = try self.node(field.ty);
            try out.append(self.owner.allocator, .{ .name = label, .ty = ty });
        }
    }

    /// Collect a tag union's tags, flattening its extension row exactly as
    /// production tag-union lowering does.
    fn tagUnionFrom(self: *CheckedWalk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);

        try self.appendTags(&tags, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            switch (self.view.payload(current)) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .flex, .rigid => |v| {
                    if (v.row_default == .empty_tag_union) break;
                    return self.skip(.open_row);
                },
                .tag_union => |tu| {
                    try self.appendTags(&tags, tu.tags);
                    current = tu.ext;
                },
                else => return self.skip(.open_row),
            }
        }

        return try self.owner.tagUnion(tags.items);
    }

    fn appendTags(self: *CheckedWalk, out: *std.ArrayList(MonoType.Store.TagInput), tags: []const checked.CheckedTag) WalkError!void {
        for (tags) |tag| {
            const label = try self.owner.internTag(self.source_names, tag.name);
            var payloads = std.ArrayList(LogicalTypeIdentity).empty;
            errdefer payloads.deinit(self.owner.allocator);
            for (tag.argsSlice(self.view)) |arg| {
                try payloads.append(self.owner.allocator, try self.node(arg));
            }
            try out.append(self.owner.allocator, .{
                .name = label,
                .checked_name = label,
                .payloads = try payloads.toOwnedSlice(self.owner.allocator),
            });
        }
    }

    fn freeTagInputs(self: *CheckedWalk, tags: *std.ArrayList(MonoType.Store.TagInput)) void {
        for (tags.items) |tag| self.owner.allocator.free(tag.payloads);
        tags.deinit(self.owner.allocator);
    }

    /// A nominal or opaque. Builtin nominals whose runtime encoding is a
    /// primitive, list, or box lower to that structural shape, matching
    /// production; the rest keep declaration identity as an erased named
    /// skeleton.
    fn nominal(self: *CheckedWalk, n: checked.CheckedNominalType) WalkError!LogicalTypeIdentity {
        switch (n.representation) {
            .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                .primitive => |value| return try self.owner.primitive(value),
                .list => {
                    if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                    return try self.owner.list(try self.node(n.args[0]));
                },
                .box => {
                    if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                    return try self.owner.box(try self.node(n.args[0]));
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

        var args = std.ArrayList(LogicalTypeIdentity).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const def_module_hash = self.source_names.moduleIdentityBytes(n.origin_module).*;
        const kind: MonoType.NamedKind = if (n.is_opaque) .@"opaque" else .nominal;
        return try self.owner.namedSkeleton(
            def_module_hash,
            n.owner_module.bytes,
            self.source_names.typeNameText(n.name),
            n.source_decl,
            kind,
            args.items,
        );
    }
};

/// One Monotype-type erasure walk (reunify.md section 8.2). Applies the same
/// erasures as the checked walk to an already-lowered node.
const MonoWalk = struct {
    owner: *LogicalStore,
    store: *const MonoType.Store,
    source_names: *const names.NameStore,
    active: std.AutoHashMap(MonoType.TypeId, void),
    skip_reason: *SkipReason,

    fn skip(self: *MonoWalk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
    }

    fn node(self: *MonoWalk, mono_ty: MonoType.TypeId) WalkError!LogicalTypeIdentity {
        if (self.active.contains(mono_ty)) return self.skip(.recursive_cycle);
        try self.active.put(mono_ty, {});
        defer _ = self.active.remove(mono_ty);

        return switch (self.store.get(mono_ty)) {
            .primitive => |value| try self.owner.primitive(value),
            .zst => self.skip(.zero_sized_or_erased),
            .erased => self.skip(.zero_sized_or_erased),
            .list => |elem| try self.owner.list(try self.node(elem)),
            .box => |elem| try self.owner.box(try self.node(elem)),
            .tuple => |span| try self.tupleFrom(span),
            .record => |span| try self.recordFrom(span),
            .tag_union => |span| try self.tagUnionFrom(span),
            .func => |fn_ty| try self.function(fn_ty),
            .named => |n| try self.named(n),
        };
    }

    fn function(self: *MonoWalk, fn_ty: std.meta.fieldInfo(MonoType.Content, .func).type) WalkError!LogicalTypeIdentity {
        var args = std.ArrayList(LogicalTypeIdentity).empty;
        defer args.deinit(self.owner.allocator);
        const arg_span = self.store.span(fn_ty.args);
        for (0..GuardedList.borrowLen(arg_span)) |index| {
            try args.append(self.owner.allocator, try self.node(GuardedList.at(arg_span, index)));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.owner.func(args.items, ret);
    }

    fn tupleFrom(self: *MonoWalk, span: MonoType.Span) WalkError!LogicalTypeIdentity {
        var lowered = std.ArrayList(LogicalTypeIdentity).empty;
        defer lowered.deinit(self.owner.allocator);
        const item_span = self.store.span(span);
        for (0..GuardedList.borrowLen(item_span)) |index| {
            try lowered.append(self.owner.allocator, try self.node(GuardedList.at(item_span, index)));
        }
        return try self.owner.tuple(lowered.items);
    }

    fn recordFrom(self: *MonoWalk, span: MonoType.Span) WalkError!LogicalTypeIdentity {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        const field_span = self.store.fieldSpan(span);
        for (0..GuardedList.borrowLen(field_span)) |index| {
            const field = GuardedList.at(field_span, index);
            const label = try self.owner.shadow_names.internRecordFieldLabel(self.source_names.recordFieldLabelText(field.name));
            const ty = try self.node(field.ty);
            try fields.append(self.owner.allocator, .{ .name = label, .ty = ty });
        }
        return try self.owner.record(fields.items);
    }

    fn tagUnionFrom(self: *MonoWalk, span: MonoType.Span) WalkError!LogicalTypeIdentity {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer {
            for (tags.items) |tag| self.owner.allocator.free(tag.payloads);
            tags.deinit(self.owner.allocator);
        }
        const tag_span = self.store.tagSpan(span);
        for (0..GuardedList.borrowLen(tag_span)) |tag_index| {
            const tag = GuardedList.at(tag_span, tag_index);
            const label = try self.owner.shadow_names.internTagLabel(self.source_names.tagLabelText(tag.name));
            var payloads = std.ArrayList(LogicalTypeIdentity).empty;
            errdefer payloads.deinit(self.owner.allocator);
            const payload_span = self.store.span(tag.payloads);
            for (0..GuardedList.borrowLen(payload_span)) |payload_index| {
                try payloads.append(self.owner.allocator, try self.node(GuardedList.at(payload_span, payload_index)));
            }
            try tags.append(self.owner.allocator, .{
                .name = label,
                .checked_name = label,
                .payloads = try payloads.toOwnedSlice(self.owner.allocator),
            });
        }
        return try self.owner.tagUnion(tags.items);
    }

    /// A named node. An alias erases to its backing (source-level identity drops
    /// every backed alias, builtin-owned included — reunify.md section 8.2). A
    /// nominal or opaque carrying representation content is outside the closed
    /// subset; otherwise it keeps declaration identity as an erased skeleton.
    fn named(self: *MonoWalk, n: MonoType.NamedContent) WalkError!LogicalTypeIdentity {
        if (n.kind == .alias) {
            const backing = n.backing orelse return self.skip(.alias_without_backing);
            return try self.node(backing.ty);
        }

        if (n.def.iterator_representation != .none or n.def.generated != null) {
            return self.skip(.representation_bearing);
        }

        var args = std.ArrayList(LogicalTypeIdentity).empty;
        defer args.deinit(self.owner.allocator);
        const arg_span = self.store.span(n.args);
        for (0..GuardedList.borrowLen(arg_span)) |index| {
            try args.append(self.owner.allocator, try self.node(GuardedList.at(arg_span, index)));
        }

        const def_module_hash = self.source_names.moduleIdentityBytes(n.def.module).*;
        return try self.owner.namedSkeleton(
            def_module_hash,
            n.named_type.module.bytes,
            self.source_names.typeNameText(n.def.type_name),
            n.def.source_decl,
            n.kind,
            args.items,
        );
    }
};

// --- Tests ---

const testing = std.testing;

/// A minimal hand-built checked type store view plus its name store, so the
/// erasure walks and instantiation can be tested without running the whole
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

    fn add(self: *TestFixture, payload: checked.StoredCheckedTypePayload) !checked.CheckedTypeId {
        const id: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.payloads.items.len)));
        try self.payloads.append(self.allocator, payload);
        return id;
    }

    fn addPrimitiveNominal(self: *TestFixture, builtin_nominal: checked.CheckedBuiltinNominal, name_text: []const u8) !checked.CheckedTypeId {
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

    fn addUserNominal(self: *TestFixture, name_text: []const u8, args: []const checked.CheckedTypeId) !checked.CheckedTypeId {
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
};

test "primitive builtin nominals erase to the same primitive skeleton" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const str_ty = try fixture.addPrimitiveNominal(.str, "Str");
    const u64_again = try fixture.addPrimitiveNominal(.u64, "U64");

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const a = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, u64_ty, &reason);
    const b = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, str_ty, &reason);
    const c = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, u64_again, &reason);

    try testing.expectEqual(a, c);
    try testing.expect(a != b);
}

test "user nominals keep declaration identity and distinguish arguments" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const str_ty = try fixture.addPrimitiveNominal(.str, "Str");
    const box_u64 = try fixture.addUserNominal("Wrapper", &.{u64_ty});
    const box_u64_again = try fixture.addUserNominal("Wrapper", &.{u64_ty});
    const box_str = try fixture.addUserNominal("Wrapper", &.{str_ty});
    const other_u64 = try fixture.addUserNominal("Other", &.{u64_ty});

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const a = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, box_u64, &reason);
    const a2 = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, box_u64_again, &reason);
    const b = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, box_str, &reason);
    const c = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, other_u64, &reason);

    try testing.expectEqual(a, a2);
    try testing.expect(a != b);
    try testing.expect(a != c);
}

test "aliases erase to their backing" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const alias_name = try fixture.source_names.internTypeName("MyU64");
    const alias_module = try fixture.source_names.internModuleIdentity(&fixture.module_hash);
    const alias_ty = try fixture.add(.{ .alias = .{
        .name = alias_name,
        .origin_module = alias_module,
        .owner_module = .{ .bytes = fixture.module_hash },
        .backing = u64_ty,
    } });

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const aliased = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, alias_ty, &reason);
    const direct = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, u64_ty, &reason);
    try testing.expectEqual(direct, aliased);
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

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const actual_logical = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, u64_ty, &reason);
    const binding = [_]BoundType{BoundType.closed(actual_logical)};
    const binders = [_]checked.CheckedTypeId{binder};

    const instantiated = try logical.instantiateScheme(
        .{ .module_bytes = fixture.module_hash, .scheme = 0 },
        fixture.view(),
        &fixture.source_names,
        checked.checked_residual_disposition_module_body_owner,
        scheme_root,
        &binders,
        &binding,
        &.{},
        &reason,
    );
    const direct = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, instantiated_root, &reason);
    try testing.expectEqual(direct, instantiated);
}

test "instantiation memo returns the same id for the same binding" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const binder = try fixture.add(.{ .rigid = .{} });
    const scheme_root = try fixture.addUserNominal("Wrapper", &.{binder});
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const actual_logical = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, u64_ty, &reason);
    const binding = [_]BoundType{BoundType.closed(actual_logical)};
    const binders = [_]checked.CheckedTypeId{binder};
    const ident = SchemeIdent{ .module_bytes = fixture.module_hash, .scheme = 0 };

    const first = try logical.instantiateScheme(ident, fixture.view(), &fixture.source_names, checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, &reason);
    const memo_count = logical.instantiation_memo.count();
    const second = try logical.instantiateScheme(ident, fixture.view(), &fixture.source_names, checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, &reason);

    try testing.expectEqual(first, second);
    try testing.expectEqual(@as(u32, 1), memo_count);
    try testing.expectEqual(memo_count, logical.instantiation_memo.count());
}

test "a free-variable actual is not a concrete binding" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const free_var = try fixture.add(.{ .rigid = .{} });
    const u64_ty = try fixture.addPrimitiveNominal(.u64, "U64");
    const wraps_free = try fixture.addUserNominal("Wrapper", &.{free_var});
    const wraps_concrete = try fixture.addUserNominal("Wrapper", &.{u64_ty});

    try testing.expect(!try LogicalStore.isConcreteBinding(testing.allocator, fixture.view(), free_var));
    try testing.expect(!try LogicalStore.isConcreteBinding(testing.allocator, fixture.view(), wraps_free));
    try testing.expect(try LogicalStore.isConcreteBinding(testing.allocator, fixture.view(), u64_ty));
    try testing.expect(try LogicalStore.isConcreteBinding(testing.allocator, fixture.view(), wraps_concrete));
}

test "unconstrained residual variables reach the empty tag union leaf" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const flex = try fixture.add(.{ .flex = .{} });
    const empty = try fixture.add(.empty_tag_union);

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const from_flex = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, flex, &reason);
    const from_empty = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, empty, &reason);
    try testing.expectEqual(from_empty, from_flex);
}

test "declarations are referenced" {
    testing.refAllDecls(@This());
}
