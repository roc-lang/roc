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

/// The sentinel module content identity every minted skolem declares itself
/// under (reunify.md 7.3). No real source module hashes to all-`0xFE`, so a
/// skolem skeleton can never collide with a real nominal's declaration identity.
const skolem_module_hash: [32]u8 = [_]u8{0xFE} ** 32;

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

/// How a translation walk treats a residual variable that carries neither a
/// recorded disposition nor a numeric/row default (reunify.md 7.3, 7.4). The
/// closed-subset roots of Slice 5 hold no such variable, so both policies agree
/// there; the difference matters only for the Slice 6 scheme-edge walk, whose
/// actuals and instantiated roots reference enclosing-scheme binders.
pub const FreeVarPolicy = enum {
    /// The checked default: an undisposed residual materializes as the
    /// uninhabited leaf (empty tag union), matching production materialization.
    default_empty,
    /// A free variable is a binder of an enclosing scheme (reunify.md 7.3):
    /// translate it to a distinct abstract logical identity ("skolem") keyed by
    /// its checked id, so the enclosing binder appears identically on both sides
    /// of the scheme-edge comparison (the parametric verification of 7.6). The
    /// same checked id yields the same skolem within one shadow run; distinct
    /// ids yield distinct skolems, so the comparison stays meaningful.
    skolemize,
};

/// The identity of one enclosing-scheme binder for skolem minting: the owning
/// module's content identity plus the binder's checked id. Two sites in one
/// module that reference the same enclosing binder share one skolem.
const SkolemIdent = struct {
    module: [32]u8,
    type_id: u32,
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
    /// One distinct abstract logical id per enclosing-scheme binder reached under
    /// `FreeVarPolicy.skolemize` (reunify.md 7.3). Keyed by `(module, checked id)`
    /// so the same enclosing binder appears identically on both sides of a
    /// scheme-edge comparison.
    skolem_ids: std.AutoHashMap(SkolemIdent, LogicalTypeIdentity),
    /// Monotonic discriminator that makes each minted skolem's synthetic name
    /// distinct, so no two enclosing binders intern to one skeleton.
    skolem_counter: u32,

    pub fn init(allocator: Allocator) LogicalStore {
        var store = MonoType.Store.init(allocator);
        store.enableInterning();
        return .{
            .allocator = allocator,
            .shadow_names = names.NameStore.init(allocator),
            .store = store,
            .instantiation_memo = std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity).init(allocator),
            .sealed_representation_memo = std.AutoHashMap(InstantiationDigest, LogicalTypeIdentity).init(allocator),
            .skolem_ids = std.AutoHashMap(SkolemIdent, LogicalTypeIdentity).init(allocator),
            .skolem_counter = 0,
        };
    }

    pub fn deinit(self: *LogicalStore) void {
        self.skolem_ids.deinit();
        self.sealed_representation_memo.deinit();
        self.instantiation_memo.deinit();
        self.store.deinit();
        self.shadow_names.deinit();
    }

    /// A distinct abstract logical id standing for one enclosing-scheme binder
    /// (reunify.md 7.3). Cached per `(module, checked id)`; minted as a named
    /// skeleton under a sentinel module identity with a monotonic name, so it
    /// never collides with a real source type or with another binder.
    fn skolem(self: *LogicalStore, module_bytes: [32]u8, checked_ty: checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
        const key = SkolemIdent{ .module = module_bytes, .type_id = @intFromEnum(checked_ty) };
        if (self.skolem_ids.get(key)) |cached| return cached;
        var name_buf: [24]u8 = undefined;
        const name_text = std.fmt.bufPrint(&name_buf, "{d}", .{self.skolem_counter}) catch unreachable;
        self.skolem_counter += 1;
        const id = try self.namedSkeleton(
            skolem_module_hash,
            skolem_module_hash,
            name_text,
            null,
            .@"opaque",
            &.{},
        );
        try self.skolem_ids.put(key, id);
        return id;
    }

    /// The 32-byte content digest of a skeleton, for bounded mismatch detail.
    pub fn digestBytes(self: *LogicalStore, id: LogicalTypeIdentity) [32]u8 {
        return self.store.typeDigest(&self.shadow_names, id).bytes;
    }

    /// A bounded S-expression of a shadow skeleton id, for mismatch diagnosis
    /// only. Depth- and breadth-limited so an unexpected cycle or a wide row
    /// cannot make the census unbounded. Skolems print as `?<name>`.
    pub fn describe(self: *LogicalStore, allocator: Allocator, id: LogicalTypeIdentity) Allocator.Error![]u8 {
        var out = std.ArrayList(u8).empty;
        errdefer out.deinit(allocator);
        try self.describeInto(allocator, &out, id, 6);
        return try out.toOwnedSlice(allocator);
    }

    fn describeInto(self: *LogicalStore, allocator: Allocator, out: *std.ArrayList(u8), id: LogicalTypeIdentity, depth: u8) Allocator.Error!void {
        if (depth == 0) {
            try out.appendSlice(allocator, "...");
            return;
        }
        switch (self.store.get(id)) {
            .primitive => |value| try out.appendSlice(allocator, @tagName(value)),
            .zst => try out.appendSlice(allocator, "zst"),
            .erased => try out.appendSlice(allocator, "erased"),
            .list => |elem| {
                try out.appendSlice(allocator, "(list ");
                try self.describeInto(allocator, out, elem, depth - 1);
                try out.append(allocator, ')');
            },
            .box => |elem| {
                try out.appendSlice(allocator, "(box ");
                try self.describeInto(allocator, out, elem, depth - 1);
                try out.append(allocator, ')');
            },
            .tuple => |span| {
                try out.appendSlice(allocator, "(tuple");
                const items = self.store.span(span);
                for (0..@min(GuardedList.borrowLen(items), 12)) |i| {
                    try out.append(allocator, ' ');
                    try self.describeInto(allocator, out, GuardedList.at(items, i), depth - 1);
                }
                try out.append(allocator, ')');
            },
            .record => |span| {
                try out.appendSlice(allocator, "(record");
                const fields = self.store.fieldSpan(span);
                for (0..@min(GuardedList.borrowLen(fields), 16)) |i| {
                    const field = GuardedList.at(fields, i);
                    try out.append(allocator, ' ');
                    try out.appendSlice(allocator, self.shadow_names.recordFieldLabelText(field.name));
                    try out.append(allocator, ':');
                    try self.describeInto(allocator, out, field.ty, depth - 1);
                }
                try out.append(allocator, ')');
            },
            .tag_union => |span| {
                try out.appendSlice(allocator, "(tags");
                const tags = self.store.tagSpan(span);
                for (0..@min(GuardedList.borrowLen(tags), 16)) |i| {
                    const tag = GuardedList.at(tags, i);
                    try out.append(allocator, ' ');
                    try out.appendSlice(allocator, self.shadow_names.tagLabelText(tag.name));
                    const payloads = self.store.span(tag.payloads);
                    for (0..@min(GuardedList.borrowLen(payloads), 8)) |j| {
                        try out.append(allocator, '/');
                        try self.describeInto(allocator, out, GuardedList.at(payloads, j), depth - 1);
                    }
                }
                try out.append(allocator, ')');
            },
            .func => |fn_ty| {
                try out.appendSlice(allocator, "(fn");
                const args = self.store.span(fn_ty.args);
                for (0..@min(GuardedList.borrowLen(args), 12)) |i| {
                    try out.append(allocator, ' ');
                    try self.describeInto(allocator, out, GuardedList.at(args, i), depth - 1);
                }
                try out.appendSlice(allocator, " -> ");
                try self.describeInto(allocator, out, fn_ty.ret, depth - 1);
                try out.append(allocator, ')');
            },
            .named => |n| {
                const is_skolem = std.meta.eql(self.shadow_names.moduleIdentityBytes(n.def.module).*, skolem_module_hash);
                try out.append(allocator, if (is_skolem) '?' else '#');
                try out.appendSlice(allocator, self.shadow_names.typeNameText(n.def.type_name));
                if (!is_skolem) {
                    const args = self.store.span(n.args);
                    for (0..@min(GuardedList.borrowLen(args), 12)) |i| {
                        try out.append(allocator, ' ');
                        try self.describeInto(allocator, out, GuardedList.at(args, i), depth - 1);
                    }
                }
            },
        }
    }

    /// Whether a shadow id is a minted skolem (an abstract enclosing-scheme
    /// binder), rather than a real source type.
    fn isSkolem(self: *LogicalStore, id: LogicalTypeIdentity) bool {
        return switch (self.store.get(id)) {
            .named => |n| std.meta.eql(self.shadow_names.moduleIdentityBytes(n.def.module).*, skolem_module_hash),
            else => false,
        };
    }

    fn collectSkolems(self: *LogicalStore, id: LogicalTypeIdentity, set: *std.AutoHashMap(LogicalTypeIdentity, void), seen: *std.AutoHashMap(LogicalTypeIdentity, void)) Allocator.Error!void {
        if (seen.contains(id)) return;
        try seen.put(id, {});
        switch (self.store.get(id)) {
            .primitive, .zst, .erased => {},
            .list, .box => |elem| try self.collectSkolems(elem, set, seen),
            .tuple => |span| {
                const items = self.store.span(span);
                for (0..GuardedList.borrowLen(items)) |i| try self.collectSkolems(GuardedList.at(items, i), set, seen);
            },
            .record => |span| {
                const fields = self.store.fieldSpan(span);
                for (0..GuardedList.borrowLen(fields)) |i| try self.collectSkolems(GuardedList.at(fields, i).ty, set, seen);
            },
            .tag_union => |span| {
                const tags = self.store.tagSpan(span);
                for (0..GuardedList.borrowLen(tags)) |i| {
                    const payloads = self.store.span(GuardedList.at(tags, i).payloads);
                    for (0..GuardedList.borrowLen(payloads)) |j| try self.collectSkolems(GuardedList.at(payloads, j), set, seen);
                }
            },
            .func => |fn_ty| {
                const args = self.store.span(fn_ty.args);
                for (0..GuardedList.borrowLen(args)) |i| try self.collectSkolems(GuardedList.at(args, i), set, seen);
                try self.collectSkolems(fn_ty.ret, set, seen);
            },
            .named => |n| {
                if (self.isSkolem(id)) {
                    try set.put(id, {});
                    return;
                }
                const args = self.store.span(n.args);
                for (0..GuardedList.borrowLen(args)) |i| try self.collectSkolems(GuardedList.at(args, i), set, seen);
            },
        }
    }

    /// Whether two shadow skeletons are equal up to a consistent renaming of the
    /// skolems that stand for independent copies of an enclosing binder — but
    /// with a skolem that occurs in BOTH skeletons pinned to itself. A binders=0
    /// nested scheme's root and the site's instantiated root use disjoint
    /// fresh copies of the enclosing binder, so they are alpha-equal here; a real
    /// binder transposition, where the SAME enclosing skolem appears on both
    /// sides, is not (the pin forces identity and the swap conflicts). Exact id
    /// equality is the fast path; this runs only when that fails.
    pub fn alphaEqual(self: *LogicalStore, left: LogicalTypeIdentity, right: LogicalTypeIdentity) Allocator.Error!bool {
        var left_skolems = std.AutoHashMap(LogicalTypeIdentity, void).init(self.allocator);
        defer left_skolems.deinit();
        var right_skolems = std.AutoHashMap(LogicalTypeIdentity, void).init(self.allocator);
        defer right_skolems.deinit();
        {
            var seen = std.AutoHashMap(LogicalTypeIdentity, void).init(self.allocator);
            defer seen.deinit();
            try self.collectSkolems(left, &left_skolems, &seen);
        }
        {
            var seen = std.AutoHashMap(LogicalTypeIdentity, void).init(self.allocator);
            defer seen.deinit();
            try self.collectSkolems(right, &right_skolems, &seen);
        }
        var forward = std.AutoHashMap(LogicalTypeIdentity, LogicalTypeIdentity).init(self.allocator);
        defer forward.deinit();
        var backward = std.AutoHashMap(LogicalTypeIdentity, LogicalTypeIdentity).init(self.allocator);
        defer backward.deinit();
        var pairs = AlphaPairs{
            .owner = self,
            .left_skolems = &left_skolems,
            .right_skolems = &right_skolems,
            .forward = &forward,
            .backward = &backward,
        };
        return try pairs.eq(left, right);
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
        return try self.checkedLogicalIdentityUnder(
            view,
            source_names,
            checked_ty,
            .default_empty,
            skolem_module_hash,
            skip_reason,
        );
    }

    /// The logical identity of a frozen checked root under an explicit free-var
    /// policy (reunify.md 7.3, 7.4). With `.skolemize` the root's free variables
    /// are enclosing-scheme binders and translate to abstract skolems keyed under
    /// `skolem_module`; with `.default_empty` an undisposed residual materializes
    /// as the uninhabited leaf. There is no active binder environment: a scheme's
    /// own binders are substituted through `instantiateScheme`, while an
    /// enclosing binder reaches the skolem path here.
    pub fn checkedLogicalIdentityUnder(
        self: *LogicalStore,
        view: checked.CheckedTypeStoreView,
        source_names: *const names.NameStore,
        checked_ty: checked.CheckedTypeId,
        free_var_policy: FreeVarPolicy,
        skolem_module: [32]u8,
        skip_reason: *SkipReason,
    ) WalkError!LogicalTypeIdentity {
        var walk = CheckedWalk{
            .owner = self,
            .view = view,
            .source_names = source_names,
            .binder_env = null,
            .scheme_owner_node = checked.checked_residual_disposition_module_body_owner,
            .free_var_policy = free_var_policy,
            .skolem_module = skolem_module,
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
        free_var_policy: FreeVarPolicy,
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
            .free_var_policy = free_var_policy,
            // A free variable reached inside a callee scheme's root that is not
            // one of the callee's own binders is a captured enclosing binder
            // (reunify.md 7.3); it skolemizes under the scheme's own module, the
            // same key the caller-side actual and instantiated-root walks use.
            .skolem_module = scheme_ident.module_bytes,
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
    free_var_policy: FreeVarPolicy,
    skolem_module: [32]u8,
    active: std.AutoHashMap(checked.CheckedTypeId, void),
    skip_reason: *SkipReason,

    fn skip(self: *CheckedWalk, reason: SkipReason) WalkError {
        self.skip_reason.* = reason;
        return error.Skip;
    }

    /// The bound logical id of a binder visible in the active environment or any
    /// lexically enclosing one (reunify.md 7.3 links environments through
    /// `parent`), or null when the checked type is not a bound binder.
    fn envBinderLogical(self: *CheckedWalk, checked_ty: checked.CheckedTypeId) ?LogicalTypeIdentity {
        var env = self.binder_env;
        while (env) |e| : (env = e.parent) {
            if (e.binderIndex(checked_ty)) |index| return e.bound[index].logical;
        }
        return null;
    }

    fn node(self: *CheckedWalk, checked_ty: checked.CheckedTypeId) WalkError!LogicalTypeIdentity {
        // A binder owned by the active scheme (or a lexically enclosing one)
        // substitutes its bound logical id (reunify.md section 9.2). Checked
        // before the cycle guard so a binder never registers as a cyclic node.
        if (self.envBinderLogical(checked_ty)) |bound| return bound;

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
        // An undisposed, undefaulted residual reached under `.skolemize` is an
        // enclosing-scheme binder (reunify.md 7.3): a distinct abstract id keyed
        // by its checked identity, appearing the same on both sides of the
        // scheme-edge comparison. Under `.default_empty` it materializes as the
        // uninhabited leaf, matching the checked default.
        return switch (self.free_var_policy) {
            .skolemize => try self.owner.skolem(self.skolem_module, checked_ty),
            .default_empty => try self.owner.emptyTagUnion(),
        };
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
                // A record extension that is a scheme binder substitutes its
                // bound actual, whose fields splice into this row (reunify.md
                // 9.2). The interned actual is closed, so the row closes here.
                if (self.envBinderLogical(current)) |bound| {
                    try self.spliceInternedFields(&fields, bound);
                    break;
                }
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
            // A tag-union extension that is a scheme binder substitutes its bound
            // actual, whose tags splice into this row (reunify.md 9.2). The
            // interned actual is closed, so the row closes here.
            if (self.envBinderLogical(current)) |bound| {
                try self.spliceInternedTags(&tags, bound);
                break;
            }
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

    /// Splice the tags of an already-interned actual (the value bound to a
    /// row-extension binder) into `out`. The actual is closed, so an interned
    /// tag-union node ends the row; any other head (a skolem standing for a still
    /// open enclosing row) leaves the row genuinely open, outside the subset.
    fn spliceInternedTags(self: *CheckedWalk, out: *std.ArrayList(MonoType.Store.TagInput), id: LogicalTypeIdentity) WalkError!void {
        switch (self.owner.store.get(id)) {
            .tag_union => |span| {
                const tag_span = self.owner.store.tagSpan(span);
                for (0..GuardedList.borrowLen(tag_span)) |i| {
                    const tag = GuardedList.at(tag_span, i);
                    const payload_span = self.owner.store.span(tag.payloads);
                    var payloads = std.ArrayList(LogicalTypeIdentity).empty;
                    errdefer payloads.deinit(self.owner.allocator);
                    for (0..GuardedList.borrowLen(payload_span)) |j| {
                        try payloads.append(self.owner.allocator, GuardedList.at(payload_span, j));
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

    /// Splice the fields of an already-interned actual (the value bound to a
    /// record-extension binder) into `out`. As with tags, only an interned
    /// record node closes the row.
    fn spliceInternedFields(self: *CheckedWalk, out: *std.ArrayList(MonoType.Field), id: LogicalTypeIdentity) WalkError!void {
        switch (self.owner.store.get(id)) {
            .record => |span| {
                const field_span = self.owner.store.fieldSpan(span);
                for (0..GuardedList.borrowLen(field_span)) |i| {
                    try out.append(self.owner.allocator, GuardedList.at(field_span, i));
                }
            },
            else => return self.skip(.open_row),
        }
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

/// Simultaneous structural comparison of two shadow skeletons up to a consistent
/// renaming of unshared skolems (reunify.md 7.6 compares complete roots; a
/// binders=0 nested scheme's two roots are alpha-equal). A skolem present in both
/// skeletons is pinned to itself, so a real binder transposition is rejected.
const AlphaPairs = struct {
    owner: *LogicalStore,
    left_skolems: *std.AutoHashMap(LogicalTypeIdentity, void),
    right_skolems: *std.AutoHashMap(LogicalTypeIdentity, void),
    forward: *std.AutoHashMap(LogicalTypeIdentity, LogicalTypeIdentity),
    backward: *std.AutoHashMap(LogicalTypeIdentity, LogicalTypeIdentity),

    fn pairSkolems(self: *AlphaPairs, l: LogicalTypeIdentity, r: LogicalTypeIdentity) Allocator.Error!bool {
        if (l != r) {
            // A skolem occurring in the OTHER skeleton is shared and pinned to
            // itself, so it cannot rename to a different skolem.
            if (self.right_skolems.contains(l)) return false;
            if (self.left_skolems.contains(r)) return false;
        }
        if (self.forward.get(l)) |m| return m == r;
        if (self.backward.get(r)) |m| return m == l;
        try self.forward.put(l, r);
        try self.backward.put(r, l);
        return true;
    }

    fn eqSpan(self: *AlphaPairs, l: MonoType.Span, r: MonoType.Span) Allocator.Error!bool {
        const ls = self.owner.store.span(l);
        const rs = self.owner.store.span(r);
        if (GuardedList.borrowLen(ls) != GuardedList.borrowLen(rs)) return false;
        for (0..GuardedList.borrowLen(ls)) |i| {
            if (!try self.eq(GuardedList.at(ls, i), GuardedList.at(rs, i))) return false;
        }
        return true;
    }

    fn eq(self: *AlphaPairs, l: LogicalTypeIdentity, r: LogicalTypeIdentity) Allocator.Error!bool {
        if (l == r and !self.owner.isSkolem(l)) return true;

        const l_sk = self.owner.isSkolem(l);
        const r_sk = self.owner.isSkolem(r);
        if (l_sk or r_sk) {
            if (l_sk != r_sk) return false;
            return try self.pairSkolems(l, r);
        }

        const lc = self.owner.store.get(l);
        const rc = self.owner.store.get(r);
        if (std.meta.activeTag(lc) != std.meta.activeTag(rc)) return false;
        switch (lc) {
            .primitive => |v| return v == rc.primitive,
            .zst => return true,
            .erased => |d| return std.meta.eql(d.bytes, rc.erased.bytes),
            .list => |e| return try self.eq(e, rc.list),
            .box => |e| return try self.eq(e, rc.box),
            .tuple => |s| return try self.eqSpan(s, rc.tuple),
            .func => |lf| {
                if (!try self.eqSpan(lf.args, rc.func.args)) return false;
                return try self.eq(lf.ret, rc.func.ret);
            },
            .record => |s| {
                const lf = self.owner.store.fieldSpan(s);
                const rf = self.owner.store.fieldSpan(rc.record);
                if (GuardedList.borrowLen(lf) != GuardedList.borrowLen(rf)) return false;
                for (0..GuardedList.borrowLen(lf)) |i| {
                    const a = GuardedList.at(lf, i);
                    const b = GuardedList.at(rf, i);
                    if (a.name != b.name) return false;
                    if (!try self.eq(a.ty, b.ty)) return false;
                }
                return true;
            },
            .tag_union => |s| {
                const lt = self.owner.store.tagSpan(s);
                const rt = self.owner.store.tagSpan(rc.tag_union);
                if (GuardedList.borrowLen(lt) != GuardedList.borrowLen(rt)) return false;
                for (0..GuardedList.borrowLen(lt)) |i| {
                    const a = GuardedList.at(lt, i);
                    const b = GuardedList.at(rt, i);
                    if (a.name != b.name) return false;
                    if (!try self.eqSpan(a.payloads, b.payloads)) return false;
                }
                return true;
            },
            .named => |ln| {
                const rn = rc.named;
                if (ln.kind != rn.kind) return false;
                if (ln.def.module != rn.def.module) return false;
                if (ln.def.type_name != rn.def.type_name) return false;
                if (ln.def.source_decl != rn.def.source_decl) return false;
                return try self.eqSpan(ln.args, rn.args);
            },
        }
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

    /// A nominal imported from another module: it carries the DEFINING module's
    /// identity, exactly as real checked data records an imported type, so its
    /// erased skeleton converges with the defining module's own occurrence.
    fn addImportedNominal(self: *TestFixture, name_text: []const u8, args: []const checked.CheckedTypeId, defining_hash: [32]u8) !checked.CheckedTypeId {
        const name = try self.source_names.internTypeName(name_text);
        const module = try self.source_names.internModuleIdentity(&defining_hash);
        const start: u32 = @intCast(self.type_id_pool.items.len);
        try self.type_id_pool.appendSlice(self.allocator, args);
        return try self.add(.{ .nominal = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = defining_hash },
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
        .default_empty,
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

    const first = try logical.instantiateScheme(ident, fixture.view(), &fixture.source_names, checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, .default_empty, &reason);
    const memo_count = logical.instantiation_memo.count();
    const second = try logical.instantiateScheme(ident, fixture.view(), &fixture.source_names, checked.checked_residual_disposition_module_body_owner, scheme_root, &binders, &binding, &.{}, .default_empty, &reason);

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

test "a scheme instantiated with enclosing binders skolemizes consistently" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // Callee scheme: Pair a b, with binders a, b, root = Pair a b.
    const a = try fixture.add(.{ .rigid = .{} });
    const b = try fixture.add(.{ .rigid = .{} });
    const scheme_root = try fixture.addUserNominal("Pair", &.{ a, b });

    // The enclosing (caller) scheme's binders X and Y, referenced by the site's
    // actuals. They are ordinary rigid variables with no default and no
    // disposition — the shape of a captured enclosing binder.
    const x = try fixture.add(.{ .rigid = .{} });
    const y = try fixture.add(.{ .rigid = .{} });
    const instantiated_root = try fixture.addUserNominal("Pair", &.{ x, y });
    const reversed_root = try fixture.addUserNominal("Pair", &.{ y, x });

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const bx = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, x, .skolemize, fixture.module_hash, &reason);
    const by = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, y, .skolemize, fixture.module_hash, &reason);
    try testing.expect(bx != by);

    const binding = [_]BoundType{ BoundType.closed(bx), BoundType.closed(by) };
    const binders = [_]checked.CheckedTypeId{ a, b };
    const instantiated = try logical.instantiateScheme(
        .{ .module_bytes = fixture.module_hash, .scheme = 0 },
        fixture.view(),
        &fixture.source_names,
        checked.checked_residual_disposition_module_body_owner,
        scheme_root,
        &binders,
        &binding,
        &.{},
        .skolemize,
        &reason,
    );
    const direct = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, instantiated_root, .skolemize, fixture.module_hash, &reason);
    const reversed = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, reversed_root, .skolemize, fixture.module_hash, &reason);

    // The binder order is preserved: instantiation matches the instantiated root
    // and is distinct from the transposed root.
    try testing.expectEqual(direct, instantiated);
    try testing.expect(reversed != instantiated);
}

test "alpha-equality renames independent skolems but pins shared ones" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    // Two independent enclosing binders X, Y on the "left"; a third and fourth
    // P, Q on the "right", built as skolems under different checked ids so their
    // skolem ids differ.
    const sx = try logical.skolem(fixture.module_hash, @enumFromInt(100));
    const sy = try logical.skolem(fixture.module_hash, @enumFromInt(101));
    const sp = try logical.skolem(fixture.module_hash, @enumFromInt(200));
    const sq = try logical.skolem(fixture.module_hash, @enumFromInt(201));

    // Iter X, Iter X -> Iter X  vs  Iter P, Iter P -> Iter P : alpha-equal.
    const left_same = try logical.func(&.{ try logical.list(sx), try logical.list(sx) }, try logical.list(sx));
    const right_same = try logical.func(&.{ try logical.list(sp), try logical.list(sp) }, try logical.list(sp));
    try testing.expect(left_same != right_same);
    try testing.expect(try logical.alphaEqual(left_same, right_same));

    // Pair X Y  vs  Pair Y X where X,Y are the SAME skolems on both sides: a
    // transposition of shared binders is NOT alpha-equal.
    const pair_xy = try logical.tuple(&.{ sx, sy });
    const pair_yx = try logical.tuple(&.{ sy, sx });
    try testing.expect(!try logical.alphaEqual(pair_xy, pair_yx));

    // Pair X Y  vs  Pair P Q with disjoint skolems: a consistent renaming, so
    // alpha-equal.
    const pair_pq = try logical.tuple(&.{ sp, sq });
    try testing.expect(try logical.alphaEqual(pair_xy, pair_pq));

    // Pair X X  vs  Pair P Q: not a bijection (X must map to both P and Q).
    const pair_xx = try logical.tuple(&.{ sx, sx });
    try testing.expect(!try logical.alphaEqual(pair_xx, pair_pq));
}

test "the same enclosing binder yields one skolem; the default policy does not" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const x = try fixture.add(.{ .rigid = .{} });

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const first = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, x, .skolemize, fixture.module_hash, &reason);
    const second = try logical.checkedLogicalIdentityUnder(fixture.view(), &fixture.source_names, x, .skolemize, fixture.module_hash, &reason);
    try testing.expectEqual(first, second);

    // The default policy leaves a bare residual at the uninhabited leaf, distinct
    // from any skolem.
    const empty = try fixture.add(.empty_tag_union);
    const defaulted = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, x, &reason);
    const empty_id = try logical.checkedLogicalIdentity(fixture.view(), &fixture.source_names, empty, &reason);
    try testing.expectEqual(empty_id, defaulted);
    try testing.expect(defaulted != first);
}

test "an imported scheme instantiates across two module views" {
    // The defining module owns the scheme root and binders; the consuming module
    // owns the actuals and the instantiated root (reunify.md 7.1, Slice 6). The
    // two id spaces meet on the shadow's text-neutral interner.
    var defining = TestFixture.init(testing.allocator);
    defining.module_hash = [_]u8{0xAA} ** 32;
    defer defining.deinit();
    var consuming = TestFixture.init(testing.allocator);
    consuming.module_hash = [_]u8{0xBB} ** 32;
    defer consuming.deinit();

    // Defining module: scheme Wrapper a, binder a, root = Wrapper a (Wrapper owned
    // by the defining module).
    const binder = try defining.add(.{ .rigid = .{} });
    const scheme_root = try defining.addUserNominal("Wrapper", &.{binder});

    // Consuming module: the actual U64 and the instantiated root Wrapper U64,
    // where Wrapper carries the DEFINING module's identity (an imported nominal).
    const u64_ty = try consuming.addPrimitiveNominal(.u64, "U64");
    const instantiated_root = try consuming.addImportedNominal("Wrapper", &.{u64_ty}, defining.module_hash);

    var logical = LogicalStore.init(testing.allocator);
    defer logical.deinit();

    var reason: SkipReason = undefined;
    const actual_logical = try logical.checkedLogicalIdentityUnder(
        consuming.view(),
        &consuming.source_names,
        u64_ty,
        .skolemize,
        consuming.module_hash,
        &reason,
    );
    const binding = [_]BoundType{BoundType.closed(actual_logical)};
    const binders = [_]checked.CheckedTypeId{binder};

    const instantiated = try logical.instantiateScheme(
        .{ .module_bytes = defining.module_hash, .scheme = 0 },
        defining.view(),
        &defining.source_names,
        checked.checked_residual_disposition_module_body_owner,
        scheme_root,
        &binders,
        &binding,
        &.{},
        .skolemize,
        &reason,
    );
    const direct = try logical.checkedLogicalIdentityUnder(
        consuming.view(),
        &consuming.source_names,
        instantiated_root,
        .skolemize,
        consuming.module_hash,
        &reason,
    );
    try testing.expectEqual(direct, instantiated);
}

test "declarations are referenced" {
    testing.refAllDecls(@This());
}
