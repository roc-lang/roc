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
        /// The declaration's own id in its module, which with the module
        /// identity names the declaration a nominal instance is an instance of.
        declaration: u32,
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
/// subset. Recorded by the caller; never a panic.
///
/// `recursive_cycle` is now emitted only when the recursive-group builder cannot
/// close a cycle (a degenerate cycle through a node that reserves no slot);
/// ordinary recursive types are built through the store's recursive-group
/// builder (reunify.md section 9.2, 10.6).
///
/// `engine_input_needed` marks a position whose representation content the
/// checked data cannot dictate — a generated opaque-evidence backing that the
/// section 10 closure engine mints in step (b) (reunify.md section 9.1's
/// minted/forced-dynamic content). The identity is derivable, but emitting a
/// backing the checked data does not contain would be wrong output, so the walk
/// skips instead. Its count, together with `direct_stored_mismatch_representation`,
/// bounds the representation content step (b) must supply.
pub const SkipReason = enum {
    recursive_cycle,
    pending_or_err,
    numeric_default_unresolved,
    open_row,
    malformed_builtin_arity,
    binder_not_found,
    missing_backing,
    engine_input_needed,
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

/// One nominal instance's identity inside a reserve-before-descend walk: the
/// declaration it instantiates plus the stored ids its arguments translated to.
/// This is the reunify.md section 9.4 instantiation key at a nominal
/// declaration — checking allocates a distinct checked id for every occurrence
/// of one nominal, so the checked address alone cannot recognize that a
/// declaration's backing reached the very instance it is the backing of, and the
/// knot would close one level deeper on the backing instead of on the nominal.
const NominalInstance = struct {
    module_bytes: [32]u8,
    declaration: u32,
    args: []const TypeId,
    slot: TypeId,

    fn sameInstance(self: NominalInstance, module_bytes: [32]u8, declaration: u32, args: []const TypeId) bool {
        if (self.declaration != declaration) return false;
        if (!std.mem.eql(u8, &self.module_bytes, &module_bytes)) return false;
        if (self.args.len != args.len) return false;
        for (self.args, args) |left, right| {
            if (left != right) return false;
        }
        return true;
    }
};

/// The nominal instances one reserve-fill walk has reserved a slot for. A walk
/// builds a handful of them, so the lookup is a scan over the exact key rather
/// than a hash of it.
const NominalInstances = struct {
    allocator: Allocator,
    items: std.ArrayList(NominalInstance),

    fn init(allocator: Allocator) NominalInstances {
        return .{ .allocator = allocator, .items = .empty };
    }

    fn deinit(self: *NominalInstances) void {
        for (self.items.items) |entry| self.allocator.free(entry.args);
        self.items.deinit(self.allocator);
    }

    fn find(self: *const NominalInstances, module_bytes: [32]u8, declaration: u32, args: []const TypeId) ?TypeId {
        for (self.items.items) |entry| {
            if (entry.sameInstance(module_bytes, declaration, args)) return entry.slot;
        }
        return null;
    }

    fn record(
        self: *NominalInstances,
        module_bytes: [32]u8,
        declaration: u32,
        args: []const TypeId,
        slot: TypeId,
    ) Allocator.Error!void {
        const owned = try self.allocator.dupe(TypeId, args);
        errdefer self.allocator.free(owned);
        try self.items.append(self.allocator, .{
            .module_bytes = module_bytes,
            .declaration = declaration,
            .args = owned,
            .slot = slot,
        });
    }
};

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
        const owner_node = checked.checked_residual_disposition_module_body_owner;
        return try self.translateUnderEnvironment(cursor, null, owner_node, checked_ty, skip_reason);
    }

    /// Translate one checked root under an already-built binder environment
    /// (reunify.md section 9.2). The caller owns `binding_env` and the storage
    /// its bound values name; `scheme_owner_node` selects the residual
    /// dispositions that apply to this walk. A recursive root reruns through the
    /// store's recursive-group builder, exactly as the ground entry point does.
    pub fn translateUnderEnvironment(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        checked_ty: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        return self.eagerWalk(cursor, binding_env, scheme_owner_node, checked_ty, skip_reason) catch |err| switch (err) {
            error.Skip => {
                if (skip_reason.* == .recursive_cycle) {
                    return try self.translateRecursiveRoot(cursor, binding_env, scheme_owner_node, checked_ty, skip_reason);
                }
                return err;
            },
            else => return err,
        };
    }

    /// Run one acyclic (eager, child-first interning) walk. A recursive cycle
    /// leaves this walk through the cycle guard so the caller can translate the
    /// root through the recursive-group builder instead (reunify.md section 9.2).
    fn eagerWalk(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .build_store = self.store,
            .binding_env = binding_env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .recursion_slots = null,
            .nominal_instances = null,
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(root);
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

        const result = self.eagerWalk(cursor, &env, scheme_owner_node, root, skip_reason) catch |err| switch (err) {
            error.Skip => if (skip_reason.* == .recursive_cycle)
                try self.translateRecursiveRoot(cursor, &env, scheme_owner_node, root, skip_reason)
            else
                return err,
            else => return err,
        };
        try self.represented_memo.put(key, result);
        return result;
    }

    /// Translate a root the eager walk found recursive, through the store's
    /// recursive-group builder (reunify.md section 9.2). A cyclic group with no
    /// representation-bearing position is built reserve-before-descend: every
    /// compound node reserves its stored slot before its children are translated,
    /// so a back-reference resolves to the reserved slot. Reserve-before-descend
    /// closes a cycle on the checked address it reached twice, which is one
    /// address among the several the checker may hold for one type, so the raw
    /// group can carry a member that repeats an ancestor's rooted graph. The
    /// interner is the structural equality authority (reunify.md sections 8.2,
    /// 8.3): the group is therefore built in an isolated scratch store and
    /// re-interned into the target, whose recursive-group builder registers each
    /// member's rooted key and collapses the repeats. An active binder
    /// environment names ids in the target store, so its bound values move into
    /// the scratch first. A target store that does not deduplicate has no
    /// recursive-group builder to hand the component to, so it is built in place.
    fn translateRecursiveRoot(
        self: *Translator,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        if (!self.store.internEnabled()) {
            return try self.reserveFillWalk(self.store, cursor, binding_env, scheme_owner_node, root, skip_reason);
        }
        var scratch = MonoType.Store.init(self.allocator);
        defer scratch.deinit();
        scratch.enableInterning();
        var moved = MovedEnvironment.init(self.allocator);
        defer moved.deinit();
        const scratch_env = try moved.move(self.store, self.target_names, &scratch, binding_env);
        const scratch_root = try self.reserveFillWalk(&scratch, cursor, scratch_env, scheme_owner_node, root, skip_reason);
        return try MonoType.reintern(self.store, self.target_names, scratch.view(), scratch_root);
    }

    /// Build `root` and its recursive component into `build_store` with
    /// reserve-before-descend cycle closure. Names always intern into the target
    /// name store, so a scratch build shares row/tag/name ids with the target and
    /// re-interns cleanly.
    fn reserveFillWalk(
        self: *Translator,
        build_store: *MonoType.Store,
        cursor: ModuleCursor,
        binding_env: ?*const BindingEnvironment,
        scheme_owner_node: u32,
        root: checked.CheckedTypeId,
        skip_reason: *SkipReason,
    ) WalkError!TypeId {
        var slots = std.AutoHashMap(ActiveNode, TypeId).init(self.allocator);
        defer slots.deinit();
        var instances = NominalInstances.init(self.allocator);
        defer instances.deinit();
        var walk = Walk{
            .owner = self,
            .cursor = cursor,
            .build_store = build_store,
            .binding_env = binding_env,
            .scheme_owner_node = scheme_owner_node,
            .active = std.AutoHashMap(ActiveNode, void).init(self.allocator),
            .recursion_slots = &slots,
            .nominal_instances = &instances,
            .skip_reason = skip_reason,
        };
        defer walk.active.deinit();
        return try walk.node(root);
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

/// One binding environment chain relocated into the scratch store a recursive
/// group is built in (reunify.md section 9.2). Every bound and captured value is
/// re-interned into that store, so a binder substitution during the scratch
/// build names a scratch id. Both buffers are sized exactly once, so the
/// relocated `parent` links and value slices stay valid for the whole build.
const MovedEnvironment = struct {
    allocator: Allocator,
    frames: std.ArrayList(BindingEnvironment),
    values: std.ArrayList(BoundType),

    fn init(allocator: Allocator) MovedEnvironment {
        return .{ .allocator = allocator, .frames = .empty, .values = .empty };
    }

    fn deinit(self: *MovedEnvironment) void {
        self.values.deinit(self.allocator);
        self.frames.deinit(self.allocator);
    }

    /// Relocate `env` and every environment it links to, returning the innermost
    /// relocated environment, or null when there is none.
    fn move(
        self: *MovedEnvironment,
        source: *const MonoType.Store,
        name_store: *const names.NameStore,
        scratch: *MonoType.Store,
        env: ?*const BindingEnvironment,
    ) Allocator.Error!?*const BindingEnvironment {
        var depth: usize = 0;
        var value_count: usize = 0;
        var cursor = env;
        while (cursor) |frame| : (cursor = frame.parent) {
            depth += 1;
            value_count += frame.bound.len + frame.captured.len;
        }
        if (depth == 0) return null;

        try self.frames.ensureTotalCapacityPrecise(self.allocator, depth);
        try self.values.ensureTotalCapacityPrecise(self.allocator, value_count);

        const chain = try self.allocator.alloc(*const BindingEnvironment, depth);
        defer self.allocator.free(chain);
        var index = depth;
        cursor = env;
        while (cursor) |frame| : (cursor = frame.parent) {
            index -= 1;
            chain[index] = frame;
        }

        for (chain) |frame| {
            const bound = try self.moveValues(source, name_store, scratch, frame.bound);
            const captured = try self.moveValues(source, name_store, scratch, frame.captured);
            const parent: ?*const BindingEnvironment = if (self.frames.items.len == 0)
                null
            else
                &self.frames.items[self.frames.items.len - 1];
            self.frames.appendAssumeCapacity(.{
                .scheme = frame.scheme,
                .binders = frame.binders,
                .bound = bound,
                .captured = captured,
                .parent = parent,
            });
        }
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn moveValues(
        self: *MovedEnvironment,
        source: *const MonoType.Store,
        name_store: *const names.NameStore,
        scratch: *MonoType.Store,
        values: []const BoundType,
    ) Allocator.Error![]const BoundType {
        const start = self.values.items.len;
        for (values) |value| {
            const moved = try MonoType.reintern(scratch, name_store, source.view(), value.stored);
            self.values.appendAssumeCapacity(BoundType.of(moved));
        }
        return self.values.items[start..];
    }
};

/// One directed translation walk (reunify.md section 9.2). Carries the active
/// map for cycle detection, the reading cursor (which changes when descending a
/// backing declaration in another module), the optional binder environment for
/// substitution, and the scheme owner node for residual disposition lookup.
///
/// `build_store` is the store this walk emits into: the target for an eager
/// walk, and the target or an isolated scratch for a reserve-fill recursive
/// build (reunify.md section 9.2). Names always intern into `owner.target_names`.
/// When `recursion_slots` is non-null the walk is in reserve-before-descend mode:
/// every compound node reserves its stored slot and records it in the map before
/// its children are translated, so a back-reference closes the cycle onto the
/// reserved slot.
const Walk = struct {
    owner: *Translator,
    cursor: ModuleCursor,
    build_store: *MonoType.Store,
    binding_env: ?*const BindingEnvironment,
    scheme_owner_node: u32,
    active: std.AutoHashMap(ActiveNode, void),
    recursion_slots: ?*std.AutoHashMap(ActiveNode, TypeId),
    /// The nominal instances this reserve-fill walk already reserved a slot for,
    /// keyed by declaration and translated arguments. Null in eager mode.
    nominal_instances: ?*NominalInstances,
    skip_reason: *SkipReason,
    /// Set when a reserve-fill node left the subset. The recursive-group builder
    /// (`Store.addRecursive`) cannot carry `error.Skip` out of its fill callback,
    /// so the skip is recorded here and re-raised once the reserved slot returns.
    /// `skip_reason` already holds the recorded reason.
    reserve_fill_skipped: bool = false,

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
        if (self.recursion_slots != null) return try self.nodeReserveFill(checked_ty);

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

    /// Reserve-before-descend translation of one node (reunify.md section 9.2,
    /// 10.6). Leaf and transparent-alias nodes need no reserved slot; a compound
    /// node reserves its stored slot, records it so a back-reference resolves,
    /// then fills it with content whose children were translated in the same
    /// mode. The finished component is a valid rooted cyclic stored graph.
    fn nodeReserveFill(self: *Walk, checked_ty: checked.CheckedTypeId) WalkError!TypeId {
        if (self.envBinder(checked_ty)) |bound| return bound;

        const key = self.activeKey(checked_ty);
        if (self.recursion_slots.?.get(key)) |reserved| return reserved;

        const p = self.cursor.view.payload(checked_ty);
        switch (p) {
            .pending, .err => return self.skip(.pending_or_err),
            .flex, .rigid => |v| return try self.variable(checked_ty, v),
            .empty_record => return try self.build_store.internRecord(self.owner.target_names, &.{}),
            .empty_tag_union => return try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            // A transparent alias erases to its backing, so it holds no stored
            // slot of its own; the cycle closes on the reserved node its backing
            // reaches. The active guard turns a degenerate alias-only cycle into a
            // recorded skip instead of a nonterminating descent.
            .alias => |alias_ty| {
                if (self.active.contains(key)) return self.skip(.recursive_cycle);
                try self.active.put(key, {});
                defer _ = self.active.remove(key);
                return try self.alias(checked_ty, alias_ty);
            },
            // A declaration-backed nominal reserves its slot under its instance
            // identity rather than its checked address, so the backing closes its
            // knot on the nominal itself (`nominalReserveFill`).
            .nominal => |nominal_ty| switch (builtinDisposition(nominal_ty)) {
                .named => {
                    if (self.owner.resolver.nominalBacking(self.cursor, nominal_ty)) |source| {
                        return try self.nominalReserveFill(checked_ty, nominal_ty, source);
                    }
                },
                else => {},
            },
            else => {},
        }

        const Ctx = struct {
            walk: *Walk,
            checked_ty: checked.CheckedTypeId,
            key: ActiveNode,
            p: checked.CheckedTypePayload,

            fn fill(ctx: @This(), reserved: TypeId) Allocator.Error!MonoType.Content {
                try ctx.walk.recursion_slots.?.put(ctx.key, reserved);
                return ctx.walk.payloadContent(ctx.checked_ty, ctx.p) catch |err| switch (err) {
                    // `skip_reason` is already recorded; signal the skip through
                    // the walk so `nodeReserveFill` re-raises it after the slot is
                    // returned (the group builder only carries allocation errors).
                    error.Skip => {
                        ctx.walk.reserve_fill_skipped = true;
                        return .zst;
                    },
                    else => |other| return other,
                };
            }
        };
        const built = try self.build_store.addRecursive(Ctx{
            .walk = self,
            .checked_ty = checked_ty,
            .key = key,
            .p = p,
        }, Ctx.fill);
        if (self.reserve_fill_skipped) return error.Skip;
        return built;
    }

    /// Reserve-before-descend translation of one declaration-backed nominal,
    /// reserving its slot under its instance identity — the declaration plus its
    /// translated arguments (`NominalInstance`) — rather than under the checked
    /// address of this occurrence. Every occurrence of one nominal instance
    /// inside the walk therefore resolves to one slot, so a recursive backing
    /// closes its knot on the nominal and the group is the rooted graph the
    /// nominal denotes (reunify.md sections 8.3, 9.4).
    ///
    /// Arguments translate before the slot is reserved, because they are part of
    /// the identity it is reserved under. A checked graph cannot reach a nominal
    /// instance from inside its own arguments — that is an infinite type in
    /// argument position, which checking never builds — and the active guard
    /// records such a walk as a cycle rather than descending forever.
    fn nominalReserveFill(
        self: *Walk,
        checked_ty: checked.CheckedTypeId,
        n: checked.CheckedNominalType,
        source: Resolver.NominalBacking,
    ) WalkError!TypeId {
        const address = self.activeKey(checked_ty);
        if (self.active.contains(address)) return self.skip(.recursive_cycle);
        try self.active.put(address, {});
        defer _ = self.active.remove(address);

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const instances = self.nominal_instances.?;
        if (instances.find(source.cursor.module_bytes, source.declaration, args.items)) |reserved| return reserved;

        const Ctx = struct {
            walk: *Walk,
            checked_ty: checked.CheckedTypeId,
            address: ActiveNode,
            n: checked.CheckedNominalType,
            source: Resolver.NominalBacking,
            args: []const TypeId,

            fn fill(ctx: @This(), reserved: TypeId) Allocator.Error!MonoType.Content {
                try ctx.walk.recursion_slots.?.put(ctx.address, reserved);
                try ctx.walk.nominal_instances.?.record(
                    ctx.source.cursor.module_bytes,
                    ctx.source.declaration,
                    ctx.args,
                    reserved,
                );
                return ctx.walk.namedContent(ctx.checked_ty, ctx.n, ctx.args) catch |err| switch (err) {
                    error.Skip => {
                        ctx.walk.reserve_fill_skipped = true;
                        return .zst;
                    },
                    else => |other| return other,
                };
            }
        };
        const built = try self.build_store.addRecursive(Ctx{
            .walk = self,
            .checked_ty = checked_ty,
            .address = address,
            .n = n,
            .source = source,
            .args = args.items,
        }, Ctx.fill);
        if (self.reserve_fill_skipped) return error.Skip;
        return built;
    }

    /// Assemble the stored content of one reserved compound node (reunify.md
    /// section 9.2). The children were translated through `node` in reserve-fill
    /// mode, so a back-reference already resolved to a reserved sibling slot.
    fn payloadContent(self: *Walk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!MonoType.Content {
        return switch (p) {
            .record_unbound => |fields| .{ .record = try self.recordSpan(fields, null) },
            .record => |record| .{ .record = try self.recordSpan(record.fields, record.ext) },
            .tuple => |items| .{ .tuple = try self.tupleSpan(items) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagSpan(tag_union.tags, tag_union.ext) },
            .function => |fn_ty| try self.functionContent(fn_ty),
            .nominal => |nominal_ty| try self.nominalContent(checked_ty, nominal_ty),
            // Leaves and aliases never reach a reserved slot (nodeReserveFill
            // builds them directly), so no other payload assembles content here.
            .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union, .alias => unreachable,
        };
    }

    fn payload(self: *Walk, checked_ty: checked.CheckedTypeId, p: checked.CheckedTypePayload) WalkError!TypeId {
        return switch (p) {
            .pending, .err => self.skip(.pending_or_err),
            .flex, .rigid => |v| try self.variable(checked_ty, v),
            .empty_record => try self.build_store.internRecord(self.owner.target_names, &.{}),
            .empty_tag_union => try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            .record_unbound => |fields| try self.recordFrom(fields, null),
            .record => |record| try self.recordFrom(record.fields, record.ext),
            .tuple => |items| try self.tupleFrom(items),
            .tag_union => |tag_union| try self.tagUnionFrom(tag_union.tags, tag_union.ext),
            .function => |fn_ty| try self.function(fn_ty),
            .alias => |alias_ty| try self.alias(checked_ty, alias_ty),
            .nominal => |nominal_ty| try self.nominal(checked_ty, nominal_ty),
        };
    }

    /// The disposition this walk's body context reads for `checked_ty`
    /// (reunify.md section 7.4). Dispositions are scoped by
    /// `(scheme owner, CheckedTypeId)`: the entry under this walk's own scheme
    /// owner is the more specific statement and wins, and the module-body entry
    /// — which the checked side records for a residual belonging to no scheme's
    /// type, and therefore holding in every body of the module — is read when
    /// the scheme owner records none.
    fn dispositionFor(self: *Walk, checked_ty: checked.CheckedTypeId) ?checked.CheckedResidualDisposition {
        var module_wide: ?checked.CheckedResidualDisposition = null;
        for (self.cursor.view.residualDispositions()) |disposition| {
            if (disposition.type_id != @intFromEnum(checked_ty)) continue;
            if (disposition.scheme_owner_node == self.scheme_owner_node) return disposition;
            if (disposition.scheme_owner_node == checked.checked_residual_disposition_module_body_owner) {
                module_wide = disposition;
            }
        }
        return module_wide;
    }

    /// A residual variable: consult its recorded disposition (reunify.md section
    /// 7.4), then apply the checked default. This matches `materializeUnresolved`
    /// exactly: a numeric default yields the defaulted primitive, a row default
    /// yields the empty record or empty tag union, and an undisposed,
    /// undefaulted residual yields the empty tag union — the same stored shape
    /// the graph materializes for an unresolved variable today.
    fn variable(self: *Walk, checked_ty: checked.CheckedTypeId, v: checked.CheckedTypeVariable) WalkError!TypeId {
        if (self.dispositionFor(checked_ty)) |disposition| {
            switch (disposition.kind) {
                .uninhabited => return try self.build_store.internTagUnion(self.owner.target_names, &.{}),
                .contextual => {
                    if (disposition.contextualTarget()) |target| return try self.node(target);
                },
            }
        }

        if (v.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                return self.skip(.numeric_default_unresolved);
            return switch (target) {
                .dec => try self.build_store.internPrimitive(self.owner.target_names, .dec),
                .str => try self.build_store.internPrimitive(self.owner.target_names, .str),
            };
        }
        if (v.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => try self.build_store.internRecord(self.owner.target_names, &.{}),
                .empty_tag_union => try self.build_store.internTagUnion(self.owner.target_names, &.{}),
            };
        }
        return try self.build_store.internTagUnion(self.owner.target_names, &.{});
    }

    fn function(self: *Walk, fn_ty: checked.CheckedFunctionType) WalkError!TypeId {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return try self.build_store.internFunc(self.owner.target_names, args.items, ret);
    }

    fn tupleFrom(self: *Walk, items: []const checked.CheckedTypeId) WalkError!TypeId {
        var lowered = std.ArrayList(TypeId).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.build_store.internTuple(self.owner.target_names, lowered.items);
    }

    /// Collect a record's fields, flattening its extension row exactly as
    /// production record lowering does (walk aliases, an empty-record default,
    /// and nested record rows). A row-extension binder substitutes its bound
    /// stored record, whose fields splice into this row. Shared by the eager and
    /// reserve-fill record builders.
    fn collectRecordFields(
        self: *Walk,
        out: *std.ArrayList(MonoType.Field),
        head: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) WalkError!void {
        try self.appendRecordFields(out, head);

        const ext_start = ext orelse return;
        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext_start;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            if (self.envBinder(current)) |bound| {
                try self.spliceStoredRecord(out, bound);
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
                    try self.appendRecordFields(out, tail);
                    break;
                },
                .record => |record| {
                    try self.appendRecordFields(out, record.fields);
                    current = record.ext;
                },
                else => return self.skip(.open_row),
            }
        }
    }

    fn recordFrom(self: *Walk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!TypeId {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        try self.collectRecordFields(&fields, head, ext);
        return try self.build_store.internRecord(self.owner.target_names, fields.items);
    }

    /// Reserve-fill record content: the same flattened fields as `recordFrom`,
    /// added to the build store as a field span rather than interned as a root.
    fn recordSpan(self: *Walk, head: []const checked.CheckedRecordField, ext: ?checked.CheckedTypeId) WalkError!MonoType.Span {
        var fields = std.ArrayList(MonoType.Field).empty;
        defer fields.deinit(self.owner.allocator);
        try self.collectRecordFields(&fields, head, ext);
        return try self.build_store.addRecordFields(self.owner.target_names, fields.items);
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
        switch (self.build_store.get(id)) {
            .record => |span| {
                const field_span = self.build_store.fieldSpan(span);
                for (0..collections.GuardedList.borrowLen(field_span)) |i| {
                    try out.append(self.owner.allocator, collections.GuardedList.at(field_span, i));
                }
            },
            else => return self.skip(.open_row),
        }
    }

    /// Collect a tag union's tags, flattening its extension row exactly as
    /// production tag-union lowering does. Shared by the eager and reserve-fill
    /// tag-union builders. The caller owns the returned inputs and frees them
    /// through `freeTagInputs`.
    fn collectTags(
        self: *Walk,
        out: *std.ArrayList(MonoType.Store.TagInput),
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) WalkError!void {
        try self.appendTags(out, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.owner.allocator);
        defer seen.deinit();
        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});
            if (self.envBinder(current)) |bound| {
                try self.spliceStoredTags(out, bound);
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
                    try self.appendTags(out, tag_union.tags);
                    current = tag_union.ext;
                },
                else => return self.skip(.open_row),
            }
        }
    }

    fn tagUnionFrom(self: *Walk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!TypeId {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);
        try self.collectTags(&tags, head, ext);
        return try self.build_store.internTagUnion(self.owner.target_names, tags.items);
    }

    /// Reserve-fill tag-union content: the same flattened tags as `tagUnionFrom`,
    /// added to the build store as a tag span rather than interned as a root.
    fn tagSpan(self: *Walk, head: []const checked.CheckedTag, ext: checked.CheckedTypeId) WalkError!MonoType.Span {
        var tags = std.ArrayList(MonoType.Store.TagInput).empty;
        defer self.freeTagInputs(&tags);
        try self.collectTags(&tags, head, ext);

        var variants = std.ArrayList(MonoType.Tag).empty;
        defer variants.deinit(self.owner.allocator);
        for (tags.items) |tag| {
            try variants.append(self.owner.allocator, .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.build_store.addSpan(tag.payloads),
            });
        }
        return try self.build_store.addTagVariants(self.owner.target_names, variants.items);
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
        switch (self.build_store.get(id)) {
            .tag_union => |span| {
                const tag_span = self.build_store.tagSpan(span);
                for (0..collections.GuardedList.borrowLen(tag_span)) |i| {
                    const tag = collections.GuardedList.at(tag_span, i);
                    const payload_span = self.build_store.span(tag.payloads);
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
        return try self.build_store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = alias_ty.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, alias_ty.origin_module, alias_ty.name, alias_ty.source_decl),
            .kind = .alias,
            .builtin_owner = null,
            .args = args.items,
            .backing = .{ .ty = backing, .use = .inspectable },
        });
    }

    /// How a nominal's builtin runtime encoding lowers before the general named
    /// build (reunify.md section 9.2). A primitive/list/box encoding lowers to that
    /// structural shape; a generated opaque-evidence encoding needs a backing the
    /// section 10 closure engine mints (reunify.md section 9.1), which the checked
    /// data cannot dictate, so it is `engine_input_needed`; every other encoding
    /// keeps declaration identity as a named node.
    const BuiltinDisposition = union(enum) {
        primitive: MonoType.Primitive,
        list,
        box,
        named,
        engine_input_needed,
    };

    fn builtinDisposition(n: checked.CheckedNominalType) BuiltinDisposition {
        return switch (n.representation) {
            .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                .primitive => |value| .{ .primitive = value },
                .list => .list,
                .box => .box,
                // Generated opaque-evidence nominals with no declaration backing:
                // the identity is derivable, but the backing the graph mints is a
                // step (b) engine decision, so emitting a named node without it
                // would be wrong output (reunify.md section 10.3). The crypto
                // digest/hasher nominals are excluded: they carry a fixed
                // declaration backing and are translated like any other nominal.
                // An iterator nominal is the same shape of case: its tier and
                // minted backing are chosen by the representation engine, not
                // recorded in checked module data.
                .parse_tag_union_spec,
                .fields,
                .field,
                .iterator,
                => .engine_input_needed,
                .bool_tag_union,
                .try_nominal,
                .dict,
                .set,
                .crypto_sha256_digest,
                .crypto_sha256_hasher,
                .crypto_blake3_digest,
                .crypto_blake3_hasher,
                => .named,
            },
            else => .named,
        };
    }

    /// A nominal or opaque. Builtin nominals whose runtime encoding is a
    /// primitive, list, or box lower to that structural shape, matching
    /// production; the rest keep declaration identity as a stored named node with
    /// its backing, dispatch owner, and declared field order. Iterator tier and
    /// generated owner are graph-minted, not in checked module data, so they stay
    /// at their defaults here.
    fn nominal(self: *Walk, checked_ty: checked.CheckedTypeId, n: checked.CheckedNominalType) WalkError!TypeId {
        switch (builtinDisposition(n)) {
            .primitive => |value| return try self.build_store.internPrimitive(self.owner.target_names, value),
            .list => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return try self.build_store.internList(self.owner.target_names, try self.node(n.args[0]));
            },
            .box => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return try self.build_store.internBox(self.owner.target_names, try self.node(n.args[0]));
            },
            .engine_input_needed => return self.skip(.engine_input_needed),
            .named => {},
        }

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }

        const backing = try self.nominalBacking(n, args.items);
        const declared_order = try self.declaredOrder(n);
        defer self.owner.allocator.free(declared_order);

        return try self.build_store.internNamed(self.owner.target_names, .{
            .named_type = .{ .module = .{ .bytes = n.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, n.origin_module, n.name, n.source_decl),
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
            .args = args.items,
            .backing = backing,
            .declared_order = declared_order,
        });
    }

    // --- Reserve-fill content assembly (reunify.md section 9.2, 10.6) ---
    //
    // These build the stored content of one reserved compound node: children were
    // translated through `node` in reserve-fill mode, so back-references already
    // resolved to reserved sibling slots. Each mirrors the eager builder of the
    // same shape but returns `Content` for the reserved slot rather than interning
    // a fresh root.

    fn functionContent(self: *Walk, fn_ty: checked.CheckedFunctionType) WalkError!MonoType.Content {
        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (fn_ty.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        const ret = try self.node(fn_ty.ret);
        return .{ .func = .{ .args = try self.build_store.addSpan(args.items), .ret = ret } };
    }

    fn tupleSpan(self: *Walk, items: []const checked.CheckedTypeId) WalkError!MonoType.Span {
        var lowered = std.ArrayList(TypeId).empty;
        defer lowered.deinit(self.owner.allocator);
        for (items) |item| {
            try lowered.append(self.owner.allocator, try self.node(item));
        }
        return try self.build_store.addSpan(lowered.items);
    }

    /// Reserve-fill named/nominal content. A builtin primitive/list/box encoding
    /// still reserved its slot, so it fills that slot with the leaf shape; a
    /// generated opaque-evidence nominal is an engine step (b) input and skips.
    fn nominalContent(self: *Walk, checked_ty: checked.CheckedTypeId, n: checked.CheckedNominalType) WalkError!MonoType.Content {
        switch (builtinDisposition(n)) {
            .primitive => |value| return .{ .primitive = value },
            .list => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return .{ .list = try self.node(n.args[0]) };
            },
            .box => {
                if (n.args.len != 1) return self.skip(.malformed_builtin_arity);
                return .{ .box = try self.node(n.args[0]) };
            },
            .engine_input_needed => return self.skip(.engine_input_needed),
            .named => {},
        }

        var args = std.ArrayList(TypeId).empty;
        defer args.deinit(self.owner.allocator);
        for (n.args) |arg| {
            try args.append(self.owner.allocator, try self.node(arg));
        }
        return try self.namedContent(checked_ty, n, args.items);
    }

    /// The stored named content of one nominal whose arguments are already
    /// translated, for a slot reserved before the descent (reunify.md section
    /// 9.2). Iterator tier and generated owner are graph-minted, not in checked
    /// module data, so they stay at their defaults here.
    fn namedContent(
        self: *Walk,
        checked_ty: checked.CheckedTypeId,
        n: checked.CheckedNominalType,
        args: []const TypeId,
    ) WalkError!MonoType.Content {
        const backing = try self.nominalBacking(n, args);
        const declared_order = try self.declaredOrder(n);
        defer self.owner.allocator.free(declared_order);

        return .{ .named = .{
            .named_type = .{ .module = .{ .bytes = n.owner_module.bytes }, .ty = checked_ty },
            .def = try self.owner.typeDef(self.cursor, n.origin_module, n.name, n.source_decl),
            .kind = if (n.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = self.owner.resolver.builtinOwner(self.cursor, n),
            .args = try self.build_store.addSpan(args),
            .backing = backing,
            .declared_order = try self.build_store.addDeclaredFields(declared_order),
        } };
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

    fn addBuiltinNominal(self: *TestFixture, builtin_nominal: checked.CheckedBuiltinNominal, name_text: []const u8) Allocator.Error!checked.CheckedTypeId {
        return try self.addPrimitiveNominal(builtin_nominal, name_text);
    }

    /// A builtin `List elem` nominal, whose runtime encoding lowers to a stored
    /// list of the translated element.
    fn addUserBuiltinList(self: *TestFixture, elem: checked.CheckedTypeId) Allocator.Error!checked.CheckedTypeId {
        const name = try self.source_names.internTypeName("List");
        const module = try self.source_names.internModuleIdentity(&self.module_hash);
        const start: u32 = @intCast(self.type_id_pool.items.len);
        try self.type_id_pool.append(self.allocator, elem);
        return try self.add(.{ .nominal = .{
            .name = name,
            .origin_module = module,
            .owner_module = .{ .bytes = self.module_hash },
            .is_opaque = false,
            .representation = .{ .builtin = .list },
            .args = .{ .start = start, .len = 1 },
        } });
    }

    /// One tag with its payload type ids appended into `type_id_pool`.
    const TagSpec = struct {
        name_text: []const u8,
        payloads: []const checked.CheckedTypeId,
    };

    /// Add a tag union with an empty closed extension. `tags` payloads may name
    /// any already-reserved id, including `self_id` to build a recursive knot.
    fn addTagUnion(self: *TestFixture, tags: []const TagSpec, ext: checked.CheckedTypeId) Allocator.Error!checked.CheckedTypeId {
        const tags_start: u32 = @intCast(self.tags.items.len);
        for (tags) |tag| {
            const name = try self.source_names.internTagLabel(tag.name_text);
            const args_start: u32 = @intCast(self.type_id_pool.items.len);
            try self.type_id_pool.appendSlice(self.allocator, tag.payloads);
            try self.tags.append(self.allocator, .{
                .name = name,
                .args_start = args_start,
                .args_len = @intCast(tag.payloads.len),
            });
        }
        return try self.add(.{ .tag_union = .{
            .tags = .{ .start = tags_start, .len = @intCast(tags.len) },
            .ext = ext,
        } });
    }

    /// The id the next `add` will assign, so a recursive payload can name the
    /// node it belongs to before the node itself is added.
    fn nextId(self: *TestFixture) checked.CheckedTypeId {
        return @enumFromInt(@as(u32, @intCast(self.payloads.items.len)));
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
            .declaration = 0,
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

test "a self-referential record is built through the recursive-group builder" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    // A record { self: <this record> }: the field type is the record's own id,
    // so the eager walk's cycle guard fires and the root is rebuilt through the
    // recursive-group builder into a closed self-recursive record.
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
    const root = try translator.translateGroundRoot(fixture.cursor(), record_ty, &reason);
    switch (store.get(root)) {
        .record => |span| {
            const field_span = store.fieldSpan(span);
            try testing.expectEqual(@as(usize, 1), collections.GuardedList.borrowLen(field_span));
            try testing.expectEqual(root, collections.GuardedList.at(field_span, 0).ty);
        },
        else => try testing.expect(false),
    }
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

/// Assert a stored root is a self-recursive tag union: its single tag's payload
/// resolves back to the root id, so the cycle closed through a reserved slot.
/// True when `root` is a single-tag, single-payload tag union whose payload
/// resolves back to the root id: a self-recursive knot closed through a reserved
/// slot. Non-fallible so the assertion stays inside the test block.
fn isSelfRecursiveTagUnion(store: *MonoType.Store, root: TypeId) bool {
    switch (store.get(root)) {
        .tag_union => |span| {
            const tag_span = store.tagSpan(span);
            if (collections.GuardedList.borrowLen(tag_span) != 1) return false;
            const tag = collections.GuardedList.at(tag_span, 0);
            const payloads = store.span(tag.payloads);
            if (collections.GuardedList.borrowLen(payloads) != 1) return false;
            return collections.GuardedList.at(payloads, 0) == root;
        },
        else => return false,
    }
}

test "a self-recursive tag union is built through the recursive-group builder" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const self_id = fixture.nextId();
    const root = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{self_id} }}, empty);
    try testing.expectEqual(self_id, root);

    // Off: the recursive group is built reserve-fill in place. On: it is built
    // into a scratch store and re-interned. Either way the knot stays closed.
    inline for (.{ false, true }) |intern_on| {
        var store = initTargetStore();
        defer store.deinit();
        if (intern_on) store.enableInterning();
        var target_names = names.NameStore.init(testing.allocator);
        defer target_names.deinit();

        var no_backing = NoBackingResolver{};
        var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
        defer translator.deinit();

        var reason: SkipReason = undefined;
        const built = try translator.translateGroundRoot(fixture.cursor(), root, &reason);
        try testing.expect(isSelfRecursiveTagUnion(&store, built));
    }
}

test "two structurally equal recursive tag unions dedup with interning on, differ off" {
    // Build the same self-recursive tag union from two independent checked roots
    // and translate each with one translator, so the second reaches the first's
    // registered rooted group under interning.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const first_id = fixture.nextId();
    const first = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{first_id} }}, empty);
    const second_id = fixture.nextId();
    const second = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{second_id} }}, empty);

    inline for (.{ true, false }) |intern_on| {
        var store = initTargetStore();
        defer store.deinit();
        if (intern_on) store.enableInterning();
        var target_names = names.NameStore.init(testing.allocator);
        defer target_names.deinit();

        var no_backing = NoBackingResolver{};
        var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
        defer translator.deinit();

        var reason: SkipReason = undefined;
        const a = try translator.translateGroundRoot(fixture.cursor(), first, &reason);
        const b = try translator.translateGroundRoot(fixture.cursor(), second, &reason);
        try testing.expect(isSelfRecursiveTagUnion(&store, a));
        try testing.expect(isSelfRecursiveTagUnion(&store, b));
        if (intern_on) {
            try testing.expectEqual(a, b);
        } else {
            try testing.expect(a != b);
        }
    }
}

test "a mutually recursive tag-union pair builds a closed two-node group" {
    // A = [ToB B], B = [ToA A]: a two-node cycle with distinct heads.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    const a_id = fixture.nextId();
    const b_id: checked.CheckedTypeId = @enumFromInt(@intFromEnum(a_id) + 1);
    const a = try fixture.addTagUnion(&.{.{ .name_text = "ToB", .payloads = &.{b_id} }}, empty);
    const b = try fixture.addTagUnion(&.{.{ .name_text = "ToA", .payloads = &.{a_id} }}, empty);
    try testing.expectEqual(a_id, a);
    try testing.expectEqual(b_id, b);

    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const a_root = try translator.translateGroundRoot(fixture.cursor(), a, &reason);

    // A's single tag payload is B; B's single tag payload is A's root — a closed
    // cycle back to the entered root.
    const b_root = payload_of: {
        switch (store.get(a_root)) {
            .tag_union => |span| {
                const tag_span = store.tagSpan(span);
                const tag = collections.GuardedList.at(tag_span, 0);
                break :payload_of collections.GuardedList.at(store.span(tag.payloads), 0);
            },
            else => return testing.expect(false),
        }
    };
    try testing.expect(a_root != b_root);
    switch (store.get(b_root)) {
        .tag_union => |span| {
            const tag_span = store.tagSpan(span);
            const tag = collections.GuardedList.at(tag_span, 0);
            try testing.expectEqual(a_root, collections.GuardedList.at(store.span(tag.payloads), 0));
        },
        else => try testing.expect(false),
    }
}

test "a recursive tag union nested under a list reproduces the graph shape" {
    // Rec = [Node (List Rec)]: the cycle passes through a builtin list nominal.
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const empty = try fixture.add(.empty_tag_union);
    // The list is added next, then the tag union, so the tag union's id is one
    // past the list's; the list's element names that future tag-union id.
    const rec_id: checked.CheckedTypeId = @enumFromInt(@intFromEnum(fixture.nextId()) + 1);
    const list_of_rec = try fixture.addUserBuiltinList(rec_id);
    const rec = try fixture.addTagUnion(&.{.{ .name_text = "Node", .payloads = &.{list_of_rec} }}, empty);
    try testing.expectEqual(rec_id, rec);

    var store = initTargetStore();
    defer store.deinit();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    const root = try translator.translateGroundRoot(fixture.cursor(), rec, &reason);
    switch (store.get(root)) {
        .tag_union => |span| {
            const tag = collections.GuardedList.at(store.tagSpan(span), 0);
            const list_id = collections.GuardedList.at(store.span(tag.payloads), 0);
            switch (store.get(list_id)) {
                .list => |elem| try testing.expectEqual(root, elem),
                else => try testing.expect(false),
            }
        },
        else => try testing.expect(false),
    }
}

test "a generated opaque-evidence builtin nominal skips as engine_input_needed" {
    var fixture = TestFixture.init(testing.allocator);
    defer fixture.deinit();

    const field_ty = try fixture.addBuiltinNominal(.field, "FieldName");

    var store = initTargetStore();
    defer store.deinit();
    store.enableInterning();
    var target_names = names.NameStore.init(testing.allocator);
    defer target_names.deinit();

    var no_backing = NoBackingResolver{};
    var translator = Translator.init(testing.allocator, &store, &target_names, no_backing.resolver());
    defer translator.deinit();

    var reason: SkipReason = undefined;
    try testing.expectError(error.Skip, translator.translateGroundRoot(fixture.cursor(), field_ty, &reason));
    try testing.expectEqual(SkipReason.engine_input_needed, reason);
}

test "declarations are referenced" {
    testing.refAllDecls(@This());
}
