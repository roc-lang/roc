//! Exact identities for generated codec proof graphs. Source contracts remain
//! intact for lowering; only equivalent contracts share specialization identity.

const std = @import("std");
const checked = @import("checked_artifact.zig");
const dispatch = @import("static_dispatch_registry.zig");
const Allocator = std.mem.Allocator;
const TypeId = checked.CheckedTypeId;
const CodecId = dispatch.GeneratedCodecDerivationId;
const Span = @import("artifact_serialize.zig").Span;
const canonical = @import("canonical_names.zig");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const collections = @import("collections");
const type_roles = .{ "source_constructor_ty", "source_runtime_ty", "source_shape_ty", "source_body_shape_ty", "source_encoding_ty", "source_state_ty", "source_error_ty", "constructor_ty", "runtime_ty", "shape_ty", "body_shape_ty", "encoding_ty", "state_ty", "error_ty" };

/// Intern each completed contract once, in table order. Type and call keys
/// select candidates; exact comparison of the proof graph is the equality
/// authority. A nested contract interned earlier in the order is reached
/// through its identity, so a comparison never walks a nested chain again.
pub fn intern(allocator: Allocator, types: checked.CheckedTypeStoreView, table: *dispatch.StaticDispatchPlanTable) Allocator.Error!void {
    var buckets = std.AutoHashMap(u64, u32).init(allocator);
    defer buckets.deinit();
    const next = try allocator.alloc(?u32, table.generated_codec_derivations.len);
    defer allocator.free(next);
    var comparer = Comparer{ .allocator = allocator, .types = types, .table = table };
    defer comparer.deinit();
    for (table.generated_codec_derivations, 0..) |*derivation, index| {
        const raw: u32 = @intCast(index);
        comparer.interned_len = raw;
        // Bucket selector only: the derivation's own types and method names
        // are package-controlled, but a Wyhash collision just adds a chain
        // step, because a candidate is accepted solely by `comparer.equal`.
        var hash = std.hash.Wyhash.init(@backingInt(derivation.kind));
        inline for (type_roles) |role| hash.update(&types.rootKey(@field(derivation, role)).bytes);
        hash.update(std.mem.asBytes(&derivation.calls.len));
        for (derivation.callsSlice(table)) |call| {
            std.hash.autoHash(&hash, call.method);
            std.hash.autoHash(&hash, call.method_role);
            std.hash.autoHash(&hash, std.meta.activeTag(call.resolution));
            hash.update(&types.rootKey(call.dispatcher_ty).bytes);
            hash.update(&types.rootKey(call.callable_ty).bytes);
            std.hash.autoHash(&hash, call.subject_ty != null);
            if (call.subject_ty) |subject| hash.update(&types.rootKey(subject).bytes);
        }
        const key = hash.final();
        var candidate = buckets.get(key);
        while (candidate) |previous| {
            if (try comparer.equal(raw, previous)) break;
            candidate = next[previous];
        }
        derivation.identity = @fromBackingInt(@intCast(candidate orelse raw));
        if (candidate == null) {
            next[raw] = buckets.get(key);
            try buckets.put(key, raw);
        }
    }
}

/// Give each contract the content digest of its proof graph. Contracts that
/// `intern` placed in one class share the digest of the class's first member.
/// `owner` is the artifact recording the table. Must run after `intern`.
pub fn digestContents(
    allocator: Allocator,
    owner: checked.ModuleId,
    store: *const checked.CheckedTypeStore,
    names: *const canonical.CanonicalNameStore,
    table: *dispatch.StaticDispatchPlanTable,
) Allocator.Error!void {
    var digester = ContentDigester{
        .allocator = allocator,
        .owner = owner,
        .store = store,
        .names = names,
        .table = table,
        .keys = checked.CheckedTypeKeys.init(allocator, names, store),
        .orderer = checked.CheckedIdentityOrderer.init(allocator, names, store),
    };
    defer digester.deinit();
    for (table.generated_codec_derivations, 0..) |*derivation, index| {
        const representative = @backingInt(derivation.identity);
        derivation.content = if (representative == index)
            try digester.digest(@intCast(index))
        else
            table.generated_codec_derivations[representative].content;
    }
}

/// A depth-first canonical encoding of one contract's proof graph. Contracts
/// and evidence nodes are numbered by first visit, so shared and recursive
/// references encode as back-references. A type encodes as its canonical key
/// plus the joint first-visit numbers of its variables in key order, so
/// variables shared between roots stay shared. Methods encode by name and
/// targets by their owning artifacts; no module-local id is written.
const ContentDigester = struct {
    allocator: Allocator,
    owner: checked.ModuleId,
    store: *const checked.CheckedTypeStore,
    names: *const canonical.CanonicalNameStore,
    table: *const dispatch.StaticDispatchPlanTable,
    keys: checked.CheckedTypeKeys,
    hasher: TypeDigestHasher = undefined,
    codec_ordinals: std.AutoHashMapUnmanaged(u32, u32) = .empty,
    evidence_ordinals: std.AutoHashMapUnmanaged(u32, u32) = .empty,
    variable_ordinals: collections.DenseMapUnmanaged(TypeId, u32) = .empty,
    /// Each root's variables in key order, as a span of `identity_order_pool`.
    /// Checked types are immutable here, so the order is computed once per
    /// root for every contract.
    identity_orders: collections.DenseMapUnmanaged(TypeId, Span) = .empty,
    identity_order_pool: std.ArrayList(TypeId) = .empty,
    orderer: checked.CheckedIdentityOrderer,
    tasks: std.ArrayList(Task) = .empty,
    children: std.ArrayList(Task) = .empty,

    const Task = union(enum) { codec: u32, evidence: u32, refs: Span };

    fn deinit(self: *ContentDigester) void {
        self.keys.deinit();
        self.codec_ordinals.deinit(self.allocator);
        self.evidence_ordinals.deinit(self.allocator);
        self.variable_ordinals.deinit(self.allocator);
        self.identity_orders.deinit(self.allocator);
        self.identity_order_pool.deinit(self.allocator);
        self.orderer.deinit();
        self.tasks.deinit(self.allocator);
        self.children.deinit(self.allocator);
    }

    fn digest(self: *ContentDigester, root: u32) Allocator.Error!dispatch.GeneratedCodecContentDigest {
        self.hasher = TypeDigestHasher.init();
        self.hasher.update("roc.generated_codec.content.v1");
        self.codec_ordinals.clearRetainingCapacity();
        self.evidence_ordinals.clearRetainingCapacity();
        self.variable_ordinals.clearRetainingCapacity();
        self.tasks.clearRetainingCapacity();
        try self.tasks.append(self.allocator, .{ .codec = root });
        while (self.tasks.pop()) |task| {
            self.children.clearRetainingCapacity();
            switch (task) {
                .codec => |id| try self.writeCodec(id),
                .evidence => |id| try self.writeEvidence(id),
                .refs => |span| try self.writeRefs(span),
            }
            // Children run in the order they were written, after this node.
            var index = self.children.items.len;
            while (index > 0) {
                index -= 1;
                try self.tasks.append(self.allocator, self.children.items[index]);
            }
        }
        return .{ .bytes = self.hasher.finalResult() };
    }

    /// Writes a back-reference and returns false when `id` was already
    /// visited; otherwise numbers it and returns true.
    fn firstVisit(self: *ContentDigester, ordinals: *std.AutoHashMapUnmanaged(u32, u32), id: u32) Allocator.Error!bool {
        const entry = try ordinals.getOrPut(self.allocator, id);
        if (entry.found_existing) {
            self.writeU8(0);
            self.writeU32(entry.value_ptr.*);
            return false;
        }
        entry.value_ptr.* = ordinals.count() - 1;
        self.writeU8(1);
        return true;
    }

    fn writeCodec(self: *ContentDigester, id: u32) Allocator.Error!void {
        if (!try self.firstVisit(&self.codec_ordinals, id)) return;
        const derivation = self.table.generated_codec_derivations[id];
        self.writeU8(@backingInt(derivation.kind));
        inline for (type_roles) |role| try self.writeType(@field(derivation, role));
        const calls = derivation.callsSlice(self.table);
        self.writeU32(@intCast(calls.len));
        for (calls) |call| {
            self.writeText(self.names.methodNameText(call.method));
            self.writeU32(call.method_role);
            try self.writeType(call.dispatcher_ty);
            try self.writeType(call.callable_ty);
            try self.writeOptionalType(call.subject_ty);
            self.writeU8(@backingInt(std.meta.activeTag(call.resolution)));
            switch (call.resolution) {
                .pending => unreachable,
                .checked_error => {},
                .callable => |node| try self.children.append(self.allocator, .{ .evidence = @backingInt(node) }),
                .structural => |nested| try self.children.append(self.allocator, .{ .codec = @backingInt(nested) }),
            }
        }
    }

    fn writeEvidence(self: *ContentDigester, id: u32) Allocator.Error!void {
        if (!try self.firstVisit(&self.evidence_ordinals, id)) return;
        const node = self.table.evidence_nodes[id];
        self.writeTarget(node.target);
        try self.writeOptionalType(node.dispatcher_ty);
        self.writeU8(@backingInt(std.meta.activeTag(node.instantiation)));
        switch (node.instantiation) {
            .monomorphic => {},
            .callable, .alias, .derived_from_callable => |ty| try self.writeType(ty),
        }
        const subst = self.table.site_substitutions[node.subst.start..][0..node.subst.len];
        self.writeU32(@intCast(subst.len));
        for (subst) |ty| try self.writeType(ty);
        self.writeU8(@intFromBool(node.generated_codec_derivation != null));
        if (node.generated_codec_derivation) |nested| try self.children.append(self.allocator, .{ .codec = @backingInt(nested) });
        self.writeU8(@backingInt(std.meta.activeTag(node.nested)));
        switch (node.nested) {
            .from_callable => {},
            .resolved => |span| try self.children.append(self.allocator, .{ .refs = span }),
        }
    }

    /// A target is named by what identifies it in every artifact. Its
    /// `module_idx` and `def_idx` index the module list of whichever artifact
    /// registered it, and its `callable_ty` is its own declared type in the
    /// target module's store, so none of them is written. A procedure is named
    /// by its owning artifact's refs, a local procedure by its lexical site in
    /// the recording artifact, and a structural target by its kind, which with
    /// the edge's dispatcher fixes its implementation.
    fn writeTarget(self: *ContentDigester, target: dispatch.MethodTarget) void {
        self.writeU8(@backingInt(std.meta.activeTag(target.kind)));
        switch (target.kind) {
            .procedure => |procedure| {
                self.hasher.update(&procedure.proc.artifact.bytes);
                self.writeU32(@backingInt(procedure.proc.proc_base));
                self.hasher.update(&procedure.template.artifact.bytes);
                self.writeU32(@backingInt(procedure.template.proc_base));
                self.writeU32(@backingInt(procedure.template.template));
            },
            .local_proc => |local| {
                self.hasher.update(&self.owner.bytes);
                self.writeU32(@backingInt(local.binder));
                self.writeU32(@backingInt(local.expr));
                self.writeU32(@backingInt(local.context_anchor));
                self.writeU8(@intFromBool(local.dispatch_scope != null));
                if (local.dispatch_scope) |scope| self.writeU32(@backingInt(scope));
            },
            .structural => |kind| self.writeU8(@backingInt(kind)),
        }
        self.writeU8(@intFromBool(target.alias != null));
        if (target.alias) |alias| switch (alias) {
            .top_level => |pattern| {
                self.writeU8(0);
                self.writeU32(@backingInt(pattern));
            },
            .local => |local| {
                self.writeU8(1);
                self.writeU32(@backingInt(local.lookup));
            },
        };
    }

    fn writeRefs(self: *ContentDigester, span: Span) Allocator.Error!void {
        const refs = self.table.evidence_refs[span.start..][0..span.len];
        self.writeU32(@intCast(refs.len));
        for (refs) |ref| {
            self.writeU8(@intFromBool(ref.runtime_dictionary));
            try self.writeType(ref.dispatcher_ty);
            self.writeU8(@backingInt(std.meta.activeTag(ref.resolution)));
            switch (ref.resolution) {
                .direct => |node| try self.children.append(self.allocator, .{ .evidence = @backingInt(node) }),
                .constraint => |constraint| {
                    self.writeOptionalU32(constraint.scheme_param);
                    self.writeU32(constraint.index.depth);
                    self.writeU32(constraint.index.index);
                    self.writeU8(@intFromBool(constraint.independent_callable));
                    self.writeU8(@intFromBool(constraint.reuse_slot_nested_evidence));
                    self.writeOptionalU32(constraint.callable_contract);
                },
                .structural => |structural| {
                    self.writeU8(@backingInt(std.meta.activeTag(structural.derivation)));
                    switch (structural.derivation) {
                        .map, .map_effectful => |plan| {
                            self.writeU32(@backingInt(plan.tag));
                            self.writeU32(plan.payload_index);
                        },
                        .equality, .hash, .parser, .encoder => {},
                    }
                    try self.writeType(structural.dispatcher_ty);
                    try self.writeType(structural.callable_ty);
                    self.writeU8(@intFromBool(structural.generated_codec_derivation != null));
                    if (structural.generated_codec_derivation) |nested| try self.children.append(self.allocator, .{ .codec = @backingInt(nested) });
                },
                .from_callable, .from_scheme, .checked_error, .unreachable_value => {},
            }
            try self.children.append(self.allocator, .{ .refs = ref.callable_contracts });
        }
    }

    fn writeType(self: *ContentDigester, ty: TypeId) Allocator.Error!void {
        self.hasher.update(&(try self.keys.key(ty)).bytes);
        const order = try self.identityOrder(ty);
        for (order) |variable| {
            const entry = try self.variable_ordinals.getOrPut(self.allocator, variable);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(self.variable_ordinals.count() - 1);
            self.writeU32(entry.value_ptr.*);
        }
    }

    fn writeOptionalType(self: *ContentDigester, ty: ?TypeId) Allocator.Error!void {
        self.writeU8(@intFromBool(ty != null));
        if (ty) |present| try self.writeType(present);
    }

    fn identityOrder(self: *ContentDigester, ty: TypeId) Allocator.Error![]const TypeId {
        const entry = try self.identity_orders.getOrPut(self.allocator, ty);
        if (!entry.found_existing) {
            errdefer _ = self.identity_orders.remove(ty);
            const order = try self.orderer.identityRoots(ty);
            const start = std.math.cast(u32, self.identity_order_pool.items.len) orelse return error.OutOfMemory;
            const len = std.math.cast(u32, order.len) orelse return error.OutOfMemory;
            try self.identity_order_pool.appendSlice(self.allocator, order);
            entry.value_ptr.* = .{ .start = start, .len = len };
        }
        const span = entry.value_ptr.*;
        return self.identity_order_pool.items[span.start..][0..span.len];
    }

    fn writeText(self: *ContentDigester, text: []const u8) void {
        self.writeU32(@intCast(text.len));
        self.hasher.update(text);
    }

    fn writeOptionalU32(self: *ContentDigester, value: ?u32) void {
        self.writeU8(@intFromBool(value != null));
        if (value) |present| self.writeU32(present);
    }

    fn writeU32(self: *ContentDigester, value: u32) void {
        var bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &bytes, value, .little);
        self.hasher.update(&bytes);
    }

    fn writeU8(self: *ContentDigester, value: u8) void {
        self.hasher.update(&.{value});
    }
};

/// Whether two calls of one generated codec body select the same method with
/// the same proof: equal checked types modulo transparent aliases and fresh
/// variable names, and equivalent resolution graphs. Repeated occurrences of a
/// subject each record their own edge, whose evidence nodes are distinct
/// allocations with the same content.
pub fn callsEquivalent(
    allocator: Allocator,
    types: checked.CheckedTypeStoreView,
    table: *const dispatch.StaticDispatchPlanTable,
    left: dispatch.GeneratedCodecCall,
    right: dispatch.GeneratedCodecCall,
) Allocator.Error!bool {
    var comparer = Comparer{ .allocator = allocator, .types = types, .table = table, .types_eql = .alias_transparent };
    defer comparer.deinit();
    if (left.method != right.method or std.meta.activeTag(left.resolution) != std.meta.activeTag(right.resolution)) return false;
    try comparer.typesPair(left.dispatcher_ty, right.dispatcher_ty);
    try comparer.typesPair(left.callable_ty, right.callable_ty);
    if (!try comparer.optionalTypes(left.subject_ty, right.subject_ty)) return false;
    switch (left.resolution) {
        .pending => unreachable,
        .checked_error => {},
        .callable => |id| try comparer.work.append(allocator, .{ .kind = .evidence, .left = @backingInt(id), .right = @backingInt(right.resolution.callable) }),
        .structural => |id| if (!try comparer.codecs(id, right.resolution.structural)) return false,
    }
    return try comparer.run();
}

const Pair = struct { kind: enum { codec, evidence }, left: u32, right: u32 };
const Comparer = struct {
    allocator: Allocator,
    types: checked.CheckedTypeStoreView,
    table: *const dispatch.StaticDispatchPlanTable,
    /// Specialization identity is exact; agreement between repeated
    /// occurrences of one role is modulo transparent aliases, like the role.
    types_eql: enum { exact, alias_transparent } = .exact,
    /// While interning, how many contracts at the front of the table already
    /// hold their identity. Two of them are equal exactly when their
    /// identities are.
    interned_len: u32 = 0,
    work: std.ArrayList(Pair) = .empty,
    seen: std.AutoHashMapUnmanaged(Pair, void) = .empty,
    left_types: std.ArrayList(TypeId) = .empty,
    right_types: std.ArrayList(TypeId) = .empty,

    fn deinit(self: *Comparer) void {
        self.work.deinit(self.allocator);
        self.seen.deinit(self.allocator);
        self.left_types.deinit(self.allocator);
        self.right_types.deinit(self.allocator);
    }

    fn typesPair(self: *Comparer, left: TypeId, right: TypeId) Allocator.Error!void {
        try self.left_types.append(self.allocator, left);
        try self.right_types.append(self.allocator, right);
    }

    fn optionalTypes(self: *Comparer, left: ?TypeId, right: ?TypeId) Allocator.Error!bool {
        if ((left == null) != (right == null)) return false;
        if (left) |ty| try self.typesPair(ty, right.?);
        return true;
    }

    fn codecs(self: *Comparer, left: ?CodecId, right: ?CodecId) Allocator.Error!bool {
        if ((left == null) != (right == null)) return false;
        const left_id = left orelse return true;
        const right_index = @backingInt(right.?);
        const left_index = @backingInt(left_id);
        if (left_index < self.interned_len and right_index < self.interned_len) {
            const derivations = self.table.generated_codec_derivations;
            return derivations[left_index].identity == derivations[right_index].identity;
        }
        try self.work.append(self.allocator, .{ .kind = .codec, .left = left_index, .right = right_index });
        return true;
    }

    /// Compare two evidence reference spans, and the callable contracts
    /// they carry, from an explicit work list.
    fn refs(self: *Comparer, root_left: Span, root_right: Span) Allocator.Error!bool {
        var pending: std.ArrayList([2]Span) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ root_left, root_right });
        while (pending.pop()) |pair| {
            if (!try self.refsSpan(pair[0], pair[1], &pending)) return false;
        }
        return true;
    }

    /// Compare one pair of evidence reference spans; the callable contracts
    /// each reference carries are queued.
    fn refsSpan(self: *Comparer, left: Span, right: Span, pending: *std.ArrayList([2]Span)) Allocator.Error!bool {
        if (left.len != right.len) return false;
        const left_refs = self.table.evidence_refs[left.start..][0..left.len];
        const right_refs = self.table.evidence_refs[right.start..][0..right.len];
        for (left_refs, right_refs) |a, b| {
            try pending.append(self.allocator, .{ a.callable_contracts, b.callable_contracts });
            if (a.runtime_dictionary != b.runtime_dictionary or std.meta.activeTag(a.resolution) != std.meta.activeTag(b.resolution)) return false;
            try self.typesPair(a.dispatcher_ty, b.dispatcher_ty);
            switch (a.resolution) {
                .direct => |id| try self.work.append(self.allocator, .{ .kind = .evidence, .left = @backingInt(id), .right = @backingInt(b.resolution.direct) }),
                .constraint => |ref| if (!std.meta.eql(ref, b.resolution.constraint)) return false,
                .structural => |value| {
                    const other = b.resolution.structural;
                    if (!std.meta.eql(value.derivation, other.derivation)) return false;
                    try self.typesPair(value.dispatcher_ty, other.dispatcher_ty);
                    try self.typesPair(value.callable_ty, other.callable_ty);
                    if (!try self.codecs(value.generated_codec_derivation, other.generated_codec_derivation)) return false;
                },
                .from_callable, .from_scheme, .checked_error, .unreachable_value => {},
            }
        }
        return true;
    }

    fn reset(self: *Comparer) void {
        self.work.clearRetainingCapacity();
        self.seen.clearRetainingCapacity();
        self.left_types.clearRetainingCapacity();
        self.right_types.clearRetainingCapacity();
    }

    fn equal(self: *Comparer, left: u32, right: u32) Allocator.Error!bool {
        self.reset();
        try self.work.append(self.allocator, .{ .kind = .codec, .left = left, .right = right });
        return try self.run();
    }

    fn run(self: *Comparer) Allocator.Error!bool {
        while (self.work.pop()) |pair| {
            const visited = try self.seen.getOrPut(self.allocator, pair);
            if (visited.found_existing) continue;
            switch (pair.kind) {
                .codec => {
                    const a = self.table.generated_codec_derivations[pair.left];
                    const b = self.table.generated_codec_derivations[pair.right];
                    if (a.kind != b.kind or a.calls.len != b.calls.len) return false;
                    inline for (type_roles) |field| {
                        try self.typesPair(@field(a, field), @field(b, field));
                    }
                    for (a.callsSlice(self.table), b.callsSlice(self.table)) |ac, bc| {
                        if (ac.method != bc.method or ac.method_role != bc.method_role or std.meta.activeTag(ac.resolution) != std.meta.activeTag(bc.resolution)) return false;
                        try self.typesPair(ac.dispatcher_ty, bc.dispatcher_ty);
                        try self.typesPair(ac.callable_ty, bc.callable_ty);
                        if (!try self.optionalTypes(ac.subject_ty, bc.subject_ty)) return false;
                        switch (ac.resolution) {
                            .pending => unreachable,
                            .checked_error => {},
                            .callable => |id| try self.work.append(self.allocator, .{ .kind = .evidence, .left = @backingInt(id), .right = @backingInt(bc.resolution.callable) }),
                            .structural => |id| if (!try self.codecs(id, bc.resolution.structural)) return false,
                        }
                    }
                },
                .evidence => {
                    const a = self.table.evidence_nodes[pair.left];
                    const b = self.table.evidence_nodes[pair.right];
                    if (!std.meta.eql(a.target, b.target) or std.meta.activeTag(a.instantiation) != std.meta.activeTag(b.instantiation) or std.meta.activeTag(a.nested) != std.meta.activeTag(b.nested) or a.subst.len != b.subst.len) return false;
                    if (!try self.optionalTypes(a.dispatcher_ty, b.dispatcher_ty)) return false;
                    if (!try self.codecs(a.generated_codec_derivation, b.generated_codec_derivation)) return false;
                    switch (a.instantiation) {
                        .monomorphic => {},
                        .callable => |ty| try self.typesPair(ty, b.instantiation.callable),
                        .alias => |ty| try self.typesPair(ty, b.instantiation.alias),
                        .derived_from_callable => |ty| try self.typesPair(ty, b.instantiation.derived_from_callable),
                    }
                    const left_subst = self.table.site_substitutions[a.subst.start..][0..a.subst.len];
                    const right_subst = self.table.site_substitutions[b.subst.start..][0..b.subst.len];
                    for (left_subst, right_subst) |at, bt| try self.typesPair(at, bt);
                    switch (a.nested) {
                        .from_callable => {},
                        .resolved => |span| if (!try self.refs(span, b.nested.resolved)) return false,
                    }
                },
            }
        }
        // One bijection covers all roots, including sharing across nested
        // evidence and source/frozen roles. Individual root equality is weaker.
        return switch (self.types_eql) {
            .exact => self.types.rootsAlphaExactEql(self.allocator, self.left_types.items, self.right_types.items),
            .alias_transparent => self.types.rootsAliasTransparentAlphaEql(self.allocator, self.left_types.items, self.right_types.items),
        };
    }
};

test "codec identity preserves cross-root sharing, method roles, and recursive selections" {
    const gpa = std.testing.allocator;
    var types = checked.CheckedTypeStore{};
    defer types.deinit(gpa);
    var variables: [3]TypeId = undefined;
    for (&variables) |*variable| {
        variable.* = try types.reserveSyntheticTypeRoot(gpa, .{ .bytes = @as([32]u8, @splat(7)) }, true);
        try types.fillSyntheticTypeRoot(gpa, variable.*, .{ .flex = .{} });
    }
    var names = @import("canonical_names.zig").CanonicalNameStore.init(gpa);
    defer names.deinit();
    const method = try names.internMethodName("to_encoder");
    var derivations: [5]dispatch.GeneratedCodecDerivation = undefined;
    var calls: [5]dispatch.GeneratedCodecCall = undefined;
    for (&derivations, &calls, 0..) |*derivation, *call, i| {
        const ty = variables[@min(i, 1)];
        derivation.* = .{
            .identity = @fromBackingInt(@intCast(@as(u32, @intCast(i)))),
            .kind = .encoder,
            .source_constructor_ty = ty,
            .source_runtime_ty = ty,
            .source_shape_ty = ty,
            .source_body_shape_ty = ty,
            .source_encoding_ty = ty,
            .source_state_ty = ty,
            .source_error_ty = ty,
            .constructor_ty = ty,
            .runtime_ty = ty,
            .shape_ty = ty,
            .body_shape_ty = ty,
            .encoding_ty = ty,
            .state_ty = ty,
            .error_ty = ty,
            .calls = .{ .start = @intCast(i), .len = 1 },
        };
        call.* = .{
            .method = method,
            .method_role = 0,
            .dispatcher_ty = ty,
            .callable_ty = ty,
            .resolution = .{ .structural = @fromBackingInt(@intCast(@as(u32, @intCast(i)))) },
        };
    }
    derivations[2].source_shape_ty = variables[2];
    calls[3].method_role = 1;
    calls[4].resolution = .checked_error;
    var table = dispatch.StaticDispatchPlanTable{
        .generated_codec_derivations = &derivations,
        .generated_codec_calls = &calls,
    };
    try intern(gpa, types.view(), &table);
    try std.testing.expectEqual(derivations[0].identity, derivations[1].identity);
    for (derivations[2..], 2..) |derivation, i| try std.testing.expectEqual(@as(CodecId, @fromBackingInt(@intCast(@as(u32, @intCast(i))))), derivation.identity);
}

/// One encoder contract over two fresh variables, after `padding` unrelated
/// roots, so equal contracts sit at different ids in different stores. Every
/// role names the first variable except `shape_ty`, which names the second
/// unless `shared`.
fn digestTestContract(padding: usize, shared: bool) (Allocator.Error || error{NoSpaceLeft})!dispatch.GeneratedCodecContentDigest {
    const gpa = std.testing.allocator;
    var types = checked.CheckedTypeStore{};
    defer types.deinit(gpa);
    var variables: [2]TypeId = undefined;
    for (0..padding + variables.len) |index| {
        const variable = try types.reserveSyntheticTypeRoot(gpa, .{ .bytes = @as([32]u8, @splat(7)) }, true);
        try types.fillSyntheticTypeRoot(gpa, variable, .{ .flex = .{} });
        if (index >= padding) variables[index - padding] = variable;
    }
    var names = canonical.CanonicalNameStore.init(gpa);
    defer names.deinit();
    for (0..padding) |index| {
        var text: [16]u8 = undefined;
        _ = try names.internMethodName(try std.fmt.bufPrint(&text, "padding_{d}", .{index}));
    }
    const ty = variables[0];
    var derivations = [_]dispatch.GeneratedCodecDerivation{.{
        .identity = undefined,
        .kind = .encoder,
        .source_constructor_ty = ty,
        .source_runtime_ty = ty,
        .source_shape_ty = ty,
        .source_body_shape_ty = ty,
        .source_encoding_ty = ty,
        .source_state_ty = ty,
        .source_error_ty = ty,
        .constructor_ty = ty,
        .runtime_ty = ty,
        .shape_ty = if (shared) ty else variables[1],
        .body_shape_ty = ty,
        .encoding_ty = ty,
        .state_ty = ty,
        .error_ty = ty,
        .calls = .{ .start = 0, .len = 1 },
    }};
    var calls = [_]dispatch.GeneratedCodecCall{.{
        .method = try names.internMethodName("to_encoder"),
        .method_role = 0,
        .dispatcher_ty = ty,
        .callable_ty = ty,
        .resolution = .checked_error,
    }};
    var table = dispatch.StaticDispatchPlanTable{
        .generated_codec_derivations = &derivations,
        .generated_codec_calls = &calls,
    };
    try intern(gpa, types.view(), &table);
    try digestContents(gpa, .{}, &types, &names, &table);
    return derivations[0].content;
}

test "codec content digest is independent of the recording module's ids and keeps cross-root sharing" {
    const shared = try digestTestContract(0, true);
    try std.testing.expectEqual(shared, try digestTestContract(5, true));
    const distinct = try digestTestContract(0, false);
    try std.testing.expectEqual(distinct, try digestTestContract(3, false));
    try std.testing.expect(!std.meta.eql(shared, distinct));
}
