//! Exact identities for generated codec proof graphs. Source contracts remain
//! intact for lowering; only equivalent contracts share specialization identity.

const std = @import("std");
const checked = @import("checked_artifact.zig");
const dispatch = @import("static_dispatch_registry.zig");
const Allocator = std.mem.Allocator;
const TypeId = checked.CheckedTypeId;
const CodecId = dispatch.GeneratedCodecDerivationId;
const Span = @import("artifact_serialize.zig").Span;
const type_roles = .{ "source_constructor_ty", "source_runtime_ty", "source_shape_ty", "source_body_shape_ty", "source_encoding_ty", "source_state_ty", "source_error_ty", "constructor_ty", "runtime_ty", "shape_ty", "body_shape_ty", "encoding_ty", "state_ty", "error_ty" };

/// Intern each completed contract once. Type and call keys select candidates;
/// exact comparison of the whole proof graph is the equality authority.
pub fn intern(allocator: Allocator, types: checked.CheckedTypeStoreView, table: *dispatch.StaticDispatchPlanTable) Allocator.Error!void {
    var buckets = std.AutoHashMap(u64, u32).init(allocator);
    defer buckets.deinit();
    const next = try allocator.alloc(?u32, table.generated_codec_derivations.len);
    defer allocator.free(next);
    var comparer = Comparer{ .allocator = allocator, .types = types, .table = table };
    defer comparer.deinit();
    for (table.generated_codec_derivations, 0..) |*derivation, index| {
        const raw: u32 = @intCast(index);
        var hash = std.hash.Wyhash.init(@intFromEnum(derivation.kind));
        inline for (type_roles) |role| hash.update(&types.rootKey(@field(derivation, role)).bytes);
        hash.update(std.mem.asBytes(&derivation.calls.len));
        for (derivation.callsSlice(table)) |call| {
            std.hash.autoHash(&hash, call.method);
            std.hash.autoHash(&hash, call.method_role);
            std.hash.autoHash(&hash, call.conditional);
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
        derivation.identity = @enumFromInt(candidate orelse raw);
        if (candidate == null) {
            next[raw] = buckets.get(key);
            try buckets.put(key, raw);
        }
    }
}

const Pair = struct { kind: enum { codec, evidence }, left: u32, right: u32 };
const Comparer = struct {
    allocator: Allocator,
    types: checked.CheckedTypeStoreView,
    table: *const dispatch.StaticDispatchPlanTable,
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
        if (left) |id| try self.work.append(self.allocator, .{ .kind = .codec, .left = @intFromEnum(id), .right = @intFromEnum(right.?) });
        return true;
    }

    fn refs(self: *Comparer, left: Span, right: Span) Allocator.Error!bool {
        if (left.len != right.len) return false;
        const left_refs = self.table.evidence_refs[left.start..][0..left.len];
        const right_refs = self.table.evidence_refs[right.start..][0..right.len];
        for (left_refs, right_refs) |a, b| {
            if (a.runtime_dictionary != b.runtime_dictionary or std.meta.activeTag(a.resolution) != std.meta.activeTag(b.resolution)) return false;
            try self.typesPair(a.dispatcher_ty, b.dispatcher_ty);
            switch (a.resolution) {
                .direct => |id| try self.work.append(self.allocator, .{ .kind = .evidence, .left = @intFromEnum(id), .right = @intFromEnum(b.resolution.direct) }),
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

    fn equal(self: *Comparer, left: u32, right: u32) Allocator.Error!bool {
        self.work.clearRetainingCapacity();
        self.seen.clearRetainingCapacity();
        self.left_types.clearRetainingCapacity();
        self.right_types.clearRetainingCapacity();
        try self.work.append(self.allocator, .{ .kind = .codec, .left = left, .right = right });
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
                        if (ac.method != bc.method or ac.method_role != bc.method_role or ac.conditional != bc.conditional or std.meta.activeTag(ac.resolution) != std.meta.activeTag(bc.resolution)) return false;
                        try self.typesPair(ac.dispatcher_ty, bc.dispatcher_ty);
                        try self.typesPair(ac.callable_ty, bc.callable_ty);
                        if (!try self.optionalTypes(ac.subject_ty, bc.subject_ty)) return false;
                        switch (ac.resolution) {
                            .pending => unreachable,
                            .checked_error => {},
                            .callable => |id| try self.work.append(self.allocator, .{ .kind = .evidence, .left = @intFromEnum(id), .right = @intFromEnum(bc.resolution.callable) }),
                            .structural => |id| _ = try self.codecs(id, bc.resolution.structural),
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
        return self.types.rootsAlphaExactEql(self.allocator, self.left_types.items, self.right_types.items);
    }
};

test "codec identity preserves cross-root sharing, conditional calls, and recursive selections" {
    const gpa = std.testing.allocator;
    var types = checked.CheckedTypeStore{};
    defer types.deinit(gpa);
    var variables: [3]TypeId = undefined;
    for (&variables) |*variable| {
        variable.* = try types.reserveSyntheticTypeRoot(gpa, .{ .bytes = [_]u8{7} ** 32 }, true);
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
            .identity = @enumFromInt(@as(u32, @intCast(i))),
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
            .resolution = .{ .structural = @enumFromInt(@as(u32, @intCast(i))) },
        };
    }
    derivations[2].source_shape_ty = variables[2];
    calls[3].conditional = true;
    calls[4].resolution = .checked_error;
    var table = dispatch.StaticDispatchPlanTable{
        .generated_codec_derivations = &derivations,
        .generated_codec_calls = &calls,
    };
    try intern(gpa, types.view(), &table);
    try std.testing.expectEqual(derivations[0].identity, derivations[1].identity);
    for (derivations[2..], 2..) |derivation, i| try std.testing.expectEqual(@as(CodecId, @enumFromInt(@as(u32, @intCast(i)))), derivation.identity);
}
