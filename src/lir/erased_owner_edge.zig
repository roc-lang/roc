//! The representation-transparent `assign_ref` edge.
//!
//! An `assign_ref` whose operation reads a single local without changing its
//! exact pointer representation makes its target denote the same
//! erased-callable allocation as that local. This is the one definition of
//! that edge: the direct LIR lowerer records it as erased-owner provenance
//! while emitting each `assign_ref`, and Debug LIR certification and LIR
//! transforms (`erased_owner.zig`) verify and re-resolve owned erased calls'
//! reuse sources with it.

const std = @import("std");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

/// The local `op` reads when the read preserves the exact pointer
/// representation of a single erased-callable allocation in `target`.
pub fn transparentSource(store: *const LirStore, layouts: *const layout_mod.Store, op: LIR.RefOp, target: LIR.LocalId) ?LIR.LocalId {
    const source = switch (op.kind) {
        .local => op.source,
        .nominal => op.source,
        inline .tag_payload, .tag_payload_struct => blk: {
            if (op.variant_index != 0) break :blk null;
            const source_layout = layouts.getLayout(store.getLocal(op.source).layout_idx);
            if (source_layout.tag != .tag_union) break :blk null;
            const data = layouts.getTagUnionData(source_layout.getTagUnion().idx);
            if (data.discriminant_size != 0) break :blk null;
            break :blk op.source;
        },
        .discriminant, .field, .list_reinterpret => null,
    } orelse return null;

    const source_size = layouts.layoutSizeAlign(layouts.getLayout(store.getLocal(source).layout_idx)).size;
    const target_size = layouts.layoutSizeAlign(layouts.getLayout(store.getLocal(target).layout_idx)).size;
    return if (source_size == layouts.targetUsize().size() and source_size == target_size) source else null;
}

const testing = std.testing;

const EdgeTest = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    erased_callable: layout_mod.Idx,

    fn init() std.mem.Allocator.Error!EdgeTest {
        var layouts = try layout_mod.Store.init(testing.allocator, .u64);
        errdefer layouts.deinit();
        const erased_callable = try layouts.insertErasedCallable();
        return .{
            .store = LirStore.init(testing.allocator),
            .layouts = layouts,
            .erased_callable = erased_callable,
        };
    }

    fn deinit(self: *EdgeTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *EdgeTest, layout_idx: layout_mod.Idx) std.mem.Allocator.Error!LIR.LocalId {
        return try self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn edge(self: *const EdgeTest, op: LIR.RefOp, target: LIR.LocalId) ?LIR.LocalId {
        return transparentSource(&self.store, &self.layouts, op, target);
    }
};

test "local and nominal reads of a pointer-width source are transparent" {
    var t = try EdgeTest.init();
    defer t.deinit();
    const source = try t.local(t.erased_callable);
    const target = try t.local(t.erased_callable);
    try testing.expectEqual(source, t.edge(LIR.RefOp.local(source), target).?);
    try testing.expectEqual(source, t.edge(LIR.RefOp.nominal(source), target).?);
}

test "transparent edges require pointer width on both sides" {
    var t = try EdgeTest.init();
    defer t.deinit();
    const pointer = try t.local(t.erased_callable);
    const wide = try t.local(.str);
    const narrow = try t.local(.u32);
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.local(wide), try t.local(.str)));
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.local(pointer), narrow));
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.nominal(narrow), pointer));
}

test "zero-discriminant first-variant payload reads are transparent" {
    var t = try EdgeTest.init();
    defer t.deinit();
    const single = try t.layouts.putTagUnion(&.{t.erased_callable});
    const source = try t.local(single);
    const target = try t.local(t.erased_callable);
    try testing.expectEqual(source, t.edge(LIR.RefOp.tagPayload(source, 0, 0, 0), target).?);
    try testing.expectEqual(source, t.edge(LIR.RefOp.tagPayloadStruct(source, 0, 0), target).?);
}

test "payload reads of a discriminated union or a later variant are not transparent" {
    var t = try EdgeTest.init();
    defer t.deinit();
    const unit = try t.layouts.ensureZstLayout();
    const pair = try t.layouts.putTagUnion(&.{ t.erased_callable, unit });
    const discriminated = try t.local(pair);
    const single = try t.local(try t.layouts.putTagUnion(&.{t.erased_callable}));
    const target = try t.local(t.erased_callable);
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.tagPayload(discriminated, 0, 0, 0), target));
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.tagPayloadStruct(single, 1, 1), target));
}

test "discriminant, field, and list reinterpret reads are never transparent" {
    var t = try EdgeTest.init();
    defer t.deinit();
    const source = try t.local(t.erased_callable);
    const target = try t.local(t.erased_callable);
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.discriminant(source), target));
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.field(source, 0), target));
    try testing.expectEqual(@as(?LIR.LocalId, null), t.edge(LIR.RefOp.listReinterpret(source), target));
}

test "the transparent edge has one definition among its LIR consumers" {
    // Names only: the declaration text is assembled at comptime so this test's
    // own source never matches it.
    const decls = [_][]const u8{
        "fn " ++ "transparentSource(",
        "fn " ++ "transparentErasedOwnershipSource(",
        "fn " ++ "sameTransparentPointerRepresentation(",
    };
    const consumers = [_][]const u8{
        @embedFile("erased_owner.zig"),
        @embedFile("arc_certify.zig"),
        @embedFile("scalarize_joins.zig"),
    };
    for (decls) |decl| {
        for (consumers) |source| try testing.expectEqual(@as(usize, 0), std.mem.count(u8, source, decl));
    }
    try testing.expectEqual(@as(usize, 1), std.mem.count(u8, @embedFile("erased_owner_edge.zig"), "pub " ++ decls[0]));
}
