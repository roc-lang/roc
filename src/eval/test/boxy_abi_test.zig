//! Unit tests for the C-ABI boxy runtime wrappers.
//!
//! Each test builds descriptor tables and layouts directly (the same data the
//! boxy lowerer serializes for a program), initializes the process-global
//! runtime over them with a tracking `RocOps`, exercises the wrappers on raw
//! value bytes, and checks refcount balance through the allocation tracker.

const std = @import("std");
const backend = @import("backend");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const builtins = @import("builtins");
const eval = @import("eval");
const boxy_abi = eval.boxy_abi;
const boxy_runtime = eval.boxy_runtime;
const RuntimeHostEnv = eval.RuntimeHostEnv;

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LirProgram = lir.Program;
const BoxyTypeDesc = LirProgram.BoxyTypeDesc;

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

const TestSetup = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    env: RuntimeHostEnv,

    fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!TestSetup {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(allocator, base.target.TargetUsize.native),
            .env = RuntimeHostEnv.init(allocator),
        };
    }

    fn startRuntime(self: *TestSetup, allocator: std.mem.Allocator, tables: boxy_runtime.BoxyTables) error{ OutOfMemory, AlreadyInitialized }!void {
        try boxy_abi.initGlobal(allocator, &self.store, &self.layouts, tables, self.env.get_ops());
    }

    fn deinit(self: *TestSetup) void {
        boxy_abi.deinitGlobal();
        self.env.deinit();
        self.layouts.deinit();
        self.store.deinit();
    }
};

fn customInspectProc(
    ops: *builtins.host_abi.RocOps,
    test_context: ?*anyopaque,
    _: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const context_observed: *bool = @ptrCast(@alignCast(test_context.?));
    context_observed.* = true;
    const rendered = builtins.str.RocStr.fromSlice("custom inspect result stored outside the small-string representation", ops);
    const out: *align(1) builtins.str.RocStr = @ptrCast(ret.?);
    out.* = rendered;
    ret_desc.* = null;
}

var expectedInspectArgDesc: ?*const BoxyTypeDesc = null;

fn customInspectChecksArgDesc(
    ops: *builtins.host_abi.RocOps,
    _: ?*anyopaque,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const raw_desc: *align(1) const usize = @ptrCast(args[1].?);
    const text = if (raw_desc.* == @intFromPtr(expectedInspectArgDesc.?)) "source descriptor" else "wrong descriptor";
    const out: *align(1) builtins.str.RocStr = @ptrCast(ret.?);
    out.* = builtins.str.RocStr.fromSlice(text, ops);
    ret_desc.* = null;
}

var reentrantInspectSourceDescs: [2]?*const BoxyTypeDesc = .{ null, null };
var reentrantInspectTargetDescs: [2]?*const BoxyTypeDesc = .{ null, null };

fn customInspectSpecializesDescriptor(
    ops: *builtins.host_abi.RocOps,
    _: ?*anyopaque,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const inspected: *align(1) const u64 = @ptrCast(args[0].?);
    const index: usize = if (inspected.* == 1) 0 else 1;
    const source_desc = reentrantInspectSourceDescs[index].?;
    const target_desc = reentrantInspectTargetDescs[index].?;

    var source: u64 = 42;
    var adapted: u64 = 0;
    var adapted_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(
        @ptrCast(&adapted),
        &adapted_desc,
        @ptrCast(&source),
        source_desc,
        target_desc,
        0,
        @intFromEnum(LIR.BoxyTransferMode.copy),
    );

    const text = if (adapted == source and adapted_desc == target_desc)
        "persistent descriptor"
    else
        "wrong descriptor";
    const out: *align(1) builtins.str.RocStr = @ptrCast(ret.?);
    out.* = builtins.str.RocStr.fromSlice(text, ops);
    ret_desc.* = null;
}

test "boxy abi structural equality compares scalars through a descriptor" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    var a: u64 = 42;
    var b: u64 = 42;
    var c: u64 = 7;
    try std.testing.expect(boxy_abi.roc_boxy_eq(@ptrCast(&a), @ptrCast(&b), @intFromEnum(layout_mod.Idx.u64), &descs[0]));
    try std.testing.expect(!boxy_abi.roc_boxy_eq(@ptrCast(&a), @ptrCast(&c), @intFromEnum(layout_mod.Idx.u64), &descs[0]));
}

test "boxy abi static descriptor lookup resolves ids to the descriptor table" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .str, .contains_refcounted = true },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    try std.testing.expectEqual(&descs[0], boxy_abi.roc_boxy_static_desc(0));
    try std.testing.expectEqual(&descs[1], boxy_abi.roc_boxy_static_desc(1));
}

test "boxy abi Box payload descriptor projection accepts both descriptor conventions" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(fixtureTableIndex(0)) }};
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{
            .payload_layout = box_layout,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 0, .len = 1 },
        },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
    });

    try std.testing.expectEqual(
        &descs[0],
        boxy_abi.roc_boxy_box_payload_desc(&descs[0], @intFromEnum(box_layout)),
    );
    try std.testing.expectEqual(
        &descs[0],
        boxy_abi.roc_boxy_box_payload_desc(&descs[1], @intFromEnum(box_layout)),
    );
}

test "boxy abi inspect renders a scalar through its descriptor" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    var value: u64 = 42;
    var rendered: builtins.str.RocStr = undefined;
    boxy_abi.roc_boxy_inspect(
        @ptrCast(&rendered),
        null,
        @ptrCast(&value),
        @intFromEnum(layout_mod.Idx.u64),
        &descs[0],
    );
    try std.testing.expectEqualStrings("42", rendered.asSlice());
    rendered.decref(setup.env.get_ops());
    try setup.env.checkForLeaks();
}

test "boxy abi inspect dispatches descriptor method and releases its owned result" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{.{
        .payload_layout = .u64,
        .contains_refcounted = false,
        .inspect_method = @enumFromInt(fixtureTableIndex(0)),
    }};
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(fixtureTableIndex(0)) }};
    const method_slots = [_]LirProgram.BoxyMethodSlot{.{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(fixtureTableIndex(0)),
        .adapter = .{
            .arg_layouts = .{ .start = 0, .len = 1 },
            .arg_descs = .{ .start = 0, .len = 1 },
        },
    }};
    const method_arg_layouts = [_]layout_mod.Idx{.u64};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .method_slots = &method_slots,
        .method_arg_layouts = &method_arg_layouts,
    });
    boxy_abi.roc_boxy_register_proc(0, &customInspectProc, @intFromEnum(layout_mod.Idx.str), 1, false, 0);

    var value: u64 = 42;
    var rendered: builtins.str.RocStr = undefined;
    var context_observed = false;
    boxy_abi.roc_boxy_inspect(
        @ptrCast(&rendered),
        @ptrCast(&context_observed),
        @ptrCast(&value),
        @intFromEnum(layout_mod.Idx.u64),
        &descs[0],
    );
    try std.testing.expect(context_observed);
    try std.testing.expectEqualStrings("custom inspect result stored outside the small-string representation", rendered.asSlice());
    rendered.decref(setup.env.get_ops());
    try setup.env.checkForLeaks();
}

test "boxy abi reentrant inspect specialization keeps descriptors outside per-call scratch" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false, .inspect_method = @enumFromInt(fixtureTableIndex(0)) },
        .{ .payload_layout = .u64, .contains_refcounted = false, .inspect_method = @enumFromInt(fixtureTableIndex(0)) },
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(fixtureTableIndex(0)) }};
    const method_slots = [_]LirProgram.BoxyMethodSlot{.{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(fixtureTableIndex(0)),
        .adapter = .{
            .arg_layouts = .{ .start = 0, .len = 1 },
            .arg_descs = .{ .start = 0, .len = 1 },
        },
    }};
    const method_arg_layouts = [_]layout_mod.Idx{.u64};
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .source_layout = .u64,
        .target_layout = .u64,
        .consumes_source = false,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .adapters = &adapters,
        .method_slots = &method_slots,
        .method_arg_layouts = &method_arg_layouts,
    });
    reentrantInspectSourceDescs = .{ &descs[2], &descs[3] };
    reentrantInspectTargetDescs = .{ &descs[4], &descs[5] };
    defer {
        reentrantInspectSourceDescs = .{ null, null };
        reentrantInspectTargetDescs = .{ null, null };
    }
    boxy_abi.roc_boxy_register_proc(0, &customInspectSpecializesDescriptor, @intFromEnum(layout_mod.Idx.str), 1, false, 0);

    var values = [_]u64{ 1, 2 };
    for (&values, 0..) |*value, index| {
        var rendered: builtins.str.RocStr = undefined;
        boxy_abi.roc_boxy_inspect(
            @ptrCast(&rendered),
            null,
            @ptrCast(value),
            @intFromEnum(layout_mod.Idx.u64),
            &descs[index],
        );
        try std.testing.expectEqualStrings("persistent descriptor", rendered.asSlice());
        rendered.decref(setup.env.get_ops());
    }
    try setup.env.checkForLeaks();
}

test "boxy abi custom inspect preserves a full descriptor across a payload-shaped borrowed boundary" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const aggregate_layout = try setup.layouts.putStructFields(&.{
        .{ .index = 0, .layout = .u64 },
        .{ .index = 1, .layout = .u64 },
    });
    const descs = [_]BoxyTypeDesc{
        .{
            .payload_layout = aggregate_layout,
            .contains_refcounted = false,
            .inspect_method = @enumFromInt(fixtureTableIndex(0)),
        },
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(1)) },
        .{ .static = @enumFromInt(fixtureTableIndex(1)) },
    };
    const hidden_sources = [_]LirProgram.BoxyMethodHiddenDescSource{
        .{ .argument = 0 },
    };
    const method_slots = [_]LirProgram.BoxyMethodSlot{.{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(5),
        .hidden_descs = .{ .start = 1, .len = 1 },
        .adapter = .{
            .arg_layouts = .{ .start = 0, .len = 1 },
            .arg_descs = .{ .start = 0, .len = 1 },
            .hidden_desc_sources = .{ .start = 0, .len = 1 },
        },
    }};
    const method_arg_layouts = [_]layout_mod.Idx{aggregate_layout};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .method_slots = &method_slots,
        .method_arg_layouts = &method_arg_layouts,
        .method_hidden_desc_sources = &hidden_sources,
    });
    expectedInspectArgDesc = &descs[0];
    defer expectedInspectArgDesc = null;
    boxy_abi.roc_boxy_register_proc(5, &customInspectChecksArgDesc, @intFromEnum(layout_mod.Idx.str), 1, false, 0);

    var value = [_]u64{ 1, 2 };
    var rendered: builtins.str.RocStr = undefined;
    boxy_abi.roc_boxy_inspect(
        @ptrCast(&rendered),
        null,
        @ptrCast(&value),
        @intFromEnum(aggregate_layout),
        &descs[0],
    );
    try std.testing.expectEqualStrings("source descriptor", rendered.asSlice());
    rendered.decref(setup.env.get_ops());

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    var boxed: usize = 0;
    var boxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&boxed),
        &boxed_desc,
        @ptrCast(&value),
        @intFromEnum(aggregate_layout),
        &descs[0],
        &descs[0],
        1, // copy
        @intFromEnum(box_layout),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[0]), boxed_desc);

    boxy_abi.roc_boxy_inspect(
        @ptrCast(&rendered),
        null,
        @ptrCast(&boxed),
        @intFromEnum(box_layout),
        boxed_desc.?,
    );
    try std.testing.expectEqualStrings("source descriptor", rendered.asSlice());
    rendered.decref(setup.env.get_ops());
    boxy_abi.roc_boxy_drop(@ptrCast(&boxed), @intFromEnum(box_layout), boxed_desc.?, 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi box and unbox round-trip a string payload with balanced refcounts" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.erasedBox()));
    const str_layout = @intFromEnum(layout_mod.Idx.str);

    // A heap string, so payload refcounts are observable.
    var source_str = builtins.str.RocStr.fromSlice("a heap string long enough to escape small-string storage", setup.env.get_ops());
    var boxed: usize = 0;
    var boxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&boxed),
        &boxed_desc,
        @ptrCast(&source_str),
        str_layout,
        null,
        &descs[0],
        2, // move
        box_layout,
    );
    try std.testing.expect(boxed != 0);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[0]), boxed_desc);

    var unboxed: builtins.str.RocStr = undefined;
    var unboxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(
        @ptrCast(&unboxed),
        &unboxed_desc,
        @ptrCast(&boxed),
        box_layout,
        boxed_desc.?,
        null,
        str_layout,
        0, // borrow
    );
    try std.testing.expectEqualStrings(source_str.asSlice(), unboxed.asSlice());

    // The unboxed copy owns one reference; release it, then release the box
    // (which drops the payload it still holds and frees the allocation).
    boxy_abi.roc_boxy_drop(@ptrCast(&unboxed), str_layout, null, 1, 1, 0);
    boxy_abi.roc_boxy_drop(@ptrCast(&boxed), box_layout, &descs[0], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi list materialization preserves reserved capacity" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(1) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{
            .payload_layout = list_layout,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 0, .len = 1 },
        },
        .{
            .payload_layout = list_layout,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 1, .len = 1 },
        },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
    });

    var source = builtins.list.listWithCapacity(
        1,
        8,
        @sizeOf(u64),
        false,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    try std.testing.expectEqual(@as(usize, 0), source.len());
    const reserved_capacity = source.getCapacity();
    try std.testing.expect(reserved_capacity > 0);

    var materialized: builtins.list.RocList = undefined;
    var materialized_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_materialize_call_result(
        @ptrCast(&materialized),
        &materialized_desc,
        @ptrCast(&source),
        @intFromEnum(list_layout),
        &descs[2],
        &descs[3],
        @intFromEnum(list_layout),
    );
    try std.testing.expectEqual(@as(usize, 0), materialized.len());
    try std.testing.expectEqual(reserved_capacity, materialized.getCapacity());
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[3]), materialized_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&materialized), @intFromEnum(list_layout), &descs[3], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi call result completes an erased source list descriptor from the concrete target" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const source_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(box_layout));
    const target_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(1) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = box_layout, .contains_refcounted = true },
        .{
            .payload_layout = source_list_layout,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 0, .len = 1 },
        },
        .{ .payload_layout = target_list_layout, .contains_refcounted = true },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
    });

    var first_payload: u64 = 97;
    var first_box: usize = 0;
    var first_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&first_box),
        &first_desc,
        @ptrCast(&first_payload),
        @intFromEnum(layout_mod.Idx.u64),
        null,
        &descs[0],
        2,
        @intFromEnum(box_layout),
    );
    var second_payload: u64 = 1234;
    var second_box: usize = 0;
    var second_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&second_box),
        &second_desc,
        @ptrCast(&second_payload),
        @intFromEnum(layout_mod.Idx.u64),
        null,
        &descs[0],
        2,
        @intFromEnum(box_layout),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[0]), first_desc);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[0]), second_desc);

    var source = builtins.list.listWithCapacity(
        2,
        @alignOf(usize),
        @sizeOf(usize),
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source.length = 2;
    const source_boxes: [*]usize = @ptrCast(@alignCast(source.bytes.?));
    source_boxes[0] = first_box;
    source_boxes[1] = second_box;

    var materialized: builtins.list.RocList = undefined;
    var materialized_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_materialize_call_result(
        @ptrCast(&materialized),
        &materialized_desc,
        @ptrCast(&source),
        @intFromEnum(source_list_layout),
        &descs[2],
        &descs[3],
        @intFromEnum(target_list_layout),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[3]), materialized_desc);
    try std.testing.expectEqualSlices(u64, &.{ 97, 1234 }, materialized.elements(u64).?[0..materialized.len()]);

    boxy_abi.roc_boxy_drop(
        @ptrCast(&materialized),
        @intFromEnum(target_list_layout),
        &descs[3],
        1,
        1,
        0,
    );
    try setup.env.checkForLeaks();
}

test "boxy abi call result transfers nested tag list ownership" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const branch_name = try setup.store.insertString("Branch");
    const list_str_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.str));
    const node_layout = try setup.layouts.putTagUnion(&.{list_str_layout});
    const list_node_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(node_layout));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(1) },
        .{ .static = @enumFromInt(2) },
        .{ .static = @enumFromInt(3) },
        .{ .static = @enumFromInt(4) },
        .{ .static = @enumFromInt(5) },
    };
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{
        .{ .payload_index = 0, .desc = .{ .static = @enumFromInt(2) } },
        .{ .payload_index = 0, .desc = .{ .static = @enumFromInt(3) } },
    };
    const variants = [_]LirProgram.BoxyTagVariant{
        .{
            .name = branch_name,
            .discriminant = 0,
            .payload_layout = list_str_layout,
            .payload_count = 1,
            .payload_descs = .{ .start = 0, .len = 1 },
        },
        .{
            .name = branch_name,
            .discriminant = 0,
            .payload_layout = list_str_layout,
            .payload_count = 1,
            .payload_descs = .{ .start = 1, .len = 1 },
        },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
        .{ .payload_layout = .str, .contains_refcounted = true },
        .{ .payload_layout = list_str_layout, .contains_refcounted = true, .nested_descs = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = list_str_layout, .contains_refcounted = true, .nested_descs = .{ .start = 1, .len = 1 } },
        .{ .payload_layout = node_layout, .contains_refcounted = true, .tag_variants = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = node_layout, .contains_refcounted = true, .tag_variants = .{ .start = 1, .len = 1 } },
        .{ .payload_layout = list_node_layout, .contains_refcounted = true, .nested_descs = .{ .start = 4, .len = 1 } },
        .{ .payload_layout = list_node_layout, .contains_refcounted = true, .nested_descs = .{ .start = 5, .len = 1 } },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var source_children = builtins.list.listWithCapacity(
        1,
        @alignOf(builtins.str.RocStr),
        @sizeOf(builtins.str.RocStr),
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source_children.length = 1;
    const child_strings: [*]builtins.str.RocStr = @ptrCast(@alignCast(source_children.bytes.?));
    child_strings[0] = builtins.str.RocStr.fromSlice(
        "a heap string nested below a tag and two list ownership boundaries",
        setup.env.get_ops(),
    );

    const node_sa = setup.layouts.layoutSizeAlign(setup.layouts.getLayout(node_layout));
    var source_node: [128]u8 align(16) = @splat(0);
    @memcpy(source_node[0..@sizeOf(builtins.list.RocList)], std.mem.asBytes(&source_children));
    const node_info = setup.layouts.getTagUnionInfo(setup.layouts.getLayout(node_layout));
    node_info.data.writeDiscriminant(&source_node, 0, setup.layouts.targetUsize());

    var source = builtins.list.listWithCapacity(
        1,
        @intCast(node_sa.alignment.toByteUnits()),
        node_sa.size,
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source.length = 1;
    @memcpy(source.bytes.?[0..node_sa.size], source_node[0..node_sa.size]);

    var materialized: builtins.list.RocList = undefined;
    var materialized_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_materialize_call_result(
        @ptrCast(&materialized),
        &materialized_desc,
        @ptrCast(&source),
        @intFromEnum(list_node_layout),
        &descs[6],
        &descs[7],
        @intFromEnum(list_node_layout),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[7]), materialized_desc);
    try std.testing.expectEqual(@as(usize, 1), materialized.len());

    boxy_abi.roc_boxy_drop(@ptrCast(&materialized), @intFromEnum(list_node_layout), &descs[7], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi relabel adapter transfers a list allocation unchanged" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(1) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = list_layout, .contains_refcounted = true, .nested_descs = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = list_layout, .contains_refcounted = true, .nested_descs = .{ .start = 1, .len = 1 } },
    };
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .operation = .relabel,
        .source_layout = list_layout,
        .target_layout = list_layout,
        .consumes_source = true,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .adapters = &adapters,
    });

    var source = builtins.list.listWithCapacity(1, 8, @sizeOf(u64), false, null, &builtins.utils.rcNone, setup.env.get_ops());
    source.length = 1;
    const source_elems: [*]u64 = @ptrCast(@alignCast(source.bytes.?));
    source_elems[0] = 42;
    const source_allocation = source.getAllocationDataPtr(setup.env.get_ops());
    const source_capacity = source.getCapacity();

    var adapted: builtins.list.RocList = undefined;
    var adapted_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(@ptrCast(&adapted), &adapted_desc, @ptrCast(&source), &descs[2], &descs[3], 0, 2);
    try std.testing.expectEqual(source_allocation, adapted.getAllocationDataPtr(setup.env.get_ops()));
    try std.testing.expectEqual(source_capacity, adapted.getCapacity());
    try std.testing.expectEqual(@as(u64, 42), adapted.elements(u64).?[0]);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[3]), adapted_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&adapted), @intFromEnum(list_layout), &descs[3], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi move adapter transfers unique boxed list elements" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const source_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(box_layout));
    const target_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = source_list_layout, .contains_refcounted = true, .nested_descs = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = target_list_layout, .contains_refcounted = true, .nested_descs = .{ .start = 1, .len = 1 } },
    };
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .operation = .materialize,
        .source_layout = source_list_layout,
        .target_layout = target_list_layout,
        .consumes_source = true,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .adapters = &adapters,
    });

    var payload: u64 = 99;
    var boxed: usize = 0;
    var boxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&boxed),
        &boxed_desc,
        @ptrCast(&payload),
        @intFromEnum(layout_mod.Idx.u64),
        null,
        &descs[0],
        2,
        @intFromEnum(box_layout),
    );

    var source = builtins.list.listWithCapacity(
        @alignOf(usize),
        8,
        @sizeOf(usize),
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source.length = 1;
    const source_elems: [*]usize = @ptrCast(@alignCast(source.bytes.?));
    source_elems[0] = boxed;
    const source_capacity = source.getCapacity();

    var adapted: builtins.list.RocList = undefined;
    var adapted_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(@ptrCast(&adapted), &adapted_desc, @ptrCast(&source), &descs[1], &descs[2], 0, 2);
    try std.testing.expectEqual(@as(usize, 1), adapted.len());
    try std.testing.expectEqual(source_capacity, adapted.getCapacity());
    try std.testing.expectEqual(@as(u64, 99), adapted.elements(u64).?[0]);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[2]), adapted_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&adapted), @intFromEnum(target_list_layout), &descs[2], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi move adapter releases tag payloads across differing discriminants" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const name_a = try setup.store.insertString("A");
    const name_b = try setup.store.insertString("B");
    const name_c = try setup.store.insertString("C");
    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const source_union_layout = try setup.layouts.putTagUnion(&.{ .u64, box_layout });
    const target_union_layout = try setup.layouts.putTagUnion(&.{ .u64, .u64 });
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{.{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    }};
    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = name_a, .discriminant = 0, .payload_layout = .u64, .payload_count = 1 },
        .{ .name = name_b, .discriminant = 1, .payload_layout = box_layout, .payload_count = 1, .payload_descs = .{ .start = 0, .len = 1 } },
        .{ .name = name_b, .discriminant = 0, .payload_layout = .u64, .payload_count = 1 },
        .{ .name = name_c, .discriminant = 1, .payload_layout = .u64, .payload_count = 1 },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{
            .payload_layout = source_union_layout,
            .contains_refcounted = true,
            .tag_variants = .{ .start = 0, .len = 2 },
        },
        .{
            .payload_layout = target_union_layout,
            .contains_refcounted = false,
            .tag_variants = .{ .start = 2, .len = 2 },
        },
    };
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .operation = .materialize,
        .source_layout = source_union_layout,
        .target_layout = target_union_layout,
        .consumes_source = true,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .adapters = &adapters,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var payload: u64 = 99;
    var boxed: usize = 0;
    var boxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&boxed),
        &boxed_desc,
        @ptrCast(&payload),
        @intFromEnum(layout_mod.Idx.u64),
        null,
        &descs[0],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(box_layout),
    );

    var source: [64]u8 align(16) = @splat(0);
    @memcpy(source[0..@sizeOf(usize)], std.mem.asBytes(&boxed));
    const source_info = setup.layouts.getTagUnionInfo(setup.layouts.getLayout(source_union_layout));
    source_info.data.writeDiscriminant(&source, 1, setup.layouts.targetUsize());
    try std.testing.expect(boxy_abi.roc_boxy_tag_match(&source, @intFromEnum(source_union_layout), &descs[1], @intFromEnum(name_b)));

    var target: [64]u8 align(16) = @splat(0);
    var target_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(
        &target,
        &target_desc,
        &source,
        &descs[1],
        &descs[2],
        0,
        @intFromEnum(LIR.BoxyTransferMode.move),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[2]), target_desc);
    try std.testing.expect(boxy_abi.roc_boxy_tag_match(&target, @intFromEnum(target_union_layout), &descs[2], @intFromEnum(name_b)));

    var adapted_payload: u64 = 0;
    var adapted_payload_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_tag_payload(
        @ptrCast(&adapted_payload),
        &adapted_payload_desc,
        &target,
        @intFromEnum(target_union_layout),
        &descs[2],
        @intFromEnum(name_b),
        0,
        @intFromEnum(layout_mod.Idx.u64),
        @intFromEnum(LIR.BoxyTransferMode.borrow),
    );
    try std.testing.expectEqual(@as(u64, 99), adapted_payload);

    boxy_abi.roc_boxy_drop(&target, @intFromEnum(target_union_layout), &descs[2], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi move adapter transfers a dynamic box into a target tag extension" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const name_ok = try setup.store.insertString("Ok");
    const name_err = try setup.store.insertString("Err");
    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const source_union_layout = try setup.layouts.putTagUnion(&.{.str});
    const target_union_layout = try setup.layouts.putTagUnion(&.{ .zst, box_layout });
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{.{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    }};
    const variants = [_]LirProgram.BoxyTagVariant{
        .{
            .name = name_ok,
            .discriminant = 0,
            .payload_layout = .str,
            .payload_count = 1,
            .payload_descs = .{ .start = 0, .len = 1 },
        },
        .{ .name = name_err, .discriminant = 0, .payload_layout = .zst, .payload_count = 0 },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
        .{
            .payload_layout = source_union_layout,
            .contains_refcounted = true,
            .tag_variants = .{ .start = 0, .len = 1 },
        },
        .{
            .payload_layout = target_union_layout,
            .contains_refcounted = true,
            .tag_variants = .{ .start = 1, .len = 1 },
            .tag_ext_desc = .{ .static = @enumFromInt(1) },
        },
    };
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .operation = .materialize,
        .source_layout = box_layout,
        .target_layout = target_union_layout,
        .consumes_source = true,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .adapters = &adapters,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var source: [64]u8 align(16) = @splat(0);
    const source_str = builtins.str.RocStr.fromSlice(
        "a heap string long enough to exercise row-extension ownership",
        setup.env.get_ops(),
    );
    @memcpy(source[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&source_str));
    const source_info = setup.layouts.getTagUnionInfo(setup.layouts.getLayout(source_union_layout));
    source_info.data.writeDiscriminant(&source, 0, setup.layouts.targetUsize());

    var boxed: usize = 0;
    var boxed_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&boxed),
        &boxed_desc,
        &source,
        @intFromEnum(source_union_layout),
        &descs[1],
        &descs[1],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(box_layout),
    );

    var target: [64]u8 align(16) = @splat(0);
    var target_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(
        &target,
        &target_desc,
        @ptrCast(&boxed),
        boxed_desc,
        &descs[2],
        0,
        @intFromEnum(LIR.BoxyTransferMode.move),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[2]), target_desc);
    try std.testing.expect(boxy_abi.roc_boxy_tag_match(
        &target,
        @intFromEnum(target_union_layout),
        &descs[2],
        @intFromEnum(name_ok),
    ));

    boxy_abi.roc_boxy_drop(&target, @intFromEnum(target_union_layout), &descs[2], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi moved reboxed payload transfers a nested list allocation" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const list_str_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.str));
    const struct_box_layout = try setup.layouts.putStructFields(&.{.{ .index = 0, .layout = box_layout }});
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .{ .static = @enumFromInt(1) },
        .{ .static = @enumFromInt(2) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
        .{ .payload_layout = list_str_layout, .contains_refcounted = true, .nested_descs = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = list_str_layout, .contains_refcounted = true, .nested_descs = .{ .start = 1, .len = 1 } },
        .{ .payload_layout = struct_box_layout, .contains_refcounted = true, .nested_descs = .{ .start = 2, .len = 1 } },
        .{ .payload_layout = struct_box_layout, .contains_refcounted = true, .nested_descs = .{ .start = 3, .len = 1 } },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
    });

    var source_list = builtins.list.listWithCapacity(
        1,
        @alignOf(builtins.str.RocStr),
        @sizeOf(builtins.str.RocStr),
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source_list.length = 1;
    const source_strings: [*]builtins.str.RocStr = @ptrCast(@alignCast(source_list.bytes.?));
    source_strings[0] = builtins.str.RocStr.fromSlice(
        "a heap string nested in a moved and reboxed list payload",
        setup.env.get_ops(),
    );

    var inner_box: usize = 0;
    var inner_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&inner_box),
        &inner_desc,
        @ptrCast(&source_list),
        @intFromEnum(list_str_layout),
        &descs[1],
        &descs[1],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(box_layout),
    );

    var source_struct: [@sizeOf(usize)]u8 align(@alignOf(usize)) = undefined;
    @memcpy(&source_struct, std.mem.asBytes(&inner_box));
    var outer_box: usize = 0;
    var outer_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&outer_box),
        &outer_desc,
        &source_struct,
        @intFromEnum(struct_box_layout),
        &descs[3],
        &descs[3],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(box_layout),
    );

    var target_struct: [@sizeOf(usize)]u8 align(@alignOf(usize)) = undefined;
    var target_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(
        &target_struct,
        &target_desc,
        @ptrCast(&outer_box),
        @intFromEnum(box_layout),
        outer_desc.?,
        &descs[4],
        @intFromEnum(struct_box_layout),
        @intFromEnum(LIR.BoxyTransferMode.move),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[4]), target_desc);

    const target_inner_box = std.mem.bytesToValue(usize, &target_struct);
    const target_list: *const builtins.list.RocList = @ptrFromInt(target_inner_box);
    try std.testing.expectEqual(@as(usize, 1), target_list.len());
    const target_strings: [*]const builtins.str.RocStr = @ptrCast(@alignCast(target_list.bytes.?));
    try std.testing.expectEqualStrings(
        "a heap string nested in a moved and reboxed list payload",
        target_strings[0].asSlice(),
    );

    boxy_abi.roc_boxy_drop(&target_struct, @intFromEnum(struct_box_layout), &descs[4], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi copied recursive tag retains boxed children" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const leaf_name = try setup.store.insertString("Leaf");
    const node_name = try setup.store.insertString("Node");
    const erased_box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const node_layout = try setup.layouts.putStructFields(&.{
        .{ .index = 0, .layout = erased_box_layout },
        .{ .index = 1, .layout = erased_box_layout },
    });
    const tree_layout = try setup.layouts.putTagUnion(&.{ .i64, node_layout });
    const concrete_box_layout = try setup.layouts.insertLayout(layout_mod.Layout.box(tree_layout));
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{
        .{ .payload_index = 0, .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) } },
        .{ .payload_index = 1, .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) } },
    };
    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = leaf_name, .discriminant = 0, .payload_layout = .i64, .payload_count = 1 },
        .{
            .name = node_name,
            .discriminant = 1,
            .payload_layout = node_layout,
            .payload_count = 2,
            .payload_descs = .{ .start = 0, .len = 2 },
        },
    };
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(fixtureTableIndex(0)) }};
    const descs = [_]BoxyTypeDesc{
        .{
            .payload_layout = tree_layout,
            .contains_refcounted = true,
            .tag_variants = .{ .start = 0, .len = 2 },
        },
        .{
            .payload_layout = concrete_box_layout,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 0, .len = 1 },
        },
    };
    const adapters = [_]LirProgram.BoxyAdapter{.{
        .kind = .boxy_to_boxy,
        .operation = .materialize,
        .source_layout = concrete_box_layout,
        .target_layout = erased_box_layout,
        .consumes_source = true,
        .produces_owned_result = true,
    }};
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .adapters = &adapters,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var left_leaf: [64]u8 align(16) = @splat(0);
    var right_leaf: [64]u8 align(16) = @splat(0);
    std.mem.writeInt(i64, left_leaf[0..@sizeOf(i64)], 7, .little);
    std.mem.writeInt(i64, right_leaf[0..@sizeOf(i64)], 11, .little);
    const tree_info = setup.layouts.getTagUnionInfo(setup.layouts.getLayout(tree_layout));
    tree_info.data.writeDiscriminant(&left_leaf, 0, setup.layouts.targetUsize());
    tree_info.data.writeDiscriminant(&right_leaf, 0, setup.layouts.targetUsize());

    var left_box: usize = 0;
    var right_box: usize = 0;
    var left_desc: ?*const BoxyTypeDesc = null;
    var right_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&left_box),
        &left_desc,
        &left_leaf,
        @intFromEnum(tree_layout),
        &descs[0],
        &descs[0],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(erased_box_layout),
    );
    boxy_abi.roc_boxy_box(
        @ptrCast(&right_box),
        &right_desc,
        &right_leaf,
        @intFromEnum(tree_layout),
        &descs[0],
        &descs[0],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(erased_box_layout),
    );

    var root: [64]u8 align(16) = @splat(0);
    const node_struct = setup.layouts.getLayout(node_layout).getStruct().idx;
    const left_offset = setup.layouts.getStructFieldOffsetByOriginalIndex(node_struct, 0);
    const right_offset = setup.layouts.getStructFieldOffsetByOriginalIndex(node_struct, 1);
    @memcpy(root[left_offset..][0..@sizeOf(usize)], std.mem.asBytes(&left_box));
    @memcpy(root[right_offset..][0..@sizeOf(usize)], std.mem.asBytes(&right_box));
    tree_info.data.writeDiscriminant(&root, 1, setup.layouts.targetUsize());

    var source_box: usize = 0;
    var source_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&source_box),
        &source_desc,
        &root,
        @intFromEnum(tree_layout),
        &descs[0],
        &descs[1],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(concrete_box_layout),
    );

    var copied_box: usize = 0;
    var copied_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_adapt(
        @ptrCast(&copied_box),
        &copied_desc,
        @ptrCast(&source_box),
        source_desc.?,
        &descs[1],
        0,
        @intFromEnum(LIR.BoxyTransferMode.move),
    );

    const copied: [*]const u8 = @ptrFromInt(copied_box);
    const copied_left_box = std.mem.bytesToValue(usize, copied[left_offset..][0..@sizeOf(usize)]);
    const copied_right_box = std.mem.bytesToValue(usize, copied[right_offset..][0..@sizeOf(usize)]);
    const copied_left: [*]const u8 = @ptrFromInt(copied_left_box);
    const copied_right: [*]const u8 = @ptrFromInt(copied_right_box);
    try std.testing.expectEqual(@as(i64, 7), std.mem.readInt(i64, copied_left[0..@sizeOf(i64)], .little));
    try std.testing.expectEqual(@as(i64, 11), std.mem.readInt(i64, copied_right[0..@sizeOf(i64)], .little));

    boxy_abi.roc_boxy_drop(@ptrCast(&copied_box), @intFromEnum(erased_box_layout), copied_desc, 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi dynamic numeric literal encodes through the descriptor payload layout" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .dec, .contains_refcounted = false },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.erasedBox()));
    const literal: i128 = 42;

    var boxed: usize = 0;
    boxy_abi.roc_boxy_dynamic_num_literal(@ptrCast(&boxed), &literal, &descs[0], box_layout);
    var out: u64 = 0;
    var out_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(@ptrCast(&out), &out_desc, @ptrCast(&boxed), box_layout, &descs[0], null, @intFromEnum(layout_mod.Idx.u64), 0);
    try std.testing.expectEqual(@as(u64, 42), out);
    boxy_abi.roc_boxy_drop(@ptrCast(&boxed), box_layout, &descs[0], 1, 1, 0);

    var dec_boxed: usize = 0;
    boxy_abi.roc_boxy_dynamic_num_literal(@ptrCast(&dec_boxed), &literal, &descs[1], box_layout);
    var dec_out: i128 = 0;
    var dec_out_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(@ptrCast(&dec_out), &dec_out_desc, @ptrCast(&dec_boxed), box_layout, &descs[1], null, @intFromEnum(layout_mod.Idx.dec), 0);
    try std.testing.expectEqual(@as(i128, 42) * builtins.dec.RocDec.one_point_zero_i128, dec_out);
    boxy_abi.roc_boxy_drop(@ptrCast(&dec_boxed), box_layout, &descs[1], 1, 1, 0);

    try setup.env.checkForLeaks();
}

test "boxy abi dynamic numeric literal publishes its default scalar descriptor" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = box_layout, .contains_refcounted = true },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    const literal: i128 = 42;
    var boxed: usize = 0;
    var literal_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_dynamic_num_literal_ref(
        @ptrCast(&boxed),
        &literal_desc,
        &literal,
        &descs[0],
        @intFromEnum(layout_mod.Idx.i64),
        @intFromEnum(box_layout),
    );

    try std.testing.expectEqual(layout_mod.Idx.i64, literal_desc.?.payload_layout);
    var out: i64 = 0;
    var out_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(
        @ptrCast(&out),
        &out_desc,
        @ptrCast(&boxed),
        @intFromEnum(box_layout),
        literal_desc.?,
        null,
        @intFromEnum(layout_mod.Idx.i64),
        0,
    );
    try std.testing.expectEqual(@as(i64, 42), out);
    boxy_abi.roc_boxy_drop(@ptrCast(&boxed), @intFromEnum(box_layout), literal_desc, 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi unbox specializes a concrete tag descriptor before materialization" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const name_err = try setup.store.insertString("Err");
    const name_ok = try setup.store.insertString("Ok");
    const erased_box_layout = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const source_union_layout = try setup.layouts.putTagUnion(&.{erased_box_layout});
    const target_union_layout = try setup.layouts.putTagUnion(&.{ .zst, .u8 });
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{
        .{ .payload_index = 0, .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) } },
        .{ .payload_index = 0, .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) } },
    };
    const variants = [_]LirProgram.BoxyTagVariant{
        .{
            .name = name_ok,
            .discriminant = 0,
            .payload_layout = erased_box_layout,
            .payload_count = 1,
            .payload_descs = .{ .start = 0, .len = 1 },
        },
        .{
            .name = name_err,
            .discriminant = 0,
            .payload_layout = .zst,
            .payload_count = 0,
        },
        .{
            .name = name_ok,
            .discriminant = 1,
            .payload_layout = .u8,
            .payload_count = 1,
            .payload_descs = .{ .start = 1, .len = 1 },
        },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u8, .contains_refcounted = false },
        .{
            .payload_layout = source_union_layout,
            .contains_refcounted = true,
            .tag_variants = .{ .start = 0, .len = 1 },
        },
        .{
            .payload_layout = target_union_layout,
            .contains_refcounted = false,
            .tag_variants = .{ .start = 1, .len = 2 },
        },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var payload: u8 = 97;
    var payload_box: usize = 0;
    var payload_box_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_box(
        @ptrCast(&payload_box),
        &payload_box_desc,
        @ptrCast(&payload),
        @intFromEnum(layout_mod.Idx.u8),
        null,
        &descs[0],
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(erased_box_layout),
    );

    var source_tag: usize = 0;
    boxy_abi.roc_boxy_tag(
        @ptrCast(&source_tag),
        &descs[1],
        @intFromEnum(name_ok),
        @ptrCast(&payload_box),
        @intFromEnum(erased_box_layout),
        payload_box_desc,
        @intFromEnum(LIR.BoxyTransferMode.move),
        @intFromEnum(erased_box_layout),
    );

    var target: [16]u8 align(8) = @splat(0);
    var target_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_unbox(
        &target,
        &target_desc,
        @ptrCast(&source_tag),
        @intFromEnum(erased_box_layout),
        &descs[1],
        &descs[2],
        @intFromEnum(target_union_layout),
        @intFromEnum(LIR.BoxyTransferMode.move),
    );

    try std.testing.expect(boxy_abi.roc_boxy_tag_match(
        &target,
        @intFromEnum(target_union_layout),
        target_desc.?,
        @intFromEnum(name_ok),
    ));
    var unboxed_payload: u8 = 0;
    var unboxed_payload_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_tag_payload(
        @ptrCast(&unboxed_payload),
        &unboxed_payload_desc,
        &target,
        @intFromEnum(target_union_layout),
        target_desc.?,
        @intFromEnum(name_ok),
        0,
        @intFromEnum(layout_mod.Idx.u8),
        @intFromEnum(LIR.BoxyTransferMode.borrow),
    );
    try std.testing.expectEqual(@as(u8, 97), unboxed_payload);

    boxy_abi.roc_boxy_drop(&target, @intFromEnum(target_union_layout), target_desc, 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi tag construction, matching, and payload reads" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const name_a = try setup.store.insertString("A");
    const name_b = try setup.store.insertString("B");
    const union_layout = try setup.layouts.putTagUnion(&.{ .u64, .u64 });

    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = name_a, .discriminant = 0, .payload_layout = .u64, .payload_count = 1 },
        .{ .name = name_b, .discriminant = 1, .payload_layout = .u64, .payload_count = 1 },
    };
    const descs = [_]BoxyTypeDesc{
        .{
            .payload_layout = union_layout,
            .contains_refcounted = false,
            .tag_variants = .{ .start = 0, .len = 2 },
        },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .tag_variants = &variants,
    });

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.erasedBox()));
    var payload: u64 = 7;
    var tagged: usize = 0;
    boxy_abi.roc_boxy_tag(
        @ptrCast(&tagged),
        &descs[0],
        @intFromEnum(name_a),
        @ptrCast(&payload),
        @intFromEnum(layout_mod.Idx.u64),
        null,
        @intFromEnum(LIR.BoxyTransferMode.copy),
        box_layout,
    );
    try std.testing.expect(tagged != 0);

    try std.testing.expect(boxy_abi.roc_boxy_tag_match(@ptrCast(&tagged), box_layout, &descs[0], @intFromEnum(name_a)));
    try std.testing.expect(!boxy_abi.roc_boxy_tag_match(@ptrCast(&tagged), box_layout, &descs[0], @intFromEnum(name_b)));

    var read_payload: u64 = 0;
    var read_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_tag_payload(
        @ptrCast(&read_payload),
        &read_desc,
        @ptrCast(&tagged),
        box_layout,
        &descs[0],
        @intFromEnum(name_a),
        0,
        @intFromEnum(layout_mod.Idx.u64),
        @intFromEnum(LIR.BoxyTransferMode.borrow),
    );
    try std.testing.expectEqual(@as(u64, 7), read_payload);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, null), read_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&tagged), box_layout, &descs[0], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi copied tag payload owns its nested list" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const empty_name = try setup.store.insertString("Empty");
    const values_name = try setup.store.insertString("Values");
    const list_str_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.str));
    const union_layout = try setup.layouts.putTagUnion(&.{ .zst, list_str_layout });
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(fixtureTableIndex(0)) }};
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{.{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(1) },
    }};
    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = empty_name, .discriminant = 0, .payload_layout = .zst, .payload_count = 0 },
        .{
            .name = values_name,
            .discriminant = 1,
            .payload_layout = list_str_layout,
            .payload_count = 1,
            .payload_descs = .{ .start = 0, .len = 1 },
        },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
        .{ .payload_layout = list_str_layout, .contains_refcounted = true, .nested_descs = .{ .start = 0, .len = 1 } },
        .{ .payload_layout = union_layout, .contains_refcounted = true, .tag_variants = .{ .start = 0, .len = 2 } },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .tag_variants = &variants,
        .tag_payload_descs = &payload_descs,
    });

    var source_list = builtins.list.listWithCapacity(
        1,
        @alignOf(builtins.str.RocStr),
        @sizeOf(builtins.str.RocStr),
        true,
        null,
        &builtins.utils.rcNone,
        setup.env.get_ops(),
    );
    source_list.length = 1;
    const source_strings: [*]builtins.str.RocStr = @ptrCast(@alignCast(source_list.bytes.?));
    source_strings[0] = builtins.str.RocStr.fromSlice(
        "a heap string retained by copied tag payload ownership",
        setup.env.get_ops(),
    );

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.erasedBox()));
    var tagged: usize = 0;
    boxy_abi.roc_boxy_tag(
        @ptrCast(&tagged),
        &descs[2],
        @intFromEnum(values_name),
        @ptrCast(&source_list),
        @intFromEnum(list_str_layout),
        &descs[1],
        @intFromEnum(LIR.BoxyTransferMode.move),
        box_layout,
    );

    var copied: builtins.list.RocList = undefined;
    var copied_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_tag_payload(
        @ptrCast(&copied),
        &copied_desc,
        @ptrCast(&tagged),
        box_layout,
        &descs[2],
        @intFromEnum(values_name),
        0,
        @intFromEnum(list_str_layout),
        @intFromEnum(LIR.BoxyTransferMode.copy),
    );
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, &descs[1]), copied_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&tagged), box_layout, &descs[2], 1, 1, 0);
    const copied_strings: [*]const builtins.str.RocStr = @ptrCast(@alignCast(copied.bytes.?));
    try std.testing.expectEqualStrings(
        "a heap string retained by copied tag payload ownership",
        copied_strings[0].asSlice(),
    );
    boxy_abi.roc_boxy_drop(@ptrCast(&copied), @intFromEnum(list_str_layout), &descs[1], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi descriptor copy materializes a template with local captures" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    // Template descriptor 1 nests a frame-local descriptor reference (local
    // id 5); the copy binds it to descriptor 0.
    const erased_box = try setup.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const desc_refs = [_]LirProgram.BoxyDescRef{
        .{ .local = @enumFromInt(5) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{
            .payload_layout = erased_box,
            .contains_refcounted = true,
            .nested_descs = .{ .start = 0, .len = 1 },
        },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
    });

    const capture_ids = [_]u32{5};
    const capture_descs = [_]?*const BoxyTypeDesc{&descs[0]};
    const copied = boxy_abi.roc_boxy_desc_copy(
        1,
        &capture_ids,
        &capture_descs,
        1,
    );
    try std.testing.expect(copied != &descs[1]);
    try std.testing.expectEqual(erased_box, copied.payload_layout);
    try std.testing.expectEqual(@as(u32, 1), copied.nested_descs.len);

    // The nested slot resolves to the captured descriptor's shape through
    // the runtime tables.
    const nested = boxy_abi.roc_boxy_nested_desc(copied, 0);
    try std.testing.expectEqual(layout_mod.Idx.u64, nested.payload_layout);
}

fn sumTwoU64s(
    _: *builtins.host_abi.RocOps,
    test_context: ?*anyopaque,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const context_observed: *bool = @ptrCast(@alignCast(test_context.?));
    context_observed.* = true;
    const a: *align(1) const u64 = @ptrCast(args[0].?);
    const b: *align(1) const u64 = @ptrCast(args[1].?);
    const out: *align(1) u64 = @ptrCast(ret.?);
    out.* = a.* + b.*;
    ret_desc.* = null;
}

var expectedDictionaryArgDesc: ?*const BoxyTypeDesc = null;

fn receivesExpectedDictionaryArgDesc(
    _: *builtins.host_abi.RocOps,
    _: ?*anyopaque,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const raw_desc: *align(1) const usize = @ptrCast(args[1].?);
    const out: *align(1) u8 = @ptrCast(ret.?);
    out.* = @intFromBool(raw_desc.* == @intFromPtr(expectedDictionaryArgDesc.?));
    ret_desc.* = null;
}

test "boxy abi dictionary dispatch calls a registered native worker" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const method_slots = [_]LirProgram.BoxyMethodSlot{
        .{
            .method = @enumFromInt(fixtureTableIndex(0)),
            .proc = @enumFromInt(3),
        },
    };
    const dicts = [_]LirProgram.BoxyDict{
        .{ .method_slots = .{ .start = 0, .len = 1 } },
    };
    try setup.startRuntime(allocator, .{
        .dicts = &dicts,
        .method_slots = &method_slots,
    });

    boxy_abi.roc_boxy_register_proc(3, &sumTwoU64s, @intFromEnum(layout_mod.Idx.u64), 0, false, 0);

    var lhs: u64 = 30;
    var rhs: u64 = 12;
    const args = [_]boxy_abi.RocBoxyCallArg{
        .{ .value = @ptrCast(&lhs), .layout = @intFromEnum(layout_mod.Idx.u64), .desc = null },
        .{ .value = @ptrCast(&rhs), .layout = @intFromEnum(layout_mod.Idx.u64), .desc = null },
    };
    var out: u64 = 0;
    var out_desc: ?*const BoxyTypeDesc = null;
    var context_observed = false;
    boxy_abi.roc_boxy_call_dict(
        @ptrCast(&out),
        &out_desc,
        @ptrCast(&context_observed),
        &dicts[0],
        0,
        0,
        &args,
        args.len,
        null,
        0,
        null,
        @intFromEnum(layout_mod.Idx.u64),
    );
    try std.testing.expect(context_observed);
    try std.testing.expectEqual(@as(u64, 42), out);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, null), out_desc);
}

test "boxy abi dictionary call preserves a full descriptor across a payload-shaped call boundary" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const aggregate_layout = try setup.layouts.putStructFields(&.{
        .{ .index = 0, .layout = .u64 },
        .{ .index = 1, .layout = .u64 },
    });
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = aggregate_layout, .contains_refcounted = false },
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(1)) },
        .{ .static = @enumFromInt(fixtureTableIndex(1)) },
    };
    const hidden_sources = [_]LirProgram.BoxyMethodHiddenDescSource{
        .{ .argument = 0 },
    };
    const method_slots = [_]LirProgram.BoxyMethodSlot{.{
        .method = @enumFromInt(fixtureTableIndex(0)),
        .proc = @enumFromInt(4),
        .hidden_descs = .{ .start = 1, .len = 1 },
        .adapter = .{
            .arg_layouts = .{ .start = 0, .len = 1 },
            .arg_descs = .{ .start = 0, .len = 1 },
            .hidden_desc_sources = .{ .start = 0, .len = 1 },
        },
    }};
    const method_arg_layouts = [_]layout_mod.Idx{aggregate_layout};
    const dicts = [_]LirProgram.BoxyDict{
        .{ .method_slots = .{ .start = 0, .len = 1 } },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .dicts = &dicts,
        .method_slots = &method_slots,
        .method_arg_layouts = &method_arg_layouts,
        .method_hidden_desc_sources = &hidden_sources,
    });
    expectedDictionaryArgDesc = &descs[0];
    defer expectedDictionaryArgDesc = null;
    boxy_abi.roc_boxy_register_proc(4, &receivesExpectedDictionaryArgDesc, @intFromEnum(layout_mod.Idx.bool), 1, false, 0);

    var value = [_]u64{ 1, 2 };
    const args = [_]boxy_abi.RocBoxyCallArg{.{
        .value = @ptrCast(&value),
        .layout = @intFromEnum(aggregate_layout),
        .desc = &descs[0],
    }};
    var out: u8 = 0;
    var out_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_call_dict(
        @ptrCast(&out),
        &out_desc,
        null,
        &dicts[0],
        0,
        0,
        &args,
        args.len,
        null,
        0,
        null,
        @intFromEnum(layout_mod.Idx.bool),
    );
    try std.testing.expectEqual(@as(u8, 1), out);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, null), out_desc);
}

test "boxy abi dictionary dispatch runs structural equality slots inline" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
    };
    const desc_refs = [_]LirProgram.BoxyDescRef{
        .{ .static = @enumFromInt(fixtureTableIndex(0)) },
    };
    const method_slots = [_]LirProgram.BoxyMethodSlot{
        .{
            .method = @enumFromInt(fixtureTableIndex(0)),
            .proc = @enumFromInt(fixtureTableIndex(0)),
            .hidden_descs = .{ .start = 0, .len = 1 },
            .structural_eq = true,
        },
    };
    const dicts = [_]LirProgram.BoxyDict{
        .{ .method_slots = .{ .start = 0, .len = 1 } },
    };
    try setup.startRuntime(allocator, .{
        .type_descs = &descs,
        .desc_refs = &desc_refs,
        .dicts = &dicts,
        .method_slots = &method_slots,
    });

    var lhs: u64 = 42;
    var rhs: u64 = 42;
    const args = [_]boxy_abi.RocBoxyCallArg{
        .{ .value = @ptrCast(&lhs), .layout = @intFromEnum(layout_mod.Idx.u64), .desc = null },
        .{ .value = @ptrCast(&rhs), .layout = @intFromEnum(layout_mod.Idx.u64), .desc = null },
    };
    var out: u8 = 0;
    var out_desc: ?*const BoxyTypeDesc = null;
    boxy_abi.roc_boxy_call_dict(
        @ptrCast(&out),
        &out_desc,
        null,
        &dicts[0],
        0,
        0,
        &args,
        args.len,
        null,
        0,
        null,
        @intFromEnum(layout_mod.Idx.bool),
    );
    try std.testing.expectEqual(@as(u8, 1), out);

    rhs = 7;
    boxy_abi.roc_boxy_call_dict(
        @ptrCast(&out),
        &out_desc,
        null,
        &dicts[0],
        0,
        0,
        &args,
        args.len,
        null,
        0,
        null,
        @intFromEnum(layout_mod.Idx.bool),
    );
    try std.testing.expectEqual(@as(u8, 0), out);
}

test "boxy abi drop balances refcounts across incref and decref" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .str, .contains_refcounted = true },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    const str_layout = @intFromEnum(layout_mod.Idx.str);
    var rs = builtins.str.RocStr.fromSlice("another heap string long enough to escape small-string storage", setup.env.get_ops());
    boxy_abi.roc_boxy_drop(@ptrCast(&rs), str_layout, null, 0, 1, 0);
    boxy_abi.roc_boxy_drop(@ptrCast(&rs), str_layout, null, 1, 1, 0);
    boxy_abi.roc_boxy_drop(@ptrCast(&rs), str_layout, null, 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi sidecar view initializes the global runtime from image bytes" {
    const allocator = std.testing.allocator;

    // Lay a minimal lowered result out in one contiguous buffer, the same
    // shape the LIR image serializer records offsets into.
    const buffer = try allocator.alignedAlloc(u8, .@"16", 1 << 20);
    defer allocator.free(buffer);
    var fba = std.heap.FixedBufferAllocator.init(buffer);
    const fba_alloc = fba.allocator();

    var lowered: LirProgram.Result = undefined;
    lowered.store = LirStore.init(fba_alloc);
    lowered.layouts = try layout_mod.Store.init(fba_alloc, base.target.TargetUsize.native);
    lowered.root_procs = .empty;
    lowered.boxy_type_descs = .empty;
    lowered.boxy_dicts = .empty;
    lowered.boxy_adapters = .empty;
    lowered.boxy_desc_refs = .empty;
    lowered.boxy_dict_refs = .empty;
    lowered.boxy_tag_variants = .empty;
    lowered.boxy_tag_payload_descs = .empty;
    lowered.boxy_field_names = .empty;
    lowered.boxy_adapt_steps = .empty;
    lowered.boxy_payload_steps = .empty;
    lowered.boxy_method_slots = .empty;
    lowered.boxy_method_arg_layouts = .empty;
    lowered.boxy_method_hidden_desc_sources = .empty;
    lowered.boxy_erased_arg_layouts = .empty;
    lowered.boxy_erased_arg_desc_keys = .empty;
    lowered.boxy_erased_arg_desc_offsets = .empty;
    lowered.boxy_erased_arg_desc_params = .empty;

    const tag_name = try lowered.store.insertString("Only");
    try lowered.boxy_type_descs.append(fba_alloc, .{
        .payload_layout = .u64,
        .contains_refcounted = false,
    });
    try lowered.boxy_tag_variants.append(fba_alloc, .{
        .name = tag_name,
        .discriminant = 0,
        .payload_layout = .u64,
        .payload_count = 1,
    });

    const sidecar = try lir.LirImage.BoxySidecar.fromProgram(buffer.ptr, buffer.len, &lowered);
    var view = try sidecar.view(buffer.ptr, buffer.len, base.target.TargetUsize.native, allocator);
    defer view.deinit();

    try std.testing.expectEqual(@as(usize, 1), view.tables.type_descs.len);
    try std.testing.expectEqual(layout_mod.Idx.u64, view.tables.type_descs[0].payload_layout);
    try std.testing.expectEqualStrings("Only", view.strings.get(view.tables.tag_variants[0].name));

    var env = RuntimeHostEnv.init(allocator);
    defer env.deinit();
    try boxy_abi.initGlobalFromSidecarView(allocator, &view, env.get_ops());
    defer boxy_abi.deinitGlobal();

    var a: u64 = 5;
    var b: u64 = 5;
    try std.testing.expect(boxy_abi.roc_boxy_eq(
        @ptrCast(&a),
        @ptrCast(&b),
        @intFromEnum(layout_mod.Idx.u64),
        &view.tables.type_descs[0],
    ));
}

test "boxy builtin parameter ABI sizes match the wrapper declarations" {
    const BoxyBuiltinFn = backend.LirCodeGenMod.BoxyBuiltinFn;

    // Apple's arm64 ABI packs overflow arguments on the stack at their natural
    // size, so the dev backend places them from `paramAbiSizes`. A row that
    // drifts from the wrapper it describes would silently misplace every
    // argument after the first sub-word one, so check every row here. A
    // wrapper may omit its row only when all of its parameters reach the
    // callee in registers on every supported target.
    const max_int_param_regs = 8; // AAPCS64; x86_64 has fewer

    inline for (std.meta.fields(BoxyBuiltinFn)) |field| {
        const boxy_fn: BoxyBuiltinFn = @enumFromInt(field.value);
        const params = @typeInfo(@TypeOf(@field(eval.boxy_abi, boxy_fn.symbolName()))).@"fn".params;

        if (boxy_fn.paramAbiSizes()) |sizes| {
            try std.testing.expectEqual(params.len, sizes.len);
            inline for (params, 0..) |param, i| {
                errdefer std.debug.print(
                    "{s} parameter {d} is {s}\n",
                    .{ boxy_fn.symbolName(), i, @typeName(param.type.?) },
                );
                try std.testing.expectEqual(@as(u8, @sizeOf(param.type.?)), sizes[i]);
            }
        } else {
            errdefer std.debug.print(
                "{s} takes {d} parameters and needs a paramAbiSizes row\n",
                .{ boxy_fn.symbolName(), params.len },
            );
            try std.testing.expect(params.len <= max_int_param_regs);
        }
    }
}
