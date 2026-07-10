//! Unit tests for the C-ABI boxy runtime wrappers.
//!
//! Each test builds descriptor tables and layouts directly (the same data the
//! boxy lowerer serializes for a program), initializes the process-global
//! runtime over them with a tracking `RocOps`, exercises the wrappers on raw
//! value bytes, and checks refcount balance through the allocation tracker.

const std = @import("std");
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

const TestSetup = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    env: RuntimeHostEnv,

    fn init(allocator: std.mem.Allocator) !TestSetup {
        return .{
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(allocator, base.target.TargetUsize.native),
            .env = RuntimeHostEnv.init(allocator),
        };
    }

    fn startRuntime(self: *TestSetup, allocator: std.mem.Allocator, tables: boxy_runtime.BoxyTables) !void {
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
    _: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    const rendered = builtins.str.RocStr.fromSlice("custom inspect result stored outside the small-string representation", ops);
    const out: *align(1) builtins.str.RocStr = @ptrCast(ret.?);
    out.* = rendered;
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
        .inspect_method = @enumFromInt(0),
    }};
    const desc_refs = [_]LIR.BoxyDescRef{.{ .static = @enumFromInt(0) }};
    const method_slots = [_]LirProgram.BoxyMethodSlot{.{
        .method = @enumFromInt(0),
        .proc = @enumFromInt(0),
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
    boxy_abi.roc_boxy_inspect(
        @ptrCast(&rendered),
        @ptrCast(&value),
        @intFromEnum(layout_mod.Idx.u64),
        &descs[0],
    );
    try std.testing.expectEqualStrings("custom inspect result stored outside the small-string representation", rendered.asSlice());
    rendered.decref(setup.env.get_ops());
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

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst()));
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
        .{ .static = @enumFromInt(0) },
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

    boxy_abi.roc_boxy_drop(@ptrCast(&source), @intFromEnum(list_layout), &descs[2], 1, 1, 0);
    boxy_abi.roc_boxy_drop(@ptrCast(&materialized), @intFromEnum(list_layout), &descs[3], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi relabel adapter transfers a list allocation unchanged" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(0) },
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

    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst());
    const source_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(box_layout));
    const target_list_layout = try setup.layouts.insertLayout(layout_mod.Layout.list(.u64));
    const desc_refs = [_]LIR.BoxyDescRef{
        .{ .static = @enumFromInt(0) },
        .{ .static = @enumFromInt(0) },
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
    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst());
    const source_union_layout = try setup.layouts.putTagUnion(&.{ .u64, box_layout });
    const target_union_layout = try setup.layouts.putTagUnion(&.{ .u64, .u64 });
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{.{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(0) },
    }};
    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = name_a, .discriminant = 0, .payload_layout = .u64 },
        .{ .name = name_b, .discriminant = 1, .payload_layout = box_layout, .payload_descs = .{ .start = 0, .len = 1 } },
        .{ .name = name_b, .discriminant = 0, .payload_layout = .u64 },
        .{ .name = name_c, .discriminant = 1, .payload_layout = .u64 },
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
    const box_layout = try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst());
    const source_union_layout = try setup.layouts.putTagUnion(&.{.str});
    const target_union_layout = try setup.layouts.putTagUnion(&.{ .zst, box_layout });
    const payload_descs = [_]LirProgram.BoxyTagPayloadDesc{.{
        .payload_index = 0,
        .desc = .{ .static = @enumFromInt(0) },
    }};
    const variants = [_]LirProgram.BoxyTagVariant{
        .{
            .name = name_ok,
            .discriminant = 0,
            .payload_layout = .str,
            .payload_descs = .{ .start = 0, .len = 1 },
        },
        .{ .name = name_err, .discriminant = 0, .payload_layout = .zst },
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

test "boxy abi dynamic numeric literal encodes through the descriptor payload layout" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{ .payload_layout = .dec, .contains_refcounted = false },
    };
    try setup.startRuntime(allocator, .{ .type_descs = &descs });

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst()));
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

test "boxy abi tag construction, matching, and payload reads" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const name_a = try setup.store.insertString("A");
    const name_b = try setup.store.insertString("B");
    const union_layout = try setup.layouts.putTagUnion(&.{ .u64, .u64 });

    const variants = [_]LirProgram.BoxyTagVariant{
        .{ .name = name_a, .discriminant = 0, .payload_layout = .u64 },
        .{ .name = name_b, .discriminant = 1, .payload_layout = .u64 },
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

    const box_layout = @intFromEnum(try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst()));
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
    );
    try std.testing.expectEqual(@as(u64, 7), read_payload);
    try std.testing.expectEqual(@as(?*const BoxyTypeDesc, null), read_desc);

    boxy_abi.roc_boxy_drop(@ptrCast(&tagged), box_layout, &descs[0], 1, 1, 0);
    try setup.env.checkForLeaks();
}

test "boxy abi descriptor copy materializes a template with local captures" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    // Template descriptor 1 nests a frame-local descriptor reference (local
    // id 5); the copy binds it to descriptor 0.
    const box_of_zst = try setup.layouts.insertLayout(layout_mod.Layout.boxOfZst());
    const desc_refs = [_]LirProgram.BoxyDescRef{
        .{ .local = @enumFromInt(5) },
    };
    const descs = [_]BoxyTypeDesc{
        .{ .payload_layout = .u64, .contains_refcounted = false },
        .{
            .payload_layout = box_of_zst,
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
    try std.testing.expectEqual(box_of_zst, copied.payload_layout);
    try std.testing.expectEqual(@as(u32, 1), copied.nested_descs.len);

    // The nested slot resolves to the captured descriptor's shape through
    // the runtime tables.
    const nested = boxy_abi.roc_boxy_nested_desc(copied, 0);
    try std.testing.expectEqual(layout_mod.Idx.u64, nested.payload_layout);
}

fn sumTwoU64s(
    ops: *builtins.host_abi.RocOps,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void {
    _ = ops;
    const a: *align(1) const u64 = @ptrCast(args[0].?);
    const b: *align(1) const u64 = @ptrCast(args[1].?);
    const out: *align(1) u64 = @ptrCast(ret.?);
    out.* = a.* + b.*;
    ret_desc.* = null;
}

test "boxy abi dictionary dispatch calls a registered native worker" {
    const allocator = std.testing.allocator;
    var setup = try TestSetup.init(allocator);
    defer setup.deinit();

    const method_slots = [_]LirProgram.BoxyMethodSlot{
        .{
            .method = @enumFromInt(0),
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
    boxy_abi.roc_boxy_call_dict(
        @ptrCast(&out),
        &out_desc,
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
    try std.testing.expectEqual(@as(u64, 42), out);
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
        .{ .static = @enumFromInt(0) },
    };
    const method_slots = [_]LirProgram.BoxyMethodSlot{
        .{
            .method = @enumFromInt(0),
            .proc = @enumFromInt(0),
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

    const tag_name = try lowered.store.insertString("Only");
    try lowered.boxy_type_descs.append(fba_alloc, .{
        .payload_layout = .u64,
        .contains_refcounted = false,
    });
    try lowered.boxy_tag_variants.append(fba_alloc, .{
        .name = tag_name,
        .discriminant = 0,
        .payload_layout = .u64,
    });

    const sidecar = try lir.LirImage.BoxySidecar.fromProgram(buffer.ptr, buffer.len, &lowered);
    var view = try sidecar.view(buffer.ptr, buffer.len, base.target.TargetUsize.native, allocator);
    defer view.layouts.interned_layouts.deinit();

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
