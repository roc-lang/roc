//! Tests of the descriptor walkers and the runtime's entry points.

const std = @import("std");
const builtins = @import("builtins");
const shape = @import("layout_shape");
const desc_mod = @import("desc.zig");
const test_descs = @import("test_descs.zig");
const walk = @import("walk.zig");
const exports = @import("exports.zig");
const compiled = @import("compiled.zig");

const testing = std.testing;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocDec = builtins.dec.RocDec;
const TestEnv = builtins.utils.TestEnv;
const utils = builtins.utils;
const erased_callable = builtins.erased_callable;
const hash_mod = builtins.hash;

const TypeDesc = desc_mod.TypeDesc;
const Name = desc_mod.Name;

fn name(comptime text: []const u8) Name {
    // The id is a SHA-256 digest; computing one at compile time takes far
    // more branches than the default quota allows.
    @setEvalBranchQuota(200_000);
    return .{ .id = desc_mod.labelId(text), .len = text.len, .bytes = text.ptr };
}

const name_a = name("a");
const name_b = name("b");
const name_c = name("c");
const name_ok = name("Ok");
const name_err = name("Err");

/// The errors these tests' helpers return.
const TestError = std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult };

/// A test host: allocation tracking, and the tests' descriptors.
const Env = struct {
    test_env: TestEnv,
    descs: test_descs.Builder,

    fn init() Env {
        return .{ .test_env = TestEnv.init(testing.allocator), .descs = .init(testing.allocator, .native) };
    }

    fn deinit(self: *Env) void {
        self.descs.deinit();
        self.test_env.deinit();
    }

    fn ops(self: *Env) *RocOps {
        return self.test_env.getOps();
    }

    fn allocations(self: *Env) usize {
        return self.test_env.getAllocationCount();
    }
};

/// Storage for one value, aligned for any descriptor.
const Slot = struct {
    bytes: [256]u8 align(16) = @splat(0),

    fn ptr(self: *Slot) [*]u8 {
        return &self.bytes;
    }

    fn write(self: *Slot, offset: u32, comptime T: type, value: T) void {
        @as(*align(1) T, @ptrCast(self.bytes[offset..].ptr)).* = value;
    }
};

fn fieldOffset(d: *const TypeDesc, field_index: u32) u32 {
    return d.fieldBySemanticIndex(field_index).offset;
}

fn bigStr(text: []const u8, ops: *RocOps) RocStr {
    std.debug.assert(!RocStr.fitsInSmallStr(text.len));
    return RocStr.fromSlice(text, ops);
}

fn i64List(values: []const i64, ops: *RocOps) RocList {
    const list = RocList.list_allocate(@alignOf(i64), values.len, @sizeOf(i64), false, ops);
    if (values.len != 0) @memcpy(@as([*]i64, @ptrCast(@alignCast(list.bytes.?)))[0..values.len], values);
    return list;
}

fn expectInspect(env: *Env, d: *const TypeDesc, ptr: [*]u8, expected: []const u8) TestError!void {
    const rendered = walk.inspect(d, env.descs.names(d), ptr, env.ops());
    defer rendered.decref(env.ops());
    try testing.expectEqualStrings(expected, rendered.asSlice());
}

fn recordAB(env: *Env, a: *const TypeDesc, b: *const TypeDesc) *const TypeDesc {
    return env.descs.record(&.{ .{ .name = &name_b, .desc = b }, .{ .name = &name_a, .desc = a } });
}

test "a record lays out by descending alignment, then field name" {
    var env = Env.init();
    defer env.deinit();
    const record = env.descs.record(&.{
        .{ .name = &name_c, .desc = desc_mod.scalarDesc(.u8) },
        .{ .name = &name_a, .desc = desc_mod.scalarDesc(.u8) },
        .{ .name = &name_b, .desc = desc_mod.scalarDesc(.u64) },
    });
    try testing.expectEqual(@as(u32, 16), record.size);
    try testing.expectEqual(@as(u32, 8), record.alignment);
    const fields = record.fields();
    try testing.expectEqual(@as(u32, 1), fields[0].field_index);
    try testing.expectEqual(@as(u32, 0), fields[0].offset);
    try testing.expectEqual(@as(u32, 0), fields[1].field_index);
    try testing.expectEqual(@as(u32, 8), fields[1].offset);
    try testing.expectEqual(@as(u32, 2), fields[2].field_index);
    try testing.expectEqual(@as(u32, 9), fields[2].offset);
    try testing.expectEqual(@as(u32, 8), fieldOffset(record, 0));
    try testing.expectEqual(@as(u32, 9), fieldOffset(record, 2));
}

test "argument records keep declared order at natural alignment" {
    var env = Env.init();
    defer env.deinit();
    const args = env.descs.args(&.{ desc_mod.scalarDesc(.u8), desc_mod.scalarDesc(.u64), desc_mod.scalarDesc(.u8) });
    try testing.expectEqual(@as(u32, 0), args.fields()[0].offset);
    try testing.expectEqual(@as(u32, 8), args.fields()[1].offset);
    try testing.expectEqual(@as(u32, 16), args.fields()[2].offset);
    try testing.expectEqual(@as(u32, 24), args.size);
    try testing.expect(args.flags.declared_order);
}

test "zero-sized records keep their kind with the zero-sized shape" {
    var env = Env.init();
    defer env.deinit();
    const empty = env.descs.record(&.{});
    const nested = env.descs.record(&.{.{ .name = &name_a, .desc = empty }});
    try testing.expectEqual(desc_mod.Kind.record, nested.kind);
    try testing.expectEqual(@as(u32, 0), nested.size);
    try testing.expectEqual(@as(u32, 1), nested.alignment);
    try testing.expectEqual(@backingInt(shape.SortKey.align_1), nested.sort_key);
    var slot: Slot = .{};
    try expectInspect(&env, empty, slot.ptr(), "{}");
    try expectInspect(&env, nested, slot.ptr(), "{ a: {} }");
}

test "inspect renders records, tuples, tag unions, lists, and boxes" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const i64_desc = desc_mod.scalarDesc(.i64);
    const str_desc = desc_mod.strDesc();

    const record = recordAB(&env, i64_desc, str_desc);
    var record_slot: Slot = .{};
    record_slot.write(fieldOffset(record, 0), i64, 1);
    record_slot.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    try expectInspect(&env, record, record_slot.ptr(), "{ a: 1, b: \"x\" }");

    const pair = env.descs.tuple(&.{ i64_desc, str_desc });
    var pair_slot: Slot = .{};
    pair_slot.write(fieldOffset(pair, 0), i64, 1);
    pair_slot.write(fieldOffset(pair, 1), RocStr, RocStr.fromSliceSmall("a"));
    try expectInspect(&env, pair, pair_slot.ptr(), "(1, \"a\")");

    const list_desc = env.descs.list(i64_desc);
    const result = env.descs.tagUnion(&.{
        .{ .name = &name_ok, .payloads = &.{list_desc} },
        .{ .name = &name_err, .payloads = &.{str_desc} },
    });
    try testing.expectEqual(@as(u32, 1), result.discriminant_size);
    var ok_slot: Slot = .{};
    const numbers = i64List(&.{ 1, 2 }, ops);
    ok_slot.write(0, RocList, numbers);
    ok_slot.write(result.discriminant_offset, u8, 1);
    try expectInspect(&env, result, ok_slot.ptr(), "Ok([1, 2])");
    walk.decref(result, ok_slot.ptr(), ops);

    var err_slot: Slot = .{};
    err_slot.write(0, RocStr, RocStr.fromSliceSmall("no"));
    try expectInspect(&env, result, err_slot.ptr(), "Err(\"no\")");

    const two_args = env.descs.tagUnion(&.{
        .{ .name = &name_ok, .payloads = &.{ str_desc, i64_desc } },
    });
    var two_slot: Slot = .{};
    two_slot.write(fieldOffset(two_args.variants()[0].payload, 0), RocStr, RocStr.fromSliceSmall("s"));
    two_slot.write(fieldOffset(two_args.variants()[0].payload, 1), i64, -7);
    try expectInspect(&env, two_args, two_slot.ptr(), "Ok(\"s\", -7)");

    const records = env.descs.list(record);
    const record_list = RocList.list_allocate(record.alignment, 2, record.size, true, ops);
    for (0..2) |index| {
        var element: Slot = .{};
        element.write(fieldOffset(record, 0), i64, @intCast(index + 1));
        element.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall(if (index == 0) "x" else "y"));
        @memcpy(record_list.bytes.?[index * record.size ..][0..record.size], element.bytes[0..record.size]);
    }
    var records_slot: Slot = .{};
    records_slot.write(0, RocList, record_list);
    try expectInspect(&env, records, records_slot.ptr(), "[{ a: 1, b: \"x\" }, { a: 2, b: \"y\" }]");
    walk.decref(records, records_slot.ptr(), ops);

    const nested = env.descs.list(list_desc);
    const outer = RocList.list_allocate(list_desc.alignment, 2, list_desc.size, true, ops);
    const inner_lists = [_]RocList{ i64List(&.{ 1, 2 }, ops), RocList.empty() };
    @memcpy(@as([*]RocList, @ptrCast(@alignCast(outer.bytes.?)))[0..2], &inner_lists);
    var nested_slot: Slot = .{};
    nested_slot.write(0, RocList, outer);
    try expectInspect(&env, nested, nested_slot.ptr(), "[[1, 2], []]");
    walk.decref(nested, nested_slot.ptr(), ops);

    const boxed = env.descs.box(i64_desc);
    const payload = utils.allocateWithRefcount(8, 8, false, ops);
    @as(*i64, @ptrCast(@alignCast(payload))).* = 5;
    var box_slot: Slot = .{};
    box_slot.write(0, ?[*]u8, payload);
    try expectInspect(&env, boxed, box_slot.ptr(), "Box(5)");
    walk.decref(boxed, box_slot.ptr(), ops);
}

test "inspect renders primitives the way the to_str builtins do" {
    var env = Env.init();
    defer env.deinit();
    var slot: Slot = .{};
    slot.write(0, f64, 1.5);
    try expectInspect(&env, desc_mod.scalarDesc(.f64), slot.ptr(), "1.5");
    slot.write(0, i128, RocDec.fromU64(3).num);
    try expectInspect(&env, desc_mod.scalarDesc(.dec), slot.ptr(), "3.0");
    slot.write(0, i8, -3);
    try expectInspect(&env, desc_mod.scalarDesc(.i8), slot.ptr(), "-3");
    slot.write(0, u128, std.math.maxInt(u128));
    try expectInspect(&env, desc_mod.scalarDesc(.u128), slot.ptr(), "340282366920938463463374607431768211455");
    slot.write(0, u8, 1);
    try expectInspect(&env, desc_mod.boolDesc(), slot.ptr(), "True");
    slot.write(0, u8, 0);
    try expectInspect(&env, desc_mod.boolDesc(), slot.ptr(), "False");
    slot.write(0, RocStr, RocStr.fromSliceSmall("q\""));
    try expectInspect(&env, desc_mod.strDesc(), slot.ptr(), "\"q\\\"\"");
    try expectInspect(&env, desc_mod.erasedCallableDesc(), slot.ptr(), "<function>");
}

test "a long rendering grows its string" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var values: [300]i64 = undefined;
    for (&values, 0..) |*value, index| value.* = @intCast(index);
    const list_desc = env.descs.list(desc_mod.scalarDesc(.i64));
    var slot: Slot = .{};
    slot.write(0, RocList, i64List(&values, ops));
    const rendered = walk.inspect(list_desc, env.descs.names(list_desc), slot.ptr(), ops);
    defer rendered.decref(ops);
    try testing.expect(std.mem.startsWith(u8, rendered.asSlice(), "[0, 1, 2, "));
    try testing.expect(std.mem.endsWith(u8, rendered.asSlice(), ", 298, 299]"));
    walk.decref(list_desc, slot.ptr(), ops);
}

test "decref releases nested allocations and incref keeps them alive" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const str_desc = desc_mod.strDesc();
    const strs = env.descs.list(str_desc);
    const record = recordAB(&env, desc_mod.scalarDesc(.i64), strs);
    const boxed = env.descs.box(record);
    const baseline = env.allocations();

    const list = RocList.list_allocate(str_desc.alignment, 2, str_desc.size, true, ops);
    const elements = @as([*]RocStr, @ptrCast(@alignCast(list.bytes.?)));
    elements[0] = bigStr("a string long enough to live on the heap", ops);
    elements[1] = bigStr("another string long enough for the heap", ops);
    const payload = utils.allocateWithRefcount(record.size, record.alignment, true, ops);
    @memset(payload[0..record.size], 0);
    @as(*RocList, @ptrCast(@alignCast(payload + fieldOffset(record, 1)))).* = list;
    var slot: Slot = .{};
    slot.write(0, ?[*]u8, payload);
    try testing.expectEqual(baseline + 4, env.allocations());

    walk.incref(boxed, slot.ptr(), 1, ops);
    walk.decref(boxed, slot.ptr(), ops);
    try testing.expectEqual(baseline + 4, env.allocations());

    // A shared list releases only its own reference.
    walk.incref(strs, payload + fieldOffset(record, 1), 1, ops);
    walk.decref(boxed, slot.ptr(), ops);
    try testing.expectEqual(baseline + 3, env.allocations());
    var list_slot: Slot = .{};
    list_slot.write(0, RocList, list);
    walk.decref(strs, list_slot.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "simple values retain multiple owners and release their allocations exactly once" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const text_desc = desc_mod.strDesc();
    const nominal_text = env.descs.nominal(text_desc, null, false);
    for ([_]*const TypeDesc{ text_desc, nominal_text }) |d| {
        for ([_][]const u8{ "small", "a string long enough to need a separate allocation" }) |text| {
            const baseline = env.allocations();
            var value = RocStr.fromSlice(text, ops);
            const held = env.allocations();
            walk.incref(d, @ptrCast(&value), 2, ops);
            for (0..2) |_| {
                walk.decref(d, @ptrCast(&value), ops);
                try testing.expectEqual(held, env.allocations());
                try testing.expectEqualStrings(text, value.asSlice());
            }
            walk.decref(d, @ptrCast(&value), ops);
            try testing.expectEqual(baseline, env.allocations());
        }
    }
    const list_desc = env.descs.list(desc_mod.scalarDesc(.i64));
    for ([_][]const i64{ &.{}, &.{ 4, 7 } }) |values| {
        const baseline = env.allocations();
        var value = i64List(values, ops);
        const held = env.allocations();
        walk.incref(list_desc, @ptrCast(&value), 2, ops);
        for (0..2) |_| {
            walk.decref(list_desc, @ptrCast(&value), ops);
            try testing.expectEqual(held, env.allocations());
        }
        walk.decref(list_desc, @ptrCast(&value), ops);
        try testing.expectEqual(baseline, env.allocations());
    }
    for ([_]*const TypeDesc{ desc_mod.zstDesc(), desc_mod.scalarDesc(.i64) }) |payload| {
        const d = env.descs.box(payload);
        const baseline = env.allocations();
        var value: ?[*]u8 = if (payload.size == 0) null else utils.allocateWithRefcount(payload.size, payload.alignment, false, ops);
        if (value) |ptr| @memset(ptr[0..payload.size], 0);
        const held = env.allocations();
        walk.incref(d, @ptrCast(&value), 2, ops);
        for (0..2) |_| {
            walk.decref(d, @ptrCast(&value), ops);
            try testing.expectEqual(held, env.allocations());
        }
        walk.decref(d, @ptrCast(&value), ops);
        try testing.expectEqual(baseline, env.allocations());
    }
}

test "decref of a deep recursive value keeps its work off the call stack" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    // A list of lists nested 10_000 deep, each level one element.
    const depth = 10_000;
    var descs: [depth + 1]*const TypeDesc = undefined;
    descs[0] = desc_mod.strDesc();
    for (1..depth + 1) |level| descs[level] = env.descs.list(descs[level - 1]);
    const baseline = env.allocations();

    var innermost = bigStr("the string at the bottom of the nesting", ops);
    var top: RocList = undefined;
    var element: [*]const u8 = @ptrCast(&innermost);
    var element_size: usize = @sizeOf(RocStr);
    for (0..depth) |_| {
        const level = RocList.list_allocate(@alignOf(usize), 1, element_size, true, ops);
        @memcpy(level.bytes.?[0..element_size], element[0..element_size]);
        top = level;
        element = @ptrCast(&top);
        element_size = @sizeOf(RocList);
    }
    var slot: Slot = .{};
    slot.write(0, RocList, top);
    walk.decref(descs[depth], slot.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "reference counting follows deep wrappers and preserves pending container releases" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var wrapped = desc_mod.strDesc();
    for (0..10_000) |_| {
        wrapped = env.descs.nominal(wrapped, null, false);
        wrapped = env.descs.tagUnion(&.{
            .{ .name = &name_ok, .payloads = &.{wrapped} },
        });
        wrapped = env.descs.tuple(&.{wrapped});
        wrapped = recordAB(&env, desc_mod.zstDesc(), wrapped);
    }
    const record = recordAB(&env, wrapped, wrapped);
    const boxed = env.descs.box(record);
    const list_desc = env.descs.list(boxed);
    const baseline = env.allocations();
    const payload = utils.allocateWithRefcount(record.size, record.alignment, true, ops);
    const text = "a heap string under many nominal and tag wrappers";
    for (0..2) |i| {
        @as(*RocStr, @ptrCast(@alignCast(payload + fieldOffset(record, @intCast(i))))).* = bigStr(text, ops);
    }
    // Retaining the wrapped record visits both children without recursion.
    walk.incref(record, payload, 1, ops);
    walk.decref(record, payload, ops);
    try testing.expectEqual(baseline + 3, env.allocations());
    for (0..2) |i| {
        const str: *const RocStr = @ptrCast(@alignCast(payload + fieldOffset(record, @intCast(i))));
        try testing.expectEqualStrings(text, str.asSlice());
    }
    var list = RocList.list_allocate(boxed.alignment, 1, boxed.size, true, ops);
    @as(*?[*]u8, @ptrCast(@alignCast(list.bytes.?))).* = payload;
    // List and box cleanup remain pending while their wrapped children run.
    walk.decref(list_desc, @ptrCast(&list), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "record traversal preserves final drop order and pending container releases" {
    const Drops = struct {
        const Self = @This();
        ids: [40]usize = undefined,
        len: usize = 0,

        const Capture = struct { drops: *Self, id: usize };

        fn onDrop(capture: ?[*]u8, _: *RocOps) callconv(.c) void {
            const saved: *const Capture = @ptrCast(@alignCast(capture.?));
            saved.drops.ids[saved.drops.len] = saved.id;
            saved.drops.len += 1;
        }
    };
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const leaf = recordAB(&env, desc_mod.scalarDesc(.u64), desc_mod.erasedCallableDesc());
    const fields: [40]*const TypeDesc = @splat(leaf);
    // One child, multiple siblings, and enough siblings to grow the work stack.
    for ([_]usize{ 1, 3, 40 }) |count| {
        const tuple = env.descs.tuple(fields[0..count]);
        const boxed = env.descs.box(tuple);
        const list_desc = env.descs.list(boxed);
        const baseline = env.allocations();
        var drops: Drops = .{};
        const payload = utils.allocateWithRefcount(tuple.size, tuple.alignment, true, ops);
        @memset(payload[0..tuple.size], 0);
        for (tuple.fields(), 0..) |field, index| {
            const callable = erased_callable.allocate(&Overrides.alwaysEqual, &Drops.onDrop, @sizeOf(Drops.Capture), ops);
            const capture: *Drops.Capture = @ptrCast(@alignCast(erased_callable.capturePtr(callable)));
            capture.* = .{ .drops = &drops, .id = index };
            const slot = payload + field.offset + fieldOffset(leaf, 1);
            @as(*[*]u8, @ptrCast(@alignCast(slot))).* = callable;
        }
        walk.incref(tuple, payload, 2, ops);
        for (0..2) |_| {
            walk.decref(tuple, payload, ops);
            try testing.expectEqual(@as(usize, 0), drops.len);
            try testing.expectEqual(baseline + count + 1, env.allocations());
        }
        var list = RocList.list_allocate(boxed.alignment, 1, boxed.size, true, ops);
        @as(*[*]u8, @ptrCast(@alignCast(list.bytes.?))).* = payload;
        walk.decref(list_desc, @ptrCast(&list), ops);
        try testing.expectEqual(count, drops.len);
        for (drops.ids[0..count], 0..) |id, index| try testing.expectEqual(index, id);
        try testing.expectEqual(baseline, env.allocations());
    }
}

test "equality compares structure and honors is_eq overrides" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const record = recordAB(&env, desc_mod.scalarDesc(.i64), desc_mod.strDesc());
    var lhs: Slot = .{};
    var rhs: Slot = .{};
    lhs.write(fieldOffset(record, 0), i64, 1);
    lhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    rhs.write(fieldOffset(record, 0), i64, 1);
    rhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    try testing.expect(walk.eq(record, lhs.ptr(), rhs.ptr(), ops));
    rhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("y"));
    try testing.expect(!walk.eq(record, lhs.ptr(), rhs.ptr(), ops));

    const list_desc = env.descs.list(desc_mod.scalarDesc(.i64));
    const result = env.descs.tagUnion(&.{
        .{ .name = &name_ok, .payloads = &.{list_desc} },
        .{ .name = &name_err, .payloads = &.{} },
    });
    var ok_a: Slot = .{};
    var ok_b: Slot = .{};
    var err: Slot = .{};
    ok_a.write(0, RocList, i64List(&.{ 1, 2 }, ops));
    ok_a.write(result.discriminant_offset, u8, 1);
    ok_b.write(0, RocList, i64List(&.{ 1, 2 }, ops));
    ok_b.write(result.discriminant_offset, u8, 1);
    err.write(result.discriminant_offset, u8, 0);
    try testing.expect(walk.eq(result, ok_a.ptr(), ok_b.ptr(), ops));
    try testing.expect(!walk.eq(result, ok_a.ptr(), err.ptr(), ops));
    walk.decref(result, ok_a.ptr(), ops);
    walk.decref(result, ok_b.ptr(), ops);

    // A nominal whose `is_eq` says every two values are equal.
    const always = erased_callable.allocate(&Overrides.alwaysEqual, null, 0, ops);
    const methods: desc_mod.Methods = .{ .is_eq = always, .to_hash = null, .to_inspect = null };
    const nominal = env.descs.nominal(desc_mod.scalarDesc(.i64), &methods, false);
    lhs.write(0, i64, 1);
    rhs.write(0, i64, 2);
    try testing.expect(walk.eq(nominal, lhs.ptr(), rhs.ptr(), ops));
    try testing.expect(!walk.eq(desc_mod.scalarDesc(.i64), lhs.ptr(), rhs.ptr(), ops));
    erased_callable.decref(always, ops);
}

const Overrides = struct {
    fn alwaysEqual(_: *RocOps, ret: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {
        ret.?[0] = 1;
    }

    fn hashToSeven(_: *RocOps, ret: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {
        @as(*align(1) u64, @ptrCast(ret.?)).* = 7;
    }

    fn inspectAsCustom(_: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {
        const value = @as(*align(1) const i64, @ptrCast(args.?)).*;
        @as(*align(1) RocStr, @ptrCast(ret.?)).* = RocStr.fromSliceSmall(if (value == 4) "four" else "other");
    }
};

test "hashing feeds the hasher builtins in structural order" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const seed: u64 = 0x1234;
    const D = hash_mod.HasherDomain;

    const record = recordAB(&env, desc_mod.scalarDesc(.u8), desc_mod.strDesc());
    var slot: Slot = .{};
    slot.write(fieldOffset(record, 0), u8, 9);
    slot.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("hi"));
    const by_hand = hash_mod.hasher_write_bytes(hash_mod.hasher_write_u64(seed, @backingInt(D.u8), 9, 1), @backingInt(D.str), "hi", 2);
    try testing.expectEqual(by_hand, walk.hash(record, seed, slot.ptr(), ops));

    const result = env.descs.tagUnion(&.{
        .{ .name = &name_ok, .payloads = &.{desc_mod.scalarDesc(.i32)} },
        .{ .name = &name_err, .payloads = &.{} },
    });
    var ok: Slot = .{};
    ok.write(0, i32, -1);
    ok.write(result.discriminant_offset, u8, 1);
    const ok_by_hand = hash_mod.hasher_write_u64(hash_mod.hasher_write_u64(seed, @backingInt(D.u64), 1, 8), @backingInt(D.i32), 0xffff_ffff, 4);
    try testing.expectEqual(ok_by_hand, walk.hash(result, seed, ok.ptr(), ops));

    const list_desc = env.descs.list(desc_mod.scalarDesc(.u16));
    var list_slot: Slot = .{};
    const list = RocList.list_allocate(2, 2, 2, false, ops);
    @as([*]u16, @ptrCast(@alignCast(list.bytes.?)))[0..2].* = .{ 3, 4 };
    list_slot.write(0, RocList, list);
    const list_by_hand = hash_mod.hasher_write_u64(
        hash_mod.hasher_write_u64(hash_mod.hasher_write_u64(seed, @backingInt(D.u64), 2, 8), @backingInt(D.u16), 3, 2),
        @backingInt(D.u16),
        4,
        2,
    );
    try testing.expectEqual(list_by_hand, walk.hash(list_desc, seed, list_slot.ptr(), ops));
    walk.decref(list_desc, list_slot.ptr(), ops);

    const seven = erased_callable.allocate(&Overrides.hashToSeven, null, 0, ops);
    const methods: desc_mod.Methods = .{ .is_eq = null, .to_hash = seven, .to_inspect = null };
    const nominal = env.descs.nominal(desc_mod.scalarDesc(.i64), &methods, false);
    try testing.expectEqual(@as(u64, 7), walk.hash(nominal, seed, slot.ptr(), ops));
    erased_callable.decref(seven, ops);
}

test "nominal inspection uses the override, hides opaque structure, or shows the backing" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const custom = erased_callable.allocate(&Overrides.inspectAsCustom, null, 0, ops);
    const methods: desc_mod.Methods = .{ .is_eq = null, .to_hash = null, .to_inspect = custom };
    var slot: Slot = .{};
    slot.write(0, i64, 4);
    try expectInspect(&env, env.descs.nominal(desc_mod.scalarDesc(.i64), &methods, false), slot.ptr(), "four");
    try expectInspect(&env, env.descs.nominal(desc_mod.scalarDesc(.i64), null, true), slot.ptr(), "<opaque>");
    try expectInspect(&env, env.descs.nominal(desc_mod.scalarDesc(.i64), null, false), slot.ptr(), "4");
    erased_callable.decref(custom, ops);
}

test "the shared capture drop releases a dynamic capture record" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const captures = env.descs.args(&.{ desc_mod.scalarDesc(.opaque_ptr), desc_mod.strDesc() });
    const baseline = env.allocations();

    const callable = erased_callable.allocate(&Overrides.alwaysEqual, &walk.captureDrop, captures.size, ops);
    const capture = erased_callable.capturePtr(callable);
    @as(*?*const anyopaque, @ptrCast(@alignCast(capture))).* = captures;
    @as(*RocStr, @ptrCast(@alignCast(capture + captures.fields()[1].offset))).* = bigStr("a captured string long enough for the heap", ops);
    try testing.expectEqual(baseline + 2, env.allocations());

    var slot: Slot = .{};
    slot.write(0, ?[*]u8, callable);
    walk.incref(desc_mod.erasedCallableDesc(), slot.ptr(), 2, ops);
    for (0..2) |_| {
        walk.decref(desc_mod.erasedCallableDesc(), slot.ptr(), ops);
        try testing.expectEqual(baseline + 2, env.allocations());
    }
    walk.decref(desc_mod.erasedCallableDesc(), slot.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "list element callbacks and C entry points reach the entered host" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const saved = builtins.in_process_host.enter(ops, null);
    defer builtins.in_process_host.leave(saved);

    const str_desc = desc_mod.strDesc();
    var element = bigStr("an element string long enough for the heap", ops);
    const baseline = env.allocations();
    walk.elemIncref(@ptrCast(@constCast(str_desc)), @ptrCast(&element));
    walk.elemDecref(@ptrCast(@constCast(str_desc)), @ptrCast(&element));
    try testing.expectEqual(baseline, env.allocations());
    exports.roc_desc_decref(str_desc, @ptrCast(&element));
    try testing.expectEqual(baseline - 1, env.allocations());

    var rendered: RocStr = undefined;
    var byte: u8 = 7;
    exports.roc_desc_inspect(&rendered, desc_mod.scalarDesc(.u8), null, @ptrCast(&byte));
    try testing.expectEqualStrings("7", rendered.asSlice());
}

test "tag construction by discriminant reaches every variant of structural and nominal descriptors" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var labels: [257][6]u8 = undefined;
    var names: [257]Name = undefined;
    var variants: [257]test_descs.VariantInput = undefined;
    const payloads = [_]*const TypeDesc{desc_mod.scalarDesc(.i64)};
    for (&labels, &names, 0..) |*label, *tag_name, index| {
        const text = try std.fmt.bufPrint(label, "Tag{d:0>3}", .{index});
        tag_name.* = .{ .id = desc_mod.labelId(text), .len = @intCast(text.len), .bytes = text.ptr };
    }
    for ([_]usize{ 1, 2, 3, 4, 17, 257 }) |count| {
        // Composition must establish sorted order from arbitrary input order.
        for (variants[0..count], 0..) |*variant, index| {
            variant.* = .{ .name = &names[count - index - 1], .payloads = &payloads };
        }
        const union_desc = env.descs.tagUnion(variants[0..count]);
        const nominal = env.descs.nominal(union_desc, null, false);
        for ([_]*const TypeDesc{ union_desc, nominal }) |desc| {
            for (names[0..count], 0..) |tag_name, index| {
                // Composition sorts the variants, so a tag's discriminant
                // is its name's rank.
                _ = tag_name;
                const value: i64 = @intCast(index + 1);
                var slot: Slot = .{};
                compiled.buildTag(desc, @intCast(index), slot.ptr(), &.{@ptrCast(&value)}, ops);
                try testing.expectEqual(@as(u32, @intCast(index)), compiled.tagDiscriminant(desc, slot.ptr(), ops));
                const result: *const i64 = @ptrCast(@alignCast(compiled.tagPayloadAddr(desc, slot.ptr(), 0, ops)));
                try testing.expectEqual(value, result.*);
            }
        }
    }
}

/// A re-tag plan from `from` to `to` whose entries are `targets`.
fn retagPlan(from: *const TypeDesc, to: *const TypeDesc, targets: []const desc_mod.AdaptVariant) desc_mod.AdaptPlan {
    return .{ .kind = .retag, .count = @intCast(targets.len), .discriminant = 0, .from = from, .to = to, .child = null, .entries = targets.ptr };
}

test "adapting follows a re-tag plan between a row holding False and True and Bool" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const name_false = name("False");
    const name_true = name("True");
    const name_maybe = name("Maybe");
    const just_false = env.descs.tagUnion(&.{.{ .name = &name_false, .payloads = &.{} }});
    const just_true = env.descs.tagUnion(&.{.{ .name = &name_true, .payloads = &.{} }});
    const wider = env.descs.tagUnion(&.{
        .{ .name = &name_false, .payloads = &.{} },
        .{ .name = &name_maybe, .payloads = &.{} },
        .{ .name = &name_true, .payloads = &.{} },
    });
    const absent = desc_mod.AdaptVariant.absent;
    const to_false = [_]desc_mod.AdaptVariant{.{ .discriminant = 0, .plan = null }};
    const to_true = [_]desc_mod.AdaptVariant{.{ .discriminant = 1, .plan = null }};
    const wider_to_bool = [_]desc_mod.AdaptVariant{ .{ .discriminant = 0, .plan = null }, .{ .discriminant = absent, .plan = null }, .{ .discriminant = 1, .plan = null } };
    const from_false = retagPlan(just_false, desc_mod.boolDesc(), &to_false);
    const from_true = retagPlan(just_true, desc_mod.boolDesc(), &to_true);
    const from_wider = retagPlan(wider, desc_mod.boolDesc(), &wider_to_bool);

    // A row's own discriminant is not Bool's byte: its tags sort around others.
    for ([_]struct { plan: *const desc_mod.AdaptPlan, discriminant: u32, byte: u8 }{
        .{ .plan = &from_false, .discriminant = 0, .byte = 0 },
        .{ .plan = &from_true, .discriminant = 0, .byte = 1 },
        .{ .plan = &from_wider, .discriminant = 0, .byte = 0 },
        .{ .plan = &from_wider, .discriminant = 2, .byte = 1 },
    }) |case| {
        var source: Slot = .{};
        compiled.buildTag(case.plan.from.?, case.discriminant, source.ptr(), &.{}, ops);
        var target: Slot = .{};
        target.bytes[0] = 0xff;
        compiled.adapt(case.plan, target.ptr(), source.ptr(), ops);
        try testing.expectEqual(case.byte, target.bytes[0]);
    }

    const bool_to_wider = [_]desc_mod.AdaptVariant{ .{ .discriminant = 0, .plan = null }, .{ .discriminant = 2, .plan = null } };
    const to_wider = retagPlan(desc_mod.boolDesc(), wider, &bool_to_wider);
    for ([_]struct { discriminant: u32, byte: u8 }{
        .{ .discriminant = 0, .byte = 0 },
        .{ .discriminant = 2, .byte = 1 },
    }) |case| {
        var source: Slot = .{};
        source.bytes[0] = case.byte;
        var target: Slot = .{};
        compiled.adapt(&to_wider, target.ptr(), source.ptr(), ops);
        try testing.expectEqual(case.discriminant, compiled.tagDiscriminant(wider, target.ptr(), ops));
    }
}

test "adapting follows a record plan's offsets and wraps, unwraps, and fills presence slots" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const byte = desc_mod.scalarDesc(.u8);
    const wide = desc_mod.scalarDesc(.u64);
    const name_missing = name("#Missing");
    const name_present = name("#Present");
    const optional_byte = env.descs.presenceSlot(&name_missing, &name_present, byte);
    // `{ a : U8, b : U64 }` to `{ a : U8?, b : U64, c : U8? }`, laid out
    // differently.
    const from = recordAB(&env, byte, wide);
    const to = env.descs.record(&.{
        .{ .name = &name_a, .desc = optional_byte },
        .{ .name = &name_b, .desc = wide },
        .{ .name = &name_c, .desc = optional_byte },
    });
    const copy_byte: desc_mod.AdaptPlan = .{ .kind = .copy, .count = 0, .discriminant = 0, .from = null, .to = byte, .child = null, .entries = null };
    const copy_wide: desc_mod.AdaptPlan = .{ .kind = .copy, .count = 0, .discriminant = 0, .from = null, .to = wide, .child = null, .entries = null };
    const wrap: desc_mod.AdaptPlan = .{ .kind = .wrap, .count = 0, .discriminant = 1, .from = null, .to = optional_byte, .child = &copy_byte, .entries = null };
    const missing: desc_mod.AdaptPlan = .{ .kind = .missing, .count = 0, .discriminant = 0, .from = null, .to = optional_byte, .child = null, .entries = null };
    const to_a = to.fieldBySemanticIndex(0);
    const to_b = to.fieldBySemanticIndex(1);
    const to_c = to.fieldBySemanticIndex(2);
    const from_a = from.fieldBySemanticIndex(0);
    const from_b = from.fieldBySemanticIndex(1);
    const fields = [_]desc_mod.AdaptField{
        .{ .to_offset = to_a.offset, .from_offset = from_a.offset, .plan = &wrap },
        .{ .to_offset = to_b.offset, .from_offset = from_b.offset, .plan = &copy_wide },
        .{ .to_offset = to_c.offset, .from_offset = 0, .plan = &missing },
    };
    const plan: desc_mod.AdaptPlan = .{ .kind = .record, .count = fields.len, .discriminant = 0, .from = from, .to = to, .child = null, .entries = &fields };
    const a: u8 = 7;
    const b: u64 = 0x0102030405060708;
    var source: Slot = .{};
    compiled.buildRecord(from, source.ptr(), &.{ @ptrCast(&a), @ptrCast(&b) }, ops);
    var target: Slot = .{ .bytes = @splat(0xa5) };
    compiled.adapt(&plan, target.ptr(), source.ptr(), ops);
    try testing.expectEqual(@as(u32, 1), compiled.tagDiscriminant(optional_byte, target.ptr() + to_a.offset, ops));
    try testing.expectEqual(a, compiled.tagPayloadAddr(optional_byte, target.ptr() + to_a.offset, 0, ops)[0]);
    try testing.expectEqual(b, @as(*align(1) const u64, @ptrCast(target.ptr() + to_b.offset)).*);
    try testing.expectEqual(@as(u32, 0), compiled.tagDiscriminant(optional_byte, target.ptr() + to_c.offset, ops));

    // Back: the present slot unwraps to its payload.
    const unwrap: desc_mod.AdaptPlan = .{ .kind = .unwrap, .count = 0, .discriminant = 1, .from = optional_byte, .to = null, .child = &copy_byte, .entries = null };
    var unwrapped: Slot = .{};
    compiled.adapt(&unwrap, unwrapped.ptr(), target.ptr() + to_a.offset, ops);
    try testing.expectEqual(a, unwrapped.bytes[0]);
}

test "single-payload tag construction initializes the exact byte image and releases boxed payloads" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var labels: [257][6]u8 = undefined;
    var names: [257]Name = undefined;
    var variants: [257]test_descs.VariantInput = undefined;
    for (&labels, &names, 0..) |*label, *tag_name, index| {
        const text = try std.fmt.bufPrint(label, "Tag{d:0>3}", .{index});
        tag_name.* = .{ .id = desc_mod.labelId(text), .len = @intCast(text.len), .bytes = text.ptr };
    }
    const wide = desc_mod.scalarDesc(.i128);
    const wide_payload = [_]*const TypeDesc{wide};
    const padded_record = recordAB(&env, desc_mod.scalarDesc(.u8), desc_mod.scalarDesc(.u64));
    const boxed_int = env.descs.recursiveBox(desc_mod.scalarDesc(.i64));
    const boxed_str = env.descs.recursiveBox(desc_mod.strDesc());
    for ([_]*const TypeDesc{ desc_mod.zstDesc(), desc_mod.scalarDesc(.u8), desc_mod.scalarDesc(.u64), wide, padded_record, boxed_int, boxed_str }) |payload| {
        const payloads = [_]*const TypeDesc{payload};
        // No discriminant, a one-byte discriminant, and a two-byte one.
        for ([_]usize{ 1, 2, 257 }) |count| {
            for (variants[0..count], 0..) |*variant, index| {
                variant.* = .{ .name = &names[index], .payloads = if (index + 1 == count) &payloads else &wide_payload };
            }
            const desc = env.descs.tagUnion(variants[0..count]);
            const allocations = env.allocations();
            var source: Slot = .{ .bytes = @splat(0x5a) };
            if (payload == boxed_str) source.write(0, RocStr, bigStr("a boxed payload with independently allocated string bytes", ops));
            var actual: Slot = .{ .bytes = @splat(0xa5) };
            compiled.buildTag(desc, @intCast(count - 1), actual.ptr(), &.{source.ptr()}, ops);

            var expected: Slot = .{ .bytes = @splat(0xa5) };
            @memset(expected.bytes[0..desc.size], 0);
            if (payload.kind == .recursive_box) {
                // The allocated pointer is unpredictable, but its entire slot
                // and the bytes of its separately allocated payload are exact.
                @memcpy(expected.bytes[0..payload.size], actual.bytes[0..payload.size]);
                const contents = compiled.componentAddr(payload, actual.ptr());
                try testing.expectEqualSlices(u8, source.bytes[0..payload.child.?.size], contents[0..payload.child.?.size]);
            } else {
                @memcpy(expected.bytes[0..payload.size], source.bytes[0..payload.size]);
            }
            for (0..desc.discriminant_size) |byte| {
                expected.bytes[desc.discriminant_offset + byte] = @truncate(@as(u64, count - 1) >> @intCast(byte * 8));
            }
            // Includes padding, unused variant space, and untouched bytes
            // beyond the destination, which started with a nonzero poison.
            try testing.expectEqualSlices(u8, &expected.bytes, &actual.bytes);
            walk.decref(desc, actual.ptr(), ops);
            try testing.expectEqual(allocations, env.allocations());
        }
    }
}

test "reference field tables select physical slots and exclude scalars and padding" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const text = desc_mod.strDesc();
    const number = desc_mod.scalarDesc(.u64);
    const structural = env.descs.record(&.{
        .{ .name = &name_a, .desc = text },
        .{ .name = &name_b, .desc = number },
        .{ .name = &name_c, .desc = text },
    });
    const tuple = env.descs.tuple(&.{ text, number, text });
    const args = env.descs.args(&.{ text, number, text });
    const declared = env.descs.declaredRecord(&.{
        .{ .name = &name_b, .desc = number, .is_padding = false },
        .{ .name = null, .desc = text, .is_padding = true },
        .{ .name = &name_a, .desc = text, .is_padding = false },
        .{ .name = &name_c, .desc = text, .is_padding = false },
    });
    const cases = [_]struct { desc: *const TypeDesc, slots: []const u32 }{
        .{ .desc = structural, .slots = &.{ 1, 2 } },
        .{ .desc = tuple, .slots = &.{ 1, 2 } },
        .{ .desc = args, .slots = &.{ 0, 2 } },
        .{ .desc = declared, .slots = &.{ 2, 3 } },
    };
    for (cases) |case| {
        try testing.expectEqualSlices(u32, case.slots, case.desc.refcountedFieldSlots());
        const baseline = env.allocations();
        var value: Slot = .{};
        // Padding is deliberately invalid as a string: it must not be visited.
        @memset(value.bytes[0..case.desc.size], 0xaa);
        for (case.slots) |index| {
            value.write(case.desc.fields()[index].offset, RocStr, bigStr("a heap string in a selected reference field", ops));
        }
        walk.incref(case.desc, value.ptr(), 1, ops);
        walk.decref(case.desc, value.ptr(), ops);
        try testing.expectEqual(baseline + 2, env.allocations());
        walk.decref(case.desc, value.ptr(), ops);
        try testing.expectEqual(baseline, env.allocations());
    }
    const plain = env.descs.args(&.{ number, desc_mod.boolDesc(), desc_mod.zstDesc() });
    try testing.expectEqual(@as(usize, 0), plain.refcountedFieldSlots().len);
    const empty = env.descs.tuple(&.{});
    try testing.expectEqual(@as(usize, 0), empty.refcountedFieldSlots().len);
}

test "reference field tables remain exact when recursive placeholders close" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const shell = env.descs.recursiveOpen();
    const box = env.descs.recursiveBox(shell);
    const node = env.descs.args(&.{ desc_mod.scalarDesc(.u64), box });
    try testing.expectEqualSlices(u32, &.{1}, node.refcountedFieldSlots());
    const closed = env.descs.recursiveClose(shell, node);
    try testing.expectEqualSlices(u32, &.{1}, closed.refcountedFieldSlots());
    const baseline = env.allocations();
    const child = utils.allocateWithRefcount(closed.size, closed.alignment, true, ops);
    @memset(child[0..closed.size], 0);
    var value: Slot = .{};
    value.write(fieldOffset(closed, 1), ?[*]u8, child);
    walk.incref(closed, value.ptr(), 1, ops);
    walk.decref(closed, value.ptr(), ops);
    try testing.expectEqual(baseline + 1, env.allocations());
    walk.decref(closed, value.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "active tag payloads preserve physical ordering, zero size, and recursive boxes" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const boxed = env.descs.recursiveBox(desc_mod.strDesc());
    const payloads = [_]*const TypeDesc{ desc_mod.scalarDesc(.u8), desc_mod.zstDesc(), boxed, desc_mod.scalarDesc(.u64) };
    const union_desc = env.descs.tagUnion(&.{
        .{ .name = &name_a, .payloads = &.{} },
        .{ .name = &name_b, .payloads = &payloads },
    });
    const desc = env.descs.nominal(union_desc, null, false);
    const baseline = env.allocations();
    const small: u8 = 37;
    const large: u64 = 0x123456789abcdef0;
    const text = bigStr("a recursively boxed payload selected by the active tag", ops);
    var value: Slot = .{};
    compiled.buildTag(desc, 1, value.ptr(), &.{ @ptrCast(&small), walk.zeroSizedPtr(), @ptrCast(&text), @ptrCast(&large) }, ops);
    try testing.expectEqual(small, compiled.tagPayloadAddr(desc, value.ptr(), 0, ops)[0]);
    try testing.expectEqual(value.ptr() + fieldOffset(union_desc.variants()[1].payload, 1), compiled.tagPayloadAddr(desc, value.ptr(), 1, ops));
    const loaded: *const RocStr = @ptrCast(@alignCast(compiled.tagPayloadAddr(desc, value.ptr(), 2, ops)));
    try testing.expectEqualStrings(text.asSlice(), loaded.asSlice());
    const loaded_large: *const u64 = @ptrCast(@alignCast(compiled.tagPayloadAddr(desc, value.ptr(), 3, ops)));
    try testing.expectEqual(large, loaded_large.*);
    walk.decref(desc, value.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "record construction initializes gaps and omitted fields without touching surrounding bytes" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const byte = desc_mod.scalarDesc(.u8);
    const wide = desc_mod.scalarDesc(.u64);
    const zero = desc_mod.zstDesc();
    const declared = env.descs.declaredRecord(&.{
        .{ .name = null, .desc = byte, .is_padding = true },
        .{ .name = &name_c, .desc = byte, .is_padding = false },
        .{ .name = &name_b, .desc = wide, .is_padding = false },
        .{ .name = null, .desc = byte, .is_padding = true },
        .{ .name = &name_a, .desc = zero, .is_padding = false },
        .{ .name = null, .desc = byte, .is_padding = true },
    });
    const descriptors = [_]*const TypeDesc{
        env.descs.record(&.{}),
        env.descs.tuple(&.{ zero, zero }),
        env.descs.tuple(&.{ wide, wide }),
        env.descs.tuple(&.{ byte, wide, byte }),
        recordAB(&env, byte, wide),
        declared,
        env.descs.nominal(declared, null, false),
    };
    var sources = [_]Slot{
        .{ .bytes = @splat(0x31) },
        .{ .bytes = @splat(0x62) },
        .{ .bytes = @splat(0x93) },
    };
    const values = [_][*]const u8{ sources[0].ptr(), sources[1].ptr(), sources[2].ptr() };
    for (descriptors) |desc| {
        const record = compiled.backing(desc);
        var named_count: usize = 0;
        for (record.fields()) |field| named_count += @intFromBool(field.is_padding == 0);
        // The constructor also supports a prefix of semantic fields. All
        // absent fields, including explicit padding, must remain zero.
        for (0..named_count + 1) |count| {
            var actual: Slot = .{ .bytes = @splat(0xa5) };
            var expected: Slot = .{ .bytes = @splat(0xa5) };
            @memset(expected.bytes[0..desc.size], 0);
            for (values[0..count], 0..) |src, index| {
                const field = record.fieldBySemanticIndex(@intCast(index));
                @memcpy(expected.bytes[field.offset..][0..field.desc.size], src[0..field.desc.size]);
            }
            compiled.buildRecord(desc, actual.ptr(), values[0..count], ops);
            try testing.expectEqualSlices(u8, &expected.bytes, &actual.bytes);
        }
    }
}

test "record construction and updates preserve boxed fields and initialize all padding" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const byte = desc_mod.scalarDesc(.u8);
    const boxed = env.descs.recursiveBox(desc_mod.strDesc());
    const record = env.descs.declaredRecord(&.{
        .{ .name = null, .desc = byte, .is_padding = true },
        .{ .name = &name_b, .desc = boxed, .is_padding = false },
        .{ .name = &name_a, .desc = byte, .is_padding = false },
        .{ .name = null, .desc = byte, .is_padding = true },
    });
    const desc = env.descs.nominal(record, null, false);
    const allocations = env.allocations();
    const text = "a heap string moved into a recursive record field";
    const source = bigStr(text, ops);
    const initial: u8 = 42;
    const changed: u8 = 91;
    var base: Slot = .{ .bytes = @splat(0xa5) };
    compiled.buildRecord(desc, base.ptr(), &.{ @ptrCast(&initial), @ptrCast(&source) }, ops);
    var actual: Slot = .{ .bytes = @splat(0xb6) };
    // Updating another field must share the existing box before releasing
    // the old record; changing the boxed field must move the new string.
    // Memory slots: padding, `b`, `a`, padding.
    const flag = desc_mod.update_value_flag;
    compiled.updateRecord(desc, base.ptr(), desc, actual.ptr(), &.{ 0, 1, flag, 0 }, &.{@ptrCast(&changed)}, ops);
    const replacement = bigStr("a replacement heap string in the recursive record field", ops);
    var replaced: Slot = .{ .bytes = @splat(0xc7) };
    compiled.updateRecord(desc, actual.ptr(), desc, replaced.ptr(), &.{ 0, flag, 2, 0 }, &.{@ptrCast(&replacement)}, ops);
    const field_a = record.fieldBySemanticIndex(0);
    const field_b = record.fieldBySemanticIndex(1);
    var expected: Slot = .{ .bytes = @splat(0xc7) };
    @memset(expected.bytes[0..desc.size], 0);
    expected.bytes[field_a.offset] = changed;
    @memcpy(expected.bytes[field_b.offset..][0..boxed.size], replaced.bytes[field_b.offset..][0..boxed.size]);
    try testing.expectEqualSlices(u8, &expected.bytes, &replaced.bytes);
    const stored: *const RocStr = @ptrCast(@alignCast(compiled.componentAddr(boxed, replaced.ptr() + field_b.offset)));
    try testing.expectEqualStrings(replacement.asSlice(), stored.asSlice());
    walk.decref(desc, replaced.ptr(), ops);
    try testing.expectEqual(allocations, env.allocations());
}

test "frame runs place each local at its alignment and report the run end" {
    const u8_desc = desc_mod.scalarDesc(.u8);
    const u64_desc = desc_mod.scalarDesc(.u64);
    try testing.expectEqual(@as(usize, 3), exports.roc_desc_frame_run_size(u8_desc, 3, 0));
    // A u64 run after three bytes starts at the next multiple of eight.
    try testing.expectEqual(@as(usize, 24), exports.roc_desc_frame_run_size(u64_desc, 2, 3));
    var region: [32]u8 align(16) = undefined;
    var slots: [3][*]u8 = undefined;
    const bytes_end = exports.roc_desc_frame_place_run(u8_desc, 3, &region, 0, &slots);
    try testing.expectEqual(@as(usize, 3), bytes_end);
    try testing.expectEqual(@as([*]u8, &region), slots[0]);
    try testing.expectEqual(@as([*]u8, region[2..]), slots[2]);
    var words: [2][*]u8 = undefined;
    const words_end = exports.roc_desc_frame_place_run(u64_desc, 2, &region, bytes_end, &words);
    try testing.expectEqual(@as(usize, 24), words_end);
    try testing.expectEqual(@as([*]u8, region[8..]), words[0]);
    try testing.expectEqual(@as([*]u8, region[16..]), words[1]);
}

/// How many times `needle` occurs in `haystack`.
fn occurrences(haystack: []const u8, needle: []const u8) usize {
    var count: usize = 0;
    var start: usize = 0;
    while (std.mem.indexOfPos(u8, haystack, start, needle)) |at| {
        count += 1;
        start = at + needle.len;
    }
    return count;
}

test "the runtime reads names only to print them" {
    // Every operation on values reaches a field or variant by the position
    // the compiler computed (`compiled.zig`, `exports.zig`); a name's text
    // is read in one place, inspect's printing of a label (`walk.zig`).
    for ([_][]const u8{ @embedFile("compiled.zig"), @embedFile("exports.zig") }) |source| {
        for ([_][]const u8{ "desc_mod.Name;", "const Name)", "const Name,", ".name", ".text()", ".id[", "std.mem.eql(u8", "std.mem.order(u8" }) |forbidden| {
            try testing.expectEqual(@as(usize, 0), occurrences(source, forbidden));
        }
    }
    const walk_source = @embedFile("walk.zig");
    try testing.expectEqual(@as(usize, 1), occurrences(walk_source, ".text()"));
    try testing.expectEqual(@as(usize, 1), occurrences(walk_source, ".label => |name| out.append(name.text(), ops)"));
    for ([_][]const u8{ ".id[", "std.mem.eql(u8", "std.mem.order(u8" }) |forbidden| {
        try testing.expectEqual(@as(usize, 0), occurrences(walk_source, forbidden));
    }
    // A name has no equality to compare by.
    try testing.expect(!@hasDecl(Name, "eql"));
}
