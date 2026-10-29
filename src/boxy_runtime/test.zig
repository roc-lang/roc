//! Tests of descriptor composition, interning, and the walkers.

const std = @import("std");
const builtins = @import("builtins");
const shape = @import("layout_shape");
const desc_mod = @import("desc.zig");
const compose = @import("compose.zig");
const walk = @import("walk.zig");
const exports = @import("exports.zig");

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
    return .{ .id = desc_mod.labelId(text), .len = text.len, .bytes = text.ptr };
}

const name_a = name("a");
const name_b = name("b");
const name_c = name("c");
const name_ok = name("Ok");
const name_err = name("Err");
const name_point = name("Point");

/// The errors these tests' helpers return.
const TestError = std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult };

/// A test host: allocation tracking plus a fresh composition cache.
const Env = struct {
    test_env: TestEnv,
    cache: compose.Cache = .{},

    fn init() Env {
        return .{ .test_env = TestEnv.init(testing.allocator) };
    }

    fn deinit(self: *Env) void {
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
    const rendered = walk.inspect(d, ptr, env.ops());
    defer rendered.decref(env.ops());
    try testing.expectEqualStrings(expected, rendered.asSlice());
}

fn recordAB(env: *Env, a: *const TypeDesc, b: *const TypeDesc) *const TypeDesc {
    return compose.record(&env.cache, env.ops(), &.{ .{ .name = &name_b, .desc = b }, .{ .name = &name_a, .desc = a } });
}

test "composing again returns the same descriptor and allocates nothing" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const u64_desc = compose.scalarDesc(.u64);
    const list = compose.list(&env.cache, ops, u64_desc);
    const record = recordAB(&env, u64_desc, compose.strDesc());
    const union_desc = compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 2, .payloads = &.{ list, record } },
        .{ .name = &name_err, .payload_count = 0, .payloads = &.{} },
    });
    const nominal = compose.nominal(&env.cache, ops, record, &name_point, null, false);
    const after_first = env.allocations();

    try testing.expectEqual(list, compose.list(&env.cache, ops, u64_desc));
    try testing.expectEqual(record, recordAB(&env, u64_desc, compose.strDesc()));
    try testing.expectEqual(union_desc, compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 2, .payloads = &.{ list, record } },
        .{ .name = &name_err, .payload_count = 0, .payloads = &.{} },
    }));
    try testing.expectEqual(nominal, compose.nominal(&env.cache, ops, record, &name_point, null, false));
    try testing.expectEqual(after_first, env.allocations());

    try testing.expect(compose.list(&env.cache, ops, compose.scalarDesc(.u32)) != list);
}

test "many compositions grow the cache and stay interned" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var lists: [200]*const TypeDesc = undefined;
    var current = compose.scalarDesc(.u8);
    for (&lists) |*slot| {
        current = compose.list(&env.cache, ops, current);
        slot.* = current;
    }
    current = compose.scalarDesc(.u8);
    for (lists) |expected| {
        current = compose.list(&env.cache, ops, current);
        try testing.expectEqual(expected, current);
    }
}

test "a record lays out by descending alignment, then field name" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const record = compose.record(&env.cache, ops, &.{
        .{ .name = &name_c, .desc = compose.scalarDesc(.u8) },
        .{ .name = &name_a, .desc = compose.scalarDesc(.u8) },
        .{ .name = &name_b, .desc = compose.scalarDesc(.u64) },
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
    const args = compose.args(&env.cache, env.ops(), &.{ compose.scalarDesc(.u8), compose.scalarDesc(.u64), compose.scalarDesc(.u8) });
    try testing.expectEqual(@as(u32, 0), args.fields()[0].offset);
    try testing.expectEqual(@as(u32, 8), args.fields()[1].offset);
    try testing.expectEqual(@as(u32, 16), args.fields()[2].offset);
    try testing.expectEqual(@as(u32, 24), args.size);
    try testing.expect(args.flags.declared_order);
}

test "zero-sized records keep their kind with the zero-sized shape" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const empty = compose.record(&env.cache, ops, &.{});
    const nested = compose.record(&env.cache, ops, &.{.{ .name = &name_a, .desc = empty }});
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
    const i64_desc = compose.scalarDesc(.i64);
    const str_desc = compose.strDesc();

    const record = recordAB(&env, i64_desc, str_desc);
    var record_slot: Slot = .{};
    record_slot.write(fieldOffset(record, 0), i64, 1);
    record_slot.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    try expectInspect(&env, record, record_slot.ptr(), "{ a: 1, b: \"x\" }");

    const pair = compose.tuple(&env.cache, ops, &.{ i64_desc, str_desc });
    var pair_slot: Slot = .{};
    pair_slot.write(fieldOffset(pair, 0), i64, 1);
    pair_slot.write(fieldOffset(pair, 1), RocStr, RocStr.fromSliceSmall("a"));
    try expectInspect(&env, pair, pair_slot.ptr(), "(1, \"a\")");

    const list_desc = compose.list(&env.cache, ops, i64_desc);
    const result = compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 1, .payloads = &.{list_desc} },
        .{ .name = &name_err, .payload_count = 1, .payloads = &.{str_desc} },
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

    const two_args = compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 2, .payloads = &.{ str_desc, i64_desc } },
    });
    var two_slot: Slot = .{};
    two_slot.write(fieldOffset(two_args.variants()[0].payload, 0), RocStr, RocStr.fromSliceSmall("s"));
    two_slot.write(fieldOffset(two_args.variants()[0].payload, 1), i64, -7);
    try expectInspect(&env, two_args, two_slot.ptr(), "Ok(\"s\", -7)");

    const records = compose.list(&env.cache, ops, record);
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

    const nested = compose.list(&env.cache, ops, list_desc);
    const outer = RocList.list_allocate(list_desc.alignment, 2, list_desc.size, true, ops);
    const inner_lists = [_]RocList{ i64List(&.{ 1, 2 }, ops), RocList.empty() };
    @memcpy(@as([*]RocList, @ptrCast(@alignCast(outer.bytes.?)))[0..2], &inner_lists);
    var nested_slot: Slot = .{};
    nested_slot.write(0, RocList, outer);
    try expectInspect(&env, nested, nested_slot.ptr(), "[[1, 2], []]");
    walk.decref(nested, nested_slot.ptr(), ops);

    const boxed = compose.box(&env.cache, ops, i64_desc);
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
    try expectInspect(&env, compose.scalarDesc(.f64), slot.ptr(), "1.5");
    slot.write(0, i128, RocDec.fromU64(3).num);
    try expectInspect(&env, compose.scalarDesc(.dec), slot.ptr(), "3.0");
    slot.write(0, i8, -3);
    try expectInspect(&env, compose.scalarDesc(.i8), slot.ptr(), "-3");
    slot.write(0, u128, std.math.maxInt(u128));
    try expectInspect(&env, compose.scalarDesc(.u128), slot.ptr(), "340282366920938463463374607431768211455");
    slot.write(0, u8, 1);
    try expectInspect(&env, compose.boolDesc(), slot.ptr(), "True");
    slot.write(0, u8, 0);
    try expectInspect(&env, compose.boolDesc(), slot.ptr(), "False");
    slot.write(0, RocStr, RocStr.fromSliceSmall("q\""));
    try expectInspect(&env, compose.strDesc(), slot.ptr(), "\"q\\\"\"");
    try expectInspect(&env, compose.erasedCallableDesc(), slot.ptr(), "<function>");
}

test "a long rendering grows its string" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    var values: [300]i64 = undefined;
    for (&values, 0..) |*value, index| value.* = @intCast(index);
    const list_desc = compose.list(&env.cache, ops, compose.scalarDesc(.i64));
    var slot: Slot = .{};
    slot.write(0, RocList, i64List(&values, ops));
    const rendered = walk.inspect(list_desc, slot.ptr(), ops);
    defer rendered.decref(ops);
    try testing.expect(std.mem.startsWith(u8, rendered.asSlice(), "[0, 1, 2, "));
    try testing.expect(std.mem.endsWith(u8, rendered.asSlice(), ", 298, 299]"));
    walk.decref(list_desc, slot.ptr(), ops);
}

test "decref releases nested allocations and incref keeps them alive" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const str_desc = compose.strDesc();
    const strs = compose.list(&env.cache, ops, str_desc);
    const record = recordAB(&env, compose.scalarDesc(.i64), strs);
    const boxed = compose.box(&env.cache, ops, record);
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

test "decref of a deep recursive value keeps its work off the call stack" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    // A list of lists nested 10_000 deep, each level one element.
    const depth = 10_000;
    var descs: [depth + 1]*const TypeDesc = undefined;
    descs[0] = compose.strDesc();
    for (1..depth + 1) |level| descs[level] = compose.list(&env.cache, ops, descs[level - 1]);
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

test "equality compares structure and honors is_eq overrides" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const record = recordAB(&env, compose.scalarDesc(.i64), compose.strDesc());
    var lhs: Slot = .{};
    var rhs: Slot = .{};
    lhs.write(fieldOffset(record, 0), i64, 1);
    lhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    rhs.write(fieldOffset(record, 0), i64, 1);
    rhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("x"));
    try testing.expect(walk.eq(record, lhs.ptr(), rhs.ptr(), ops));
    rhs.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("y"));
    try testing.expect(!walk.eq(record, lhs.ptr(), rhs.ptr(), ops));

    const list_desc = compose.list(&env.cache, ops, compose.scalarDesc(.i64));
    const result = compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 1, .payloads = &.{list_desc} },
        .{ .name = &name_err, .payload_count = 0, .payloads = &.{} },
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
    const nominal = compose.nominal(&env.cache, ops, compose.scalarDesc(.i64), &name_point, &methods, false);
    lhs.write(0, i64, 1);
    rhs.write(0, i64, 2);
    try testing.expect(walk.eq(nominal, lhs.ptr(), rhs.ptr(), ops));
    try testing.expect(!walk.eq(compose.scalarDesc(.i64), lhs.ptr(), rhs.ptr(), ops));
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

    const record = recordAB(&env, compose.scalarDesc(.u8), compose.strDesc());
    var slot: Slot = .{};
    slot.write(fieldOffset(record, 0), u8, 9);
    slot.write(fieldOffset(record, 1), RocStr, RocStr.fromSliceSmall("hi"));
    const by_hand = hash_mod.hasher_write_bytes(hash_mod.hasher_write_u64(seed, @backingInt(D.u8), 9, 1), @backingInt(D.str), "hi", 2);
    try testing.expectEqual(by_hand, walk.hash(record, seed, slot.ptr(), ops));

    const result = compose.tagUnion(&env.cache, ops, &.{
        .{ .name = &name_ok, .payload_count = 1, .payloads = &.{compose.scalarDesc(.i32)} },
        .{ .name = &name_err, .payload_count = 0, .payloads = &.{} },
    });
    var ok: Slot = .{};
    ok.write(0, i32, -1);
    ok.write(result.discriminant_offset, u8, 1);
    const ok_by_hand = hash_mod.hasher_write_u64(hash_mod.hasher_write_u64(seed, @backingInt(D.u64), 1, 8), @backingInt(D.i32), 0xffff_ffff, 4);
    try testing.expectEqual(ok_by_hand, walk.hash(result, seed, ok.ptr(), ops));

    const list_desc = compose.list(&env.cache, ops, compose.scalarDesc(.u16));
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
    const nominal = compose.nominal(&env.cache, ops, compose.scalarDesc(.i64), &name_point, &methods, false);
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
    try expectInspect(&env, compose.nominal(&env.cache, ops, compose.scalarDesc(.i64), &name_point, &methods, false), slot.ptr(), "four");
    try expectInspect(&env, compose.nominal(&env.cache, ops, compose.scalarDesc(.i64), &name_point, null, true), slot.ptr(), "<opaque>");
    try expectInspect(&env, compose.nominal(&env.cache, ops, compose.scalarDesc(.i64), &name_point, null, false), slot.ptr(), "4");
    erased_callable.decref(custom, ops);
}

test "the shared capture drop releases a dynamic capture record" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const captures = compose.args(&env.cache, ops, &.{ compose.scalarDesc(.opaque_ptr), compose.strDesc() });
    const baseline = env.allocations();

    const callable = erased_callable.allocate(&Overrides.alwaysEqual, &walk.captureDrop, captures.size, ops);
    const capture = erased_callable.capturePtr(callable);
    @as(*?*const anyopaque, @ptrCast(@alignCast(capture))).* = captures;
    @as(*RocStr, @ptrCast(@alignCast(capture + captures.fields()[1].offset))).* = bigStr("a captured string long enough for the heap", ops);
    try testing.expectEqual(baseline + 2, env.allocations());

    var slot: Slot = .{};
    slot.write(0, ?[*]u8, callable);
    walk.decref(compose.erasedCallableDesc(), slot.ptr(), ops);
    try testing.expectEqual(baseline, env.allocations());
}

test "list element callbacks and C entry points reach the entered host" {
    var env = Env.init();
    defer env.deinit();
    const ops = env.ops();
    const saved = builtins.in_process_host.enter(ops, null);
    defer builtins.in_process_host.leave(saved);

    const str_desc = compose.strDesc();
    var element = bigStr("an element string long enough for the heap", ops);
    const baseline = env.allocations();
    walk.elemIncref(@ptrCast(@constCast(str_desc)), @ptrCast(&element));
    walk.elemDecref(@ptrCast(@constCast(str_desc)), @ptrCast(&element));
    try testing.expectEqual(baseline, env.allocations());
    exports.roc_desc_decref(str_desc, @ptrCast(&element));
    try testing.expectEqual(baseline - 1, env.allocations());

    const list = exports.roc_desc_list(compose.scalarDesc(.u8));
    try testing.expectEqual(list, exports.roc_desc_list(compose.scalarDesc(.u8)));
    var rendered: RocStr = undefined;
    var byte: u8 = 7;
    exports.roc_desc_inspect(&rendered, compose.scalarDesc(.u8), @ptrCast(&byte));
    try testing.expectEqualStrings("7", rendered.asSlice());
}
