//! x86-64 C-ABI parameter/return classification for Roc layouts — both the System V
//! (Linux/macOS) eightbyte algorithm and the Windows x64 convention.
//!
//! Adapted from the Zig compiler (MIT License, "Copyright (c) Zig contributors"):
//! `src/codegen/x86_64/abi.zig` @ 24fdd5b7a4 (Release 0.16.0). The classification
//! algorithms — System V's per-eightbyte INTEGER/SSE classification with struct/union
//! recursion and post-merge cleanup, and the Win64 size-based rules — are unchanged; they
//! have been rewritten to read Roc's layout store instead of Zig's `Type`/`Zcu`.
//!
//! Roc aggregates always use a C-compatible (extern-like) layout: fields are laid out at
//! natural alignment with no `auto`/`packed` distinction, which removes the packed-struct
//! branches present in the upstream code.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;
const Layout = layout.Layout;

/// One eightbyte's class. See the System V AMD64 ABI for the full definitions.
pub const Class = enum {
    integer,
    sse,
    sseup,
    x87,
    x87up,
    none,
    memory,
    /// Win64 passes 128-bit integers in memory but returns them in an SSE register.
    win_i128,
    /// An SSE eightbyte holding a single f32.
    float,
    /// An SSE eightbyte holding two f32s.
    float_combine,

    pub const one_integer: [8]Class = .{ .integer, .none, .none, .none, .none, .none, .none, .none };
    pub const two_integers: [8]Class = .{ .integer, .integer, .none, .none, .none, .none, .none, .none };
    pub const three_integers: [8]Class = .{ .integer, .integer, .integer, .none, .none, .none, .none, .none };
    pub const four_integers: [8]Class = .{ .integer, .integer, .integer, .integer, .none, .none, .none, .none };

    pub const @"f32": [8]Class = .{ .float, .none, .none, .none, .none, .none, .none, .none };
    pub const @"f64": [8]Class = .{ .sse, .none, .none, .none, .none, .none, .none, .none };

    pub const stack: [8]Class = .{ .memory, .none, .none, .none, .none, .none, .none, .none };

    pub fn isX87(class: Class) bool {
        return switch (class) {
            .x87, .x87up => true,
            else => false,
        };
    }

    /// Combine a field's class with the eightbyte's running class (System V).
    fn combineSystemV(prev_class: Class, next_class: Class) Class {
        if (prev_class == next_class)
            return if (prev_class == .float) .float_combine else prev_class;
        if (prev_class == .none) return next_class;
        if (prev_class == .memory or next_class == .memory) return .memory;
        if (prev_class == .integer or next_class == .integer) return .integer;
        if (prev_class.isX87() or next_class.isX87()) return .memory;
        return .sse;
    }
};

/// Whether the value is being classified as an argument or a return value.
pub const Context = enum { ret, arg, other };

/// Classify under the Windows x64 calling convention.
/// "Any argument that doesn't fit in 8 bytes, or isn't 1, 2, 4, or 8 bytes, must be passed
/// by reference. … Structs and unions of size 8, 16, 32, or 64 bits … are passed as if they
/// were integers of the same size." Floats use the SSE registers.
pub fn classifyWindows(store: *const Store, idx: Idx) Class {
    const lay = store.getLayout(idx);
    const size = store.layoutSize(lay);
    std.debug.assert(size > 0);

    switch (lay.tag) {
        .scalar => {
            const scalar = lay.getScalar();
            switch (scalar.tag) {
                .frac => switch (scalar.getFrac()) {
                    .f32, .f64 => return .sse,
                    // Dec is a 128-bit integer-backed value.
                    .dec => return .win_i128,
                },
                .int, .opaque_ptr => {},
                // RocStr is a 24-byte aggregate -> by reference.
                .str => return .memory,
            }
        },
        .box, .box_of_zst, .ptr => return .integer, // single pointer
        .list, .list_of_zst => return .memory, // 24-byte aggregate
        .struct_, .tag_union, .closure, .erased_callable => {},
        .zst => unreachable,
    }

    return switch (size) {
        1, 2, 4, 8 => .integer,
        else => switch (lay.tag) {
            .scalar => .win_i128, // a >8-byte integer scalar (i128)
            else => .memory,
        },
    };
}

/// Classify under the System V AMD64 ABI. Returns up to eight eightbyte classes; unused
/// trailing slots are `.none`.
pub fn classifySystemV(store: *const Store, idx: Idx, ctx: Context) [8]Class {
    // Roc has no f16, f128, or SIMD vector types, so the only ABI types whose classification
    // depends on arg-vs-return position do not occur here; `ctx` is accepted for API
    // symmetry with the consumer (which threads it through) but is not needed.
    _ = ctx;
    const lay = store.getLayout(idx);
    const size = store.layoutSize(lay);
    std.debug.assert(size > 0);

    switch (lay.tag) {
        .scalar => {
            const scalar = lay.getScalar();
            switch (scalar.tag) {
                .opaque_ptr => return Class.one_integer,
                .int => return integerAggregateSysV(size),
                .frac => switch (scalar.getFrac()) {
                    .f32 => return Class.f32,
                    .f64 => return Class.f64,
                    .dec => return Class.two_integers, // i128-backed
                },
                // RocStr: three integer eightbytes -> exceeds 16 bytes -> memory.
                .str => return integerAggregateSysV(size),
            }
        },
        // An erased callable is a refcounted pointer to its boxed payload,
        // exactly like a box. Classifying it as an aggregate would recurse
        // forever: classifyAggregateSysV has no case for it and would route
        // it right back here.
        .box, .box_of_zst, .ptr, .erased_callable => return Class.one_integer,
        .list, .list_of_zst => return integerAggregateSysV(size),
        .struct_, .tag_union, .closure => {
            if (size > 64) return Class.stack;
            var result: [8]Class = @splat(.none);
            classifyAggregateSysV(store, &result, 0, idx);
            return finishSystemV(result, size);
        },
        .zst => unreachable,
    }
}

/// An aggregate composed entirely of integer/pointer words, classified by size. More than
/// two eightbytes of integers is passed in memory under System V's post-merge rules.
fn integerAggregateSysV(size: u32) [8]Class {
    if (size <= 8) return Class.one_integer;
    if (size <= 16) return Class.two_integers;
    return Class.stack;
}

/// Recursively classify an aggregate's eightbytes, combining each member into the eightbyte
/// it occupies. `base_offset` is the aggregate's start within the top-level value.
fn classifyAggregateSysV(store: *const Store, result: *[8]Class, base_offset: u32, idx: Idx) void {
    const lay = store.getLayout(idx);
    switch (lay.tag) {
        .struct_ => {
            const struct_idx = lay.getStruct().idx;
            const field_count = store.getStructData(struct_idx).fields.count;
            var i: u32 = 0;
            while (i < field_count) : (i += 1) {
                const field_off = base_offset + store.getStructFieldOffset(struct_idx, i);
                // Unnamed padding holds uninitialized, alignment-1 bytes — not a
                // typed member — so it classifies as INTEGER bytes (like a C
                // `char[N]`), regardless of the type it borrowed its size from.
                if (store.getStructFieldIsPadding(struct_idx, i)) {
                    classifyPaddingBytesSysV(result, field_off, store.getStructFieldSize(struct_idx, i));
                    continue;
                }
                const field_idx = store.getStructFieldLayout(struct_idx, i);
                classifyMemberSysV(store, result, field_off, field_idx);
            }
        },
        .tag_union => {
            const tu_idx = lay.getTagUnion().idx;
            const data = store.getTagUnionData(tu_idx);
            // Variant payloads overlap at the aggregate's start (union semantics).
            const variants = store.getTagUnionVariants(data);
            var v: usize = 0;
            while (v < variants.len) : (v += 1) {
                const payload_idx = variants.get(v).payload_layout;
                if (store.getLayout(payload_idx).tag == .zst) continue;
                classifyMemberSysV(store, result, base_offset, payload_idx);
            }
            // The discriminant is an integer placed at its offset.
            if (data.discriminant_size > 0) {
                const disc_off = base_offset + store.getTagUnionDiscriminantOffset(tu_idx);
                combineInto(result, disc_off / 8, .integer);
            }
        },
        .closure => classifyAggregateSysV(store, result, base_offset, lay.getClosure().captures_layout_idx),
        else => classifyMemberSysV(store, result, base_offset, idx),
    }
}

/// Classify a single member at `offset`: recurse into nested aggregates, otherwise combine
/// the member's own eightbyte classes into the result.
fn classifyMemberSysV(store: *const Store, result: *[8]Class, offset: u32, idx: Idx) void {
    const lay = store.getLayout(idx);
    switch (lay.tag) {
        .struct_, .tag_union, .closure => classifyAggregateSysV(store, result, offset, idx),
        else => {
            const member = classifySystemV(store, idx, .other);
            var j: usize = 0;
            while (j < member.len and member[j] != .none) : (j += 1) {
                combineInto(result, offset / 8 + j, member[j]);
            }
        },
    }
}

fn combineInto(result: *[8]Class, eightbyte: usize, class: Class) void {
    result[eightbyte] = Class.combineSystemV(result[eightbyte], class);
}

/// Classify `size` bytes of unnamed padding starting at `offset` as INTEGER,
/// marking every eightbyte the padding touches. Padding bytes are opaque
/// (alignment 1), so they behave like a C `char[N]` member for ABI purposes.
fn classifyPaddingBytesSysV(result: *[8]Class, offset: u32, size: u32) void {
    if (size == 0) return;
    var eightbyte = offset / 8;
    const last = (offset + size - 1) / 8;
    while (eightbyte <= last) : (eightbyte += 1) {
        combineInto(result, eightbyte, .integer);
    }
}

/// Apply System V's post-merge cleanup rules and return the final eightbyte classes.
fn finishSystemV(result_in: [8]Class, size: u32) [8]Class {
    var result = result_in;

    // "If one of the classes is MEMORY, the whole argument is passed in memory."
    // "If X87UP is not preceded by X87, the whole argument is passed in memory."
    for (result, 0..) |class, i| switch (class) {
        .memory => return Class.stack,
        .x87up => if (i == 0 or result[i - 1] != .x87) return Class.stack,
        else => continue,
    };

    // "If the size of the aggregate exceeds two eightbytes and the first eightbyte isn't SSE
    // or any other eightbyte isn't SSEUP, the whole argument is passed in memory."
    if (size > 16) {
        if (result[0] != .sse) return Class.stack;
        for (result[1..]) |class| {
            if (class != .sseup and class != .none) return Class.stack;
        }
    }

    // "If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE."
    for (&result, 0..) |*item, i| {
        if (item.* == .sseup) switch (result[i - 1]) {
            .sse, .sseup => continue,
            else => item.* = .sse,
        };
    }
    return result;
}

const testing = std.testing;

fn testStruct(store: *Store, field_idxs: []const Idx) std.mem.Allocator.Error!Idx {
    var fields: [16]layout.StructField = undefined;
    for (field_idxs, 0..) |field_idx, i| {
        fields[i] = .{ .index = @intCast(i), .layout = field_idx };
    }
    return store.putStructFields(fields[0..field_idxs.len]);
}

test "x86_64 SysV: scalars" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    try testing.expectEqual(Class.one_integer, classifySystemV(&store, .i32, .arg));
    try testing.expectEqual(Class.one_integer, classifySystemV(&store, .i64, .arg));
    try testing.expectEqual(Class.two_integers, classifySystemV(&store, .i128, .arg));
    try testing.expectEqual(Class.two_integers, classifySystemV(&store, .dec, .arg));
    try testing.expectEqual(Class.one_integer, classifySystemV(&store, .opaque_ptr, .arg));
    try testing.expectEqual(Class.f32, classifySystemV(&store, .f32, .arg));
    try testing.expectEqual(Class.f64, classifySystemV(&store, .f64, .arg));
}

test "x86_64 SysV: Plant and other small structs use registers" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // Plant { i32, u32 } -> one INTEGER eightbyte.
    const plant = try testStruct(&store, &.{ .i32, .u32 });
    try testing.expectEqual(Class.one_integer, classifySystemV(&store, plant, .arg));

    // Two f32s share one eightbyte and become a single SSE (float_combine) slot.
    const two_f32 = try testStruct(&store, &.{ .f32, .f32 });
    const tf = classifySystemV(&store, two_f32, .arg);
    try testing.expectEqual(Class.float_combine, tf[0]);
    try testing.expectEqual(Class.none, tf[1]);

    // { f64, f64 } -> two SSE eightbytes.
    const two_f64 = try testStruct(&store, &.{ .f64, .f64 });
    try testing.expectEqual([8]Class{ .sse, .sse, .none, .none, .none, .none, .none, .none }, classifySystemV(&store, two_f64, .arg));

    // { i64, f64 } -> INTEGER then SSE.
    const mixed = try testStruct(&store, &.{ .i64, .f64 });
    try testing.expectEqual([8]Class{ .integer, .sse, .none, .none, .none, .none, .none, .none }, classifySystemV(&store, mixed, .arg));
}

test "x86_64 SysV: unnamed padding classifies as integer bytes, not its declared type" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // { x : U64, _ : F64 } — the second eightbyte is 8 bytes of unnamed, alignment-1
    // padding. It must classify as INTEGER (like a C `char[8]`), NOT SSE as a real
    // `double` member would, so a nominal mirroring `{ uint64_t; char[8]; }` is passed
    // in two integer registers rather than one integer + one SSE register.
    const padded = try store.putNominalStructFields(&.{
        .{ .index = 0, .layout = .u64 },
        .{ .index = 1, .layout = .f64, .is_padding = true },
    });
    try testing.expectEqual(Class.two_integers, classifySystemV(&store, padded, .arg));
}

test "x86_64 SysV: large aggregates go to memory" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // RocStr / RocList are 24 bytes -> memory.
    try testing.expectEqual(Class.stack, classifySystemV(&store, .str, .arg));
    const list_idx = try store.insertLayout(Layout.list(.u8));
    try testing.expectEqual(Class.stack, classifySystemV(&store, list_idx, .arg));

    // A 24-byte all-integer struct -> memory.
    const three_words = try testStruct(&store, &.{ .i64, .i64, .i64 });
    try testing.expectEqual(Class.stack, classifySystemV(&store, three_words, .arg));
}

test "x86_64 SysV: erased callable is passed as a pointer" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const erased_callable = try store.insertLayout(Layout.erasedCallable());

    try testing.expectEqual(Class.one_integer, classifySystemV(&store, erased_callable, .arg));
}

test "x86_64 Win64: size-based classification" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    try testing.expectEqual(Class.integer, classifyWindows(&store, .i32));
    try testing.expectEqual(Class.integer, classifyWindows(&store, .i64));
    try testing.expectEqual(Class.win_i128, classifyWindows(&store, .i128));
    try testing.expectEqual(Class.win_i128, classifyWindows(&store, .dec));
    try testing.expectEqual(Class.sse, classifyWindows(&store, .f64));
    try testing.expectEqual(Class.integer, classifyWindows(&store, .opaque_ptr));

    // Plant is 8 bytes -> passed by value as an integer.
    const plant = try testStruct(&store, &.{ .i32, .u32 });
    try testing.expectEqual(Class.integer, classifyWindows(&store, plant));

    // 16-byte struct: not in {1,2,4,8} -> by reference (memory).
    const two_words = try testStruct(&store, &.{ .i64, .i64 });
    try testing.expectEqual(Class.memory, classifyWindows(&store, two_words));

    // RocStr/RocList -> memory.
    try testing.expectEqual(Class.memory, classifyWindows(&store, .str));
}
