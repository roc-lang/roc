//! Under `.boxy`, a direct call of a `List` template whose list item variable
//! takes exactly one of the fixed-size integer types calls the template's
//! pre-specialized worker for that type, whose body is static; every other
//! instantiation calls the generic worker (design.md "Pre-Specialized Integer
//! List Workers").
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy integer list call sites target their item type's worker, other instantiations the generic one" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\pick_u8 : List(U8), U64 -> Try(U8, [OutOfBounds])
            \\pick_u8 = |items, index| items.get(index)
            \\pick_i8 : List(I8), U64 -> Try(I8, [OutOfBounds])
            \\pick_i8 = |items, index| items.get(index)
            \\pick_u16 : List(U16), U64 -> Try(U16, [OutOfBounds])
            \\pick_u16 = |items, index| items.get(index)
            \\pick_i16 : List(I16), U64 -> Try(I16, [OutOfBounds])
            \\pick_i16 = |items, index| items.get(index)
            \\pick_u32 : List(U32), U64 -> Try(U32, [OutOfBounds])
            \\pick_u32 = |items, index| items.get(index)
            \\pick_i32 : List(I32), U64 -> Try(I32, [OutOfBounds])
            \\pick_i32 = |items, index| items.get(index)
            \\pick_u64 : List(U64), U64 -> Try(U64, [OutOfBounds])
            \\pick_u64 = |items, index| items.get(index)
            \\pick_i64 : List(I64), U64 -> Try(I64, [OutOfBounds])
            \\pick_i64 = |items, index| items.get(index)
            \\pick_u128 : List(U128), U64 -> Try(U128, [OutOfBounds])
            \\pick_u128 = |items, index| items.get(index)
            \\pick_i128 : List(I128), U64 -> Try(I128, [OutOfBounds])
            \\pick_i128 = |items, index| items.get(index)
            \\pick_f32 : List(F32), U64 -> Try(F32, [OutOfBounds])
            \\pick_f32 = |items, index| items.get(index)
            \\pick_f64 : List(F64), U64 -> Try(F64, [OutOfBounds])
            \\pick_f64 = |items, index| items.get(index)
            \\pick_dec : List(Dec), U64 -> Try(Dec, [OutOfBounds])
            \\pick_dec = |items, index| items.get(index)
            \\pick_bool : List(Bool), U64 -> Try(Bool, [OutOfBounds])
            \\pick_bool = |items, index| items.get(index)
            \\pick_str : List(Str), U64 -> Try(Str, [OutOfBounds])
            \\pick_str = |items, index| items.get(index)
            \\pick_record : List({ value : U32 }), U64 -> Try({ value : U32 }, [OutOfBounds])
            \\pick_record = |items, index| items.get(index)
            \\second : List(a) -> Try(a, [OutOfBounds])
            \\second = |items| List.get(items, 1)
            \\main! = |args| {
            \\    echo!(Str.inspect(pick_u8([U8.lowest, U8.highest], args.len())))
            \\    echo!(Str.inspect(pick_i8([I8.lowest, I8.highest], args.len())))
            \\    echo!(Str.inspect(pick_u16([U16.lowest, U16.highest], args.len())))
            \\    echo!(Str.inspect(pick_i16([I16.lowest, I16.highest], args.len())))
            \\    echo!(Str.inspect(pick_u32([U32.lowest, U32.highest], args.len())))
            \\    echo!(Str.inspect(pick_i32([I32.lowest, I32.highest], args.len())))
            \\    echo!(Str.inspect(pick_u64([U64.lowest, U64.highest], args.len())))
            \\    echo!(Str.inspect(pick_i64([I64.lowest, I64.highest], args.len())))
            \\    echo!(Str.inspect(pick_u128([U128.lowest, U128.highest], args.len())))
            \\    echo!(Str.inspect(pick_i128([I128.lowest, I128.highest], args.len())))
            \\    echo!(Str.inspect(pick_f32([1.5, 2.5], args.len())))
            \\    echo!(Str.inspect(pick_f64([1.5, 2.5], args.len())))
            \\    echo!(Str.inspect(pick_dec([1.5, 2.5], args.len())))
            \\    echo!(Str.inspect(pick_bool([True, False], args.len())))
            \\    echo!(Str.inspect(pick_str(args, args.len())))
            \\    echo!(Str.inspect(pick_record([{ value: 1 }], args.len())))
            \\    echo!(Str.inspect(second(args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, inspect);
    }
}

fn nameIs(name: []const u8, expected: []const u8) bool {
    return std.mem.eql(u8, name, expected) or
        (std.mem.endsWith(u8, name, expected) and name[name.len - expected.len - 1] == '.');
}

/// The debug names of the procedures `proc`'s body calls directly.
fn calledNames(store: *const lir.LirStore, proc: lir.LIR.LirProcSpec, out: *std.ArrayList([]const u8)) std.mem.Allocator.Error!void {
    var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
    defer pending.deinit(std.testing.allocator);
    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
    defer visited.deinit();
    try pending.append(std.testing.allocator, proc.body.?);
    while (pending.pop()) |id| {
        if ((try visited.getOrPut(id)).found_existing) continue;
        const stmt = store.getCFStmt(id);
        if (stmt == .assign_call) {
            if (store.procDebugName(stmt.assign_call.proc)) |name| try out.append(std.testing.allocator, name);
        }
        try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
    }
}

/// The `get` worker has no remaining open item or result type: no
/// dynamic parameter, no dynamic result, and no descriptor-guided statement.
fn expectStaticWorker(store: *const lir.LirStore, proc: lir.LIR.LirProcSpec) !void {
    const args = store.getLocalSpan(proc.args);
    for (0..collections.GuardedList.borrowLen(args)) |arg_index| {
        try std.testing.expect(store.localDyn(collections.GuardedList.at(args, arg_index)) == null);
    }
    try std.testing.expect(proc.dyn_result == null);
    var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
    defer pending.deinit(std.testing.allocator);
    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
    defer visited.deinit();
    try pending.append(std.testing.allocator, proc.body.?);
    while (pending.pop()) |id| {
        if ((try visited.getOrPut(id)).found_existing) continue;
        try std.testing.expect(store.getCFStmt(id) != .dyn);
        try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
    }
}

const Caller = enum { pick_u8, pick_i8, pick_u16, pick_i16, pick_u32, pick_i32, pick_u64, pick_i64, pick_u128, pick_i128, pick_f32, pick_f64, pick_dec, pick_bool, pick_str, pick_record, second };

/// The one `List.get` worker each caller must call.
fn expectedCallee(caller: Caller) []const u8 {
    return switch (caller) {
        .pick_u8 => "Builtin.List.get@U8",
        .pick_i8 => "Builtin.List.get@I8",
        .pick_u16 => "Builtin.List.get@U16",
        .pick_i16 => "Builtin.List.get@I16",
        .pick_u32 => "Builtin.List.get@U32",
        .pick_i32 => "Builtin.List.get@I32",
        .pick_u64 => "Builtin.List.get@U64",
        .pick_i64 => "Builtin.List.get@I64",
        .pick_u128 => "Builtin.List.get@U128",
        .pick_i128 => "Builtin.List.get@I128",
        .pick_f32, .pick_f64, .pick_dec, .pick_bool, .pick_str, .pick_record, .second => "Builtin.List.get",
    };
}

const get_workers = [_][]const u8{
    "Builtin.List.get",
    "Builtin.List.get@U8",
    "Builtin.List.get@I8",
    "Builtin.List.get@U16",
    "Builtin.List.get@I16",
    "Builtin.List.get@U32",
    "Builtin.List.get@I32",
    "Builtin.List.get@U64",
    "Builtin.List.get@I64",
    "Builtin.List.get@U128",
    "Builtin.List.get@I128",
};

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    var found: std.EnumSet(Caller) = .empty;
    var static_workers: usize = 0;
    for (store.getProcSpecs(), 0..) |proc, index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (std.mem.startsWith(u8, name, "Builtin.List.get@")) {
            static_workers += 1;
            try expectStaticWorker(store, proc);
            continue;
        }
        const caller: Caller = inline for (comptime std.enums.values(Caller)) |candidate| {
            if (nameIs(name, @tagName(candidate))) break candidate;
        } else continue;
        found.insert(caller);
        var called: std.ArrayList([]const u8) = .empty;
        defer called.deinit(std.testing.allocator);
        try calledNames(store, proc, &called);
        var expected_calls: usize = 0;
        var other_get_calls: usize = 0;
        for (called.items) |callee| {
            if (std.mem.eql(u8, callee, expectedCallee(caller))) {
                expected_calls += 1;
                continue;
            }
            for (get_workers) |worker| {
                if (std.mem.eql(u8, callee, worker)) other_get_calls += 1;
            }
        }
        try std.testing.expectEqual(@as(usize, 1), expected_calls);
        try std.testing.expectEqual(@as(usize, 0), other_get_calls);
    }
    try std.testing.expect(found.eql(.full));
    try std.testing.expectEqual(@as(usize, 10), static_workers);
}
