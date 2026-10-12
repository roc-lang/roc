//! Programs whose tag unions, records, tuples, and payloads exceed 16-bit
//! counts and offsets build and run exactly (design.md "Representation
//! Capacity"). Each program is generated here rather than checked in, and
//! `main!` exits nonzero when a value comes back wrong.

const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };

/// One more than a 16-bit count can represent.
const wide_count = 65_537;

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram) RunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    const program = &lowered.lir_result;
    var static_strings = try eval.LirInterpreter.buildStaticStrings(std.testing.allocator, &program.store);
    defer static_strings.deinit();
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        eval.LirInterpreter.BoxyTables.fromResult(program),
        static_strings.view(),
        host.get_ops(),
    );
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    var exit_code: i8 = -1;
    _ = try interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&exit_code),
    });
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    try host.checkForLeaks();
}

fn expectAppRuns(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("representation capacity app run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn expectBodyRuns(body: []const u8) harness.LowerToLirHarnessError!void {
    try harness.runLoweredInspection(body, .{ .specialization_strategy = .lss }, expectAppRuns);
    try harness.runLoweredInspection(body, .{ .specialization_strategy = .boxy }, expectAppRuns);
}

/// Tags sort by label text, so `T9998` and `T9999` hold discriminants 65,535
/// and 65,536: the last 16-bit value and the first one past it.
fn wideTagUnionBody(allocator: std.mem.Allocator) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();
    try out.writer.writeAll("Big : [");
    for (0..wide_count) |i| {
        if (i != 0) try out.writer.writeAll(", ");
        try out.writer.print("T{d}", .{i});
    }
    try out.writer.writeAll(
        \\]
        \\
        \\digit : Big -> U64
        \\digit = |value| match value {
        \\    T0 => 1
        \\    T9998 => 2
        \\    T9999 => 3
        \\    _ => 4
        \\}
        \\
        \\main! = |_args| {
        \\    var $total = 0.U64
        \\    for value in [T0, T9998, T9999, T123] {
        \\        $total = $total * 10 + digit(value)
        \\    }
        \\    if $total == 1234 { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
    return out.toOwnedSlice();
}

fn wideRecordBody(allocator: std.mem.Allocator) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();
    try out.writer.writeAll("main! = |_args| {\n    var $record = { ");
    for (0..wide_count) |i| {
        if (i != 0) try out.writer.writeAll(", ");
        try out.writer.print("f{d}: {d}.I64", .{ i, i });
    }
    try out.writer.writeAll(
        \\ }
        \\    if $record.f0 + $record.f65535 + $record.f65536 == 131071 { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
    return out.toOwnedSlice();
}

fn wideTupleBody(allocator: std.mem.Allocator) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var out: std.Io.Writer.Allocating = .init(allocator);
    errdefer out.deinit();
    try out.writer.writeAll("main! = |_args| {\n    var $tuple = (");
    for (0..wide_count) |i| {
        if (i != 0) try out.writer.writeAll(", ");
        try out.writer.print("{d}.I64", .{i});
    }
    try out.writer.writeAll(
        \\)
        \\    if $tuple.0 + $tuple.65535 + $tuple.65536 == 131071 { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
    return out.toOwnedSlice();
}

test "a tag union past 65,536 tags keeps every discriminant" {
    const body = try wideTagUnionBody(std.testing.allocator);
    defer std.testing.allocator.free(body);
    try expectBodyRuns(body);
}

test "a record past 65,536 fields reads its last fields" {
    const body = try wideRecordBody(std.testing.allocator);
    defer std.testing.allocator.free(body);
    try expectBodyRuns(body);
}

test "a tuple past 65,536 elements reads its last elements" {
    const body = try wideTupleBody(std.testing.allocator);
    defer std.testing.allocator.free(body);
    try expectBodyRuns(body);
}

test "a tag union with a 64 KiB payload places its discriminant past 16-bit offsets" {
    // `P13` doubles an 8-byte integer 13 times: a 65,536-byte payload, so the
    // discriminant sits at offset 65,536.
    try expectBodyRuns(
        \\P0 : I64
        \\P1 : { a : P0, b : P0 }
        \\P2 : { a : P1, b : P1 }
        \\P3 : { a : P2, b : P2 }
        \\P4 : { a : P3, b : P3 }
        \\P5 : { a : P4, b : P4 }
        \\P6 : { a : P5, b : P5 }
        \\P7 : { a : P6, b : P6 }
        \\P8 : { a : P7, b : P7 }
        \\P9 : { a : P8, b : P8 }
        \\P10 : { a : P9, b : P9 }
        \\P11 : { a : P10, b : P10 }
        \\P12 : { a : P11, b : P11 }
        \\P13 : { a : P12, b : P12 }
        \\
        \\Model : [Empty, Full(P13)]
        \\
        \\mk1 : I64 -> P1
        \\mk1 = |x| { a: x, b: x + 1 }
        \\mk2 : I64 -> P2
        \\mk2 = |x| { a: mk1(x), b: mk1(x + 1) }
        \\mk3 : I64 -> P3
        \\mk3 = |x| { a: mk2(x), b: mk2(x + 1) }
        \\mk4 : I64 -> P4
        \\mk4 = |x| { a: mk3(x), b: mk3(x + 1) }
        \\mk5 : I64 -> P5
        \\mk5 = |x| { a: mk4(x), b: mk4(x + 1) }
        \\mk6 : I64 -> P6
        \\mk6 = |x| { a: mk5(x), b: mk5(x + 1) }
        \\mk7 : I64 -> P7
        \\mk7 = |x| { a: mk6(x), b: mk6(x + 1) }
        \\mk8 : I64 -> P8
        \\mk8 = |x| { a: mk7(x), b: mk7(x + 1) }
        \\mk9 : I64 -> P9
        \\mk9 = |x| { a: mk8(x), b: mk8(x + 1) }
        \\mk10 : I64 -> P10
        \\mk10 = |x| { a: mk9(x), b: mk9(x + 1) }
        \\mk11 : I64 -> P11
        \\mk11 = |x| { a: mk10(x), b: mk10(x + 1) }
        \\mk12 : I64 -> P12
        \\mk12 = |x| { a: mk11(x), b: mk11(x + 1) }
        \\mk13 : I64 -> P13
        \\mk13 = |x| { a: mk12(x), b: mk12(x + 1) }
        \\
        \\describe : Model -> I64
        \\describe = |model| match model {
        \\    Empty => 0
        \\    Full(p) => p.b.b.b.b.b.b.b.b.b.b.b.b.b
        \\}
        \\
        \\main! = |_args| {
        \\    var $total = 0.I64
        \\    for model in [Empty, Full(mk13(1))] {
        \\        $total = $total * 100 + describe(model)
        \\    }
        \\    if $total == 14 { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
}

test "a boxed value behind a long alias chain lowers its layout" {
    var out: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer out.deinit();
    try out.writer.writeAll("A0 : I64\n");
    for (1..41) |i| try out.writer.print("A{d} : A{d}\n", .{ i, i - 1 });
    try out.writer.writeAll(
        \\
        \\unbox : Box(A40) -> A40
        \\unbox = |boxed| Box.unbox(boxed)
        \\
        \\main! = |_args| {
        \\    var $boxed = Box.box(42)
        \\    if unbox($boxed) == 42 { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
    try expectBodyRuns(out.written());
}
