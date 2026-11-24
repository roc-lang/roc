//! Keep LLVM's transitive leaf inlining scoped to loop-free, call-free helpers.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");
const harness = @import("lower_to_lir_harness.zig");

test "LLVM keeps pure transitive leaves always inline and leaves looping or runtime-calling helpers out of line" {
    try harness.runLoweredInspection(
        \\add_step : U64, U64 -> U64
        \\add_step = |x, y| x + y
        \\
        \\pure_leaf : U64 -> U64
        \\pure_leaf = |n| {
        \\    a = add_step(n, 1)
        \\    b = add_step(a, 2)
        \\    c = add_step(b, 3)
        \\    d = add_step(c, 4)
        \\    e = add_step(d, 5)
        \\    f = add_step(e, 6)
        \\    g = add_step(f, 7)
        \\    h = add_step(g, 8)
        \\    i = add_step(h, 9)
        \\    j = add_step(i, 10)
        \\    k = add_step(j, 11)
        \\    l = add_step(k, 12)
        \\    m = add_step(l, 13)
        \\    o = add_step(m, 14)
        \\    p = add_step(o, 15)
        \\    q = add_step(p, 16)
        \\    add_step(q, 17)
        \\}
        \\
        \\looping_helper : U64 -> U64
        \\looping_helper = |limit| {
        \\    var $acc = 0
        \\    var $i = 0
        \\    while $i < limit {
        \\        $acc = $acc + $i
        \\        $i = $i + 1
        \\    }
        \\    $acc
        \\}
        \\
        \\compare_bytes : Str, Str -> Bool
        \\compare_bytes = |left, right| {
        \\    left_bytes = Str.to_utf8(left)
        \\    right_bytes = Str.to_utf8(right)
        \\    a = List.len(left_bytes) + 1
        \\    b = List.len(right_bytes) + 1
        \\    c = a + b
        \\    d = c + a
        \\    e = d + b
        \\    f = e + c
        \\    g = f + d
        \\    h = g + e
        \\    i = h + f
        \\    j = i + g
        \\    k = j + h
        \\    l = k + i
        \\    m = l + j
        \\    o = m + k
        \\    p = o + l
        \\    q = p + m
        \\    q > 0 and List.len(left_bytes) == List.len(right_bytes)
        \\}
        \\
        \\main! = |args| {
        \\    n = args.len()
        \\    first = Dict.insert(Dict.with_capacity(n), n, n)
        \\    second = Dict.insert(first, n + 1, n + 1)
        \\    same = compare_bytes(Str.inspect(n), Str.inspect(pure_leaf(n) + looping_helper(n)))
        \\    echo!(Str.inspect((Dict.len(first), Dict.len(second), same)))
        \\    Ok({})
        \\}
    , .{
        // Leave ordinary helper calls for the LLVM backend to expand.
        .inline_mode = .none,
        .spec_constr_clone_inlining = .no_calls,
    }, inspectInlinePolicy);
}

fn functionText(ir: []const u8, symbol: []const u8) ?[]const u8 {
    var lines = std.mem.splitScalar(u8, ir, '\n');
    while (lines.next()) |line| {
        if (!std.mem.startsWith(u8, line, "define ") or std.mem.find(u8, line, symbol) == null) continue;
        const start = @intFromPtr(line.ptr) - @intFromPtr(ir.ptr);
        const end = std.mem.findPos(u8, ir, start, "\n}") orelse return null;
        return ir[start .. end + 2];
    }
    return null;
}

fn functionAttrGroup(ir: []const u8, proc_index: usize) harness.LowerToLirHarnessError![]const u8 {
    const gpa = std.testing.allocator;
    const symbol = try std.fmt.allocPrint(gpa, "@roc_proc_{d}.fast(", .{proc_index});
    defer gpa.free(symbol);
    const body = functionText(ir, symbol) orelse return error.TestUnexpectedResult;
    const header = body[0 .. std.mem.findScalar(u8, body, '\n') orelse return error.TestUnexpectedResult];
    const group_start = (std.mem.findScalar(u8, header, '#') orelse return error.TestUnexpectedResult) + 1;
    const group_end = std.mem.findScalar(u8, header[group_start..], ' ') orelse return error.TestUnexpectedResult;
    const group = header[group_start..][0..group_end];
    const attribute_prefix = try std.fmt.allocPrint(gpa, "attributes #{s} = ", .{group});
    defer gpa.free(attribute_prefix);
    const attrs_start = std.mem.find(u8, ir, attribute_prefix) orelse return error.TestUnexpectedResult;
    const attrs_end = std.mem.findScalar(u8, ir[attrs_start..], '\n') orelse return error.TestUnexpectedResult;
    return ir[attrs_start..][0..attrs_end];
}

fn inspectInlinePolicy(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const store = &lowered.lir_result.store;
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(gpa, store, builtin.target);
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;
    codegen.proc_symbol_mode = .local_index;
    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    codegen.printEntrypointModule("inline_policy", &.{}, &output.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };
    const ir = output.written();

    var saw_pure_leaf = false;
    var saw_looping = false;
    var saw_compare_bytes = false;
    var saw_dict_insert = false;

    for (store.getProcSpecs(), 0..) |_, proc_index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(proc_index))) orelse continue;
        if (std.mem.endsWith(u8, name, "pure_leaf")) {
            saw_pure_leaf = true;
            const attrs = try functionAttrGroup(ir, proc_index);
            try std.testing.expect(std.mem.find(u8, attrs, "alwaysinline") != null);
        } else if (std.mem.endsWith(u8, name, "looping_helper")) {
            saw_looping = true;
            const attrs = try functionAttrGroup(ir, proc_index);
            try std.testing.expect(std.mem.find(u8, attrs, "alwaysinline") == null);
            try std.testing.expect(std.mem.find(u8, attrs, "inlinehint") != null);
        } else if (std.mem.endsWith(u8, name, "compare_bytes")) {
            saw_compare_bytes = true;
            const attrs = try functionAttrGroup(ir, proc_index);
            try std.testing.expect(std.mem.find(u8, attrs, "alwaysinline") == null);
            try std.testing.expect(std.mem.find(u8, attrs, "inlinehint") != null);
        } else if (std.mem.eql(u8, name, "dict_insert_new_data")) {
            saw_dict_insert = true;
            const attrs = try functionAttrGroup(ir, proc_index);
            try std.testing.expect(std.mem.find(u8, attrs, "alwaysinline") == null);
            try std.testing.expect(std.mem.find(u8, attrs, "inlinehint") != null);
        }
    }
    try std.testing.expect(saw_pure_leaf);
    try std.testing.expect(saw_looping);
    try std.testing.expect(saw_compare_bytes);
    try std.testing.expect(saw_dict_insert);
}
