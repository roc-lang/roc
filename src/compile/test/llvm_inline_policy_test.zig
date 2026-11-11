//! Keep LLVM's transitive helper inlining independent of entrypoint policy.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");
const harness = @import("lower_to_lir_harness.zig");

test "LLVM keeps Dict.insert's expanded leaf helper always inline" {
    try harness.runLoweredInspection(
        \\main! = |args| {
        \\    n = args.len()
        \\    first = Dict.insert(Dict.with_capacity(n), n, n)
        \\    second = Dict.insert(first, n + 1, n + 1)
        \\    echo!(Str.inspect((Dict.len(first), Dict.len(second))))
        \\    Ok({})
        \\}
    , .{
        // Leave ordinary helper calls for the LLVM backend to expand.
        .inline_mode = .none,
        .spec_constr_clone_inlining = .iterator_fusion,
        .proc_debug_names = true,
    }, inspectDictInlining);
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

fn reachesHelper(ir: []const u8, caller: usize, helper: usize) harness.LowerToLirHarnessError!bool {
    const gpa = std.testing.allocator;
    var pending: std.ArrayList(usize) = .empty;
    defer pending.deinit(gpa);
    var seen: std.AutoHashMapUnmanaged(usize, void) = .empty;
    defer seen.deinit(gpa);
    try pending.append(gpa, caller);
    while (pending.pop()) |index| {
        if (index == helper) return true;
        if ((try seen.getOrPut(gpa, index)).found_existing) continue;
        const symbol = try std.fmt.allocPrint(gpa, "@roc_proc_{d}.fast(", .{index});
        defer gpa.free(symbol);
        const body = functionText(ir, symbol) orelse return error.TestUnexpectedResult;
        var lines = std.mem.splitScalar(u8, body, '\n');
        while (lines.next()) |line| {
            const call = std.mem.find(u8, line, "call ") orelse continue;
            const prefix = "@roc_proc_";
            const callee = std.mem.find(u8, line[call..], prefix) orelse continue;
            const digits = line[call + callee + prefix.len ..];
            var end: usize = 0;
            while (end < digits.len and std.ascii.isDigit(digits[end])) : (end += 1) {}
            const callee_index = std.fmt.parseInt(usize, digits[0..end], 10) catch return error.TestUnexpectedResult;
            try pending.append(gpa, callee_index);
        }
    }
    return false;
}

fn inspectDictInlining(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const store = &lowered.lir_result.store;
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(gpa, store, builtin.target);
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;
    codegen.proc_symbol_mode = .local_index;
    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    codegen.printEntrypointModule("dict_inline_policy", &.{}, &output.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };
    const ir = output.written();
    var found_call = false;
    for (store.getProcSpecs(), 0..) |_, helper_index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(helper_index))) orelse continue;
        if (!std.mem.eql(u8, name, "dict_insert_new_data")) continue;
        const helper_symbol = try std.fmt.allocPrint(gpa, "@roc_proc_{d}.fast(", .{helper_index});
        defer gpa.free(helper_symbol);
        const helper = functionText(ir, helper_symbol) orelse return error.TestUnexpectedResult;
        const header = helper[0 .. std.mem.findScalar(u8, helper, '\n') orelse return error.TestUnexpectedResult];
        const group_start = (std.mem.findScalar(u8, header, '#') orelse return error.TestUnexpectedResult) + 1;
        const group_end = std.mem.findScalar(u8, header[group_start..], ' ') orelse return error.TestUnexpectedResult;
        const group = header[group_start..][0..group_end];
        const attribute_prefix = try std.fmt.allocPrint(gpa, "attributes #{s} = ", .{group});
        defer gpa.free(attribute_prefix);
        const attrs_start = std.mem.find(u8, ir, attribute_prefix) orelse return error.TestUnexpectedResult;
        const attrs_end = std.mem.findScalar(u8, ir[attrs_start..], '\n') orelse return error.TestUnexpectedResult;
        try std.testing.expect(std.mem.find(u8, ir[attrs_start..][0..attrs_end], "alwaysinline") != null);
        for (store.getProcSpecs(), 0..) |_, caller_index| {
            const caller_name = store.procDebugName(@fromBackingInt(@intCast(caller_index))) orelse continue;
            if (!std.mem.endsWith(u8, caller_name, "Dict.insert")) continue;
            if (try reachesHelper(ir, caller_index, helper_index)) found_call = true;
        }
    }
    // Dict.insert can reach this leaf through another insertion helper.
    // Pin the real call path, not an unrelated always-inline function.
    try std.testing.expect(found_call);
}
