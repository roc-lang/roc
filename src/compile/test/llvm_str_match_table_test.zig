//! Keep LLVM's code for a table of string literal patterns one dispatch on the
//! matched string, whatever the number of arms, and its code for an equality
//! with a string literal a comparison against constants.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");
const harness = @import("lower_to_lir_harness.zig");

test "LLVM dispatches a table of string literal patterns on length and bytes" {
    try harness.runLoweredInspection(
        \\keyword : Str -> U64
        \\keyword = |name| match name {
        \\    "as" => 1
        \\    "do" => 2
        \\    "fn" => 3
        \\    "if" => 4
        \\    "in" => 5
        \\    "a" => 6
        \\    "b" => 7
        \\    "c" => 8
        \\    "abstract+final" => 9
        \\    _ => 0
        \\}
        \\
        \\main! = |args| {
        \\    echo!(keyword(Str.inspect(args.len())).to_str())
        \\    Ok({})
        \\}
    , .{
        // Keep the match in one procedure of its own.
        .inline_mode = .none,
        .spec_constr_clone_inlining = .no_calls,
    }, inspectStrMatchTable);
}

test "LLVM compares a string with a literal against the literal's bytes as constants" {
    try harness.runLoweredInspection(
        \\is_ab : Str -> Bool
        \\is_ab = |name| name == "ab"
        \\
        \\main! = |args| {
        \\    echo!(Str.inspect(is_ab(Str.inspect(args.len()))))
        \\    Ok({})
        \\}
    , .{
        // Keep the comparison in one procedure of its own.
        .inline_mode = .none,
        .spec_constr_clone_inlining = .no_calls,
    }, inspectStrLiteralEq);
}

fn inspectStrLiteralEq(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(gpa, &lowered.lir_result.store, builtin.target);
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;
    codegen.proc_symbol_mode = .local_index;
    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    codegen.printEntrypointModule("str_literal_eq", &.{}, &output.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };

    // The two bytes of "ab" are compared as one little-endian word.
    const word = @as(u16, 'a') | (@as(u16, 'b') << 8);
    const operand = try std.fmt.allocPrint(gpa, ", {d}", .{word});
    defer gpa.free(operand);
    var found = false;
    var lines = std.mem.splitScalar(u8, output.written(), '\n');
    while (lines.next()) |line| {
        if (std.mem.find(u8, line, "icmp ne i16 ") != null and std.mem.endsWith(u8, line, operand)) found = true;
    }
    try std.testing.expect(found);
}

/// The number of cases in the one `switch` on a value of type `ty` that `ir`
/// holds, or null when it holds none or several.
fn onlySwitchCaseCount(ir: []const u8, comptime ty: []const u8) ?usize {
    const header = "switch " ++ ty ++ " ";
    if (std.mem.count(u8, ir, header) != 1) return null;
    const start = std.mem.find(u8, ir, header).?;
    const end = std.mem.findPos(u8, ir, start, "]") orelse return null;
    return std.mem.count(u8, ir[start..end], "\n    " ++ ty ++ " ");
}

fn inspectStrMatchTable(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(gpa, &lowered.lir_result.store, builtin.target);
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;
    codegen.proc_symbol_mode = .local_index;
    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    codegen.printEntrypointModule("str_match_table", &.{}, &output.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };
    const ir = output.written();

    // One switch on the two bytes tells the five two-byte literals apart; no
    // arm has a test of its own.
    try std.testing.expectEqual(@as(?usize, 5), onlySwitchCaseCount(ir, "i16"));
    try std.testing.expectEqual(@as(usize, 0), std.mem.count(u8, ir, "icmp eq i16 "));
    try std.testing.expectEqual(@as(usize, 0), std.mem.count(u8, ir, "icmp ne i16 "));
}
