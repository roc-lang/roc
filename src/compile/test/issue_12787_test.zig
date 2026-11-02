//! A derived parser's code for one record field stays in that field's share of
//! the straight-line code the LLVM backend emits. LLVM's wasm register
//! stackifier scans a whole basic block for each use of a frequently read
//! register, such as the frame pointer, so a block whose length grows steeply
//! with a record's field count costs quadratic compile time.
const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");
const harness = @import("lower_to_lir_harness.zig");

const small_record = 15;
const large_record = 30;

/// The longest run of instructions in one basic block of the printed module.
/// Static allocas are skipped: they only reserve frame space and emit no code.
fn longestBlock(ir: []const u8) usize {
    var longest: usize = 0;
    var current: usize = 0;
    var in_function = false;
    var lines = std.mem.splitScalar(u8, ir, '\n');
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "define ")) {
            in_function = true;
            current = 0;
            continue;
        }
        if (!in_function or line.len == 0) continue;
        if (line[0] != ' ' and line[0] != '\t') {
            longest = @max(longest, current);
            current = 0;
            if (line[0] == '}') in_function = false;
            continue;
        }
        if (std.mem.find(u8, line, " = alloca ") == null) current += 1;
    }
    return longest;
}

var measured_block: usize = 0;

fn measure(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(
        std.testing.allocator,
        &lowered.lir_result.store,
        builtin.target,
    );
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;

    var ir: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer ir.deinit();
    codegen.printEntrypointModule("issue_12787", &.{}, &ir.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };
    measured_block = longestBlock(ir.written());
}

/// `Json.parse` into a record of `fields` fields, each of its own tag union.
fn longestBlockAt(fields: usize, strategy: base.SpecializationStrategy) (std.Io.Writer.Error || harness.LowerToLirHarnessError)!usize {
    var app: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer app.deinit();
    try app.writer.writeAll("Model : {\n");
    for (0..fields) |i| try app.writer.print("    f{d} : [A{d}(U64), B{d}(Str)],\n", .{ i, i, i });
    try app.writer.writeAll(
        \\}
        \\main! = |args| {
        \\    r : Try(Model, _)
        \\    r = Json.parse(Str.inspect(args))
        \\    match r {
        \\        Ok(_) => echo!("ok")
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    );
    try harness.runLoweredInspection(app.written(), .{ .specialization_strategy = strategy }, measure);
    return measured_block;
}

test "issue 12787: a boxy derived parser's longest block grows with fields as a specialized parser's does" {
    const lss_small = try longestBlockAt(small_record, .lss);
    const lss_large = try longestBlockAt(large_record, .lss);
    const boxy_small = try longestBlockAt(small_record, .boxy);
    const boxy_large = try longestBlockAt(large_record, .boxy);

    const added_fields = large_record - small_record;
    const lss_per_field = (lss_large -| lss_small) / added_fields;
    const boxy_per_field = (boxy_large -| boxy_small) / added_fields;
    try std.testing.expect(lss_per_field > 0);
    // A closed record lowers to the same kind of code under both strategies,
    // so the per-field growth of the longest block is of one order.
    if (boxy_per_field > 2 * lss_per_field) {
        std.debug.print("longest block per field: boxy {d} ({d} to {d}), specialized {d} ({d} to {d})\n", .{
            boxy_per_field,
            boxy_small,
            boxy_large,
            lss_per_field,
            lss_small,
            lss_large,
        });
        return error.TestUnexpectedResult;
    }
}
