//! Regression test for issue #12423.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");

const harness = @import("lower_to_lir_harness.zig");

/// Each `match` branch of `walk` makes its own out-of-line call to `walk`,
/// which returns its record through a result slot and takes its `List`
/// arguments by pointer. The branches are mutually exclusive, so LLVM's stack
/// coloring can overlap those slots only when the backend scopes them with
/// `llvm.lifetime.start` / `llvm.lifetime.end`.
const app_body =
    \\R : { pos : U64, a : List(U64), b : List(U64), c : List(U64), node : U64 }
    \\
    \\leaf : U64, List(U64), List(U64), List(U64), U64 -> R
    \\leaf = |pos, a, b, c, k| { pos: pos + k + 1, a: a.append(pos), b: b.append(k), c, node: pos * k }
    \\
    \\walk : U64, List(U64), List(U64), List(U64), U64 -> R
    \\walk = |pos, a, b, c, depth| {
    \\    if depth == 0 {
    \\        { pos, a, b, c, node: 0 }
    \\    } else {
    \\        match pos % 4 {
    \\            0 => {
    \\                r = leaf(pos, a, b, c, 0)
    \\                s = walk(r.pos, r.a, r.b, r.c, depth - 1)
    \\                { pos: s.pos, a: s.a, b: s.b, c: s.c.append(r.node), node: s.node + r.node }
    \\            }
    \\            1 => {
    \\                r = leaf(pos, a, b, c, 1)
    \\                s = walk(r.pos, r.a, r.b, r.c, depth - 1)
    \\                { pos: s.pos, a: s.a, b: s.b, c: s.c.append(r.node), node: s.node + r.node }
    \\            }
    \\            2 => {
    \\                r = leaf(pos, a, b, c, 2)
    \\                s = walk(r.pos, r.a, r.b, r.c, depth - 1)
    \\                { pos: s.pos, a: s.a, b: s.b, c: s.c.append(r.node), node: s.node + r.node }
    \\            }
    \\            _ => {
    \\                r = leaf(pos, a, b, c, 3)
    \\                s = walk(r.pos, r.a, r.b, r.c, depth - 1)
    \\                { pos: s.pos, a: s.a, b: s.b, c: s.c.append(r.node), node: s.node + r.node }
    \\            }
    \\        }
    \\    }
    \\}
    \\
    \\arg : List(Str), U64 -> U64
    \\arg = |args, i| match args.get(i) {
    \\    Ok(s) => U64.from_str(s) ?? 0
    \\    Err(_) => 0
    \\}
    \\
    \\main! = |args| {
    \\    r = walk(arg(args, 1), [], [], [], arg(args, 0))
    \\    echo!((r.pos + r.node + r.a.len() + r.c.len()).to_str())
    \\    Ok({})
    \\}
;

/// A function definition in textual IR, with its name as it appears after `@`.
const Function = struct { name: []const u8, body: []const u8 };

/// The only function in `ir` that calls itself from `sites` call sites.
fn findSelfCaller(ir: []const u8, sites: usize) ?Function {
    var found: ?Function = null;
    var rest = ir;
    while (std.mem.find(u8, rest, "\ndefine ")) |define| {
        rest = rest[define + 1 ..];
        const name_start = std.mem.findScalar(u8, rest, '@').? + 1;
        const name_end = name_start + std.mem.findScalar(u8, rest[name_start..], '(').?;
        const body_start = std.mem.findScalar(u8, rest, '{').?;
        const body_end = body_start + std.mem.find(u8, rest[body_start..], "\n}").?;
        const function = Function{ .name = rest[name_start..name_end], .body = rest[body_start..body_end] };
        var call_pattern: [256]u8 = undefined;
        const pattern = std.fmt.bufPrint(&call_pattern, "@{s}(", .{function.name}) catch unreachable;
        if (std.mem.count(u8, function.body, pattern) == sites) {
            if (found != null) return null;
            found = function;
        }
        rest = rest[body_end..];
    }
    return found;
}

fn expectRecursiveCallSlotsHaveLifetimes(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) harness.LowerToLirHarnessError!void {
    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(
        std.testing.allocator,
        &lowered.lir_result.store,
        lowered.lir_result.boxy_erased_arg_desc_offsets.items,
        lowered.lir_result.boxy_erased_arg_desc_params.items,
        lowered.lir_result.boxy_worker_procs.items,
        builtin.target,
    );
    defer codegen.deinit();
    codegen.layout_store = &lowered.lir_result.layouts;

    var ir: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer ir.deinit();
    codegen.printEntrypointModule("issue_12423", &.{}, &ir.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.TestUnexpectedResult,
    };

    const walk = findSelfCaller(ir.written(), 4) orelse return error.TestUnexpectedResult;
    var call_pattern: [256]u8 = undefined;
    const pattern = std.fmt.bufPrint(&call_pattern, "@{s}(", .{walk.name}) catch unreachable;

    var recursive_calls: usize = 0;
    var bracketed_slots: usize = 0;
    var lines = std.mem.splitScalar(u8, walk.body, '\n');
    while (lines.next()) |line| {
        const call = std.mem.find(u8, line, pattern) orelse continue;
        recursive_calls += 1;
        var args = std.mem.splitSequence(u8, line[call + pattern.len ..], ", ");
        while (args.next()) |arg| {
            if (!std.mem.startsWith(u8, arg, "ptr ")) continue;
            const operand = std.mem.trimEnd(u8, arg[std.mem.findScalarLast(u8, arg, ' ').? + 1 ..], ")");
            var buf: [256]u8 = undefined;
            const alloca = std.fmt.bufPrint(&buf, "{s} = alloca ", .{operand}) catch unreachable;
            if (std.mem.find(u8, walk.body, alloca) == null) continue;
            // Every slot this call addresses lives only while the branch
            // that makes the call needs it.
            const start = std.fmt.bufPrint(&buf, "@llvm.lifetime.start.p0(ptr {s})", .{operand}) catch unreachable;
            try std.testing.expect(std.mem.find(u8, walk.body, start) != null);
            const end = std.fmt.bufPrint(&buf, "@llvm.lifetime.end.p0(ptr {s})", .{operand}) catch unreachable;
            try std.testing.expect(std.mem.find(u8, walk.body, end) != null);
            bracketed_slots += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 4), recursive_calls);
    try std.testing.expect(bracketed_slots >= recursive_calls);
}

test "issue 12423: LLVM slots addressed by out-of-line calls carry lifetime markers" {
    // Repro for https://github.com/roc-lang/roc/issues/12423.
    //
    // Every slot whose address one of `walk`'s four recursive call sites
    // passes must be bracketed by `llvm.lifetime.start` / `llvm.lifetime.end`,
    // so the four exclusive branches can share stack space instead of
    // `walk`'s frame holding every call site's slots at once.
    try harness.runLoweredInspection(app_body, .{ .proc_debug_names = true }, expectRecursiveCallSlotsHaveLifetimes);
}
