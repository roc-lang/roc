//! A single-use helper whose body holds a `?` early return is inlined into
//! the loop that calls it, so the loop's append is promoted to the unchecked
//! form like an append written in the loop itself.
//! https://github.com/roc-lang/roc/issues/13035

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

const app =
    "step : List(U8), U64 -> Try(List(U8), [E])\n" ++
    "step = |out, x| Ok(out.append(x.to_u8_wrap()))\n" ++
    "\n" ++
    "wrap : List(U8), U64 -> Try(List(U8), [E])\n" ++
    "wrap = |out, x| {\n" ++
    "    next = step(out, x)?\n" ++
    "    Ok(next)\n" ++
    "}\n" ++
    "\n" ++
    "run : U64 -> List(U8)\n" ++
    "run = |n| {\n" ++
    "    var $out = List.with_capacity(1024)\n" ++
    "    for k in (0.U64).until(n) {\n" ++
    "        $out = match wrap($out, k) {\n" ++
    "            Ok(v) => v\n" ++
    "            Err(_) => []\n" ++
    "        }\n" ++
    "    }\n" ++
    "    $out\n" ++
    "}\n" ++
    "\n" ++
    "main! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
    "main! = |args| {\n" ++
    "    echo!(Str.inspect(run(args.len()).len()))\n" ++
    "    Ok({})\n" ++
    "}\n";

var promoted_appends: usize = 0;

fn countPromotedAppends(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    promoted_appends = 0;
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 22);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @fromBackingInt(@intCast(@as(u32, @intCast(index)))), &writer);
        promoted_appends += std.mem.count(u8, writer.buffered(), "list_slack_unique");
    }
}

test "issue 13035: an append behind a single-use helper with an early return is promoted in the calling loop" {
    try harness.expectLirInspectionWithOptions(app, .{ .inline_mode = .wrappers }, countPromotedAppends);
    try std.testing.expect(promoted_appends >= 1);
}
