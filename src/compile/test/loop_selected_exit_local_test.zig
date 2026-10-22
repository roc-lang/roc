//! Regression tests for loop exit selection.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLir = harness.expectLowersToLir;

test "selected loop exit carrying substituted local binder state lowers to LIR" {
    try expectLowersToLir(
        \\walk_closure = |s, v| {
        \\    v_num = match U64.from_str(v) { Ok(n) => n, _ => 0 }
        \\    match s {
        \\        (Unknown(Undefined), _) => Continue((Unknown(Value(v_num)), 0))
        \\        (Unknown(Value(prev)), _) =>
        \\            if v_num > prev {
        \\                Continue((Ascending(v_num), 0))
        \\            } else {
        \\                Break((Unknown(Undefined), 1))
        \\            }
        \\        (Ascending(_), _) => Break((Unknown(Undefined), 1))
        \\    }
        \\}
        \\
        \\main! = |_| {
        \\    (s, _) = List.fold_until(Str.split_on("", ""), (Unknown(Undefined), 0), walk_closure)
        \\    match s {
        \\        Unknown(_) => Ok({})
        \\        Ascending(_) => Ok({})
        \\    }
        \\}
    );
}

const ExitShape = struct {
    found: bool = false,
    increfs: usize = 0,
    structs: usize = 0,
};

var exit_shape: ExitShape = .{};

/// Count the retains and struct constructions in the proc that writes the
/// walked list.
fn countExitShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    exit_shape = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 22);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @fromBackingInt(@intCast(@as(u32, @intCast(index)))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "list_set") == 0) continue;
        exit_shape = .{
            .found = true,
            .increfs = std.mem.count(u8, text, "incref "),
            .structs = std.mem.count(u8, text, "= struct(l"),
        };
        return;
    }
}

// A loop left by `break` from inside its body, with a list among its
// variables and a loop around it. Every variable the inner loop changes is
// read after it, so no component is unused, yet the exit still hands each
// over directly: built as a tuple and read back field by field, every read
// would retain and release the list inside it on each pass of the outer loop.
test "a loop left by break hands every variable its continuation reads to it directly" {
    try harness.expectLirInspectionWithOptions(
        \\walk : List(U16), List(U32), U64 -> List(U32)
        \\walk = |xs0, out0, n| {
        \\    var $xs = xs0
        \\    var $out = out0
        \\    var $i = 0.U64
        \\    while $i < n {
        \\        var $found = 0.U64
        \\        while True {
        \\            v = List.get($xs, $i) ?? 0
        \\            $xs = match List.set($xs, $i.bitwise_and(63), 3) {
        \\                Ok(next) => next
        \\                Err(_) => crash "set"
        \\            }
        \\            if v == 7 {
        \\                $found = $i
        \\                break
        \\            } else {
        \\                $i = $i.plus_wrap(1)
        \\                if $i >= n {
        \\                    break
        \\                } else {
        \\                }
        \\            }
        \\        }
        \\        $out = List.append($out, $found.to_u32_wrap())
        \\        $i = $i.plus_wrap(1)
        \\    }
        \\    $out
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    out = walk(List.repeat(0.U16, 100 + args.len()), List.with_capacity(8), args.len())
        \\    echo!(Str.inspect(List.len(out)))
        \\    Ok({})
        \\}
    ,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countExitShape,
    );
    try std.testing.expect(exit_shape.found);
    try std.testing.expectEqual(@as(usize, 0), exit_shape.structs);
    try std.testing.expectEqual(@as(usize, 0), exit_shape.increfs);
}
