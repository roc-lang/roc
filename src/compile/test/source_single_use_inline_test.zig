//! Dev builds inline a function by how many call sites its module's source
//! has, never by how many callers one program has, so every program that
//! links a procedure from the object cache makes the same inline decisions.
//! Each case counts the procedures dev lowering keeps beyond those of a
//! program that has only `main!`.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

// A loop keeps these bodies from being wrappers, which every rule inlines.
const helper =
    \\helper : U64 -> U64
    \\helper = |x| {
    \\    var $acc = x
    \\    var $i = 0
    \\    while $i < x {
    \\        $acc = $acc + $i
    \\        $i = $i + 1
    \\    }
    \\    $acc
    \\}
    \\
;

var lowered_proc_count: usize = 0;

fn recordProcCount(store: *const lir.LirStore, _: *const @import("layout").Store) harness.LowerToLirHarnessError!void {
    lowered_proc_count = store.procSpecCount();
}

fn devProcCount(source: []const u8) harness.LowerToLirHarnessError!usize {
    try harness.expectLirInspectionWithOptions(source, .{
        .inline_mode = .wrappers_and_source_single_use,
        .spec_constr_clone_inlining = .iterator_fusion,
    }, recordProcCount);
    return lowered_proc_count;
}

/// Procedures dev lowering keeps for `source` beyond the host entry and the
/// app's provided `main!`.
fn procsDevKeeps(source: []const u8) harness.LowerToLirHarnessError!usize {
    const baseline = try devProcCount(
        \\main! = |args| {
        \\    if args.len() == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    );
    const count = try devProcCount(source);
    try std.testing.expect(count >= baseline);
    return count - baseline;
}

test "dev inlines a private function its module calls at one site" {
    try std.testing.expectEqual(@as(usize, 0), try procsDevKeeps(helper ++
        \\main! = |args| {
        \\    if helper(args.len()) == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    ));
}

test "dev keeps a function its source calls twice even when one program reaches one call" {
    // This program calls `helper` once. A program that also reached
    // `unreached` would call it twice, so the decision cannot depend on it.
    try std.testing.expectEqual(@as(usize, 1), try procsDevKeeps(helper ++
        \\unreached : U64 -> U64
        \\unreached = |x| helper(x) + 1
        \\
        \\main! = |args| {
        \\    if helper(args.len()) == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    ));
}

test "dev keeps a method another module could dispatch to" {
    try std.testing.expectEqual(@as(usize, 1), try procsDevKeeps(
        \\Counter := [Counter(U64)].{
        \\    total : Counter -> U64
        \\    total = |Counter(n)| {
        \\        var $acc = n
        \\        var $i = 0
        \\        while $i < n {
        \\            $acc = $acc + $i
        \\            $i = $i + 1
        \\        }
        \\        $acc
        \\    }
        \\}
        \\
        \\main! = |args| {
        \\    counter = Counter.Counter(args.len())
        \\    if counter.total() == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    ));
}

test "dev keeps a function its source also uses as a value" {
    try std.testing.expectEqual(@as(usize, 1), try procsDevKeeps(helper ++
        \\twice : (U64 -> U64), U64 -> U64
        \\twice = |f, x| f(f(x))
        \\
        \\main! = |args| {
        \\    if twice(helper, args.len()) == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    ));
}

fn expectInlinedProcedureMarked(store: *const lir.LirStore, _: *const @import("layout").Store) harness.LowerToLirHarnessError!void {
    var marked: usize = 0;
    for (store.getProcSpecs()) |proc| {
        if (proc.inlined_at_calls) marked += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), marked);
}

test "a procedure whose calls dev inlines is marked so the object cache never offers it" {
    // `bump` is a wrapper: its call is inlined, and passing it as a value
    // still gives it a procedure, which a program taking a cache hit for it
    // before inlining could not inline.
    try harness.expectLirInspectionWithOptions(
        \\bump : U64 -> U64
        \\bump = |x| x + 1
        \\
        \\apply : (U64 -> U64), U64 -> U64
        \\apply = |f, x| f(x)
        \\
        \\main! = |args| {
        \\    if bump(args.len()) + apply(bump, 3) == 0 { Err(Exit(1)) } else { Ok({}) }
        \\}
    , .{
        .inline_mode = .wrappers_and_source_single_use,
        .spec_constr_clone_inlining = .iterator_fusion,
    }, expectInlinedProcedureMarked);
}
