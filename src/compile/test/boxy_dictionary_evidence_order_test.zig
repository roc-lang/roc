//! Boxy pairs each worker dictionary group with its checked evidence slot.
//! Checking orders a scheme's evidence by its own walk of the scheme's row
//! chains, so `p.y`, read first, puts `y`'s `is_eq` ahead of `x`'s `trim`,
//! while the worker's label-sorted signature lists `x`'s group first.

const harness = @import("lower_to_lir_harness.zig");

const app_body =
    \\run = |p| if p.y == "" p.x.trim() else p.y
    \\
    \\main! = |args| {
    \\    echo!(run({ x: " b ", y: if args.is_empty() "" else "a" }))
    \\    Ok({})
    \\}
;

test "boxy dictionary groups read their checked evidence slot, not their signature position" {
    inline for (.{ .boxy, .lss }) |strategy| {
        try harness.expectLowersToLirWithOptions(app_body, .{ .specialization_strategy = strategy });
    }
}
