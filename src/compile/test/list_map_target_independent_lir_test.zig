//! Regression test guarding that lowering produces a target-independent LIR op
//! stream — the property that lets one lowered LIR image be cached across 32-bit
//! and 64-bit targets.
//!
//! The sharp case is `List.map`'s in-place reuse. Reuse is allowed only when the
//! input and output element layouts are interchangeable in one allocation, which
//! depends on pointer width through `@max(ptr_width, alignment)`. Mapping a
//! `List(U64)` to a `List((U32, U32))` is the minimal case where the two widths
//! disagree: both element layouts are 8 bytes wide on both targets, but `U64`
//! has alignment 8 while `(U32, U32)` has alignment 4, so
//!   - 64-bit: max(8, 8) == max(8, 4)  -> interchangeable
//!   - 32-bit: max(4, 8) != max(4, 4)  -> not interchangeable
//!
//! Lowering must therefore emit the same op stream for both widths, carrying the
//! per-width interchangeability on the `list_map_can_reuse` op so each backend
//! resolves it for the width it is building. If a regression baked that decision
//! for a single width again, the in-place branch would be present for one width
//! and dropped for the other, and the two LIR dumps would diverge here.

const harness = @import("lower_to_lir_harness.zig");
const expectTargetIndependentLir = harness.expectTargetIndependentLir;

test "list_map_can_reuse: width-divergent interchangeability lowers to one op stream" {
    try expectTargetIndependentLir(
        \\main! = |_args| {
        \\    nums : List(U64)
        \\    nums = [1, 2, 3]
        \\    pairs : List((U32, U32))
        \\    pairs = nums.map(|_n| (0, 0))
        \\    match pairs.len() {
        \\        0 => Err(Exit(1))
        \\        _ => Ok({})
        \\    }
        \\}
    );
}
