//! Regression test for issue #12221.

const harness = @import("lower_to_lir_harness.zig");
const lir = @import("lir");

// Repro for https://github.com/roc-lang/roc/issues/12221: `suffix` is a static
// `[48]` literal on one branch and a fresh `List.repeat` on the other, so it is
// not unique and the `List.append` on it must not be marked check-free. ARC
// certification accepts the lowered program in every inline mode.
test "issue 12221: a static literal joined with a fresh list is not claimed unique by append" {
    for ([_]lir.CheckedPipeline.InlineMode{ .none, .wrappers, .wrappers_and_source_single_use }) |inline_mode| {
        try harness.expectLowersToLirWithOptions(
            \\line_for : U64 -> List(U8)
            \\line_for = |n| {
            \\    suffix = if n == 0 [48] else List.repeat(49, n)
            \\    suffix.append(10)
            \\}
            \\
            \\main! = |args| {
            \\    echo!(Str.from_utf8_lossy(line_for(args.len() // 100)))
            \\    Ok({})
            \\}
        , .{
            .inline_mode = inline_mode,
            .include_internal_static_data = true,
            .consume_dead_boxes = true,
        });
    }
}
