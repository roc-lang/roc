//! Focused regression repros for GitHub issues #8900-#8999.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "issue 8949: wasm evaluates to_str after boxed closure allocation",
        .source_kind = .module,
        .source =
        \\State : { count : I64 }
        \\
        \\main = {
        \\    initialState : State
        \\    initialState = { count: 42 }
        \\    _updater = Box.box(|state| { count: state.count + 1 })
        \\    countStr = initialState.count.to_str()
        \\
        \\    "Count: ${countStr}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Count: 42\"" },
    },
};
