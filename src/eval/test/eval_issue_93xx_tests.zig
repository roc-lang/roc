//! Focused regression repros for GitHub issues #9389-#9396.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "issue 9389: non-existent list method reports a problem instead of crashing",
        .source_kind = .module,
        .source =
        \\Min :: [].{
        \\    foo : List(a) -> List(a)
        \\    foo = |list| list.reverse()
        \\}
        \\
        \\main = Min.foo([0])
        ,
        .expected = .{ .problem = {} },
    },
    .{
        .name = "issue 9390: var initialized from polymorphic list.len reads back as U64",
        .source_kind = .module,
        .source =
        \\foo : List(a) -> U64
        \\foo = |list| {
        \\    var $idx = list.len()
        \\    $idx
        \\}
        \\
        \\main = foo([0])
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "issue 9390: var initialized from polymorphic list.len controls one loop iteration",
        .source_kind = .module,
        .source =
        \\foo : List(a) -> U64
        \\foo = |list| {
        \\    var $idx = list.len()
        \\    var $iterations = 0
        \\    while $idx > 0 {
        \\        $idx = $idx - 1
        \\        $iterations = $iterations + 1
        \\    }
        \\    $iterations
        \\}
        \\
        \\main = foo([0])
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "issue 9391: fractional method receiver specializes to F64 argument",
        .source_kind = .module,
        .source =
        \\f : U64 -> F64
        \\f = |_n| 0.001
        \\
        \\main = 0.001.is_lte(f(3))
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9393: greater-than-or-equal on numeric list element terminates",
        .source_kind = .module,
        .source =
        \\uppercase_help : List(I64) -> List(I64)
        \\uppercase_help = |bytes| {
        \\    first = bytes.get(0).ok_or(0)
        \\    if is_lowercase(first) bytes else bytes
        \\}
        \\
        \\is_lowercase = |c| c >= 97
        \\
        \\main = [100]->uppercase_help
        ,
        .expected = .{ .inspect_str = "[100]" },
    },
    .{
        .name = "issue 9395: List.all static dispatch on list literal terminates",
        .source_kind = .module,
        .source =
        \\bar : U8 -> Bool
        \\bar = |_c| Bool.True
        \\
        \\main = [0].all(bar)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9395: List.any static dispatch on list literal terminates",
        .source_kind = .module,
        .source =
        \\bar : U8 -> Bool
        \\bar = |_c| Bool.True
        \\
        \\main = [0].any(bar)
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "issue 9396: undefined identifier in type-module method body reports a problem",
        .source_kind = .module,
        .source =
        \\Min :: [].{
        \\    foo : U8 -> Bool
        \\    foo = |_bar| U8.is_zero(baz)
        \\}
        \\
        \\main = Min.foo(0)
        ,
        .expected = .{ .problem = {} },
    },
};
