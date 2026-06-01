//! Regression test for GitHub issue #9502.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        // Calling `Foo.inspect(Baz)` leaves the nominal type parameter `a`
        // unconstrained: `Baz` carries no `a`, so the `where [a.inspect : a -> Str]`
        // clause has nothing to bind `a` to. Earlier this panicked in mono
        // specialization ("constrained variable matched multiple checked method
        // owners"). The body never invokes the constraint, so the owner is
        // irrelevant and the call simply evaluates to "ok".
        .name = "issue 9502: static dispatch on no-payload variant of polymorphic nominal evaluates",
        .source_kind = .module,
        .source =
        \\Foo(a) := [Bar(a), Baz].{
        \\    inspect : Foo(a) -> Str where [a.inspect : a -> Str]
        \\    inspect = |_foo| "ok"
        \\}
        \\
        \\main = Foo.inspect(Baz)
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
};
