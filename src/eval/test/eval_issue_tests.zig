//! Focused regression repros for GitHub issues.

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
    .{
        // A nominal record whose declared field order ({ id, balance }) differs
        // from alphabetical order ({ balance, id }). It is constructed in the
        // imported module and its fields read in the main module, so an
        // inconsistent cross-module layout would read the wrong bytes.
        .name = "nominal record imported across modules reads correct fields",
        .source_kind = .module,
        .imports = &.{.{
            .name = "Acct",
            .source =
            \\module [Account, sample]
            \\
            \\Account := { id : U8, balance : U32 }
            \\
            \\sample : Account
            \\sample = { id : 7, balance : 99 }
            ,
        }},
        .source =
        \\import Acct exposing [Account]
        \\
        \\describe : Account -> U32
        \\describe = |{ id, balance }| id.to_u32() * 1000 + balance
        \\
        \\main = describe(Acct.sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
    .{
        // A nominal record with unnamed padding fields (mirroring a C struct's
        // explicit padding). Construction provides only the named fields and the
        // padding bytes stay reserved; reading the named fields back must use the
        // offsets that account for the padding (a@0, b@4), not packed offsets.
        .name = "nominal record with unnamed padding reads correct fields",
        .source_kind = .module,
        .source =
        \\Padded := { a : U8, _ : U8, _ : U8, _ : U8, b : U32 }
        \\
        \\sample : Padded
        \\sample = { a : 7, b : 99 }
        \\
        \\describe : Padded -> U32
        \\describe = |{ a, b }| a.to_u32() * 1000 + b
        \\
        \\main = describe(sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
    .{
        // An unnamed padding field whose type is refcounted (`Str`). Its bytes
        // are uninitialized garbage and must never be refcounted, compared, or
        // inspected — only its size is reserved. If the padding spacer were
        // treated as a live Str, dropping the value would decref garbage and
        // crash, so this exercises the refcount/equality padding skip on every
        // backend.
        .name = "nominal record with refcounted-typed padding is never refcounted",
        .source_kind = .module,
        .source =
        \\Padded := { a : U8, _ : Str, b : U32 }
        \\
        \\sample : Padded
        \\sample = { a : 7, b : 99 }
        \\
        \\describe : Padded -> U32
        \\describe = |{ a, b }| a.to_u32() * 1000 + b
        \\
        \\main = describe(sample)
        ,
        .expected = .{ .inspect_str = "7099" },
    },
};
