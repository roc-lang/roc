//! A forward method reference through a nominal alias preserves polymorphism.
const TestEnv = @import("TestEnv.zig");

test "issue 13104: forward alias method uses its polymorphic annotation" {
    var env = try TestEnv.init("Repro",
        \\Counter(tag) : Count(tag)
        \\
        \\Count(tag) :: { n : U64 }.{
        \\    twice : Counter(tag) -> Counter(tag)
        \\    twice = |c| Counter.bump(Counter.bump(c))
        \\
        \\    bump : Counter(tag) -> Counter(tag)
        \\    bump = |Count.(c)| Count.({ n: c.n + 1 })
        \\}
        \\
        \\c : Counter([A])
        \\c = Count.({ n: 0 }).twice()
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13104: alias method reference after its definition" {
    var env = try TestEnv.init("Repro",
        \\Counter(tag) : Count(tag)
        \\
        \\Count(tag) :: { n : U64 }.{
        \\    bump : Counter(tag) -> Counter(tag)
        \\    bump = |Count.(c)| Count.({ n: c.n + 1 })
        \\
        \\    twice : Counter(tag) -> Counter(tag)
        \\    twice = |c| Counter.bump(Counter.bump(c))
        \\}
        \\
        \\c : Counter([A])
        \\c = Count.({ n: 0 }).twice()
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13104: forward alias reference to an unannotated generic method" {
    var env = try TestEnv.init("Repro",
        \\Boxed(a) : Wrap(a)
        \\
        \\Wrap(a) :: { item : a }.{
        \\    both = |x| (Boxed.same(x), Boxed.same("s"), Boxed.same(1.U8))
        \\
        \\    same = |x| x
        \\}
        \\
        \\pair = Wrap.both(Wrap.({ item: 1.U8 }))
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13104: mutually recursive methods referenced through an alias" {
    var env = try TestEnv.init("Repro",
        \\Counter : Count
        \\
        \\Count :: { n : U64 }.{
        \\    down : Counter -> Counter
        \\    down = |Count.(c)| if (c.n == 0) Count.(c) else Counter.step(Count.(c))
        \\
        \\    step : Counter -> Counter
        \\    step = |Count.(c)| Counter.down(Count.({ n: c.n - 1 }))
        \\
        \\    spin = |c| if (Counter.is_zero(c)) c else Counter.spin_more(c)
        \\
        \\    spin_more = |c| Counter.spin(Counter.step(c))
        \\
        \\    is_zero = |Count.(c)| c.n == 0
        \\}
        \\
        \\c : Counter
        \\c = Count.spin(Count.({ n: 3 }))
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13104: a value cycle through an alias is reported as a cycle" {
    var env = try TestEnv.init("Repro",
        \\Counter : Count
        \\
        \\Count :: { n : U64 }.{
        \\    first = Counter.second
        \\
        \\    second = Counter.first
        \\}
    );
    defer env.deinit();
    try env.assertFirstTypeError("Circular Value Definition");
}
