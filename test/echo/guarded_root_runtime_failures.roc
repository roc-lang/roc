# Compile-time-known code inside branches is evaluated while checking. A crash
# there is not a compile error: the original code runs, and crashes, only when
# its branch is taken.

positive : U64 -> Try(U64, [Zero])
positive = |n| if n == 0 { Err(Zero) } else { Ok(n) }

unreachable : Str -> U64
unreachable = |msg| crash msg

fail : Str -> {}
fail = |msg| crash msg

main! = |args| {
    n = if args.len() > 100 { unreachable("untaken branch") } else { 1 }
    if args.len() < 100 {
        Ok(one) = positive(1)
        echo!("validated ${(n + one).to_str()}")
    } else {
        {}
    }
    if args.len() < 100 {
        echo!("before the crash")
        fail("taken branch")
        echo!("after the crash")
    } else {
        {}
    }
    Ok({})
}
