# repro for https://github.com/roc-lang/roc/issues/10788
#
# Same re-wrapping update as `Sh.roc`, written inside an inline lambda argument
# so the receiver is still a flex var when the nominal constructor backing
# relation runs. Whichever way the checker settles this program, compiling it
# has to terminate.
Sc := { depth : U8, n : U64 }.{
    f : List(Sc) -> List(Sc)
    f = |xs| List.map(xs, |x| Sc.{ ..x, depth: 1 })
}

expect List.len(Sc.f([])) == 0

expect 1 + 1 == 2
