# Regression fixture for https://github.com/roc-lang/roc/issues/10156. The
# comparisons below are over types that contain `Str.Utf8Problem` (inside
# `Str.from_utf8`'s error payload). `Utf8Problem.is_eq` is the compiler's only
# structural_eq intrinsic wrapper, and lowering these comparisons requests that
# wrapper as a standalone procedure, which used to panic in Debug/ReleaseSafe
# ("structural equality intrinsic wrapper must lower through checked dispatch
# plans") and miscompile in ReleaseFast.
import Split

Main := [].{
    run = |bytes| {
        { head, tail } = match Split.split_on(bytes, ['\r', '\n']) {
            Ok({ before: b, after: a }) => Ok({ head: b, tail: a })
            Err(_) => Err(NoDelimiter)
        }?
        match Str.from_utf8(head) {
            Ok(text) => Ok({ text, tail })
            Err(cause) => Err(BadText(head, cause))
        }
    }
}

expect Main.run(Str.to_utf8("\r\nbody")) == Ok({ text: "", tail: Str.to_utf8("body") })

# A correct value must also compare unequal to a near miss.
expect Main.run(Str.to_utf8("\r\nbody")) != Ok({ text: "", tail: Str.to_utf8("BODY") })

# The expects above only compare `Ok` values, so the `Utf8Problem.is_eq`
# wrapper body is lowered but never executed. Comparing two equal
# `Err(BadUtf8(...))` values runs it: a wrapper body that dispatches back to
# itself turns this expect into an infinite loop instead of a test result.
expect Str.from_utf8([255]) == Str.from_utf8([255])

# Different problems (invalid start byte vs truncated sequence) must compare
# unequal.
expect Str.from_utf8([255]) != Str.from_utf8([195])
