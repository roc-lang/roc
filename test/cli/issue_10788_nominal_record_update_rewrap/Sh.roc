# repro for https://github.com/roc-lang/roc/issues/10788
#
# `Sh.{ ..x, depth: 1 }` re-wraps an already-nominal receiver, so the nominal
# constructor backing relation rejects it and the checker reports INVALID
# NOMINAL RECORD. The poisoned root must lower the way every other checked
# error does, and the independent test root below must still run.
Sh := { depth : U8, n : U64 }.{
    f : Sh -> Sh
    f = |x| Sh.{ ..x, depth: 1 }
}

expect Sh.f(Sh.{ depth: 0, n: 0 }).depth == 1

expect 1 + 1 == 2
