## Regression repro for https://github.com/roc-lang/roc/issues/10844.
##
## Each `deep` call keeps sixteen interpolated strings live across its
## recursive call: they are all re-consumed AFTER the recursion returns, and
## string construction goes through the allocator, so no optimization level can
## drop them from the frame. At 20,000 levels the optimized (`--opt=speed`)
## build needs roughly 26 MiB of stack -- comfortably inside the 64 MiB the
## `roc` executable reserves for Roc recursion (see `exe.stack_size` in
## build.zig), so these expects must pass no matter which thread the test
## runner executes them on.
##
## There are two expects on purpose: a single test root runs on the calling
## thread, while two or more make the optimized runner spawn worker threads,
## which is the configuration this file exists to exercise.

DeepStackParallel := {}.{
    deep : U64 -> U64
    deep = |n| {
        if n == 0 {
            0
        } else {
            s0 = "${n.to_str()}-abcdefghijklmnopqrstuvwxyz"
            s1 = "${s0}-01"
            s2 = "${s1}-02"
            s3 = "${s2}-03"
            s4 = "${s3}-04"
            s5 = "${s4}-05"
            s6 = "${s5}-06"
            s7 = "${s6}-07"
            s8 = "${s7}-08"
            s9 = "${s8}-09"
            s10 = "${s9}-10"
            s11 = "${s10}-11"
            s12 = "${s11}-12"
            s13 = "${s12}-13"
            s14 = "${s13}-14"
            s15 = "${s14}-15"
            rest = deep(n - 1)
            combined = "${rest.to_str()}|${s0}|${s1}|${s2}|${s3}|${s4}|${s5}|${s6}|${s7}|${s8}|${s9}|${s10}|${s11}|${s12}|${s13}|${s14}|${s15}"
            rest + Str.count_utf8_bytes(combined)
        }
    }
}

expect DeepStackParallel.deep(20000) == 17729634
expect DeepStackParallel.deep(20001) == 17730530
