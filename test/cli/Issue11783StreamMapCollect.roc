app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

# Repro for https://github.com/roc-lang/roc/issues/11783
#
# A Stream pipeline must specialize/fuse like the equivalent List pipeline.
# This program prints the collected length; the CLI runner case for this file
# enforces a wall-clock budget that only a specialized (fused) stream pipeline
# can meet.

main! = |_args| {
    var $acc = 0.U64
    var $k = 0.U64
    base = List.repeat(1.U64, 5_000_000)
    while $k < 10 {
        xs = base.iter().stream().map(|x| x * 2 + $k).collect!()
        $acc = $acc + xs.len()
        $k = $k + 1
    }
    Stdout.line!($acc.to_str())
    Ok({})
}
