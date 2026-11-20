app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Noiter twin of iter_for_static_lib_app.roc: the same sums computed by
# `for` over plain list literals, with no iterator adapters. Its
# `--opt=size` wasm size is the baseline; the iter build's size minus this is
# the iterator-adapter "premium" that CI tracks.
main! : U64 => Str
main! = |_seed| {
    var $append_sum = 0.U64
    for x in [1.U64, 2, 3, 9] {
        $append_sum = $append_sum + x
    }

    var $map_sum = 0.U64
    for x in [2.U64, 3, 4] {
        $map_sum = $map_sum + x
    }

    var $concat_sum = 0.U64
    for x in [1.U64, 2, 3, 4] {
        $concat_sum = $concat_sum + x
    }

    var $chain_sum = 0.U64
    for x in [11.U64, 21, 100] {
        $chain_sum = $chain_sum + x
    }

    if $append_sum == 15 and $map_sum == 9 and $concat_sum == 10 and $chain_sum == 132 {
        "ok"
    } else {
        "bad"
    }
}
