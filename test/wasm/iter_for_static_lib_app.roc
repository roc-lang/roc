app [main!] { pf: platform "./static-lib-platform/main.roc" }

# End-to-end cart gate for the minted-iterator `for`-loop drive on the
# `--opt=size` (LLVM) build path. A `for` over a minted chain sinks the loop
# into the chain and rebases the step's inline captures; a drive that loses the
# advanced successor `rest` freezes the inner iterator and never terminates
# (the regression that hung the Rocci cart at boot). Each `for` here is inlined
# in `main!` so it exercises that drive directly, and the whole app must boot
# and return "ok".
main! : U64 => Str
main! = |_seed| {
    var append_sum = 0.U64
    for x in [1.U64, 2, 3].iter().append(9) {
        append_sum = append_sum + x
    }

    var map_sum = 0.U64
    for x in [1.U64, 2, 3].iter().map(|n| n + 1) {
        map_sum = map_sum + x
    }

    var concat_sum = 0.U64
    for x in [1.U64, 2].iter().concat([3.U64, 4].iter()) {
        concat_sum = concat_sum + x
    }

    var chain_sum = 0.U64
    for x in [10.U64, 20].iter().map(|n| n + 1).append(100) {
        chain_sum = chain_sum + x
    }

    if append_sum == 15 and map_sum == 9 and concat_sum == 10 and chain_sum == 132 {
        "ok"
    } else {
        "bad"
    }
}
