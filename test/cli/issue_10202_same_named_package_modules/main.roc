app [main!] {
    pf: platform "../../fx/platform/main.roc",
    first: "./first/main.roc",
    second: "./second/main.roc",
}

import first.Random as FirstRandom
import second.Random

# repro for https://github.com/roc-lang/roc/issues/10202
main! = || {
    _ = FirstRandom.from_first({})
    _ = Random.from_second({})

    {}
}
