app [main!] {
    pf: platform "./platform/main.roc",
    thing: "./pkg/main.roc",
}

import thing.Thing
import pf.ThingFx

main! = || {
    thing = Thing.new(4)

    _ = ThingFx.is_even(thing)
    # repro for https://github.com/roc-lang/roc/issues/9864
    _ = thing.is_even()

    {}
}
