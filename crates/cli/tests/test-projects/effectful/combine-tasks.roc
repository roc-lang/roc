app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.PlatformTasks

main! = \{} ->
    multipleIn =
        { sequential <-
            a: Ok 123,
            b: Ok "abc",
            c: Ok [123],
            _d: Ok ["abc"],
            _: Ok (Dict.single "a" "b"),
        }?

    PlatformTasks.putLine! "For multiple tasks: $(Inspect.toStr multipleIn)"

sequential : Result a err, Result b err, (a, b -> c) -> Result c err
sequential = \firstTask, secondTask, mapper ->
    first = firstTask
    second = secondTask
    Ok (mapper first second)
