app [main] { pf: platform "platform/main.roc" }

import pf.PlatformTasks

main =
    multipleIn =
        { sequential <-
            a: Task.ok 123,
            b: Task.ok "abc",
            c: Task.ok [123],
            _d: Task.ok ["abc"],
            _: Task.ok (Dict.single "a" "b"),
        }!

    PlatformTasks.putLine! "For multiple tasks: $(Inspect.toStr multipleIn)"

sequential : Task a err, Task b err, (a, b -> c) -> Task c err
sequential = \firstTask, secondTask, mapper ->
    first = firstTask!
    second = secondTask!
    Task.ok (mapper first second)
