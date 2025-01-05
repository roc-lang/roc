app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

main =
    multiple_in =
        { sequential <-
            a: Task.ok(123),
            b: Task.ok("abc"),
            c: Task.ok([123]),
            _d: Task.ok(["abc"]),
            _: Task.ok(Dict.single("a", "b")),
        }!

    Stdout.line!("For multiple tasks: $(Inspect.to_str(multiple_in))")

sequential : Task a err, Task b err, (a, b -> c) -> Task c err
sequential = \first_task, second_task, mapper ->
    first = first_task!
    second = second_task!
    Task.ok(mapper(first, second))
