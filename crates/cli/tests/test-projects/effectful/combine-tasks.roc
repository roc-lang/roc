app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

main =
    multipleIn =
        { sequential <-
            a: Task.ok 123,
            b: Task.ok "abc",
            c: Task.ok [123],
            _d: Task.ok ["abc"],
            _: Task.ok (Dict.single "a" "b"),
        }!

    Stdout.line! "For multiple tasks: $(Inspect.toStr multipleIn)"

sequential : Task a err, Task b err, (a, b -> c) -> Task c err
sequential = \firstTask, secondTask, mapper ->
    first = firstTask!
    second = secondTask!
    Task.ok (mapper first second)
