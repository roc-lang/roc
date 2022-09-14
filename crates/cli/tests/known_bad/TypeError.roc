app "type-error"
    packages { pf: "../../../../examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout.{ line }, pf.Task.{ await }]
    provides [main] to pf

main = \_args ->
    _ <- await (line "a")
    _ <- await (line "b")
    _ <- await (line "c")
    _ <- await (line d)
    line "e"
