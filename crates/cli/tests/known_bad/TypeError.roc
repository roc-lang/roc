app "type-error"
    packages { pf: "../../../../examples/interactive/cli-platform/main.roc" }
    imports [pf.Stdout.{ line }, pf.Task.{ await }, pf.Program]
    provides [main] to pf

main =
    _ <- await (line "a")
    _ <- await (line "b")
    _ <- await (line "c")
    _ <- await (line "d")
    line "e"
    # Type mismatch because this line is missing:
    # |> Program.quick
