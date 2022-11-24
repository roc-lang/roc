app "type-error"
    packages { pf: "../../../../examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout.{ line }, pf.Task.{ await }, pf.Path, pf.File]
    provides [main] to pf

main =
    task =
        _ <- await (line "a")
        _ <- await (line "b")
        _ <- await (line "c")
        _ <- await (line "d")
        _ <- await (File.readUtf8 (Path.fromStr "blah.txt"))
        line "e"

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "Done!"
            # Type mismatch because the File.readUtf8 error case is not handled
            Err {} -> Stdout.line "Problem!"
