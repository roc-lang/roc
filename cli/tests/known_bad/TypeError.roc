app "type-error"
    packages { pf: "platform" }
    imports [ pf.Stdout.{ line }, pf.Task.{ await } ]
    provides [ main ] to pf

main =
    _ <- await (line "a")
    _ <- await (line "b")
    _ <- await (line "c")
    _ <- await (line d)
    line "e"
