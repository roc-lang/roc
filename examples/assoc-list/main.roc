app "main"
    packages { pf: "cli-platform" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, loop, succeed }, AssocList]
    provides [main] to pf

main =
    _ <- await (Stdout.line "Input pairs of lines which will be used to build an association.\nQuit by inputting an empty line.")
    _assocs <- readAssociations AssocList.empty
    succeed {}


readAssociations = \assocs, after ->
    key <- await Stdin.line
    if key == "" then
        _ <- printAssociations assocs
        after assocs
    else
        value <- await Stdin.line
        res = AssocList.insert assocs key value
        readAssociations res after

printAssociations = \assocs, after ->
    _ <- await (Stdout.line "Associations right now:")
    assocs
        |> AssocList.walk (succeed {}) \_, key, value ->
            _ <- await (Stdout.line "\(key) => \(value)")
            succeed {}
        |> after
