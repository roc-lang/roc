app "main"
    packages { pf: "cli-platform" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ Task, await, loop, succeed }, AssocList.{ AssocList }]
    provides [main] to pf

## An example program
## that takes [AssocList] for a spin.

main =
    _ <- await (Stdout.line "This example program takes the AssocList interface for a spin.")
    _ <- await (Stdout.line "Input pairs of lines.\n Each pair will become an association in the first AssocList.\nFinish by inputting an empty line.")
    assocs1 <- readAssociations AssocList.empty
    _ <- await (Stdout.line "assocs1:")
    _ <- printAssociations assocs1

    _ <- await (Stdout.line "Input pairs of lines.\n Each pair will become an association in the second AssocList.\nFinish by inputting an empty line.")
    assocs2 <- readAssociations AssocList.empty
    _ <- await (Stdout.line "assocs2:")
    _ <- printAssociations assocs2

    _ <- await (Stdout.line "insertAll assocs1 assocs2:")
    _ <- printAssociations (AssocList.insertAll assocs1 assocs2)


    # TODO: There seems to be a problem with 'remove'
    _ <- await (Stdout.line "remove assocs1 \"2\":")
    _ <- printAssociations (AssocList.remove assocs1 "3")

    succeed {}


readAssociations = \assocs, after ->
    key <- await Stdin.line
    if key == "" then
        after assocs
    else
        value <- await Stdin.line
        assocs
          |> AssocList.insert key value
          |> readAssociations after

printAssociations = \assocs, after ->
    (assocs
        |> AssocList.walk (succeed {}) \_, key, value ->
            _ <- await (Stdout.line "\(key) => \(value)")
            succeed {})
        |> after
