app "main"
    packages { pf: "cli-platform" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, loop, succeed }]
    provides [main] to pf

main =
    _ <- await (Stdout.line "Please input some lines")
    lines <- readLines []
    _ <- printLines lines
    # -- joinedLines = Str.joinWith lines ", "
    # -- _ <- await (Stdout.line "You input \(joinedLines)")
    succeed {}


readLines = \lines, after ->
    line <- await Stdin.line
    if line == "" then
        after lines
    else
        lines
        |> List.append line
        |> readLines after

printLines = \lines, after ->
    lines
        |> List.walk (succeed {}) \_, line ->
            _ <- await (Stdout.line "You input: \(line)")
            succeed {}
        |> after
