app "http-get"
    packages { base: "platform" }
    imports [ base.Task.{ await }, base.Stdout, base.Stdin, base.Http ]
    provides [ main ] to base

main : Task.Task {} *
main =
    _ <- await (Stdout.line "What URL should I get?")

    url <- await Stdin.line

    result <- Task.attempt (Http.getUtf8 url)

    when result is
        Ok contents -> Stdout.line "The contents of \(url) are:\n\(contents)"
        Err err -> Stdout.line "Error retrieving \(url) - error was: \(err)"
