app "http-get"
    packages { base: "platform" }
    imports [ base.Task.{ await }, base.Stdout, base.Stdin, base.Http ]
    provides [ main ] to base

main : Task.Task {} *
main =
    {} <- await (Stdout.line "What URL should I get?")

    url <- await Stdin.line

    {} <- await (Stdout.line "The contents of \(url) are: ")

    contents <- await (Http.getUtf8 url)

    Stdout.line contents
