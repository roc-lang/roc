app "form"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Stderr, pf.Task.{ Task }]
    provides [main] to pf

main : Task {} I32
main =

    _ <- Stdout.line "What's your first name?" |> Task.await
    firstName <- Stdin.line |> Task.await

    _ <- Stdout.line "What's your last name?" |> Task.await
    lastName <- Stdin.line |> Task.await

    when (firstName, lastName) is
        (Input firstNameStr, Input lastNameStr) -> Stdout.line "Hi, \(firstNameStr) \(lastNameStr)! 👋"
        _ -> Stderr.line "expected some input"
