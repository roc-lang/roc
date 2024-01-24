app "countdown"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ Task }]
    provides [main] to pf

main : Task {} I32
main =
    _ <- Stdout.line "\nLet's count down from 10 together - all you have to do is press <ENTER>." |> Task.await
    _ <- Stdin.line |> Task.await

    Task.loop 10 tick

tick : I64 -> Task [Done {}, Step I64] *
tick = \n ->
    if n == 0 then
        _ <- Stdout.line "🎉 SURPRISE! Happy Birthday! 🎂" |> Task.await

        Task.ok (Done {})
    else
        _ <-
            Num.toStr n
            |> \s -> "\(s)..."
            |> Stdout.line
            |> Task.await

        _ <- Stdin.line |> Task.await

        Task.ok (Step (n - 1))
