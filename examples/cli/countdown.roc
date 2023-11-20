app "countdown"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, loop, succeed }]
    provides [main] to pf

main =
    _ <- await (Stdout.line "\nLet's count down from 10 together - all you have to do is press <ENTER>.")
    _ <- await Stdin.line
    loop 10 tick

tick = \n ->
    if n == 0 then
        _ <- await (Stdout.line "🎉 SURPRISE! Happy Birthday! 🎂")
        succeed (Done {})
    else
        _ <- await (n |> Num.toStr |> \s -> "\(s)..." |> Stdout.line)
        _ <- await Stdin.line
        succeed (Step (n - 1))
