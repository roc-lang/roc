app "countdown"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, loop }]
    provides [main] to pf

main =
    _ <- await (Stdout.line "\nLet's count down from 3 together - all you have to do is press <ENTER>.")
    _ <- await Stdin.line
    loop 3 tick

tick = \n ->
    if n == 0 then
        _ <- await (Stdout.line "🎉 SURPRISE! Happy Birthday! 🎂")
        Task.ok (Done {})
    else
        _ <- await (n |> Num.toStr |> \s -> "\(s)..." |> Stdout.line)
        _ <- await Stdin.line
        Task.ok (Step (n - 1))
