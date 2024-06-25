app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Stdin
import pf.Stdout

main =
    _ <- Task.await (Stdout.line "\nLet's count down from 3 together - all you have to do is press <ENTER>.")
    _ <- Task.await Stdin.line
    Task.loop 3 tick

tick = \n ->
    if n == 0 then
        _ <- Task.await (Stdout.line "🎉 SURPRISE! Happy Birthday! 🎂")
        Task.ok (Done {})
    else
        _ <- Task.await (n |> Num.toStr |> \s -> "$(s)..." |> Stdout.line)
        _ <- Task.await Stdin.line
        Task.ok (Step (n - 1))
