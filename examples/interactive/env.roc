app "env"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Env, pf.Task, pf.Program.{ Program }]
    provides [main] to pf

main : Program
main =
    Env.decode "EDITOR"
    |> Task.await (\editor -> Stdout.line "Your favorite editor is \(editor)!")
    |> Task.await (\{} -> Env.decode "SHLVL")
    |> Task.await (\lvl ->
        when lvl is
            1u8 -> Stdout.line "You're running this in a root shell!"
            n ->
                lvlStr = Num.toStr n
                Stdout.line "Your current shell level is \(lvlStr)!")
    |> Program.quick
