app "env"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Env, pf.Task.{ Task }]
    provides [main] to pf

main : Task {} []
main =
    task =
        Env.decode "EDITOR"
        |> Task.await (\editor -> Stdout.line "Your favorite editor is \(editor)!")
        |> Task.await (\{} -> Env.decode "SHLVL")
        |> Task.await
            (\lvl ->
                when lvl is
                    1u8 -> Stdout.line "You're running this in a root shell!"
                    n ->
                        lvlStr = Num.toStr n

                        Stdout.line "Your current shell level is \(lvlStr)!")
        |> Task.await \{} -> Env.decode "LETTERS"

    Task.attempt task \result ->
        when result is
            Ok letters ->
                joinedLetters = Str.joinWith letters " "

                Stdout.line "Your favorite letters are: \(joinedLetters)"

            Err _ ->
                Stderr.line "I couldn't find your favorite letters in the environment variables!"
