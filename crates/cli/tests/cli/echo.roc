app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import pf.Stdin
import pf.Stdout
import pf.Task exposing [Task]

main =
    _ <- Task.await (Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂")

    Task.loop {} tick

tick : {} -> Task [Step {}, Done {}] _
tick = \{} ->
    when Stdin.line |> Task.result! is
        Ok str -> Stdout.line (echo str) |> Task.map Step
        Err (StdinErr EndOfFile) -> Stdout.line (echo "Received end of input (EOF).") |> Task.map Done
        Err (StdinErr err) -> Stdout.line (echo "Unable to read input $(Inspect.toStr err)") |> Task.map Done

echo : Str -> Str
echo = \shout ->
    silence = \length ->
        spaceInUtf8 = 32

        List.repeat spaceInUtf8 length

    shout
    |> Str.toUtf8
    |> List.mapWithIndex
        (\_, i ->
            length = (List.len (Str.toUtf8 shout) - i)
            phrase = (List.split (Str.toUtf8 shout) length).before

            List.concat (silence (if i == 0 then 2 * length else length)) phrase)
    |> List.join
    |> Str.fromUtf8
    |> Result.withDefault ""
