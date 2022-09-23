app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ Task }, pf.Program.{ Program, ExitCode }]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : List Str -> Task ExitCode [] [Read [Stdin], Write [Stdout]]
mainTask = \_args ->
    _ <- Task.await (Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂")
    Task.loop {} (\_ -> Task.map tick Step)
    |> Program.exit 0

tick : Task.Task {} [] [Read [Stdin]*, Write [Stdout]*]*
tick =
    shout <- Task.await Stdin.line
    Stdout.line (echo shout)

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
