app "echo"
    packages { pf: "platform" }
    imports [ pf.Stdin, pf.Stdout, pf.Task ]
    provides [ main ] to pf

main : Task.Task {} []
main =
    _ <- Task.await (Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂")
    Task.loop {} (\_ -> Task.map tick Step)

tick : Task.Task {} []
tick =
    shout <- Task.await Stdin.line
    Stdout.line (echo shout)

echo : Str -> Str
echo = \shout ->
    silence = \length ->
        spaceInUtf8 = 32

        List.repeat length spaceInUtf8

    shout
        |> Str.toUtf8
        |> List.mapWithIndex
        (\i, _ ->
                length = (List.len (Str.toUtf8 shout) - i)
                phrase = (List.split (Str.toUtf8 shout) length).before

                List.concat (silence (if i == 0 then 2 * length else length)) phrase)
        |> List.join
        |> Str.fromUtf8
        |> Result.withDefault ""
