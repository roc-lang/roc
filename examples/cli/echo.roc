app "echo"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ Task }]
    provides [main] to pf

main : Task {} I32
main =
    _ <- Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂" |> Task.await

    Task.loop {} \_ -> Task.map tick Step

tick : Task.Task {} I32
tick =
    shout <- Stdin.line |> Task.await

    when shout is
        End -> Task.ok {}
        Input shoutStr -> Stdout.line (echo shoutStr)

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
