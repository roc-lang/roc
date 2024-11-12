app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} -> tick! {}

tick! = \{} ->
    line = Effect.getLine! {}

    if !(Str.isEmpty line) then
        Effect.putLine! (echo line)
    else
        Effect.putLine! "Received no input."

echo : Str -> Str
echo = \shout ->
    silence = \length -> List.repeat ' ' length

    shout
    |> Str.toUtf8
    |> List.mapWithIndex \_, i ->
        length = (List.len (Str.toUtf8 shout) - i)
        phrase = (List.splitAt (Str.toUtf8 shout) length).before

        List.concat (silence (if i == 0 then 2 * length else length)) phrase
    |> List.join
    |> Str.fromUtf8
    |> Result.withDefault ""

expect
    message = "hello!"
    echoedMessage = echo message

    echoedMessage == "            hello!     hello    hell   hel  he h"
