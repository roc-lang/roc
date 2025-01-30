app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} -> tick!({})

tick! = \{} ->
    line = Effect.get_line!({})

    if !(Str.is_empty(line)) then
        Effect.put_line!(echo(line))
    else
        Effect.put_line!("Received no input.")

echo : Str -> Str
echo = \shout ->
    silence = \length -> List.repeat(' ', length)

    shout
    |> Str.to_utf8
    |> List.map_with_index(\_, i ->
        length = (List.len(Str.to_utf8(shout)) - i)
        phrase = (List.split_at(Str.to_utf8(shout), length)).before

        List.concat(silence((if i == 0 then 2 * length else length)), phrase))
    |> List.join
    |> Str.from_utf8
    |> Result.with_default("")

expect
    message = "hello!"
    echoed_message = echo(message)

    echoed_message == "            hello!     hello    hell   hel  he h"
