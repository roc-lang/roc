app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    first = ask! "What's your first name?"
    last = ask! "What's your last name?"

    Effect.putLine! "\nHi, $(first) $(last)!\n"

    when Str.toU8 (ask! "How old are you?") is
        Err InvalidNumStr ->
            Effect.putLine! "Enter a valid number"

        Ok age if age >= 18 ->
            Effect.putLine! "\nNice! You can vote!"

        Ok age ->
            Effect.putLine! "\nYou'll be able to vote in $(Num.toStr (18 - age)) years"

    Effect.putLine! "\nBye! 👋"

ask! : Str => Str
ask! = \question ->
    Effect.putLine! question
    Effect.getLine! {}
