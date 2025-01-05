app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    first = ask!("What's your first name?")
    last = ask!("What's your last name?")

    Effect.put_line!("\nHi, $(first) $(last)!\n")

    when Str.to_u8(ask!("How old are you?")) is
        Err(InvalidNumStr) ->
            Effect.put_line!("Enter a valid number")

        Ok(age) if age >= 18 ->
            Effect.put_line!("\nNice! You can vote!")

        Ok(age) ->
            Effect.put_line!("\nYou'll be able to vote in $(Num.to_str((18 - age))) years")

    Effect.put_line!("\nBye! 👋")

ask! : Str => Str
ask! = \question ->
    Effect.put_line!(question)
    Effect.get_line!({})
