app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    _ =
        authenticate!({})
        |> Result.on_err!(\BadPass ->
            Effect.put_line!("LOG: Failed login attempt")
            Ok("Bad password"))

    {}

authenticate! : {} => Result Str [BadPass]
authenticate! = \{} ->
    Effect.put_line!("Enter your password:")

    password = Effect.get_line!({})

    if password == "password" then
        Ok("You are in")
    else
        Err(BadPass)
