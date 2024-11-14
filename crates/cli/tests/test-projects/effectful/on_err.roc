app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    _ =
        authenticate! {}
        |> Result.onErr! \BadPass ->
            Effect.putLine! "LOG: Failed login attempt"
            Ok "Bad password"

    {}

authenticate! : {} => Result Str [BadPass]
authenticate! = \{} ->
    Effect.putLine! "Enter your password:"

    password = Effect.getLine! {}

    if password == "password" then
        Ok "You are in"
    else
        Err BadPass
