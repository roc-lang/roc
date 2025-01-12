app [main!] { pf: platform "platform/main.roc" }

main! : () => ()
main! = \() ->
    closure1(())
    |> Result.try(closure2)
    |> Result.try(closure3)
    |> Result.try(closure4)
    |> Result.with_default(())

# ---
closure1 : () -> Result () []
closure1 = \_ ->
    Ok(foo(to_unit_borrowed, "a long string such that it's malloced"))
    |> Result.map(\_ -> ())

to_unit_borrowed = \x -> Str.count_utf8_bytes(x)

foo = \f, x -> f(x)

# ---
closure2 : () -> Result () []
closure2 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Ok(())
    |> Result.map(\_ -> x)
    |> Result.map(to_unit)

to_unit = \_ -> ()

# # ---
closure3 : () -> Result () []
closure3 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Ok(())
    |> Result.try(\_ -> Ok(x) |> Result.map(\_ -> ()))

# # ---
closure4 : () -> Result () []
closure4 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Ok(())
    |> Result.try(\_ -> Ok(x))
    |> Result.map(\_ -> ())
