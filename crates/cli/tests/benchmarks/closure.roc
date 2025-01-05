app [main] { pf: platform "platform/main.roc" }

main : Task {} []
main =
    closure1({})
    |> Task.await(\_ -> closure2({}))
    |> Task.await(\_ -> closure3({}))
    |> Task.await(\_ -> closure4({}))
# ---
closure1 : {} -> Task {} []
closure1 = \_ ->
    Task.ok(foo(to_unit_borrowed, "a long string such that it's malloced"))
    |> Task.map(\_ -> {})

to_unit_borrowed = \x -> Str.count_utf8_bytes(x)

foo = \f, x -> f(x)

# ---
closure2 : {} -> Task {} []
closure2 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Task.ok({})
    |> Task.map(\_ -> x)
    |> Task.map(to_unit)

to_unit = \_ -> {}

# # ---
closure3 : {} -> Task {} []
closure3 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Task.ok({})
    |> Task.await(\_ -> Task.ok(x) |> Task.map(\_ -> {}))

# # ---
closure4 : {} -> Task {} []
closure4 = \_ ->
    x : Str
    x = "a long string such that it's malloced"

    Task.ok({})
    |> Task.await(\_ -> Task.ok(x))
    |> Task.map(\_ -> {})
