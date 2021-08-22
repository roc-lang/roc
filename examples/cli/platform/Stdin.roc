interface Stdin
    exposes [ getInt ]
    imports [ fx.Effect, Task ]


getInt : Task.Task I64 []
getInt =
    Effect.after Effect.getInt \{ isError, value, errorCode } ->
        when isError is
            True ->
                when errorCode is
                    # A -> Task.fail InvalidCharacter
                    # B -> Task.fail IOError
                    _ -> Task.succeed -1

            False ->
                Task.succeed value
