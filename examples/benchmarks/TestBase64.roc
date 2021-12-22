app "test-base64"
    packages { pf: "platform" }
    imports [pf.Task, Base64 ]
    provides [ main ] to pf

IO a b : Task.Task a b

main : IO {} _
main =
    when Base64.fromBytes (Str.toUtf8 "Hello World") is
        Err _ ->
            Task.putLine "sadness"

        Ok encoded ->
            Task.after (Task.putLine (Str.concat "encoded: " encoded)) \_ ->
                when Base64.toStr encoded is
                    Ok decoded -> 
                        Task.putLine (Str.concat "decoded: " decoded)

                    Err _ -> 
                        Task.putLine "sadness"

