app "test-base64"
    packages { base: "platform" }
    imports [base.Task, Base64 ]
    provides [ main ] to base

IO a : Task.Task a []

main : IO {}
main =
    when Base64.fromBytes (Str.toBytes "Hello World") is
        Err _ ->
            Task.putLine "sadness"

        Ok encoded ->
            Task.after (Task.putLine (Str.concat "encoded: " encoded)) \_ ->
                when Base64.toStr encoded is
                    Ok decoded -> 
                        Task.putLine (Str.concat "decoded: " decoded)

                    Err _ -> 
                        Task.putLine "sadness"

