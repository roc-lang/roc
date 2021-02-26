app "test-base64"
    packages { base: "platform" }
    imports [base.Task, Base64 ]
    provides [ main ] to base

IO a : Task.Task a []

main : IO {}
main =
    when Base64.fromBytes (Str.toBytes "Hello World") is
        Ok str ->
            Task.putLine str

        Err _ ->
            Task.putLine "sadness"

