app "helloWorld"
    packages { pf: "examples/hello-world/platform/main.roc" }
    imports []
    provides [main] to pf

main = Str.splitFirst "abc" "b"
    |> Result.withDefault {before:"oh",after:"no"}
    |> \x -> x.before
