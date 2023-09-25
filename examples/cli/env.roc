app "env"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Stderr, pf.Env, pf.Task.{ Task }]
    provides [main] to pf

main : Task { body : List U8, headers : List Str, status : U16 } []
main =
    Task.ok { status: 200, headers: [], body: "The Answer" |> Str.toUtf8 }
