app "effect-example"
    packages { base: "./thing/platform-dir" }
    imports [ base.Task.{ Task, after } ]
    provides [ main ] to base

main : Task {}
main =
    Task.putLine "Hello world"
