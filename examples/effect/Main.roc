app "effect-example"
    packages { base: "./platform" }
    imports [ base.Task.{ Task, after } ]
    provides [ main ] to base

main : Task {}
main =
    Task.putLine "Hello world"
    