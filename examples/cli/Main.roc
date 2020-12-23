app "cli-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task, after }, base.Stdout ]
    provides [ main ] to base

main : Task.Task {} *
main =
    Stdout.write "Hello, World!"
    # TODO accept args : List Str
    #when Path.fromStr "Cargo.toml" is
    #    Ok path ->
    #        Task.after (Task.putLine "Our Cargo.toml:") \_ ->
    #        Task.after (File.readUtf8 path) (\line -> Task.putLine line)
    #    _ -> Task.putLine "invalid path"
