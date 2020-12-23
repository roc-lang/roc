app "task-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task, after }, base.File, base.Path ]
    provides [ main ] to base

main : Task.Task {} (File.FileReadErr [BadUtf8])
main =
    when Path.fromStr "Cargo.toml" is
        Ok path ->
            Task.after (Task.putLine "Our Cargo.toml:") \_ ->
            Task.after (File.readUtf8 path) (\line -> Task.putLine line)
        _ -> Task.putLine "invalid path"
