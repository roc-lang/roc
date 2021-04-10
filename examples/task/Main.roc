app "task-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.File, base.Path ]
    provides [ main ] to base

main : Task.Task {} (File.FileReadErr [ BadUtf8 Str.Utf8ByteProblem Nat ])
main =
    when Path.fromStr "Cargo.toml" is
        Ok path ->
            {} <- Task.await (Task.putLine "Our Cargo.toml:")

            line <- Task.await (File.readUtf8 path)

            Task.putLine line
        _ -> Task.putLine "invalid path"
