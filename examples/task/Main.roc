app "task-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.File, base.Path ]
    provides [ main ] to base

main : Task.Task {} []
main =
    when Path.fromStr "vendor" is
        Ok path ->
            {} <- Task.await (Task.putLine "Our Cargo.toml:")

            result <- Task.attempt (File.readUtf8 path)

            # pathStr = Path.toStr path

            when result is
                Ok contents -> Task.putLine contents
                Err (FileNotFound _) -> Task.putLine "file not found"
                Err (BadUtf8 _ _) -> Task.putLine "bad utf8"
                Err (FileWasDir _) -> Task.putLine "file was dir"
                Err _ -> Task.putLine "Error retrieving file - error"

        _ -> Task.putLine "invalid path"
