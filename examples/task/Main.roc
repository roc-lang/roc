app "task-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.File, base.Path ]
    provides [ main ] to base

main : Task.Task {} []
main =
    when Path.fromStr "thing.txt" is
        Ok path ->
            {} <- Task.await (Task.putLine "Writing to file")

            result <- Task.attempt (File.writeUtf8 path "zig is awesome")

            when result is
                Ok _ -> Task.putLine "successfully wrote to file"
                Err BadThing -> Task.putLine "error writing to file"
                Err _ -> Task.putLine "something worse"

        _ -> Task.putLine "invalid path"
