app "memory-example"
    packages { platform: "memory-observable" }
    imports [ platform.Task.{ Task }, platform.File, platform.Path ]
    provides [ main ] to platform


main : Task.Task {} []
main =
    filename = "demo.txt"

    when Path.fromStr filename is
        Ok path ->
            {} <- Task.await (Task.putLine "Writing to \(filename)")

            result <- Task.attempt (File.writeUtf8 path "Hello, Philly ETE!")

            when result is
                Ok _ -> Task.putLine "Successfully wrote to \(filename)"
                Err _ -> Task.putLine "Uh oh, there was an error!"

        _ -> Task.putLine "Invalid path!"
