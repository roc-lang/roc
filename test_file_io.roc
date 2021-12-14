app "test_file_io"
    packages { pf: "examples/cli/platform" }
    imports [ pf.File, pf.Stdout, pf.Task.{ await, Task } ]
    provides [ main ] to pf

inputPath = "test_file_io.roc"

main =
    _ <- await (Stdout.line "Trying to read file...")
    task = File.readUtf8 inputPath
    Task.attempt task \result ->
      when result is
          Ok input -> Stdout.line "Contents:\n\(input)"
          Err _ -> Stdout.line "Failed to read input."
