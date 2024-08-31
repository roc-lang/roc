app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.File
import pf.Path
import pf.Env

main : Task {} [Exit I32 Str]_
main =
    pathStr = "out.txt"

    task =
        cwdPath = Env.cwd!
        cwdStr = Path.display cwdPath
        Stdout.line! "Current working directory: $(cwdStr)"

        dirEntries = Path.listDir! cwdPath
        contentsStr = Str.joinWith (List.map dirEntries Path.display) "\n    "
        Stdout.line! "Directory contents:\n    $(contentsStr)\n"
        Stdout.line! "Writing a string to out.txt"
        File.writeUtf8! pathStr "a string!"
        contents = File.readUtf8! pathStr
        Stdout.line! "I read the file back. Its contents: \"$(contents)\""

    when Task.result! task is
        Ok {} -> Stdout.line! "Successfully wrote a string to out.txt"
        Err err ->
            msg =
                when err is
                    FileWriteErr _ PermissionDenied -> "PermissionDenied"
                    FileWriteErr _ Unsupported -> "Unsupported"
                    FileWriteErr _ (Unrecognized _ other) -> other
                    FileReadErr _ _ -> "Error reading file"
                    _ -> "Uh oh, there was an error!"

            Task.err (Exit 1 msg)
