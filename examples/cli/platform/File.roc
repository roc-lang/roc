## Interacting with files and directories on the filesystem.
interface File
    exposes [ read, write, append ]
    imports [ Task.{ Task }, File.Internal as Internal ]

## Reading consists of two steps, both of which can fail: opening the file and
## actually reading from it.
ReadProblem a : [ OpenFailed OpenErr Str, ReadFailed ReadErr Str ]a

## Writing consists of two steps, both of which can fail: opening the file and
## actually writing to it.
WriteProblem a : [ OpenFailed OpenErr Str, WriteFailed WriteErr Str ]a

## Appending consists of two steps, both of which can fail: opening the file and
## actually appending to it.
AppendProblem a : [ OpenFailed OpenErr Str, AppenFailed WriteErr Str ]a

## Open a file and read all of its bytes.
##
## For example, here's how to read a file's bytes and interpret them as a
## UTF-8 string:
##
##     File.readAll "myfile.txt"
##         |> Task.map Str.fromUtf8
read : Str -> Task (List U8) (ReadProblem *)
read = \path ->
    Effect.map (Effect.openRead path (NonTemp {})) \result ->
        when result is
            Ok fd ->
                Task.mapErr (Effect.readUntilEof fd) \err ->
                    ReadFailed err path

            Err err -> OpenFailed err path

## Open a file and read all of its bytes as a string encoded in UTF-8.
readUtf8 : Str -> Task Str (ReadProblem [ BadUtf8 ]*)
readUtf8 = \path ->
    Effect.map (Effect.openRead path (NonTemp {})) \result ->
        when result is
            Ok fd ->
                Effect.map (Effect.readUntilEof fd) \bytesResult ->
                    when bytesResult is
                        Ok bytes -> Str.fromUtf8 bytes
                        Err err -> Err (ReadFailed err path)

            Err err -> OpenFailed err path


## Open a file and write all of the given bytes to it.
##
## If the file did not exist, it will be created. If it did exist, its old
## contents will be replaced by these bytes.
##
## For example, here's how to write a string to a file using UTF-8 encoding:
##
##     File.writeAll "filename.txt" (Str.toUtf8 "file contents")
write : Str, List U8 -> Task.Task {} (WriteProblem *)
write = \path, contents ->
    mode = {} # TODO
    config = NonTemp { ifNotExists: (Create mode), truncate: True }

    Effect.map (Effect.openWrite path writeConfig) \result ->
        when result is
            Ok fd ->
                Task.mapErr (Effect.writeStart fd 0 contents) \err ->
                    WriteFailed err path

            Err err -> OpenFailed err path

## Open a file and append all of the given bytes to it.
##
## For example, here's how to append a string to a file using UTF-8 encoding:
##
##     File.append "filename.txt" (Str.toUtf8 "string to append")
append : List U8, Str -> Task.Task {} (AppendProblem *)
append = \bytes, str ->
    Effect.map (Effect.openAppend path (NonTemp {})) \result ->
        when result is
            Ok fd ->
                Task.mapErr (Effect.append fd contents) \err ->
                    AppendFailed err path

            Err err -> OpenFailed err path

## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)
