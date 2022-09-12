interface File
    exposes [ReadErr, WriteErr, write, writeUtf8, writeBytes, readUtf8, readBytes]
    imports [Effect, Task.{ Task }, InternalTask, InternalFile, Path.{ Path }, InternalPath]

ReadErr : InternalFile.ReadErr

WriteErr : InternalFile.WriteErr

## For example, suppose you have a [JSON](https://en.wikipedia.org/wiki/JSON)
## `EncodingFormat` named `Json.toCompactUtf8`. Then you can use that format
## to write some encodable data to a file as JSON, like so:
##
##     File.write
##         (Path.fromStr "output.json")
##         { some: "json stuff" }
##         Json.toCompactUtf8
##     # Writes the following to the file `output.json`:
##     #
##     # {"some":"json stuff"}
##
## If writing to the file fails, for example because of a file permissions issue,
## the task fails with [WriteErr].
##
## This opens the file first and closes it after writing to it.
##
## To write unformatted bytes to a file, you can use [File.writeBytes] instead.
write : Path, val, fmt -> Task {} [FileWriteErr Path WriteErr]* [Write [File]*]* | val has Encode.Encoding, fmt has Encode.EncoderFormatting
write = \path, val, fmt ->
    bytes = Encode.toBytes val fmt

    # TODO handle encoding errors here, once they exist
    writeBytes path bytes

## Write bytes to a file.
##
##     # Writes the bytes 1, 2, 3 to the file `myfile.dat`.
##     File.writeBytes (Path.fromStr "myfile.dat") [1, 2, 3]
##
## This opens the file first and closes it after writing to it.
##
## To format data before writing it to a file, you can use [File.write] instead.
writeBytes : Path, List U8 -> Task {} [FileWriteErr Path WriteErr]* [Write [File]*]*
writeBytes = \path, bytes ->
    InternalPath.toBytes path
    |> Effect.fileWriteBytes bytes
    |> InternalTask.fromEffect
    |> Task.mapFail \err -> FileWriteErr path err

## Write a [Str] to a file, encoded as [UTF-8](https://en.wikipedia.org/wiki/UTF-8).
##
##     # Writes "Hello!" encoded as UTF-8 to the file `myfile.txt`.
##     File.writeUtf8 (Path.fromStr "myfile.txt") "Hello!"
##
## This opens the file first and closes it after writing to it.
##
## To write unformatted bytes to a file, you can use [File.writeBytes] instead.
writeUtf8 : Path, Str -> Task {} [FileWriteErr Path WriteErr]* [Write [File]*]*
writeUtf8 = \path, str ->
    InternalPath.toBytes path
    |> Effect.fileWriteUtf8 str
    |> InternalTask.fromEffect
    |> Task.mapFail \err -> FileWriteErr path err

## Read all the bytes in a file.
##
##     # Read all the bytes in `myfile.txt`.
##     File.readBytes (Path.fromStr "myfile.txt")
##
## This opens the file first and closes it after reading its contents.
##
## To read and decode data from a file, you can use `File.read` instead.
readBytes : Path -> Task (List U8) [FileReadErr Path ReadErr]* [Read [File]*]*
readBytes = \path ->
    InternalPath.toBytes path
    |> Effect.fileReadBytes
    |> InternalTask.fromEffect
    |> Task.mapFail \err -> FileReadErr path err

## Read a [Str] from a file containing [UTF-8](https://en.wikipedia.org/wiki/UTF-8)-encoded text.
##
##     # Reads UTF-8 encoded text into a `Str` from the file `myfile.txt`.
##     File.readUtf8 (Path.fromStr "myfile.txt")
##
## This opens the file first and closes it after writing to it.
## The task will fail with `FileReadUtf8Err` if the given file contains invalid UTF-8.
##
## To read unformatted bytes from a file, you can use [File.readBytes] instead.
readUtf8 :
    Path
    -> Task
    Str
    [FileReadErr Path ReadErr, FileReadUtf8Err Path _]*
    [Read [File]*]*
readUtf8 = \path ->
    effect = Effect.map (Effect.fileReadBytes (InternalPath.toBytes path)) \result ->
        when result is
            Ok bytes ->
                Str.fromUtf8 bytes
                |> Result.mapErr \err -> FileReadUtf8Err path err

            Err readErr -> Err (FileReadErr path readErr)

    InternalTask.fromEffect effect

# read :
#     Path,
#     fmt
#     -> Task
#         Str
#         [FileReadErr Path ReadErr, FileReadDecodeErr Path [Leftover (List U8)]Decode.DecodeError ]*
#         [Read [File]*]*
#     | val has Decode.Decoding, fmt has Decode.DecoderFormatting
# read = \path, fmt ->
#     effect = Effect.map (Effect.fileReadBytes (InternalPath.toBytes path)) \result ->
#         when result is
#             Ok bytes ->
#                 when Decode.fromBytes bytes fmt is
#                     Ok val -> Ok val
#                     Err decodingErr -> Err (FileReadDecodeErr decodingErr)
#             Err readErr -> Err (FileReadErr readErr)
#     InternalTask.fromEffect effect
