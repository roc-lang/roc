interface File
    exposes [ReadErr, WriteErr, writeUtf8, writeBytes, readUtf8, readBytes]
    imports [Effect, Task.{ Task }, InternalTask, InternalFile, Path.{ Path }, InternalPath]

ReadErr a : [
    FileReadErr InternalFile.ReadErr
]a

WriteErr a : [
    FileWriteErr InternalFile.WriteErr
]a

## For example, suppose you have a [JSON](https://en.wikipedia.org/wiki/JSON)
## [EncodingFormat] named `Json.toCompactUtf8`. Then you can use that format
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
## To write unformatted bytes to a file, you can use [writeBytes] instead.
# write : Path, val, fmt -> Task {} (WriteErr *) [Write [File]*]*
#     | val has Encode.Encoding, fmt has Encode.EncoderFormatting
# write = \path, val, fmt ->
#     Encode.toBytes val fmt
#     # TODO handle encoding errors here, once they exist
#     |> writeBytes

## To format data before writing it to a file, you can use [write] instead.
writeBytes : Path, List U8 -> Task {} (WriteErr *) [Write [File]*]*
writeBytes = \path, bytes ->
    InternalPath.toBytes path
    |> Effect.fileWriteBytes bytes
    |> InternalTask.fromEffect
    |> Task.mapFail FileWriteErr

## To write unformatted bytes to a file, you can use [writeBytes] instead.
writeUtf8 : Path, Str -> Task {} (WriteErr *) [Write [File]*]*
writeUtf8 = \path, str ->
    InternalPath.toBytes path
    |> Effect.fileWriteUtf8 str
    |> InternalTask.fromEffect
    |> Task.mapFail FileWriteErr

readBytes : Path -> Task (List U8) (ReadErr *) [Read [File]*]*
readBytes = \path ->
    InternalPath.toBytes path
    |> Effect.fileReadBytes
    |> InternalTask.fromEffect
    |> Task.mapFail FileReadErr

readUtf8 : Path -> Task Str (ReadErr [FileReadUtf8Err _]*) [Read [File]*]*
readUtf8 = \path ->
    effect = Effect.map (Effect.fileReadBytes (InternalPath.toBytes path)) \result ->
        when result is
            Ok bytes ->
                Str.fromUtf8 bytes
                |> Result.mapErr FileReadUtf8Err

            Err readErr -> Err (FileReadErr readErr)

    InternalTask.fromEffect effect

# read : Path, fmt -> Task val (ReadErr [FileReadDecodeErr DecodeError]*) [Read [File]*]*
#     | val has Decode.Decoding, fmt has Decode.DecoderFormatting
# read = \path ->
#     effect = Effect.after (Effect.fileReadBytes path) \result ->
#         when result is
#             Ok bytes ->
#                 when Decode.fromBytes bytes fmt is
#                     Ok val -> InternalTask.succeed val
#                     Err decodingErr -> Err (FileReadDecodeErr decodingErr)

#             Err readErr -> Err (FileReadErr readErr)

#     InternalTask.fromEffect effect
