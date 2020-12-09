interface File
    exposes [ Err, readUtf8,  ]
    imports [ Task.{ Task }, Effect.{ after }, Path.{ Path } ]

## These errors can happen when opening a file, before attempting to read from
## it or write to it. The #FileReadErr and #FileWriteErr tag unions begin with
## these tags and then add more specific ones.
FileOpenErr a :
    [
        FileNotFound Path,
        PermissionDenied Path,
        SymLinkLoop Path,
        TooManyOpenFiles Path,
        IoError Path,
        UnknownError I64 Path,
    ]a

## Errors when attempting to read a non-directory file.
FileReadErr a :
    FileOpenErr
        [
            FileWasDir Path,
            InvalidSeek Path,
            IllegalByteSequence Path,
            FileBusy Path,
        ]a

## Errors when attempting to read a directory.
DirReadErr a :
    FileOpenErr
        [
            FileWasNotDir Path,
        ]a

## Errors when attempting to write a non-directory file.
FileWriteErr a :
    FileOpenErr
        [
            FileWasDir Path,
            ReadOnlyFileSystem Path,
        ]a


## Read a file's raw bytes
#readBytes : Path -> Task (List U8) (FileReadErr *)
#readBytes = \path ->
#    Effect.readBytes (Path.toStr path)

## Read a file's bytes and interpret them as UTF-8 encoded text.
readUtf8 : Path -> Task Str (FileReadErr [ BadUtf8 ]*)
readUtf8 = \path ->
   Effect.map (Effect.readAllUtf8 (Path.toStr path)) \answer ->
      # errno values - see
      # https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html
      when answer.errno is
          0 -> Ok answer.bytes # TODO use Str.fromUtf8 to validate a byte list as UTF-8 and return (Err BadUtf8) if validation fails
          1 -> Err (PermissionDenied path)
          2 -> Err (FileNotFound path)
          19 -> Err (FileWasDir path)
          # TODO handle other errno scenarios that could come up
          _ -> Err (UnknownError errno path)

## Read a file's bytes, one chunk at a time, and use it to build up a state.
##
## After each chunk is read, it gets passed to a callback which builds up a
## state - optionally while running other tasks.
#readChunks : Path, U64, state, (state, List U8 -> Task state []err) -> Task state (FileReadErr err)

## Like #readChunks except after each chunk you can either `Continue`,
## specifying how many bytes you'd like to read next, or `Stop` early.
#readChunksOrStop : Path, U64, state, (state, List U8 -> [ Continue U64 (Task state []err), Stop (Task state []err) ]) -> Task state (FileReadErr err)

readBytes : Str -> Task.Task Str (FileReadErr *)
readBytes = \path -> Effect.always (Ok path)
