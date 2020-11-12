fibonacci = \num ->
    if num < 2 then
        num
    else
        fibonacci (num - 1) + fibonacci (num - 2)

fibonacci 9


rust:

extern fn roc_fx_fopen(filename: RocStr) -> *mut FILE {
    filename.cstr(|filename| libc::fopen(filename, _))
}


roc:

effects
    {
        fopen : Str -> Effect Ptr,
        readBytes : Ptr, Len -> Effect ReadBytesResult,
        fclose : Ptr -> Effect {}
    }


Ptr : [ @Ptr Len ]

ReadBytesResult :
    [
        Bytes (List U8)
        Eof
        Err File.Err
    ]

walkBytes : Str, Len, state, (state, List U8 -> state) -> Task state (File.Err *)
walkBytes = \filename, chunkBytes, initialState, step ->
    Task.after (Task.fromEffect (Effect.fopen filename)) \filePtr ->

    finish =
        Task.after (walkBytesHelp filePtr chunkBytes initialState step) \state ->
        # Close the file before returning success.
        Task.after (Task.fromEffect (Effect.fclose filePtr)) \{} ->
            Task.succeed state

    Task.onError finish \err ->
    # If there was an error, close the file before returning the error.
    Task.after (Task.fromEffect (Effect.fclose filePtr)) \{} ->
        Task.fail err


walkBytesHelp : Ptr, Len, state, (state, List U8 -> state) -> Task state (File.Err *)
walkBytesHelp = \filePtr, chunkBytes, state, step ->
    Effect.after (Task.fromEffect (Effect.readBytes filePtr chunkBytes)) \result ->
        when result is
            Bytes bytes -> walkBytesHelp filePtr (step state bytes) step
            Eof -> Task.succeed state
            Err err -> Task.fail err

## This is the most flexible way to read bytes from a file. It lets you:
## * Specify the number of bytes to read at a time, and change it in between readings
## * Run other tasks between readings
## * Stop reading early if desired
## Once everything is finished, the file gets closed automatically.
readBytesUntil : Str, Len, state, (state, List U8 -> Task [ Continue Len state, Done state ] []err) -> Task state (File.Err err)
readBytesUntil = \filename, chunkBytes, initialState, step ->
    Task.after (Task.fromEffect (Effect.fopen filename)) \filePtr ->
    Task.after (readBytesHelp filePtr chunkBytes initialState step) \state ->
    Task.after (Task.fromEffect (Effect.fclose filePtr)) \{} ->
        Task.succeed state


readBytesHelp : Ptr, Len, state, (state, List U8 -> Task state []err) -> Task state (File.Err err)
readBytesHelp = \filePtr, chunkBytes, state, step ->
    Effect.after (Task.fromEffect (Effect.readBytes filePtr chunkBytes)) \result ->
        when result is
            Bytes bytes -> Task.after (step state bytes) \answer ->
                when answer is
                    Continue newChunkBytes newState -> walkBytesHelp filePtr newChunkBytes newState step
                    Done finalState -> Task.succeed finalState
            Eof -> Task.succeed state
            Err err -> Task.fail err

## Reads the UTF-8 contents of a file into a #Str.
readUtf8 : File.Path -> Task Str (File.Err *)

## Reads the UTF-8 contents of a file into a #Str.
readUtf8 : File.Path -> Task Str File.Err*


## Reads UTF-8 strings from a file in chunks. Each chunk will be up to 4096
## bytes in size, and will contain only whole graphemes.
readUtf8 : File.Path, state, (state, Str -> Task state []err) -> Task state (File.Err err)
readUtf8 = \filename, chunkBytes, initialState, step ->
    Task.after (Task.fromEffect (Effect.fopen filename)) \filePtr ->
    Task.after (readBytesHelp filePtr chunkBytes initialState step) \state ->
    Task.after (Task.fromEffect (Effect.fclose filePtr)) \{} ->
        Task.succeed state


readBytesHelp : Ptr, Len, state, (state, List U8 -> Task state []err) -> Task state (File.Err err)
readBytesHelp = \filePtr, chunkBytes, state, step ->
    Effect.after (Task.fromEffect (Effect.readBytes filePtr chunkBytes)) \result ->
        when result is
            Bytes bytes -> Task.after (step state bytes) \newState ->
                walkBytesHelp filePtr newState step
            Eof -> Task.succeed state
            Err err -> Task.fail err
