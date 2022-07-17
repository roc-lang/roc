interface File
    exposes [writeUtf8, write]
    imports [Effect, Path.{ Path }, Task.{ Task }, InternalTask]

FileWriteErr a : [
    FileWriteErr [
        NotFound Path,
        FileWasDir Path,
    ]
]a

writeUtf8 : Path, Str -> Task {} (FileWriteErr *) [Write [Disk]*]*
writeUtf8 = \path, str ->
    Effect.writeUtf8 path str
    |> Effect.map  (\_ -> Ok {}) # TODO actually handle errors
    |> InternalTask.fromEffect

write : Path, List U8 -> Task {} (FileWriteErr *) [Write [Disk]*]*
write = \path, bytes ->
    Effect.writeBytes path bytes
    |> Effect.map  (\_ -> Ok {}) # TODO actually handle errors
    |> InternalTask.fromEffect
