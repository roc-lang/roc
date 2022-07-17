interface File
    exposes [writeUtf8]
    imports [Effect, Path, Task.{ Task }, InternalTask]

FileWriteErr a : [
    NotFound Path,
    FileWasDir Path,
]a

writeUtf8 : Path, Str -> Task {} (FileWriteErr *) [Write [Disk]*]*
writeUtf8 = \path, str ->
    Effect.writeUtf8 path str
    |> InternalTask.fromEffect
