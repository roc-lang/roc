interface Dir
    exposes [ReadErr, DeleteErr, DirEntry, deleteEmptyDir, deleteRecursive, list]
    imports [Effect, Task.{ Task }, InternalTask, Path.{ Path }, InternalPath, InternalDir]

ReadErr : InternalDir.ReadErr

DeleteErr : InternalDir.DeleteErr

DirEntry : InternalDir.DirEntry

## Lists the files and directories inside the directory.
list : Path -> Task (List Path) [DirReadErr Path ReadErr]
list = \path ->
    effect = Effect.map (Effect.dirList (InternalPath.toBytes path)) \result ->
        when result is
            Ok entries -> Ok (List.map entries InternalPath.fromOsBytes)
            Err err -> Err (DirReadErr path err)

    InternalTask.fromEffect effect

## Deletes a directory if it's empty.
deleteEmptyDir : Path -> Task {} [DirDeleteErr Path DeleteErr]

## Recursively deletes the directory as well as all files and directories inside it.
deleteRecursive : Path -> Task {} [DirDeleteErr Path DeleteErr]
