## Interacting with files and directories on the filesystem.
interface File.Temp
    exposes []
    imports [ Task.{ Task }, File.Internal as Internal ]
