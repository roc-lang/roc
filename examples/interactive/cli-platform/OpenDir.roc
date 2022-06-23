interface OpenDir
    exposes [OpenDir, open, entries, nextEntry, walk, walkUntil]
    imports []

OpenDir permissions := {
    # Nat fits both the UNIX and Windows versions of this.
    handleOrFd : Nat,

    # Unlike `OpenFile`, these can never be stdout/stdin/stderr, so we always need a refcount.
    # Otherwise this works the same way as OpenFile.
    references : Box [],
} # has Eq, Hash, Ord # no Encode or Decode; you should never serialize these!

# uses opendir or FindFirstIndexW under the hood
open : Path -> Task OpenDir (FileOpenErr *) [Read [Disk]*]*

## Advances the OpenDir to the next entry and returns it. If there are no more entries, returns Err NoMoreDirEntries.
nextEntry : OpenDir -> Task DirEntry (DirReadErr [NoMoreDirEntries]*) [Read [Disk]*]*

# uses readdir repeatedly - not faster, just more convenient.
entries : OpenDir -> Task (List DirEntry) (DirReadErr *) [Read [Disk]*]*

# uses readdir because readdir_r is deprecated https://man7.org/linux/man-pages/man3/readdir_r.3.html
# a nice thing about readdir is that it tells you not to attempt to clean up the pointer it returns, since it may be statically allocated,
# so since we can't improve upon the performance of the malloc/free it might be using, we have no reason to spend time doing that
walk :
    OpenDir,
    state,
    (state, DirEntry -> Task state (ReadDirErr x) [Read [Disk]a]b)
    -> Task state (ReadDirErr x) [Read [Disk]a]b

walkUntil :
    OpenDir,
    state,
    (state, DirEntry -> Task [Continue state, Done] (ReadDirErr x) [Read [Disk]a]b)
    -> Task state (ReadDirErr x) [Read [Disk]a]b
