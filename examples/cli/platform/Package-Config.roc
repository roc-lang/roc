platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File.{ ReadErr, ReadErrTag } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
           #readAllBytes : Str -> Effect (Result (List U8)
           #                    # TODO FIXME it should be able to replace this whole union
           #                    # with just `ReadErr`, but that gives an error - so instead,
           #                    # ReadErr is inlined here.
           #                    [
           #                        FileWasDir Str,
           #                        InvalidSeek Str,
           #                        IllegalByteSequence Str,
           #                        FileBusy Str,
           #                    ]
           #                ),
            # TODO FIXME why does referencing File here (e.g. File.ReadErrTag or just ReadErrTag) not work?
            #            I guess there's no evidence that the Task import works either. Maybe imports don't work in
            #            platform modules in general? But we do get a warning if they're imported but not used!
            readAllBytes : Str -> Effect (Result (List U8) { path : Str, tag : [ FileBusy, FileWasDir, IllegalByteSequence, InvalidSeek ] }),
            # readAllBytes : Str -> Effect (List U8),
            # TODO FIXME moving this to the end of the list (even after removing trailing comma)
            # gives a parse error on the `Str, Str` arguments
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
