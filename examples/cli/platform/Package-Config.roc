platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File.{ ReadErr } ]
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
            # readAllBytes : Str -> Effect (Result (List U8) *),
            readAllBytes : Str -> Effect (List U8),
            # TODO FIXME moving this to the end of the list (even after removing trailing comma)
            # gives a parse error on the `Str, Str` arguments
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
