platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File.{ ReadErr, ReadErrTag, ReadUtf8Err } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            # TODO FIXME it should be possible to replace this whole union
            # with just `ReadErr`, but using imported type aliases here
            # gives an error
            readAllBytes : Str -> Effect (Result (List U8) [ FileBusy Str, FileWasDir Str, IllegalByteSequence Str, InvalidSeek Str ]),
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
