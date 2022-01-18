platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File.{ ReadErr, ReadErrTag, ReadUtf8Err } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllBytes :
                Str ->
                Effect
                    (
                        Result (List U8)
                        # THIS MUST BE MANUALLY KEPT IN SYNC with `ReadErrTag` in File.roc,
                        # ane should be replaced with it once the bug has been fixed where
                        # imported type aliases don't work in these declarations
                        [ FileBusy, FileWasDir, IllegalByteSequence, InvalidSeek ]
                    ),
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
