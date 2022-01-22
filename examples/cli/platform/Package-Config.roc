platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File.{ ReadErrTag } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllBytes :
                Str ->
                Effect
                    (
                        Result (List U8)
                        # THIS MUST BE MANUALLY KEPT IN SYNC with `ReadErrTag` in File.roc.
                        # TODO: replace this with just `File.ReadErrTag` once the bug has been fixed
                        # where imported type aliases don't work in these declarations.
                        ReadErrTag
                    ),
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

Foo : ReadErrTag

mainForHost : Task {} [] as Fx
mainForHost = main
