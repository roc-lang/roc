platform examples/cli
    requires {}{ main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllBytes : Str -> Effect { bytes : List U8, error: FileReadErr [ A ] },
            # TODO FIXME moving this to the end of the list (even after removing trailing comma)
            # gives a parse error on the `Str, Str` arguments
            writeAllBytes : Str, List U8 -> Effect I32,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
