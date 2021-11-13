platform examples/cli
    requires {}{ main : Str -> Task {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            openFile : Str -> Effect U64,
            closeFile : U64 -> Effect {},
            withFileOpen : Str, (U64 -> Effect (Result ok err)) -> Effect {},
            getFileLine : U64 -> Effect Str,
            getFileBytes : U64 -> Effect (List U8),
            putLine : Str -> Effect {},
            putRaw : Str -> Effect {},
            # Is there a limit to the number of effect, uncomment the next line and it crashes
            #getLine : Effect Str,
            getChar : Effect U8
        }

mainForHost : Str -> Task {} [] as Fx
mainForHost = \file -> main file
