platform "typescript-interop"
    requires {} { main : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \message ->
    main message
