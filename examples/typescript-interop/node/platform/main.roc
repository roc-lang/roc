platform "typescript-interop"
    requires {} { main : Str -> Str }
    exposes []
    packages {}
    imports [Json]
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \message ->
    main message
