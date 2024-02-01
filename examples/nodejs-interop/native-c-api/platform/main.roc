platform "nodejs-interop"
    requires {} { main : Str -> Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str -> Str
mainForHost = \message ->
    main message
