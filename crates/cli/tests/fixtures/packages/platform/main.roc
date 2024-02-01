platform "multi-module"
    requires {}{ main : Str }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : Str
mainForHost = main
