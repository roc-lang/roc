platform examples/reverse
    requires {}{ reverse : List Str -> List Str }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : List Str -> List Str
mainForHost = \list -> reverse list
