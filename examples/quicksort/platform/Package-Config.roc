platform "examples/quicksort"
    requires {}{ quicksort : List I64 -> List I64 }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}

mainForHost : List I64 -> List I64
mainForHost = \list -> quicksort list
