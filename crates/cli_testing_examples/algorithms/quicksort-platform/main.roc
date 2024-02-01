platform "quicksort"
    requires {} { quicksort : List I64 -> List I64 }
    exposes []
    packages {}
    provides [mainForHost]

mainForHost : List I64 -> List I64
mainForHost = \list -> quicksort list
