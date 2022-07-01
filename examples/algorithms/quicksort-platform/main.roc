platform "quicksort"
    requires {} { quicksort : List Str -> List Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : List Str -> List Str
mainForHost = \list -> quicksort list
