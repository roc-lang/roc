platform "quicksort"
    requires {} { quicksort : List I64 -> List I64 }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : List I64 -> List I64
main_for_host = \list -> quicksort(list)
