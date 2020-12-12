platform examples/quicksort
    requires { quicksort : List I64 -> List I64 }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect {}

mainForHost : List I64 -> List I64 
mainForHost = \list -> quicksort list
