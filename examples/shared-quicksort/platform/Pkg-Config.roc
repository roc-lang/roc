platform examples/shared-quicksort
    requires { quicksort : List I64 -> List I64 }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : List I64 -> List I64 
mainForHost = \list -> quicksort list
    
