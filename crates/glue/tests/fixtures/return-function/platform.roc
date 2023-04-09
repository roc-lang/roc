platform "test-platform"
    requires {} { main : { f: I64, I64 -> I64, g: I64, I64 -> I64 } }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : { f: I64, I64 -> I64, g: I64, I64 -> I64 }
mainForHost = main 
