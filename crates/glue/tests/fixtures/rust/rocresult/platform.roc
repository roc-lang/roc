platform "test-platform"
    requires {} { main : Bool -> Result Str I32 }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : Bool -> Result Str I32
main_for_host = \u -> main(u)
