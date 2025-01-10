platform "test-platform"
    requires {} { main : Bool -> [Some Str, None] }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : Bool -> [Some Str, None]
main_for_host = \u -> main(u)
