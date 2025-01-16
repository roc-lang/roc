platform "echo-in-rust"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : Str
main_for_host = main
