platform "nodejs-interop"
    requires {} { main : Str -> Str }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : Str -> Str
main_for_host = \message ->
    main(message)
