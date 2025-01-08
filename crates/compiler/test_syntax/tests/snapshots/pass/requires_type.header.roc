platform "test/types"
    requires { Flags, Model, } { main : App Flags Model }
    exposes []
    packages {}
    imports []
    provides [ main_for_host ]

main_for_host : App Flags Model
main_for_host = main
