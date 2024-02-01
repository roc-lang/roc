platform "test/types"
    requires { Flags, Model, } { main : App Flags Model }
    exposes []
    packages {}
    provides [ mainForHost ]

mainForHost : App Flags Model
mainForHost = main
