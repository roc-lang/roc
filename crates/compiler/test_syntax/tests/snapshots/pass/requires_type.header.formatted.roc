platform "test/types"
    requires { Flags, Model } { main : App Flags Model }
    exposes []
    packages {}
    imports []
    provides [mainForHost]
