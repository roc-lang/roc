platform "foo/barbaz"
    requires { Model } { main : {} }
    exposes []
    packages { foo: "./foo" }
    imports []
    provides [main_for_host]
