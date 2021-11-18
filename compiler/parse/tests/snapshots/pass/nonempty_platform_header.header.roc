platform foo/barbaz
    requires {model=>Model} { main : {} }
    exposes []
    packages { foo: "./foo" }
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}
