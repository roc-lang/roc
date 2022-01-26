platform "foo/barbaz"
    requires {Model} { main : {} }
    exposes []
    packages { foo: "./foo" }
    imports []
    provides [ mainForHost ]
    effects fx.Effect {}
