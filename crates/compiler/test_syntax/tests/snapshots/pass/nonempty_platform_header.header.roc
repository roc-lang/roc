platform "foo/barbaz"
    requires {Model} { main : {} }
    exposes []
    packages { foo: "./foo" }
    provides [ mainForHost ]
