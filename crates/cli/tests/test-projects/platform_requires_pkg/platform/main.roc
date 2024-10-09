platform "test"
    requires {} { main : _ }
    exposes []
    packages {
        foo: "../foo/main.roc",
    }
    imports []
    provides [mainForHost]

import foo.Foo

mainForHost : Str
mainForHost =
    "$(main) $(Foo.foo)"
