platform "test"
    requires {} { main : _ }
    exposes []
    packages {
        foo: "../foo/main.roc",
    }
    imports []
    provides [main_for_host]

import foo.Foo

main_for_host : Str
main_for_host =
    "$(main) $(Foo.foo)"
