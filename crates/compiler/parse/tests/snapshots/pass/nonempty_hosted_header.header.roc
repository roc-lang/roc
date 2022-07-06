hosted Foo
    exposes
        [
            Stuff,
            Things,
            somethingElse,
        ]
    imports
        [
            Blah,
            Baz.{ stuff, things },
        ]
    generates Bar with
        [
            map,
            after,
            loop,
        ]
