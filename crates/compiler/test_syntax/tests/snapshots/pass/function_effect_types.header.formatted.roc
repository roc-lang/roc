platform "cli"
    requires {} { main! : {} => Result {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [Foo.{ Foo }]
    provides [main_for_host]
