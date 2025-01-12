platform "cli"
    requires {}{ main! : () => Result () [] }
    exposes []
    packages {}
    imports [ Foo.{ Foo } ]
    provides [ main_for_host ]
