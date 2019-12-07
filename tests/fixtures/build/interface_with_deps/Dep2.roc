interface Dep2
    exposes [ two ]
    imports [ Dep3.Blah.{ foo, bar } ]

one = 1

two = 2.0

foo = "foo"
