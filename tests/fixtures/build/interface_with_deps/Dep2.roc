interface Dep2
    exposes [ two ]
    imports [ Dep3.Blah.{ foo, bar } ]

one = 1

foo = "foo" # TODO FIXME this should be reported as shadowing!

two = 2.0

one = 1

