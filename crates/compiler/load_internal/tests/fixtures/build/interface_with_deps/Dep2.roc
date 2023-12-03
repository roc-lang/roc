interface Dep2
    exposes [one, two, blah]
    imports [Dep3.Blah.{ foo, bar }]

one = 1

blah = foo

two = 2.0

