interface WithBuiltins
    exposes [ blah ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

blah = Int.highestSupported

#two = Float.highestSupported

#three = Float.highestSupported / 2.0
