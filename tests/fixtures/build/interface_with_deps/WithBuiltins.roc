interface WithBuiltins
    exposes [ blah ]
    #imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]
    imports []


floatTest = Float.highest

divisionFn = Float.div

divisionTest = Float.highest / 2.0

intTest = Int.highest

constantInt = 5
