interface WithBuiltins
    exposes [ blah ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

floatTest = Float.highest

divisionFn = Float.div

divisionTest = Float.highest / 2.0

intTest = Int.highest

constantInt = 5

divDep1ByDep2 = Dep1.three / 2.0
