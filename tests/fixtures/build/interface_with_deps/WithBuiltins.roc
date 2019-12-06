interface WithBuiltins
    exposes [ blah ]
    imports [ Dep1, Dep2.{ two, foo }, Dep3.Blah.{ bar } ]

floatTest = Float.highest

divisionFn = Float.div

x = 5.0

divisionTest = Float.highest / x

intTest = Int.highest

constantInt = 5

fromDep2 = two

divDep1ByDep2 = Dep1.three / fromDep2
