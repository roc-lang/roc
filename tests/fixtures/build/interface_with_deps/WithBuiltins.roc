interface WithBuiltins
    exposes [ floatTest, divisionFn, divDep1ByDep2, x, divisionTest, intTest, constantInt ]
    imports [ Dep1, Dep2.{ two } ]

floatTest = Float.highest

divisionFn = Float.div

x = 5.0

divisionTest = Float.highest / x

intTest = Int.highest

constantInt = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2
