interface WithBuiltins
    exposes [ floatTest, divisionFn, divisionTest, intTest, constantInt, fromDep2, divDep1ByDep2 ]
    imports [ Dep1, Dep2.{ two } ]

floatTest = Float.highest

divisionFn = Float.div

x = 5.0

divisionTest = Float.highest / x

intTest = Int.highest

constantInt = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2
