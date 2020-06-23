interface WithBuiltins
    exposes [ floatTest, divisionFn, divisionTest, intTest, constantNum, fromDep2, divDep1ByDep2 ]
    imports [ Dep1, Dep2.{ two } ]

floatTest = Float.highest

divisionFn = Num.div

x = 5.0

divisionTest = Float.highest / x

intTest = Int.highest

constantNum = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2
