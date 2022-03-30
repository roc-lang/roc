interface WithBuiltins
    exposes [ floatTest, divisionFn, divisionTest, intTest, constantNum, fromDep2, divDep1ByDep2 ]
    imports [ Dep1, Dep2.{ two } ]

floatTest = Num.maxFloat

divisionFn = Num.div

x = 5.0

divisionTest = Num.maxFloat / x

intTest = Num.maxI64

constantNum = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2
