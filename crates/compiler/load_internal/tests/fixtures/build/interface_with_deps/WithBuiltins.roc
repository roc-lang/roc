interface WithBuiltins
    exposes [floatTest, divisionFn, divisionTest, intTest, constantNum, fromDep2, divDep1ByDep2]

import Dep1
import Dep2

floatTest = Num.maxF64

divisionFn = Num.div

x = 5.0

divisionTest = Num.maxF64 / x

intTest = Num.maxI64

constantNum = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2
