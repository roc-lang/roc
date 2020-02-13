interface WithBuiltins
    exposes [ floatTest, divisionFn, divDep1ByDep2, x, divisionTest, intTest, constantInt, swap ]
    imports [ Dep1, Dep2.{ two } ]

floatTest = Float.highest

divisionFn = Float.div

x = 5.0

divisionTest = Float.highest / x

intTest = Int.highest

constantInt = 5

fromDep2 = Dep2.two

divDep1ByDep2 = Dep1.three / fromDep2

swap = \i, j, list ->
    when Pair (List.get list i) (List.get list j) is
        Pair (Ok atI) (Ok atJ) ->
            list
                |> List.set i atJ
                |> List.set j atI

        _ ->
            list
