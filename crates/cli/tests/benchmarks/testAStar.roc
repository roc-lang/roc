app [main] { pf: platform "platform/main.roc" }

import pf.PlatformTask
import AStar

main : Task {} []
main =
    PlatformTask.putLine! (showBool test1)

    n = PlatformTask.getInt!
    when n is
        1 ->
            PlatformTask.putLine (showBool test1)

        _ ->
            ns = Num.toStr n
            PlatformTask.putLine "No test $(ns)"

showBool : Bool -> Str
showBool = \b ->
    if
        b
    then
        "True"
    else
        "False"

test1 : Bool
test1 =
    example1 == [2, 4]

example1 : List I64
example1 =
    step : I64 -> Set I64
    step = \n ->
        when n is
            1 -> Set.fromList [2, 3]
            2 -> Set.fromList [4]
            3 -> Set.fromList [4]
            _ -> Set.fromList []

    cost : I64, I64 -> F64
    cost = \_, _ -> 1

    when AStar.findPath cost step 1 4 is
        Ok path -> path
        Err _ -> []
