app [main!] { pf: platform "platform/main.roc" }

import pf.Host
import AStar

main! = \{} ->
    Host.put_line!(show_bool(test1))

show_bool : Bool -> Str
show_bool = \b ->
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
            1 -> Set.from_list([2, 3])
            2 -> Set.from_list([4])
            3 -> Set.from_list([4])
            _ -> Set.from_list([])

    cost : I64, I64 -> F64
    cost = \_, _ -> 1

    when AStar.find_path(cost, step, 1, 4) is
        Ok(path) -> path
        Err(_) -> []
