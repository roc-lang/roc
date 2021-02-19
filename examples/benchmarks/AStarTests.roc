app "astar-tests"
    packages { base: "platform" }
    imports [base.Task, AStar]
    provides [ main ] to base

fromList : List a -> Set a
fromList = \list -> List.walk list (\x, a -> Set.insert a x) Set.empty


main : Task.Task {} []
main =
    Task.after Task.getInt \n ->
        when n is
            1 -> 
                Task.putLine (showBool test1)

            _ -> 
                ns = Str.fromInt n
                Task.putLine "No test \(ns)"

showBool : Bool -> Str
showBool = \b ->
    when b is
        True -> "True"
        False -> "False"

test1 : Bool
test1 = 
    example1 == [3, 4]

example1 : List I64
example1 =
    step : I64 -> Set I64
    step = \n ->
        when n is
            1 -> fromList [ 2,3 ]
            2 -> fromList [4]
            3 -> fromList [4]
            _ -> fromList []

    cost : I64, I64 -> F64
    cost = \_, _ -> 1 

    when AStar.findPath cost step 1 4 is 
        Ok path -> path
        Err _ -> []
