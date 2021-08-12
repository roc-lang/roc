interface Test exposes [  Test, eval, run ] imports [ Utility]

Test : { name : Str, test: Bool }

passFail : Bool -> Str 
passFail = \pass -> if pass then "Pass :: " else "Fail :: "

eval : Test -> Str
eval = \{ name, test } -> Str.concat (passFail test) name

run : List Test -> Str
run = \tests -> List.map tests eval |> Utility.concatStrListWithSeparator "\n" |> Utility.spaceAboveBelow





