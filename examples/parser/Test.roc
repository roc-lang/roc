interface Test exposes [  Test, eval, run, validate1, validate2 ] imports [ Utility]

Test : { name : Str, test: Bool }

passFail : Bool -> Str 
passFail = \pass -> if pass then "Pass :: " else "Fail :: "

eval : Test -> Str
eval = \{ name, test } -> Str.concat (passFail test) name

run : List Test -> Str
run = \tests -> List.map tests eval |> Utility.concatStrListWithSeparator "\n" |> Utility.spaceAboveBelow

failures  : List Test -> List Test
failures = \tests -> Utility.filterList tests (\t -> t.test) 

t1 = {name: "Addition", test: 1 + 1 == 2 }
t2 = {name: "Bozo", test: 1 + 1 == 3}
tt1 = {name: "Addition", test: 1 + 1 == 2 }
tt2 = {name: "Bozo", test: 1 + 1 == 3}
t3 = {name: "Extract field", test: (\t -> t.test) t1 == True}
t4 = {name: "failures", test: List.len (failures [tt1,tt2]) == 1}

validate1 = [t1, t2, t3]
validate2 = [t4]



