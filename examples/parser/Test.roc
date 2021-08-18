interface Test exposes [  Test, eval, run, runF, failures, testList ] imports [ Console]

# This interface provides a simple way to run tests.  A test is a record
# with two fields:
#
#     Test : { name : Str, test: Bool }
#
# for example,
#
#     t1 = {name : "1 + 1 == 2", test: 1 + 1 == 2}
#
# You run tests like this:
#
#     [t1,t2] |> Test.run "Test of potrezebie interface"  |> Task.putLine
#
# or 
#
#     [t1,t2] |> Test.runF "Test of potrezebie interface"  |> Task.putLine
#
# if you want to see failures only.
#
#

Test : { name : Str, test: Bool }

passFail : Bool -> Str 
passFail = \pass -> if pass then (Console.cyan "Pass :: ") else (Console.magenta "Fail :: ")

eval : Test -> Str
eval = \{ name, test } -> Str.concat (passFail test) name

# run: List Test -> Str
# run = \tests -> List.map tests eval |> concatStrListWithSeparator "\n" |> spaceAboveBelow


run : List Test, Str -> Str
run = \tests, title -> 
   results = List.map tests eval |> Str.joinWith "\n" 
   n = Str.countGraphemes title
   underline = strRepeat n "-"
   Str.joinWith [title, "\n", underline, "\n", results] "" |> spaceAboveBelow

runF : List Test, Str -> Str
runF = \tests, title -> 
   results = tests |> failures |> List.map eval |> Str.joinWith "\n" 
   n = Str.countGraphemes title
   underline = strRepeat n "-"
   Str.joinWith [title, "\n", underline, "\n", results] "" |> spaceAboveBelow


failures  : List Test -> List Test
failures = \testss -> filterList testss (\t -> t.test == False) 

t1 = {name: "Addition", test: 1 + 1 == 2 }
t2 = {name: "Bozo", test: 1 + 1 == 3}
t3 = {name: "Extract field", test: (\t -> t.test) t1 == True}


testList = [t1, t2, t3]


## Functions from parser/Utility.roc

## Lists  


filterList : List a, (a -> Bool) -> List a 
filterList = \list, predicate ->
   when List.first list is 
      Ok a -> if predicate a 
         then 
            filterList (List.drop list 1) predicate |> List.prepend a 
         else 
            filterList (List.drop list 1) predicate
      Err _ -> [ ]      


## Strings

strRepeat: Nat, Str -> Str
strRepeat = \n, str -> List.repeat n str |> Str.joinWith ""

spaceAboveBelow: Str -> Str
spaceAboveBelow = \str -> Str.joinWith ["\n", str, "\n"] ""






