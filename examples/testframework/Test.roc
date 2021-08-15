interface Test exposes [  Test, eval, run, runF, failures, testList ] imports [ ]

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
passFail = \pass -> if pass then "Pass :: " else "Fail :: "

eval : Test -> Str
eval = \{ name, test } -> Str.concat (passFail test) name

# run: List Test -> Str
# run = \tests -> List.map tests eval |> concatStrListWithSeparator "\n" |> spaceAboveBelow


run : List Test, Str -> Str
run = \tests, title -> 
   results = List.map tests eval |> concatStrListWithSeparator "\n" 
   n = Str.countGraphemes title
   underline = strRepeat n "-"
   concatStrList [title, "\n", underline, "\n", results] |> spaceAboveBelow

runF : List Test, Str -> Str
runF = \tests, title -> 
   results = tests |> failures |> List.map eval |> concatStrListWithSeparator "\n" 
   n = Str.countGraphemes title
   underline = strRepeat n "-"
   concatStrList [title, "\n", underline, "\n", results] |> spaceAboveBelow


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
strRepeat = \n, str -> List.repeat n str |> concatStrList

spaceAboveBelow: Str -> Str
spaceAboveBelow = \str -> concatStrList ["\n", str, "\n"]


concatStrList : List Str -> Str 
concatStrList = \list -> 
    when List.first list is 
       Ok head -> 
           Str.concat head (concatStrList (List.drop list 1))
       Err _ -> "" 


concatStrListWithSeparator : List Str, Str -> Str 
concatStrListWithSeparator = \list, separator -> 
    when List.first list is 
       Ok head -> 
            # TODO: Not great code following
            rest = List.drop list 1
              if List.len rest > 0 then 
                Str.concat (Str.concat head separator) (concatStrListWithSeparator rest separator)
              else 
                Str.concat head (concatStrListWithSeparator rest separator)
       Err _ -> "" 




