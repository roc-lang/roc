interface Test exposes [  Test, run, runSuite, runSuites, runF, runSuiteF, runSuitesF ] imports [ Console]

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
# There is also the notion of a Suite:
#
#     Suite : { name : Str, tests : List Test}  
#
# To run a suite do this:
#
#     runSuite myTestSuit
#
# You can also run lists of suites:
#
#     runSuites [suite1, suite2]
#
# The functions `runSuiteF` and `runSuitesF` display failures only.
#
# See the file `TestTest.roc` for examples or to test this interface.

Test : { name : Str, test: Bool }

Suite : { name : Str, tests : List Test}

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



failures  : List Test -> List Test
failures = \testss -> filterList testss (\t -> t.test == False) 

runSuite : Suite -> Str
runSuite = 
    \suite -> run suite.tests suite.name



runSuites : List Suite -> Str
runSuites = 
   \suites -> List.prepend (List.map suites runSuite) "TESTS\n=====\n" |> Str.joinWith ""    


runF : List Test, Str -> Str
runF = \tests, title -> 
   results = tests |> failures |> List.map eval |> Str.joinWith "\n" 
   n = Str.countGraphemes title
   underline = strRepeat n "-"
   Str.joinWith [title, "\n", underline, "\n", results] "" |> spaceAboveBelow

runSuiteF : Suite -> Str
runSuiteF = 
    \suite -> runF suite.tests suite.name

runSuitesF : List Suite -> Str
runSuitesF = 
     \suites -> List.prepend (List.map suites runSuiteF) "TESTS\n=====\n" |> Str.joinWith ""   

t1 = {name: "Addition", test: 1 + 1 == 2 }
t2 = {name: "Bozo", test: 1 + 1 == 3}
t3 = {name: "Extract field", test: (\t -> t.test) t1 == True}


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






