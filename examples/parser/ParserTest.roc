app "parseapp"
     packages { base: "platform" }
     imports [base.Task, 
        Parser2.{runU8, any, satisfy, andThen, second, first, map },
        Test]
     provides [ main ] to base


# Definition of the tests

anyT = {name : "(1) run \"abcd any => \"a\"", test: runU8 "abcd" any == "a" }

satisfyT = {name : "(2) run \"abcd\" satisfy (\\u -> u == 97)) => \"a\"", test : runU8 "abcd" (satisfy (\u -> u == 97)) == "a" }

andThenT = { name: "(3) andThen: recognize strings with repeated first character", test: runU8 "aaxyz" (andThen any satisfyWhatCameBefore) == "a"}
satisfyWhatCameBefore = \u2 -> satisfy (\u3 -> u3 == u2)

satisfyA = satisfy (\u -> u == 97)
satisfyB = satisfy (\u -> u == 98)

secondT = {name : "(4) Use 'second' to recognize strings with second character 'b'", test : runU8 "abcd" (second  any satisfyB) == "b"}
firstT = {name : "(5) Use 'first' to recognize strings beginning with 'a' and with any second character", test : runU8 "abcd" (first  satisfyA any) == "a"}

mapT = {name : "(6) Use map to shift output of parser: run \"abcd\" (map any (\\u -> u + 25)) == \"z\"", test : runU8 "abcd" (map any (\u -> u + 25)) == "z"  }


# Test suites

suite1a =  {name: "1. anyT", tests: [anyT]}
suite1b =  {name: "2. satisfyT", tests: [satisfyT]}
suite1c =  {name: "3. andThen", tests: [andThenT]}
suite1d =  {name: "4. second", tests: [secondT]}
suite1e=  {name: "5. first", tests: [firstT]}
suite1f=  {name: "6. map", tests: [mapT]}

suite2a = {name: "2a", tests: [anyT, satisfyT, andThenT]}
suite2b = {name: "2b", tests: [anyT, satisfyT, andThenT, secondT]}
suite3 =  {name: "All combinators", tests: [anyT, satisfyT, andThenT, secondT, firstT, mapT] }


# NOTE.  All of the functions imported from module Parser2 pass their inidividua respective
#        test. (Use suit1 with substitutions).  However, these tests do **not** necessarily
#        pass when used in combination with others.  Weird!


main : Task.Task {} []
main =
 
# Test.runSuite suite1a
# Test.runSuite suite1b
# Test.runSuite suite1c
# Test.runSuite suite1d
# Test.runSuite suite1e
# Test.runSuite suite1f
Test.runSuites [suite1a, suite1b, suite1c, suite1d, suite1e, suite1f, suite2a, suite2b, suite3]
   |> Task.putLine





